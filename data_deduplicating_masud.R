# =========================================================================================================
# 1. Explanation
# =========================================================================================================
#The collected data consisted of --- observations and majority of the observations were duplicated entries
#because of the anonymization process by the data collecting agency. #Therefore, first we got rid of duplicates.
#Secondly, the information which farm each observation belong to was also missing. In order to find farms with single 
#or both crops we combined the crops that are the closest by distance on the map. The rest of the observations are 
#considered to be farms with single crop. In the data, some outliers such as the ones that lies outside of the study
#area and the ones that get value zero have been filtered out.








# =========================================================================================================
# 2. Loading packages 
# =========================================================================================================

library(tidyverse)
library(combinat)
library(raster)
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(tidyr)

# =========================================================================================================
# 3. Specifications, loading and preparing data
# =========================================================================================================
df <- read_csv('data/data_masud_2.csv')
colnames(df) <- tolower(colnames(df))


# Checking unique number of values
df %>% 
  group_by(value) %>%
  tally() %>% 
  arrange(desc(n))


# Checking unique number of values based on other variables
df %>% 
  group_by(fruchtart,year,value) %>%
  tally() %>% 
  arrange(desc(n))

df %>% 
  mutate(farm_id=paste0(standort.breite,'_',standort.laenge)) %>% 
  select(referenznummer,farm_id) %>% 
  distinct() %>% 
  group_by(farm_id) %>% 
  tally() %>% 
  arrange(desc(n))

# =========================================================================================================
# 4. Deleting duplicated entries
# =========================================================================================================


# Defining cumulative sum resetting after zero.
csum_rst = function(x){
  
  cs = cumsum(x)
  cs - cummax((x == 0) * cs)
}

# Defining a function that merges two data.frame and calculate cumulative sum
# of number of equal values
get_eq <- function(list_two){
  
  list_two[[1]] %>% 
    
    inner_join(y = list_two[[2]],by='year') %>% 
    mutate(
      flag_eq = ifelse(value.x==value.y,1,0),
      flag_cons = csum_rst(flag_eq)
    )
}


### Winter Wheat ### 

# Creating winter wheat data
dt_wnt <- df %>% 
  filter(fruchtart=='Winterweizen')

# Splitting data into lists based on reference numbers for next use
all_list <- dt_wnt %>% 
  group_split(referenznummer) %>% 
  map(~.x %>% select(1,6,7)) 

# Creating combination vector that is for comparing all combination of
# reference numbers
cmb_vec <- combn(length(all_list),2)
dim(cmb_vec)

# Creating null list and data frame to store values in the next loop
list_itr <- list()
all_df <- data.frame(refernznummer.x=NULL,refernznummer.y=NULL, flag_cons=NULL)

for(i in 1:dim(cmb_vec)[2]){
  
  # Creating a temporary data frame as a result of merging two reference
  # numbers with the number of equal crop values
  df_tmp <- get_eq(all_list[cmb_vec[,i]])
  
  # If they have at least 3 same values, we store them as duplicates for next use
  if(any(df_tmp$flag_cons == 3)){
    
    all_df <- rbind(all_df, df_tmp %>%  
                      arrange(desc(flag_cons)) %>%
                      select(1,4,7) %>% 
                      slice(1) %>% 
                      mutate(it=i))
  }
  list_itr[[i]] <- df_tmp
}

# Creating a new list, counter and a vector to store unique IDs
list_pairid <- list()
j <- 0
id_vec <- c()
for(i in unique(all_df$referenznummer.x)){
  
  if(! i %in% id_vec){
    j <- j+1 # Increment for new id
    
    # Store pair duplicate ids to store in the list to deduplicate them
    ids_tmp  <- c(i, all_df %>% filter(referenznummer.x %in% i) %>% 
                    select(referenznummer.y) %>% 
                    pull())
    
    list_pairid[[j]] <- ids_tmp
    
    # Adding chekced ids to id_vec not to iterate again
    id_vec <- c(id_vec, ids_tmp)
  }
}

# Creating data frame to store duplicated reference numbers
# as groups to drop them
df_idgrp <- data.frame(id=NULL,grp=NULL)

for(i in 1:length(list_pairid)){
  df_idgrp <- rbind(df_idgrp, data.frame(id=list_pairid[[i]],grp=i))
}


# Creating a dataframe showing number of years/values in a reference number
df_year_cnt <- 
  dt_wnt %>% 
  group_by(referenznummer) %>% 
  summarise(n=n())

# Creating groupped data frame to bring number of values for each reference
# number to keep one of the duplicated ones with high number of years/values
df_grp <- 
  all_df %>% select(-it) %>% 
  pivot_longer(cols = c('referenznummer.x','referenznummer.y')) %>%
  select(-name) %>% 
  distinct() %>% 
  left_join(df_idgrp,by=c('value'='id')) %>% 
  left_join(df_year_cnt, by = c('value'='referenznummer')) %>% 
  select(-flag_cons)

# Keeping only ids to be dropped
df_grp1 <- df_grp %>% 
  group_split(grp) %>%
  map(~ .x %>% arrange(desc(n)) %>%  slice(2:nrow(.))) %>% 
  bind_rows()

# Dropping irrelevant/duplicates IDs
dt_wnt_dedup <-
  dt_wnt %>% 
  filter(!referenznummer %in% df_grp1$value)



### Winter Rapeseed  ###

# Creating winter data
dt_rps <- df %>% 
  filter(fruchtart=='Raps')

# Splitting data into lists based on reference numbers for next use
all_list <- dt_rps %>% 
  group_split(referenznummer) %>% 
  map(~.x %>% select(1,6,7))

# Creating combination vector that is for comparing all combination of
# reference numbers
cmb_vec <- combn(length(all_list),2)
dim(cmb_vec)

# Creating null list and data frame to store values in the next loop
list_itr <- list()
all_df <- data.frame(refernznummer.x=NULL,refernznummer.y=NULL, flag_cons=NULL)

for(i in 1:dim(cmb_vec)[2]){
  
  # Creating a temporary data frame as a result of merging two reference
  # numbers with the number of equal crop values
  df_tmp <- get_eq(all_list[cmb_vec[,i]])
  if(any(df_tmp$flag_cons == 3)){
    
    # If they have at least 3 same values, we store them as duplicates for next use
    all_df <- rbind(all_df, df_tmp %>%  
                      arrange(desc(flag_cons)) %>%
                      select(1,4,7) %>% 
                      slice(1) %>% 
                      mutate(it=i))
  }
  list_itr[[i]] <- df_tmp
}

# Creating a new list, counter and a vector to store unique IDs
list_pairid <- list()
j <- 0
id_vec <- c()
for(i in unique(all_df$referenznummer.x)){
  
  if(! i %in% id_vec){
    j <- j+1 # Increment for  each new id
  
    # Store pair duplicate ids to store in the list to deduplicate them
    ids_tmp  <- c(i, all_df %>% filter(referenznummer.x %in% i) %>% 
                    select(referenznummer.y) %>% 
                    pull())
    
    list_pairid[[j]] <- ids_tmp
    id_vec <- c(id_vec, ids_tmp)
  }
}

# Creating data frame to store duplicated reference numbers
# as groups to drop them
df_idgrp <- data.frame(id=NULL,grp=NULL)

for(i in 1:length(list_pairid)){
  df_idgrp <- rbind(df_idgrp, data.frame(id=list_pairid[[i]],grp=i))
}

# Creating a dataframe showing number of years/values in a reference number
df_year_cnt <- 
  dt_rps %>% 
  group_by(referenznummer) %>% 
  summarise(n=n())

# Creating groupped data frame to bring number of values for each reference
# number to keep one of the duplicated ones with high number of years/values
df_grp <- 
  all_df %>% select(-it) %>% 
  pivot_longer(cols = c('referenznummer.x','referenznummer.y')) %>%
  select(-name) %>% 
  distinct() %>% 
  left_join(df_idgrp,by=c('value'='id')) %>% 
  left_join(df_year_cnt, by = c('value'='referenznummer')) %>% 
  select(-flag_cons)

# Keeping only ids to be dropped
df_grp1 <- df_grp %>% 
  group_split(grp) %>%
  map(~ .x %>% arrange(desc(n)) %>%  slice(2:nrow(.))) %>% 
  bind_rows()


# Dropping irrelevant/duplicates IDs
dt_rps_dedup <-
  dt_rps %>% 
  filter(!referenznummer %in% df_grp1$value)

# =========================================================================================================
# 5. Grouping relevant observations under unique farm 
# =========================================================================================================

#########################################3

dt_wnt_dedup %>% View
dt_rps_dedup %>% View

# Combining deduplicated data back
dt_all <- bind_rows(dt_wnt_dedup,dt_rps_dedup)

# In this step, we want to group relevant farms which we
# achieve in few steps

# Splitting data into two list based on crop(wheat and rapeseed)
# Then split each list based on reference numbers
list_fruch_refn <- dt_all %>%
  select(1:2,4:6) %>%
  arrange(referenznummer) %>% 
  group_split(fruchtart) %>% 
  map(group_split,referenznummer)

# Creating cross list of these two crop lists and join each of them with all
# from all reference numbers by year
list_fruch_refn_cross <- cross2(list_fruch_refn[[1]], list_fruch_refn[[2]]) %>%
  simplify_all() %>% 
  map(~full_join(.x[[1]], .x[[2]], by='year'))


# Based on joined reference numbers, calculate their coordinate distance with
# abs(long.x-long.y)+abs(lat.x-lat.y) formula in which coord.x is one crop's
# reference number's coordinate while coord.y is for the another crop's
# reference number
dt_coord <- 
  list_fruch_refn_cross %>%  
  
  map(
    ~ .x %>% 
      mutate(coord_diff=abs(standort.laenge.x-standort.laenge.y)+abs(standort.breite.x-standort.breite.y),
             na_cnt=sum(is.na(fruchtart.y),is.na(fruchtart.x))) %>%
      select(-c(2,5,7))) %>% 
  bind_rows() %>% 
  filter(na_cnt==0)

# Keep only reference numbers and coordinate differences
st_x <-
  dt_coord %>%  
  select(referenznummer.x,referenznummer.y,coord_diff)  

# Select reference number and coordinate differences and
# split to lists based on reference number y to iterate and find
# minimum of coordinate differences
st_y <-
  dt_coord %>% 
  select(referenznummer.x,referenznummer.y,coord_diff) %>% 
  group_split(referenznummer.y) 


# We need to to find min coordinate difference of each 
# reference number x in the reference number y pair and validate it
# to make sure it ref.y's min value is ref.x's min value

# 1. Slice min from list_y
# 2. Filter ref.x 1 from list_x
# 3. Check ref.x 2 to be the minimum for ref.y[i]
# 4. If yes, then add grp, if no, check second ref.x 2
# start to store results and grouping and coord_diff of each group
# and apply cut off on coord_diff

grp_list <- list(refx=c(0),refy=c(0), grp=c(0))
j <- 1
for(i in 1:length(st_y)){
  f <- TRUE # flag to keep while loop iterating
  k <- 1 # Should break while loop when nrow exceeds k
  
  while(f){
    
    if(k>nrow(st_y[[i]])){
      grp=0 # if it exceeds the nrow then it is not a group
      print(grp)
      break
    }
    
    # ref y iterating on
    min_yx = st_y[[i]] %>% pull(referenznummer.y) %>% .[1]
    
    # ref x with min value with ref y iterating on
    min_x=st_y[[i]] %>% 
      arrange(coord_diff) %>% 
      slice(k) %>% 
      pull(referenznummer.x)
    
    # ref y min of ref x
    min_xy = st_x %>% 
      filter(referenznummer.x==min_x) %>% 
      arrange(coord_diff) %>% 
      slice(1) %>% 
      pull(referenznummer.y)
    
    
    # Comparing ref y iterating on with its minimum ref x's min y
    f_eq = (min_xy==min_yx)
    
    if(f_eq){
      f <- FALSE
      grp_list$refx[j]=min_x
      grp_list$refy[j]=min_yx
      grp_list$grp[j] = j
      j <- j+1
    } else{
      k <- k+1
    }
    
  }
}


dt_crops_grouped <- 
  tibble(ref.x=grp_list$refx,
         ref.y=grp_list$refy,
         grp_farm=grp_list$grp) %>% 
  pivot_longer(cols = c('ref.x','ref.y')) %>% 
  select(1,3)


dt_final <-
  dt_all %>% 
  left_join(dt_crops_grouped, by=c('referenznummer'='value')) %>% 
  mutate(grp_farm=ifelse(is.na(grp_farm),0,grp_farm)) %>% 
  filter(!(between(standort.laenge,10,11.2) & between(standort.breite,51.5,53))) %>% 
  filter(!(between(standort.laenge,6,10) & between(standort.breite,47,55))) %>%
  select(-anbauflaeche) %>% 
  filter(!referenznummer %in% c('ELNPR','250FY','ZHF30','AO1AE')) %>% # out of region
  filter(!year %in% c('1991','1992','1993','1994','1995','1996','1997','1998','1999','2019','2020'))%>%
  filter(referenznummer != 'MO8K9') %>%  # excessive values, outlier
  filter(referenznummer != 'S1GU4') # one of year's value equal to 0

# Uniforming referenznummer and coordinates
dt_new_map <-
  dt_final %>%
  select(referenznummer,grp_farm,fruchtart,standort.breite,standort.laenge) %>% 
  distinct() %>% 
  filter(grp_farm!=0 & fruchtart=='Raps') %>% 
  arrange(grp_farm) %>%
  select(grp_farm,referenznummer,standort.breite,standort.laenge)

dt_nonzero <-
  dt_final %>% 
  filter(grp_farm!=12) %>% 
  filter(grp_farm!=0) %>%
  select(-c(referenznummer,standort.breite,standort.laenge)) %>% 
  left_join(dt_new_map, by = 'grp_farm') %>% 
  select(referenznummer, fruchtart,standort.breite,standort.laenge,year,value,grp_farm)

dt_zero <-
  dt_final %>% 
  filter(grp_farm==0)

# Uniformed version dt_final
dt_final <-
  bind_rows(dt_nonzero,dt_zero)

write.csv(dt_final,'data/yield/dt_final.csv',row.names = FALSE)


