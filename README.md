# Replication_of_Bucheli_et_al._-2022-
This repository provides codes for the replication study of Bucheli et al. 2022 Temperature effects on crop yields in heat index insurance.
You find the following R codes:

ags_merging_masud.R: data preperation process for municipality level direct replication
data_deduplicating_masud.R: data cleaning procedure for direct replication yield data 


data_1_plot.R: plots for the direct replication data at farm level
data_2_plot.R: plots for the direct replication data at municipality level
data_3_plot.R: plots for the extended replication data

 
full_project_direct_f_level.R: insurance calibration (with restricted cubic splines) and evaluation (with expected utility model) using farm level direct replication data
full_project_direct_m_level.R: insurance calibration (with restricted cubic splines) and evaluation (with expected utility model) using municipality level direct replication data
full_project_extended_m_level.R: insurance calibration (with restricted cubic splines) and evaluation (with expected utility model) using extended replication data
