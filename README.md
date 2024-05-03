# Replication_of_Bucheli_et_al._-2022-
# Replication Study: Temperature Effects on Crop Yields in Heat Index Insurance

This repository contains R codes for the replication study of Bucheli et al. (2022) titled "Temperature Effects on Crop Yields in Heat Index Insurance."

## Overview of R Codes:

1. **ags_merging_masud.R:** This script performs the data preparation process for the direct replication study at the municipality level.

2. **data_deduplicating_masud.R:** This script implements the data cleaning procedure for the direct replication yield data.

3. **data_1_plot.R:** Generates plots for the direct replication data at the farm level.

4. **data_2_plot.R:** Generates plots for the direct replication data at the municipality level.

5. **data_3_plot.R:** Generates plots for the extended replication data.

6. **full_project_direct_f_level.R:** Performs insurance calibration (with restricted cubic splines) and evaluation (with expected utility model) using farm-level direct replication data.

7. **full_project_direct_m_level.R:** Performs insurance calibration (with restricted cubic splines) and evaluation (with expected utility model) using municipality-level direct replication data.

8. **full_project_extended_m_level.R:** Performs insurance calibration (with restricted cubic splines) and evaluation (with expected utility model) using extended replication data.
   
# Data Sources

## 1. Phenology Data
- **Source:** Møller, Markus (2020). ”Data for: PhenoWin - A R Shiny application for visualization and extraction of phenological windows in Germany.” Mendeley Data, V1. DOI: [10.17632/37jxk3n9fy](https://doi.org/10.17632/37jxk3n9fy)
- **Availability:** The phenology data used in this study is publicly available on the Mendeley Data repository. Researchers can access and download the dataset using the provided DOI link.

## 2. Temperature Data
- **Source:** Cornes, R., G. van der Schrier, E.J.M. van den Besselaar, and P.D. Jones (2018). ”An Ensemble Version of the E-OBS Temperature and Precipitation Datasets.” J. Geophys. Res. Atmos., 123. DOI: [10.1029/2017JD02820](https://doi.org/10.1029/2017JD02820)
- **Availability:** The temperature data utilized in this study is accessible through the Copernicus Climate Change Service (C3S) website. Interested parties can access the data files via the provided DOI link.

## 3. Municipality Locations Data
- **Source:** German Federal Agency for Cartography and Geodesy (BKG)
- **Availability:** Municipality locations data at a scale of 1:5,000,000, as of December 31, is available on the BKG website. The data can be accessed [here](https://gdz.bkg.bund.de/index.php/default/digitale-geodaten/verwaltungsgebiete/verwaltungsgebiete-1-5-000-000-stand-31-12-vg5000-12-31.html).

## 4. Yield Data
### Direct replication data:
- **Source:** Confidential data collected by the insurance company ”gvf VersicherungsMakler AG.”
- **Availability:** Due to data confidentiality, the yield data used in this study is not publicly available. Access to the data may be restricted, and interested parties should contact ”gvf VersicherungsMakler AG” for further information.
### Extended replication data:
- **Source:** Confidential data from the Financial Accountancy Data Network (FADN) of the Free State of Saxony.
- **Availability:** The data is not publicly available. If required we can provide access to the data for replication purposes in a protected and confidential environment.



# Usage:

To replicate the analysis conducted in the study, clone or download this repository and execute the R scripts in a suitable environment.
1. **data_deduplicating_masud.R:** Run the script to clean the data by 

2. **data_deduplicating_masud.R:** This script implements the data cleaning procedure for the direct replication yield data.

3. **data_1_plot.R:** Generates plots for the direct replication data at the farm level.
