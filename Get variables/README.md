# Get Variables folder

The 'Get Variables' folder contains all the code needed to get you started with exploring time dimensions in UK Biobank data.

1. Extracting 'core' variables
   We have identified a set of core variables from the Biobank dataset which we think are essential to any time dimensions analysis project. These include variables such as demographics and geographic location.

   [Core variables data dictionary (XLSX file)](core_var_data_dict.xlsx)
   
   [Extract core variables from .tab files (R script)](var_extract_R.R)

   [Extract lifetime shiftwork metrics (R script)](extract_SW_metrics.R)

2. Calculating time & season variables
   Using date and location variables from the Biobank dataset, we have created the following code to calculate variables relating to time and seasons.

   [Biobank assessment centre latitudes (CSV file)](Table_latitude_assessment_centres.csv)

   [Biobank assessment centre historical weather (XLSX file)](weather_ukb_270923.xlsx)

   [Calculate photoperiod rate of change (R script)](extract_photoperiod_roc.R)

   [Extract assessment centre weather (R Markdown script)](Get_weather_data.Rmd)
   
3. Extract relevant Biobank variables
   The following R scripts can be used to extract and wrangle variables which are relevant for different time dimensions projects.

   **Blood biochemistry variables**
   
   [Blood biochemistry variables data dictionary (XLSX file)](blood_biochemistry_data_dict.xlsx)

   [Extract blood biochemistry variables (R script)](get_blood_biochem.R)

   **Metabolomics variables**
   
   [Metabolomics variables data dictionary (XLSX file)](metabolomics_data_dict.xlsx)

   [Extract metabolomics variables (R script)](get_metabolomics.R)

   **Additional sleep data variables (Jones)**
   
   [Extract sleep variables (R script)](jones_sleep_data.R)  
   
