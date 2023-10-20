# Get Variables folder

The 'Get Variables' folder contains all the code needed to get you started with exploring time dimensions in UK Biobank data.

### **1. Create a core dataset**
   
   We have identified a set of core variables from the Biobank dataset which we think are essential to any time dimensions analysis project. These include Biobank variables such as demographics and geographic location,       and derived variables such as weather and photoperiod length.

   **Downloading data**
   
   * [Core variables field list for data fetching from basket 4055647(.txt file)](field_list.txt)
     > Use this field_list.txt file to download core variables and r encodings using 'ukbconv ukb673864.enc_ukb r -eencoding.dat -ifield_list.txt' command

   * [Core variables field list for data fetching from basket 4066697(.txt file)](need to insert)

   **Understanding the core variables**
   * [Core variables data dictionary (XLSX file)](core_var_data_dict.xlsx)
     > This data dictionary details all of the core Biobank & derived variables in the dataset

   **Create a core dataset called 'UKB_master' to use in your project**
   * [Extract core variables from .tab files (R script)](var_extract_R.R)
     > This script includes the following code/files already. You do not need to do anything apart from download them:
     >    * [Biobank assessment centre latitudes (CSV file)](Table_latitude_assessment_centres.csv)
     >    * [Calculate photoperiod rate of change (R script)](extract_photoperiod_roc.R)


### **2. Extract relevant Biobank variables**
   
   The following R scripts can be used to extract and wrangle variables which are relevant for different time dimensions projects.
   
   **Weather variables**
   
   This code extracts Met Office weather data (monthly mean temperature) for the assessment centre visit
   
   * [Biobank assessment centre historical weather (XLSX file)](weather_ukb_270923.xlsx)

   * [Extract assessment centre weather (R Markdown script)](Get_weather_data.Rmd)

   **Blood biochemistry variables**

   * [Blood biochemistry variables data dictionary (XLSX file)](blood_biochemistry_data_dict.xlsx)

   * [Extract blood biochemistry variables (R script)](get_blood_biochem.R)

   **Metabolomics variables**
   
   * [Metabolomics variables data dictionary (XLSX file)](metabolomics_data_dict.xlsx)

   * [Extract metabolomics variables (R script)](get_metabolomics.R)

   **Accelerometer-derived sleep data variables (Jones et al.)**

   This is code to process the activity monitor derived measures of sleep timing returned by Jones et al., calculated using GGIR. See https://biobank.ndph.ox.ac.uk/showcase/dset.cgi?id=1862 and 
   https://pubmed.ncbi.nlm.nih.gov/30696823.  
   
   * [Sleep variables data dictionary (XLSX file)](sleep_data_dict.xlsx)
     
   * [Extract sleep variables (R script)](jones_sleep_data.R)
      
   **Lifetime shiftwork variables**  
   
   This is code to extract metrics on shiftwork jobs throughout lifetime taken from online survey completed by 120k UK Biobank participants in 2015: https://biobank.ndph.ox.ac.uk/ukb/label.cgi?id=130  
   
   * [Shiftwork variables data dictionary (XLSX file)](shiftwork_data_dict.xlsx)
   
   * [Extract shiftwork variables (R script)](extract_SW_metrics.R)   
