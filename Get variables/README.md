# Get Variables folder

The 'Get Variables' folder contains all the code needed to get you started with exploring time dimensions in UK Biobank data.

### **1. Extracting 'core' variables**
   
   We have identified a set of core variables from the Biobank dataset which we think are essential to any time dimensions analysis project. These include variables such as demographics and geographic location.

   * [Core variables field list for data fetching (.txt file)](field_list.txt)

   * [Core variables data dictionary (XLSX file)](core_var_data_dict.xlsx)
   
   * [Extract core variables from .tab files (R script)](var_extract_R.R)

   * [Extract lifetime shiftwork metrics (R script)](extract_SW_metrics.R)

### **2. Calculating time & season variables**
   
   Using date and location variables from the Biobank dataset, we have created the following code to calculate variables relating to time and seasons.

   * [Biobank assessment centre latitudes (CSV file)](Table_latitude_assessment_centres.csv)

   * [Biobank assessment centre historical weather (XLSX file)](weather_ukb_270923.xlsx)

   * [Calculate photoperiod rate of change (R script)](extract_photoperiod_roc.R)

   * [Extract assessment centre weather (R Markdown script)](Get_weather_data.Rmd) - This is code that extracts Met Office weather data (monthly mean temperature) for the assessment centre visit
   
### **3. Extract relevant Biobank variables**
   
   The following R scripts can be used to extract and wrangle variables which are relevant for different time dimensions projects.

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
