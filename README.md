# Wolf-Valuation
Code and data for Hoag et al. (2023), Willingness to pay for reintroducing wolves in a divided voting base.  
The raw survey data is the data set called "raw_data.csv". To clean the raw survey data, run the code "import_clean_export_to_R.do" in Stata. This file cleans the survey data and creates the file "cleaned_data_for_R.csv". The primary analysis is done in R using the "analysis.final.R" script. This file imports the "cleaned_data_for_R.csv" file and runs several mixed logit models, calculates WTP, and produces figures. It also produces the WTP data for Table 5. Run "wtp_analysis.do" to produce Table 5. 
