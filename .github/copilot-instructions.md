# Read the file 
Read the file located in the folder /data/locations_of_study_sites_train.csv 

# Do the following steps to make edits to the above file
- Use only the columns `traits` and `site_name`.
 For each of the names in the trait column, filter out the unique names in the `site_name` column, and for each site name, make variable called "url" which will have entries as " https://ecosis.org/package/[site_name]" 
- Make a .csv file titled "datasets.csv", which will have the columns "trait", "site_name", and "url", but no repeating site_name value for a particular trait.
- Save the .csv file in /data folder

