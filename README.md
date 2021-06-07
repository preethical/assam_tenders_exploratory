# assam_tenders_exploratory  

This repository stores all information regarding the public contracting information from Assam.

## Table of Contents  

- **Background** - We recieved 5 years worth of public procurement data from Assam and we are trying to identify insights from this dataset. 
In order to this, we first need to load, clean and prep the data. 
- **Methodology**  
    - Once you recieve the data. Saves the files in your working directory and clone and run the **Assam_tenders.R file**. This file will load the datasets, convert all the dates to the date-time format, clean up all the column names and then merge the different years into 4 files - published tenders, AOC tenders, inprogress, and stopped. Split the org chain into organization/Role and add a published year.  
    - Exploratory analysis - Once this is completed, run the **assam_proc_explore.rmd** file to see an ios ppt of the data and the initial exploration with the data. This file also contains the code to write the merge file to be used next - assam_merge.csv  
    - Shiny APP: The **App.R** is a file with UI & Server files in the Assam_tenders folder and running it along with the [assam_merge.csv](https://drive.google.com/drive/u/0/folders/1NOOBMgr-0P_Gu3NMd89BSIk07_7ALo13) will result in a simple dashboard which takes organization names as input and gives the organizations performance across tender category and year as well as a table with the data.  
    - Assam_tenders_census.rmd is a r markdown file with different ways to merge the the assam public procurement data (assam_tenders_published) with the census information(census combined). Both files can be found [here](https://drive.google.com/drive/u/0/folders/1NOOBMgr-0P_Gu3NMd89BSIk07_7ALo13)
    - Assam_health - extracts health public procurements from the assam_tenders_published file. 
 
 ### Next steps     
 - Add comparators between organizations  
 - Add significance of change noted in KPI over time  
 - Clean up plots so they are within the given size  
 - Put it up on a server.   
