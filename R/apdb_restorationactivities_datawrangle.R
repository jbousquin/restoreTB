#####Script to help select completed Tampa Bay Action activities
#### that fall within the 4 watersheds of interest
#### 1. Read in two data sets, one with project descriptive info the other with a list of completed projects
###  2. Combine the data sets on shared field (HeaderID)
###  3. Subset the data to the sub-basins of interest
###  4. Find data in the project descriptions website not present in the completed projects data
###  5. Write both out to data-raw
###  6. Script used to create the following two files:
###     "data-raw/CompletedTBData_fromScript.csv")
###     "data-raw/MissingData_fromScript.csv"
###    which were then edited by Ed Sherwood to categorize water treatment projects
####Author: Katie Ireland 07/18/2017

###load required libraries

library(tidyverse)
library(sqldf)

#set up file names
restorefile<-file.path("data-raw/APDB_Projects_Header_Information_for_all_Projects-1.csv")  #name of file containing restoration activities
completedfile<-file.path("data-raw/APDB_Completed_Projects_Dump_07172017.csv") #name of file with completed restoration projects


#####READ IN THE DATA SETS######

#read in the file of restoration projects completed to date
completed<-read_csv(completedfile)  #read in the csv
completed<-rename(completed, HeaderID =Header_ID) #change Header_ID field to HeaderID 
#so that we can join this table to the restoration activities table on their common field, HeaderID.  

#read in the file with details on each restoration project
restore_cols <- colnames(read_csv(restorefile, n_max = 1)) #get the column names within plots to look up

restore_scols <- c("HeaderID","ProjectName","OngoingInitiation","DiscontinuedDate","CompletionDate","ActualProjectCost","FundingSource",
               "ProjectDescriptionText","NonPointProject","PointProject","ProjectLatitude","ProjectLongitude") #and create a list of the column you want to reduce csv load time

data<-read_csv(restorefile) %>% select(restore_scols)


################DONE READING IN DATA #####################

# join the completed projects to the descriptions of those projects. 
complete_desc <-left_join(completed,data,by="HeaderID") 

# subset to subbasins of interest
subset <- filter(complete_desc,Bay_Segment == "Hillsborough Bay" |Bay_Segment == "Middle Tampa Bay"
                 | Bay_Segment == "Old Tampa Bay" | Bay_Segment == "Lower Tampa Bay" | Bay_Segment == "All Segments" 
                 | Bay_Segment == "Not Entered") 

# find any projects in the header id file that are not in the completed projects file
unmatched <- anti_join(data,completed, by="HeaderID")

## write the data sets out 

write_csv(subset, "data-raw/CompletedTBData_fromScript.csv")
write_csv(unmatched, "data-raw/MissingData_fromScript.csv")



