library(plyr)
library(dplyr)
library(datapkg)
library(readxl)
library(lubridate)
library(stringi)

##################################################################
#
# Processing Script for Major Employers
# Created by Jenna Daly
# On 05/22/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
data_location <- grep("raw", sub_folders, value=T)
path_to_raw_data <- (paste0(getwd(), "/", data_location))
ME_xlsx <- dir(path_to_raw_data, recursive=T, pattern = "MajorE") 
ME_data <- (read_excel(paste0(path_to_raw_data, "/", ME_xlsx), sheet=1, skip=0))

#remove #employees columns
emp_cols <- grep("Employees", colnames(ME_data))
ME_data <- ME_data[,-emp_cols]

#setup year column (rename and extract year from POSIXct)
ME_data <- plyr::rename(ME_data, c("Date Received"="Year"))

#Convert town names to correct case
ME_data$Town <- stri_trans_totitle(ME_data$Town)

#remove rows where year is NA (old data)
ME_complete <- ME_data[!is.na(ME_data$Year),]

#Find towns where theres no 2016 data
ME_na <- unique(ME_data[is.na(ME_data$`Year`),]$Town)
ME_comp <- unique(ME_data[!is.na(ME_data$`Year`),]$Town)

ME_complete$Year <- 2016

#Convert to long format
ME_complete_long <- data.frame(stringsAsFactors = F)
# iterate through pairs of columns (ie get the name and value for rank 1 GL entry, then rank 2, etc)
for(i in 3:7) {
  cols = c(1, 2, i)
  ranked <- ME_complete[, cols]
  names(ranked) <- c("Year", "Town", "Value")
  ranked$Rank <- i - 2
  ME_complete_long <- rbind(ME_complete_long, ranked)
  remove(ranked, cols)
}

#Merge in FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

ME_complete_long_fips <- merge(ME_complete_long, fips, by = "Town", all=T)
#remove CT and is.na(year) rows
ME_complete_long_fips <- ME_complete_long_fips[!is.na(ME_complete_long_fips$Year),]

#Add Measure Type and Variable
ME_complete_long_fips$"Measure Type" <- "Name"
ME_complete_long_fips$"Variable" <- "Employer"

ME_complete_long_fips <- ME_complete_long_fips %>% 
  select(`Town`, `FIPS`, `Year`, `Rank`, `Variable`, `Measure Type`, `Value`) %>% 
  arrange(Town, Variable, Rank)

#Bring back in (old data) from 2014
legacy_ME <- dir(path_to_raw_data, recursive=T, pattern = "2014") 
old_data <- read.csv(paste0(path_to_raw_data, "/", legacy_ME), header=T, stringsAsFactors = F)
old_data <- old_data[old_data$Year == 2014,]
old_data <- old_data[!old_data$Value == "",]
replace_data <- old_data[old_data$Town %in% ME_na,]
replace_data$Variable <- gsub("Major Employer ", "", replace_data$Variable)
names(replace_data)[names(replace_data) == "Variable"] <- "Rank"

#Merge in FIPS
replace_data_fips <- merge(replace_data, fips, by = "Town", all.x=T)

#Add Measure Type and Variable
replace_data_fips$"Measure Type" <- "Name"
replace_data_fips$"Variable" <- "Employer"

replace_data_fips <- replace_data_fips %>% 
  select(`Town`, `FIPS`, `Year`, `Rank`, `Variable`, `Measure Type`, `Value`) %>% 
  arrange(Town, Variable, Rank)

#Combine old and new data
ME_data_combined <- rbind(ME_complete_long_fips, replace_data_fips)
ME_data_combined <- arrange(ME_data_combined, Town, Variable, Rank)

# Write to File
write.table(
  ME_data_combined,
  file.path(getwd(), "data", "major_employers_2016.csv"),
  sep = ",",
  row.names = F
)
