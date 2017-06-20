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

#bring in 2014 data only 
legacy_ME <- dir(path_to_raw_data, recursive=T, pattern = "2014") 
ME_data_2014 <- read.csv(paste0(path_to_raw_data, "/", legacy_ME), header=T, stringsAsFactors = F)
ME_data_2014 <- ME_data_2014[ME_data_2014$Year == 2014,]
ME_data_2014 <- ME_data_2014[ME_data_2014$Value != "",]
ME_data_2014$Variable <- gsub("Major Employer ", "", ME_data_2014$Variable)
names(ME_data_2014)[names(ME_data_2014) == "Variable"] <- "Rank"

#Now bring in updated 2014 data from municipalities
updated_2014 <- (read_excel(paste0(path_to_raw_data, "/", ME_xlsx), sheet=2, skip=0))
updated_2014 <- updated_2014[!is.na(updated_2014$`1`) & !is.na(updated_2014$`2`),]

#remove #employees columns
emp_cols <- grep("Employees", colnames(updated_2014))
updated_2014 <- updated_2014[,-emp_cols]
updated_2014$Year <- 2014
#Convert to long format
cols_to_stack <- c("1", "2", "3", "4", "5")

long_row_count = nrow(updated_2014) * length(cols_to_stack)

updated_2014_long <- reshape(updated_2014,
                              varying = cols_to_stack,
                              v.names = "Value",
                              timevar = "Rank",
                              times = cols_to_stack,
                              new.row.names = 1:long_row_count,
                              direction = "long"
)

updated_2014_long$id <- NULL

#Convert town names to correct case
updated_2014_long$Town <- stri_trans_totitle(updated_2014_long$Town)

replace_towns <- unique(updated_2014_long$Town)

ME_data_2014 <- ME_data_2014[!ME_data_2014$Town %in% replace_towns,]

#Bind original 2014 with updated 2014
ME_complete_2014 <- rbind(ME_data_2014, updated_2014_long)

#Merge in FIPS
town_fips_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-town-list/master/datapackage.json'
town_fips_dp <- datapkg_read(path = town_fips_dp_URL)
fips <- (town_fips_dp$data[[1]])

ME_complete_2014_fips <- merge(ME_complete_2014, fips, by = "Town", all=T)
#remove CT and is.na(year) rows
ME_complete_2014_fips <- ME_complete_2014_fips[!is.na(ME_complete_2014_fips$Year),]

#Add Measure Type and Variable
ME_complete_2014_fips$"Measure Type" <- "Name"
ME_complete_2014_fips$"Variable" <- "Employer"
ME_complete_2014_fips$`Year Submitted` <- ME_complete_2014_fips$Year
ME_complete_2014_fips$`Town Profile Year` <- 2016

ME_complete_2014_fips <- ME_complete_2014_fips %>% 
  select(`Town`, `FIPS`, `Year`, `Year Submitted`, `Town Profile Year`, `Rank`, `Variable`, `Measure Type`, `Value`) %>% 
  arrange(Town, Variable, Rank)

#bring in new data (for 2017 profiles)
ME_data_2017 <- (read_excel(paste0(path_to_raw_data, "/", ME_xlsx), sheet=1, skip=0))
#remove towns with no update
ME_data_2017 <- ME_data_2017[!is.na(ME_data_2017$`Date Received`),]
emp_cols <- grep("Employees", colnames(ME_data_2017))
ME_data_2017 <- ME_data_2017[,-emp_cols]
ME_data_2017$`Year Submitted` <- year(ME_data_2017$`Date Received`)
ME_data_2017$`Date Received` <- NULL
ME_data_2017$Year <- (ME_data_2017$`Year Submitted` - 1)
ME_data_2017$`Town Profile Year` <- ME_data_2017$`Year Submitted`

ME_data_2017 <- ME_data_2017 %>% 
  select(`Town`, `Year`, `Year Submitted`, `Town Profile Year`, `1`, `2`, `3`, `4`, `5`) 

#Convert to long format
ME_2017_long <- data.frame(stringsAsFactors = F)
# iterate through pairs of columns (ie get the name and value for rank 1 GL entry, then rank 2, etc)
for(i in 5:9) {
  cols = c(1, 2, 3, 4, i)
  ranked <- ME_data_2017[, cols]
  names(ranked) <- c("Town", "Year", "Year Submitted", "Town Profile Year", "Value")
  ranked$Rank <- i - 4
  ME_2017_long <- rbind(ME_2017_long, ranked)
  remove(ranked, cols)
}

#towns names
ME_2017_long$Town <- stri_trans_totitle(ME_2017_long$Town)

#Merge in FIPS
ME_2017_long_fips <- merge(ME_2017_long, fips, by = "Town", all=T)

#remove CT 
ME_2017_long_fips <- ME_2017_long_fips[ME_2017_long_fips$Town != "Connecticut",]

#Add Measure Type and Variable
ME_2017_long_fips$"Measure Type" <- "Name"
ME_2017_long_fips$"Variable" <- "Employer"

#Find which towns do not have 2017 data, and copy 2016 data to 2017
no_2017 <- unique(ME_2017_long_fips[is.na(ME_2017_long_fips$`Year`),]$Town)
#subset 2014 dataset based on these towns
bring_from_2016 <- ME_complete_2014_fips[ME_complete_2014_fips$Town %in% no_2017,]
bring_from_2016$`Town Profile Year` <- 2017

#bind ME_2017_long_fips and bring_from_2016
ME_2017_long_fips <- ME_2017_long_fips[!is.na(ME_2017_long_fips$Year),]

ME_2017_long_fips <- ME_2017_long_fips %>% 
  select(`Town`, `FIPS`, `Year`, `Year Submitted`, `Town Profile Year`, `Rank`, `Variable`, `Measure Type`, `Value`) 

ME_complete_2017_fips <- rbind(ME_2017_long_fips, bring_from_2016)

ME_complete_fips <- rbind(ME_complete_2014_fips, ME_complete_2017_fips)

#Towns with non-descript values 
#need to find these manually, no way of knowing that "We are a beach town."
#is a non-descript answer rather than an actual employer.
ME_incomplete <- unique(ME_complete_fips[is.na(ME_complete_fips$`Value`),]$Town)
ME_incomplete_df <- ME_complete_fips[ME_complete_fips$Town %in% ME_incomplete,]
#look through ME_incomplete_df to find any non-descript/ incorrect entries, 
#if towns have incomplete ranking (less than 5) that's ok
non_descript <- c("Easton", "Old Lyme")
#set these towns to their 2016 values
bring_from_2016_again <- ME_complete_2014_fips[ME_complete_2014_fips$Town %in% non_descript,]
bring_from_2016_again$`Town Profile Year` <- 2017
ME_complete_fips <- ME_complete_fips[!(ME_complete_fips$Town %in% non_descript & ME_complete_fips$`Town Profile Year` == 2017),]

ME_complete_fips <- rbind(bring_from_2016_again, ME_complete_fips)

ME_complete_fips <- ME_complete_fips %>% 
  select(`Town`, `FIPS`, `Year`, `Year Submitted`, `Town Profile Year`, `Rank`, `Variable`, `Measure Type`, `Value`) %>% 
  arrange(Town, `Town Profile Year`, Rank)

#set towns with less than 5 ranks, set rest to -9999
# Write to File
write.table(
  ME_complete_fips,
  file.path(getwd(), "data", "major_employers_2014_2016.csv"),
  sep = ",",
  na = "-9999", 
  row.names = F
)
