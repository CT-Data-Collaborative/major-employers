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
ME_data_2016 <- (read_excel(paste0(path_to_raw_data, "/", ME_xlsx), sheet=1, skip=0))

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

# Write to File
write.table(
  ME_complete_2014_fips,
  file.path(getwd(), "data", "major_employers_2014.csv"),
  sep = ",",
  na = "-9999", 
  row.names = F
)

#any(is.na(ME_data_2014_fips))

# #Convert to long format
# ME_complete_long <- data.frame(stringsAsFactors = F)
# # iterate through pairs of columns (ie get the name and value for rank 1 GL entry, then rank 2, etc)
# for(i in 3:7) {
#   cols = c(1, 2, i)
#   ranked <- ME_complete[, cols]
#   names(ranked) <- c("Year", "Town", "Value")
#   ranked$Rank <- i - 2
#   ME_complete_long <- rbind(ME_complete_long, ranked)
#   remove(ranked, cols)
# }
# 
# 
# #Bring back in (old data) from 2014
# legacy_ME <- dir(path_to_raw_data, recursive=T, pattern = "2014") 
# old_data <- read.csv(paste0(path_to_raw_data, "/", legacy_ME), header=T, stringsAsFactors = F)
# old_data <- old_data[old_data$Year == 2014,]
# old_data <- old_data[!old_data$Value == "",]
# replace_data <- old_data[old_data$Town %in% ME_na,]
# replace_data$Variable <- gsub("Major Employer ", "", replace_data$Variable)
# names(replace_data)[names(replace_data) == "Variable"] <- "Rank"
# 
# #Merge in FIPS
# replace_data_fips <- merge(replace_data, fips, by = "Town", all.x=T)
# 
# #Add Measure Type and Variable
# replace_data_fips$"Measure Type" <- "Name"
# replace_data_fips$"Variable" <- "Employer"
# 
# replace_data_fips <- replace_data_fips %>% 
#   select(`Town`, `FIPS`, `Year`, `Rank`, `Variable`, `Measure Type`, `Value`) %>% 
#   arrange(Town, Variable, Rank)
# 
# #Combine old and new data
# ME_data_combined <- rbind(ME_complete_long_fips, replace_data_fips)
# ME_data_combined <- arrange(ME_data_combined, Town, Variable, Rank)
# 
# #find towns without complete top 5 (or gave non-descriptive answers)
# ME_incomplete <- unique(ME_data_combined[is.na(ME_data_combined$`Value`),]$Town)
# 
# #non-descript (show all ranks as NA) 
# #need to call these out individually, no way of knowing that "We are a beach town." 
# #is a non-descript answer rather than an actual employer. 
# non_descript <- c("Easton", "Old Lyme")
# ME_data_combined$"Value"[which(ME_data_combined$Town %in% non_descript)] <- -6666
# 
# #all other NAs are from towns with incomplete ranks (less than 5) (set missing to -6666)
# #("Redding", "Roxbury", "Sharon", "Warren")
# 
