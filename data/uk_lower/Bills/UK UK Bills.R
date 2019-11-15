# remove objects
rm(list=ls())
# detach all libraries

detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}

detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

lapply(c("stringr", "dplyr", "plyr", "tidyverse", "rvest", "zoo", "lubridate"), pkgTest)

library(dplyr)

# working directoy
setwd("~/Documents/GitHub/CompLegFall2019/data/uk_lower/")



download_csv <- function(type, maxPages, fileName){
  for(i in 0:maxPages) {
    # make URL
    url <- str_c("http://lda.data.parliament.uk/", type,".csv?_pageSize=500&_page=", i, collapse = "")
    # make file name
    file <- str_c(getwd(), "/", fileName, "/", type, "_page_", i, ".csv", collapse = "")
    # download file
    tryCatch(download.file(url, file, quiet = TRUE), error = function(e) print(paste(file, 'questions missing')))
    
    # random delay
    Sys.sleep(runif(1, 0, 0.15))
  }
}


#download_csv("bills", 4, "bills")
setwd("~/Documents/GitHub/CompLegFall2019/data/uk_lower/Bills")

result <- read.csv("bills_page_0.csv")

result$identifier <-NULL


for(i in 1:4){
  file <- str_c("bills_page_", i,".csv", collpase = "")
  interm <- read.csv(file)
  tryCatch({interm$identifier <- NULL}, warnings =function(e) print(paste(file, 'no identifier')))
  result <- rbind(result,interm)
}


####Rename the column 
names(result)
result <- result %>% 
  select(date, bill.type, title, sponsors...sponsor.printed, session...display.name)

result <- dplyr::rename(result, date_introduced = date, bill_type = bill.type, bill_title = title, member_name = sponsors...sponsor.printed, session = session...display.name )
names(result)


####Session and House

result$session <- as.character(result$session)

result$parliament <- NA

result[result$session == "2005-2006", ]$parliament <- 54
result[result$session == "2006-2007", ]$parliament <- 54
result[result$session == "2007-2008", ]$parliament <- 54
result[result$session == "2008-2009", ]$parliament <- 54
result[result$session == "2009-2010", ]$parliament <- 55
result[result$session == "2010-2012", ]$parliament <- 55
result[result$session == "2012-2013", ]$parliament <- 55
result[result$session == "2013-2014", ]$parliament <- 55
result[result$session == "2014-2015", ]$parliament <- 56
result[result$session == "2015-2016", ]$parliament <- 56
result[result$session == "2016-2017", ]$parliament <- 56
result[result$session == "2017-2019", ]$parliament <- 57
result[result$session == "", ]$parliament <- NA

result$session <-as.factor(result$session)

levels(result$session)

result$chamber_number <- NA

result$chamber_number[str_detect(result$bill_title, "HL")] <- 2
result$chamber_number[is.na(result$chamber_number)] <- 1

####Name, constituency

# result$member_name <- str_replace_all(result$member_name, "Mr ", "")
# result$member_name <- str_replace_all(result$member_name, "Ms ", "")
# result$member_name <- str_replace_all(result$member_name, "Mrs ", "")
# result$member_name <- str_replace_all(result$member_name, "Sir ", "")
# result$member_name <- str_replace_all(result$member_name, "Dr ", "")

members <- read.csv("uk_lower_members.csv")

final <- merge(result, members, by.x = "member_name", by.y = "full_name")

final <- final %>%
  select(date_introduced, bill_type, bill_title, member_name, member_id = member_number, constituency_name, constituency_ID, chamber_number = chamber_number.y, parliament)

final <- final[order(final$date_introduced), ]

final <- final %>%
  mutate(observation_number = 1:length(final$member_name))
  
  final <- mutate(final, parliament_path = paste0("/parliament-", parliament)) 
  
  final <- mutate(final, chamber_path = paste0(final$parliament_path, "/chamber-", chamber_number)) 
  final <- mutate(final, observation_path = paste0(final$chamber_path, "/bill-", observation_number))
  final <- mutate(final, bill_path = final$observation_path)

  
write_csv(final, "bill.csv")

         
    


