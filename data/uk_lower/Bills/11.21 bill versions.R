remove(list = ls())

library(rvest)
library(xml2)
library(dplyr)

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
  select(date, bill.type, title, sponsors...sponsor.printed, session...display.name, home.page...uri)



result <- dplyr::rename(result, date_introduced = date, bill_type = bill.type, bill_title = title, member_name = sponsors...sponsor.printed, session = session...display.name, url = home.page...uri )
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


###

result <- result %>% 
  group_by(parliament, chamber_number, session) %>%
  dplyr::mutate(bill_number = 1:n())


bill_versions <- setNames(data.frame(matrix(ncol = 11, nrow = 0)), 
                        c("date_introduced", "date_version","bill_type", "bill_title", "version",
                          "parliment_number","session_number", "chamber_number","bill_number",
                          "version_number", "document_url"))

prefix <- 'https://services.parliament.uk/'

for(i in 1:nrow(result)){
  first <- result[i,]
  url <- as.character(first$url)
  
  tryCatch({
    
    webpage <- read_html(url)
    
    version <- webpage %>% html_node(xpath = '//*[@id="secondary-navigation"]/ul/li[3]/ul/li/ul/li[2]/a') %>%
      html_attr('href')
    further <- read_html(paste0(prefix, version))
    
    document_name <- further %>% html_node(xpath = '//*[@id="bill-summary"]') %>%
      html_nodes('table.bill-items') %>% html_nodes('td.bill-item-description') %>% html_node('a') %>% html_text()
    
    document_link <- further %>% html_node(xpath = '//*[@id="bill-summary"]') %>%
      html_nodes('table.bill-items') %>% html_nodes('td.bill-item-description')
    
    document_date <- further %>% html_node(xpath = '//*[@id="bill-summary"]') %>%
      html_nodes('table.bill-items') %>% html_nodes('td.bill-item-date') %>% html_text()
    
    #create list 
    final_link <- list()
    
    for(p in 1:length(document_link)) {
      
      key <- document_link[p] %>% html_node('span.application-pdf')
      
      if(is.na(key)){
        key <- document_link[p] %>% html_node('a') %>% html_attr('href')
        final_link[p] <- key
        
      } else {
        
        key <- key %>% html_node('a')
        
        if(!is.na(key)){
          
          key <- key %>% html_attr('href')
          
        } else {
          
          key <- document_link[p] %>% html_node('a') %>% html_attr('href')
          
        }
        
        final_link[p] <- key
        
      }
      
      
    }
    
    for(o in 1:length(document_name)){
      
      new_row <- data.frame(date_introduced = first$date_introduced , date_version = document_date[o], bill_type = first$bill_type, bill_title = first$bill_title, version = document_name[o] ,
                            parliment_number = first$parliament, session_number = first$session, chamber_number = first$chamber_number, bill_number = first$bill_number,
                            version_number = o , document_url = final_link[[o]])
      
      
      bill_versions <- rbind(bill_versions, new_row)
    }
    
    
  }, error = function(e) {message(e)}, warning = function(se) {message(se)})
  
}
