remove(list = ls())

library(rvest)
library(xml2)
library(dplyr)


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


result <- result %>% 
  group_by(parliament, chamber_number, session) %>%
  dplyr::mutate(bill_number = 1:n())


bill_events <- setNames(data.frame(matrix(ncol = 13, nrow = 0)), 
                        c("title", "bill_number","event_description", "observation_number", "parliament_number",
                          "session_number", "chamber_number","bill_number",
                          "event_number", "member_name", "event_date", "event_chamber", "document_url"))

heading <- 'https://services.parliament.uk'

for(i in 1:nrow(result)){
  first <- result[i,]
  url <- as.character(first$url)
  
  webpage <- read_html(url)
  
  further <- webpage %>%
    html_node(xpath = '//*[@id="secondary-navigation"]/ul/li[3]/ul/li/ul/li[1]/a') %>%
    html_attr('href')
  
  further <- paste0(heading, further, seperate = '')
  
  version <- read_html(further) %>%
    html_node(xpath = '//*[@id="bill-summary"]/table/tbody') %>%
    html_nodes('td.bill-item-description')
  
  event <- version %>% html_text()
  event <- gsub("  |\r\n", "", event)
  time <- version %>% html_nodes('span') %>% html_text()
  document <- version %>% html_node('a') %>% html_attr('href')
  
  for(k in 1 : length(event)){
  new_row <- data.frame(title = first$bill_title, event_description = event[k], observation_number = NA, parliament_number = first$parliament,
                        session_number = first$session, chamber_number = first$chamber_number, bill_number = first$bill_number, event_number = k, member_name = first$member_name,
                        event_date = time[k], event_chamber = NA, document_url = document[k])
  
  bill_events <- rbind(bill_events, new_row)
                          
  }
}

bill_events <- bill_events %>%
  dplyr::mutate(observation_number = 1:n())

bill_events[bill_events$chamber_number == 1, ]$event_chamber <- 'House of Commons'
bill_events[bill_events$chamber_number == 2, ]$event_chamber <- 'House of Lords'


bill_events <- mutate(bill_events, parliament_path = paste0("/parliament-", parliament_number)) 

bill_events <- mutate(bill_events, session_path = paste0(parliament_path, "/session-", session_number)) 

bill_events <- mutate(bill_events, chamber_path = paste0(session_path, "/chamber-", chamber_number)) 

bill_events <- mutate(bill_events, bill_path = paste0(chamber_path, "/bill-", bill_number)) 

bill_events <- mutate(bill_events, event_path = paste0(bill_path, "/event-", event_number)) 

bill_events <- mutate(bill_events, observation_path = event_path)

write.csv(bill_events, "bill_events.csv")



#Text only 

# prefix <- 'https://hansard.parliament.uk/'
# 
# setwd()
# 
# for(i in 1: nrow(bill_events)){
#   
#   html1 <- as.character(bill_events[i, ]$document_url)
#   
#   page <- read_html(html1)
#   
#   tryCatch({
#     text_only <- page %>% html_node(xpath = '//*[@id="main"]/div[2]/div[1]/div[1]/div[1]/div/ol/li[3]/div/a') %>%
#       html_attr('href')
#     
#     download_url <- paste0(prefix, text_only)
#     download.file(download_url, destfile = paste0('/Users/mark_alfie/Desktop/UK/', bill_events[i, ]$event_description)) 
#     #You need to fill in your own destfile parameter, where the downloaded file will be saved.  
#     },error = function(e) {message(e)}, warning = function(se) {message(se)})
#   
#   
# }

