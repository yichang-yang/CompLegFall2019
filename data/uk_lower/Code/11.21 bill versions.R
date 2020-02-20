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







