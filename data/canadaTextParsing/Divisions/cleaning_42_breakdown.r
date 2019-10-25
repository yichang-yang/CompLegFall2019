library(dplyr)
library(tidyverse)

setwd("/Users/mark_alfie/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Divisions")

bd42 <- read.csv("42_breakdown.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
bdelse<- read.csv("canada_divisions_breakdown.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
bd_single <- read.csv("42division.csv", encoding = "UTF-8", stringsAsFactors = FALSE)


interm <- select(bd_single, division_ID, date)

bd42 <- bd42 %>%
  mutate(key_ID = 585578:(585578+276247)) %>%
  select(key_ID, parliament_ID = ParliamentNumber, session_ID = SessionNumber, division_number = DecisionDivisionNumber,
         constituency = ConstituencyName, yea =Yea, nay =Nay, paired = Paired)%>%
  mutate(member_name = paste(bd42$FirstName, bd42$LastName, sep = " "))%>%
  mutate(division_ID = paste0(parliament_ID, "-",session_ID, "-", division_number))

bd42 <- bd42 %>%
  merge(interm, by = "division_ID") %>%
  bd42[, c(2,3,4,1,)]
  
  
  


