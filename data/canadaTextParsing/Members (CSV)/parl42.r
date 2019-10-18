remove(list = ls())
library("tidyverse")
library("dplyr")

setwd("~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Members (CSV)")
##Extract Data 
parl42 <- read.csv("canada_commons_members_parl42.csv",encoding = "UTF-8", stringsAsFactors = F)
parlbefore <- read.csv("canada_constituencies.csv", encoding ="UTF-8", stringsAsFactors = F)

##Select 42nd constituency and Province from the 42nd parliament 

constituency42 <- select(parl42, Constituency:Province...Territory)
names(constituency42) <- c("Constituency_name", "Province")
constituencybefore <- select(parlbefore, constituency_name:province_name)
names(constituencybefore) <- c("Constituency_name", "Province")

#Remove duplicates and create new constituencies 
consTotal <- rbind(constituency42, constituencybefore)%>%
  distinct()%>%
  arrange(Constituency_name)%>%
  mutate(number = 1:458)%>%
  mutate(constituency_path = paste0("/chamber-1/constituency-", number))%>%
  mutate(chamber_name = "House of Common")


#stack all the ministries 
setwd("~/Documents/GitHub/CompLegFall2019/data/canadaTextParsing/Ministries (CSV)")

harper <- read.csv("canada_commons_ministry_harper.csv",encoding = "UTF-8", stringsAsFactors = F)
martin <- read.csv("canada_commons_ministry_martin.csv",encoding = "UTF-8", stringsAsFactors = F)
trudeau <- read.csv("canada_commons_ministry_trudeau.csv",encoding = "UTF-8", stringsAsFactors = F)
ministry <- rbind(harper, martin, trudeau)


#create ministry_num
ministry_num <- ministry %>%
  select(Title) %>%
  distinct() %>%
  arrange(Title)%>%
  mutate(ministry_number = 1:124)%>%
  mutate(ministry_path = paste0("/ministry-", ministry_number))

#using member IDs created by LU
member <- read.csv("canada_members_updated.csv",encoding = "UTF-8", stringsAsFactors = F)

#create the first part of ministries.csv
ministry <- ministry %>%
  mutate(full_name = paste(First.Name, Last.Name)) %>%
  merge(member, by = "full_name")

new_ministry <- select(ministry,First.Name, Last.Name, full_name, observation_path,constituency_name,constituency_name, Title,constituency_ID, start_date, end_date)

new_ministry <- new_ministry %>%
  merge(ministry_num, by = "Title")%>%
  arrange(full_name)%>%
  mutate(protfolio_number = 1:233)


new_ministry<-new_ministry %>%
  arrange(Last.Name, First.Name, constituency_name)%>%
  group_by(ministry_number) %>%
  mutate(minister_number = row_number()) %>%
  mutate(minister_path = paste0("/ministry-", ministry_number, "/minister-", minister_number)) %>%
  mutate(portfolio_path = paste0(minister_path, "/portfolio-", protfolio_number))%>%
  mutate(observation_path = portfolio_path)%>%
  select(observation_path, ministry_path, minister_path, portfolio_path, ministry_number, minister_number, member_name = full_name, member_ID = ministry_number, constituency_ID = constituency_name, profolio_name = Title,start_date, end_date)
  
write.csv(new_ministry, file = "canada_ministries.csv")
