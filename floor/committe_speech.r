library(xml2)
library(XML)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(methods)
library(data.table)
library(stringr)
library(rvest)


remove(list = ls())


setwd("/Users/mark_alfie/Desktop/QTM\ 499R/floor")

committee <- c("ACVA", "AGRI","CHPC", "CIIT","CIMM", "ENVI","ERRE","ESPE","ETHI","FAAE","FEWO", "FINA","FOPO","HESA")

num <- c(123, 149, 163, 151, 167, 164, 57, 12, 160, 151, 149, 223, 152, 154)

#download speeches 

#for(i in 1 : length(committee)) {
     
#    for(j in 1 : num[i]){
#      url <- str_c("https://www.ourcommons.ca/DocumentViewer/en/42-1/", committee[i], "/meeting-",j,"/evidence", collapse = "")
#      
#      tryCatch( {
#        htm <- read_html(url)
#        scrap <- htm %>% html_nodes("btn.hidden-xs") %>% html_attr("href")
#        scrap1 <- str_c("https://www.ourcommons.ca", scrap, collapse = "")
#        file <- str_c("42-parliament-1-session", committee[i], "-meeting", j, ".xml", collapse = "")
        
#      }, error = function(e) {message(e)}, warning = function(se) {message(se)})
#      tryCatch(download.file(scrap1, file, quiet = TRUE),
#               error = function(e) print(paste(file, 'question missing')))
#      }
      
#    }

final <- xmlToDataFrame(x(file = "42-parliament-1-sessionACVA-meeting1.xml"))

for(i in 1 : length(committee)){
  
  for(j in 1 : num[i]) {
    
    tryCatch({ data <- paste("42-parliament-1-session", committee[i], "-meeting", j, ".xml", collapse = "")
    xmlfile <- xmlParse(file = data)
    interm <- xmlToDataFrame(xmlfile)
    final <- rbind(final, interm) }, error = function(e) {message(e)})

  
  }
}



