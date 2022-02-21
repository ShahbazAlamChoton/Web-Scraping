rm(list = ls())
graphics.off()

current_path <- rstudioapi::getSourceEditorContext()$path
setwd(dirname(current_path))
library(tidyverse)
library(lubridate)
library(stringr)  
# Parsing of HTML/XML files  
library(rvest)    
# Verbose regular expressions
library(rebus)
library(readxl)
library(RMySQL)
source('sqlQuery.R')
#=============================================== Main Code file Starts here================================

startDate <- "2021-01-01"
endDate <- "2021-01-10"



dailystatus <- sqlQuery(paste0("SELECT  a.Date,
        SUM(a.pchange > 0) `advance`,
        SUM(a.pchange < 0) `decline`,
        SUM(a.pchange = 0) `unchange`
FROM    tbloneyeardata a
WHERE a.date between '",startDate,"' and '",endDate,"' GROUP   BY a.Date"))


url<- paste0("https://www.dsebd.org/market_summary.php?startDate=",startDate, "&endDate=", endDate, "&archive=data")

tbl_company <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="RightBody"]/div[1]/div/table') %>%
  html_table(fill = TRUE)

for (i in seq_along(tbl_company)) {
  

index_all<- tbl_company[[i]] 

indexData <- index_all %>%
  mutate(trade = as.numeric(trimws(gsub(",", "", index_all[2,4]))), # to avoid comma separator
         volume = as.numeric(trimws(gsub(",", "", index_all[4,4]))),
         value = as.numeric(trimws(gsub(",", "", index_all[3,4]))),
         marketEquity = 0,
         marketCap = as.numeric(trimws(gsub(",", "", index_all[5,4])))*10^6,
         advance = dailystatus$advance[nrow(dailystatus)+1-i],
         decline = dailystatus$decline[nrow(dailystatus)+1-i],
         unchange = dailystatus$unchange[nrow(dailystatus)+1-i]
         
  ) %>%
  #mutate(date=as.Date(st_date)) 
  mutate(date=as.Date(dailystatus$Date[nrow(dailystatus)+1-i])) %>%
  slice(1) %>%
  select(date, trade, volume, value, marketEquity, marketCap, advance, decline, unchange) 

write.table(indexData, "indexData.csv", sep = ",", append = T, row.names = F, col.names = T)


#dbWriteTable(DB, name = "tblindex", value = indexData, append = TRUE, row.names = FALSE)

}
