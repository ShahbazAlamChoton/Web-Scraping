
rm(list = ls())
graphics.off()

library(tidyverse)  
library(lubridate)
library(stringr)  
# Parsing of HTML/XML files  
library(rvest)    
# Verbose regular expressions
library(rebus)

library(RMySQL)
# creating DB connection object with RMysql package
DB <- dbConnect(MySQL(), user="____", password="____",
                dbname="____", host="____") #7.185.69.27 192.168.64.16


source("sqlQuery.R")

#end of global declaration
Sys.setenv(TZ = "Asia/Dhaka")

print_time = function(interval = 60) {
  if(Sys.time()>"2021-09-27 14:35:00 +06")
  {
    #break
    stop("Time up")
  }
  timestamp()
  later::later(print_time, interval)
  scrapping_date<- format(Sys.time(), "%d-%b-%Y")
  
  url<- 'https://www.dsebd.org/latest_share_price_scroll_by_ltp.php'
  
  tbl_company <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="RightBody"]/div[1]/div/div[1]/table') %>%
    html_table(fill = TRUE)
  
  dse_all_company_stock_data<- tbl_company[[1]] 
  
  library(lubridate)
  priceData <- dse_all_company_stock_data %>% 
    mutate(ticker = trimws(`TRADING CODE`),
           high = as.numeric(trimws(gsub(",", "", HIGH))),
           low = as.numeric(trimws(gsub(",", "", LOW))),
           ltp = as.numeric(trimws(gsub(",", "", `LTP*`))),
           ycp = as.numeric(trimws(gsub(",", "", `YCP*`))),
           trade = as.numeric(trimws(gsub(",", "", TRADE))),
           volume = as.numeric(trimws(gsub(",", "", VOLUME))),
           value = as.numeric(trimws(gsub(",", "", `VALUE (mn)`))) * 10^6
    ) %>% 
    mutate(pchange = ((ltp - ycp)/ycp) * 100,
           date=Sys.time()) %>% 
    select(date,ticker,ltp,high,low,ycp,pchange,trade,volume,value) %>% 
    arrange(ticker)
  
  
  dbWriteTable(DB, name = "minutedata", value = priceData, append = TRUE, row.names = FALSE)
  
}

print_time()

