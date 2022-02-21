rm(list = ls())
graphics.off()

current_path <- rstudioapi::getSourceEditorContext()$path
setwd(dirname(current_path))
getwd()
# install.packages("rvest")
# install.packages("rebus")

library(tidyverse)  
library(lubridate)
library(stringr)  
# Parsing of HTML/XML files  
library(rvest)    
# Verbose regular expressions
library(rebus)
# library(readxl )
library(RMySQL)
DB <- dbConnect(MySQL(), user="____", password="____",
                dbname="____", host="____") #7.185.69.27 192.168.64.16


source("sqlQuery.R")
startDate<-'2018-01-01'  # yyy-mm-dd

dseNews<- sqlQuery(paste0("select * from news where date >= '", startDate, "' and news like '%EPS was Tk.%' "))

epsLocation <- str_locate(dseNews$news, "EPS was Tk.")
forLocation <- str_locate(dseNews$news, " for")
eps <- trimws(substr(dseNews$news, epsLocation[, 2] + 1, forLocation[, 1]- 1))

monthLocation <- str_locate(dseNews$news, "as against")
month <-  trimws(substr(dseNews$news, forLocation[, 2] + 1, monthLocation[, 1] - 1))

convert.brackets <- function(x){
  if(grepl("\\(.*\\)", x)){
    paste0("-", gsub("\\(|\\)", "", x))
  } else {
    x
  }
}
epsraw <- data.frame(ticker = dseNews$ticker, EPS = eps, monthYear = month)

# epsCheck <- epsraw %>% 
#   subset(str_length(eps) == 5)
# 
# monthCheck <- epsraw %>% 
#   subset(str_length(month) == 12)

epsData <- data.frame(ticker = dseNews$ticker, EPS = eps, monthYear = month) %>% 
  subset(str_length(eps) < 10 & str_length(month) < 24 ) %>% 
  mutate(monthYear = trimws(monthYear),
         numEps = trimws(as.numeric(sapply(EPS, convert.brackets, USE.NAMES = F)))) %>% 
  mutate(year = as.numeric(trimws(substr(monthYear, str_length(monthYear)-3, str_length(monthYear)))),
         monthRange = substr(monthYear, 1, str_length(monthYear)-4)) %>% 
  filter(!is.na(year)) %>% 
  mutate(monthRange = trimws(gsub(",", " ", monthRange)))

# write.csv(epsData, "epsData.csv", row.names = F)
epsMonthCategory <- read.csv("eps_month_category.csv", header = TRUE)


db_var <- c("id", "ticker", "year_name", "segment", "period", "eps")

quarterlyEPS <- merge(epsData, epsMonthCategory, by.x = "monthRange", by.y = "Range", all.x = TRUE) %>% 
  filter(!(Category == 99)) %>% 
  select(ticker, numEps, year, monthRange, Category) %>% 
  rename(segment = Category, year_name = year, period = monthRange, eps = numEps) %>%
  mutate(id = paste0(ticker, year_name, segment)) %>%
  select(all_of(db_var))

# dbWriteTable(DB, name = "epsdata", value = quarterlyEPS, append = TRUE, row.names = FALSE)
