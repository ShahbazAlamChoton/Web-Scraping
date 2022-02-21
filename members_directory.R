rm(list = ls())
graphics.off()

dir<- "E:\\shahbaz\\R_files\\web_scraping"
setwd(dir)
# install.packages("rvest")
# install.packages("rebus")

library(tidyverse)  
library(lubridate)
library(stringr)  
# Parsing of HTML/XML files  
library(rvest)    
 # Verbose regular expressions
library(rebus)
library(writexl)

url<- 'http://www.aepcindia.com/members/'
# url<- 'http://www.aepcindia.com/members/?pageno=157%20%3E157'
# url<- 'http://www.aepcindia.com/members/?page=1'

pages_data<- read_html(url) %>%
  html_nodes('.pager-current') %>%
  html_text()
pages_data[length(pages_data)-1]%>%
  unname() %>%
  as.numeric()
length(pages_data)


# get_last_page <- function(html){
#   
#   pages_data <- html %>% 
#     # The '.' indicates the class
#     html_nodes('.pager-current') %>% 
#     # Extract the raw text as a list
#     html_text()                   
#   
#   # The second to last of the buttons is the one
#   pages_data[(length(pages_data)-1)] %>%            
#     # Take the raw string
#     unname() %>%                                     
#     # Convert to number
#     as.numeric()                                     
# }

first_page <- read_html(url)
# (latest_page_number <- get_last_page(first_page))
latest_page_number<- 157

list_of_pages <- str_c(url, '?pageno=', 1:latest_page_number)

get_market_title <- function(html){
  html %>% 
    # The relevant tag
    html_nodes('.market_title') %>%      
    html_text() %>% 
    # Trim additional white space
    str_trim() %>%                       
    # Convert the list into a vector
    unlist()                             
}

get_market_person<- function(html){
  html %>%
    html_nodes('.market_person') %>%
    html_text() %>%
    str_trim()%>%
    unlist()
}

get_market_address<- function(html){
  html%>%
    html_nodes('.market_address') %>%
    html_text()%>%
    str_trim()%>%
    unlist()
}

get_market_city_pin<- function(html){
  html%>%
    html_nodes('.market_city_pin') %>%
    html_text() %>%
    str_trim() %>%
    unlist()
}

get_market_state<- function(html){
  html %>%
    html_nodes('.market_state')%>%
    html_text() %>%
    str_trim() %>%
    unlist()
}

get_market_email<- function(html){
  html%>%
    html_nodes('.market_email') %>%
    html_text()%>%
    str_trim()%>%
    unlist()
}

get_data_table<- function(html){
  market_title<- get_market_title(html)
  # market_person<- get_market_person(html)
  market_address<- get_market_address(html)
  market_city_pin<- get_market_city_pin(html)
  market_state<- get_market_state(html)
  market_email<- get_market_email(html)
  # combine into a tibble(
  
  combined_data<- tibble(
    Title = market_title,
    # Person = market_person,
    Address = market_address,
    City_Pin = market_city_pin,
    State =  market_state,
    Email = market_email
  )
  
}
get_data_from_url<- function(url){
  html<- read_html(url)
  get_data_table(html)
}

scrap_write_table<- function(url){
  list_of_pages <- str_c(url, '?pageno=', 1:latest_page_number)
  
  list_of_pages %>%
    map(get_data_from_url) %>%
    bind_rows()%>%
    write_xlsx('members_directory.xlsx')
    
  
}

start.time <- Sys.time()
scrap_write_table(url = 'http://www.aepcindia.com/members/')
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
