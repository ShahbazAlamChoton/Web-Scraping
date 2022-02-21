rm(list = ls())
graphics.off()
current_path <- rstudioapi::getSourceEditorContext()$path
setwd(dirname(current_path))
getwd()
# dir<- "E:\\shahbaz\\R_files\\web_scraping"
# setwd(dir)
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

url<- 'https://www.dsebd.org/displayCompany.php?name=BATBC'

# pages_data<- read_html(url) %>%
#   html_nodes(xpath = '//th | //td') %>%
#   html_text() %>%
#   str_trim() %>%
#   unlist()

company_all_data<- read_html(url) %>%
  html_nodes('.table-responsive') %>%
  html_text() %>%
  str_trim() %>%
  unlist()

# first table

split_info1<- as.data.frame(str_split(company_all_data[2], "\n", simplify = TRUE))%>%
  # .[, 1:19] %>%
  gather(key = "variable", value = "mixed", everything()) %>%
  mutate_if(is.character, str_trim) %>%
  mutate_all(na_if,"") %>%
  mutate( id = ifelse(is.na(mixed) & is.na(lead(mixed,1)), 1, 0)) %>%
  subset(id == 0) %>%
  select(-id) %>%
  filter(!(variable %in% c("V29")))

even_seq1<- seq(2, nrow(split_info1), 2)
odd_seq1<- seq(1, nrow(split_info1), 2)
tbl1<- data.frame(variable = split_info1$mixed[odd_seq1], value = split_info1$mixed[even_seq1])

#---------------------------------

# -----second table
split_info2<- as.data.frame(str_split(company_all_data[3], "\n", simplify = TRUE))%>%
  .[, 1:19] %>%
  gather(key = "variable", value = "mixed", everything()) %>%
  mutate_if(is.character, str_trim) %>%
  mutate_all(na_if,"") %>%
  mutate( id = ifelse(is.na(mixed) & is.na(lead(mixed,1)), 1, 0)) %>%
  subset(id == 0) %>%
  select(-id) %>%
  filter(!(variable %in% c("V9", "V12")))


even_seq2<- seq(2, nrow(split_info2), 2)
odd_seq2<- seq(1, nrow(split_info2), 2)
tbl2<- data.frame(variable = split_info2$mixed[odd_seq2], value = split_info2$mixed[even_seq2])

#----------------third table

split_info3_h<- as.data.frame(str_split(company_all_data[5], "\n", simplify = TRUE))%>%
  .[, c(3:22)] %>%
  gather(key = "variable", value = "mixed", everything()) %>%
  mutate_if(is.character, str_trim) %>%
  mutate_all(na_if,"") 

p1<- paste0(split_info3_h$mixed[split_info3_h$variable == "V3"],"-",split_info3_h$mixed[split_info3_h$variable == "V10"], "-",split_info3_h$mixed[split_info3_h$variable == "V16"] )
p2<- paste0(split_info3_h$mixed[split_info3_h$variable == "V4"],"-",split_info3_h$mixed[split_info3_h$variable == "V11"], "-",split_info3_h$mixed[split_info3_h$variable == "V18"] )
p3<- paste0(split_info3_h$mixed[split_info3_h$variable == "V5"],"-",split_info3_h$mixed[split_info3_h$variable == "V12"])
p4<- paste0(split_info3_h$mixed[split_info3_h$variable == "V6"],"-",split_info3_h$mixed[split_info3_h$variable == "V13"], "-",split_info3_h$mixed[split_info3_h$variable == "V20"] )
p5<- paste0(split_info3_h$mixed[split_info3_h$variable == "V7"],"-",split_info3_h$mixed[split_info3_h$variable == "V8"])
p6<- paste0(split_info3_h$mixed[split_info3_h$variable == "V9"],"-",split_info3_h$mixed[split_info3_h$variable == "V14"])
perticulars<- c("Particulars", p1,p2,p3,p4,p5, p6)

split_info3_1_value<- as.data.frame(str_split(company_all_data[5], "\n", simplify = TRUE))%>%
  # .[, c(3:22)] %>%
  gather(key = "variable", value = "mixed", everything()) %>%
  mutate_if(is.character, str_trim) %>%
  mutate_all(na_if,"") 


# %>%
#   mutate( id = ifelse(is.na(mixed) & is.na(lead(mixed,1)), 1, 0)) %>%
#   subset(id == 0) %>%
#   select(-id) %>%
#   filter(!(variable %in% c("V9", "V12")))


even_seq2<- seq(2, nrow(split_info2), 2)
odd_seq2<- seq(1, nrow(split_info2), 2)
tbl2<- data.frame(variable = split_info2$mixed[odd_seq2], value = split_info2$mixed[even_seq2])

# checkit<- split_info2 %>%
#   group_by(grp = cumsum(mixed != lag(mixed, default = first(mixed)))) %>%
#   slice(1) %>%
#   ungroup %>%
#   select(-grp)
# 
# checkit<- split_info2 %>% 
#   mutate(id = lag(mixed, 1), 
#          decision = if_else(mixed != id, 1, 0), 
#          final = lead(decision, 1, default = 1)) %>% 
#   filter(final == 1) %>% 
#   select(-id, -decision, -final)

# var_seq<- seq(1, 19, 2)
# val_seq<- seq(2, 19, 2)
# info_val<-  trimws(split_info2$mixed[val_seq])
# tbl2<- data.frame(variable = split_info2$mixed[var_seq], value = split_info2$mixed[val_seq])

# write.table(pages_data_th, "test_stock_tr_TBL_RES.csv", sep = ",")



# test<- gsub("  +", "\t", str_trim(company_all_data[3]))

# splt_try<- as.data.frame(str_split(test, "\t", simplify = TRUE))%>%
#   .[, 1:19] %>%
#   gather(key = "variable", value = "mixed", everything()) %>%
#   mutate_if(is.character, str_trim) %>%
#   mutate_all(na_if,"") %>%
#   mutate( id = ifelse(is.na(mixed) & is.na(lead(mixed,1)), 1, 0)) %>%
#   subset(id == 0) %>%
#   select(-id)

# 
# try<-split_info2 %>%
#   mutate( id = ifelse(is.na(mixed) & is.na(lead(mixed,1)), 1, 0))
# 
# mkt_raw_1<- as.data.frame(str_split(company_all_data[2], "\n", simplify = TRUE)) %>%
#   gather(key = "variable", value = "mixed",everything()) %>%
#   mutate_if(is.character, str_trim)
# 
# var_seq<- seq(1, 28, 2)
# info_title<- trimws(mkt_raw_1$mixed[var_seq])
# basics<- ifelse(str_length(info_title) == 0, "Change*", info_title)
# val_seq<- seq(2, 28, 2)
# info_val<-  trimws(mkt_raw_1$mixed[val_seq])

# //*[@id="company"]

tbl.page <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="company"]') %>%
  html_table(fill = TRUE)
# mkt_raw_2<- gsub("[[:space:]]", " ", company_all_data[3])

