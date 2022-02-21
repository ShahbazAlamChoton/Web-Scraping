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
library(readxl)


scrapping_date<- format(Sys.time(), "%d-%b-%Y")
folderName<- paste0("dse_company_info_all_", scrapping_date)
createFolder<- dir.create(folderName)


all_com_name<- read_xlsx("trading_codes.xlsx", col_names = TRUE)

# %>%
#   separate(Company_info, into = c("company", "state1", "state2", "state3"), sep = "[[:space:]]") %>%  #(?=[^ ]+$)
#   select(company)

company_name<- all_com_name$`Trading.Code`

st_time<- Sys.time()
# seq_along(company_name)

# for(i in 116:120){

  url<- paste0('https://www.dsebd.org/displayCompany.php?name=', company_name[1]) # change it to 'i' later to run loop


tbl_company <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="company"]') %>%
  html_table(fill = TRUE)

company_id_code<- colnames(tbl_company[[1]])

#--------------------------------- table: Market Information -----------------------------------------------------------------

tbl_1_p1<- tbl_company[[2]] %>%
  select(X1, X2) %>%
  rename(variable =X1, value = X2)

tbl_1_p2<- tbl_company[[2]] %>%
  select(X3, X4)%>%
  rename(variable = X3, value = X4)

df1<- bind_rows(tbl_1_p1, tbl_1_p2) %>%
  mutate(company_name = substr(company_id_code[1], 15, str_length(company_id_code[1])), 
         company_code = substr(company_id_code[2], 13, str_length(company_id_code[2]))) %>%
  select(company_name, company_code, everything())


write.table(df1, paste0("./", folderName,"/market_info_table.csv"), sep = ",", append = TRUE, row.names = FALSE, col.names = TRUE)

#----------------------------------- table: Basic Information ---------------------------------------------------

tbl_2<- tbl_company[[3]] %>%
.[-c(5, 6), ]

tbl_2_p1<- tbl_2 %>%
  select(X1, X2) %>%
  rename(variable =X1, value = X2)
  
tbl_2_p2<- tbl_2 %>%
  select(X3, X4)%>%
  rename(variable = X3, value = X4)

df2<- bind_rows(tbl_2_p1, tbl_2_p2) %>%
  mutate(company_name = substr(company_id_code[1], 15, str_length(company_id_code[1])), 
         company_code = substr(company_id_code[2], 13, str_length(company_id_code[2]))) %>%
  select(company_name, company_code, everything())

write.table(df2, paste0("./", folderName,"/basic_info_table.csv"), sep = ",", append = TRUE, row.names = FALSE, col.names = TRUE)

#---------------------------------table: Interim Financial Performance ------------------------------------------

tbl_3_header_data <- tbl_company[[5]] %>%
  .[c(1,2,3,4), ] %>%
  t %>%
  as.data.frame()

tbl_3_data_val<- tbl_company[[5]] %>%
  .[-c(1,2,3,4), ]

# df3_header<- paste(tbl_3_header_data$`1`, tbl_3_header_data$`2`, 
#                    tbl_3_header_data$`3`,tbl_3_header_data$`4`, sep = "-")

col_name_df3<- c("Particulars", "Unaudited / Audited-Q1-Ending on-201909", "Unaudited / Audited-Q2-Ending on-201912", 
                 "Unaudited / Audited-Half Yearly-6 Months201912", "Unaudited / Audited-Q3-Ending on-202003",
                 "Unaudited / Audited-9 Months-202003", "Unaudited / Audited-Annual-Ending on-202006")

colnames(tbl_3_data_val)<- col_name_df3

df3<- tbl_3_data_val %>%
  mutate(company_name = substr(company_id_code[1], 15, str_length(company_id_code[1])),
         company_code = substr(company_id_code[2], 13, str_length(company_id_code[2]))) %>%
  select(company_name, company_code, everything())


write.table(df3,paste0("./", folderName,"/IFP_table.csv"), sep = ",", append = TRUE, row.names = FALSE, col.names = TRUE)

#--------------------------------- Price/Earning Ratio Table ---------------------------------------------------

tbl_4_header<- as.character(tbl_company[[6]][1, ])

tbl_4_data_val<- tbl_company[[6]] %>%
  slice(-1)
colnames(tbl_4_data_val)<- tbl_4_header

df4<- tbl_4_data_val %>%
  mutate(company_name = substr(company_id_code[1], 15, str_length(company_id_code[1])),
         company_code = substr(company_id_code[2], 13, str_length(company_id_code[2]))) %>%
  select(company_name, company_code, everything())

write.table(df4, paste0("./", folderName,"/PE_ratio_table.csv"), sep = ",", append = TRUE, row.names = FALSE, col.names = TRUE)

#--------------------------------- table: Price/Earning Ratio extended ------------------------------------------
tbl_5_header<- as.character(tbl_company[[7]][1, ])

tbl_5_data_val<- tbl_company[[7]] %>%
  slice(-1)
colnames(tbl_5_data_val)<- tbl_5_header

df5<- tbl_5_data_val %>%
  mutate(company_name = substr(company_id_code[1], 15, str_length(company_id_code[1])),
         company_code = substr(company_id_code[2], 13, str_length(company_id_code[2]))) %>%
  select(company_name, company_code, everything())

write.table(df5, paste0("./", folderName,"/PE_ratio_2_table.csv"), sep = ",", append = TRUE, row.names = FALSE, col.names = TRUE)

#--------------------------------- table: Financial Performance ------------------------------------------------

tbl_6_data_val <- tbl_company[[8]] %>%
  select(-X1)%>%
  slice(-c(1:3))

col_name_df6<- c("Year", "Earnings per share(EPS)-Basic-Original", "Earnings per share(EPS)-Basic-Restated",
                 "Earnings per share(EPS)-Diluted",
                 "EPS - Continuing Operations-Basic-Original","EPS - Continuing Operations-Basic-Restated",
                 "EPS - Continuing Operations-Diluted",
                 "NAV Per Share-Original", "NAV Per Share-Restated", "NAV Per Share-Diluted",
                 "Profit/(Loss) and OCI-PCO*", "Profit/(Loss) and OCI-Profit for the year (mn)", "Profit/(Loss) and OCI-TCI*")

# tbl_6_data_val<- tbl_company[[8]] %>%
#   slice(-c(1:3))
# 
# df6_header<- paste(tbl_6_header_data$V1, tbl_6_header_data$V2, 
#                    tbl_6_header_data$V3, sep = "-")

colnames(tbl_6_data_val)<- col_name_df6

df6<- tbl_6_data_val %>%
  mutate(company_name = substr(company_id_code[1], 15, str_length(company_id_code[1])),
         company_code = substr(company_id_code[2], 13, str_length(company_id_code[2]))) %>%
  select(company_name, company_code, everything())


write.table(df6, paste0("./", folderName,"/FP_1_table.csv"), sep = ",", append = TRUE, row.names = FALSE, col.names = TRUE)

#--------------------------------- table: Financial Performance extended ---------------------------------------

tbl_7_data_val <- tbl_company[[9]] %>%
  select(-X1)%>%
  slice(-c(1:4))

col_name_df7<- c("Year", "Year end Price Earnings (P/E) ratio-Earnings per share(EPS)-Using Basic EPS-Original",
                 "Year end Price Earnings (P/E) ratio-Earnings per share(EPS)-Using Basic EPS-Restated",
                 "Year end Price Earnings (P/E) ratio-Earnings per share(EPS)-Using Diluted EPS",
                 "Year end Price Earnings (P/E) ratio-EPS - Continuing Operations-Using Basic EPS-Original",
                 "Year end Price Earnings (P/E) ratio-EPS - Continuing Operations-Using Basic EPS-Restated",
                 "Year end Price Earnings (P/E) ratio-EPS - Continuing Operations-Using Diluted EPS",
                 "Dividend in %*", "Dividend Yield in %")


colnames(tbl_7_data_val)<- col_name_df7

df7<- tbl_7_data_val %>%
  mutate(company_name = substr(company_id_code[1], 15, str_length(company_id_code[1])),
         company_code = substr(company_id_code[2], 13, str_length(company_id_code[2]))) %>%
  select(company_name, company_code, everything())


write.table(df7, paste0("./", folderName,"/FP_2_table.csv"), sep = ",", append = TRUE, row.names = FALSE, col.names = TRUE)

#--------------------------------- table: Other Information ------------------------------------------------------

tbl_8_data_val <- tbl_company[[11]] %>%
  .[, c(1,2)] %>%
  slice(-c(5,7,9,10))

  var1<- gsub("\\s+"," ",tbl_8_data_val$X1)[c(4:6)]
  tbl_8_raw_data<-gsub("\\s+"," ",tbl_8_data_val$X2)%>%
    .[-c(1,2,3)]
  
  tbl_8_raw_data_first3<-gsub("\\s+"," ",tbl_8_data_val$X2)%>%
    .[c(1,2,3)]
  
  cname<- c("Sponsor/Director", "Govt", "Institute", "Public")
  try_df<- as.data.frame(str_split(tbl_8_raw_data, " ", simplify = TRUE), stringsAsFactors = F)
  
  `Sponsor/Director`<-read.table(text = try_df$V1, sep = ":", colClasses = "character") %>%
    .[, 2] %>%
    as.numeric()
  
  `Govt`<- read.table(text = try_df$V2, sep = ":", colClasses = "character") %>%
    .[, 2] %>%
    as.numeric()
  
  `Institute`<-read.table(text = try_df$V3, sep = ":", colClasses = "character") %>%
    .[, 2] %>%
    as.numeric()
  
  `Public`<- read.table(text = try_df$V4, sep = ":", colClasses = "character") %>%
    .[, 2] %>%
    as.numeric()
  
  df8<- as.data.frame(cbind(var1, `Sponsor/Director`, `Govt`, `Institute`, `Public`), stringsAsFactors = F) %>%
    mutate(`Listing Year` = tbl_8_raw_data_first3[1], 
           `Market Category` = tbl_8_raw_data_first3[2],
           `Electronic Share` = tbl_8_raw_data_first3[3],
           company_name = substr(company_id_code[1], 15, str_length(company_id_code[1])),
           company_code = substr(company_id_code[2], 13, str_length(company_id_code[2]))
           ) %>%
    select(company_name, company_code, `Listing Year`, `Market Category`, `Electronic Share`, everything())

  write.table(df8, paste0("./", folderName,"/other_info_table.csv"), sep = ",", append = TRUE, row.names = FALSE, col.names = TRUE)
  
  #--------------------------------- table: Corporate Performance ----------------------------------------------
  
  df9 <- tbl_company[[12]] %>%
    select(X2, X3) %>%
    mutate(info = ifelse(X3 == X2, "-", X3)) %>%
    rename(header = X2) %>%
    select(header, info) %>%
    mutate(company_name = substr(company_id_code[1], 15, str_length(company_id_code[1])),
           company_code = substr(company_id_code[2], 13, str_length(company_id_code[2]))) %>%
    select(company_name, company_code, everything())
  
  write.table(df9, paste0("./", folderName,"/corporate_performance_table.csv"), sep = ",", append = TRUE, row.names = FALSE, col.names = TRUE)
  
 
  #--------------------------------- table: Company Address -----------------------------------------------------
  
  df10 <- tbl_company[[13]] %>%
    rename(header = X1, info = X2) %>%
    mutate(company_name = substr(company_id_code[1], 15, str_length(company_id_code[1])),
           company_code = substr(company_id_code[2], 13, str_length(company_id_code[2]))) %>%
    select(company_name, company_code, everything())
  
  write.table(df10, paste0("./", folderName,"/company_address_table.csv"), sep = ",", append = TRUE, row.names = FALSE, col.names = TRUE)
  
  
  agm_date<- url %>% 
    read_html() %>% 
    html_nodes(xpath = '//*[@id="section-to-print"]/h2[4]/div[1]') %>% 
    html_text() %>% 
    str_trim() %>% 
    unlist() %>% 
    gsub("\\s+"," ", .)
  
  
  year_ended<-  url %>% 
    read_html() %>% 
    html_nodes(xpath = '//*[@id="section-to-print"]/h2[4]/div[2]') %>% 
    html_text() %>% 
    str_trim() %>% 
    unlist() %>% 
    gsub("\\s+"," ", .)
  
  agm_df<- cbind(agm_date, year_ended)
  write.table(agm_df, paste0("./", folderName,"/agm_data.csv"), sep = ",", append = TRUE, row.names = FALSE, col.names = TRUE)
  
# }

end_tm<- Sys.time()

(total_time_taken<- end_tm-st_time)
