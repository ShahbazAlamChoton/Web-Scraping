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


#-----------------------------------------------------------------------------------------------
#---------------------------------------Stock price data----------------------------------------
#-----------------------------------------------------------------------------------------------
scrapping_date<- format(Sys.time(), "%d-%b-%Y")
folderName_sp<- paste0("dse_all_company_stockprice_data_", scrapping_date)
createFolder<- dir.create(folderName_sp)


url<- 'https://www.dsebd.org/latest_share_price_scroll_by_ltp.php'

# stock price data Xpath: //*[@id="RightBody"]/div[1]/div/div[1]/table

tbl_company <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="RightBody"]/div[1]/div/div[1]/table') %>%
  html_table(fill = TRUE)

dse_all_company_stock_data<- tbl_company[[1]]
all_company_trading_code<-data.frame(`Trading Code` = dse_all_company_stock_data$`TRADING CODE`)

library(writexl)

write_xlsx(dse_all_company_stock_data, paste0("./",folderName_sp, "/stockprice_data.xlsx"), col_names = TRUE)
write_xlsx(all_company_trading_code, "./trading_codes.xlsx", col_names = TRUE)

# write.table(all_company_trading_code, paste0("./",folderName_sp, "/stockprice.csv"), row.names = F, sep = ",", col.names = T)

#-----------------------------------------------------------------------------------------------
#-------------------------------------- Stock news Compilation----------------------------------
#-----------------------------------------------------------------------------------------------
st_time<- Sys.time()

scrapping_date<- format(Sys.time(), "%d-%b-%Y")
folderName_news<- paste0("stock_news_", scrapping_date)
createFolder<- dir.create(folderName_news)

st_date<- "2021-01-25"    # format must be "y-m-d"
end_date<- "2021-01-25"   # format must be "y-m-d"

# 'https://www.dsebd.org/old_news.php?startDate=2020-10-26&endDate=2020-10-28&criteria=4&archive=news'

news_url<- paste0("https://www.dsebd.org/old_news.php?startDate=",st_date, "&endDate=", end_date, "&criteria=4&archive=news")

# news path: '/html/body/div[2]/section/div/div[3]/div[1]/div/div[2]/table'
# or, : '//*[@id="RightBody"]/div[1]/div/div[2]/table'

tbl_news <- news_url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="RightBody"]/div[1]/div/div[2]/table') %>%
  html_table()

df<- tbl_news[[1]] %>%
  na_if("") %>%
  na.omit()
cname<- c("vars", "value")
colnames(df)<- cname

df<- df %>%
  mutate(cat_wide = rep(1:(nrow(df)/4), each = 4)) %>%
  select(vars, value, cat_wide)
  

all_news_df<- data.frame(`ticker` = df$value[seq(1, nrow(df), 4)],
                       `newstitle` = df$value[seq(2, nrow(df), 4)],
                       `news` = df$value[seq(3, nrow(df), 4)],
                       `date` = lubridate:: ymd(df$value[seq(4, nrow(df), 4)])) %>%
  select(`ticker`, `date`, everything())

all_news_df<- all_news_df %>%
  mutate(`newsid` = row_number()) %>%
  select(`newsid`, everything())

library(writexl)
# write_xlsx(all_news_df, paste0("./", folderName_news,"/trade_news_from_",st_date,"_to_",end_date,".xlsx"), col_names = TRUE)

# write.table(all_news_df, paste0("./", folderName_news,"/trade_news_from_",st_date,"_to_",end_date,".csv"),
#                                 row.names = F, sep = ",", col.names = T)

write.table(all_news_df, paste0("./", folderName_news,"/test_news.csv"), sep = ",", append = TRUE, row.names = FALSE, col.names = TRUE)

end_time<- Sys.time()

(time_taken<- end_time - st_time)
