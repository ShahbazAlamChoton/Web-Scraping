
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
