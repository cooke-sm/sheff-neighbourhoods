#to scrape data
library(tidyverse)
library(gsheet)


url <- "https://docs.google.com/spreadsheets/d/1E-z17KdP1RNzUOwubxs9q79fF119pSg5/edit#gid=2061677816"

hood_reader <- function(url, sheet){
  data <- gsheet::construct_download_url(url = url, format = "csv")
  data <- gsheet::gsheet2tbl(url,sheetid = sheet)
  data_types <- unlist(data[1,])
  hoods <- type_convert(data[2:nrow(data),])
  
  list(hoods, data_types)
}

hoods_map <- hood_reader(url,3)


hoods_map[[1]] %>% 
  ggplot(aes(y=fct_infreq(`What is the first part of your current postcode? If you don't have one, or it changes quite often, either leave it blank or choose one that you spend some time in.`)))+
  geom_bar()

question_cleaner <- function(data){
  data %>% rename(
    'id'= 1,
    'sub'= 2,
    'sub_time'= 3,
    'postcode'= 4,
    'live_in_sheff'= 7,
    'fixed_abode' = 14,
    'age' = 21,
    'community_feels' = 33,
    'community_feels_free' = 50,
    'hood_id_full' = 51) %>% 
    select('id',
           'sub',
           'sub_time',
           'postcode',
           'live_in_sheff',
           'fixed_abode',
           'age',
           'community_feels',
           'community_feels_free',
           'hood_id_full') %>% 
    mutate(postcode = case_when(NA ~ NA,
                                str_detect(postcode, "\\d+") ~ paste("S",str_extract(postcode, "\\d+"), sep = ""),
                                str_detect(postcode, "^[^\\d]+$") ~ NA,
                                TRUE ~ postcode))

}

data2 <- question_cleaner(data)



data2 %>% 
  ggplot(aes(x=fct_infreq(postcode), fill = sub))+
  geom_bar(position = 'dodge')+
  coord_flip()

data2 %>% 
  ggplot(aes(x = age, fill = sub))+
  geom_bar()

data2$community_feels






