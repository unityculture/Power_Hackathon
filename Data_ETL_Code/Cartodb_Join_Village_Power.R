library(dplyr)
library(stringr)
setwd('/Users/sheng/Documents/DSP/Power_Hackathon/origin_data/')
tw_v <- read.csv('tw_village.csv')
kao_power <- read.csv('1040708_kao_power.csv')
household <- read.csv('104_household_m07.csv')
tw_v %>% 
  filter(c_name == '高雄市') %>% 
  mutate(substitute = str_replace_all(substitute,c("\\[部\\]北里" = '廍北里',
                                                   "\\[部\\]南里" = '廍南里',
                                                   "\\[帝\\]埔里" = '坔埔里',
                                                   "\\[峰\\]山里" = '峯山里',
                                                   "公\\[館\\]里" = '公舘里')),
         tmp = paste0(t_name,substitute)) %>% 
  merge(kao_power, by.x = 'tmp', by.y = '行政區域') %>% 
  merge(household %>% select(行政區域, 戶數), by.x = 'tmp', by.y = '行政區域') %>% 
  mutate(power_mean = 售電量度數/戶數)-> join_data
dplyr::setdiff(kao_power$行政區域,join_data$tmp)

join_data %>% write.csv('kao_cartodb.csv',row.names = F)

