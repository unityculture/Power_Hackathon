library(dplyr)
library(stringr)
library(data.table)
setwd("~/Documents/DSP/Power_Hackathon/origin_data")
tp_power_sml <- fread('1050607台北市最小行政區用電量.csv', stringsAsFactors = F)
tp_power_sml %>% 
  group_by(YM, CODEBASE) %>% 
  summarise(kW = kW %>% sum ) %>% 
  write.csv('smlarea_kao_cartodb.csv',row.names=F)