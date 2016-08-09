## 取出有偶人數
library(gdata)
library(stringr)
setwd('~/Documents/DSP/Power_Hackathon/origin_data')
temp <- list.files(pattern = '^不動產資訊平台')  
data <- NULL
for(i in 1:length(temp)){
  tmp <- read.xls(temp[i], sheet = 1)
  value <- gsub("選擇查詢地區：","",tmp$選擇查詢季別.104Q3[1])
  tmp %>% 
    mutate(行政區域 = value) %>% 
    slice(-c(1:3)) %>% 
    select(c(1,3:4,15)) %>% 
    setnames(1:4,c('村里','有偶人數','有偶比例','行政區域')) %>% 
    mutate(有偶比例 = str_replace(有偶比例,"%",'')) %>% 
    bind_rows(data) -> data
  
}
data %>% 
  mutate(縣市 = str_sub(行政區域,1,3),
         行政區域 = paste0(str_replace(行政區域,'高雄市',''),村里)) %>% 
  select(-村里) %>% 
  filter(行政區域 != '鳳山區誠正里') %>% 
  mutate(統計年月 = 10407) -> data

data[data$行政區域 == '那瑪夏區達卡努瓦', 3] <- '那瑪夏區達卡努瓦里'

write.csv(data,'104_resident.csv',row.names=F)
