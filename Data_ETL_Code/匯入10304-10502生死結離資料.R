library(dplyr)
### 讀入10304-10502年各村里生死結離資料 ###
readBin("origin_data/BDMD.csv", "raw", n = 3L)
readLines(file("origin_data/BDMD.csv", encoding = "UTF-8"), n = 3)
life <- read.csv("origin_data/BDMD.csv", header = T, sep = ',', fileEncoding = 'UTF-8', stringsAsFactors = F)

life %>% 
  filter(substr(區域別,1,3)=="高雄市") %>% 
  mutate(縣市 = substr(區域別,1,3), 行政區域 = paste(substr(區域別,4,6), 村里, sep = '')) %>% 
  select(統計年月, 縣市, 行政區域, 出生數, 出生數.男, 出生數.女, 死亡數, 死亡數.男, 死亡數.女, 結婚對數, 離婚對數)-> life_kao

# 將生死結離資料以雙月合併
index <- which(as.numeric(life_kao$統計年月)%%2 == 0)
life_kao[index,"統計年月"] <- as.numeric(life_kao[index,"統計年月"])-1

life_kao %>% 
  group_by(統計年月,縣市,行政區域) %>% 
  summarise(出生數 = sum(出生數), 出生數.男 = sum(出生數.男), 出生數.女 = sum(出生數.女),
            死亡數 = sum(死亡數), 死亡數.男 = sum(死亡數.男), 死亡數.女 = sum(死亡數.女),
            結婚對數 = sum(結婚對數), 離婚對數 = sum(離婚對數)) %>% 
  filter(統計年月 != '10303', 統計年月 != '10501')-> life_kao2 # 將雙數月份-1，並且跟單數月group_by


life_kao2 %>% 
  filter(substr(統計年月,1,3)=='104') -> life_kao_104

setwd("~/Documents/DSP/Power_Hackathon/origin_data")
write.csv(life_kao2, "life_twomonth.csv", row.names = F)
write.csv(life_kao_104, "104_life_twomonth.csv", row.names = F)

