library(dplyr)
### 讀入10304-10502年各村里生死結離資料 ###
setwd('/Users/sheng/Documents/DSP/Power_Hackathon')
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

life_kao2 <- read.csv('origin_data/104_life_twomonth.csv', stringsAsFactors = F)
substr(life_kao2[substr(life_kao2$行政區域, 1, 2) == '鳳山', 3], 3, 3) <- rep('區', 765)
substr(life_kao2[substr(life_kao2$行政區域, 1, 2) == '三民', 3], 3, 3) <- rep('區', 860)
life_kao2[life_kao2$行政區域 == '那瑪夏達卡努瓦里', 3] <- '那瑪夏區達卡努瓦里'
life_kao2[life_kao2$行政區域 == '那瑪夏瑪雅里', 3] <- '那瑪夏區瑪雅里'
life_kao2[life_kao2$行政區域 == '那瑪夏南沙魯里', 3] <- '那瑪夏區南沙魯里'

## 處理鳳山區海風里，併入鳳山區海光里
## 處理鳳山區誠正里，拆一半併入鳳山區生明里以及鳳山區誠智里
## 處理左營區復興里，併入左營區永清里
life_kao2[life_kao2$行政區域 == '鳳山區海風里', 3] <- '鳳山區海光里'
life_kao2[life_kao2$行政區域 == '左營區復興里', 3] <- '左營區永清里'
life_kao2 %>% 
  group_by(統計年月, 縣市, 行政區域) %>% 
  summarise(出生數 = sum(出生數),
            出生數.男 = sum(出生數.男),
            出生數.女 = sum(出生數.女),
            死亡數 = sum(死亡數),
            死亡數.男 = sum(死亡數.男),
            死亡數.女 = sum(死亡數.女),
            結婚對數 = sum(結婚對數),
            離婚對數 = sum(離婚對數)) %>% 
  filter(行政區域 != '鳳山區誠正里') -> life_kao2


life_kao2 %>% 
  filter(substr(統計年月,1,3)=='104') -> life_kao_104

setwd("origin_data/")
write.csv(life_kao2, "life_twomonth.csv", row.names = F)
write.csv(life_kao_104, "104_life_twomonth.csv", row.names = F)

