### 讀入101年各里綜合所得稅總額資料 ###
setwd('/Users/sheng/Documents/DSP/Power_Hackathon')
readLines(file("origin_data/高雄市綜合所得.CSV", encoding = "UTF-8"), n = 3)
income101 <- read.csv("origin_data/高雄市綜合所得.CSV", skip = 1, header = T, stringsAsFactors = F)

# 將村里為"其他"與"合計"的資料給刪除
library(dplyr)
income101 %>% 
  filter(村里 != "其　他", 村里 != "合　計")  -> income101


# 將資料推估至月資料，並且將101年的資料推估至102-104年資料
month_1 <- rep(c(1,3,5,7,9), each=898)
month_2 <- rep(11, each=898)
month <- c(paste('0', month_1, sep = ''), month_2)

income101_month <- NULL
for(i in 1:6){
  income101_month <- bind_rows(income101_month, income101)
}
income101_month %>% 
  mutate(月份 = month,
         行政區域 = paste(鄉鎮市區, 村里, sep = '')) -> income101_month

year <- rep(101:104, each=5388)
income <- NULL
for(i in 1:4){
  income <- bind_rows(income, income101_month)
}

income %>% 
  mutate(統計年月 = paste(year, month, sep = ''),
         綜合所得IQR = 第三分位數 - 第一分位數) %>% 
  select(統計年月, 行政區域, 納稅單位, 綜合所得總額,
             平均數, 中位數, 綜合所得IQR, 第一分位數, 第三分位數, 標準差, 變異係數)-> income
colnames(income) <-
  c(colnames(income)[1], '行政區域', '納稅單位', '綜合所得總額', 
    '綜合所得平均數', '綜合所得中位數', '綜合所得IQR', '綜合所得Q1', '綜合所得Q3', '綜合所得標準差', '綜合所得變異係數')

income %>% 
  filter(substr(統計年月,1,3)=='104') -> income_104

# 輸出資料
write.csv(income, "income_twomonth.csv",  row.names = F)
write.csv(income_104, "104_income_twomonth.csv", row.names = F)
