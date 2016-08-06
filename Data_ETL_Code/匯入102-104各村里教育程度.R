### 讀入102-103年各村里教育程度資料 ###
setwd("~/Documents/DSP/Power_Hackathon")
# 讀入104年各村里教育程度資料
readBin("origin_data/104各村里教育程度資料.csv", "raw", n = 3L)  # 發現編碼為UTF8
readLines(file("origin_data/104各村里教育程度資料.csv", encoding = "UTF-8"), n = 1) # 讀入資料第一行看看
edu104 <- read.csv("origin_data/104各村里教育程度資料.csv", header = T, sep =",", skip = 1, stringsAsFactors = F, fileEncoding = "UTF8")

# 將104年資料男女人數合併
library(dplyr)
edu104 %>% 
  mutate( 博畢 = 博畢_男.+博畢_女.,博肄 = 博肄_男.+博肄_女.,
          碩畢 = 碩畢_男.+碩畢_女.,
          碩肄 = 碩肄_男.+碩肄_女.,
          大畢 = 大畢_男.+大畢_女.,
          大肄 = 大肄_男.+大肄_女.,
          二畢 = 二畢_男.+二畢_女.,
          二肄 = 二肄_男.+二肄_女.,
          後二畢 = 後二畢_男.+後二畢_女.,
          後二肄 = 後二肄_男.+後二肄_女.,
          前三肄 = 前三肄_男.+前三肄_女.,
          高畢 = 高畢_男.+高畢_女.,
          高肄 = 高肄_男.+高肄_女.,
          職畢 = 職畢_男.+職畢_女.,
          職肄 = 職肄_男.+職肄_女.,
          國畢 = 國畢_男.+國畢_女.,
          國肄 = 國肄_男.+國肄_女.,
          初畢 = 初畢_男.+初畢_女.,
          初肄 = 初肄_男.+初肄_女.,
          小畢 = 小畢_男.+小畢_女.,
          小肄 = 小肄_男.+小肄_女.,
          自修 = 自修_男.+自修_女.,
          不識 = 不識_男.+不識_女) -> edu104_temp
edu104_total <- edu104_temp[,c(1:4,51:73)]

# 統一欄位名稱
colname <- c(colnames(edu104_total)[1], '區域別', '村里', '教育程度總計', colnames(edu104_total)[5:27])
colnames(edu102) <- colname
colnames(edu103) <- colname
colnames(edu104_total) <- colname

# 將各年度高雄市的資料給篩選出來
edu104_total %>% 
  filter(substr(區域別,1,3) == "高雄市") -> edu104_kao

# 將年資料推估至月資料(同一年度的各里月資料皆相同，考慮到每月的變化量不會太大)
month_1 <- rep(c(1,3,5,7,9), each=456)
month_2 <- rep(11, each=456)
month <- c(paste('0', month_1, sep = ''), month_2)

edu104_kao_month <- NULL
for(i in 1:6){
  edu104_kao_month <- bind_rows(edu104_kao_month,edu104_kao)
}
edu104_kao_month %>% 
  mutate(統計年月 = paste(統計年., month, sep = ''),
         縣市 = substr(區域別,1,3),
         行政區域 = paste(substr(區域別,4,6), 村里, sep = '')) -> edu104_kao_month

edu104_kao_month %>% 
  mutate(大學以上 = 碩畢 + 碩肄 + 博畢 + 博肄,
         大學 = 大畢 + 大肄,
         大學以下 = 二畢 + 二肄 + 後二畢 + 後二肄 + 前三肄 + 高畢 + 高肄 + 職畢 + 職肄 + 
                    國畢 + 國肄 + 初畢 + 初肄 + 小畢 + 小肄 + 自修 + 不識) %>% 
  select(統計年月, 縣市, 行政區域, 教育程度總計, 大學以上, 大學, 大學以下) %>% 
  mutate(大學以上比例 = round(大學以上/教育程度總計,4),
         大學比例 = round(大學/教育程度總計,4),
         大學以下比例 = round(大學以下/教育程度總計,4)) %>% 
  filter(substr(統計年月,1,3)=='104') -> edu_104

write.csv(edu_104, "origin_data/104_edu_twomonth.csv", row.names = F)
