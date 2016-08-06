library(stringr)
library(dplyr)
library(sqldf)

# 設定資料讀取資料夾
setwd('~/Downloads/power/')
data <- NULL

# 將路徑裡面符合檔案開頭為2014~2016的資料名稱列出來
temp <- list.files(pattern = '^2014|^2015|^2016')  

# 將資料用迴圈方式一口氣讀進來
for(i in 1:length(temp)){
  year <- str_sub(temp[i],1,4)
  month <- str_sub(temp[i],5,6)
  read.csv(temp[i], fileEncoding = "BIG5", stringsAsFactors = F) %>% 
    mutate(年份 = year,月份 = month) %>% 
    bind_rows(data) -> data
}

# 將村里為無法分類以及縣市為合計的列資料去除
data %>% 
  filter(村里 != '無法分類', 縣市 != '合計') %>% 
  mutate(行政區域 = paste(鄉鎮市區, 村里, sep = ''),
         統計年月 = paste(as.numeric(年份)-1911, 月份, sep = '')) %>% 
  select(統計年月, 縣市, 行政區域, 售電量.度.)-> data
  
data$縣市 <- as.character(data$縣市)
data$行政區域 <- as.character(data$行政區域)
data$統計年月 <- as.character(data$統計年月)


data %>% 
  group_by(統計年月, 縣市) %>% 
  summarise(count = n())   # 發現有些村里在某些月份沒有資料

data[data$行政區域 == "萬華區糖_里","行政區域"] <- "萬華區糖部里" # 調整村里名稱，將糖_里轉為糖部里

#匯入台北市各村里清單，並且調整年度到與售電量資料一樣10304-10502
region <- read.csv('~/Dropbox/DSP/Power/data/台北市各區村里清單.csv', fileEncoding = 'big5', header = T, stringsAsFactors = F)
power_region <- NULL
Year_1 <- rep(10304:10312, each = 456)
Year_2 <- rep(10401:10412, each = 456)
Year_3 <- rep(10501:10502, each = 456)
Year <- c(Year_1, Year_2, Year_3)

for (i in 1:23){
  power_region <- bind_rows(power_region,region)
}
power_region_date <- data.frame(統計年月 = Year, power_region)
power_region_date$統計年月 <- as.character(power_region_date$統計年月)


power <- sqldf(
  "select power_region_date.*, data.`售電量.度.` from power_region_date left join data
  on power_region_date.統計年月 = data.統計年月
  and power_region_date.縣市 = data.縣市
  and power_region_date.行政區域 = data.行政區域"
)

power[is.na(power$售電量.度.) == T, "售電量.度."] <- 0

# 將電力資料以雙月合併
index <- which(as.numeric(power$統計年月)%%2 == 0)
power[index,"統計年月"] <- as.numeric(power[index,"統計年月"])-1

power %>% 
  group_by(統計年月,縣市,行政區域) %>% 
  summarise(售電量度數 = sum(售電量.度.)) %>% 
  filter(統計年月 != '10303', 統計年月 != '10501')-> power2 # 將雙數月份-1，並且跟單數月group_by
# 檢查並成雙月之後，還有沒有售電量度數為0，結果發現糖部里在103年有NA
power2 %>% filter(售電量度數==0) %>% group_by(統計年月,行政區域) %>% summarise(count = n()) -> miss

##檢查##
# power %>% 
  # filter(統計年月 == '10305', 行政區域 == '北投區豐年里')

# power2 %>% 
  # filter(統計年月 == '10305', 行政區域 == '北投區豐年里')


power2 %>% 
  filter(substr(統計年月,1,3)=='104') -> power_104_twomonth


write.csv(power2, "~/Downloads/power/power_twomonth.csv", fileEncoding = "big5", row.names = F)
write.csv(power_104_twomonth, "~/Downloads/power/104_power_twomonth.csv", fileEncoding = "big5", row.names = F)
