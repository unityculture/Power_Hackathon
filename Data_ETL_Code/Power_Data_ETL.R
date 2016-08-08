library(dplyr)
library(stringr)

## 先處理七月份
setwd("~/Documents/DSP/Power_Hackathon/origin_data/201507to08_Power/")
temp <- list.files(pattern = '^201507')  
data07 <- NULL
for(i in 1:length(temp)){
  readLines(temp[i]) %>%  ## 因為台電資料下載似乎不能用 read.xls開啟，只能用readLines
    str_extract_all('<td>.+?</td>', simplify = T) %>%  ## 挑出介於 <td> and </td> 的字串
    as.data.frame() %>%  ## matrix -> df
    filter(V1 != '') %>%  ## 空列去掉
    mutate_each(funs(str_replace_all(.,"<td>|</td>|,| ",""))) %>%  ## 把<td> </td> , 空格取代掉
    filter(V1 != '合計', V3 != '無法分類') %>% ## 清除無法分類、與合計的列
    bind_rows(data07) -> data07
}

temp <- list.files(pattern = '^201508')  
data08 <- NULL
for(i in 1:length(temp)){
  readLines(temp[i]) %>% 
    str_extract_all('<td>.+?</td>', simplify = T) %>% 
    as.data.frame() %>% 
    filter(V1 != '') %>% 
    mutate_each(funs(str_replace_all(.,"<td>|</td>|,| ",""))) %>% 
    filter(V1 != '合計', V3 != '無法分類') %>% 
    bind_rows(data08) -> data08
}

## 將兩個月份合併起來
data07 %>% 
  mutate(ym = 10407) %>% 
  bind_rows(
    data08 %>% mutate(ym = 10408)
    ) %>% 
  mutate(tmp = paste0(V2,V3)) %>% 
  select(V1, ym, tmp, V4) -> data104


## 根據官方資料(ref), 高雄市有891個村里，確認村里是否一致
## ref : http://www.stat.gov.tw/ct.asp?xItem=39438&CtNode=1519&mp=4
## 下載之後轉檔成：台灣村里清單.csv
kao_v <- read.csv('../台灣村里清單.csv', fileEncoding = 'big5') %>% 
  filter(縣市名稱 == '高雄市') %>% 
  mutate(tmp = paste0(區鄉鎮名稱,村里名稱))
  ## 因為不一樣 找出差異
  ## 電力資料內沒有：杉林區大愛里
  ## 村里資料內沒有：左營區復興里、鳳山區海風里、鳳山區誠正里、阿蓮區峯山里
(data104$tmp %>% n_distinct) == (kao_v$tmp %>% n_distinct())
dplyr::setdiff(data104$tmp, kao_v$tmp)
dplyr::setdiff(kao_v$tmp, data104$tmp)

## 將湖內區公館里更改為湖內區公舘里
data104[data104$ym == 10407 & data104$tmp == '湖內區公館里', 3] <- '湖內區公舘里'
data104[data104$ym == 10408 & data104$tmp == '湖內區公館里', 3] <- '湖內區公舘里'

## 將10407杉林區月眉里的電量，依照戶數比例留下589867*0.4 = 235947
## 將10408杉林區月眉里的電量，依照戶數比例留下618109*0.4 = 247245
## 新增10407杉林區大愛里電量，為589867-235947 = 353920
## 新增10407杉林區大愛里電量，為618109-247245 = 370864
data104[data104$ym == 10407 & data104$tmp == '杉林區月眉里', 4] <- 235947
data104[data104$ym == 10408 & data104$tmp == '杉林區月眉里', 4] <- 247245
dai_10407 <- as.data.frame(matrix(c('高雄市', '10407', '杉林區大愛里', '353920'), nrow = 1, ncol = 4))
dai_10408 <- as.data.frame(matrix(c('高雄市', '10408', '杉林區大愛里', '370864'), nrow = 1, ncol = 4))
colnames(dai_10407) <- c('V1', 'ym', 'tmp', 'V4')
colnames(dai_10408) <- c('V1', 'ym', 'tmp', 'V4')
data104 <- rbind(data104, dai_10407, dai_10408)

## 將10407左營區復興里的電力資料合併到左營區永清里，281778+5089 = 286867
data104[data104$ym == 10407 & data104$tmp == '左營區永清里', 4] <- 286867
data104 %>% 
  filter(tmp != '左營區復興里') -> data104

## 將10407鳳山區海風里的電力資料合併到鳳山區海光里，0+2500 = 2500
## 將10408鳳山區海風里的電力資料合併到鳳山區海光里，21654+1197141 = 1218795
data104[data104$ym == 10407 & data104$tmp == '鳳山區海光里', 4] <- 2500
data104[data104$ym == 10408 & data104$tmp == '鳳山區海光里', 4] <- 1218795
data104 %>% 
  filter(tmp != '鳳山區海風里') -> data104

## 將10408鳳山區誠正里的電力資料拆一半(36704/2 = 18352)
## 一半合到生明里(922805+18352 = 941157)
## 另一半合到誠智里(1079110+18352 = 1097462)
data104[data104$ym == 10408 & data104$tmp == '鳳山區生明里', 4] <- 941157
data104[data104$ym == 10408 & data104$tmp == '鳳山區誠智里', 4] <- 1097462
data104 %>% 
  filter(tmp != '鳳山區誠正里') -> data104

colnames(data104) <- c('縣市','統計年月','行政區域', '售電量度數')

data104 %>% 
  group_by(縣市,行政區域) %>% 
  summarise(售電量度數 = 售電量度數 %>% as.numeric() %>% sum) %>% 
  mutate(統計年月 = "10407") %>% 
  write.csv('../1040708_kao_power.csv',row.names = F)
