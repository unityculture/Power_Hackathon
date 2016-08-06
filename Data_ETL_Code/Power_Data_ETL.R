library(dplyr)
library(stringr)

## 先處理七月份
setwd('Origin_Data/201507to08_Power/')
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

colnames(data104) <- c('縣市','統計年月','行政區域', '售電量度數')
data104 %>% head
data104 %>% write.csv('../1040708_kao_power.csv',row.names = F)
