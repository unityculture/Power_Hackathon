library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
### 讀入台北市各里戶籍資訊###
setwd("~/Documents/DSP/Power_Hackathon")
household <- fread("origin_data/tw_household.csv") 
household %>% 
  filter(str_detect(area, "高雄市"), year == c(10407)) %>% 
  mutate(縣市 = substr(area,1,3), 行政區域 = paste(substr(area,4,6), sub_area, sep = '')) %>% 
  select(year, 縣市, 行政區域, household, ppl.total, gender, age, counts) -> household
colnames(household) <- c("統計年月","縣市","行政區域","戶數","總人數","性別","年齡","人數")

#將年齡分成四組：少年、青年、壯年、老年
household %>% 
  group_by(統計年月, 縣市, 行政區域, 戶數, 總人數, 年齡) %>% 
  summarise(num = sum(人數)) %>% 
  spread(年齡, num) %>% 
  mutate(少年人口 = `0` + `1` + `2` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + `10` +
                    `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `20`,
         青年人口 = `21` + `22` + `23` + `24` + `25` + `26` + `27` + `28` + `29` + `30` +
               `31` + `32` + `33` + `34` + `35` + `36` + `37` + `38` + `39` + `40`,
         壯年人口 = `41` + `42` + `43` + `44` + `45` + `46` + `47` + `48` + `49` + `50` +
               `51` + `52` + `53` + `54` + `55` + `56` + `57` + `58` + `59` + `60` + 
               `61` + `62` + `63` + `64` + `65`,
         老年人口 = `66` + `67` + `68` + `69` + `70` +
               `71` + `72` + `73` + `74` + `75` + `76` + `77` + `78` + `79` + `80` +
               `81` + `82` + `83` + `84` + `85` + `86` + `87` + `88` + `89` + `90` +
               `91` + `92` + `93` + `94` + `95` + `96` + `97` + `98` + `99` + `100`) -> household_age
  household_age %>% 
    select(c(1:5, 107:110)) -> household_age
  
# 整理各里性別人數
household %>% 
  group_by(統計年月, 縣市, 行政區域, 戶數, 總人數, 性別) %>% 
  summarise(num = sum(人數)) %>% 
  spread(性別,num) -> household_gender
colnames(household_gender) <- c("統計年月", "縣市", "行政區域", "戶數", "總人數", "女性", "男性")

library(sqldf)
household <- sqldf(
  "select household_gender.*, household_age.少年人口, 青年人口, 壯年人口, 老年人口 
   from household_gender left join household_age
   on household_gender.統計年月 = household_age.統計年月
   and household_gender.縣市 = household_age.縣市
   and household_gender.行政區域 = household_age.行政區域"
)

household[household$行政區域=="萬華區糖廍里","行政區域"] <- "萬華區糖部里"

household %>% 
  mutate(女性比例 = round(女性/總人數,4),
         男性比例 = round(男性/總人數,4),
         少年人口比例 = round(少年人口/總人數,4),
         青年人口比例 = round(青年人口/總人數,4),
         壯年人口比例 = round(壯年人口/總人數,4),
         老年人口比例 = round(老年人口/總人數,4)) -> household2

write.csv(household2, "origin_data/104_household_m07.csv", row.names = F)

