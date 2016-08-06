library(dplyr)
library(tidyr)
library(sqldf)
library(ggplot2)
library(data.table)
library(plotly)

###將所有資料讀入，並且合併###
setwd("~/Documents/DSP/Power_Hackathon/origin_data")
power <- read.csv("1040708_kao_power.csv", header = T, stringsAsFactors = F)
edu <- read.csv("104_edu_twomonth.csv", header = T, stringsAsFactors = F)
income <- read.csv("104_income_twomonth.csv",  header = T, stringsAsFactors = F)
household <- read.csv("104_household_m07.csv", header = T, stringsAsFactors = F)
life <- read.csv("104_life_twomonth.csv", header = T, stringsAsFactors = F)


data <- sqldf(
  "select power.*, edu.教育程度總計, 大學以上比例, 大學比例, 大學以下比例,
          income.納稅單位, 綜合所得總額, 綜合所得中位數, 綜合所得IQR,
          household.戶數, 總人數, 女性比例, 男性比例, 少年人口比例, 青年人口比例, 壯年人口比例, 老年人口比例,
          life.出生數, `出生數.男`, `出生數.女`, 結婚對數, 離婚對數
   from power left join edu on power.統計年月 = edu.統計年月
                            and power.縣市 = edu.縣市
                            and power.行政區域 = edu.行政區域
              left join income on power.統計年月 = income.統計年月
                               and power.縣市 = income.縣市
                               and power.行政區域 = income.行政區域
              left join household on power.統計年月 = household.統計年月
                                  and power.縣市 = household.縣市
                                  and power.行政區域 = household.行政區域
              left join life on power.統計年月 = life.統計年月
                             and power.縣市 = life.縣市
                             and power.行政區域 = life.行政區域"
)
data %>% 
  mutate(每戶平均用電度數 = round(售電量度數/戶數,2),
         每人平均用電度數 = round(售電量度數/總人數,2)) -> data

##EDA
data %>% 
  ggplot(aes(x = as.character(統計年月), y = 售電量度數, label = data$售電量度數)) +
  geom_bar(stat = 'identity', fill = 'darkblue')+
  geom_text(aes(y = 售電量度數), family='STHeiti',nudge_y = 1e+9, nudge_x = 0, size = 6, check_overlap = T, color='red')+
  theme_bw(base_family='STHeiti')+
  labs(title = '104年台北市售電量度數分佈',x='統計年月',y='售電量度數')+
  theme(plot.title = element_text(size=rel(1.3)),
        axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(hjust =1))+
  ggsave('~/Dropbox/DSP/Power/data/104年台北市售電量度數分佈.png', width = 10, height = 10)
  
data %>% 
  mutate(行政區 = substr(行政區域,1,3)) %>% 
  ggplot(aes(x = reorder(行政區, -每戶平均用電度數), y = 每戶平均用電度數)) +
  geom_bar(stat = 'identity', fill = 'darkblue')+
  theme_bw(base_family='STHeiti')+
  labs(title = '104年台北市各區每戶平均用電度數分佈',x='行政區')+
  theme(plot.title = element_text(size=rel(1.3)),
        axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        axis.text.x = element_text(angle = 45, hjust =1))+
  ggsave('~/Dropbox/DSP/Power/data/104年台北市各區每戶平均用電度數分佈.png', width = 10, height = 10)



#write.csv(data, "~/Dropbox/DSP/Power/data/data.csv", fileEncoding = "big5", row.names = F)

data %>% 
  filter(統計年月 == c(10407)) %>% 
  mutate(有偶比例 = 有偶人數/((1-少年人口比例)*總人數),
          男女比 = 男性比例 / 女性比例) %>% 
  select(行政區域, 每戶平均用電度數, 戶數,
             少年人口比例, 青年人口比例, 老年人口比例, 男女比, 有偶比例,
             設有戶籍宅數之平均人口數, 一名老年人口宅數, 每戶平均老年人口數,
             綜合所得中位數, 大學以上比例, 大學比例, 大學以下比例)-> data_7

data %>% 
  filter(統計年月 == c(10407)) %>% 
  select(-c(統計年月,縣市)) %>% 
  group_by(行政區域) %>% 
  summarise_each(funs(mean(.))) %>% 
  select(行政區域, 每戶平均用電度數, 戶數, 戶長平均年齡, 總人數,
             少年人口比例, 青年人口比例, 老年人口比例, 女性比例, 男性比例, 有偶人數,
             設有戶籍宅數之平均人口數, 一名老年人口宅數, 每戶平均老年人口數,
             綜合所得中位數, 大學以上比例, 大學比例, 大學以下比例) %>% 
  mutate(有偶比例 = 有偶人數/((1-少年人口比例)*總人數),
            男女比 = round(男性比例/女性比例,4)) %>% 
  select(行政區域, 每戶平均用電度數, 戶數,
             少年人口比例, 青年人口比例, 老年人口比例, 男女比, 有偶比例,
             設有戶籍宅數之平均人口數, 一名老年人口宅數, 每戶平均老年人口數,
             綜合所得中位數, 大學以上比例, 大學比例, 大學以下比例) %>%
  mutate_each(funs(scale(.)),-行政區域)  -> data.h

data.h %>% as.data.frame() %>% datatable() %>% formatRound(2:44,digit=2)


