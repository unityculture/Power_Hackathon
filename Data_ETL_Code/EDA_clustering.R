library(dplyr)
library(stringr)
library(data.table)
library(tidyr)
library(ggplot2)
library(highcharter)
setwd("~/Documents/DSP/Power_Hackathon/origin_data")
data <- read.csv('ks_data.csv', stringsAsFactors = F)
data %>% 
  select(行政區域,每戶平均用電度數,
             女性比例, 男性比例,
             少年人口比例,青年人口比例,壯年人口比例,老年人口比例,總人數,有偶人數,
             綜合所得中位數, 大學以上比例, 大學比例, 大學以下比例) %>% 
  mutate(扶老比_log = log10((1-青年人口比例-壯年人口比例-少年人口比例)/(青年人口比例+壯年人口比例)),
             有偶比例 = 有偶人數/((1-少年人口比例)*總人數),
             平均教育程度 = 大學以上比例*6+大學比例*4+大學以下比例*0,
             女男比 = round(女性比例/男性比例,4),
             綜合所得中位數_log = log10(綜合所得中位數),
             每戶平均用電度數 = 每戶平均用電度數) %>% 
  select(行政區域,綜合所得中位數_log,平均教育程度,有偶比例,女男比,扶老比_log,每戶平均用電度數) %>%
  mutate_each(funs(scale(.)),-行政區域) %>% 
  gather('index','value',-行政區域) %>% 
  group_by(index) %>% 
  mutate(value = (value-min(value))/(max(value)-min(value))) %>% 
  ggplot(aes(x=value)) +
  geom_histogram() +
  theme_grey(base_family = 'STHeiti') +
  facet_wrap(~index)


data %>% 
  select(行政區域,每戶平均用電度數,
             女性比例, 男性比例,
             少年人口比例,青年人口比例,壯年人口比例,老年人口比例,總人數,有偶人數,
             綜合所得中位數, 大學以上比例, 大學比例, 大學以下比例) %>% 
  mutate(扶老比_log = log10((1-青年人口比例-壯年人口比例-少年人口比例)/(青年人口比例+壯年人口比例)),
             有偶比例 = 有偶人數/((1-少年人口比例)*總人數),
             平均教育程度 = 大學以上比例*6+大學比例*4+大學以下比例*0,
             女男比 = round(女性比例/男性比例,4),
             綜合所得中位數_log = log10(綜合所得中位數),
             每戶平均用電度數 = 每戶平均用電度數) %>% 
  select(行政區域,綜合所得中位數_log,平均教育程度,有偶比例,女男比,扶老比_log,每戶平均用電度數) %>%
  mutate_each(funs(scale(.)),-行政區域) %>% 
  gather('index','value',-行政區域) %>% 
  group_by(index) %>% 
  mutate(value = (value-min(value))/(max(value)-min(value)))  %>% 
  spread(index,value) -> data.h

set.seed(20160807)
data.h %>% 
  mutate(cluster = kmeans(data.h %>% select(-1),centers=8)$cluster %>% as.factor())  -> data_cluster

data_cluster %>% 
  group_by(cluster) %>% 
  select(-1) %>% 
  summarise_each(funs(mean),-cluster) %>% 
  gather('index','value',-cluster) %>% 
  spread(cluster,value) %>% 
  as.data.frame() -> tmp

tmp$mean <- rowMeans(tmp[,-1])

highchart() %>% 
  hc_chart(polar = TRUE, type = "line") %>% 
  hc_xAxis(categories = c('綜合所得中位數_log',
                          '平均教育程度',
                          '有偶比例',
                          '女男比',
                          '扶老比_log',
                          '每戶平均用電度數'),
           tickmarkPlacement = 'on',
           lineWidth = 0) %>% 
  hc_yAxis(gridLineInterpolation = 'polygon',
           lineWidth = 0,
           min = 0, max = 1) %>% 
  hc_series(
    list(
      data = tmp[,c(2)],
      pointPlacement = 'on',color=col.raw[1]),
    list(
      data = tmp[,c(3)],
      pointPlacement = 'on',color=col.raw[2]),
    list(
      data = tmp[,c(4)],
      pointPlacement = 'on',color=col.raw[3]),
    list(
      data = tmp[,c(5)],
      pointPlacement = 'on',color=col.raw[4]),
    list(
      data = tmp[,c(6)],
      pointPlacement = 'on',color=col.raw[5]),
    list(
      data = tmp[,c(7)],
      pointPlacement = 'on',color=col.raw[6]),
    list(
      data = tmp[,c(8)],
      pointPlacement = 'on',color=col.raw[7]),
    list(
      data = tmp[,c(9)],
      pointPlacement = 'on',color=col.raw[8]),
    list(
      data = tmp[,c(10)],
      pointPlacement = 'on',color=col.raw[9])
  )


c("#A6CEE3","#1F78B4","#B2DF8A","#33A02C","#FB9A99","#E31A1C","#FDBF6F","#FF7F00","#CAB2D6","#6A3D9A") -> col.raw
c('綜合所得中位數_log','平均教育程度','有偶比例','女男比','扶老比_log','每戶平均用電度數') -> radar.name
highchart() %>% 
  hc_chart(polar = TRUE, type = "line") %>% 
  hc_title(text = "第3群：高知識份子小康家庭族群") %>% 
  hc_xAxis(categories = radar.name,
           tickmarkPlacement = 'on',
           lineWidth = 0) %>% 
  hc_yAxis(gridLineInterpolation = 'polygon',
           lineWidth = 0,
           min = 0, max = 1) %>% 
  hc_series(
    list(
      name = "第3群：高知識份子小康家庭族群",
      data = tmp[,c(4)],
      pointPlacement = 'on',color=col.raw[3]),
    list(
      name = "各群平均",
      data = tmp[,c(10)],
      pointPlacement = 'on',color='#474747')
  )



## clustering map 
map <- fread('kao_cartodb.csv') %>% mutate(行政區域 = paste0(t_name,substitute))
map %>% 
  merge(data_cluster, by = '行政區域') %>% 
  write.csv('ks_cluster_map.csv',row.names=F)
