### 讀入台灣各縣市村里清單資料 ###

village <- read.csv('origin_data/台灣村里清單.csv', header = T, stringsAsFactors = F, fileEncoding = 'big5')

### 篩選高雄市的村里資料 ###
village %>% 
  filter(縣市名稱 == '高雄市') -> ks_village

### 檢查高雄市的村里資料有無難字 ###
ks_village %>% 
  filter(substr(村里名稱, 1, 1) == '_')
ks_village %>% 
  filter(substr(村里名稱, 2, 2) == '_')

ks_village[ks_village$村里名稱 == "_北里", 6] <- "廍北里"
ks_village[ks_village$村里名稱 == "_南里", 6] <- "廍南里"
ks_village[ks_village$村里名稱 == "_埔里", 6] <- "坔埔里"
ks_village[ks_village$村里名稱 == "_山里", 6] <- "峯山里"
ks_village[ks_village$村里名稱 == "公_里", 6] <- "公舘里"

# write.csv(ks_village, '~/Downloads/Power_Hackathon-master/origin_data/高雄市村里清單.csv', fileEncoding = "utf8", row.names = F)

ks_village %>% 
  mutate(行政區域 = paste(區鄉鎮名稱, 村里名稱, sep = '')) %>% 
  select(縣市名稱, 行政區域) -> ks_village

ks_village %>% write.csv('高雄市村里清單.csv',row.names=F)
