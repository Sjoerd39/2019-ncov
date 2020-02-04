library(tidyverse)
library(googlesheets4)
library(janitor)
library(maps)
library(maptools)
library(mapdata)

# https://docs.google.com/spreadsheets/d/1wQVypefm946ch4XDp37uZ-wartW4V7ILdg-qYiDXUHM/edit?usp=sharing
# Time series google sheet: https://docs.google.com/spreadsheets/d/1UF2pSkFTURko2OvfHWWlFpDFAr1UxCBA4JLwlSP6KFo/edit?usp=sharing

# source: https://docs.google.com/spreadsheets/d/1yZv9w9zRKwrGTaR-YzmAqMefw4wMlaXocejdxZaTs6w/htmlview?sle=true#gid=1979823633
# https://en.wikipedia.org/wiki/Timeline_of_the_2019%E2%80%9320_Wuhan_coronavirus_outbreak
# https://en.wikipedia.org/wiki/Template:2019-20_Wuhan_coronavirus_data/China_medical_cases
# https://en.wikipedia.org/wiki/Template:2019-20_Wuhan_coronavirus_data/International_medical_cases
# https://www.reddit.com/r/dataisbeautiful/comments/ey4bma/oc_feb_03_update_on_timelapse_of_confirmed_wuhan/


sheets_id <- "1yZv9w9zRKwrGTaR-YzmAqMefw4wMlaXocejdxZaTs6w"

meta_data <- sheets_get(sheets_id)

n_tabs <- nrow(meta_data$sheets)

all_data_list <- list()

for (i in 1:n_tabs) {
  
  gsheet <- read_sheet(sheets_id, sheet = i)
  
  all_data_list[[i]] <- gsheet
  
}


all_data <- bind_rows(all_data_list) %>% 
  clean_names()

all_data <- all_data %>% 
  mutate(country = ifelse(country == "China", "Mainland China", country),
         country = ifelse(country == "United States", "US", country),
         country_region = ifelse(is.na(country_region), country, country_region),
         deaths = ifelse(is.na(deaths), demised, deaths),
         day = ifelse(is.na(last_update), as.Date(date_last_updated), as.Date(last_update)),
         day = as.Date(day, origin = "1970-01-01"),
         hubei_ornot = ifelse(province_state != "Hubei" | is.na(province_state), "Outside Hubei", "Hubei"),
         china_ornot = ifelse(country_region != "Mainland China", "Outside China", "Mainland China")) %>% 
  select(-suspected, -demised, -country, -date_last_updated)

# write.csv(all_data, "2019-ncov/all_data.csv", row.names = FALSE)




all_data_max_day <- all_data %>% 
  group_by(day, country_region, province_state) %>% 
  summarise_all(.funs = max) %>% 
  ungroup()

  


all_data_max_day %>% 
  group_by(day) %>% 
  summarise(confirmed = sum(confirmed, na.rm = TRUE)) %>% 
  ggplot(aes(x = day, y = confirmed)) +
  geom_line() +
  geom_point()

all_data_max_day %>% 
  group_by(day, hubei_ornot) %>% 
  summarise(confirmed = sum(confirmed, na.rm = TRUE),
            deaths = sum(deaths, na.rm = TRUE)) %>% 
  ggplot(aes(x = day, y = confirmed, color = hubei_ornot)) +
  geom_line() +
  geom_point()


all_data_max_day %>%
  mutate(hubei_china_world = ifelse(hubei_ornot == "Hubei", "Hubei",
                                    ifelse(china_ornot == "Mainland China", "Mainland China", "Rest of the world"))) %>% 
  group_by(day, hubei_china_world) %>% 
  summarise(confirmed = sum(confirmed, na.rm = TRUE)) %>% 
  ggplot(aes(x = day, y = confirmed, color = hubei_china_world, label = confirmed)) +
  geom_line() +
  geom_point() +
  geom_label()


#### World map (excl antartica) ####

world_map <- map_data("world") %>% 
  filter(region != "Antarctica") %>% 
  ## 'correcting' country names according to provided 2019-ncov data
  mutate(region = ifelse(region == "China", "Mainland China", 
                         ifelse(region == "USA", "US",
                                ifelse(subregion == "Macau", "Macau",
                                       ifelse(subregion == "Hong Kong", "Hong Kong", region)))))

## add latest confirmed cases

world_map_cases <- world_map %>% 
  left_join(all_data_max_day %>% filter(day == max(day)) %>% group_by(country_region) %>% tally(confirmed),
            by = c("region" = "country_region"))


ggplot(world_map_cases, aes(x = long, y = lat, group = group, fill = n)) +
  geom_polygon(colour = "white") +
  theme_void()



## eu map
eu_countries <- c(
  "Portugal", "Spain", "France", "Switzerland", "Germany",
  "Austria", "Belgium", "UK", "Netherlands",
  "Denmark", "Poland", "Italy", 
  "Croatia", "Slovenia", "Hungary", "Slovakia",
  "Czech republic"
)


# Retrievethe map data
eu_map_cases <- world_map_cases %>% 
  filter(region %in% eu_countries)

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
eu_map_label <- eu_map_cases %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

ggplot(eu_map_cases, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = n))+
  geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
  # scale_fill_viridis()+
  theme_void()

#### map of China ####
library(rgdal)  
library(ggplot2)
require(maptools)
require(plyr)
library(svglite)
library(data.table)

# First read in the shapefile

china_map = readOGR(dsn=("C:/Users/Beheerder/Desktop/Projects/2020/corona virus/maps"), layer="bou2_4p")

china_province = setDT(china_map@data)
setnames(china_province, "NAME", "province")

# transform to UTF-8 coding format
china_province[, province:=iconv(province, from = "GBK", to = "UTF-8")]
# create id to join province back to lat and long, id = 0 ~ 924
china_province[, id:= .I-1] 
# there are more shapes for one province due to small islands
china_province[, province:= as.factor(province)]

dt_china = setDT(fortify(china_map))
dt_china[, id:= as.numeric(id)]

setkey(china_province, id); setkey(dt_china, id)
dt_china <- china_province[dt_china]

# make the province EN, CH label file
province_CH <- china_province[, levels(province)] # the CH are in UTF-8 code
province_EN <- c("Shanghai", "Yunnan", "Inner Mongolia", "Beijing", "Taiwan",
                 "Jilin", "Sichuan", "Tianjin City", "Ningxia", "Anhui",
                 "Shandong", "Shanxi", "Guangdong", "Guangxi", "Xinjiang",
                 "Jiangsu", "Jiangxi", "Hebei", "Henan", "Zhejiang",
                 "Hainan", "Hubei", "Hunan", "Gansu", "Fujian",
                 "Tibet", "Guizhou", "Liaoning", "Chongqing", "Shaanxi",
                 "Qinghai", "Hong Kong", "Heilongjiang"
)

# some population data (from years ago too)

infected <- all_data_max_day %>% 
  filter(day == max(day), country_region %in% c("Mainland China", "Taiwan", "Hong Kong")) %>% 
  select(confirmed, province_state) %>% 
  mutate(province_state = ifelse(province_state == "Tianjin", "Tianjin City", province_state)) %>% 
  as.data.table()
  
setkey(infected, province_state)

input_data <- data.table(province_CH, province_EN)
setkey(input_data, province_EN)

input_data <- infected[input_data]

setkey(input_data, province_CH)
setkey(dt_china, province)

# remove small islands on the South China Sea
china_map_infected <- input_data[dt_china[AREA>0.1,]]

# create label file of province `label_dt`
label_dt <- china_map_infected[, .(x = mean(range(long)), y = mean(range(lat)), province_state, province_CH), by = id]
label_dt <- unique(label_dt)
setkey(label_dt, province_state)
# I have fine-tuned the label position of some provinces
label_dt['Inner Mongolia', `:=` (x = 110, y = 42)]
label_dt['Gansu', `:=` (x = 96.3, y = 40)]
label_dt['Hebei', `:=` (x = 115.5, y = 38.5)]
label_dt['Liaoning', `:=` (x = 123, y = 41.5)]

# data look like this: 
# plot
ggplot(china_map_infected, aes(x = long, y = lat, group = group, fill=confirmed)) +
  labs(fill = "amount infected")+
  geom_polygon()+
  geom_path()+
  scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90",
                       guide = guide_colourbar(barwidth = 0.8, barheight = 10)) +
  theme_void() +
  geom_text(data = label_dt, aes(x=x, y=y, label = province_state),inherit.aes = F)
  scalebar(data = china_map_pop, dist = 500, dist_unit = "km",
           transform = T, model = "WGS84",
           border.size = 0.4, st.size = 2)


#### map of China excl Hubei province####

  infected <- all_data_max_day %>% 
    filter(day == max(day), country_region %in% c("Mainland China", "Taiwan", "Hong Kong")) %>% 
    filter(province_state != "Hubei") %>% 
    select(confirmed, province_state) %>% 
    mutate(province_state = ifelse(province_state == "Tianjin", "Tianjin City", province_state)) %>% 
    as.data.table()
  
  setkey(infected, province_state)
  
  input_data <- data.table(province_CH, province_EN)
  setkey(input_data, province_EN)
  
  input_data <- infected[input_data]
  
  setkey(input_data, province_CH)
  setkey(dt_china, province)
  
  # remove small islands on the South China Sea
  china_map_infected <- input_data[dt_china[AREA>0.1,]]
  
  # create label file of province `label_dt`
  label_dt <- china_map_infected[, .(x = mean(range(long)), y = mean(range(lat)), province_state, province_CH), by = id]
  label_dt <- unique(label_dt)
  setkey(label_dt, province_state)
  # I have fine-tuned the label position of some provinces
  label_dt['Inner Mongolia', `:=` (x = 110, y = 42)]
  label_dt['Gansu', `:=` (x = 96.3, y = 40)]
  label_dt['Hebei', `:=` (x = 115.5, y = 38.5)]
  label_dt['Liaoning', `:=` (x = 123, y = 41.5)]
  
  # data look like this: 
  # plot
  ggplot(china_map_infected, aes(x = long, y = lat, group = group, fill=confirmed)) +
    labs(fill = "People infected")+
    geom_polygon()+
    geom_path()+
    scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90",
                         guide = guide_colourbar(barwidth = 0.8, barheight = 10)) +
    theme_void() +
    geom_text(data = label_dt, aes(x=x, y=y, label = province_state),inherit.aes = F)
  
  