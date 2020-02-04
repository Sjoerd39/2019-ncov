########
library(tidyverse)
library(googlesheets4)
library(janitor)

# source https://docs.google.com/spreadsheets/d/1yZv9w9zRKwrGTaR-YzmAqMefw4wMlaXocejdxZaTs6w/htmlview?sle=true#gid=1979823633

## loading in base
all_data <- read.csv("all_data.csv", stringsAsFactors = FALSE) %>% 
  mutate(last_update = as.POSIXct(last_update),
         day = as.Date(day))

## how many tabs the data contains up untill now
ntabs <- 19

## google sheets meta
sheets_id <- "1yZv9w9zRKwrGTaR-YzmAqMefw4wMlaXocejdxZaTs6w"

meta_data <- sheets_get(sheets_id)

## how many tabs in current source
max_tabs <- nrow(meta_data$sheets)

all_data_list <- list()

for (i in 1:(max_tabs-ntabs)) {
  
  gsheet <- read_sheet(sheets_id, sheet = i)
  
  all_data_list[[i]] <- gsheet
  
}


all_data_nw <- bind_rows(all_data_list) %>% 
  clean_names()

all_data_nw <- all_data_nw %>% 
  mutate(country_region = ifelse(is.na(country_region), country, country_region),
         day = ifelse(is.na(last_update), as.Date(date_last_updated), as.Date(last_update)),
         day = as.Date(day, origin = "1970-01-01"),
         hubei_ornot = ifelse(province_state != "Hubei" | is.na(province_state), "Outside Hubei", "Hubei"),
         china_ornot = ifelse(country_region != "Mainland China", "Outside China", "Mainland China"))

all_data <- all_data %>%
  bind_rows(all_data_nw)


#### calculations ####

all_data_max_day <- all_data %>% 
  group_by(day, country_region, province_state) %>% 
  summarise_all(.funs = max) %>% 
  ungroup()

date_last_update <- max(all_data_max_day$day)

confirmed <- all_data_max_day %>% 
  filter(day == max(day)) %>% 
  tally(confirmed) %>% 
  unlist() %>% 
  as.numeric()

deaths <- all_data_max_day %>% 
  filter(day == max(day)) %>% 
  tally(deaths) %>% 
  unlist() %>% 
  as.numeric()

recovered <- all_data_max_day %>% 
  filter(day == max(day)) %>% 
  tally(recovered) %>% 
  unlist() %>% 
  as.numeric()

#### plot helpers ####

plot_labels1 <- all_data_max_day %>% 
  filter(day == max(day)) %>%
  mutate(hubei_china_world = ifelse(hubei_ornot == "Hubei", "Hubei",
                                    ifelse(china_ornot == "Mainland China", "Mainland China", "Rest of the world"))) %>% 
  group_by(day, hubei_china_world) %>% 
  summarise(confirmed = sum(confirmed, na.rm = TRUE))

plot_labels2 <- all_data_max_day %>% 
  filter(day == max(day)) %>%
  group_by(day, china_ornot) %>% 
  summarise(confirmed = sum(confirmed, na.rm = TRUE))

plot_labels3 <- all_data_max_day %>% 
  filter(day == max(day)) %>%
  group_by(day) %>% 
  summarise(confirmed = sum(confirmed, na.rm = TRUE))

