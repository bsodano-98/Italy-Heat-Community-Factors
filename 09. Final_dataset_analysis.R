########
# Create final dataset dat_65pl_bio for the analysis

#Extract summer months and merge pop_deaths files, temperature files and 
#shapefile (name of municipalities and other codes)

# Extract summer months (may-september) from temperature and save
temp <- readRDS("C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset R/temperature_XX/temperature_2013.rds")

# Verify date is a date
temp$date <- as.Date(temp$date)

# Extract the month from date 
temp$month <- as.numeric(format(temp$date, "%m"))

#Filter for may-september
library(dplyr)

temp <- temp %>%
  filter(month %in% 5:9)

#Save file temperature in summer months
saveRDS(temp, file ="C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset R/temp_summerXX/temp_summer2013.rds")

##################################################################################
#Extract summer months from pop_deaths datasets
deaths <- readRDS("C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset R/pop_deaths_XX/pop_deaths_2023.rds")

# Verify date is a date
deaths$date <- as.Date(deaths$date)

# Extract the month from date 
deaths$month <- as.numeric(format(deaths$date, "%m"))

#Filter for may-september
library(dplyr)

deaths <- deaths %>%
  filter(month %in% 5:9)

#Save file temperature in summer months
saveRDS(deaths, file ="C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset R/popdeaths_summerXX/popdeaths_summer2023.rds")

###################################################################################
#Link between pop/deaths and daily temperature in summer months, one dataset per month
pop <- readRDS("C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset R/popdeaths_summerXX/popdeaths_summer2024.rds")
temp <- readRDS("C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset R/temp_summerXX/temp_summer2024.rds")

pop <- pop %>% select(-month, -Municipality)
temp <- temp %>% select(-month)

pop$date <- as.Date(pop$date)
temp$date <- as.Date(temp$date)

# Unione dei dataset con left_join
daily <- pop %>%
  left_join(temp, by = c("date", "Code" = "mun"))


#Open shapefile, create a dataset with code municipality and name of the municipality, link by code
library(sf)
shp <- read_sf("C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset R/Shapefile_modificato/shp.shp")
#7895 municipalities

# Rimuovi la colonna 'geometry' (spaziale) e ottieni solo le colonne attributive
shp <- st_drop_geometry(shp)

shp <- shp %>% select(PRO_COM_T, COMUNE, COD_REG, COD_PROV)

#Add values for REG e PROV 
shp <- shp %>%
  mutate(
    COD_REG = ifelse(PRO_COM_T == "025075", 5, COD_REG),
    COD_PROV = ifelse(PRO_COM_T == "025075", 25, COD_PROV),
    COD_REG = ifelse(PRO_COM_T == "028108", 5, COD_REG),
    COD_PROV = ifelse(PRO_COM_T == "028108", 28, COD_PROV),
    COD_REG = ifelse(PRO_COM_T == "024128", 5, COD_REG),
    COD_PROV = ifelse(PRO_COM_T == "024128", 24, COD_PROV),
    COD_REG = ifelse(PRO_COM_T == "081021", 19, COD_REG),
    COD_PROV = ifelse(PRO_COM_T == "081021", 81, COD_PROV)
  )

# Union with the left_join
daily_2024 <- daily %>%
  left_join(shp, by = c("Code" = "PRO_COM_T"))

#Add holidays to the daily datasets
daily_2011 <- readRDS("C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset R/Deaths_temperature/daily_2011.rds")

daily_2011 <- daily_2011 %>%
  left_join(holiday_df, by = c("date" = "Data"))

daily_2011 <- daily_2011 %>%
  mutate(holiday = replace(holiday, is.na(holiday), 0))

#Now add relative humidity for 2011-2023
#Aggiungo la relative humidity separatamente per i dataset di daily
# 153 gg may-september *13*7895
library(stringr)
Italy_municipalities_relative_humidity_2011_2023$PRO_COM_T <- str_pad(Italy_municipalities_relative_humidity_2011_2023$PRO_COM_T, width = 6, pad = "0")
Italy_municipalities_relative_humidity_2011_2023$date <- as.Date(Italy_municipalities_relative_humidity_2011_2023$date)

daily_2023 <- daily_2023 %>%
  left_join(Italy_municipalities_relative_humidity_2011_2023, by = c("date"="date", "Code" = "PRO_COM_T"))

#Save daily_XX
saveRDS(daily_2023, file ="C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset R/Deaths_temperature/daily_2023.rds")

daily_2013$date <- as.Date(daily_2013$date, format = "%Y-%m-%d")

# Keep only extended summer (for lag construction)
df_filtrato <- daily_2013 %>%
  filter(date >= ymd("2013-05-27") & date <= ymd("2013-09-05"))

# Aggregate by sex 
df_ridotto <- df_filtrato %>%
  group_by(Code, date, age, year , ID, lon, lat, temperature, X, Y, COMUNE, COD_REG, COD_PROV, holiday, rh_mean) %>%  
  summarise(
    deaths = sum(deaths),
    pop = sum(pop),
    .groups = "drop"            
  )

# Aggregate by age-group (<65 and 65+)
df_ridotto <- df_ridotto %>%
  mutate(age = case_when(
    age == "less65" ~ "less65",              
    age %in% c("65-74", "75-84", "85plus") ~ "65+",  
    TRUE ~ age                                 
  )) %>%
  group_by(Code, date, age, year , ID, lon, lat, temperature, X, Y, COMUNE, COD_REG, COD_PROV, holiday, rh_mean) %>%  
  summarise(
    deaths = sum(deaths),  
    pop = sum(pop),        
    .groups = "drop"             
  )

# ---- LAGS ----

df <- df_ridotto %>%
  arrange(date) %>%
  group_by(Code, age) %>%
  mutate(
    lag0_t = lag(temperature, 0),
    lag1_t = lag(temperature, 1),
    lag2_t = lag(temperature, 2),
    lag3_t = lag(temperature, 3),
    lag0_rh = lag(rh_mean, 0),
    lag1_rh = lag(rh_mean, 1),
    lag2_rh = lag(rh_mean, 2),
    lag3_rh = lag(rh_mean, 3)
  ) %>%
  ungroup()

# Mean of lags
df$lag_t_mean <- rowMeans(df[, c("lag0_t", "lag1_t", "lag2_t", "lag3_t")], na.rm = TRUE)
df$lag_rh_mean <- rowMeans(df[, c("lag0_rh", "lag1_rh", "lag2_rh", "lag3_rh")], na.rm = TRUE)

# Keep only core summer
dat_2013 <- df %>%
  filter(date >= ymd("2013-06-01") & date <= ymd("2013-08-31"))


### ---- ADD TIME VARIABLES (example for 2023) ----

dat_2023$date <- as.Date(dat_2023$date)

inizio_anno <- as.Date("2023-06-01") 

dat_2023$doy <- as.integer(dat_2023$date - inizio_anno) + 1

dat_2023$dow <- as.integer(format(dat_2023$date, "%u"))


### ---- CONCATENATE ----

dat_65pl_bio <- bind_rows(
  dat_2011, dat_2012, dat_2013, dat_2014, dat_2015,
  dat_2016, dat_2017, dat_2018, dat_2019, dat_2020,
  dat_2021, dat_2022, dat_2023
)


### ---- CREATE IDS ----

dat_65pl_bio$id_region <- as.numeric(factor(dat_65pl_bio$Code))

dat_65pl_bio$id_region1 <- dat_65pl_bio$id_region
dat_65pl_bio$id_region2 <- dat_65pl_bio$id_region
dat_65pl_bio$id_region3 <- dat_65pl_bio$id_region
dat_65pl_bio$id_region4 <- dat_65pl_bio$id_region

dat_11_23$id.year <- dat_11_23$year - 2010


### ---- SAVE FINAL DATASET ----

saveRDS(
  dat_65pl_bio,
  file ="C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset R/pop_deaths_temp_lag/dat_65pl_bio.rds.rds"
)