# Retrieve and clean the mortality file in Italy. 


#---------------------------------------------------------------------------------

# One can download the mortality data for 2011-2024 at 
# https://www.istat.it/it/archivio/240401. We selected the file that includes
# deaths till end of January. After downloading this file, put it in the data folder.
#Dataset with daily deaths (in .csv format) for each individual municipality of residence  
#by sex and five-year age groups | January 1, 2011 – October 31, 2024

library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(stringr)
library(lubridate)

#Open dataset deaths 2011-2024
deaths <- read_csv("C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset-decessi-comunali-giornalieri-4/comuni_giornaliero_31ottobre24.csv")

# subset the dataset
deaths %>% select_at(
  vars("COD_PROVCOM", "NOME_COMUNE", "CL_ETA", "GE",
       paste0("M_", 11:24), 
       paste0("F_", 11:24))
) -> deaths

# Change to long format
deaths <- gather(deaths, agesex, deaths, M_11:F_24, factor_key=TRUE)

deaths %>% mutate(
  sex = substr(agesex, start = 1, stop = 1),
  year = as.numeric(paste0("20",substr(agesex, start = 3, stop = 4)))
) -> deaths

deaths$agesex <- NULL

# Fix the age. The CL_ETA is the age variable denoting the following age groups:

# 0=0
# 1=1-4
# 2=5-9
# 3=10-14
# 4=15-19
# 5=20-24
# 6=25-29
# 7=30-34
# 8=35-39
# 9=40-44
# 10=45-49
# 11=50-54
# 12=55-59
# 13=60-64
# 14=65-69
# 15=70-74
# 16=75-79
# 17=80-84
# 18=85-89
# 19=90-94
# 20=95-99
# 21=100+
# see also https://www.istat.it/it/archivio/240401

#We create 4 age groups "less65", "65-74", "75-84", "85plus"

deaths$age <- NA
deaths$age[deaths$CL_ETA %in% 0:13] <- "less65"
deaths$age[deaths$CL_ETA %in% 14:15] <- "65-74"
deaths$age[deaths$CL_ETA %in% 16:17] <- "75-84"
deaths$age[deaths$CL_ETA %in% 18:21] <- "85plus"
deaths$CL_ETA <- NULL


# Fix the date
deaths %>% mutate(
  date = paste0(year, "-", 
                substr(GE, start = 1, stop = 2), "-", 
                substr(GE, start = 3, stop = 4))
) %>% mutate(date = as.Date(date)) -> deaths

deaths <- deaths[!is.na(deaths$date),] # the NAs are "false" leap years


#Separate each year to create separate datasets from 2011 to 2024

#Run the following code for every year from 2011 to 2024

#--------------------------------------------------------------------------------
deaths_2024 <- deaths %>%
  filter(year == 2024)

# Replace "n.d." with NA
deaths_2024$deaths[deaths_2024$deaths == "n.d."] <- NA
sum(is.na(deaths_2024$deaths))

deaths_2024 <- deaths_2024 %>% filter(!is.na(deaths))
missing <- deaths_2024 %>% filter(is.na(deaths))

# Aggregate by day and age group
deaths_2024 %>% select(date, COD_PROVCOM, sex, age, deaths, year) %>% 
  group_by(COD_PROVCOM, sex, age, date) %>% 
  summarise(deaths = sum(as.numeric(deaths), na.rm = TRUE)) -> deaths_2024

#Merge Trapani and Misiliscemi (in the death dataset, Misiliscemi is always included)
#081021 (Trapani) + 081025 (Misiliscemi) = 081021 (Trapani) 
deaths_2024$deaths <- as.numeric(deaths_2024$deaths)

deaths_2024 <- deaths_2024 %>%
  filter(COD_PROVCOM %in% c("081021", "081025")) %>%
  group_by(age, sex, date) %>%
  summarise(deaths = sum(deaths), .groups = "drop") %>%
  mutate(COD_PROVCOM = "081021",
         NOME_COMUNE = "Trapani",
         year = 2024) %>%
  bind_rows(deaths_2024 %>% filter(!COD_PROVCOM %in% c("081021", "081025")))
#----------------------------------------------------------------------------------

#Here we have deaths_2011, deaths_2012, deaths_2013, deaths_2014, deaths_2015, deaths_2016, 
#             deaths_2017, deaths_2018, deaths_2019, deaths_2020, deaths_2021, deaths_2022,
#             deaths_2023, deaths_2024

#######################################################
#MERGE DEATHS AND POPULATION: for every year we merge deaths dataset and pop dataset

#Open pop11_daily, pop12_daily, pop13_daily, pop14_daily, pop15_daily, pop16_daily,
# pop17_daily, pop18_daily, pop19_daily, pop20_daily, pop21_daily, pop22_daily, pop23_daily, pop24_daily

#Run the following code for every year from 2011 to 2024

#--------------------------------------------------------------------------------
pop24_daily$date <- as.Date(pop24_daily$date)
deaths_2024$date <- as.Date(deaths_2024$date)

deaths_2024 <- deaths_2024 %>%
  select(date, sex, age, COD_PROVCOM, deaths)

# No duplicates
sum(duplicated(deaths_2024[c("date", "sex", "age", "COD_PROVCOM")]))  

# Left join
pop_deaths_2024 <- pop24_daily %>%
  left_join(deaths_2024, by = c("date" = "date", "sex" = "sex", "age" = "age", "Code" = "COD_PROVCOM"))


# Change NA's in 'deaths' with 0
pop_deaths_2024$deaths[is.na(pop_deaths_2024$deaths)] <- 0

#Store the dataset
saveRDS(pop_deaths_2024, file = "C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset R/pop_deaths_XX/pop_deaths_2024.rds")
#--------------------------------------------------------------------------------
#At the end have pop_deaths_2011   #pop_deaths_2016   #pop_deaths_2021
                #pop_deaths_2012   #pop_deaths_2017   #pop_deaths_2022
                #pop_deaths_2013   #pop_deaths_2018   #pop_deaths_2023
                #pop_deaths_2014   #pop_deaths_2019   #pop_deaths_2024
                #pop_deaths_2015   #pop_deaths_2020