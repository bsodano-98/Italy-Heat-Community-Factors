#03. Population daily


# Calculate daily population

#---------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(stringr)
library(lubridate)
library(stringr)
library(ISOweek)

#Open datasets from pop11 to pop24
pop11 <- read.csv2("C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset SAS/pop11.csv", fileEncoding = "ISO-8859-1", sep = ",")
pop12 <- read.csv2("C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset SAS/pop12.csv", fileEncoding = "ISO-8859-1", sep = ",")
pop13 <- read.csv2("C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset SAS/pop13.csv", fileEncoding = "ISO-8859-1", sep = ",")
pop14 <- read.csv2("C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset SAS/pop14.csv", fileEncoding = "ISO-8859-1", sep = ",")
pop15 <- read.csv2("C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset SAS/pop15.csv", fileEncoding = "ISO-8859-1", sep = ",")
pop16 <- read.csv2("C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset SAS/pop16.csv", fileEncoding = "ISO-8859-1", sep = ",")
pop17 <- read.csv2("C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset SAS/pop17.csv", fileEncoding = "ISO-8859-1", sep = ",")
pop18 <- read.csv2("C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset SAS/pop18.csv", fileEncoding = "ISO-8859-1", sep = ",")
pop19 <- read.csv2("C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset SAS/pop19.csv", fileEncoding = "ISO-8859-1", sep = ",")
pop20 <- read.csv2("C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset SAS/pop20.csv", fileEncoding = "ISO-8859-1", sep = ",")
pop21 <- read.csv2("C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset SAS/pop21.csv", fileEncoding = "ISO-8859-1", sep = ",")
pop22 <- read.csv2("C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset SAS/pop22.csv", fileEncoding = "ISO-8859-1", sep = ",")
pop23 <- read.csv2("C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset SAS/pop23.csv", fileEncoding = "ISO-8859-1", sep = ",")
pop24 <- read.csv2("C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset SAS/pop24.csv", fileEncoding = "ISO-8859-1", sep = ",")

# add 0 in front of municipality codes that have less than 6 numbers in the code
# sometimes the code has some 0 sometimes not, need to uniform 

pad_code <- function(df) {
  df$Code <- str_pad(df$Code, width = 6, pad = "0")
  return(df)
}

pop11 <- pad_code(pop11)
pop12 <- pad_code(pop12)
pop13 <- pad_code(pop13)
pop14 <- pad_code(pop14)
pop15 <- pad_code(pop15)
pop16 <- pad_code(pop16)
pop17 <- pad_code(pop17)
pop18 <- pad_code(pop18)
pop19 <- pad_code(pop19)
pop20 <- pad_code(pop20)
pop21 <- pad_code(pop21)
pop22 <- pad_code(pop22)
pop23 <- pad_code(pop23)
pop24 <- pad_code(pop24)

# As the population is only available for the 1st of January of every year, we need to create a daily version
# to feed in the model. 
# We will assume population costant all the year


process_daily_data <- function(pop_data, year_value) {
  # Create a sequence of dates for the specified year
  date_seq <- seq.Date(from = as.Date(paste0(year_value, "-01-01")), 
                       to = as.Date(paste0(year_value, "-12-31")), 
                       by = "day")
  
  # Create the EUROSTAT_ISO data frame with the date sequence
  EUROSTAT_ISO <- data.frame(EURO_TIME = date_seq)
  
  # Expand grid for age, sex, Code, and date
  pop_daily <- expand.grid(age = c("less65", "65-74", "75-84", "85plus"), 
                           sex = c("M", "F"), 
                           Code = unique(pop_data$Code), 
                           date = unique(EUROSTAT_ISO$EURO_TIME))
  
  # Convert date to character and then back to Date format
  pop_daily$date <- as.character(pop_daily$date)
  pop_daily$date <- as.Date(pop_daily$date) # Ensure the correct date format
  
  # Merge with the population data
  pop_daily <- left_join(pop_daily, pop_data, by = c("age" = "Age", "sex" = "sex", "Code" = "Code"))
  
  return(pop_daily)
}

# Apply the function for each dataset from pop11 to pop24
pop11_daily <- process_daily_data(pop11, 2011)
pop12_daily <- process_daily_data(pop12, 2012)
pop13_daily <- process_daily_data(pop13, 2013)
pop14_daily <- process_daily_data(pop14, 2014)
pop15_daily <- process_daily_data(pop15, 2015)
pop16_daily <- process_daily_data(pop16, 2016)
pop17_daily <- process_daily_data(pop17, 2017)
pop18_daily <- process_daily_data(pop18, 2018)
pop19_daily <- process_daily_data(pop19, 2019)
pop20_daily <- process_daily_data(pop20, 2020)
pop21_daily <- process_daily_data(pop21, 2021)
pop22_daily <- process_daily_data(pop22, 2022)
pop23_daily <- process_daily_data(pop23, 2023)
pop24_daily <- process_daily_data(pop24, 2024)

#Store daily datasets from pop11_daiy to pop24_daily
saveRDS(pop11_daily, file ="C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset R/pop11_daily.rds")
saveRDS(pop12_daily, file ="C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset R/pop12_daily.rds")
saveRDS(pop13_daily, file ="C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset R/pop13_daily.rds")
saveRDS(pop14_daily, file ="C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset R/pop14_daily.rds")
saveRDS(pop15_daily, file ="C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset R/pop15_daily.rds")
saveRDS(pop16_daily, file ="C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset R/pop16_daily.rds")
saveRDS(pop17_daily, file ="C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset R/pop17_daily.rds")
saveRDS(pop18_daily, file ="C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset R/pop18_daily.rds")
saveRDS(pop19_daily, file ="C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset R/pop19_daily.rds")
saveRDS(pop20_daily, file ="C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset R/pop20_daily.rds")
saveRDS(pop21_daily, file ="C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset R/pop21_daily.rds")
saveRDS(pop22_daily, file ="C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset R/pop22_daily.rds")
saveRDS(pop23_daily, file ="C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset R/pop23_daily.rds")
saveRDS(pop24_daily, file ="C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/Dataset R/pop24_daily.rds")
######################################################################################