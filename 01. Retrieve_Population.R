###RETRIEVE POPULATION FROM ISTAT####

#Download population dataset

#---------------------------------------------------------------------------------

#From 2020 to 2023, there is a dataset for each year with all municipalities (4 datasets)
#For 2022, upload the dataset in SAS and then in R to be able to open it (it has problems if you try to open it in R)
#From 2002 to 2019, there is a dataset for each province with all the years (108 datasets)

#Libraries

library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(sf)
options(encoding = "ISO-8859-1")
library(readr)

#Add 0s to the municipality code 
#codici2024$codici2024 <- str_pad(codici2024$codici2024, width = 6, pad = "0")


################Step 1: retrieve population for the years 2020-2023#######################
# Population for the years 2020-2023 is available here: https://demo.istat.it/#sezione1
#To download the years 2020-2023, select the year (starting with 2020, then 2021, and so on up to 2023), go to the download area, 
#scroll to the bottom, and download the zip file "Comuni." You will get the POSAS_year_it_Comuni files
# We have a dataset for each year with all municipalities(4 datasets from 2020 to 2023)
# Totals (not divided by sex) are coded as 999 

# Skip 1st line as it is a table header with a description of the data
#Datasets from POSAS_2020_it_Comuni.csv to POSAS_2023_it_Comuni.csv
process_data <- function(year) {
  # Load the data for the given year
  file_path <- paste0("~/Per comune_2020_2023/POSAS_", year, "_it_Comuni.csv")
  
  pop <- read.csv2(file_path, skip = 1)
  
  # Rename columns
  colnames(pop)[3] <- 'Eta'
  
  # Select the relevant columns
  pop = pop %>% select(`Codice.comune`, `Comune`, `Totale.maschi`, `Totale.femmine`, `Eta`)
  
  # Bring together male and female population data
  pop <- pop %>%
    select(`Codice.comune`, `Comune`, `Totale.maschi`, `Eta`) %>%
    mutate(sex = "M") %>%
    rename(pop := `Totale.maschi`) %>%
    rbind(., 
          pop %>% select(`Codice.comune`, `Comune`, `Totale.femmine`, `Eta`) %>% 
            mutate(sex = "F") %>% 
            rename(pop := `Totale.femmine`)) %>%
    rename(Code := `Codice.comune`, 
           Municipality = Comune, 
           Age := `Eta`)
  
  # Remove rows with missing values
  pop <- pop[complete.cases(pop), ]
  
  # Remove the total age code (999) or "Totale"
  pop <- pop %>% filter(!Age %in% "999")
  
  # Set the year
  pop$year <- year
  
  # Aggregate by age group
  pop %>% 
    mutate(Age = as.numeric(Age)) %>%
    mutate(
      Age = cut(Age, breaks = c(-1, 64, 74, 84, 101), 
                labels = c("less65", "65-74", "75-84", "85plus"))
    ) %>%
    group_by(Code, Municipality, Age, sex, year) %>%
    summarise(pop = sum(as.numeric(pop))) -> pop_data
  
  return(pop_data)
}

# Run the function for each year
pop20 <- process_data(2020)
pop21 <- process_data(2021)
pop23 <- process_data(2023)
pop24 <- process_data(2024)

##########Csv of 2022 is different, so open and run it here:
pop <- read.csv("~/Per comune_2020_2024/POSAS_2022_it_Comuni.csv")

colnames(pop)[3] <- 'Eta'

# Select the relevant columns

pop = pop %>% select(Codice.comune, Comune, Totale.maschi, Totale.femmine, Eta)

# bring together
pop %>% select(Codice.comune, Comune, Totale.maschi, Eta) %>% 
  mutate(sex = "M") %>% 
  rename(pop := Totale.maschi) %>% 
  rbind(., 
        pop %>% select(Codice.comune, Comune, Totale.femmine, Eta) %>% 
          mutate(sex = "F") %>% 
          rename(pop := Totale.femmine)) %>% 
  rename(Code := Codice.comune, 
         Municipality = Comune, 
         Age := Eta) -> pop

pop <- pop[complete.cases(pop),]

# also remove the total age, coded as 999, sometimes coded as Totale
pop %>% filter(!Age %in% "999") -> pop

pop$year <- 2022

# aggregate by age group
pop %>% 
  mutate(Age = as.numeric(Age)) %>% 
  mutate(
    Age = cut(Age, breaks = c(-1, 64, 74, 84, 101), 
              labels = c("less65", "65-74", "75-84", "85plus"))
  ) %>% 
  group_by(Code, Municipality, Age, sex, year) %>% 
  summarise(pop = sum(as.numeric(pop))) -> pop22 

#Store datasets from pop20 to pop24

#############Step 2: retrieve population for the years from 2011 to 2019############################################
# Population for the years 2011-2019 is available here: https://demo.istat.it/ricostruzione/download.php?lingua=ita
#To download the files for 2011-2019, go to the section "Intercensus reconstruction of the resident population by age as of January 1st, 
#years 2002-2019, by territory" and download the zip file "Comuni Intercensus reconstruction of the resident population by age as of January 1st, 
#years 2002-2019: Comuni."
# There is a dataset for each province with all the years (108 datasets)

# Specify path of the folder with all the CSV files downloaded from ISTAT (108 datasets)
cartella <- "~/PopolazioneEta-Territorio-Comuni_2002_2019"

file_csv <- list.files(path = cartella, pattern = "PopolazioneEta-Territorio-ComuniProvincia_.*\\.csv", full.names = TRUE)

# List to save final datasets
dataset_finali <- list()

for (file in file_csv) {
  nome_provincia <- sub(".*_([^_]+)\\.csv$", "\\1", basename(file))
  dati <- read.csv(file, sep = ";", header = FALSE, 
                   skip=1)
  
  # Dinamic strings
  stringa_inizio <- paste("Tutte le cittadinanze - Anno: 2011 - Provincia:", nome_provincia)
  stringa_fine <- paste("Cittadinanza italiana - Anno: 2002 - Provincia:", nome_provincia)
  
  colnames(dati)[1] <- "Territorio/Eta"
  
  # rename the columns:
  colnames(dati)[colnames(dati) %in% paste0("V", 3:103)] <- 3:103
  
  colnames(dati)[3:103]<- 0:100
  
  colnames(dati)[2]<-"X" 
  
  dati <- dati[(which(dati$`Territorio/Eta` == stringa_inizio)):(which(dati$`Territorio/Eta` == stringa_fine)), ]
  
  which.keep <- substr(dati$`Territorio/Eta`, 1, stop = 1) == '0' | substr(dati$`Territorio/Eta`, 1, stop = 1) == '1'
  
  dati$`Territorio/Eta`[which.keep] %>% as.numeric() %>% unique() %>% length() -> n.dat
  
  lapply(c("Maschi", "Femmine"), function(Y){
    
    lapply(which(dati$`0` %in% Y), function(X) seq(from = X+1, to  = X + n.dat, by = 1)) -> list.sex
    
    dati_sex <- NULL
    
    for(i in 1:length(list.sex)){
      dati_sex_loop <- dati[list.sex[[i]],]
      dati_sex_loop$year <- 2010+i
      dati_sex <- rbind(dati_sex, dati_sex_loop)
    }
    
    return(dati_sex)
  }
  ) -> pop.sex
  
  pop.sex[[1]]$sex <- "M"
  pop.sex[[2]]$sex <- "F"
  
  dati <- rbind(pop.sex[[1]], pop.sex[[2]])
  
  dati <- gather(dati, Age, pop, `0`:`100`)
  
  colnames(dati)[1] <- "Code"
  
  
dati %>% 
    mutate(Age = as.numeric(Age)) %>% 
    mutate(
      Age = cut( Age, breaks = c(-1, 64, 74, 84, 101), 
                 labels = c("less65", "65-74", "75-84", "85plus"))
    ) %>% 
    group_by(Code, X, Age, sex, year) %>% 
    summarise(pop = sum(as.numeric(pop))) -> dati
  
  #Rename colunms and add the column of the municipality
  
  dati$Province <- nome_provincia
  colnames(dati)[2] <- "Municipality"
  
  
  # Save dataset in the list
  dataset_finali[[nome_provincia]] <- dati
}

# Bind all the dataset in a unique dataframe
dataset_unico <- do.call(rbind, dataset_finali)

View(dataset_unico)

#store dataset dataset_unico

############Step 3: Valle d'Aosta, Forlì-Cesena e Bolzano cannot be read with the loop, so  here create the extra ones
#Put the 3 datasets of the 3 municipalities in a folder

#########################Aosta
#Open csv of Aosta
extra <- read.csv2("C:/Users/barba/OneDrive/Desktop/IMPERIAL/Dati/PopolazioneEta-Territorio-ComuniProvincia_Valle d'Aosta.csv",
                   sep = ";", header = FALSE, 
                   skip=1) 

colnames(extra)[1] <- "Territorio/Eta"

# rename the columns:
colnames(extra)[colnames(extra) %in% paste0("V", 3:103)] <- 3:103
colnames(extra)[3:103]<- 0:100
colnames(extra)[2]<-"X"


# We are interested in all nationalities and the years 2011:2019

extra <- extra[(which(extra$`Territorio/Eta` == "Tutte le cittadinanze - Anno: 2011 - Provincia: Valle d'Aosta/Vallée d'Aoste")):
                 (which(extra$`Territorio/Eta` == "Cittadinanza italiana - Anno: 2002 - Provincia: Valle d'Aosta/Vallée d'Aoste")), ]

which.keep <- substr(extra$`Territorio/Eta`, 1, stop = 1) == '0' | substr(extra$`Territorio/Eta`, 1, stop = 1) == '1'
extra$`Territorio/Eta`[which.keep] %>% as.numeric() %>% unique() %>% length() -> n.dat

# Seperate by sex and bring together
lapply(c("Maschi", "Femmine"), function(Y){
  
  lapply(which(extra$`0` %in% Y), function(X) seq(from = X+1, to  = X + n.dat, by = 1)) -> list.sex
  
  extra_sex <- NULL
  
  for(i in 1:length(list.sex)){
    extra_sex_loop <- extra[list.sex[[i]],]
    extra_sex_loop$year <- 2010+i
    extra_sex <- rbind(extra_sex, extra_sex_loop)
  }
  return(extra_sex)
}
) -> pop.sex

pop.sex[[1]]$sex <- "M"
pop.sex[[2]]$sex <- "F"

extra <- rbind(pop.sex[[1]], pop.sex[[2]])

# and make long format
asti <- gather(extra, Age, pop, `0`:`100`)
colnames(extra)[c(1:2)] <- c("Code", "Municipality") 

extra %>% 
  mutate(Age = as.numeric(Age)) %>% 
  mutate(
    Age = cut( Age, breaks = c(-1, 64, 74, 84, 101), 
               labels = c("less65", "65-74", "75-84", "85plus"))
  ) %>% 
  group_by(Code, Municipality, Age, sex, year) %>% 
  summarise(pop = sum(as.numeric(pop))) -> aosta


######################Forli
#Open csv of Forli-Cesena
extra <- read.csv2("~/PopolazioneEta-Territorio-ComuniProvincia_Forlì-Cesena.csv",
                   sep = ";", header = FALSE, 
                   skip=1) 

colnames(extra)[1] <- "Territorio/Eta"

# rename the columns:
colnames(extra)[colnames(extra) %in% paste0("V", 3:103)] <- 3:103
colnames(extra)[3:103]<- 0:100
colnames(extra)[2]<-"X"


# We are interested in all nationalities and the years 2011:2019
#Here change the string putting the name of the "Provincia:"

extra <- extra[(which(extra$`Territorio/Eta` == "Tutte le cittadinanze - Anno: 2011 -Provincia: Forlì-Cesena")):
                 (which(extra$`Territorio/Eta` == "Cittadinanza italiana - Anno: 2002 - Provincia: Forlì-Cesena")), ]

which.keep <- substr(extra$`Territorio/Eta`, 1, stop = 1) == '0' | substr(extra$`Territorio/Eta`, 1, stop = 1) == '1'
extra$`Territorio/Eta`[which.keep] %>% as.numeric() %>% unique() %>% length() -> n.dat

# Seperate by sex and bring together
lapply(c("Maschi", "Femmine"), function(Y){
  
  lapply(which(extra$`0` %in% Y), function(X) seq(from = X+1, to  = X + n.dat, by = 1)) -> list.sex
  
  extra_sex <- NULL
  
  for(i in 1:length(list.sex)){
    extra_sex_loop <- extra[list.sex[[i]],]
    extra_sex_loop$year <- 2010+i
    extra_sex <- rbind(extra_sex, extra_sex_loop)
  }
  return(extra_sex)
}
) -> pop.sex

pop.sex[[1]]$sex <- "M"
pop.sex[[2]]$sex <- "F"

extra <- rbind(pop.sex[[1]], pop.sex[[2]])

# and make long format
asti <- gather(extra, Age, pop, `0`:`100`)
colnames(extra)[c(1:2)] <- c("Code", "Municipality") 

extra %>% 
  mutate(Age = as.numeric(Age)) %>% 
  mutate(
    Age = cut( Age, breaks = c(-1, 64, 74, 84, 101), 
               labels = c("less65", "65-74", "75-84", "85plus"))
  ) %>% 
  group_by(Code, Municipality, Age, sex, year) %>% 
  summarise(pop = sum(as.numeric(pop))) -> forli


#################Bolzano
#Open csv of Bolzano
extra <- read.csv2("~/PopolazioneEta-Territorio-ComuniProvincia_Bolzano-Bozen.csv",
                      sep = ";", header = FALSE, 
                      skip=1) 
colnames(extra)[1] <- "Territorio/Eta"

# rename the columns:
colnames(extra)[colnames(extra) %in% paste0("V", 3:103)] <- 3:103
colnames(extra)[3:103]<- 0:100
colnames(extra)[2]<-"X"


# We are interested in all nationalities and the years 2011:2019
#Here change the string putting the name of the "Provincia:"

extra <- extra[(which(extra$`Territorio/Eta` == "Tutte le cittadinanze - Anno: 2011 - Provincia: Bolzano/Bozen")):
                       (which(extra$`Territorio/Eta` == "Cittadinanza italiana - Anno: 2002 - Provincia: Bolzano/Bozen")), ]

which.keep <- substr(extra$`Territorio/Eta`, 1, stop = 1) == '0' | substr(extra$`Territorio/Eta`, 1, stop = 1) == '1'
extra$`Territorio/Eta`[which.keep] %>% as.numeric() %>% unique() %>% length() -> n.dat

# Seperate by sex and bring together
lapply(c("Maschi", "Femmine"), function(Y){
  
  lapply(which(extra$`0` %in% Y), function(X) seq(from = X+1, to  = X + n.dat, by = 1)) -> list.sex
  
  extra_sex <- NULL
  
  for(i in 1:length(list.sex)){
    extra_sex_loop <- extra[list.sex[[i]],]
    extra_sex_loop$year <- 2010+i
    extra_sex <- rbind(extra_sex, extra_sex_loop)
  }
  return(extra_sex)
}
) -> pop.sex

pop.sex[[1]]$sex <- "M"
pop.sex[[2]]$sex <- "F"

extra <- rbind(pop.sex[[1]], pop.sex[[2]])

# and make long format
asti <- gather(extra, Age, pop, `0`:`100`)
colnames(extra)[c(1:2)] <- c("Code", "Municipality") 

extra %>% 
  mutate(Age = as.numeric(Age)) %>% 
  mutate(
    Age = cut( Age, breaks = c(-1, 64, 74, 84, 101), 
               labels = c("less65", "65-74", "75-84", "85plus"))
  ) %>% 
  group_by(Code, Municipality, Age, sex, year) %>% 
  summarise(pop = sum(as.numeric(pop))) -> bolzano

### Step 4: Rbind to add also the municipalities of the Step 3 (bolzano, forli, aosta)

dataset_unico <- rbind(dataset_unico, bolzano, forli, aosta)


################## Step 5: Create a unique dataset adding 2020-2024 (pop20-pop24) to 2011-2019 (dataset_unico)

process_and_bind <- function(df, dataset) {
  df$Code <- as.character(df$Code)  
  dataset <- dataset[, colnames(df)]  
  dataset <- rbind(dataset, df)  
  return(dataset)
}

dataset_unico <- process_and_bind(pop20, dataset_unico)
dataset_unico <- process_and_bind(pop21, dataset_unico)
dataset_unico <- process_and_bind(pop22, dataset_unico)
dataset_unico <- process_and_bind(pop23, dataset_unico)
dataset_unico <- process_and_bind(pop24, dataset_unico)


#Store the dataset dataset_unico
saveRDS(dataset_unico, "~/dataset_unico.rds")


#Warning: the number of rows for the datasets of different years is different because
# there is a different number of municipalities between different years --> need to change and obtain same number
# of municipalities for all the years


