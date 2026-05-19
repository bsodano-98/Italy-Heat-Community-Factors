#02. Cleaning municipalities 

#---------------------------------------------------------------------------------


library(dplyr)

#Open dataset dataset_unico created in 01.

#Split the dataset dataset_unico to have one dataset per year

df_split <- split(dataset_unico , dataset_unico$year)

for (year in 2011:2023) {
  assign(paste0("deaths", year), df_split[[as.character(year)]])
}

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

#########################################################
#Uniform the number of municipalities for every year to the municipalities at 2023, dataset from ISTAT

process_municipality <- function(df, code_to_merge, new_code, new_municipality, year_value) {
  df <- df %>%
    filter(Code %in% code_to_merge) %>%
    group_by(Age, sex) %>%
    summarise(pop = sum(pop), .groups = "drop") %>%
    mutate(Code = new_code,
           Municipality = new_municipality,
           year = year_value) %>%
    bind_rows(df %>% filter(!Code %in% code_to_merge))
  return(df)
}

#Clean POP23 
# 013199 (Ronago) + 013228 (Uggiate-Trevano) = 013256 (Uggiate con Ronago)
# 025002 (Alano di Piave) + 025070 (Quero Vas) = 025075 (Setteville)
# 028022 (Carceri) + 028098 (Vighizzolo d'Este) = 028108 (Santa Caterina d'Este)
# 024044 (Gambugliano) + 024103 (Sovizzo) = 024128 (Sovizzo)
# 018002 (Albaredo Arnaboldi) + 018026 (Campospinoso) = 018026 (Campospinoso Albaredo) (Albaredo è stato inglobato nel comune di Campospinoso)

# Apply for pop23 
pop23 <- process_municipality(pop23, c("013199", "013228"), "013256", "Uggiate con Ronago", 2023)
pop23 <- process_municipality(pop23, c("025002", "025070"), "025075", "Setteville", 2023)
pop23 <- process_municipality(pop23, c("028022", "028098"), "028108", "Santa Caterina d'Este", 2023)
pop23 <- process_municipality(pop23, c("024044", "024103"), "024128", "Sovizzo", 2023)
pop23 <- process_municipality(pop23, c("018002", "018026"), "018026", "Campospinoso Albaredo", 2023)


#Clean POP22
# 005079 (Moransengo) + 005110 (Tonengo) = 005122 (Moransengo-Tonengo)
# 012009 (Bardello) + 012018 (Bregano) + 012095 (Malgesso)= 012144 (Bardello con Malgesso e Bregano)
# 013199 (Ronago) + 013228 (Uggiate-Trevano) = 013256 (Uggiate con Ronago)
# 025002 (Alano di Piave) + 025070 (Quero Vas) = 025075 (Setteville)
# 028022 (Carceri) + 028098 (Vighizzolo d'Este) = 028108 (Santa Caterina d'Este)
# 024044 (Gambugliano) + 024103 (Sovizzo) = 024128 (Sovizzo)
# 018002 (Albaredo Arnaboldi) + 018026 (Campospinoso) = 018026 (Campospinoso Albaredo) (Albaredo è stato inglobato nel comune di Campospinoso)


# Apply for pop22 
pop22 <- process_municipality(pop22, c("005079", "005110"), "005122", "Morasengo-Tonengo", 2022)
pop22 <- process_municipality(pop22, c("012009", "012018", "012095"), "012144", "Bardello con Malgesso e Bregano", 2022)
pop22 <- process_municipality(pop22, c("013199", "013228"), "013256", "Uggiate con Ronago", 2022)
pop22 <- process_municipality(pop22, c("025002", "025070"), "025075", "Setteville", 2022)
pop22 <- process_municipality(pop22, c("028022", "028098"), "028108", "Santa Caterina d'Este", 2022)
pop22 <- process_municipality(pop22, c("024044", "024103"), "024128", "Sovizzo", 2022)
pop22 <- process_municipality(pop22, c("018002", "018026"), "018026", "Campospinoso Albaredo", 2022)


#Clean POP21 
# 041060 (Sassofeltrio) = 099031 (Sassofeltrio) (Change number)
# 041033 (Montecopiolo) = 099030 (Montecopiolo) (Change number)
# 081021 (Trapani) = 	081021 (Trapani) + 081025 (Misiliscemi) sum deaths for Trapani
# 005079 (Moransengo) + 005110 (Tonengo) = 005122 (Moransengo-Tonengo)
# 012009 (Bardello) + 012018 (Bregano) + 012095 (Malgesso)= 012144 (Bardello con Malgesso e Bregano)
# 013199 (Ronago) + 013228 (Uggiate-Trevano) = 013256 (Uggiate con Ronago)
# 025002 (Alano di Piave) + 025070 (Quero Vas) = 025075 (Setteville)
# 028022 (Carceri) + 028098 (Vighizzolo d'Este) = 028108 (Santa Caterina d'Este)
# 024044 (Gambugliano) + 024103 (Sovizzo) = 024128 (Sovizzo)
# 018002 (Albaredo Arnaboldi) + 018026 (Campospinoso) = 018026 (Campospinoso Albaredo) (Albaredo è stato inglobato nel comune di Campospinoso)

#Change the number for those 2 municipalities
#Sassofeltrio
pop21$Code <- gsub("041060", "099031", pop21$Code)
#Montecopiolo
pop21$Code <- gsub("041033", "099030", pop21$Code)

# Apply for pop21 
pop21 <- process_municipality(pop21, c("005079", "005110"), "005122", "Morasengo-Tonengo", 2021)
pop21 <- process_municipality(pop21, c("012009", "012018", "012095"), "012144", "Bardello con Malgesso e Bregano", 2021)
pop21 <- process_municipality(pop21, c("013199", "013228"), "013256", "Uggiate con Ronago", 2021)
pop21 <- process_municipality(pop21, c("025002", "025070"), "025075", "Setteville", 2021)
pop21 <- process_municipality(pop21, c("028022", "028098"), "028108", "Santa Caterina d'Este", 2021)
pop21 <- process_municipality(pop21, c("024044", "024103"), "024128", "Sovizzo", 2021)
pop21 <- process_municipality(pop21, c("018002", "018026"), "018026", "Campospinoso Albaredo", 2021)


#Clean POP11-20 
# 041060 (Sassofeltrio) = 099031 (Sassofeltrio) (Change number)
# 041033 (Montecopiolo) = 099030 (Montecopiolo) (Change number)
# 097085 (Vendrogno) + 097008 (Bellano) = 097008 (Bellano) (Vendrogno è stato inglobato in Bellano)
# 041032 (Monteciccardo) + 041044 (Pesaro = 041044 (Pesaro) (Monteciccardo è stato inglobato in Pesaro)
# 022167 (San Michele all'Adige) + 022080 (Faedo) = 022167 (San Michele all'Adige) (Faedo è stato inglobato nel comune di San Michele dell'Adige)
# 022041 (Carano) + 022070 (Daiano) + 022211 (Varena)= 022254 (Ville di Fiemme)
# 022027 (Brez) + 022030 (Cagnò) + 022063 (Cloz) + 022152 (Revò) + 022154 (Romallo) = 022253 (Novella)
# 097085 (Vendrogno) + 097008 (Bellano) = 097008 (Bellano) 
# 022046 (Castelfondo) + 022088 (Fondo) + 022111 (Malosco) = 022252 (Borgo d'Anaunia)
# 081021 (Trapani) = 	081021 (Trapani) + 081025 (Misiliscemi) here sum deaths in Trapani
# 005079 (Moransengo) + 005110 (Tonengo) = 005122 (Moransengo-Tonengo)
# 012009 (Bardello) + 012018 (Bregano) + 012095 (Malgesso)= 005122 (Bardello con Malgesso e Bregano)
# 013199 (Ronago) + 013228 (Uggiate-Trevano) = 013256 (Uggiate con Ronago)
# 025002 (Alano di Piave) + 025070 (Quero Vas) = 025075 (Setteville)
# 028022 (Carceri) + 028098 (Vighizzolo d'Este) = 028108 (Santa Caterina d'Este)
# 024044 (Gambugliano) + 024103 (Sovizzo) = 024128 (Sovizzo)
# 018002 (Albaredo Arnaboldi) + 018026 (Campospinoso) = 018026 (Campospinoso Albaredo) (Albaredo è stato inglobato nel comune di Campospinoso)


#Change numbers for Sassofeltrio and Montecopiolo
replace_code <- function(dataset, old_code, new_code) {
  dataset$Code <- gsub(old_code, new_code, dataset$Code)
  return(dataset)
}

pop11 <- replace_code(pop11, "041060", "099031")
pop12 <- replace_code(pop12, "041033", "099030")
pop13 <- replace_code(pop13, "041060", "099031")
pop14 <- replace_code(pop14, "041033", "099030")
pop15 <- replace_code(pop15, "041060", "099031")
pop16 <- replace_code(pop16, "041033", "099030")
pop17 <- replace_code(pop17, "041060", "099031")
pop18 <- replace_code(pop18, "041033", "099030")
pop19 <- replace_code(pop19, "041060", "099031")
pop20 <- replace_code(pop20, "041033", "099030")

# Bellano (097085 + 097008)
pop11 <- process_municipality(pop11, c("097085", "097008"), "097008", "Bellano", 2011)
pop12 <- process_municipality(pop12, c("097085", "097008"), "097008", "Bellano", 2012)
pop13 <- process_municipality(pop13, c("097085", "097008"), "097008", "Bellano", 2013)
pop14 <- process_municipality(pop14, c("097085", "097008"), "097008", "Bellano", 2014)
pop15 <- process_municipality(pop15, c("097085", "097008"), "097008", "Bellano", 2015)
pop16 <- process_municipality(pop16, c("097085", "097008"), "097008", "Bellano", 2016)
pop17 <- process_municipality(pop17, c("097085", "097008"), "097008", "Bellano", 2017)
pop18 <- process_municipality(pop18, c("097085", "097008"), "097008", "Bellano", 2018)
pop19 <- process_municipality(pop19, c("097085", "097008"), "097008", "Bellano", 2019)
pop20 <- process_municipality(pop20, c("097085", "097008"), "097008", "Bellano", 2020)

# Pesaro (041032 + 041044)
pop11 <- process_municipality(pop11, c("041032", "041044"), "041044", "Pesaro", 2011)
pop12 <- process_municipality(pop12, c("041032", "041044"), "041044", "Pesaro", 2012)
pop13 <- process_municipality(pop13, c("041032", "041044"), "041044", "Pesaro", 2013)
pop14 <- process_municipality(pop14, c("041032", "041044"), "041044", "Pesaro", 2014)
pop15 <- process_municipality(pop15, c("041032", "041044"), "041044", "Pesaro", 2015)
pop16 <- process_municipality(pop16, c("041032", "041044"), "041044", "Pesaro", 2016)
pop17 <- process_municipality(pop17, c("041032", "041044"), "041044", "Pesaro", 2017)
pop18 <- process_municipality(pop18, c("041032", "041044"), "041044", "Pesaro", 2018)
pop19 <- process_municipality(pop19, c("041032", "041044"), "041044", "Pesaro", 2019)
pop20 <- process_municipality(pop20, c("041032", "041044"), "041044", "Pesaro", 2020)

# San Michele all'Adige (022167 + 022080)
pop11 <- process_municipality(pop11, c("022167", "022080"), "022167", "San Michele all'Adige", 2011)
pop12 <- process_municipality(pop12, c("022167", "022080"), "022167", "San Michele all'Adige", 2012)
pop13 <- process_municipality(pop13, c("022167", "022080"), "022167", "San Michele all'Adige", 2013)
pop14 <- process_municipality(pop14, c("022167", "022080"), "022167", "San Michele all'Adige", 2014)
pop15 <- process_municipality(pop15, c("022167", "022080"), "022167", "San Michele all'Adige", 2015)
pop16 <- process_municipality(pop16, c("022167", "022080"), "022167", "San Michele all'Adige", 2016)
pop17 <- process_municipality(pop17, c("022167", "022080"), "022167", "San Michele all'Adige", 2017)
pop18 <- process_municipality(pop18, c("022167", "022080"), "022167", "San Michele all'Adige", 2018)
pop19 <- process_municipality(pop19, c("022167", "022080"), "022167", "San Michele all'Adige", 2019)
pop20 <- process_municipality(pop20, c("022167", "022080"), "022167", "San Michele all'Adige", 2020)

# Ville di Fiemme (022041 + 022070 + 022211)
pop11 <- process_municipality(pop11, c("022041", "022070", "022211"), "022254", "Ville di Fiemme", 2011)
pop12 <- process_municipality(pop12, c("022041", "022070", "022211"), "022254", "Ville di Fiemme", 2012)
pop13 <- process_municipality(pop13, c("022041", "022070", "022211"), "022254", "Ville di Fiemme", 2013)
pop14 <- process_municipality(pop14, c("022041", "022070", "022211"), "022254", "Ville di Fiemme", 2014)
pop15 <- process_municipality(pop15, c("022041", "022070", "022211"), "022254", "Ville di Fiemme", 2015)
pop16 <- process_municipality(pop16, c("022041", "022070", "022211"), "022254", "Ville di Fiemme", 2016)
pop17 <- process_municipality(pop17, c("022041", "022070", "022211"), "022254", "Ville di Fiemme", 2017)
pop18 <- process_municipality(pop18, c("022041", "022070", "022211"), "022254", "Ville di Fiemme", 2018)
pop19 <- process_municipality(pop19, c("022041", "022070", "022211"), "022254", "Ville di Fiemme", 2019)
pop20 <- process_municipality(pop20, c("022041", "022070", "022211"), "022254", "Ville di Fiemme", 2020)

# Novella (022027 + 022030 + 022063 + 022152 + 022154)
pop11 <- process_municipality(pop11, c("022027", "022030", "022063", "022152", "022154"), "022253", "Novella", 2011)
pop12 <- process_municipality(pop12, c("022027", "022030", "022063", "022152", "022154"), "022253", "Novella", 2012)
pop13 <- process_municipality(pop13, c("022027", "022030", "022063", "022152", "022154"), "022253", "Novella", 2013)
pop14 <- process_municipality(pop14, c("022027", "022030", "022063", "022152", "022154"), "022253", "Novella", 2014)
pop15 <- process_municipality(pop15, c("022027", "022030", "022063", "022152", "022154"), "022253", "Novella", 2015)
pop16 <- process_municipality(pop16, c("022027", "022030", "022063", "022152", "022154"), "022253", "Novella", 2016)
pop17 <- process_municipality(pop17, c("022027", "022030", "022063", "022152", "022154"), "022253", "Novella", 2017)
pop18 <- process_municipality(pop18, c("022027", "022030", "022063", "022152", "022154"), "022253", "Novella", 2018)
pop19 <- process_municipality(pop19, c("022027", "022030", "022063", "022152", "022154"), "022253", "Novella", 2019)
pop20 <- process_municipality(pop20, c("022027", "022030", "022063", "022152", "022154"), "022253", "Novella", 2020)

# Borgo d'Anaunia (022046 + 022088 + 022111)
pop11 <- process_municipality(pop11, c("022046", "022088", "022111"), "022252", "Borgo d'Anaunia", 2011)
pop12 <- process_municipality(pop12, c("022046", "022088", "022111"), "022252", "Borgo d'Anaunia", 2012)
pop13 <- process_municipality(pop13, c("022046", "022088", "022111"), "022252", "Borgo d'Anaunia", 2013)
pop14 <- process_municipality(pop14, c("022046", "022088", "022111"), "022252", "Borgo d'Anaunia", 2014)
pop15 <- process_municipality(pop15, c("022046", "022088", "022111"), "022252", "Borgo d'Anaunia", 2015)
pop16 <- process_municipality(pop16, c("022046", "022088", "022111"), "022252", "Borgo d'Anaunia", 2016)
pop17 <- process_municipality(pop17, c("022046", "022088", "022111"), "022252", "Borgo d'Anaunia", 2017)
pop18 <- process_municipality(pop18, c("022046", "022088", "022111"), "022252", "Borgo d'Anaunia", 2018)
pop19 <- process_municipality(pop19, c("022046", "022088", "022111"), "022252", "Borgo d'Anaunia", 2019)
pop20 <- process_municipality(pop20, c("022046", "022088", "022111"), "022252", "Borgo d'Anaunia", 2020)

# Morasengo-Tonengo (005079 + 005110)
pop11 <- process_municipality(pop11, c("005079", "005110"), "005122", "Morasengo-Tonengo", 2011)
pop12 <- process_municipality(pop12, c("005079", "005110"), "005122", "Morasengo-Tonengo", 2012)
pop13 <- process_municipality(pop13, c("005079", "005110"), "005122", "Morasengo-Tonengo", 2013)
pop14 <- process_municipality(pop14, c("005079", "005110"), "005122", "Morasengo-Tonengo", 2014)
pop15 <- process_municipality(pop15, c("005079", "005110"), "005122", "Morasengo-Tonengo", 2015)
pop16 <- process_municipality(pop16, c("005079", "005110"), "005122", "Morasengo-Tonengo", 2016)
pop17 <- process_municipality(pop17, c("005079", "005110"), "005122", "Morasengo-Tonengo", 2017)
pop18 <- process_municipality(pop18, c("005079", "005110"), "005122", "Morasengo-Tonengo", 2018)
pop19 <- process_municipality(pop19, c("005079", "005110"), "005122", "Morasengo-Tonengo", 2019)
pop20 <- process_municipality(pop20, c("005079", "005110"), "005122", "Morasengo-Tonengo", 2020)

# Bardello con Malgesso e Bregano (012009 + 012018 + 012095)
pop11 <- process_municipality(pop11, c("012009", "012018", "012095"), "012144", "Bardello con Malgesso e Bregano", 2011)
pop12 <- process_municipality(pop12, c("012009", "012018", "012095"), "012144", "Bardello con Malgesso e Bregano", 2012)
pop13 <- process_municipality(pop13, c("012009", "012018", "012095"), "012144", "Bardello con Malgesso e Bregano", 2013)
pop14 <- process_municipality(pop14, c("012009", "012018", "012095"), "012144", "Bardello con Malgesso e Bregano", 2014)
pop15 <- process_municipality(pop15, c("012009", "012018", "012095"), "012144", "Bardello con Malgesso e Bregano", 2015)
pop16 <- process_municipality(pop16, c("012009", "012018", "012095"), "012144", "Bardello con Malgesso e Bregano", 2016)
pop17 <- process_municipality(pop17, c("012009", "012018", "012095"), "012144", "Bardello con Malgesso e Bregano", 2017)
pop18 <- process_municipality(pop18, c("012009", "012018", "012095"), "012144", "Bardello con Malgesso e Bregano", 2018)
pop19 <- process_municipality(pop19, c("012009", "012018", "012095"), "012144", "Bardello con Malgesso e Bregano", 2019)
pop20 <- process_municipality(pop20, c("012009", "012018", "012095"), "012144", "Bardello con Malgesso e Bregano", 2020)

# Uggiate con Ronago (013199 + 013228)
pop11 <- process_municipality(pop11, c("013199", "013228"), "013256", "Uggiate con Ronago", 2011)
pop12 <- process_municipality(pop12, c("013199", "013228"), "013256", "Uggiate con Ronago", 2012)
pop13 <- process_municipality(pop13, c("013199", "013228"), "013256", "Uggiate con Ronago", 2013)
pop14 <- process_municipality(pop14, c("013199", "013228"), "013256", "Uggiate con Ronago", 2014)
pop15 <- process_municipality(pop15, c("013199", "013228"), "013256", "Uggiate con Ronago", 2015)
pop16 <- process_municipality(pop16, c("013199", "013228"), "013256", "Uggiate con Ronago", 2016)
pop17 <- process_municipality(pop17, c("013199", "013228"), "013256", "Uggiate con Ronago", 2017)
pop18 <- process_municipality(pop18, c("013199", "013228"), "013256", "Uggiate con Ronago", 2018)
pop19 <- process_municipality(pop19, c("013199", "013228"), "013256", "Uggiate con Ronago", 2019)
pop20 <- process_municipality(pop20, c("013199", "013228"), "013256", "Uggiate con Ronago", 2020)

# Setteville (025002 + 025070)
pop11 <- process_municipality(pop11, c("025002", "025070"), "025075", "Setteville", 2011)
pop12 <- process_municipality(pop12, c("025002", "025070"), "025075", "Setteville", 2012)
pop13 <- process_municipality(pop13, c("025002", "025070"), "025075", "Setteville", 2013)
pop14 <- process_municipality(pop14, c("025002", "025070"), "025075", "Setteville", 2014)
pop15 <- process_municipality(pop15, c("025002", "025070"), "025075", "Setteville", 2015)
pop16 <- process_municipality(pop16, c("025002", "025070"), "025075", "Setteville", 2016)
pop17 <- process_municipality(pop17, c("025002", "025070"), "025075", "Setteville", 2017)
pop18 <- process_municipality(pop18, c("025002", "025070"), "025075", "Setteville", 2018)
pop19 <- process_municipality(pop19, c("025002", "025070"), "025075", "Setteville", 2019)
pop20 <- process_municipality(pop20, c("025002", "025070"), "025075", "Setteville", 2020)

# Santa Caterina d'Este (028022 + 028098)
pop11 <- process_municipality(pop11, c("028022", "028098"), "028108", "Santa Caterina d'Este", 2011)
pop12 <- process_municipality(pop12, c("028022", "028098"), "028108", "Santa Caterina d'Este", 2012)
pop13 <- process_municipality(pop13, c("028022", "028098"), "028108", "Santa Caterina d'Este", 2013)
pop14 <- process_municipality(pop14, c("028022", "028098"), "028108", "Santa Caterina d'Este", 2014)
pop15 <- process_municipality(pop15, c("028022", "028098"), "028108", "Santa Caterina d'Este", 2015)
pop16 <- process_municipality(pop16, c("028022", "028098"), "028108", "Santa Caterina d'Este", 2016)
pop17 <- process_municipality(pop17, c("028022", "028098"), "028108", "Santa Caterina d'Este", 2017)
pop18 <- process_municipality(pop18, c("028022", "028098"), "028108", "Santa Caterina d'Este", 2018)
pop19 <- process_municipality(pop19, c("028022", "028098"), "028108", "Santa Caterina d'Este", 2019)
pop20 <- process_municipality(pop20, c("028022", "028098"), "028108", "Santa Caterina d'Este", 2020)

# Sovizzo (024044 + 024103)
pop11 <- process_municipality(pop11, c("024044", "024103"), "024128", "Sovizzo", 2011)
pop12 <- process_municipality(pop12, c("024044", "024103"), "024128", "Sovizzo", 2012)
pop13 <- process_municipality(pop13, c("024044", "024103"), "024128", "Sovizzo", 2013)
pop14 <- process_municipality(pop14, c("024044", "024103"), "024128", "Sovizzo", 2014)
pop15 <- process_municipality(pop15, c("024044", "024103"), "024128", "Sovizzo", 2015)
pop16 <- process_municipality(pop16, c("024044", "024103"), "024128", "Sovizzo", 2016)
pop17 <- process_municipality(pop17, c("024044", "024103"), "024128", "Sovizzo", 2017)
pop18 <- process_municipality(pop18, c("024044", "024103"), "024128", "Sovizzo", 2018)
pop19 <- process_municipality(pop19, c("024044", "024103"), "024128", "Sovizzo", 2019)
pop20 <- process_municipality(pop20, c("024044", "024103"), "024128", "Sovizzo", 2020)

# Campospinoso Albaredo (018002 + 018026)
pop11 <- process_municipality(pop11, c("018002", "018026"), "018026", "Campospinoso Albaredo", 2011)
pop12 <- process_municipality(pop12, c("018002", "018026"), "018026", "Campospinoso Albaredo", 2012)
pop13 <- process_municipality(pop13, c("018002", "018026"), "018026", "Campospinoso Albaredo", 2013)
pop14 <- process_municipality(pop14, c("018002", "018026"), "018026", "Campospinoso Albaredo", 2014)
pop15 <- process_municipality(pop15, c("018002", "018026"), "018026", "Campospinoso Albaredo", 2015)
pop16 <- process_municipality(pop16, c("018002", "018026"), "018026", "Campospinoso Albaredo", 2016)
pop17 <- process_municipality(pop17, c("018002", "018026"), "018026", "Campospinoso Albaredo", 2017)
pop18 <- process_municipality(pop18, c("018002", "018026"), "018026", "Campospinoso Albaredo", 2018)
pop19 <- process_municipality(pop19, c("018002", "018026"), "018026", "Campospinoso Albaredo", 2019)
pop20 <- process_municipality(pop20, c("018002", "018026"), "018026", "Campospinoso Albaredo", 2020)


##################################################
##  	081021 (Trapani) + 081025 (Misiliscemi) = 081021 (Trapani) from pop22 to pop23

pop22 <- process_municipality(pop22, c("081021", "081025"), "081021", "Trapani", 2022)
pop23 <- process_municipality(pop23, c("081021", "081025"), "081021", "Trapani", 2023)

#########end

#Store the separate datasets from pop11 to pop23
saveRDS(pop11, file ="~/pop11.rds")
saveRDS(pop12, file ="~/pop12.rds")
saveRDS(pop13, file ="~/pop13.rds")
saveRDS(pop14, file ="~/pop14.rds")
saveRDS(pop15, file ="~/pop15.rds")
saveRDS(pop16, file ="~/pop16.rds")
saveRDS(pop17, file ="~/pop17.rds")
saveRDS(pop18, file ="~/pop18.rds")
saveRDS(pop19, file ="~/pop19.rds")
saveRDS(pop20, file ="~/pop20.rds")
saveRDS(pop21, file ="~/pop21.rds")
saveRDS(pop22, file ="~/pop22.rds")
saveRDS(pop23, file ="~/pop23.rds")
