# 0. set up ----

## 0.1. librarys ----

# data wrangling
library(tidyverse)
library(data.table)
library(fs)

# modelling
library(dlnm)
library(splines)
library(INLA)

# spatial data
library(sf)

# parallel
library(future)
library(future.apply)

## 0.2. directories ---- 

# retrieve directories
if (Sys.info()['sysname'] == 'Windows') {
  
  print('Running on Windows')
  dir.path <- rstudioapi::getActiveDocumentContext()$path
  dir.home <- sub('(italyExcessTemperature).*', '\\1', dir.path)
  n.cores <- round(future::availableCores()*0.5)
  plan(multisession, workers = n.cores)
  
} else if (Sys.info()['sysname'] == 'Linux') {
  
  print('Running on Linux')
  # retrieve directories
  dir.home <- '/rds/general/user/cgascoig/home/italyExcessTemperature' # TERMINAL ON HPC
  n.cores <- future::availableCores()
  plan(multisession, workers = n.cores)
  data.table::setDTthreads(threads = n.cores)
  
} else {
  
  print('Unknown OS')
  
}
print(sprintf('number of cores being called: %s', n.cores))

dir.code <- file.path(dir.home, 'code')
dir.data <- file.path(dir.home, 'data')
dir.res <- file.path(dir.home, 'results')

## 0.2.2. create ----

dir.res <- file.path(dir.res, '01_pipeline')
fs::dir_create(dir.res)

## 0.3 import ----

# spatial polygon
poly.munc <- 
  file.path(dir.data, 'shapefile') %>% 
  sf::st_read(dsn = ., layer = 'shp')

# spatial amat
if( file.path(dir.data, 'italy_amat.rds') %>% fs::file_exists() ){
  
  load(file.path(dir.data, 'italy_amat.rds'))
  
} else {
  
  # 1. adjacent matrices ----
  
  mat.munc.temp <- spdep::poly2nb(as(poly.munc, 'Spatial'))
  mat.munc <- spdep::nb2mat(mat.munc.temp, zero.policy = TRUE)
  colnames(mat.munc) <- rownames(mat.munc) <- paste0('munc_', 1:dim(mat.munc)[1])
  
  # 2. save amat ----
  
  save(mat.munc, file = file.path(dir.data, 'italy_amat.rds'))
  
  # 3. remove unwanted ----
  
  rm(mat.munc.temp); gc()
  
}

# 0.4 functions ----

helper.formatTime <- 
  function(time.end, 
           time.start) {
    
    # 1. time difference ----
    
    time.diff <- 
      difftime(time1 = time.end, time2 = time.start, units = 'secs') %>% 
      as.numeric()
    
    # 2. time lengths -----
    
    hours   <- floor(time.diff / 3600)
    minutes <- floor((time.diff %% 3600) / 60)
    seconds <- round(time.diff %% 60)
    
    # 3. return (message) ----
    
    sprintf('%02d hours %02d minutes %02d seconds', 
            hours, minutes, seconds)
    
  }

helper.objectLoadRun <-
  function(filepath, 
           FUN, 
           message.run, 
           message.load) {
    
    # 0. function arguments ----
    
    # filepath 
    # FUN
    # message.run 
    # message.load
    
    # 1. load or run ----
    
    if (!file.exists(filepath)) {
      message(message.run)
      t0 <- Sys.time()
      obj <- FUN()
      t1 <- Sys.time()
      message(sprintf(' time: %s.', helper.formatTime(t1, t0)))
      saveRDS(obj, filepath) # saved returned oject
    } else {
      message(message.load)
      t0 <- Sys.time()
      obj <- readRDS(filepath)
      t1 <- Sys.time()
      message(sprintf(' time: %s.', helper.formatTime(t1, t0)))
    }
    
    # 2. return ----
    
    return(obj)
    
  }

my.map.theme <-
  function(...){
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   legend.text = ggplot2::element_text(hjust = 0),
                   legend.key = ggplot2::element_rect(fill = NA, colour = NA),
                   panel.background = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   ...)
  }

my.theme <- 
  function(...){
    ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(colour = 'black'),
                   legend.text = ggplot2::element_text(hjust = 0),
                   legend.key = ggplot2::element_rect(fill = NA, colour = NA),
                   ...)
  }

# 1. data wrangling ----

dir.datawrangling <- file.path(dir.res, '01a_dataWrangling')
fp.datawrangling <- file.path(dir.datawrangling, 'data_dataWrangling.csv')

if(!fs::file_exists(fp.datawrangling)) {
  
  message(sprintf('Sorting data...'))
  
  local({
    source(file.path(dir.code, '01a_mainanalysis_dataWrangling.R'))
  })
  
}

# 2. define crossbasis ----

## 2.1. municipality details ----

municipality.details <- 
  poly.munc %>% 
  sf::st_drop_geometry() %>% 
  data.table::as.data.table() %>% 
  .[, `:=` (
    # REMEMBER PRO_COM_T == Code
    Code = PRO_COM_T %>% as.numeric(),
    # add a correct region_id
    municipality_id = .I
  )] %>% 
  .[, .(Code, COMUNE, municipality_id)]

max.characters <- municipality.details$Code %>% max() %>% nchar()

## 2.2. define all files ----

dir.definecrossbasis <- file.path(dir.res, '01b_defineCrossbasis')
fp.definecrossbasis.expected <- 
  file.path(dir.definecrossbasis,
            sprintf('crossbasis_municipality_%s.csv', 
                    sprintf('%0*d', max.characters, municipality.details$Code))) %>% 
  sort()
fp.definecrossbasis.existing <-
  dir.definecrossbasis %>% 
  fs::dir_ls(path = .,
             recurse = TRUE,
             type = 'file') %>% 
  as.character() %>% 
  sort()

if(!all.equal(fp.definecrossbasis.existing,
             fp.definecrossbasis.expected)) {
  
  message(sprintf('Defining Crossbasis...'))
  
  local({
    source(file.path(dir.code, '01b_mainanalysis_defineCrossbasis.R'))
  })
  
}

rm(fp.definecrossbasis.expected)
gc()

# 3. model fit ----

dir.modelfit <- file.path(dir.res, '01c_modelFit')

local({
  source(file.path(dir.code, '01c_mainanalysis_modelFit.R'))
})

# 4. sample crossbasis ----

dir.samplecb <- file.path(dir.res, '01d_sampleCrossbasisPosterior')

local({
  source(file.path(dir.code, '01d_mainanalysis_sampleCrossbasisPosterior.R'))
})

# 5. define results ----

dir.finalres <- file.path(dir.res, '01e_resultsGenerating')

local({
  source(file.path(dir.code, '01e_mainanalysis_resultsGenerating.R'))
})

