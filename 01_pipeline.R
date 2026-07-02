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

timing.format <- 
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

timing.reload <- 
  function(load.expression,
           label = NULL) {
    
    # 0. function arguments ----
    
    # load.expression
    # label = NULL
    
    # 1. set up ----
    
    if (!is.null(label)) message(sprintf('%s', label))
    
    # 2. load ----
    
    t0 <- Sys.time()
    obj <- eval(load.expression)
    t1 <- Sys.time()
    
    time.load <- as.numeric(t1 - t0, units = 'mins')
    
    message(sprintf(' load time: %s.',
                    timing.format(time.end = t1, time.start = t0)))
    
    # 3. return ----
    
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
    source(file.path(dir.code, '01a_dataWrangling.R'))
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
    source(file.path(dir.code, '01b_defineCrossbasis.R'))
  })
  
}

# 3. model fit ----

dir.modelfit <- file.path(dir.res, '01c_modelFit')

local({
  source(file.path(dir.code, '01c_modelFit.R'))
})




# 99. OLD ----
  
# 
# # 2. model fit ----
# 
# ## 2.1. prior specification ----
# 
# hyper.pc <- list(prec = list(prior = 'pc.prec', param = c(1, 0.01)))
# hyper.pc.space <- list(prec = list(prior = 'pc.prec', param = c(1, 0.01)),
#                        phi = list(prior = 'pc', param = c(0.5, 0.5)))
# 
# ## 2.2. values ----
# 
# date.min <- data.final$date %>% min()
# date.max <- data.final$date %>% max()
# year.min <- date.min %>% lubridate::year()
# year.max <- date.max %>% lubridate::year()
# 
# n.dow <- 7
# n.doy <- 366
# n.year <- length(year.min:year.max)
# n.munc <- nrow(poly.munc)
# 
# n.munc.doy <- n.munc*n.doy
# n.munc.year <- n.munc*n.year
# 
# 
# ## 2.3. inla arguments ----
# 
# control.family <- inla.set.control.family.default() # for changing the prior distribution of the likelihood hyperparameters
# control.compute <- list(config = TRUE)  # for computing measures of fit
# control.inla <- list(strategy = 'simplified.laplace', int.strategy = 'eb')
# control.predictor <- list(link = 1)
# control.mode <- list(restart = TRUE)
# 
# ## 1.3. formula ----
# 
# f1 <- 
#   deaths ~ 
#   1 +
#   # offset
#   offset(log(pop)) +
#   # fixed effects
#   ## temporal confounders
#   factor(dow) +
#   factor(holiday) +  
#   ## relative humidity
#   lag_rh_mean +
#   ## deprivation 
#   factor(IFC_2018) +
#   ## temperature confounders
#   Xpred1 + Xpred2 + Xpred3 + Xpred4 +
#   # random effects
#   ## day of year - seasonality
#   f(id_doy, model = 'rw2', scale.model = TRUE, constr = TRUE, hyper = hyper.pc.rw2) +
#   ## year - long term trend
#   f(id_year, model = 'iid', constr = TRUE, hyper = hyper.pc.iid) +
#   ## spatial
#   f(id_region, model = 'bym2', graph = mat.munc, hyper = hyper.pc.space, scale.model = TRUE, constr = TRUE) +
#   ## spatially varying coefficients 
#   f(id_region1, Xpred1, model = 'bym2', graph = mat.munc, hyper = hyper.pc.space, scale.model = TRUE, constr = TRUE) +
#   f(id_region2, Xpred2, model = 'bym2', graph = mat.munc, hyper = hyper.pc.space, scale.model = TRUE, constr = TRUE) +
#   f(id_region3, Xpred3, model = 'bym2', graph = mat.munc, hyper = hyper.pc.space, scale.model = TRUE, constr = TRUE) +
#   f(id_region4, Xpred4, model = 'bym2', graph = mat.munc, hyper = hyper.pc.space, scale.model = TRUE, constr = TRUE)
# 
# f2 <- 
#   deaths ~ 
#   1 +
#   # offset
#   offset(log(pop)) +
#   # fixed effects
#   ## temporal confounders
#   factor(dow) +
#   factor(holiday) +  
#   ## relative humidity
#   lag_rh_mean +
#   ## deprivation 
#   factor(IFC_2018) +
#   ## other confounders
#   ndvi + Obesity + Smoking + Mean_Alt +
#   ## temperature confounders
#   Xpred1 + Xpred2 + Xpred3 + Xpred4 +
#   # random effects
#   ## day of year - seasonality
#   f(id_doy, model = 'rw2', scale.model = TRUE, constr = TRUE, hyper = hyper.pc.rw2) +
#   ## year - long term trend
#   f(id_year, model = 'iid', constr = TRUE, hyper = hyper.pc.iid) +
#   ## spatial
#   f(id_region, model = 'bym2', graph = mat.munc, hyper = hyper.pc.space, scale.model = TRUE, constr = TRUE) +
#   ## spatially varying coefficients 
#   f(id_region1, Xpred1, model = 'bym2', graph = mat.munc, hyper = hyper.pc.space, scale.model = TRUE, constr = TRUE) +
#   f(id_region2, Xpred2, model = 'bym2', graph = mat.munc, hyper = hyper.pc.space, scale.model = TRUE, constr = TRUE) +
#   f(id_region3, Xpred3, model = 'bym2', graph = mat.munc, hyper = hyper.pc.space, scale.model = TRUE, constr = TRUE) +
#   f(id_region4, Xpred4, model = 'bym2', graph = mat.munc, hyper = hyper.pc.space, scale.model = TRUE, constr = TRUE) 
# 
# ## 1.3. inla arguments ----
# 
# control.family <- inla.set.control.family.default() # for changing the prior distribution of the likelihood hyperparameters
# control.compute <- list(config = TRUE)  # for computing measures of fit
# control.inla <- list(strategy = 'simplified.laplace', int.strategy = 'eb')
# control.predictor <- list(link = 1)
# control.mode <- list(restart = TRUE)
# 
# ## 1.4. fit ----
# 
# cat('\nModel Fit (Started) - Deprivation only:\n')
# time.start <- Sys.time()
# m1 <-  inla(formula = f1,
#             family = 'Poisson',
#             data = data.final,
#             control.family = control.family, # for changing the prior distribution of the likelihood hyperparameters
#             control.compute = control.compute,
#             control.inla = control.inla,
#             control.mode = control.mode,
#             control.predictor = control.predictor,
#             num.threads = n.cores,
#             verbose = FALSE)
# time.end <- Sys.time()
# cat('\nModel Fit (Finished) - Deprivation only:\n')
# time.end - time.start
# 
# cat('\nModel Fit (Started) - All confounders:\n')
# time.start <- Sys.time()
# m2 <-  inla(formula = f2,
#             family = 'Poisson',
#             data = data.final,
#             control.family = control.family, # for changing the prior distribution of the likelihood hyperparameters
#             control.compute = control.compute,
#             control.inla = control.inla,
#             control.mode = control.mode,
#             control.predictor = control.predictor,
#             num.threads = n.cores,
#             verbose = FALSE)
# time.end <- Sys.time()
# cat('\nModel Fit (Finished) - All confounders:\n')
# time.end - time.start
# 
# ## 1.5. save ----
# 
# setwd(dir.res)
# cat('\nSave Time (Started) - Deprivation only:\n')
# time.start <- Sys.time()
# saveRDS(m1, 'mod_confounder_deprivationOnly.rds')
# time.end <- Sys.time()
# cat('\nSave Time (Finished) - Deprivation only:\n')
# time.end - time.start
# 
# setwd(dir.res)
# cat('\nSave Time (Started) - All confounders:\n')
# time.start <- Sys.time()
# saveRDS(m2, 'mod_confounder_all.rds')
# time.end <- Sys.time()
# cat('\nSave Time (Finished) - All confounders:\n')
# time.end - time.start
# 
# # 2. posterior sampling ----
# 
# ## 2.1. remove predictors ----
# 
# cs1 <- m1$misc$configs$contents$tag
# cs1 <- cs1[cs1 != 'Predictor']
# select1 <- stats::setNames(as.list(rep(0, length(cs1))), cs1)
# 
# cs2 <- m2$misc$configs$contents$tag
# cs2 <- cs2[cs2 != 'Predictor']
# select2 <- stats::setNames(as.list(rep(0, length(cs2))), cs2)
# 
# ## 2.2. samples ----
# 
# cat('Posterior samples (Started) - Deprivation only:\n')
# time.start <- Sys.time()
# s1 <-
#   INLA::inla.posterior.sample(n = 1000,
#                               result = m1,
#                               selection = select1,
#                               num.threads = n.cores)
# time.end <- Sys.time()
# cat('Posterior samples (Finished) - Deprivation only:\n')
# time.end - time.start
# 
# cat('Posterior samples (Started) - All confounders:\n')
# time.start <- Sys.time()
# s2 <-
#   INLA::inla.posterior.sample(n = 1000,
#                               result = m2,
#                               selection = select2,
#                               num.threads = n.cores)
# time.end <- Sys.time()
# cat('Posterior samples (Finished) - All confounders:\n')
# time.end - time.start
# 
# ## 2.3. save ----
# 
# setwd(dir.res)
# cat('\nPosterior samples save (Started) - Deprivation only\n')
# time.start <- Sys.time()
# saveRDS(s1, 'samples_confounder_deprivationOnly.rds')
# time.end <- Sys.time()
# cat('\nPosterior samples save (Finished) - Deprivation only\n')
# time.end - time.start
# 
# setwd(dir.res)
# cat('\nPosterior samples save (Started) - All confounders\n')
# time.start <- Sys.time()
# saveRDS(s2, 'samples_confounder_all.rds')
# time.end <- Sys.time()
# cat('\nPosterior samples save (Finished) - All confounders\n')
# time.end - time.start
# 
