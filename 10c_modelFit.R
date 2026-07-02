# 0. set up ----

## 0.1. directory ----

fs::dir_create(dir.modelfit) # defined in pipeline

## 0.2. functions ----

object.loadOrRun <-
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
      message(sprintf(' time: %s.', timing.format(t1, t0)))
      saveRDS(obj, filepath) # saved returned oject
    } else {
      message(message.load)
      t0 <- Sys.time()
      obj <- readRDS(filepath)
      t1 <- Sys.time()
      message(sprintf(' time: %s.', timing.format(t1, t0)))
    }
    
    # 2. return ----
    
    return(obj)
    
  }


model.fitAndSample <- function(
    formula,
    data,
    filepath.fit,
    filepath.samples,
    n.samples = 1000,
    n.sample.subset = 100,
    control.family = inla.set.control.family.default(),
    control.compute = list(config = TRUE),
    control.inla = list(strategy = 'simplified.laplace', int.strategy = 'eb'),
    control.predictor = list(link = 1),
    control.mode = list(restart = TRUE)
) {
  
  # 0. function parameters ----
  
  # formula <- formula
  # data <- model.data
  # filepath.fit <- fp.modelfit
  # filepath.samples <- fp.modelsample
  # n.samples <- 1000
  # n.sample.subset <- 100
  # control.family <- inla.set.control.family.default() # for changing the prior distribution of the likelihood hyperparameters
  # control.compute <- list(config = TRUE)  # for computing measures of fit
  # control.inla <- list(strategy = 'simplified.laplace', int.strategy = 'eb')
  # control.predictor <- list(link = 1)
  # control.mode <- list(restart = TRUE)
  
  # 1. model fit ----
  
  model.fit <- 
    object.loadOrRun(
      filepath = filepath.fit,
      message.run  = 'Model fitting...',
      message.load = 'Model loading...',
      FUN = function() {
        INLA::inla(
          formula = formula,
          family  = 'poisson',
          data    = data,
          control.family    = control.family,
          control.compute   = control.compute,
          control.inla      = control.inla,
          control.predictor = control.predictor,
          control.mode      = control.mode
        ) # saved in object.loadOrRun
      }
    )
  
  # 2. poster sample ----
  
  model.samples <- 
    object.loadOrRun(
      filepath = filepath.samples,
      message.run  = 'Model sampling...',
      message.load = 'Model samples loading...',
      FUN = function() {
        
        cs <- model.fit$misc$configs$contents$tag
        cs <- cs[cs != 'Predictor']
        select <- stats::setNames(as.list(rep(0, length(cs))), cs)
        
        samples <- 
          INLA::inla.posterior.sample(
            n = n.samples,
            result = model.fit,
            selection = select
          )
        
        if (!is.null(n.sample.subset)) {
          saveRDS(
            object = samples[1:n.sample.subset],
            file = filepath.samples %>% gsub(pattern = '\\.rds$', replacement = sprintf('_%s.rds', n.sample.subset), x = .)
          )
        }
        
        samples # saved in object.loadOrRun
      }
    )
  
  # 3. clear ----
  
  rm(
    model.fit,
    model.samples
  )
  gc()
  
  # 4. return ----
  
  return(invisible(NULL))
  
}

# 1. final data ----

fp.modeldata <- file.path(dir.modelfit, 'model_data.csv')

if(!fs::file_exists(fp.modeldata)){
  
  model.data <- 
    future.apply::future_lapply(
      X = 
        fp.definecrossbasis.existing %>% # defined in pipeline
        seq_along(),
      FUN = function(i) {
        
        # 0. function arguments ----
        
        # i <- 1
        
        # 1. data ----
        
        data.table::fread(file = fp.crossbasis[i])
        
      },
      future.packages = c('tidyverse', 'data.table'),
      future.globals = c('fp.definecrossbasis.existing')) %>% 
    data.table::rbindlist()
  
  data.table::fwrite(x = model.data,
                     file = fp.modeldata)
  
} else {
  
  model.data <-
    data.table::fread(file = fp.modeldata)
  
}

# 2. prior specification ----

hyper.pc <- list(prec = list(prior = 'pc.prec', param = c(1, 0.01)))
hyper.pc.space <- list(prec = list(prior = 'pc.prec', param = c(1, 0.01)),
                       phi = list(prior = 'pc', param = c(0.5, 0.5)))

# 3. values ----

date.min <- model.data$date %>% min()
date.max <- model.data$date %>% max()
year.min <- date.min %>% lubridate::year()
year.max <- date.max %>% lubridate::year()

n.dow <- 7
n.doy <- 366
n.year <- length(year.min:year.max)
n.munc <- nrow(poly.munc)

n.munc.doy <- n.munc*n.doy
n.munc.year <- n.munc*n.year

# 4. formula ----

## 4.1. shared terms ----

terms.f <-
  deaths ~
  # intercept
  1 +
  # offset
  offset(log(population)) +
  # fixed effects
  ## temporal confounders
  dow_id +
  holiday_id + 
  # random effects
  ## day of year - seasonality
  f(doy_id, model = 'rw2', scale.model = TRUE, constr = TRUE, hyper = hyper.pc, values = 1:n.doy, cyclic = TRUE) +
  ## year - long term trend
  f(year_id, model = 'iid', constr = TRUE, hyper = hyper.pc, values = 1:n.year) +
  ## spatial
  f(municipality_id, model = 'bym2', scale.model = TRUE, constr = TRUE, hyper = hyper.pc.space, graph = mat.munc)

terms.cb <-
  sprintf('cb%s', 1:4) %>% 
  paste(., collapse = ' + ')

terms.svc <-
  sprintf(
    "f(municipality_cb%s_id, cb%s, model = 'bym2', 
    scale.model = TRUE, constr = TRUE, 
    hyper = hyper.pc.space, graph = mat.munc)",
    1:4, 1:4
  ) %>% 
  paste(. , collapse = ' + ')

## 4.2. base ----

formula.base <-
  terms.f %>% 
  # update for cb terms
  update(.,
         paste('. ~ . +', terms.cb, '+', terms.svc)
  )

## 5.3. sensitivity ---

formula.sens <- 
  terms.f %>% 
  # update for cb terms
  update(.,
         paste(
           '. ~ . +',
           "f(municipality_doy_id, model = 'iid', constr = TRUE,
         hyper = hyper.pc, values = 1:n.munc.doy)",
           '+',
           "f(municipality_year_id, model = 'iid', constr = TRUE,
         hyper = hyper.pc, values = 1:n.munc.year)",
           '+',
           terms.cb,
           '+',
           terms.svc
         )
  )

# 5. fit and sample ----

## 5.1. base ----

message('Base model:')

run.base <-
  model.fitAndSample(
    formula = formula.base,
    data = model.data,
    filepath.fit = file.path(dir.modelfit, 'model_fit.rds'),
    filepath.samples = file.path(dir.modelfit, 'model_samples.rds'))


## 5.2. sensitivity ----

message('Sensitivity model:')

run.sens <-
  model.fitAndSample(
    formula = formula.sens,
    data = model.data,
    filepath.fit = file.path(dir.modelfit, 'model_fit_sensitivity.rds'),
    filepath.samples = file.path(dir.modelfit, 'model_samples_sensitivity.rds'))

