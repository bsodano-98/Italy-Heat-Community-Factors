# 1. base model ----

fp.cbcoeffcients <- file.path(dir.modelfit, 'model_crossbasis_posterior.rds')

t0 <- Sys.time()
name.cb.fe <- sprintf('cb%s', 1:4)
name.cb.re <- sprintf('municipality_cb%s_id', 1:4)

cb.coef <-
  future.apply::future_lapply(
    X = 1:1000,
    FUN =  function(i){
      
      # 0. function arguments ----
      
      # i <- 1
      
      # 1. parameters -----
      
      sample.import <- readRDS(file = fp.modelsample)
      sample.latent <- sample.import[[i]]$latent
      
      rm(sample.import)
      gc()
      
      # 2. format ----
      
      sample.latent.format <-
        sample.latent %>%
        data.table::as.data.table(x = .,
                                  keep.rownames = 'parameter')
      
      # 3. linear predictor ----
      
      lp.fe <-
        data.table::copy(sample.latent.format) %>%
        .[
          paste(name.cb.fe, collapse = '|') %>%
            sprintf('^(%s)', .) %>%
            grepl(pattern = ., x = parameter)
        ] %>%
        .[, `:=` (
          parameter.fe = parameter,
          sample.fe = V1,
          cb = parameter %>% sub(pattern = ':.*', replacement = '', x = .)
        )] %>%
        .[, .(parameter.fe, cb, sample.fe)]
      
      lp.re <-
        data.table::copy(sample.latent.format) %>%
        .[
          paste(name.cb.re, collapse = '|') %>%
            sprintf('^(%s)', .) %>%
            grepl(pattern = ., x = parameter)
        ] %>%
        .[, `:=` (
          parameter.re = parameter,
          sample.re = V1,
          cb = parameter %>% sub(pattern = '.*(cb[0-9]+).*', replacement = '\\1', x = .)
        )] %>%
        .[, .(parameter.re, cb, sample.re)]
      
      # 4. add specific fixed effect to random effects ----
      
      res <-
        data.table::copy(lp.re) %>%
        .[data.table::copy(lp.fe), on = 'cb', nomatch = 0] %>%
        .[, sample.full := sample.re + sample.fe]
      
      rm(sample.latent,
         sample.latent.format,
         lp.re,
         lp.fe)
      gc()
      
      # 5. return ----
      
      return(res)
      
    },
    future.packages = c('tidyverse', 'data.table'),
    future.globals = c('fp.modelsample')
  )

t1 <- Sys.time()

message(
  sprintf(' time to extract: %s.',
          timing.format(time.end = t1, time.start = t0))
)

t0 <- Sys.time()
saveRDS(object = cb.coef,
        file = fp.cbcoeffcients)
t1 <- Sys.time()

message(
  sprintf(' time to save: %s.',
          timing.format(time.end = t1, time.start = t0))
)


# 2. sensitivity model ----

fp.cbcoeffcients <- file.path(dir.modelfit, 'model_crossbasis_posterior_sensitivity.rds')

t0 <- Sys.time()
name.cb.fe <- sprintf('cb%s', 1:4)
name.cb.re <- sprintf('municipality_cb%s_id', 1:4)

cb.coef <-
  future.apply::future_lapply(
    X = 1:1000,
    FUN =  function(i){
      
      # 0. function arguments ----
      
      # i <- 1
      
      # 1. parameters -----
      
      sample.import <- readRDS(file = fp.modelsample)
      sample.latent <- sample.import[[i]]$latent
      
      rm(sample.import)
      gc()
      
      # 2. format ----
      
      sample.latent.format <-
        sample.latent %>%
        data.table::as.data.table(x = .,
                                  keep.rownames = 'parameter')
      
      # 3. linear predictor ----
      
      lp.fe <-
        data.table::copy(sample.latent.format) %>%
        .[
          paste(name.cb.fe, collapse = '|') %>%
            sprintf('^(%s)', .) %>%
            grepl(pattern = ., x = parameter)
        ] %>%
        .[, `:=` (
          parameter.fe = parameter,
          sample.fe = V1,
          cb = parameter %>% sub(pattern = ':.*', replacement = '', x = .)
        )] %>%
        .[, .(parameter.fe, cb, sample.fe)]
      
      lp.re <-
        data.table::copy(sample.latent.format) %>%
        .[
          paste(name.cb.re, collapse = '|') %>%
            sprintf('^(%s)', .) %>%
            grepl(pattern = ., x = parameter)
        ] %>%
        .[, `:=` (
          parameter.re = parameter,
          sample.re = V1,
          cb = parameter %>% sub(pattern = '.*(cb[0-9]+).*', replacement = '\\1', x = .)
        )] %>%
        .[, .(parameter.re, cb, sample.re)]
      
      # 4. add specific fixed effect to random effects ----
      
      res <-
        data.table::copy(lp.re) %>%
        .[data.table::copy(lp.fe), on = 'cb', nomatch = 0] %>%
        .[, sample.full := sample.re + sample.fe]
      
      rm(sample.latent,
         sample.latent.format,
         lp.re,
         lp.fe)
      gc()
      
      # 5. return ----
      
      return(res)
      
    },
    future.packages = c('tidyverse', 'data.table'),
    future.globals = c('fp.modelsample')
  )

t1 <- Sys.time()

message(
  sprintf(' time to extract: %s.',
          timing.format(time.end = t1, time.start = t0))
)

t0 <- Sys.time()
saveRDS(object = cb.coef,
        file = fp.cbcoeffcients)
t1 <- Sys.time()

message(
  sprintf(' time to save: %s.',
          timing.format(time.end = t1, time.start = t0))
)
