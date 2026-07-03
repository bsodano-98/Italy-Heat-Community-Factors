# 0. set up ----

## 0.1. directory ----

fs::dir_create(dir.samplecb) # defined in pipeline

## 0.2. function ----

posterior.sortCBcoefficients <-
  function(i, 
           samples, 
           n.munc = 7895,
           names.fe = sprintf('cb%s', 1:4), 
           names.re = sprintf('municipality_cb%s_id', 1:4)){
    
    # 0. function arguments ----
    
    # i <- 1
    # samples <- model.samples
    # n.munc <- 7895
    # names.fe = sprintf('cb%s', 1:4)
    # names.re = sprintf('municipality_cb%s_id', 1:4)
    
    # 1. parameters -----
    
    name.theta <- sprintf('theta:%s', i)
    sample.latent <- samples[[i]]$latent
    
    # 2. format ----
    
    sample.latent.format <-
      sample.latent %>%
      data.table::as.data.table(x = .,
                                keep.rownames = 'parameter')
    
    # 3. linear predictor ----
    
    lp.fe <-
      data.table::copy(sample.latent.format) %>%
      .[
        paste(names.fe, collapse = '|') %>%
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
        paste(names.re, collapse = '|') %>%
          sprintf('^(%s)', .) %>%
          grepl(pattern = ., x = parameter)
      ] %>%
      .[, `:=` (
        parameter.re = parameter,
        sample.re = V1,
        cb = parameter %>% sub(pattern = '.*(cb[0-9]+).*', replacement = '\\1', x = .),
        municipality_id = parameter %>% sub(pattern = '.*:', replacement = '', x = .) %>% as.integer()
      )] %>%
      .[municipality_id <= n.munc] %>% 
      .[, .(parameter.re, cb, sample.re)]
    
    # 4. add specific fixed effect to random effects ----
    
    res <-
      data.table::copy(lp.re) %>%
      .[data.table::copy(lp.fe), on = 'cb', nomatch = 0] %>%
      .[, (name.theta) := sample.re + sample.fe] %>% 
      .[, parameter := parameter.re] %>% 
      .[, c('parameter', 'cb', name.theta), with = FALSE]
      
    
    # 5. clear ----
    
    rm(sample.latent,
       sample.latent.format,
       lp.re,
       lp.fe)
    gc()
    
    # 6. return ----
    
    return(res)
    
  }

# 1. base model ----

message('Base Model:')

cb.coef <-
  helper.objectLoadRun(
    filepath = file.path(dir.samplecb, 'model_crossbasis_posterior.rds'),
    FUN = function() {
      
      # 1. load samples
      model.samples <- 
        file.path(dir.modelfit, 'model_samples.rds') %>% 
        readRDS(file = .)
      
      # 2. define cb posterior
      res <-
        lapply(X = seq_len(1000),
               FUN = function(i) {
                 posterior.sortCBcoefficients(i = i, samples = model.samples)
               }
               ) %>% 
        # join across
        Reduce(f = function(x, y) merge(x, y, by = c('parameter', 'cb'), all = TRUE),
               x = .)
      
      # 3. clear
      rm(model.samples)
      gc()
      
      # 4. return 
      res
    },
    message.run  = 'Running base model posterior extraction...',
    message.load = 'Loading base model posterior...'
  )

# # 2. sensitivity model ----
# 
# message('Sensitivity Model:')
# 
# cb.coef.sens <-
#   helper.objectLoadRun(
#     filepath = file.path(dir.samplecb, 'model_crossbasis_posterior_sensitivity.rds'),
#     FUN = function() {
# 
#       # 1. load samples
#       model.samples.sens <-
#         file.path(dir.modelfit, 'model_samples_sensitivity.rds') %>%
#         readRDS()
# 
#       # 2. define cb posterior
#       res <-
#         lapply(X = seq_len(1000),
#                FUN = function(i) {
#                  posterior.sortCBcoefficients(i = i, samples = model.samples.sens)
#                }
#         ) %>% 
#         Reduce(f = function(x, y) merge(x, y, by = c('parameter', 'cb'), all = TRUE),
#                x = .)
# 
#       # 3. clear
#       rm(model.samples.sens)
#       gc()
# 
#       # 4. return
#       res
#     },
#     message.run  = 'Running sensitivity model posterior extraction...',
#     message.load = 'Loading sensitivity model posterior from disk...'
#   )

