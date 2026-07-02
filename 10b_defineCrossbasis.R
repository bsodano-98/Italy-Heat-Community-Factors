# 1. define cb per municipality ----

future.apply::future_lapply(
  X = 1:nrow(municipality.details),
  FUN = function(i,
                 dir.target = getwd()){
    
    # 0. function arguments ----
    
    # i <- 1
    # dir.target <- file.path(dir.data, 'organised', '01_modelFit', '01b_defineCrossbasis')
    
    # 1. parameters ----
    
    name.munc.idx <- municipality.details[i, municipality_id]
    name.munc.real <- municipality.details[i, COMUNE]
    name.munc.code <- municipality.details[i, Code]
    name.munc.code.full <- sprintf('%0*d', max.characters, name.munc.code)
    
    name.file <- sprintf('crossbasis_municipality_%s.csv', name.munc.code.full)
    name.filepath <- file.path(dir.target, name.file)
    
    if(file.exists(name.filepath)){
      
      message(sprintf(' file for municipality %s (%s - %s) exists already as %s. Skipping.',
                      name.munc.idx, name.munc.code.full, name.munc.real, name.file))
      return(invisible(NULL))
      
    }
    
    # 2. data import ----
    
    data.import <- 
      data.table::fread(file = fp.datawrangling)
    
    # 3. data subset ----
    
    data.subset <- 
      data.import %>% 
      .[Code == name.munc.code]
    
    # 4. define cross basis ----
    
    ## 4.1. exposure info ----
    
    expInfo.x <- data.subset$tmp_lag_03
    expInfo.fun <- 'ns'
    expInfo.boundary <- 
      expInfo.x %>% 
      range()
    expInfo.knots <- 
      expInfo.x %>% 
      stats::quantile(x = ., probs = c(10, 75, 90)/100, na.rm = TRUE)
    expInfo.cen <- 12
    
    ## 4.2. cross basis ----
    
    cb.exp <- 
      dlnm::onebasis(x = expInfo.x, 
                     fun = expInfo.fun,
                     knots = expInfo.knots,
                     Boundary.knots = expInfo.boundary,
                     intercept = FALSE)
    
    ## 4.3. re-scale ----
    
    cb.cen <- 
      dlnm::onebasis(x = expInfo.cen, 
                     fun = expInfo.fun,
                     knots = expInfo.knots,
                     Boundary.knots = expInfo.boundary,
                     intercept = FALSE)
    
    cb.exp.scaled <- 
      scale(x = cb.exp, 
            center = cb.cen,
            scale = FALSE)
    
    ## 4.4. format ----
    
    crossbasis <-
      cb.exp.scaled %>% 
      data.table::as.data.table() %>% 
      data.table::setnames(x = .,
                           old = names(.),
                           new = sprintf('cb%s', 1:(ncol(.)))) %>% 
      .[, date := data.subset$date]
    
    # 5. data.finalise ----
    
    data.final <-
      data.table::copy(data.subset) %>% 
      merge(x = .,
            y = crossbasis,
            by = 'date',
            all.x = TRUE) %>% 
      # add id columns for the municipality cross basis
      .[, sprintf('municipality_cb%s_id', 1:ncol(cb.exp.scaled)) := municipality_id]
    
    # 4. save ----
    
    fs::dir_create(dir.target)
    data.table::fwrite(x = data.final,
                       file = name.filepath)
    
    message(sprintf(' file for municipality %s (%s- %s) saved as %s.', 
                    name.munc.idx, name.munc.code.full, name.munc.real, name.file))
    
    # 5. clear ----
    
    rm(
      name.munc.idx,
      name.munc.real,
      name.munc.code,
      name.munc.code.full,
      name.file,
      name.filepath,
      data.import,
      data.subset,
      expInfo.x,
      expInfo.fun,
      expInfo.boundary,
      expInfo.knots,
      expInfo.cen,
      cb.exp,
      cb.cen,
      cb.exp.scaled,
      crossbasis,
      data.final
    )
    gc()
    
    # 6. return ----
    
    return(invisible(NULL))
    
    
  },
  dir.target = dir.definecrossbasis, # defined in pipeline 
  future.packages = c('tidyverse', 'data.table', 'fs'),
  future.globals = c('dir.definecrossbasis',
                     'fp.datawrangling',
                     'municipality.details',
                     'max.characters')) %>% 
  invisible()
