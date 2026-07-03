# 0. set up ----

## 0.1. directory ----

fs::dir_create(dir.finalres) # defined in pipeline

## 0.2. function ----

helper.processTimer <- function(label, expr) {
  message(label)
  t0 <- Sys.time()
  out <- force(expr)
  t1 <- Sys.time()
  message(sprintf(' time: %s.', helper.formatTime(t1, t0)))
  out
}

helper.mergeOrder <-
  function (x, y, ...) {
    
    colnames.x <- colnames(x)
    
    merge(
      x = x,
      y = y,
      ...
    ) %>% 
      data.table::setcolorder(
        x = .,
        neworder = c(
          colnames.x,
          setdiff(x = names(.), y = colnames.x)
        )
      )
  }

helper.summarisePosterior <-
  function(data, summarise.columns) {
    
    # 1. make matrix ----
    
    m <- as.matrix(data[, ..summarise.columns])
    
    # 2. summarise ----
    
    data[, `:=`(
      mean   = Matrix::rowMeans(m),
      var    = matrixStats::rowVars(m),
      median = matrixStats::rowMedians(m),
      lower  = matrixStats::rowQuantiles(m, probs = 0.025),
      upper  = matrixStats::rowQuantiles(m, probs = 0.975)
    )]
    
    # 3. remove columns ----
    
    data[, -summarise.columns, with = FALSE]
    
  }

calculate.areaResults <-
  function(data,
           coefficient.posterior,
           spatial.level = c('municipality', 'regional', 'national')[1],
           percentiles = NULL,
           return.AF = FALSE){
    
    # 0. function arguments ----
    
    # data <- modeldata.munc[[1]]
    # spatial.level <- c('municipality', 'regional', 'national')[1]
    # 
    # # data <- modeldata.rgn[[1]]
    # # spatial.level <- c('municipality', 'regional', 'national')[2]
    # #
    # # data <- model.data
    # # spatial.level <- c('municipality', 'regional', 'national')[3]
    # 
    # coefficient.posterior <- cb.coef
    # percentiles <- (0:100)/100
    # # percentiles <- NULL
    # return.AF <- TRUE
    
    # 1. parameters ----
    
    munc.idx <- data$municipality_id %>% unique()
    munc.cde <- data$Code %>% unique()
    munc.nme <- data$COMUNE %>% unique()
    munc.rgn <- data$Region %>% unique()
    
    colnames.cb <-
      data %>% 
      colnames() %>% 
      grep(pattern = '^cb', value = TRUE)
    
    colnames.id <-
      data %>% 
      colnames() %>% 
      grep(pattern = '_id$', value = TRUE) %>% 
      setdiff(x = ., y = 'municipality_id')
    
    colnames.theta <- 
      coefficient.posterior %>% 
      colnames() %>% 
      grep(pattern = '^theta:', value = TRUE)
    
    # 2. subset ----
    
    ## 2.1. crossbasis ----
    
    subset.crossbasis <- 
      data.table::copy(data) %>% 
      .[, colnames.cb, with = FALSE] %>% 
      as.matrix()
    
    ## 2.2. coeffcients posterior ----
    
    svc.idx <- 
      data.table::CJ(cb = 1:ncol(subset.crossbasis),
                     municipality_id = munc.idx)
    
    subset.posterior <-
      data.table::copy(coefficient.posterior) %>% 
      # 1. filter out needed muncs
      .[parameter %in% sprintf('municipality_cb%s_id:%s', svc.idx$cb, svc.idx$municipality_id)] %>% 
      # 2. average over cb 1, 2, 3, 4 
      .[, lapply(X = .SD, FUN = mean), by = cb, .SDcols = colnames.theta] %>% 
      # 3. select only posterior
      .[, ..colnames.theta] %>% 
      # 4. ensure matrix 
      as.matrix()
    
    # 3. define crossbasis ----
    
    ## 3.1. temperature prediction ----
    
    res.tmp <- 
      data.table::copy(data) %>%
      # 1. define country column 
      .[, Country := 'Italy'] %>% 
      # 2. re-define area-based columns
      {
        if (spatial.level == 'national') {
          .[, Region := 'All']
        }
        if (spatial.level != 'municipality') {
          .[, `:=`(
            Code = NA_integer_,
            COMUNE = 'All',
            municipality_id = NA_integer_
          )]
        }
        .
      } %>%
      # 3. define predicted exposure groups
      ## either based on percentiles or temperature order
      .[, 
        idx.exposure :=
          if (!is.null(percentiles)) {
            # compute percentile breaks
            breaks.exposure <- stats::quantile(
              tmp_lag_03,
              probs = percentiles,
              na.rm = TRUE
            )
            # assign percentile group
            cut(
              tmp_lag_03,
              breaks = breaks.exposure,
              include.lowest = TRUE,
              labels = FALSE
            )
          } else {
            
            tmp_lag_03 %>% 
              data.table::frank(x = ., na.last = 'keep', ties.method = 'first')
            
          } %>% 
          as.numeric()
      ] %>% 
      .[, exposure := sprintf('exposure:%s', idx.exposure)] %>% 
      # 4. define date / year
      .[, `:=` (
        date =
          if (!is.null(percentiles)) {
            as.Date(NA)
          } else {
            date
          },
        year = 
          if (!is.null(percentiles)) {
            NA_integer_
          } else {
            date %>% lubridate::year()
          }
      )] %>% 
      # 5. summarise over municipality and exposure group
      .[, .(
        tmp_lag_03 = tmp_lag_03 %>% mean(),
        deaths = deaths %>% sum(),
        population = population %>% sum(),
        ndvi = ndvi %>% mean(),
        rh_lag_03 = rh_lag_03 %>% mean(),
        ifc2018 = ifc2018 %>% mean(),
        obesity = obesity %>% mean(),
        smoking = smoking %>% mean(),
        meanAlt = meanAlt %>% mean()
      ),
      by = .(date, year, Country, Region, Code, COMUNE, municipality_id, exposure)] %>% 
      # 6. order by exposure group
      data.table::setorder(tmp_lag_03) %>% 
      # 7. new ordered temp
      .[, tmp_lag_03_ordered :=
          if(!is.null(percentiles)) {
            .I
          } else {
            tmp_lag_03
          }
        ]
    
    ## 3.1. spline details (MUST MATCH THOSE TO DEFINE CB INITIALLY) ----
    
    expInfo.x <- res.tmp$tmp_lag_03
    expInfo.fun <- 'ns'
    expInfo.boundary <- 
      data$tmp_lag_03 %>% 
      range()
    expInfo.knots <- 
      data$tmp_lag_03 %>% 
      stats::quantile(x = ., probs = c(10, 75, 90)/100, na.rm = TRUE)
    
    ## 3.2. cross basis ----
    
    cb.exp <- 
      dlnm::onebasis(x = expInfo.x, 
                     fun = expInfo.fun,
                     knots = expInfo.knots,
                     Boundary.knots = expInfo.boundary,
                     intercept = FALSE)
    
    # 4. define posterior for mmt, (centered) rr, and af ----
    
    ## 4.1. set up ----
    
    # 1. parameters
    n.exposure <- expInfo.x %>% length()
    n.theta <- colnames.theta %>% length()
    rownames.exposure <- sprintf('exposure:%s', 1:n.exposure)
    
    # 2. results arrays
    mmt.array <- array(data = NA_real_, dim = c(1, n.theta), dimnames = list('mmt', colnames.theta))
    rr.array <- array(data = NA_real_, dim = c(n.exposure, n.theta), dimnames = list(rownames.exposure, colnames.theta))
    if(return.AF) {
      af.array <- array(data = NA_real_, dim = c(n.exposure, n.theta), dimnames = list(rownames.exposure, colnames.theta))
    }
    
    # 3. mmt range
    range.mmt <- 
      data$tmp_lag_03 %>% 
      # 1. reasonable temperature range
      stats::quantile(x = ., prob = c(25, 90)/100, na.rm = TRUE) %>% 
      # 2. convert to numeric
      as.numeric()
    
    # 4. index of temperaures from data in the range 
    idx.temperature <- (data$tmp_lag_03 >= range.mmt[1] & data$tmp_lag_03 <= range.mmt[2])
    
    ## 4.2. calculate results ----
    
    for (j in seq_len(n.theta)) {
      
      # 0. loop parameter ----
      
      # j <- 1
      
      # 1. define mmt for 1 sample ----
      
      # a. location of min-rr
      idx.minRR <- 
        # define rr
        (subset.crossbasis[idx.temperature, , drop = FALSE] %*% subset.posterior[, j]) %>% 
        # location of min
        which.min()
      # b. value of min-temp from min-rr
      mmt.j <- data$tmp_lag_03[idx.temperature][idx.minRR]
      
      # 2. define centering basis for mmt ----
      
      # a. cloest defined temp to min rr
      idx.tmp.closest <- which.min(abs(expInfo.x - mmt.j))
      # b. centering matrix
      cb.cen <- 
        dlnm::onebasis(
          x = expInfo.x[idx.tmp.closest],
          fun = expInfo.fun,
          knots = expInfo.knots,
          Boundary.knots = expInfo.boundary,
          intercept = FALSE
        )
      
      # 3. rescale cb ----
      
      # a. uncetered eta 
      eta.j.uncen <- cb.exp %*% subset.posterior[, j]
      # b. value to shift by
      eta.j.shift <- as.numeric(cb.cen %*% subset.posterior[, j])
      # c. subtract shift (more efficient than scale())
      eta.j <- 
        sweep(
          x = eta.j.uncen, 
          MARGIN = 2, 
          STATS = eta.j.shift, 
          FUN = '-'
        )
      
      # 4. update array ----
      
      # a. mmt 
      mmt.array[,j] <- mmt.j
      # b. rr
      rr.array[,j] <- exp(eta.j)
      # c. af
      if (return.AF) { af.array[, j] <- -expm1(-eta.j) } # RR - 1 / RR
      
    }
    
    # 5. rr and af from median mmt ----
    
    # a. median mmt
    mmt.median <- mmt.array %>% median()
    # b. temperature closest 
    idx.tmp.closest <- which.min(abs(expInfo.x - mmt.median))
    # c. centering basis
    cb.cen <- 
      dlnm::onebasis(
        x = expInfo.x[idx.tmp.closest],
        fun = expInfo.fun,
        knots = expInfo.knots,
        Boundary.knots = expInfo.boundary,
        intercept = FALSE
      )
    # d. uncetered eta 
    eta.uncen <- cb.exp %*% subset.posterior
    # e. value to shift by
    eta.shift <- as.numeric(cb.cen %*% subset.posterior)
    # f. subtract shift (more efficient than scale())
    eta <- 
      sweep(
        x = eta.uncen, 
        MARGIN = 2, 
        STATS = eta.shift, 
        FUN = '-'
      )
    # g. 
    rr.mmt.median <- exp(eta)
    rownames(rr.mmt.median) <- rownames.exposure
    if (return.AF) { 
      af.mmt.median <- -expm1(-eta)
      rownames(af.mmt.median) <- rownames.exposure
    }
    
    # 6. meta data ----
    
    metadata.mmt <- 
      res.tmp %>% 
      .[, .(Country, Region, Code, COMUNE, municipality_id)] %>% 
      unique()
    
    metadata.rr <- res.tmp
    
    # 7. return ----
    
    out <- list(
      metadata.mmt = metadata.mmt,
      metadata.rr = metadata.rr,
      mmt.posterior = mmt.array,
      rr.posterior = rr.array,
      mmt.median = mmt.median,
      rr.posterior.medianMMT = rr.mmt.median
    )
    
    if (return.AF) { 
      out$af.posterior <- af.array
      out$af.posterior.medianMMT <- af.mmt.median
    }
    
    return(out)
    
  }

format.mmt <-
  function(results,
           spatial.level = c('municipality', 'regional', 'national')[1],
           dir.target) {
    
    # 0. function arguments ----
    
    # results <- res
    # spatial.level <- c('municipality', 'regional', 'national')[1]
    # dir.target <- fp.munc.observ
    
    # 1. parameters ----
    
    # a. create directory
    fs::dir_create(dir.target)
    
    # b define merge by cols
    by.col <-
      switch(EXPR = spatial.level,
             municipality = 'municipality_id',
             regional = 'Region',
             national = 'Country')
    
    # c. posterior fp 
    fp.mmt <- file.path(dir.target, 'posterior_mmt.rds')
    
    # d. summary fp
    fp.mmt.smry <- file.path(dir.target, 'summary_mmt.rds')
    
    # 2. meta data ----
    
    metadata.mmt <-
      lapply(X = results,
             FUN = function(x) x$metadata.mmt) %>% 
      data.table::rbindlist()
    
    # 3. posterior ----
    
    if(!file.exists(fp.mmt)){
      
      mmt.posterior <-
        helper.processTimer(
          label = ' formatting mmt posterior...',
          expr = 
            lapply(X = results,
                   FUN = function(x) x$mmt.posterior %>% data.table::as.data.table()) %>% 
            data.table::rbindlist(l = ., 
                                  use.names = TRUE,
                                  idcol = by.col) %>% 
            {
              if(spatial.level == 'municipality') {
                .[, (by.col) := get(by.col) %>% as.integer()]
              } else {
                .
              }
            } %>% 
            helper.mergeOrder(x = metadata.mmt,
                              y = ., 
                              by = by.col,
                              all.x = TRUE) %>% 
            data.table::setorderv(x = ., by.col)
        )
      
      helper.processTimer(
        label = ' saving mmt posterior...',
        expr = saveRDS(object = mmt.posterior, file = fp.mmt)
      )
      
    } else {
      
      mmt.posterior <-
        helper.processTimer(
          label = ' loading mmt posterior...',
          expr = readRDS(file = fp.mmt)
        )
      
    }
    
    # 4. summary ----
    
    if(!file.exists(fp.mmt.smry)){
      
      mmt.posterior.summary <-
        helper.processTimer(
          label = ' summarise mmt posterior...',
          expr = {
            
            colnames.theta <- mmt.posterior %>% colnames() %>% grep(pattern = 'theta:', x = ., value = TRUE)
            
            data.table::copy(mmt.posterior) %>% 
              helper.summarisePosterior(data = ., summarise.columns = colnames.theta)
            
          }
        )
      
      helper.processTimer(
        label = ' saving mmt summary...',
        expr = saveRDS(object = mmt.posterior.summary, file = fp.mmt.smry)
      )
      
    } else {
      
      helper.processTimer(
        label = ' mmt summary already exists. skipping...',
        expr = NULL
      )
      
    }
    
    
    
  }

format.rr <-
  function(results,
           spatial.level = c('municipality', 'regional', 'national')[1],
           dir.target,
           with.medianMMT = FALSE){
    
    # 0. function arguments ----
    
    # # results <- res.obsv
    # results <- res.perc
    # spatial.level <- c('municipality', 'regional', 'national')[1]
    # dir.target <- fp.munc.obsv
    # with.medianMMT <- FALSE
    
    # 1. parameters ----
    
    # a. create directory
    fs::dir_create(dir.target)
    
    # b define merge by cols
    by.col <-
      switch(EXPR = spatial.level,
             municipality = 'municipality_id',
             regional = 'Region',
             national = 'Country')
    
    # posterior filepath
    fp.rr <- if (with.medianMMT) {
      file.path(dir.target, 'posterior_rr_medianMMT.rds')
    } else {
      file.path(dir.target, 'posterior_rr.rds')
    }
    
    # summary filepath
    fp.rr.smry <- if (with.medianMMT) {
      file.path(dir.target, 'summary_rr_medianMMT.rds')
    } else {
      file.path(dir.target, 'summary_rr.rds')
    }
    
    # samples from results
    rr.smpls <- if (with.medianMMT) {
      'rr.posterior.medianMMT'
    } else {
      'rr.posterior'
    }
    
    # 2. meta data ----
    
    metadata.rr <-
      lapply(X = results,
             FUN = function(x) x$metadata.rr) %>% 
      data.table::rbindlist()
    
    # 3. posterior ----
    
    if(!file.exists(fp.rr)) {
      
      rr.posterior <-
        helper.processTimer(
          label = ' formatting rr posterior...',
          expr = 
            lapply(X = results,
                   FUN = function(x) x[[rr.smpls]] %>% data.table::as.data.table(x = ., keep.rownames = 'exposure')) %>% 
            data.table::rbindlist(l = ., 
                                  use.names = TRUE,
                                  idcol = by.col) %>% 
            {
              if(spatial.level == 'municipality') {
                .[, (by.col) := get(by.col) %>% as.integer()]
              } else {
                .
              }
            } %>% 
            helper.mergeOrder(x = metadata.rr,
                              y = ., 
                              by = c(by.col, 'exposure'),
                              all.x = TRUE) %>% 
            data.table::setorderv(x = ., c(by.col, 'tmp_lag_03'))
        )
      
      helper.processTimer(
        label = ' saving rr posterior...',
        expr = saveRDS(object = rr.posterior, file = fp.rr)
      )
      
    } else {
      
      rr.posterior <-
        helper.processTimer(
          label = ' loading rr posterior...',
          expr = readRDS(file = fp.rr)
        )
      
    }
    
    # 4. summary ----
    
    if(!file.exists(fp.rr.smry)) {
      
      rr.posterior.summary <-
        helper.processTimer(
          label = ' summarise rr posterior...',
          expr = {
            
            # 1. set up ----
            
            colnames.theta <- rr.posterior %>% colnames() %>% grep(pattern = 'theta:', x = ., value = TRUE)
            
            rr <- list()
            
            # 2. country ----
            
            rr.ctry <- 
              data.table::copy(rr.posterior) %>% 
              # 1. aggregate over rgn 
              ## 1.a. rename 
              .[, `:=` (
                Region = 'All',
                Code = NA_integer_,
                COMUNE = 'All',
                municipality_id = NA_integer_
              )] %>% 
              ## 1.b. average
              .[, lapply(X = .SD, FUN = mean),
                by = .(Country, Region, Code, COMUNE, municipality_id, tmp_lag_03_ordered),
                .SDcols = colnames.theta] %>% 
              # 2. summarise 
              helper.summarisePosterior(data = ., summarise.columns = colnames.theta)
            
            rr$rr.country <- rr.ctry
            
            # 3. region ----
            
            if(spatial.level %in% c('municipality', 'regional')){
              
              rr.rgn <- 
                data.table::copy(rr.posterior) %>% 
                # 1. aggregate over rgn 
                ## 1.a. rename 
                .[, `:=` (
                  Code = NA_integer_,
                  COMUNE = 'All',
                  municipality_id = NA_integer_
                )] %>% 
                ## 1.b. average
                .[, lapply(X = .SD, FUN = mean),
                  by = .(Country, Region, Code, COMUNE, municipality_id, tmp_lag_03_ordered),
                  .SDcols = colnames.theta] %>% 
                # 2. summarise 
                helper.summarisePosterior(data = ., summarise.columns = colnames.theta)
              
              rr$rr.region <- rr.rgn
              
            }
            
            # 4. municipality ----
            
            if(spatial.level == 'municipality') {
              
              rr.munc <- 
                data.table::copy(rr.posterior) %>% 
                # 1. aggregate over munc 
                ## 1.b. average
                .[, lapply(X = .SD, FUN = mean),
                  by = .(Country, Region, Code, COMUNE, municipality_id, tmp_lag_03_ordered),
                  .SDcols = colnames.theta] %>% 
                # 1. summarise 
                helper.summarisePosterior(data = ., summarise.columns = colnames.theta)
              
              rr$rr.municipality <- rr.munc
              
            }
            
            # r. return ----
            
            rr # do NOT need return(...)
            
          }
          
        )
      
      helper.processTimer(
        label = ' saving rr summary...',
        expr = saveRDS(object = rr.posterior.summary, file = fp.rr.smry)
      )
      
    } else {
      helper.processTimer(
        label = ' rr summary already exists. skipping...',
        expr = NULL
      )
    }
    
  }

format.af <-
  function(results,
           spatial.level = c('municipality', 'regional', 'national')[1],
           dir.target,
           with.medianMMT = FALSE){
    
    # 0. function arguments ----
    
    # results <- res.obsv
    # # results <- res.perc
    # spatial.level <- c('municipality', 'regional', 'national')[1]
    # dir.target <- fp.munc.obsv
    # with.medianMMT <- FALSE
    
    # 1. parameters ----
    
    # a. create directory
    fs::dir_create(dir.target)
    
    # b define merge by cols
    by.col <-
      switch(EXPR = spatial.level,
             municipality = 'municipality_id',
             regional = 'Region',
             national = 'Country')
    
    # posterior filepath
    fp.af <- if (with.medianMMT) {
      file.path(dir.target, 'posterior_af_medianMMT.rds')
    } else {
      file.path(dir.target, 'posterior_af.rds')
    }
    
    # summary filepath
    fp.af.smry <- if (with.medianMMT) {
      file.path(dir.target, 'summary_af_medianMMT.rds')
    } else {
      file.path(dir.target, 'summary_af.rds')
    }
    
    # samples from results
    af.smpls <- if (with.medianMMT) {
      'af.posterior.medianMMT'
    } else {
      'af.posterior'
    }
    
    # 2. meta data ----
    
    metadata.af <-
      lapply(X = results,
             # is the same as rr -- not a typo
             FUN = function(x) x$metadata.rr) %>% 
      data.table::rbindlist()
    
    # 3. posterior ----
    
    if(!file.exists(fp.af)) {
      
      af.posterior <-
        helper.processTimer(
          label = ' formatting af posterior...',
          expr = 
            lapply(X = results,
                   FUN = function(x) x[[af.smpls]] %>% data.table::as.data.table(x = ., keep.rownames = 'exposure')) %>% 
            data.table::rbindlist(l = ., 
                                  use.names = TRUE,
                                  idcol = by.col) %>% 
            {
              if(spatial.level == 'municipality') {
                .[, (by.col) := get(by.col) %>% as.integer()]
              } else {
                .
              }
            } %>% 
            helper.mergeOrder(x = metadata.af,
                              y = ., 
                              by = c(by.col, 'exposure'),
                              all.x = TRUE) %>% 
            data.table::setorderv(x = ., c(by.col, 'tmp_lag_03'))
        )
      
      helper.processTimer(
        label = ' saving af posterior...',
        expr = saveRDS(object = af.posterior, file = fp.af)
      )
      
    } else {
      
      af.posterior <-
        helper.processTimer(
          label = ' loading af posterior...',
          expr = readRDS(file = fp.af)
        )
      
    }
    
    # 4. summary ----
    
    if(!file.exists(fp.af.smry)) {
      
      af.posterior.summary <-
        helper.processTimer(
          label = ' summarise af posterior...',
          expr = {
            
            # 1. set up ----
            
            colnames.theta <- af.posterior %>% colnames() %>% grep(pattern = 'theta:', x = ., value = TRUE)
            
            af.base <- 
              data.table::copy(af.posterior) %>% 
              .[, (colnames.theta) := lapply(.SD, `*`, deaths), .SDcols = colnames.theta]
            
            af <- list()
            
            # 2. country ----
            
            af.ctry <-
              data.table::copy(af.base) %>% 
              # 1. aggregate over ctry 
              ## 1.a. rename 
              .[, `:=` (
                Region = 'All',
                Code = NA_integer_,
                COMUNE = 'All',
                municipality_id = NA_integer_
              )] %>% 
              ## 1.b. sum
              .[, lapply(X = .SD, FUN = sum),
                by = .(year, Country, Region, Code, COMUNE, municipality_id),
                .SDcols = colnames.theta] %>% 
              # 2. summarise 
              helper.summarisePosterior(data = ., summarise.columns = colnames.theta)
            
            af$af.country <- af.ctry
            
            # 3. region ----
            
            if(spatial.level %in% c('municipality', 'regional')){
              
              af.rgn <- 
                data.table::copy(af.base) %>% 
                # 1. aggregate over rgn 
                ## 1.a. rename 
                .[, `:=` (
                  Code = NA_integer_,
                  COMUNE = 'All',
                  municipality_id = NA_integer_
                )] %>% 
                ## 1.b. sum
                .[, lapply(X = .SD, FUN = sum),
                  by = .(year, Country, Region, Code, COMUNE, municipality_id),
                  .SDcols = colnames.theta] %>% 
                # 2. summarise 
                helper.summarisePosterior(data = ., summarise.columns = colnames.theta)
              
              af$af.region <- af.rgn
              
            }
            
            # 4. municipality ----
            
            if(spatial.level == 'municipality') {
              
              af.munc <- 
                data.table::copy(af.base) %>% 
                # 1. aggregate
                .[, lapply(X = .SD, FUN = sum),
                  by = .(year, Country, Region, Code, COMUNE, municipality_id),
                  .SDcols = colnames.theta] %>% 
                # 2. summarise 
                helper.summarisePosterior(data = ., summarise.columns = colnames.theta)
              
              af$af.municipality <- af.munc
              
            }
            
            # 5. return ----
            
            af # do NOT need return(...)
            
          }
          
        )
      
      helper.processTimer(
        label = ' saving af summary...',
        expr = saveRDS(object = af.posterior.summary, file = fp.af.smry)
      )
      
    } else {
      helper.processTimer(
        label = ' af summary already exists. skipping...',
        expr = NULL
      )
    }
    
  }

format.areaResults <- 
  function(results,
           spatial.level = c('municipality', 'regional', 'national')[1],
           dir.target,
           save.from.medianMMT = TRUE) {
    
    # 0. function arguments ----
    
    # results <- res
    # spatial.level <- c('municipality', 'regional', 'national')[1]
    # dir.target <- fp.munc.observ
    
    # 1. parameters ----
    
    # a. create directory
    fs::dir_create(dir.target)
    
    # b define merge by cols
    by.col <-
      switch(EXPR = spatial.level,
             municipality = 'municipality_id',
             regional = 'Region',
             national = 'Country')
    
    # 2. meta data ----
    
    # a. mmt meta data
    metadata.mmt <-
      lapply(X = results,
             FUN = function(x) x$metadata.mmt) %>% 
      data.table::rbindlist()
    
    # b. rr (and af) metadata
    metadata.rr <-
      lapply(X = results,
             FUN = function(x) x$metadata.rr) %>% 
      data.table::rbindlist()
    
    # 3. mmt ----
    
    ## 3.1. posterior ----
    
    fp.mmt <- file.path(dir.target, 'posterior_mmt.rds')
    if(!file.exists(fp.mmt)){
      
      mmt.posterior <-
        helper.processTimer(
          label = ' formatting mmt posterior...',
          expr = 
            lapply(X = results,
                   FUN = function(x) x$mmt.posterior %>% data.table::as.data.table()) %>% 
            data.table::rbindlist(l = ., 
                                  use.names = TRUE,
                                  idcol = by.col) %>% 
            {
              if(spatial.level == 'municipality') {
                .[, (by.col) := get(by.col) %>% as.integer()]
              } else {
                .
              }
            } %>% 
            helper.mergeOrder(x = metadata.mmt,
                              y = ., 
                              by = by.col,
                              all.x = TRUE) %>% 
            data.table::setorderv(x = ., by.col)
        )
      
      helper.processTimer(
        label = ' saving mmt posterior...',
        expr = saveRDS(object = mmt.posterior, file = fp.mmt)
      )
      
    } else {
      
      mmt.posterior <-
        helper.processTimer(
          label = ' loading mmt posterior...',
          expr = readRDS(file = fp.mmt)
        )
      
    }
    
    ## 3.2. summary ----
    
    fp.mmt.smmry <- file.path(dir.target, 'summary_mmt.rds')
    if(!file.exists(fp.mmt.smmry)){
      
      mmt.posterior.summary <-
        helper.processTimer(
          label = ' summarise mmt posterior...',
          expr = {
            
            colnames.theta <- mmt.posterior %>% colnames() %>% grep(pattern = 'theta:', x = ., value = TRUE)
            
            data.table::copy(mmt.posterior) %>% 
              # summarise 
              {
                dt <- .
                m  <- as.matrix(dt[, ..colnames.theta])
                
                dt[, `:=`(
                  mean   = Matrix::rowMeans(m),
                  var    = matrixStats::rowVars(m),
                  median = matrixStats::rowMedians(m),
                  lower  = matrixStats::rowQuantiles(m, probs = 0.025),
                  upper  = matrixStats::rowQuantiles(m, probs = 0.975)
                )]
                
                dt
              } %>%
              # remove theta
              .[, -colnames.theta, with = FALSE]
            
          }
        )
      
      helper.processTimer(
        label = ' saving mmt summary...',
        expr = saveRDS(object = mmt.posterior.summary, file = fp.mmt.smmry)
      )
      
    } else {
      helper.processTimer(
        label = ' mmt summary already exists. skipping...',
        expr = NULL
      )
    }
    
    ## 3.3. clear ----
    
    rm(mmt.posterior, 
       mmt.posterior.summary)
    gc()
    
    # 4. rr ----
    
    ## 4.1. posterior ----
    
    fp.rr <- file.path(dir.target, 'posterior_rr.rds')
    if(!file.exists(fp.rr)) {
      
      rr.posterior <-
        helper.processTimer(
          label = ' formatting rr posterior...',
          expr = 
            lapply(X = results,
                   FUN = function(x) x$rr.posterior %>% data.table::as.data.table(x = ., keep.rownames = 'exposure')) %>% 
            data.table::rbindlist(l = ., 
                                  use.names = TRUE,
                                  idcol = by.col) %>% 
            {
              if(spatial.level == 'municipality') {
                .[, (by.col) := get(by.col) %>% as.integer()]
              } else {
                .
              }
            } %>% 
            helper.mergeOrder(x = metadata.rr,
                              y = ., 
                              by = c(by.col, 'exposure'),
                              all.x = TRUE) %>% 
            data.table::setorderv(x = ., c(by.col, 'tmp_lag_03'))
        )
      
      helper.processTimer(
        label = ' saving rr posterior...',
        expr = saveRDS(object = rr.posterior, file = fp.rr)
      )
      
    } else {
      rr.posterior <-
        helper.processTimer(
          label = ' loading rr posterior...',
          expr = readRDS(file = fp.rr)
        )
    }
    
    ## 4.2. summary ----
    
    fp.rr.smmry <- file.path(dir.target, 'summary_rr.rds')
    if(!file.exists(fp.rr.smmry)) {
      
      rr.posterior.summary <-
        helper.processTimer(
          label = ' summarise rr posterior...',
          expr = {
            
            colnames.theta <- rr.posterior %>% colnames() %>% grep(pattern = 'theta:', x = ., value = TRUE)
            
            data.table::copy(rr.posterior) %>% 
              # 1. summarise 
              {
                dt <- .
                m  <- as.matrix(dt[, ..colnames.theta])
                
                dt[, `:=`(
                  mean   = Matrix::rowMeans(m),
                  var    = matrixStats::rowVars(m),
                  median = matrixStats::rowMedians(m),
                  lower  = matrixStats::rowQuantiles(m, probs = 0.025),
                  upper  = matrixStats::rowQuantiles(m, probs = 0.975)
                )]
                
                dt
              } %>%
              # 2. remove theta
              .[, -colnames.theta, with = FALSE]
            
          }
        )
      
      helper.processTimer(
        label = ' saving rr summary...',
        expr = saveRDS(object = rr.posterior.summary, file = fp.rr.smmry)
      )
      
    } else {
      helper.processTimer(
        label = ' rr summary already exists. skipping...',
        expr = NULL
      )
    }
    
    ## 4.3. clear ----
    
    rm(rr.posterior, 
       rr.posterior.summary)
    gc()
    
    # 5. af ----
    
    ## 5.1. posterior ----
    
    fp.af <- file.path(dir.target, 'posterior_af.rds')
    if(!file.exists(fp.af)) {
      
      af.posterior <-
        helper.processTimer(
          label = ' formatting af posterior...',
          expr = 
            lapply(X = results,
                   FUN = function(x) x$af.posterior %>% data.table::as.data.table(x = ., keep.rownames = 'exposure')) %>% 
            data.table::rbindlist(l = ., 
                                  use.names = TRUE,
                                  idcol = by.col) %>% 
            {
              if(spatial.level == 'municipality') {
                .[, (by.col) := get(by.col) %>% as.integer()]
              } else {
                .
              }
            } %>% 
            helper.mergeOrder(x = metadata.rr,
                              y = ., 
                              by = c(by.col, 'exposure'),
                              all.x = TRUE) %>% 
            data.table::setorderv(x = ., c(by.col, 'tmp_lag_03'))
        )
      
      helper.processTimer(
        label = ' saving af posterior...',
        expr = saveRDS(object = af.posterior, file = fp.af)
      )
      
    } else {
      af.posterior <-
        helper.processTimer(
          label = ' loading af posterior...',
          expr = readRDS(file = fp.af)
        )
    }
    
    ## 5.2. summary ----
    
    fp.af.smmry <- file.path(dir.target, 'summary_af.rds')
    if(spatial.level == 'municipality') {
      if(!file.exists(fp.af.smmry)) {
        
        af.posterior.summary <-
          helper.processTimer(
            label = ' summarise af posterior...',
            expr = {
              
              colnames.theta <- af.posterior %>% colnames() %>% grep(pattern = 'theta:', x = ., value = TRUE)
              
              af.munc <- 
                data.table::copy(af.posterior) %>% 
                # 1. multiple theta columns by deaths 
                .[, (colnames.theta) := lapply(.SD, `*`, deaths), .SDcols = colnames.theta] %>% 
                # 2. summarise over all exposures for country/region/code/comune/munc_id
                .[, lapply(X = .SD, FUN = sum),
                  by = .(year, Country, Region, Code, COMUNE, municipality_id),
                  .SDcols = colnames.theta] %>% 
                # 3. summarise
                {
                  dt <- .
                  m  <- as.matrix(dt[, ..colnames.theta])
                  
                  dt[, `:=`(
                    mean   = Matrix::rowMeans(m),
                    var    = matrixStats::rowVars(m),
                    median = matrixStats::rowMedians(m),
                    lower  = matrixStats::rowQuantiles(m, probs = 0.025),
                    upper  = matrixStats::rowQuantiles(m, probs = 0.975)
                  )]
                  
                  dt
                } %>%
                # 4. remove theta
                .[, -colnames.theta, with = FALSE]
              
              af.rgn <-
                data.table::copy(af.posterior) %>% 
                # 1. rename for region
                .[, `:=` (
                  Code = NA_integer_,
                  COMUNE = 'All',
                  municipality_id = NA_integer_
                )] %>% 
                # 2. multiple theta columns by deaths 
                .[, (colnames.theta) := lapply(.SD, `*`, deaths), .SDcols = colnames.theta] %>% 
                # 3. summarise over all exposures for country/region/code/comune/munc_id
                .[, lapply(X = .SD, FUN = sum),
                  by = .(year, Country, Region, Code, COMUNE, municipality_id),
                  .SDcols = colnames.theta] %>% 
                # 4. summarise
                {
                  dt <- .
                  m  <- as.matrix(dt[, ..colnames.theta])
                  
                  dt[, `:=`(
                    mean   = Matrix::rowMeans(m),
                    var    = matrixStats::rowVars(m),
                    median = matrixStats::rowMedians(m),
                    lower  = matrixStats::rowQuantiles(m, probs = 0.025),
                    upper  = matrixStats::rowQuantiles(m, probs = 0.975)
                  )]
                  
                  dt
                } %>%
                # 5. remove theta
                .[, -colnames.theta, with = FALSE]
              
              af.ctry <-
                data.table::copy(af.posterior) %>% 
                # 1. rename for country
                .[, `:=` (
                  Region = 'All',
                  Code = NA_integer_,
                  COMUNE = 'All',
                  municipality_id = NA_integer_
                )] %>% 
                # 2. multiple theta columns by deaths 
                .[, (colnames.theta) := lapply(.SD, `*`, deaths), .SDcols = colnames.theta] %>% 
                # 3. summarise over all exposures for country/region/code/comune/munc_id
                .[, lapply(X = .SD, FUN = sum),
                  by = .(year, Country, Region, Code, COMUNE, municipality_id),
                  .SDcols = colnames.theta] %>% 
                # 4. summarise
                {
                  dt <- .
                  m  <- as.matrix(dt[, ..colnames.theta])
                  
                  dt[, `:=`(
                    mean   = Matrix::rowMeans(m),
                    var    = matrixStats::rowVars(m),
                    median = matrixStats::rowMedians(m),
                    lower  = matrixStats::rowQuantiles(m, probs = 0.025),
                    upper  = matrixStats::rowQuantiles(m, probs = 0.975)
                  )]
                  
                  dt
                } %>%
                # 5. remove theta
                .[, -colnames.theta, with = FALSE]
              
              af <- 
                list(af.country = af.ctry,
                     af.region = af.rgn,
                     af.municipality = af.munc)
              
              af
              
            }
          )
        
        helper.processTimer(
          label = ' saving af summary...',
          expr = saveRDS(object = af.posterior.summary, file = fp.af.smmry)
        )
        
      } else {
        helper.processTimer(
          label = ' af summary already exists. skipping...',
          expr = NULL
        )
      }
      rm(af.posterior.summary)
    }
    
    ## 5.3. clear ----
    
    rm(af.posterior)
    gc()
    
    # 6. rr posterior (from median mmt) ----
    
    if(save.from.medianMMT){
      
      ## 6.1. posterior ----
      
      fp.rr.medianMMT <- file.path(dir.target, 'posterior_rr_medianMMT.rds')
      if(!file.exists(fp.rr.medianMMT)) {
        
        rr.posterior.medianMMT <-
          helper.processTimer(
            label = ' formatting rr (from median mmt) posterior...',
            expr = 
              lapply(X = results,
                     FUN = function(x) x$rr.posterior.medianMMT %>% data.table::as.data.table(x = ., keep.rownames = 'exposure')) %>% 
              data.table::rbindlist(l = ., 
                                    use.names = TRUE,
                                    idcol = by.col) %>% 
              {
                if(spatial.level == 'municipality') {
                  .[, (by.col) := get(by.col) %>% as.integer()]
                } else {
                  .
                }
              } %>% 
              helper.mergeOrder(x = metadata.rr,
                                y = ., 
                                by = c(by.col, 'exposure'),
                                all.x = TRUE) %>% 
              data.table::setorderv(x = ., c(by.col, 'tmp_lag_03'))
          )
        
        helper.processTimer(
          label = ' saving rr (from median mmt) posterior...',
          expr = saveRDS(object = rr.posterior.medianMMT, file = fp.rr.medianMMT)
        )
        
      } else {
        rr.posterior.medianMMT <-
          helper.processTimer(
            label = ' loading rr (from median mmt) posterior...',
            expr = readRDS(file = fp.rr.medianMMT)
          )
      }
      
      ## 6.2. summary ----
      
      fp.rr.medianMMT.smmry <- file.path(dir.target, 'summary_rr_medianMMT.rds')
      if(!file.exists(fp.rr.medianMMT.smmry)) {
        
        rr.posterior.medianMMT.summary <-
          helper.processTimer(
            label = ' summarise rr (from median mmt) posterior...',
            expr = {
              
              colnames.theta <- rr.posterior.medianMMT %>% colnames() %>% grep(pattern = 'theta:', x = ., value = TRUE)
              
              data.table::copy(rr.posterior.medianMMT) %>% 
                # 1. summarise 
                {
                  dt <- .
                  m  <- as.matrix(dt[, ..colnames.theta])
                  
                  dt[, `:=`(
                    mean   = Matrix::rowMeans(m),
                    var    = matrixStats::rowVars(m),
                    median = matrixStats::rowMedians(m),
                    lower  = matrixStats::rowQuantiles(m, probs = 0.025),
                    upper  = matrixStats::rowQuantiles(m, probs = 0.975)
                  )]
                  
                  dt
                } %>%
                # 2. remove theta
                .[, -colnames.theta, with = FALSE]
              
            }
          )
        
        helper.processTimer(
          label = ' saving rr (from median mmt) summary...',
          expr = saveRDS(object = rr.posterior.medianMMT.summary, file = fp.rr.medianMMT.smmry)
        )
        
      } else {
        helper.processTimer(
          label = ' rr (from median mmt) summary already exists. skipping...',
          expr = NULL
        )
      }
      
      ## 6.3. clear ----
      
      rm(rr.posterior.medianMMT, 
         rr.posterior.medianMMT.summary)
      gc()
      
    }
    
    # 7. af (from mmt median) ----
    
    if(save.from.medianMMT){
      
      ## 7.1. posterior ----
      
      fp.af.medianMMT <- file.path(dir.target, 'posterior_af_medianMMT.rds')
      if(!file.exists(fp.af.medianMMT)) {
        
        af.posterior.medianMMT <-
          helper.processTimer(
            label = ' formatting af (from median mmt) posterior...',
            expr = 
              lapply(X = results,
                     FUN = function(x) x$af.posterior.medianMMT %>% data.table::as.data.table(x = ., keep.rownames = 'exposure')) %>% 
              data.table::rbindlist(l = ., 
                                    use.names = TRUE,
                                    idcol = by.col) %>% 
              {
                if(spatial.level == 'municipality') {
                  .[, (by.col) := get(by.col) %>% as.integer()]
                } else {
                  .
                }
              } %>% 
              helper.mergeOrder(x = metadata.rr,
                                y = ., 
                                by = c(by.col, 'exposure'),
                                all.x = TRUE) %>% 
              data.table::setorderv(x = ., c(by.col, 'tmp_lag_03'))
          )
        
        helper.processTimer(
          label = ' saving af (from median mmt) posterior...',
          expr = saveRDS(object = af.posterior.medianMMT, file = fp.af.medianMMT)
        )
        
      } else {
        af.posterior.medianMMT <-
          helper.processTimer(
            label = ' loading af (from median mmt) posterior...',
            expr = readRDS(file = fp.af.medianMMT)
          )
      }
      
      ## 7.2. summary ----
      
      fp.af.medianMMT.smmry <- file.path(dir.target, 'summary_af_medianMMT.rds')
      if(spatial.level == 'municipality') {
        if(!file.exists(fp.af.medianMMT.smmry)) {
          
          af.posterior.medianMMT.summary <-
            helper.processTimer(
              label = ' summarise af (from median mmt) posterior...',
              expr = {
                
                colnames.theta <- af.posterior.medianMMT %>% colnames() %>% grep(pattern = 'theta:', x = ., value = TRUE)
                
                af.munc <- 
                  data.table::copy(af.posterior.medianMMT) %>% 
                  # 1. multiple theta columns by deaths 
                  .[, (colnames.theta) := lapply(.SD, `*`, deaths), .SDcols = colnames.theta] %>% 
                  # 2. summarise over all exposures for country/region/code/comune/munc_id
                  .[, lapply(X = .SD, FUN = sum),
                    by = .(year, Country, Region, Code, COMUNE, municipality_id),
                    .SDcols = colnames.theta] %>% 
                  # 3. summarise
                  {
                    dt <- .
                    m  <- as.matrix(dt[, ..colnames.theta])
                    
                    dt[, `:=`(
                      mean   = Matrix::rowMeans(m),
                      var    = matrixStats::rowVars(m),
                      median = matrixStats::rowMedians(m),
                      lower  = matrixStats::rowQuantiles(m, probs = 0.025),
                      upper  = matrixStats::rowQuantiles(m, probs = 0.975)
                    )]
                    
                    dt
                  } %>%
                  # 4. remove theta
                  .[, -colnames.theta, with = FALSE]
                
                af.rgn <-
                  data.table::copy(af.posterior.medianMMT) %>% 
                  # 1. rename for region
                  .[, `:=` (
                    Code = NA_integer_,
                    COMUNE = 'All',
                    municipality_id = NA_integer_
                  )] %>% 
                  # 2. multiple theta columns by deaths 
                  .[, (colnames.theta) := lapply(.SD, `*`, deaths), .SDcols = colnames.theta] %>% 
                  # 3. summarise over all exposures for country/region/code/comune/munc_id
                  .[, lapply(X = .SD, FUN = sum),
                    by = .(year, Country, Region, Code, COMUNE, municipality_id),
                    .SDcols = colnames.theta] %>% 
                  # 4. summarise
                  {
                    dt <- .
                    m  <- as.matrix(dt[, ..colnames.theta])
                    
                    dt[, `:=`(
                      mean   = Matrix::rowMeans(m),
                      var    = matrixStats::rowVars(m),
                      median = matrixStats::rowMedians(m),
                      lower  = matrixStats::rowQuantiles(m, probs = 0.025),
                      upper  = matrixStats::rowQuantiles(m, probs = 0.975)
                    )]
                    
                    dt
                  } %>%
                  # 5. remove theta
                  .[, -colnames.theta, with = FALSE]
                
                af.ctry <-
                  data.table::copy(af.posterior.medianMMT) %>% 
                  # 1. rename for country
                  .[, `:=` (
                    Region = 'All',
                    Code = NA_integer_,
                    COMUNE = 'All',
                    municipality_id = NA_integer_
                  )] %>% 
                  # 2. multiple theta columns by deaths 
                  .[, (colnames.theta) := lapply(.SD, `*`, deaths), .SDcols = colnames.theta] %>% 
                  # 3. summarise over all exposures for country/region/code/comune/munc_id
                  .[, lapply(X = .SD, FUN = sum),
                    by = .(year, Country, Region, Code, COMUNE, municipality_id),
                    .SDcols = colnames.theta] %>% 
                  # 4. summarise
                  {
                    dt <- .
                    m  <- as.matrix(dt[, ..colnames.theta])
                    
                    dt[, `:=`(
                      mean   = Matrix::rowMeans(m),
                      var    = matrixStats::rowVars(m),
                      median = matrixStats::rowMedians(m),
                      lower  = matrixStats::rowQuantiles(m, probs = 0.025),
                      upper  = matrixStats::rowQuantiles(m, probs = 0.975)
                    )]
                    
                    dt
                  } %>%
                  # 5. remove theta
                  .[, -colnames.theta, with = FALSE]
                
                af
                
              }
            )
          
          helper.processTimer(
            label = ' saving af (from median mmt) summary...',
            expr = saveRDS(object = af.posterior.medianMMT.summary, file = fp.af.medianMMT.smmry)
          )
          
        } else {
          helper.processTimer(
            label = ' af (from median mmt) summary already exists. skipping...',
            expr = NULL
          )
        }
        rm(af.posterior.medianMMT.summary)
      }
      
      ## 7.3. clear ----
      
      rm(af.posterior.medianMMT)
      gc()
      
    }
    
  }

## 0.3. plotting parameter ----

plot.height <- 10
plot.width <- 10
plot.textsize <- 20 

italyRegion.grid <- 
  tibble::tribble(
    ~name,                                   ~name.eng,                    ~code, ~row, ~col,
    "Valle d'Aosta / Vallée d'Aoste",         'Aosta Valley',               'AO',   1,    2,
    'Trentino Alto Adige / Südtirol',         'Trentino-Alto Adige',        'TN',   1,    4,
    'Piemonte',                               'Piedmont',                   'PI',   2,    2,
    'Lombardia',                              'Lombardy',                   'LO',   2,    3,
    'Veneto',                                 'Veneto',                     'VE',   2,    4,
    'Friuli-Venezia Giulia',                  'Friuli-Venezia Giulia',      'FR',   2,    5,
    'Liguria',                                'Liguria',                    'LI',   3,    3,
    'Emilia-Romagna',                         'Emilia-Romagna',             'ER',   3,    4,
    'Toscana',                                'Tuscany',                    'TO',   4,    3,
    'Marche',                                 'Marche',                     'MA',   4,    4,
    'Sardegna',                               'Sardinia',                   'SA',   5,    1,
    'Lazio',                                  'Lazio',                      'LA',   5,    3,
    'Umbria',                                 'Umbria',                     'UM',   5,    4,
    'Campania',                               'Campania',                   'CA',   6,    3,
    'Abruzzo',                                'Abruzzo',                    'AB',   6,    4,
    'Basilicata',                             'Basilicata',                 'BA',   7,    3,
    'Molise',                                 'Molise',                     'MO',   7,    4,
    'Sicilia',                                'Sicily',                     'SI',   8,    1,
    'Calabria',                               'Calabria',                   'CL',   8,    2,
    'Puglia',                                 'Apulia',                     'PU',   8,    4
  ) %>%
  data.table::as.data.table()

# 1. municipality mean temperature ----

temp.munc.mean.data <-
  data.table::copy(model.data) %>% 
  .[, .(
    tmp_lag_03 = 
      tmp_lag_03 %>% 
      mean(x = ., 
           na.rm = TRUE)
  ),
  by = Code] %>% 
  as.data.frame() %>% 
  merge(x = poly.munc, 
        y = .,
        by.x = 'PRO_COM',
        by.y = 'Code',
        all.x = TRUE) %>% 
  sf::st_as_sf()

temp.munc.mean.plot <-
  ggplot2::ggplot() +
  ggplot2::geom_sf(data = temp.munc.mean.data,
                   mapping = ggplot2::aes(fill = tmp_lag_03),
                   colour = NA) +
  ggplot2::scale_fill_gradientn(name = 'Mean Temperature (C) Lag (0-3)',
                                guide = guide_colourbar(title.position = 'top'),
                                colors = rev(heat.colors(100))) +
  my.map.theme(text = element_text(size = plot.textsize),
               legend.position = 'bottom',
               legend.title = element_text(hjust = 0.5),
               legend.key.width = unit(2, 'cm')); temp.munc.mean.plot

ggplot2::ggsave(filename = file.path(dir.finalres, 'municipality_meantemp.png'),
                plot = temp.munc.mean.plot,
                height = plot.height, 
                width = plot.width)

rm(temp.munc.mean.data,
   temp.munc.mean.plot)
gc()

# 2. area based results ----

## 2.0. set up ----

modeldata.munc <- 
  model.data %>% 
  split(x = ., by = 'municipality_id')

modeldata.rgn <- 
  model.data %>% 
  split(x = ., by = 'Region')

modeldata.ctry <- list()
modeldata.ctry[['Italy']] <- model.data

## 2.1. municipality ----

### 2.1.1. temp percentile ----

fp.munc.perc <- file.path(dir.finalres, '01_municipality', '02_temperature_percentile')
fs::dir_create(fp.munc.perc)

if(
  file.path(fp.munc.perc, c('summary_mmt.rds', 'summary_rr.rds')) %>% 
  file.exists() %>% 
  {any(!.)}
){
  
  message('Calculating municipality results at the temperature observed...')
  
  res <- 
    helper.processTimer(
      label = ' generating results...',
      expr = 
        future.apply::future_lapply(
          X = modeldata.munc,
          FUN = function(x) {
            calculate.areaResults(
              data = x,
              coefficient.posterior = cb.coef,
              spatial.level = c('municipality', 'regional', 'national')[1],
              percentiles = (0:100)/100,
              return.AF = TRUE
            )
          },
          future.packages = c('tidyverse', 'data.table', 'matrixStats', 'Matrix', 'dlnm'),
          future.globals  = c('cb.coef', 'calculate.areaResults')
        ) %>% 
        setNames(object = .,
                 nm = names(modeldata.munc))
    )
  
  format.mmt(
    results = res,
    spatial.level = c('municipality', 'regional', 'national')[1],
    dir.target = fp.munc.perc
  )
  
  format.rr(
    results = res,
    spatial.level = c('municipality', 'regional', 'national')[1],
    dir.target = fp.munc.perc
  )
  
  format.af(
    results = res,
    spatial.level = c('municipality', 'regional', 'national')[1],
    dir.target = fp.munc.perc
  )
  
  format.areaResults(
    results = res,
    spatial.level = c('municipality', 'regional', 'national')[1],
    dir.target = fp.munc.perc,
    save.from.medianMMT = FALSE
  )
  
  rm(res)
  gc()
  
}

## 2.2. region ----

### 2.2.1. temp percentile ----

fp.rgn.perc <- file.path(dir.finalres, '02_region', '02_temperature_percentile')
fs::dir_create(fp.rgn.perc)

if(
  file.path(fp.rgn.perc, c('summary_mmt.rds', 'summary_rr.rds')) %>% 
  file.exists() %>% 
  {any(!.)}
){
  
  message('Calculating region results at the temperature percentile...')
  
  res <- 
    helper.processTimer(
      label = ' generating results...',
      expr = 
        lapply(
          X = modeldata.rgn,
          FUN = function(x) {
            calculate.areaResults(
              data = x,
              coefficient.posterior = cb.coef,
              spatial.level = c('municipality', 'regional', 'national')[2],
              percentiles = (0:100)/100,
              return.AF = TRUE
            )
          }
        ) %>% 
        setNames(object = .,
                 nm = names(modeldata.rgn))
    )
  
  format.mmt(
    results = res,
    spatial.level = c('municipality', 'regional', 'national')[2],
    dir.target = fp.rgn.perc
  )
  
  format.rr(
    results = res,
    spatial.level = c('municipality', 'regional', 'national')[2],
    dir.target = fp.rgn.perc
  )
  
  rm(res)
  gc()
  
}

## 2.3. nation ----

### 2.3.1. temp percentile ----

fp.ctry.perc <- file.path(dir.finalres, '03_country', '02_temperature_percentile')
fs::dir_create(fp.ctry.perc)

if(
  file.path(fp.ctry.perc, c('summary_mmt.rds', 'summary_rr.rds')) %>% 
  file.exists() %>% 
  {any(!.)}
){
  
  message('Calculating country results at the temperature percentile...')
  
  res <- 
    helper.processTimer(
      label = ' generating results...',
      expr = 
        lapply(
          X = modeldata.ctry,
          FUN = function(x) {
            calculate.areaResults(
              data = x,
              coefficient.posterior = cb.coef,
              spatial.level = c('municipality', 'regional', 'national')[3],
              percentiles = (0:100)/100,
              return.AF = TRUE
            )
          }
        ) %>% 
        setNames(object = .,
                 nm = names(modeldata.ctry))
    )
  
  format.mmt(
    results = res,
    spatial.level = c('municipality', 'regional', 'national')[3],
    dir.target = fp.ctry.perc
  )
  
  format.rr(
    results = res,
    spatial.level = c('municipality', 'regional', 'national')[3],
    dir.target = fp.ctry.perc
  )
  
  rm(res)
  gc()
  
}


## 2.4. clear ----

rm(
  modeldata.munc,
  modeldata.rgn,
  modeldata.ctry
)
gc()

# 3. RR plots ----
## 3.1. import data ----

rr <-
  helper.objectLoadRun(
    filepath = file.path(fp.munc.perc, 'summary_rr.rds'),
    FUN = NULL,
    message.run = 'Need to run posterior for municipality at temperature percentile',
    message.load = 'Loading posterior for municipality RR at temperature percentile'
  )

rr.munc <- rr$rr.municipality
rr.rgn <- rr$rr.region
rr.ctry <- rr$rr.country

## 3.2. regional plot ----

rgn.tmp <- rr.rgn$Region %>% unique()

rr.ctry.rgn <-
  data.table::copy(rr.ctry) %>%
  .[, Region := NULL] %>% 
  .[rep(1:.N, times = length(rgn.tmp))] %>% 
  .[, Region := rep(rgn.tmp, each = nrow(rr.ctry))]

plot.rr.rgn <-
  ggplot2::ggplot(data = rr.munc, mapping = ggplot2::aes(x = tmp_lag_03_ordered)) +
  # null effect
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1),
                      colour = 'black', linetype = 'dashed') +
  # lines
  ggplot2::geom_line(mapping = ggplot2::aes(y = median, group = municipality_id, colour = 'Municipality'),
                     linewidth = 0.5, alpha = 0.5) +
  ggplot2::geom_line(data = rr.rgn,
                     mapping = ggplot2::aes(y = median, group = Region, colour = 'Region'),
                     linewidth = 0.75) +
  ggplot2::geom_line(data = rr.ctry.rgn,
                     mapping = ggplot2::aes(y = median, group = Country, colour = 'Country'),
                     linewidth = 1) +
  # axis labels
  ggplot2::scale_x_continuous(name = 'Temperature lag (0-3) percentile') +
  ggplot2::scale_y_continuous(name = 'Relative risk') + 
  ggplot2::scale_colour_manual(name = '',
                               values = c('Municipality' = 'lightskyblue2',
                                          'Region' = 'darkorange3',
                                          'Country' = 'springgreen4'),
                               breaks = c('Municipality', 'Region', 'Country'),
                               guide = ggplot2::guide_legend(override.aes = list(linewidth = 1.25))) +
  # facet by Region
  ## make sure facet_geo up to date from GitHuB
  geofacet::facet_geo(~ Region, grid = italyRegion.grid, label = 'name.eng') +
  # theme
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(legend.position = c(0.02, 0.98),
                 legend.justification = c(0, 1),
                 legend.background = element_blank(),
                 legend.key = element_blank(),
                 strip.background = element_rect(fill = 'grey80', color = NA),
                 strip.text = element_text(color = 'black', face = 'bold')) +
  ggplot2::coord_cartesian(clip = 'off')

ggplot2::ggsave(filename = file.path(dir.finalres, 'rr_percentile_region.png'),
                plot = plot.rr.rgn,
                height = plot.height,
                width = plot.width)

## 3.3. national plot ----

plot.rr.ctry.1 <- 
  ggplot2::ggplot(data = rr.munc,
                  mapping = ggplot2::aes(x = tmp_lag_03_ordered)) +
  # null effect
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1),
                      colour = 'black', linetype = 'dashed') +
  # lines
  ggplot2::geom_line(mapping = ggplot2::aes(y = median, group = municipality_id, colour = 'Municipality'),
                     linewidth = 0.5, alpha = 0.5) +
  ggplot2::geom_line(data = rr.rgn,
                     mapping = ggplot2::aes(y = median, group = Region, colour = 'Region'),
                     linewidth = 1.25) +
  # axis labels
  ggplot2::scale_x_continuous(name = 'Temperature lag (0-3)') +
  ggplot2::scale_y_continuous(name = 'Relative risk') + 
  ggplot2::scale_colour_manual(name = '',
                               values = c('Municipality' = 'lightskyblue2',
                                          'Region' = 'darkorange3',
                                          'Country' = 'springgreen4'),
                               breaks = c('Municipality', 'Region', 'Country'),
                               guide = ggplot2::guide_legend(override.aes = list(linewidth = 1.25))) +
  # theme
  ggplot2::theme_minimal(base_size = 11) +
  theme(legend.position = c(0.02, 0.98),
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key = element_blank())

plot.rr.ctry.2 <-
  ggplot2::ggplot(data = rr.munc,
                  mapping = ggplot2::aes(x = tmp_lag_03_ordered)) +
  # null effect
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1),
                      colour = 'black', linetype = 'dashed') +
  # lines
  ggplot2::geom_line(mapping = ggplot2::aes(y = median, group = municipality_id, colour = 'Municipality'),
                     linewidth = 0.5, alpha = 0.5) +
  ggplot2::geom_line(data = rr.ctry,
                     mapping = ggplot2::aes(y = median, group = Country, colour = 'Country'),
                     linewidth = 1.25) +
  # axis labels
  ggplot2::scale_x_continuous(name = 'Temperature lag (0-3)') +
  ggplot2::scale_y_continuous(name = 'Relative risk') + 
  ggplot2::scale_colour_manual(name = '',
                               values = c('Municipality' = 'lightskyblue2',
                                          'Region' = 'darkorange3',
                                          'Country' = 'springgreen4'),
                               breaks = c('Municipality', 'Region', 'Country'),
                               guide = ggplot2::guide_legend(override.aes = list(linewidth = 1.25))) +
  # theme
  ggplot2::theme_minimal(base_size = 11) +
  theme(legend.position = c(0.02, 0.98),
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key = element_blank())

plot.rr.ctry.3 <-
  ggplot2::ggplot(data = rr.munc,
                  mapping = ggplot2::aes(x = tmp_lag_03_ordered)) +
  # null effect
  ggplot2::geom_hline(mapping = ggplot2::aes(yintercept = 1),
                      colour = 'black', linetype = 'dashed') +
  # lines
  ggplot2::geom_line(mapping = ggplot2::aes(y = median, group = municipality_id, colour = 'Municipality'),
                     linewidth = 0.5, alpha = 0.5) +
  ggplot2::geom_line(data = rr.rgn,
                     mapping = ggplot2::aes(y = median, group = Region, colour = 'Region'),
                     linewidth = 1.25) +
  ggplot2::geom_line(data = rr.ctry,
                     mapping = ggplot2::aes(y = median, group = Country, colour = 'Country'),
                     linewidth = 1.25) +
  # axis labels
  ggplot2::scale_x_continuous(name = 'Temperature lag (0-3)') +
  ggplot2::scale_y_continuous(name = 'Relative risk') + 
  ggplot2::scale_colour_manual(name = '',
                               values = c('Municipality' = 'lightskyblue2',
                                          'Region' = 'darkorange3',
                                          'Country' = 'springgreen4'),
                               breaks = c('Municipality', 'Region', 'Country'),
                               guide = ggplot2::guide_legend(override.aes = list(linewidth = 1.25))) +
  # theme
  ggplot2::theme_minimal(base_size = 11) +
  theme(legend.position = c(0.02, 0.98),
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.key = element_blank())

ggplot2::ggsave(filename = file.path(dir.finalres, 'rr_percentile_country_1.png'),
                plot = plot.rr.ctry.1,
                height = plot.height,
                width = plot.width)

ggplot2::ggsave(filename = file.path(dir.finalres, 'rr_percentile_country_2.png'),
                plot = plot.rr.ctry.2,
                height = plot.height,
                width = plot.width)

ggplot2::ggsave(filename = file.path(dir.finalres, 'rr_percentile_country_3.png'),
                plot = plot.rr.ctry.3,
                height = plot.height,
                width = plot.width)

## 3.4. clear ----

rm(
  rr.munc,
  rr.rgn,
  rr.ctry,
  rgn.tmp,
  rr.ctry.rgn,
  plot.rr.rgn,
  plot.rr.ctry
)
gc()

# 4. parameters -----

## 4.1. load samples -----

model.fit <-
  helper.objectLoadRun(
    filepath = file.path(dir.modelfit, 'model_fit.rds'),
    FUN = NULL,
    message.run = 'Need to run model samples',
    message.load = 'Loading model samples'
  )

## 4.2. extract hyper parameters ----

model.hyper <-
  lapply(X = model.fit$marginals.hyperpar %>% seq_along(),
         FUN = function(i) {
           
           # 0. function arguments ----
           
           # i <- 1
           
           # 1. parameters ----
           
           name.hyper <- names(model.fit$marginals.hyperpar)[i]
           
           # 2. data ----
           
           set.seed(1234)
           
           data <- 
             INLA::inla.rmarginal(
               n = 1000, 
               marginal = model.fit$marginals.hyperpar[[name.hyper]]
               )
           
           if (startsWith(name.hyper, 'Precision')) {
             data <- 1 / sqrt(data)
             name.hyper <- name.hyper %>% sub(pattern = '^Precision', replacement = 'Standard deviation', x = .)
           }
           
           # 3. summary ----
           
           res <-
             data.table::data.table(
               parameter = name.hyper,
               var = stats::var(data),
               mean = mean(data),
               median = stats::median (data),
               lower = stats::quantile(x = data, probs = 0.025),
               upper = stats::quantile(x = data, probs = 0.975)
             )
           
           # 3. return ----
           
           return(res)
           
         }) %>%
  data.table::rbindlist()

## 4.3. save ----

data.table::fwrite(
  x = model.hyper,
  file = file.path(dir.finalres, 'sensitivity_hyperparameters.csv')
)
