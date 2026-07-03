# 1. import data ----

data.import <- 
  file.path(dir.data, 'dat_65pl_bio.rds') %>%
  readRDS(file = .) %>% 
  data.table::as.data.table()

# 2. generic ----

## 2.1. spatial ----

generic.spatial <-
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

## 2.2. temporal ----

date.min <- data.import$date %>% min()
date.max <- data.import$date %>% max()
date.seq <- seq(from = date.min, to = date.max, by = 'day')

generic.temporal <-
  data.table::data.table(date = date.seq) %>% 
  .[, `:=` (
    year = date %>% lubridate::year(),
    month = date %>% lubridate::month(),
    # Sunday = 1
    dow = date %>% lubridate::wday(x = ., week_start = 7),   
    doy = date %>% lubridate::yday() 
  )] %>% 
  # _id variable for inla
  .[, `:=` (
    year_id  = year - min(year) + 1L,
    month_id = month,
    dow_id = factor(dow, levels = 1:7),
    doy_id = doy 
  )]

## 2.3. spatiotemporal ----

generic.spatiotemporal <-
  data.table::CJ(Code = poly.munc$PRO_COM_T %>% as.numeric(),
                 date = date.seq) %>% 
  merge(x = .,
        y = generic.spatial,
        by = 'Code',
        all.x = TRUE) %>% 
  merge(x = .,
        y = generic.temporal,
        by = 'date',
        all.x = TRUE) %>% 
  .[, `:=`(
    municipality_year_id = (year_id - 1L) * max(municipality_id) + municipality_id,
    municipality_doy_id  = (doy_id  - 1L) * max(municipality_id) + municipality_id
  )]

# 3. health-exposure data ----

data.import.sort <-
  data.table::copy(data.import) %>% 
  # selection
  .[, .(
    # spatial
    Code, COMUNE, Region,
    # temporal
    date, holiday,
    # observations
    deaths, pop, 
    # exposure
    temperature, lag_t_mean, 
    # confounder
    rh_mean, lag_rh_mean,
    # biodiversity
    IFC_2018, Obesity, Smoking, ndvi, Mean_Alt
  )] %>% 
  # refomarmatting 
  .[, `:=` (
    # spatial
    ## need numeric for linking
    ## NOTE:: Code == PRO_COM_T (in poly.munc)
    Code = Code %>% as.numeric(),
    # temporal
    holiday = holiday %>% factor(x = ., levels = 0:1, labels = 0:1),
    # observations
    population = pop,
    # exposure
    tmp_lag_03 = lag_t_mean,
    # confounder
    rh_lag_03 = lag_rh_mean,
    # biodiversity
    ifc2018 = IFC_2018 %>% factor(x = ., labels = 1:10, levels = 1:10),
    obesity = Obesity %>% as.numeric(),
    smoking = Smoking %>% as.numeric(),
    ndvi = ndvi %>% as.numeric(),
    meanAlt = Mean_Alt %>% as.numeric()
  )] %>% 
  # _id vars for inla
  .[, `:=` (
    # temporal
    holiday_id = holiday,
    # biodiversity
    ifc2018_id = ifc2018,
    obesity_id = obesity %>% scale() %>% as.numeric(),
    smoking_id = smoking %>% scale() %>% as.numeric(),
    ndvi_id = ndvi %>% scale() %>% as.numeric(),
    meanAlt_id = meanAlt %>% scale() %>% as.numeric()
  )] %>% 
  # remove old 
  .[, `:=` (
    pop = NULL,
    rh_mean = NULL,
    IFC_2018 = NULL,
    Obesity = NULL,
    Smoking = NULL,
    Mean_Alt = NULL
  )] %>% 
  # add spatial information
  merge(x = .,
        y = generic.spatiotemporal,
        by = c('Code', 'COMUNE', 'date'),
        all.x = TRUE) %>% 
  data.table::as.data.table() %>% 
  .[order(municipality_id, date)]

# 4. save ----

fs::dir_create(dir.datawrangling) # defined in pipeline
data.table::fwrite(x = data.import.sort,
                   file = fp.datawrangling # defined in pipeline
                   )
# 5. clear ----

rm(data.import,
   generic.spatial,
   date.min,
   date.max,
   date.seq,
   generic.temporal,
   generic.spatiotemporal,
   data.import.sort,
   dir.datawrangling)
gc()
