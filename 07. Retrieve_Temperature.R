##TEMPERATURE


# Clean and download temperature


#---------------------------------------------------------------------------------

# Step 1. Download temperature data from ERA5

# and create a new directory to store the output
if(!dir.exists("Output"))
  dir.create("Output/")

# load packages
library(ecmwfr)
library(doParallel)

# You need to create an account here https://cds.climate.copernicus.eu/cdsapp#!/home, 
# agree with the terms here: https://cds.climate.copernicus.eu/cdsapp/#!/terms/licence-to-use-copernicus-products,
# log in and once you are ok and logged in, click on your name on the top right next to logout
# and retrieve the information about the API key.

cds.user <- "a2d84253-ff26-4690-9d8f-691adbb7c420" # Insert your CDS user here
cds.key <- "fb088f3b-31f0-4a0b-9e49-729a9821c2e2" #"Insert_your_CDS_API_KEY_here"

# Set up the API and UID
#wf_set_key(user = cds.user, key = cds.key, service = "cds")
wf_set_key(user = cds.user, key = cds.key)
if(is.null(cds.user) | is.null(cds.key)) {
  print("You need to create an account here https://cds.climate.copernicus.eu/cdsapp#!/home, and once you are ok and logged in, click on your name on the top right next to logout and retrieve the information about the API key.")
}



# function to download the temperature

DonwloadTemperature <- function(X){
  
  request <- list(
    dataset_short_name = "reanalysis-era5-land",
    product_type   = "reanalysis",
    format = "netcdf",
    variable = "2m_temperature",
    date = X, # this is to match the ISO weeks
    time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", 
             "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", 
             "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
    # area is specified as N, W, S, E
    area = c(48, 6, 34, 20),
    target = paste0("temperature", sub(pattern = "/", replacement = "_", x=X), ".nc")
  )
  
  if(!file.exists(paste0("Output/temperature", sub(pattern = "/", replacement = "_", x=X), ".nc"))) {
    file <- wf_request(user = cds.user,
                       request = request,
                       transfer = TRUE,
                       path = "Output",
                       time_out = 3600*24,
                       verbose = TRUE)
  }
  
}

# 2014-12-28/2021-01-03
#Put from 1/01/2011 to 31/10/2024 where I have mortality data

start_date <- as.Date("2011-01-01")
end_date <- as.Date("2011-01-31")
define_dates <- seq(from = start_date, to = end_date, length.out = 60)

toloop <- paste(define_dates[-length(define_dates)], define_dates[-1], sep = "/")


# run on parallel
funpar <- function(k) DonwloadTemperature(X = toloop[k])

t_0 <- Sys.time()

# Set up parallel environment
#ncores <- 20
ncores <- detectCores() - 1
k <- 1:length(toloop)
cl_inla <- makeCluster(ncores, methods=FALSE)

# extract packages on parallel environment 
clusterEvalQ(cl_inla, {
  library(ecmwfr)
})

# extract R objects on parallel environment
clusterExport(cl_inla, c("toloop", "DonwloadTemperature", "cds.user", "cds.key"))

# run the the function in parallel
outpar <- parLapply(cl = cl_inla, k, funpar)

# close parallel environment
stopCluster(cl_inla)
t_1 <- Sys.time()
t_1 - t_0 




#####From here clean the dataset 

# Step 2. Clean the temperature files

# load packages
library(ncdf4)
library(plyr)
library(sf)
library(raster)
library(lubridate)
library(patchwork)
library(dplyr)
library(stringr)
library(data.table)
library(abind)
library(ggplot2)

# read the files
files2read <- list.files("Output/")[list.files("Output/") %>% startsWith(.,"temperature")]
temperature <- lapply(paste0("Output/", files2read), nc_open) 
extr.tmp <- lapply(temperature, function(X) ncvar_get(X, varid="t2m"))


# extract space 
lon <- lapply(temperature, function(X) ncvar_get(X,"longitude")) 
lon <- lon[[1]]
lat <- lapply(temperature, function(X) ncvar_get(X,"latitude")) 
lat <- lat[[1]]
# and time
#hour <- lapply(temperature, function(X) ncvar_get(X,"time"))
hour <- lapply(temperature, function(X) ncvar_get(X,"valid_time")) 
hour <- do.call(c, hour)
# the format is hours since 1900-01-01:
#maybe the format is seconds since 1970-01-01
#hour_tr <- as.POSIXct(hour*3600, origin="1900-01-01 00:00")
hour_tr <- as.POSIXct(hour, origin="1970-01-01 00:00")
# Set time zone (UTC)
attr(hour_tr, "tzone") <- "UTC"

# set the correct timezone for Italy
hour_tr <- format(hour_tr, format='%Y-%m-%d', tz = "Europe/Rome")


# and from this string we need to remove the dates outside the 2015-2020 ISO weeks, ie everything before 2014-12-29 and
# after 2021-01-03
#datestart <- "2014-12-29"
datestart <- "2011-01-01"
#dateend <- "2021-01-03"
dateend <- "2011-01-31"

extr.tmp <- abind(extr.tmp, along = 3)
extr.tmp[,,(hour_tr>=datestart) & (hour_tr<=dateend)] -> extr.tmp
hour_tr[(hour_tr>=datestart) & (hour_tr<=dateend)] -> hour_tr

# define the start/end points of each date
dat <- as.data.frame(table(hour_tr))

start <- numeric(nrow(dat))
stop <- numeric(nrow(dat))

start[1] <- 1
stop[1] <- dat$Freq[1]

for(i in 2:nrow(dat)){
  start[i] <- stop[i-1] + 1
  stop[i] <- start[i] + dat$Freq[i] - 1
}

dat$start <- start
dat$stop <- stop


# function to retrieve daily mean

DailyMean <- function(start, stop, date){
  
  tmp <- aaply(extr.tmp[,,start:stop], .margin = c(1,2), .fun = function(Y) mean(Y-273.15))
  tmp <- as.data.frame(tmp)
  
  colnames(tmp) <- lat
  rownames(tmp) <- lon
  
  mat2store <- expand.grid(lon, lat)
  colnames(mat2store) <- c("lon", "lat")
  mat2store <- cbind(mat2store, as.vector(as.matrix(tmp)))  
  
  mat2store <- as.data.frame(mat2store)
  colnames(mat2store)[3] <- "temperature"
  
  mat2store <- as.data.frame(mat2store)
  mat2store$date <- as.Date(date)
  
  mat2store <- mat2store[complete.cases(mat2store$temperature),]
  
  return(mat2store)
}

# run the DailyMean function across the data
t_0 <- Sys.time()
GetTemperature <- 
  apply(dat, 1, function(X){
    
    return(DailyMean(start = X[3], stop = X[4], date = X[1]))
    
  } 
  ) # approximately 1h
t_1 <- Sys.time()
t_1-t_0

##
## RUN FROM HERE TO CHECK IF FINE

GetTemperature <- do.call(rbind, GetTemperature)

# create and id by latitude and longitude
GetTemperature %>% 
  dplyr::group_by(lon, lat) %>% 
  dplyr::mutate(ID = dplyr::cur_group_id()) -> GetTemperature


# Now we need the shp in Italy.
mun <- read_sf("input/shp")

# make sure shp and temperature file are in the same projection
DT_sf <- st_as_sf(GetTemperature[, c("lon", "lat")], coords = c("lon", "lat"), crs = 4326)
DT_sf <- st_transform(DT_sf, crs = st_crs(mun))
DT_sf <- st_coordinates(DT_sf)
DT_sf <- as.data.frame(DT_sf)

GetTemperature <- cbind(GetTemperature, DT_sf)

#Save GetTemperature
saveRDS(GetTemperature, file = "Output/GetTemperature")

