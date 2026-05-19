
### Perform KNN to obtain temperature data

# Fast Nearest Neighbor Search, used to find the closest points in a dataset
library(FNN)


GetTemperature %>% head()

mun %>% st_centroid() %>% st_coordinates()      # st_centroid computes the centroid of each municipality
# st_coordinates extracts the coordinates of those centroids

FNN::get.knnx(data = GetTemperature[, c("X", "Y")], 
              query = mun %>% st_centroid() %>% st_coordinates(),  # This function is used to find, for each centroid (query points),
              k = 1) -> check                                      # the k nearest points in the GetTemperature dataset containing spatial coordinates (X, Y).
# Here k = 1, so it returns the single nearest neighbor for each centroid


# inspect what this function does and add comments

## link ID with mun                       # datlin: creates a dataframe linking the municipality ID (mun$PRO_COM_T)
datlin <- data.frame(                     # with the corresponding temperature station ID (GetTemperature$ID[check$nn.index]),
  mun_id = mun$PRO_COM_T,                 # using the nearest neighbor indices stored in check$nn.index.
  ID = GetTemperature$ID[check$nn.index]  # In practice, this associates each municipality with the closest temperature measurement point
)

## link ID with mun

expand.grid(                                                                             # expand.grid(): creates a dataframe dat_full containing all possible combinations of
  date = seq(from = as.Date("2013-01-01"), to = as.Date("2013-12-31"), by = "days"), 
  mun =  mun$PRO_COM_T
) -> dat_full                                                                            # date (from January 1st to December 31st, 2013) and municipality (mun$PRO_COM_T).
# This results in one row per date and municipality, covering the full period



dat_full <- left_join(dat_full, datlin, by = c("mun"="mun_id"))   # left_join(): merges dat_full with datlin using mun (municipality)
# and mun_id. This adds the corresponding station ID to each municipality-date pair

dat_full <- left_join(dat_full,                             # left_join(): merges dat_full with GetTemperature using date and ID.
                      # The resulting dataframe contains temperature data for each municipality and date
                      GetTemperature, 
                      by = c(
                        "date" = "date",
                        "ID"="ID"))
head(dat_full)
summary(dat_full)

# Map for a specific day temperature
library(sf)
mun %>% 
  left_join(., dat_full %>% 
              filter(date == "2024-08-13"), 
            by = c("PRO_COM_T" = "mun")) %>% 
  ggplot() +
  geom_sf(aes(fill=temperature), col = NA) +
  scale_fill_viridis_c() + 
  theme_bw()

#Do this for every year and save temperature_2011,...,temperature_2024

temperature_2013 <- dat_full


# Save the temperature dataset
saveRDS(dat_full, file = file = "Output/temperature_2013.rds")

#At the end obtain: 
# 2011 temperature_2011
# 2012 temperature_2012
# 2013 temperature_2013
# 2014 temperature_2014
# 2015 temperature_2015
# 2016 temperature_2016
# 2017 temperature_2017
# 2018 temperature_2018
# 2019 temperature_2019
# 2020 temperature_2020
# 2021 temperature_2021
# 2022 temperature_2022
# 2023 temperature_2023
