
#Fix shapefile
#The shapefile needs to be fixed to match the 2023 number of municipalities.

shp <- read_sf("~/Shapefile_modificato/shp.shp")
dat_65pl <- dat_65pl[, !names(dat_65pl) %in% c("id_region", "id_region1", "id_region2", "id_region3", "id_region4")]

shp$id_region <- 1:nrow(shp)

shp <- data.frame(shp)

shp_subset <- shp %>%
  select(id_region, PRO_COM_T, COMUNE)

#  left join
dat_65pl <- dat_65pl %>%
  left_join(shp_subset, by = c("Code" = "PRO_COM_T"))

dat_65pl$id_region1 <- dat_65pl$id_region
dat_65pl$id_region2 <- dat_65pl$id_region
dat_65pl$id_region3 <- dat_65pl$id_region
dat_65pl$id_region4 <- dat_65pl$id_region

trapani <- dat_65pl[dat_65pl$COMUNE == "Trapani",]

sub_dat <- dat_65pl[, c("Code", "COMUNE", "id_region")]

sub_dat_unique <- unique(sub_dat)

shp_subset <- shp_subset %>%
  select(PRO_COM_T, COMUNE, id_region)

colnames(shp_subset)[1] <- "Code"

identical(shp_subset, sub_dat_unique)

differences <- sub_dat_unique$Code != shp_subset$Code




scilla <- subset(dat_65pl, COMUNE == "Scilla")

shp[6576, ]


#Check again
