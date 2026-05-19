#Plots

#Load packages
library(dplyr)
library(ggplot2)
library(patchwork)
library(sf)

#Load data
dat_65pl_bio <- readRDS("~/dat_65pl.rds")

##############################################################################
#1) Exploratory analysis

#A. Daily mean temperature Italy

temp_daily <- dat_65pl %>%
  group_by(date, year) %>%
  summarise(
    mean_temp = mean(temperature, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(year, date) %>%
  mutate(time_index = row_number())


#B. Mean temperature by municipality

temp_spatial <- dat_65pl %>%
  group_by(id_region) %>%
  summarise(
    mean_temp = mean(temperature, na.rm = TRUE)
  ) %>%
  ungroup()


#C. Daily mortality rate 

mort_daily <- data_65pl %>%
  group_by(date, year) %>%
  summarise(
    deaths_tot = sum(deaths, na.rm = TRUE),
    pop_tot    = sum(pop, na.rm = TRUE)
  ) %>%
  mutate(mort_rate = deaths_tot / pop_tot) %>%
  ungroup() %>%
  arrange(year, date) %>%
  mutate(time_index = row_number())

#D. Mortality rate by municipality
mort_spatial <- data_65pl %>%
  group_by(id_region) %>%
  summarise(
    deaths_tot  = sum(deaths, na.rm = TRUE),
    person_days = sum(pop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    daily_mort_rate = deaths_tot / person_days
  )

#########


#Shapefile

shp <- read_sf("~/Shapefile_modificato")

shp <- shp %>%
  mutate(id_region = row_number())

map_temp <- shp %>%
  left_join(temp_spatial, by = "id_region")

map_mort <-shp %>%
  left_join(mort_spatial, by = "id_region")


theme_paper <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_line(color = "grey85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.7)
  )

italy_xlim <- c(6.5, 18)
italy_ylim <- c(36, 47.5)

lon_formatter <- function(x) paste0(x, "°E")
lat_formatter <- function(x) paste0(y, "°N")



#Panel A — Daily mean temperature

year_positions <- temp_daily %>%
  group_by(year) %>%
  summarise(
    start_index = min(time_index),
    .groups = "drop"
  )

pA <- ggplot(temp_daily, aes(x = time_index, y = mean_temp)) +
  geom_line(color = "black", linewidth = 0.6) +        
  geom_smooth(method = "lm", se = FALSE,
              color = "red", linewidth = 0.6) +        
  geom_vline(xintercept = year_positions$start_index[-1],
             color = "grey70", linewidth = 0.3) +
  scale_x_continuous(
    breaks = year_positions$start_index,
    labels = as.character(year_positions$year),
    limits = range(temp_daily$time_index),
    expand = c(0, 0)
  ) +
  labs(
    title = "Daily temperature",
    y = expression("Temperature (" * degree * "C)")
  ) +
  theme_paper +
  theme(
    aspect.ratio = 1,
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.length = unit(0.25, "cm"),
    plot.margin = margin(10, 10, 10, 20),
    axis.text = element_text(size = 11),
    theme_minimal(base_size = 11)
  ) 



#Panel B -  Spatial temperature
bb <- st_bbox(map_temp) 

pB <- ggplot(map_temp) +
  geom_sf(aes(fill = mean_temp), color = NA) +
  coord_sf(
    xlim = c(bb["xmin"], bb["xmax"]),
    ylim = c(bb["ymin"], bb["ymax"]),
    expand = FALSE
  ) +
  scale_fill_viridis_c(
    name = NULL
  ) +
  guides(
    fill = guide_colorbar(
      barheight = 10,
      barwidth = 1.2
    )
  ) +
  labs(
    title = expression(bold("Temperature (" * degree * "C)")
    )) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 11),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    )
  )


#Panel C — Daily mortality rate
year_positions_mort <- mort_daily %>%
  group_by(year) %>%
  summarise(
    start_index = min(time_index),
    .groups = "drop"
  )

mort_daily <- mort_daily %>%
  mutate(mort_rate_1000 = mort_rate * 1000)


year_lines_mort <- year_positions_mort$start_index[-1]



pC <- ggplot(mort_daily, aes(x = time_index, y = mort_rate_1000)) +
  geom_line(color = "black", linewidth = 0.6) +        
  geom_smooth(method = "lm", se = FALSE,
              color = "red", linewidth = 0.6) +       
  geom_vline(xintercept = year_positions_mort$start_index[-1],
             color = "grey70", linewidth = 0.3) +
  scale_x_continuous(
    breaks = year_positions_mort$start_index,
    labels = as.character(year_positions_mort$year),
    limits = range(mort_daily$time_index),
    expand = c(0, 0)
  ) +
  labs(
    title = "Daily mortality",
    y = "Mortality rate (per 1,000)"
  ) +
  theme_paper +
  theme(
    aspect.ratio = 1,
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.ticks.length = unit(0.25, "cm"),
    plot.margin = margin(10, 10, 10, 20),
    axis.text = element_text(size = 11),
    theme_minimal(base_size = 11)
  ) 



#Panel D - Spatial mortality rate
bb <- st_bbox(map_mort) 

map_mort <- map_mort %>%
  mutate(daily_mort_rate_1000 = daily_mort_rate * 1000)


map_mort <- map_mort %>%
  mutate(
    mort_class = cut(
      daily_mort_rate_1000,
      breaks = quantile(daily_mort_rate_1000,
                        probs = seq(0, 1, length.out = 6),
                        na.rm = TRUE),
      include.lowest = TRUE
    )
  )

red_palette <- c(
  "#fee5d9",
  "#fcae91",
  "#fb6a4a",
  "#de2d26",
  "#67000d"
) 

breaks <- c(0.00, 0.0957, 0.107, 0.118, 0.133, 0.384)

labels <- paste0(
  format(round(head(breaks, -1), 2), nsmall = 2),
  "–",
  format(round(tail(breaks, -1), 2), nsmall = 2)
)

map_mort$mort_class <- cut(
  map_mort$daily_mort_rate_1000,
  breaks = breaks,
  labels = labels,
  include.lowest = TRUE,
  right = FALSE   
)


pD <- ggplot(map_mort) +
  geom_sf(aes(fill = mort_class), color = NA) +
  coord_sf(
    xlim = c(bb["xmin"], bb["xmax"]),
    ylim = c(bb["ymin"], bb["ymax"]),
    expand = FALSE
  ) +
  scale_fill_manual(
    values = c("#fbe5d6", "#f4b183", "#ed7d31", "#c00000", "#7f0000"),
    name = NULL
  ) +
  labs(
    title = "Mortality rate (per 1,000)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text = element_text(size = 11),
    legend.text = element_text(size = 11),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8)
  )


pA <- pA + theme(
  aspect.ratio = 1
)

pB <- pB + theme(
  aspect.ratio = 1.2
)

pC <- pC + theme(
  aspect.ratio = 1
)

pD <- pD + theme(
  aspect.ratio = NULL
)

plot1 <- pA | pB +
  plot_layout(widths = c(1, 0.1))

plot2 <- pC | pD +
  plot_layout(widths = c(1, 0.001))

ggsave("plot1.png", plot1, width = 10, height = 5)
ggsave("plot2.png", plot2, width = 10, height = 5)

##############################################################################
#Plot RR, ERH, AFH and exceedance probability

#RR
#Load data
RR <- readRDS("~/posterior_rr_pecentile_90_95_99.rds")

shp <- shp %>%
  mutate(id_region = row_number())

theta_cols <- grep("^theta:", names(RR), value = TRUE)
df_with_median <- RR %>%
  rowwise() %>%
  mutate(theta_median = median(c_across(all_of(theta_cols)), na.rm = TRUE)) %>%
  ungroup()
df_p90 <- df_with_median %>%
  filter(temperaturePercentile == 90)
df_p95 <- df_with_median %>%
  filter(temperaturePercentile == 95)
df_p99 <- df_with_median %>%
  filter(temperaturePercentile == 99)

theta_cols <- grep("^theta:", names(df_p90))
theta_cols <- grep("^theta:", names(df_p95))
theta_cols <- grep("^theta:", names(df_p99))

df_p90$theta_median <- apply(df_p90[, theta_cols], 1, median)
df_p95$theta_median <- apply(df_p95[, theta_cols], 1, median)
df_p99$theta_median <- apply(df_p99[, theta_cols], 1, median)

theta_cols_90 <- grep("^theta:", names(df_p90), value = TRUE)
theta_cols_95 <- grep("^theta:", names(df_p95), value = TRUE)
theta_cols_99 <- grep("^theta:", names(df_p99), value = TRUE)

mean_RR90 <- mean(as.matrix(df_p90[, theta_cols_90]), na.rm = TRUE)
mean_RR95 <- mean(as.matrix(df_p95[, theta_cols_95]), na.rm = TRUE)
mean_RR99 <- mean(as.matrix(df_p99[, theta_cols_99]), na.rm = TRUE)

RR90_post <- apply(df_p90[, theta_cols_90], 1,
                   function(x) mean(x > mean_RR90))

RR95_post <- apply(df_p95[, theta_cols_95], 1,
                   function(x) mean(x > mean_RR95))

RR99_post <- apply(df_p99[, theta_cols_99], 1,
                   function(x) mean(x > mean_RR99))
res_RR <- data.frame(
  id_region = df_p90$municipality_id,
  RR90_post = RR90_post,
  RR95_post = RR95_post,
  RR99_post = RR99_post
)


######################################
#Map with the median RR

map_rr_90 <- merge(
  shp,
  df_p90,
  by.x = "id_region",
  by.y = "municipality_id"
)

map_rr_95 <- merge(
  shp,
  df_p95,
  by.x = "id_region",
  by.y = "municipality_id"
)


map_rr_99 <- merge(
  shp,
  df_p99,
  by.x = "id_region",
  by.y = "municipality_id"
)

#df_p90
bb <- st_bbox(map_rr_90)
quantile(map_rr_90$theta_median, probs = seq(0, 1, 0.2), na.rm = TRUE)

map_rr_90$RR_cut <- cut(
  map_rr_90$theta_median,
  breaks = c(-Inf,
             1.132401,
             1.169476,
             1.193546,
             1.222531,
             Inf),
  labels = c("1.06-1.13",
             "1.13-1.17",
             "1.17-1.19",
             "1.19-1.22",
             ">1.22"),
  include.lowest = TRUE
)

library(RColorBrewer)

colors_RR<- colorRampPalette(brewer.pal(9, "Reds"))(6)[-1]
#colorRampPalette(brewer.pal(9, "Purples"))(5)[-1]
names(colors_RR) <- c("1.06-1.13","1.13-1.17","1.17-1.19","1.19-1.22", ">1.22")


map_RR <- left_join(
  shp,
  st_drop_geometry(map_rr_90),
  by = c("id_region" = "id_region")
)

bb <- st_bbox(map_RR)

p_median_90 <- ggplot(map_RR) +
  geom_sf(aes(fill = RR_cut), color = NA) +
  coord_sf(
    xlim = c(bb["xmin"], bb["xmax"]),
    ylim = c(bb["ymin"], bb["ymax"]),
    expand = FALSE
  ) +
  scale_fill_manual(
    values = colors_RR,
    drop = TRUE,
    name = NULL
  ) +
  labs(
    title = expression(bold("A. RR at 90th temperature percentile"))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.text  = element_text(size = 8),
    legend.text = element_text(size = 7),
    
    legend.position = c(0.18, 0.14), 
    legend.key.height = unit(0.3, "cm"),
    legend.key.width  = unit(0.5, "cm"),
    # legenda dentro la mappa
    legend.background = element_blank(),
    
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    )
  )


p_median_90

#this one will be put with ERH and AFH

#df_p95
bb <- st_bbox(map_rr_95)
quantile(map_rr_95$theta_median, probs = seq(0, 1, 0.2), na.rm = TRUE)

map_rr_95$RR_cut <- cut(
  map_rr_95$theta_median,
  breaks = c(-Inf,
             1.195629,
             1.246209,
             1.281446,
             1.313281,
             Inf),
  labels = c("1.10-1.19",
             "1.19-1.25",
             "1.25-1.28",
             "1.28-1.31",
             ">1.31"),
  include.lowest = TRUE
)

library(RColorBrewer)

colors_RR<- colorRampPalette(brewer.pal(9, "Reds"))(6)[-1]

names(colors_RR) <- c("1.10-1.19","1.19-1.25","1.25-1.28","1.28-1.31", ">1.31")


map_RR <- left_join(
  shp,
  st_drop_geometry(map_rr_95),
  by = c("id_region" = "id_region")
)

bb <- st_bbox(map_RR)

p_median_95 <- ggplot(map_RR) +
  geom_sf(aes(fill = RR_cut), color = NA) +
  coord_sf(
    xlim = c(bb["xmin"], bb["xmax"]),
    ylim = c(bb["ymin"], bb["ymax"]),
    expand = FALSE
  ) +
  scale_fill_manual(
    values = colors_RR,
    drop = TRUE,
    name = NULL
  ) +
  labs(
    title = expression(bold("A. RR at 95th temperature percentile"))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.text  = element_text(size = 8),
    legend.text = element_text(size = 7),
    
    legend.position = c(0.18, 0.14), 
    legend.key.height = unit(0.3, "cm"),
    legend.key.width  = unit(0.5, "cm"),
    # legenda dentro la mappa
    legend.background = element_blank(),
    
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    )
  )


p_median_95

#df_p99
bb <- st_bbox(map_rr_99)
quantile(map_rr_99$theta_median, probs = seq(0, 1, 0.2), na.rm = TRUE)

map_rr_99$RR_cut <- cut(
  map_rr_99$theta_median,
  breaks = c(-Inf,
             1.301325,
             1.387131,
             1.450311,
             1.515380,
             Inf),
  labels = c("1.13-1.30",
             "1.30-1.39",
             "1.39-1.45",
             "1.45-1.51",
             ">1.51"),
  include.lowest = TRUE
)

library(RColorBrewer)

colors_RR<- colorRampPalette(brewer.pal(9, "Reds"))(6)[-1]

names(colors_RR) <- c("1.13-1.30","1.30-1.39","1.39-1.45","1.45-1.51", ">1.51")


map_RR <- left_join(
  shp,
  st_drop_geometry(map_rr_99),
  by = c("id_region" = "id_region")
)

bb <- st_bbox(map_RR)

p_median_99 <- ggplot(map_RR) +
  geom_sf(aes(fill = RR_cut), color = NA) +
  coord_sf(
    xlim = c(bb["xmin"], bb["xmax"]),
    ylim = c(bb["ymin"], bb["ymax"]),
    expand = FALSE
  ) +
  scale_fill_manual(
    values = colors_RR,
    drop = TRUE,
    name = NULL
  ) +
  labs(
    title = expression(bold("B. RR at 99th temperature percentile"))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.text  = element_text(size = 8),
    legend.text = element_text(size = 7),
    
    legend.position = c(0.18, 0.14), 
    legend.key.height = unit(0.3, "cm"),
    legend.key.width  = unit(0.5, "cm"),
    # legenda dentro la mappa
    legend.background = element_blank(),
    
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    )
  )


p_median_99

#################################################
#Exceedance probability RR

map_data_RR <- left_join(
  shp,
  st_drop_geometry(res_RR),
  by = "id_region"
)

#RR 90th

# Breaks

breaks_prob <- c(0, 0.2, 0.8, 1)

map_data_RR$RR_90_cut <- cut(
  map_data_RR$RR90_post,
  breaks = breaks_prob,
  include.lowest = TRUE
)


labels_prob <- c("0-0.2", "0.2-0.8", "0.8-1")

levels(map_data_RR$RR_90_cut) <- labels_prob

colors_terz <- colorRampPalette(brewer.pal(9, "Greens"))(6)[c(3,5,6)]
names(colors_terz) <- labels_prob

# plot

map_data_RR$RR_post_terz <- cut(
  map_data_RR$RR90_post,
  breaks = c(0, 0.2, 0.8, 1),
  include.lowest = TRUE,
  labels = labels_prob
)


bb <- st_bbox(map_data)


p_RR90_terz <- ggplot(map_data_RR) +
  geom_sf(aes(fill = RR_post_terz), color = NA) +
  coord_sf(
    xlim = c(bb["xmin"], bb["xmax"]),
    ylim = c(bb["ymin"], bb["ymax"]),
    expand = FALSE
  ) +
  scale_fill_manual(
    values = colors_terz,
    drop = TRUE,
    name = NULL
  ) +
  labs(
    title = expression(bold("D. Pr(RR 90th percentile > mean)"))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.text  = element_text(size = 8),
    legend.text = element_text(size = 7),
    
    legend.position = c(0.14, 0.14), 
    legend.key.height = unit(0.5, "cm"),
    legend.key.width  = unit(0.5, "cm"),
    # legenda dentro la mappa
    legend.background = element_blank(),
    
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    )
  )

p_RR90_terz

##############
#RR 95th

# Breaks

breaks_prob <- c(0, 0.2, 0.8, 1)

map_data_RR$RR_95_cut <- cut(
  map_data_RR$RR95_post,
  breaks = breaks_prob,
  include.lowest = TRUE
)

labels_prob <- c("0-0.2", "0.2-0.8", "0.8-1")

levels(map_data_RR$RR_95_cut) <- labels_prob

colors_terz <- colorRampPalette(brewer.pal(9, "Greens"))(6)[c(3,5,6)]
names(colors_terz) <- labels_prob

# plot

map_data_RR$RR_post_terz <- cut(
  map_data_RR$RR95_post,
  breaks = c(0, 0.2, 0.8, 1),
  include.lowest = TRUE,
  labels = labels_prob
)


bb <- st_bbox(map_data)


p_RR95_terz <- ggplot(map_data_RR) +
  geom_sf(aes(fill = RR_post_terz), color = NA) +
  coord_sf(
    xlim = c(bb["xmin"], bb["xmax"]),
    ylim = c(bb["ymin"], bb["ymax"]),
    expand = FALSE
  ) +
  scale_fill_manual(
    values = colors_terz,
    drop = TRUE,
    name = NULL
  ) +
  labs(
    title = expression(bold("C. Pr(RR 95th percentile > mean)"))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.text  = element_text(size = 8),
    legend.text = element_text(size = 7),
    
    legend.position = c(0.14, 0.14), 
    legend.key.height = unit(0.5, "cm"),
    legend.key.width  = unit(0.5, "cm"),
    # legenda dentro la mappa
    legend.background = element_blank(),
    
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    )
  )

p_RR95_terz


##############
#RR 99th

# Breaks

breaks_prob <- c(0, 0.2, 0.8, 1)

map_data_RR$RR_99_cut <- cut(
  map_data_RR$RR99_post,
  breaks = breaks_prob,
  include.lowest = TRUE
)

labels_prob <- c("0-0.2", "0.2-0.8", "0.8-1")

levels(map_data_RR$RR_99_cut) <- labels_prob

colors_terz <- colorRampPalette(brewer.pal(9, "Greens"))(6)[c(3,5,6)]
names(colors_terz) <- labels_prob

# plot

map_data_RR$RR_post_terz <- cut(
  map_data_RR$RR99_post,
  breaks = c(0, 0.2, 0.8, 1),
  include.lowest = TRUE,
  labels = labels_prob
)


bb <- st_bbox(map_data)


p_RR99_terz <- ggplot(map_data_RR) +
  geom_sf(aes(fill = RR_post_terz), color = NA) +
  coord_sf(
    xlim = c(bb["xmin"], bb["xmax"]),
    ylim = c(bb["ymin"], bb["ymax"]),
    expand = FALSE
  ) +
  scale_fill_manual(
    values = colors_terz,
    drop = TRUE,
    name = NULL
  ) +
  labs(
    title = expression(bold("D. Pr(RR 99th percentile > mean)"))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.text  = element_text(size = 8),
    legend.text = element_text(size = 7),
    
    legend.position = c(0.14, 0.14), 
    legend.key.height = unit(0.5, "cm"),
    legend.key.width  = unit(0.5, "cm"),
    # legenda dentro la mappa
    legend.background = element_blank(),
    
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    )
  )

p_RR99_terz

##############################
#Plot AF and ERH
#Open res_ERH

#----AFH, ERH-------

shp <- shp %>%
  mutate(id_region = row_number())

quantile(res_ERH$AFH_med, probs = seq(0, 1, 0.2), na.rm = TRUE)

res_ERH$AFH_cut <- cut(
  res_ERH$AFH_med,
  breaks = c(-Inf,
             0.0206682156,
             0.0332403530,
             0.0430738604,
             0.0556892262,
             Inf),
  labels = c("0.0%-2.07%",
             "2.07%-3.32%",
             "3.32%-4.31%",
             "4.31%-5.57%",
             ">5.57%"),
  include.lowest = TRUE
)


colors_AFH <- colorRampPalette(brewer.pal(9, "Reds"))(6)[-1]
names(colors_AFH) <-  c("0.0%-2.07%", "2.07%-3.32%", "3.32%-4.31%","4.31%-5.57%", ">5.57%")

map_AF <- left_join(shp, res_ERH, by = c("id_region" = "id_region"))
#Heat


bb <- st_bbox(map_AF)
p_AF <- ggplot(map_AF) +
  geom_sf(aes(fill = AFH_cut), color = NA) +
  coord_sf(
    xlim = c(bb["xmin"], bb["xmax"]),
    ylim = c(bb["ymin"], bb["ymax"]),
    expand = FALSE
  ) +
  scale_fill_manual(
    values = colors_AFH,
    drop = TRUE,
    name = NULL
  ) +
  labs(
    title = expression(bold("B. Attributable fraction of heat (AFH)"))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.text  = element_text(size = 8),
    legend.text = element_text(size = 7),
    
    legend.position = c(0.20, 0.14), 
    legend.key.height = unit(0.3, "cm"),
    legend.key.width  = unit(0.5, "cm"),
    # legenda dentro la mappa
    legend.background = element_blank(),
    
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    )
  )

p_AF

########ERH

res_ERH$ERH_med <- res_ERH$ERH_med * 1000

quantile(res_ERH$ERH_med, probs = seq(0, 1, 0.2), na.rm = TRUE)

res_ERH$ERH_cut <- cut(
  res_ERH$ERH_med,
  breaks = c(-Inf,
             2.60508102,
             4.28911773,
             5.91536709,
             7.99616153,
             Inf),
  labels = c("0.00-2.61",
             "2.61-4.29",
             "4.29-5.92",
             "5.92-8.00",
             ">8.00"),
  include.lowest = TRUE
)


colors_ERH <- colorRampPalette(brewer.pal(9, "Reds"))(6)[-1]

names(colors_ERH) <- c("0.00-2.61","2.61-4.29","4.29-5.92","5.92-8.00", ">8.00")


map_ERH <- left_join(shp, res_ERH, by = c("id_region" = "id_region"))

bb <- st_bbox(map_ERH)

p_ERH <- ggplot(map_ERH) +
  geom_sf(aes(fill = ERH_cut), color = NA) +
  coord_sf(
    xlim = c(bb["xmin"], bb["xmax"]),
    ylim = c(bb["ymin"], bb["ymax"]),
    expand = FALSE
  ) +
  scale_fill_manual(
    values = colors_ERH,
    drop = TRUE,
    name = NULL
  ) +
  labs(
    title = expression(bold("C. ERH (per thousand population)"))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.text  = element_text(size = 8),
    legend.text = element_text(size = 7),
    
    legend.position = c(0.18, 0.14), 
    legend.key.height = unit(0.3, "cm"),
    legend.key.width  = unit(0.5, "cm"),
    # legenda dentro la mappa
    legend.background = element_blank(),
    
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    )
  )


p_ERH


#############################
#Exceedance probabilty ERH and AFH

shp <- shp %>%
  mutate(id_region = row_number())

map_data <- left_join(shp, res_ERH, by = "id_region")

# Breaks

breaks_prob <- c(0, 0.2, 0.8, 1)

map_data$ERH_cut <- cut(
  map_data$ERH_post,
  breaks = breaks_prob,
  include.lowest = TRUE
)

map_data$AFH_cut <- cut(
  map_data$AFH_post,
  breaks = breaks_prob,
  include.lowest = TRUE
)

labels_prob <- c("0-0.2", "0.2-0.8", "0.8-1")

levels(map_data$ERH_cut) <- labels_prob
levels(map_data$AFH_cut) <- labels_prob


colors_terz <- colorRampPalette(brewer.pal(9, "Greens"))(6)[c(3,5,6)]
names(colors_terz) <- labels_prob

# plot

map_data$AFH_post_terz <- cut(
  map_data$AFH_post,
  breaks = c(0, 0.2, 0.8, 1),
  include.lowest = TRUE,
  labels = labels_prob
)


bb <- st_bbox(map_data)


p_AFH_terz <- ggplot(map_data) +
  geom_sf(aes(fill = AFH_post_terz), color = NA) +
  coord_sf(
    xlim = c(bb["xmin"], bb["xmax"]),
    ylim = c(bb["ymin"], bb["ymax"]),
    expand = FALSE
  ) +
  scale_fill_manual(
    values = colors_terz,
    drop = TRUE,
    name = NULL
  ) +
  labs(
    title = expression(bold("E. Pr(AFH > mean)"))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.text  = element_text(size = 8),
    legend.text = element_text(size = 7),
    
    legend.position = c(0.14, 0.14), 
    legend.key.height = unit(0.5, "cm"),
    legend.key.width  = unit(0.5, "cm"),
    # legenda dentro la mappa
    legend.background = element_blank(),
    
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    )
  )

p_AFH_terz


bb <- st_bbox(map_data)

map_data$ERH_post_terz <- cut(
  map_data$ERH_post,
  breaks = c(0, 0.2, 0.8, 1),
  include.lowest = TRUE,
  labels = labels_prob
)

p_ERH_terz <- ggplot(map_data) +
  geom_sf(aes(fill = ERH_post_terz), color = NA) +
  coord_sf(
    xlim = c(bb["xmin"], bb["xmax"]),
    ylim = c(bb["ymin"], bb["ymax"]),
    expand = FALSE
  ) +
  scale_fill_manual(
    values = colors_terz,
    drop = TRUE,
    name = NULL
  ) +
  labs(
    title = expression(bold("F. Pr(ERH > mean)"))
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(size = 10, face = "bold"),
    axis.text  = element_text(size = 8),
    legend.text = element_text(size = 7),
    
    legend.position = c(0.14, 0.14), 
    legend.key.height = unit(0.5, "cm"),
    legend.key.width  = unit(0.5, "cm"),
    # legenda dentro la mappa
    legend.background = element_blank(),
    
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    )
  )


p_ERH_terz


###############################################################
#######Plot 2x3 RR 90th, AFH and Plot 2x2 RR 95th and RR 99th

#Plot 2x3 RR 90th, AFH and ERH
library(patchwork)

top_theme <- theme(
  plot.margin = margin(t = 2, r = 4, b = 8, l = 4)
)

bottom_theme <- theme(
  plot.margin = margin(t = 8, r = 4, b = 2, l = 4)
)

panel_2x3 <-
  ((p_median_90 + top_theme) |
     (p_AF + top_theme) |
     (p_ERH + top_theme)) /
  ((p_RR90_terz + bottom_theme) |
     (p_AFH_terz + bottom_theme) |
     (p_ERH_terz + bottom_theme))

ggsave(
  "Panel_main.png",
  panel_2x3,
  width = 12,
  height = 7,
  dpi = 600,
  bg = "white"
)


#Plot RR 95th and RR 99th
compact_theme <- theme(
  plot.margin = margin(t = 4, r = 4, b = 4, l = 4)
)

panel_2x2 <-
  ((p_median_95 + compact_theme) |
     (p_median_99 + compact_theme)) /
  ((p_RR95_terz + compact_theme) |
     (p_RR99_terz + compact_theme)) +
  plot_layout(heights = c(1, 1.05))

ggsave(
  "Panel_RR_2x2.png",
  panel_2x2,
  width = 10,
  height = 9,
  dpi = 600,
  bg = "white"
)


###############################################################################

##################
#TOTAL EFFECT MODIFICATION PLOT
###################

#Upload data from univariable, multivariable and spatial models


# =========================
# 0) LIBRARIES
# =========================
library(dplyr)
library(ggplot2)
library(viridisLite)

# =========================
# 1) UNIVARIABLE CONTINUOUS
# =========================
uni_cont <- bind_rows(
  ndvi,
  tmp_lag_03,
  beds,
  plus_85,
  health_exp,
  smoking,
  obesity
) %>%
  mutate(
    term = variable,
    model = "Univariable"
  )

# =========================================================
# 2) BASELINE ROWS
# =========================================================

new_row_u <- tibble(
  variable = "Rural Areas",
  mean = 0,
  median = 0,
  LL = 0,
  UL = 0,
  Method = "UP"
)

new_row_f <- tibble(
  variable = "Fragility = 1",
  mean = 0,
  median = 0,
  LL = 0,
  UL = 0,
  Method = "UP"
)

new_row_d <- tibble(
  variable = "Mid Population Density",
  mean = 0,
  median = 0,
  LL = 0,
  UL = 0,
  Method = "UP"
)

# =========================================================
# 3) ADD BASELINES
# =========================================================

fragility <- bind_rows(new_row_f, fragility)
urban    <- bind_rows(new_row_u, urban)
dens_cat <- bind_rows(new_row_d, dens_cat)

# =========================================================
# 4) MULTIVARIABLE + SPATIAL
# =========================================================

multi <- bind_rows(new_row_f, multi)
spat  <- bind_rows(new_row_f, `fixed-effects_SP`)

multi <- bind_rows(new_row_u, multi)
spat  <- bind_rows(new_row_u, spat)

multi <- bind_rows(new_row_d, multi)
spat  <- bind_rows(new_row_d, spat)

# =========================================================
# 5) FIX FRAGILITY NAMES
# =========================================================

multi <- multi %>%
  mutate(
    variable = gsub("Fragility Index", "Fragility", variable)
  )

spat <- spat %>%
  mutate(
    variable = gsub("Fragility Index", "Fragility", variable)
  )

# =========================================================
# 6) UNIVARIABLE DATASETS
# =========================================================

uni_fragility <- fragility %>%
  mutate(
    variable = "Fragility",
    term = c(
      "Fragility = 1",
      "Fragility = 2",
      "Fragility = 3",
      "Fragility = 4",
      "Fragility = 5"
    ),
    model = "Univariable"
  )

uni_urban <- urban %>%
  mutate(
    variable = "Urbanicity",
    term = c(
      "Rural Areas",
      "Towns/Suburbs",
      "Cities"
    ),
    model = "Univariable"
  )

uni_density <- dens_cat %>%
  mutate(
    variable = "Population Density Categories",
    term = c(
      "Mid Population Density",
      "Low Population Density",
      "High Population Density"
    ),
    model = "Univariable"
  )

# =========================================================
# 7) COMBINE UNIVARIABLE
# =========================================================

uni_all <- bind_rows(
  uni_cont,
  uni_urban,
  uni_fragility,
  uni_density
)

# =========================================================
# 8) MULTI + SPATIAL FORMAT
# =========================================================

multi_nospatial <- multi %>%
  mutate(
    variable = as.character(variable),
    term = as.character(variable),
    model = "Multivariable"
  )

multi_spatial <- spat %>%
  mutate(
    variable = as.character(variable),
    term = as.character(variable),
    model = "Spatial"
  )

# =========================================================
# 9) FINAL DATA
# =========================================================

plot_data <- bind_rows(
  uni_all,
  multi_nospatial
  #multi_spatial
)

# =========================================================
# 10) CLEAN LABELS
# =========================================================

plot_data <- plot_data %>%
  mutate(
    term = trimws(term),
    variable = trimws(variable),
    
    term = case_when(
      grepl("town", term, ignore.case = TRUE) ~ "Towns/Suburbs",
      grepl("cities", term, ignore.case = TRUE) ~ "Cities",
      grepl("rural", term, ignore.case = TRUE) ~ "Rural Areas",
      TRUE ~ term
    ),
    
    variable = case_when(
      grepl("town", variable, ignore.case = TRUE) ~ "Towns/Suburbs",
      grepl("cities", variable, ignore.case = TRUE) ~ "Cities",
      grepl("rural", variable, ignore.case = TRUE) ~ "Rural Areas",
      TRUE ~ variable
    ),
    
    term = case_when(
      term == "Fragility = 1" ~ "Fragility = 1 (Low)",
      term == "Fragility = 5" ~ "Fragility = 5 (High)",
      TRUE ~ term
    ),
    
    variable = case_when(
      variable == "Fragility = 1" ~ "Fragility = 1 (Low)",
      variable == "Fragility = 5" ~ "Fragility = 5 (High)",
      TRUE ~ variable
    )
  )

# =========================================================
# 11) ORDER OF VARIABLES
# =========================================================

order_vars <- c(
  "Green Spaces",
  "Average Temperature",
  "Population Density",
  
  "Mid Population Density",
  "Low Population Density",
  "High Population Density",
  
  "Rural Areas",
  "Towns/Suburbs",
  "Cities",
  
  "Proportion 85+",
  "Hospital Beds",
  "Health Expenditure",
  "Smoking",
  "Obesity",
  
  "Fragility = 1 (Low)",
  "Fragility = 2",
  "Fragility = 3",
  "Fragility = 4",
  "Fragility = 5 (High)"
)

plot_data$term <- factor(
  plot_data$term,
  levels = order_vars
)

# IMPORTANTE:
# tiene solo i livelli effettivamente presenti nel plot.
# Così le bande vengono calcolate sulle stesse posizioni usate da ggplot.
present_levels <- plot_data %>%
  filter(!is.na(term)) %>%
  pull(term) %>%
  as.character() %>%
  unique()

present_levels <- order_vars[order_vars %in% present_levels]

plot_data <- plot_data %>%
  mutate(
    term = factor(as.character(term), levels = present_levels)
  )

# =========================================================
# 12) MODEL ORDER
# =========================================================

plot_data$model <- factor(
  plot_data$model,
  levels = c(
    "Univariable",
    "Multivariable"
    #"Spatial"
  )
)

# =========================================================
# 13) VARIABLE GROUPS
# =========================================================

plot_data <- plot_data %>%
  mutate(
    variable_group = case_when(
      term %in% c(
        "Mid Population Density",
        "Low Population Density",
        "High Population Density"
      ) ~ "Density Categories",
      
      term %in% c(
        "Rural Areas",
        "Towns/Suburbs",
        "Cities"
      ) ~ "Urbanicity",
      
      grepl("Fragility", term) ~ "Fragility",
      
      TRUE ~ as.character(term)
    )
  )

# =========================================================
# 14) SORT DATA
# =========================================================

plot_data <- plot_data %>%
  arrange(term, model)

# =========================================================
# 15) GREY BANDS - CORRECTED
# =========================================================

make_band <- function(levels_vec, terms, band_name, pad = 0.45) {
  idx <- match(terms, levels_vec)
  idx <- idx[!is.na(idx)]
  
  if (length(idx) == 0) {
    return(tibble(
      band = character(),
      xmin = numeric(),
      xmax = numeric()
    ))
  }
  
  tibble(
    band = band_name,
    xmin = min(idx) - pad,
    xmax = max(idx) + pad
  )
}

bands <- bind_rows(
  make_band(
    present_levels,
    "Average Temperature",
    "Average Temperature"
  ),
  
  make_band(
    present_levels,
    c("Rural Areas", "Towns/Suburbs", "Cities"),
    "Urbanicity"
  ),
  
  make_band(
    present_levels,
    "Proportion 85+",
    "Proportion 85+"
  ),
  
  make_band(
    present_levels,
    c(
      "Fragility = 1 (Low)",
      "Fragility = 2",
      "Fragility = 3",
      "Fragility = 4",
      "Fragility = 5 (High)"
    ),
    "Fragility"
  )
)

# =========================================================
# 16) THEME
# =========================================================

theme_set(
  theme_bw(base_family = "Arial")
)

# =========================================================
# 17) COLORS
# =========================================================

cols <- viridisLite::cividis(
  3,
  begin = 0.2,
  end = 0.8
)

# =========================================================
# 18) SHAPES
# =========================================================

shape_values <- c(
  "Univariable" = 15,
  "Multivariable" = 16
  #"Spatial" = 17
)

# =========================================================
# 19) CHECK LEVELS
# =========================================================

levels(plot_data$term)
bands

# =========================
# LINES POSITIONS
# =========================

plot_order <- plot_data %>%
  filter(!is.na(term)) %>%
  distinct(term, variable_group) %>%
  mutate(
    term_chr = as.character(term),
    x_pos = as.numeric(term)
  ) %>%
  arrange(x_pos)

lines_pos <- plot_order %>%
  mutate(
    next_group = lead(variable_group)
  ) %>%
  filter(
    !is.na(next_group),
    variable_group != next_group
  ) %>%
  mutate(
    line_pos = x_pos + 0.5
  ) %>%
  pull(line_pos)


# =========================
# FIX TERM LEVELS USED BY THE PLOT
# =========================

displayed_levels <- order_vars[
  order_vars %in% as.character(plot_data$term)
]

displayed_levels <- displayed_levels[!is.na(displayed_levels)]

plot_data <- plot_data %>%
  filter(!is.na(term)) %>%
  mutate(
    term = factor(as.character(term), levels = displayed_levels)
  )

# =========================
# VERTICAL GREY LINES
# =========================

group_for_term <- tibble(
  term = displayed_levels
) %>%
  mutate(
    variable_group = case_when(
      term %in% c(
        "Mid Population Density",
        "Low Population Density",
        "High Population Density"
      ) ~ "Density Categories",
      
      term %in% c(
        "Rural Areas",
        "Towns/Suburbs",
        "Cities"
      ) ~ "Urbanicity",
      
      term %in% c(
        "Fragility = 1 (Low)",
        "Fragility = 2",
        "Fragility = 3",
        "Fragility = 4",
        "Fragility = 5 (High)"
      ) ~ "Fragility",
      
      TRUE ~ term
    ),
    x_pos = row_number(),
    next_group = lead(variable_group)
  )

lines_pos <- group_for_term %>%
  filter(
    !is.na(next_group),
    variable_group != next_group
  ) %>%
  mutate(
    line_pos = x_pos + 0.5
  ) %>%
  pull(line_pos)

lines_pos <- lines_pos[
  !is.na(lines_pos) &
    lines_pos > 0.5 &
    lines_pos < length(displayed_levels) + 0.5
]

# =========================
# PLOT
# =========================

p <- ggplot(
  plot_data,
  aes(
    x = term,
    y = median,
    ymin = LL,
    ymax = UL,
    shape = model,
    colour = model
  )
) +
  
  geom_hline(
    yintercept = 0,
    colour = "red",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  
  geom_vline(
    xintercept = lines_pos,
    colour = "grey80",
    linewidth = 0.5
  ) +
  
  geom_pointrange(
    position = position_dodge(width = 0.5),
    size = 0.7
  ) +
  
  scale_shape_manual(values = c(
    "Univariable" = 15,
    "Multivariable" = 16,
    "Spatial" = 17
  )) +
  
  scale_colour_manual(values = cols) +
  
  scale_x_discrete(
    limits = displayed_levels,
    drop = FALSE,
    expand = expansion(mult = c(0.03, 0.03)),
    labels = c(
      "Fragility = 2" = "2",
      "Fragility = 3" = "3",
      "Fragility = 4" = "4"
    )
  ) +
  
  labs(
    x = "",
    y = "RR at 90th temperature percentile (95% CrI)",
    shape = NULL,
    colour = NULL
  ) +
  
  theme_bw(base_size = 18) +
  
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA),
    
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 0.8
    ),
    
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    
    text = element_text(family = "Arial", size = 18),
    
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      size = 18
    ),
    
    axis.text.y = element_text(size = 18),
    axis.title.y = element_text(size = 20),
    
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    
    legend.margin = margin(t = -4),
    legend.box.margin = margin(t = -4),
    legend.spacing.x = unit(4, "mm")
  )

p

# =========================
# VERSIONE FINALE PER SALVATAGGIO
# =========================

# =========================
# AUMENTA SOLO I FONT DEGLI ASSI
# =========================

p_font <- p +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 42,
      colour = "black"
    ),
    
    axis.text.y = element_text(
      size = 42,
      colour = "black"
    ),
    
    axis.title.y = element_text(
      size = 42,
      colour = "black"
    ),
    
    legend.text = element_text(
      size = 35
    )
  )

p_font

ggsave(
  "RR_dens_cat.png",
  plot = p_font,
  width = 12,
  height = 6,
  units = "in",
  dpi = 300,
  limitsize = FALSE
)



#Put all the Fragility index components in one plot
# =========================
# 0) Libraries
# =========================
library(dplyr)
library(ggplot2)
library(patchwork)
library(viridisLite)
library(grid)

# =========================
# 1) Dataset
# =========================
#d1
library(dplyr)
library(stringr)

make_model_dataset <- function(univ_df, multi_df, spat_df, variable_name) {
  
  fix_one <- function(df, model_name) {
    
    df_clean <- df %>%
      filter(str_detect(variable, fixed(variable_name))) %>%
      mutate(
        term = str_remove(variable, fixed(variable_name)),
        term = str_remove(term, "^\\s*=\\s*"),
        term = case_when(
          term == "" ~ paste0(variable_name, " = 1 (Low)"),
          term == "1" ~ paste0(variable_name, " = 1 (Low)"),
          term == "5" ~ paste0(variable_name, " = 5 (High)"),
          TRUE ~ term
        ),
        variable = variable_name,
        model = model_name,
        variable_group = variable_name
      )
    
    reference_row <- tibble(
      variable = variable_name,
      mean = 0,
      median = 0,
      LL = 0,
      UL = 0,
      Method = "UP",
      term = paste0(variable_name, " = 1 (Low)"),
      model = model_name,
      variable_group = variable_name
    )
    
    df_clean <- df_clean %>%
      filter(term != paste0(variable_name, " = 1 (Low)"))
    
    bind_rows(reference_row, df_clean)
  }
  
  bind_rows(
    fix_one(univ_df, "Univariable"),
    fix_one(multi_df, "Multivariable"),
    fix_one(spat_df, "Spatial")
  )
}

d1 <- make_model_dataset(
  Access_services_Q,
  multi_Access_services_Q,
  spat_Access_services_Q,
  "Time To Access Services"
)

d2 <- make_model_dataset(
  dependency_index_Q,
  multi_dependency_index_Q,
  spat_dependency_index_Q,
  "Dependency Index"
)

d3 <- make_model_dataset(
  education_Q,
  multi_education_Q,
  spat_education_Q,
  "Low Education"
)

d4 <- make_model_dataset(
  industry_Q,
  multi_industry_Q,
  spat_industry_Q,
  "Firms per Capita"
)

d5 <- make_model_dataset(
  land_cons_Q,
  multi_land_cons_Q,
  spat_land_cons_Q,
  "Land Consumption"
)

d6 <- make_model_dataset(
  landslide_risk_Q,
  multi_landslide_risk_Q,
  spat_landslide_risk_Q,
  "Landslide Risk"
)

d7 <- make_model_dataset(
  low_product_Q,
  multi_low_product_Q,
  spat_low_product_Q,
  "Low Productivity Employees"
)

d8 <- make_model_dataset(
  migration_Q,
  multi_migration_Q,
  spat_migration_Q,
  "Migration Rate"
)

d9 <- make_model_dataset(
  motor_Q,
  multi_motor_Q,
  spat_motor_Q,
  "High Transport Emission Rate"
)

d10 <- make_model_dataset(
  occupation_Q,
  multi_occupation_Q,
  spat_occupation_Q,
  "Employment Rate"
)

d11 <- make_model_dataset(
  prot_areas_Q,
  multi_prot_areas_Q,
  spat_prot_areas_Q,
  "Protected Areas"
)

d12 <- make_model_dataset(
  waste_Q,
  multi_waste_Q,
  spat_waste_Q,
  "Unsorted Waste"
)


list_data <- list(d1,d2,d3,d4,d5,d6,d7,d8,d9,d10,d11,d12)

titles <- c(
  "Travel Time to Access Services",
  "Dependency Index",
  "Low Education Rate",
  "Firms per Capita",
  "Land Use",
  "Landslide Risk",
  "Low Productivity Employees",
  "Migration Rate",
  "Transport Emissions",
  "Employment Rate",
  "Protected Natural Areas (%)",
  "Unsorted Waste"
)

# =========================
# 3) Order
# =========================
ord <- order(titles)
titles <- titles[ord]
list_data <- list_data[ord]

discordant_vars <- c(
  "Employment Rate",
  "Migration Rate",
  "Firms per Capita",
  "Protected Natural Areas (%)"
)

get_pol <- function(t) {
  if (t %in% discordant_vars) return("Discordant")
  return("Concordant")
}

# =========================
# 5) Quintiles
# =========================
fixed_order <- c("Q1 (Low)", "Q2", "Q3", "Q4", "Q5 (High)")
model_order <- c("Univariable", "Multivariable", "Spatial")

fix_data <- function(df, titolo) {
  df %>%
    mutate(
      term = case_when(
        grepl("=\\s*1", term) ~ "Q1 (Low)",
        grepl("=\\s*5", term) ~ "Q5 (High)",
        term %in% c("2") ~ "Q2",
        term %in% c("3") ~ "Q3",
        term %in% c("4") ~ "Q4",
        TRUE ~ term
      ),
      term = factor(term, levels = fixed_order),
      model = factor(model, levels = model_order)
    ) %>%
    filter(term %in% fixed_order)
}

# =========================
# 6) Limits
# =========================
all_data <- bind_rows(Map(fix_data, list_data, titles))

y_min <- min(all_data$LL, na.rm = TRUE)
y_max <- max(all_data$UL, na.rm = TRUE)

# =========================
# 7) Colors
# =========================
cols <- c(
  "Univariable" = "#2B4C7E",
  "Multivariable" = "#8A8A8A",
  "Spatial" = "#D8BF5A"
)

shapes <- c(
  "Univariable" = 15,
  "Multivariable" = 16,
  "Spatial" = 17
)


# =========================
# 8) Plot function
# =========================
make_plot <- function(df, titolo) {
  
  df <- fix_data(df, titolo)
  
  ggplot(df, aes(
    x = term,
    y = median,
    ymin = LL,
    ymax = UL,
    shape = model,
    colour = model
  )) +
    
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      colour = "red",
      linewidth = 0.5
    ) +
    
    geom_vline(
      xintercept = 1:4 + 0.5,
      colour = "grey85",
      linewidth = 0.5
    ) +
    
    geom_linerange(
      position = position_dodge(0.4),
      linewidth = 0.5
    ) +
    
    geom_point(
      position = position_dodge(0.4),
      size = 3.5
    ) +
    
    scale_shape_manual(
      values = shapes,
      breaks = model_order,
      limits = model_order
    ) +
    
    scale_colour_manual(
      values = cols,
      breaks = model_order,
      limits = model_order
    ) +
    
    coord_cartesian(ylim = c(y_min, y_max)) +
    
    labs(
      x = "",
      y = NULL,
      title = paste0(
        titolo, " ",
        ifelse(get_pol(titolo) == "Concordant", "⊕", "⊖")
      ),
      colour = NULL,
      shape = NULL
    ) +
    
    theme_minimal(base_size = 16) +
    theme(
      plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
      axis.text.y = element_text(size = 14),
      panel.grid.major.x = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA)
    )
}

plots <- Map(make_plot, list_data, titles)


ncol_plot <- 4
nrow_plot <- 3

plots <- lapply(seq_along(plots), function(i) {
  
  p <- plots[[i]]
  
  row <- ceiling(i / ncol_plot)
  col <- i %% ncol_plot
  if (col == 0) col <- ncol_plot
  
  if (row != nrow_plot) {
    p <- p + theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  }
  
  if (col != 1) {
    p <- p + theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank()
    )
  }
  
  p
})

# =========================
# 11) GRID
# =========================
main_plot <- wrap_plots(
  plots,
  ncol = 4,
  nrow = 3,
  guides = "collect"
) &
  theme(
    legend.position = "bottom",
    legend.justification = "center",
    legend.text = element_text(size = 14),
    panel.spacing = unit(2, "lines")
  )


ylab <- wrap_elements(
  grid::textGrob(
    "ERH (95% Credible Intervals)",
    rot = 90,
    gp = grid::gpar(fontsize = 20, fontface = "bold")
  )
)

# =========================
# 13) Combine
# =========================
final_plot <- ylab + main_plot +
  plot_layout(widths = c(0.06, 1))


final_plot <- final_plot +
  plot_annotation(
    caption = "⊕ Positive association with Fragility Index   ⊖ Negative association with Fragility Index",
    theme = theme(
      plot.caption = element_text(
        size = 14,
        hjust = 0.5
      )
    )
  )

# =========================
# 15) Save
# =========================
ggsave(
  "ifc_components.png",
  final_plot,
  width = 16,
  height = 12,
  dpi = 300
)

final_plot