
# Spatial effect-modification models for ERH, RR or AFH outcomes.
# The script can run either:
# 1. a full model including the IFC quintile indicator, or
# 2. one model for each IFC component, excluding the overall IFC indicator.



# Packages ----------------------------------------------------------------

library(dplyr)
library(sf)
library(spdep)
library(INLA)
library(ggplot2)
library(showtext)
library(readxl)
library(purrr)
library(fastDummies)
library(parallel)

# Load data ---------------------------------------------------------------

df_ERH <- readRDS("~/df_ERH.rds")
df_AFH <- readRDS("~df_AFH.rds")
df_p90 <- readRDS("~df_p90.rds")

theta_vars <- grep("theta", colnames(df_p90), value = TRUE)
df_p90 <- df_p90[, c(theta_vars, setdiff(colnames(df_p90), theta_vars))]

# Divide population density into three categories -------------------------

q25 <- quantile(df_p90$dens_media, 0.25, na.rm = TRUE)
q75 <- quantile(df_p90$dens_media, 0.75, na.rm = TRUE)

df_p90$dens_cat <- with(
  df_p90,
  ifelse(
    dens_media < q25,
    "low_density",
    ifelse(dens_media > q75, "high_density", "mid_density")
  )
)

df_p90$dens_cat <- factor(
  df_p90$dens_cat,
  levels = c("mid_density", "low_density", "high_density")
)

table(df_p90$dens_cat)

# Divide IFC into quintiles ------------------------------------------------

df_p90 <- df_p90 %>%
  mutate(ifc_5 = ntile(ifc2018, 5))

# DGURBA recoding ----------------------------------------------------------

# Original coding: 1 = cities, 2 = towns/suburbs, 3 = rural areas.
# Recoded coding: 1 = rural areas, 2 = towns/suburbs, 3 = cities.
# With remove_first_dummy = TRUE, rural areas are the reference category.
df_p90 <- df_p90 %>%
  mutate(DGURBA = case_when(
    DGURBA == 1 ~ 3,
    DGURBA == 3 ~ 1,
    TRUE ~ DGURBA
  ))

df_p90$DGURBA <- factor(df_p90$DGURBA, levels = c(1, 2, 3))

table(df_p90$DGURBA)
message("DGURBA_2 = towns/suburbs; DGURBA_3 = cities; reference = rural areas.")

if (any(is.na(df_p90$DGURBA))) {
  stop("DGURBA contains missing values after recoding.")
}

# Divide all IFC components into quintiles --------------------------------

vars <- c(
  "land_cons",
  "Access_services",
  "waste",
  "motor",
  "prot_areas",
  "landslide_risk",
  "dependency_index",
  "education",
  "occupation",
  "migration",
  "industry",
  "low_product"
)

df_p90 <- df_p90 %>%
  mutate(across(all_of(vars), ~ ntile(.x, 5), .names = "{.col}_Q"))

################################################################################
# Effect modification model
################################################################################

# Load shapefile -----------------------------------------------------------

shp <- read_sf("~/Shapefile_modificato")
nb <- spdep::poly2nb(shp)
summary(nb)

nb2INLA("map_adj", nb)
graph <- inla.read.graph(filename = "map_adj")

# Model settings -----------------------------------------------------------

hyper_iid <- list(theta = list(prior = "pc.prec", param = c(1, 0.01)))

# Choose which model to run:
# - "zero": spatial random effect only;
# - "uni": one selected variable only;
# - "multi": all variables in X_mat, without spatial random effect;
# - "spatial": all variables in X_mat, with Besag spatial random effect.
model_type <- "spatial"

# Use this only when model_type = "uni".
# Examples:
# variable_type <- "obesity"
# variable_type <- c("DGURBA_2", "DGURBA_3")
# variable_type <- c("dens_cat_low_density", "dens_cat_high_density")
variable_type <- "obesity"

# Select continuous adjustment variables.
vars_cont <- c(
  "obesity",
  "smoking",
  "tmp_lag_03",
  "ndvi",
  "prop_85plus_mean",
  "exp_prov_pc",
  "posti_letto_pc"
)

X_mat <- df_p90[, vars_cont]

# IFC or single components -------------------------------------------------

# Full IFC model.
ifc_dummies <- dummy_cols(
  df_p90,
  select_columns = "ifc_5",
  remove_first_dummy = TRUE,
  remove_selected_columns = TRUE
)

ifc_dummies <- ifc_dummies[, grepl("^ifc_5_", names(ifc_dummies))]

# To run a single IFC component instead of the full IFC model, comment the
# block above and use one of the blocks below. Do not include ifc_5 in the
# same model when using a single component.

# ifc_dummies <- dummy_cols(
#   df_p90,
#   select_columns = "low_product_Q",
#   remove_first_dummy = TRUE,
#   remove_selected_columns = TRUE
# )
#
# ifc_dummies <- ifc_dummies[, grepl("^low_product_Q_", names(ifc_dummies))]

# DGURBA -------------------------------------------------------------------

dgurba_dummies <- dummy_cols(
  df_p90,
  select_columns = "DGURBA",
  remove_first_dummy = TRUE,
  remove_selected_columns = TRUE
)

dgurba_dummies <- dgurba_dummies[, grepl("^DGURBA_", names(dgurba_dummies))]

if (!all(c("DGURBA_2", "DGURBA_3") %in% names(dgurba_dummies))) {
  stop("DGURBA dummy variables were not created as expected.")
}

# Population density -------------------------------------------------------

dens_dummies <- dummy_cols(
  df_p90,
  select_columns = "dens_cat",
  remove_first_dummy = TRUE,
  remove_selected_columns = TRUE
)

dens_dummies <- dens_dummies[, grepl("^dens_cat_", names(dens_dummies))]

# Final model matrix -------------------------------------------------------

X_mat <- cbind(
  X_mat,
  ifc_dummies,
  dgurba_dummies,
  dens_dummies
)

colnames(X_mat)

# Use this object if you want to run a uni model for density categories.
deprivation_vars <- names(X_mat)[grepl("^dens_cat_", names(X_mat))]

# Scale continuous variables only.
for (i in seq_along(vars_cont)) {
  X_mat[, vars_cont[i]] <- scale(X_mat[, vars_cont[i]])
}

# Model function -----------------------------------------------------------

par.fun <- function(k, model = "multi", variable = NULL) {
  # model = "zero", variable = NULL
  # model = "spatial", variable = NULL
  # model = "multi", variable = NULL
  # model = "uni", variable = "obesity"
  
  if (model == "uni") {
    if (is.null(variable)) {
      stop("For model = 'uni', provide variable.")
    }
    
    form <- paste0(
      "Y ~ ",
      paste(variable, collapse = " + ")
    )
  }
  
  if (model == "multi") {
    form <- paste0(
      "Y ~ ",
      paste(colnames(X_mat), collapse = " + ")
    )
    variable <- colnames(X_mat)
  }
  
  if (model == "spatial") {
    form <- paste0(
      "Y ~ ",
      paste(colnames(X_mat), collapse = " + "),
      " + ",
      'f(id_region, model = "besag", constr = TRUE,
        hyper = hyper_iid, graph = graph, scale.model = TRUE)'
    )
    variable <- colnames(X_mat)
  }
  
  if (model == "zero") {
    form <- paste0(
      "Y ~ 1 + ",
      'f(id_region, model = "besag", constr = TRUE,
        hyper = hyper_iid, graph = graph, scale.model = TRUE)'
    )
    variable <- NULL
  }
  
  dat2mod <- X_mat
  
  # Change this line if the outcome is ERH or AFH.
  # Example: dat2mod$Y <- df_ERH[[k]] * 10000
  dat2mod$Y <- df_p90[[k]]
  dat2mod$id_region <- df_p90$id_region
  
  form <- as.formula(form)
  
  mod <- inla(
    formula = form,
    data = dat2mod,
    family = "gaussian",
    verbose = FALSE,
    control.compute = list(config = TRUE),
    control.mode = list(restart = TRUE),
    num.threads = round(parallel::detectCores() * 0.8),
    control.predictor = list(link = 1)
  )
  
  samples.post <- inla.posterior.sample(n = 200, result = mod)
  sams <- inla.posterior.sample.eval(c("(Intercept)", variable), samples = samples.post)
  rownames(sams) <- c("(Intercept)", variable)
  
  # Calculate residuals without the spatial random effect.
  Z_mat <- cbind(1, dat2mod[, rownames(sams)[-1], drop = FALSE])
  fitted.no.errors <- t(sams) %*% t(Z_mat)
  residuals <- sweep(fitted.no.errors, MARGIN = 2, STATS = dat2mod$Y, FUN = "-") * (-1)
  residuals <- t(residuals)
  
  ret.list <- list(
    fixed = sams,
    residuals = residuals,
    bayesian.rsq = apply(fitted.no.errors, 1, var) /
      (apply(fitted.no.errors, 1, var) + apply(residuals, 2, var))
  )
  
  return(ret.list)
}

# Run model in parallel ----------------------------------------------------

t_0 <- Sys.time()

k <- 1:200
ncores <- 20
cl_inla <- makeCluster(ncores, methods = FALSE)

clusterEvalQ(cl_inla, {
  library(INLA)
  library(dplyr)
  library(sf)
})

clusterExport(cl_inla, c("X_mat", "df_p90", "df_ERH", "df_AFH", "hyper_iid", "graph", "par.fun"))

outpar <- parLapply(
  cl = cl_inla,
  X = k,
  fun = par.fun,
  model = model_type,
  variable = variable_type
)

# Example for a uni model with density categories:
# outpar <- parLapply(
#   cl = cl_inla,
#   X = k,
#   fun = par.fun,
#   model = "uni",
#   variable = deprivation_vars
# )

stopCluster(cl_inla)

gc()
t_1 <- Sys.time()
parallel_time <- t_1 - t_0
parallel_time

# Extract posterior samples ------------------------------------------------

fixed.effects <- lapply(outpar, function(X) X$fixed) %>%
  do.call(cbind, .)

fixed.effects <- fixed.effects[, sample(1:ncol(fixed.effects), size = 1000)]

residuals <- lapply(outpar, function(X) X$residuals) %>%
  do.call(cbind, .)

residuals <- residuals[, sample(1:ncol(residuals), size = 1000)]

bayesian.rsq <- lapply(outpar, function(X) X$bayesian.rsq) %>%
  do.call(c, .)

bayesian.rsq <- bayesian.rsq[sample(1:length(bayesian.rsq), size = 1000)]


################################################################################
# Fixed-effect summary
################################################################################

fixed.effects <- fixed.effects[-1, , drop = FALSE]

RR_UP <- data.frame(matrix(ncol = 0, nrow = nrow(fixed.effects)))
RR_UP$variable <- rownames(fixed.effects)

fixed.effects <- as.data.frame(t(fixed.effects))

RR_UP$mean <- apply(fixed.effects, 2, mean)
RR_UP$median <- apply(fixed.effects, 2, median)
RR_UP$LL <- apply(fixed.effects, 2, quantile, 0.025)
RR_UP$UL <- apply(fixed.effects, 2, quantile, 0.975)
RR_UP$Method <- "UP"

# Change variable names ----------------------------------------------------

RR_UP$variable[RR_UP$variable == "dens_cat_low_density"] <- "Low Population Density"
RR_UP$variable[RR_UP$variable == "dens_cat_high_density"] <- "High Population Density"
RR_UP$variable[RR_UP$variable == "prop_85plus_mean"] <- "Proportion 85+"
RR_UP$variable[RR_UP$variable == "DGURBA_2"] <- "Towns/suburbs"
RR_UP$variable[RR_UP$variable == "DGURBA_3"] <- "Cities"
RR_UP$variable[RR_UP$variable == "tmp_lag_03"] <- "Average Temperature"
RR_UP$variable[RR_UP$variable == "obesity"] <- "Obesity"
RR_UP$variable[RR_UP$variable == "smoking"] <- "Smoking"
RR_UP$variable[RR_UP$variable == "posti_letto_pc"] <- "Hospital Beds"
RR_UP$variable[RR_UP$variable == "exp_prov_pc"] <- "Health Expenditure"
RR_UP$variable[RR_UP$variable == "ndvi"] <- "Green Spaces"

RR_UP$variable[RR_UP$variable == "ifc_5_2"] <- "Fragility Index = 2"
RR_UP$variable[RR_UP$variable == "ifc_5_3"] <- "Fragility Index = 3"
RR_UP$variable[RR_UP$variable == "ifc_5_4"] <- "Fragility Index = 4"
RR_UP$variable[RR_UP$variable == "ifc_5_5"] <- "Fragility Index = 5"

RR_UP$variable[RR_UP$variable == "land_cons_Q_2"] <- "Land Consumption = 2"
RR_UP$variable[RR_UP$variable == "land_cons_Q_3"] <- "Land Consumption = 3"
RR_UP$variable[RR_UP$variable == "land_cons_Q_4"] <- "Land Consumption = 4"
RR_UP$variable[RR_UP$variable == "land_cons_Q_5"] <- "Land Consumption = 5"
RR_UP$variable[RR_UP$variable == "Access_services_Q_2"] <- "Time To Access Services = 2"
RR_UP$variable[RR_UP$variable == "Access_services_Q_3"] <- "Time To Access Services = 3"
RR_UP$variable[RR_UP$variable == "Access_services_Q_4"] <- "Time To Access Services = 4"
RR_UP$variable[RR_UP$variable == "Access_services_Q_5"] <- "Time To Access Services = 5"
RR_UP$variable[RR_UP$variable == "waste_Q_2"] <- "Unsorted Waste = 2"
RR_UP$variable[RR_UP$variable == "waste_Q_3"] <- "Unsorted Waste = 3"
RR_UP$variable[RR_UP$variable == "waste_Q_4"] <- "Unsorted Waste = 4"
RR_UP$variable[RR_UP$variable == "waste_Q_5"] <- "Unsorted Waste = 5"
RR_UP$variable[RR_UP$variable == "motor_Q_2"] <- "High Transport Emission Rate = 2"
RR_UP$variable[RR_UP$variable == "motor_Q_3"] <- "High Transport Emission Rate = 3"
RR_UP$variable[RR_UP$variable == "motor_Q_4"] <- "High Transport Emission Rate = 4"
RR_UP$variable[RR_UP$variable == "motor_Q_5"] <- "High Transport Emission Rate = 5"
RR_UP$variable[RR_UP$variable == "prot_areas_Q_2"] <- "Protected Areas = 2"
RR_UP$variable[RR_UP$variable == "prot_areas_Q_3"] <- "Protected Areas = 3"
RR_UP$variable[RR_UP$variable == "prot_areas_Q_4"] <- "Protected Areas = 4"
RR_UP$variable[RR_UP$variable == "prot_areas_Q_5"] <- "Protected Areas = 5"
RR_UP$variable[RR_UP$variable == "landslide_risk_Q_2"] <- "Landslide Risk = 2"
RR_UP$variable[RR_UP$variable == "landslide_risk_Q_3"] <- "Landslide Risk = 3"
RR_UP$variable[RR_UP$variable == "landslide_risk_Q_4"] <- "Landslide Risk = 4"
RR_UP$variable[RR_UP$variable == "landslide_risk_Q_5"] <- "Landslide Risk = 5"
RR_UP$variable[RR_UP$variable == "dependency_index_Q_2"] <- "Dependency Index = 2"
RR_UP$variable[RR_UP$variable == "dependency_index_Q_3"] <- "Dependency Index = 3"
RR_UP$variable[RR_UP$variable == "dependency_index_Q_4"] <- "Dependency Index = 4"
RR_UP$variable[RR_UP$variable == "dependency_index_Q_5"] <- "Dependency Index = 5"
RR_UP$variable[RR_UP$variable == "education_Q_2"] <- "Low Education = 2"
RR_UP$variable[RR_UP$variable == "education_Q_3"] <- "Low Education = 3"
RR_UP$variable[RR_UP$variable == "education_Q_4"] <- "Low Education = 4"
RR_UP$variable[RR_UP$variable == "education_Q_5"] <- "Low Education = 5"
RR_UP$variable[RR_UP$variable == "occupation_Q_2"] <- "Employment Rate = 2"
RR_UP$variable[RR_UP$variable == "occupation_Q_3"] <- "Employment Rate = 3"
RR_UP$variable[RR_UP$variable == "occupation_Q_4"] <- "Employment Rate = 4"
RR_UP$variable[RR_UP$variable == "occupation_Q_5"] <- "Employment Rate = 5"
RR_UP$variable[RR_UP$variable == "migration_Q_2"] <- "Migration Rate = 2"
RR_UP$variable[RR_UP$variable == "migration_Q_3"] <- "Migration Rate = 3"
RR_UP$variable[RR_UP$variable == "migration_Q_4"] <- "Migration Rate = 4"
RR_UP$variable[RR_UP$variable == "migration_Q_5"] <- "Migration Rate = 5"
RR_UP$variable[RR_UP$variable == "industry_Q_2"] <- "Firms per Capita = 2"
RR_UP$variable[RR_UP$variable == "industry_Q_3"] <- "Firms per Capita = 3"
RR_UP$variable[RR_UP$variable == "industry_Q_4"] <- "Firms per Capita = 4"
RR_UP$variable[RR_UP$variable == "industry_Q_5"] <- "Firms per Capita = 5"
RR_UP$variable[RR_UP$variable == "low_product_Q_2"] <- "Low Productivity Employees = 2"
RR_UP$variable[RR_UP$variable == "low_product_Q_3"] <- "Low Productivity Employees = 3"
RR_UP$variable[RR_UP$variable == "low_product_Q_4"] <- "Low Productivity Employees = 4"
RR_UP$variable[RR_UP$variable == "low_product_Q_5"] <- "Low Productivity Employees = 5"

RR_UP$variable <- factor(RR_UP$variable, levels = unique(RR_UP$variable))
colnames(RR_UP) <- c("variable", "mean", "median", "LL", "UL", "Method")

saveRDS(RR_UP, "~/fixed_effects.rds")