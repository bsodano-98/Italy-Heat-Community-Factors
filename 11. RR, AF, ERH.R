##############################################
#Calculate RR, ERH, AF

#Libraries 

library(dlnm)
library(splines)
library(INLA)
library(dplyr)
library(ggplot2)
library(spdep)
library(lubridate)
library(reshape2)
library(RColorBrewer)
library(sf)
library(gridExtra)
library(grid)
library(patchwork)
library(viridis)

#Open datasets
data_65 <- readRDS("~/dat_65pl.rds")
model_samples <- readRDS("~/model_samples.rds")  #INLA results

#Total number of deaths
sum(data_65$deaths)

#Total population by year

pop_year <- data_65 %>%
  filter(year >= 2011 & year <= 2023) %>%
  distinct(year, id_region, .keep_all = TRUE) %>%  
  group_by(year) %>%
  summarise(pop_tot = sum(pop, na.rm = TRUE)) %>%
  arrange(year)


#Range of RR at 90th temperature percentile

lapply(model_samples, function(X){
  X$latent[!startsWith(rownames(X$latent), "Predictor"),] %>% return()
}) -> res_samples

latent_df <- as.data.frame(model_samples[[1]]$latent)
str(model_samples[[1]])

reg1 <- data2list_byReg[[1]]
reg1_df <- as.data.frame(reg1)


#======================================================================
# national / municipality-level results:
# 1) nationwide and municipality-level J shape curve
# 2) epi metrics: 
#   -- MMT (minimum mortality temperature)
#   -- MMP (the percentile of MMT among the region-specific temperature observations)
#   -- AF_x (daily attributable fraction at specific temperature x)
#   -- AFH (fraction of death attributable to heat, when x>MMT)
#   -- ERH (excess death rates due to heat)
#=======================================================================

# load inla posterior samples 
res_samples_svc <- res_samples
res_df <- as.data.frame(do.call(rbind, res_samples_svc))
rm(res_samples_svc)

#---- coef per region ----
# function to calculate the varying coefficients on basis functions of temperature

#eliminate columns
cols <- names(res_df)
cols_to_remove <- cols[
  
  
  (grepl("^municipality_id[0-9]+$", cols) &
     as.numeric(sub("^municipality_id", "", cols)) >= 7896) |
    
    
    (grepl("^municipality_cb[1-4]_id:[0-9]+$", cols) &
       as.numeric(sub(".*:", "", cols)) >= 7896)
]

length(cols_to_remove)
head(cols_to_remove)
tail(cols_to_remove)

res_df <- res_df[, !(cols %in% cols_to_remove)]


##
calc_coefX_byReg <- function(res_df, n_region) {
  coef_list <- vector("list", n_region)
  names(coef_list)<-paste0("id_region",1:n_region)
  for (basis_num in 1:4) {
    # colnames of fixed effects
    X_basis_fix <- paste0("cb", basis_num, ":1")
    
    for (i in 1:n_region) {
      # colnames of random effects
      region_random <- paste0("municipality_cb", basis_num, "_id:", i)
      # spatially varying coefficients across small areas
      coef_vec <- res_df[[X_basis_fix]] + res_df[[region_random]]#sum of fixed effect and spatially random effect
      
      col_name <- paste0("coef_X", basis_num)
      
      # put coefficients in list
      if (basis_num == 1) {
        coef_list[[i]] <- data.frame(coef_X1 = coef_vec)
      } else {
        coef_list[[i]][[col_name]] <- coef_vec
      }
    }
  }
  
  return(coef_list)
}

#calculate spatially varying coefficients
n_region <- data_65$id_region%>%unique()%>%length() ###number of small areas (municipalities)
coefX_list_byReg <- calc_coefX_byReg(res_df, n_region)



#build X basis
#natural cubic spline and rescaling basis for temperature
ref_temp <- 12
# without intercept
onebasis_temp <- onebasis(data_65$lag_t_mean, fun = "ns", 
                          knots=quantile(data_65$lag_t_mean, 
                                         c(10,75,90)/100, na.rm=T))

# change the reference temperature
match1<- c("fun",names(formals(attr(onebasis_temp, "fun"))))
match2<- names(attributes(onebasis_temp))

ind<- match(match1, match2, nomatch = 0) 

varvec <- as.numeric(data_65$lag_t_mean)
basisvar <- do.call("onebasis", c(list(x=varvec), attributes(onebasis_temp)[ind]))

cen <- ref_temp # reference temp
basiscen <- do.call("onebasis", c(list(x=cen), attributes(onebasis_temp)[ind]))

# define basis for prediction
Xpred <- scale(basisvar, center = basiscen, scale = FALSE)
## Xpred = basisvar - basiscen

rm(match1, match2, ind, varvec, basisvar, basiscen)

# and add to the data
data_65$Xpred1 <- Xpred[,1]
data_65$Xpred2 <- Xpred[,2]
data_65$Xpred3 <- Xpred[,3]
data_65$Xpred4 <- Xpred[,4]

data_65$id_region1 <- data_65$id_region
data_65$id_region2 <- data_65$id_region
data_65$id_region3 <- data_65$id_region
data_65$id_region4 <- data_65$id_region

Xpred<- Xpred%>%as.data.frame()
Xpred$temp03<- data_65$lag_t_mean
X_basis12 <- Xpred


# X basis 
set.seed(123456)
X_basis12<-X_basis12%>%arrange(temp03)
X_basis12_toSample<-X_basis12[-c(1,nrow(X_basis12)),]
# sample temperature points to improve the efficiency in RR J curve plot
X_basis12_sampled<- rbind(X_basis12[1,],
                          X_basis12_toSample[sample(nrow(X_basis12_toSample),200-2),],
                          X_basis12[nrow(X_basis12),])%>%arrange(temp03)



#----- J curve RR----
## nationwide (NW)------
X_sampled<- X_basis12_sampled[,c("b1","b2","b3","b4")]%>%as.matrix()

nationwide_coef<-data.frame(
  beta1 = res_df$`cb1:1`, beta2 = res_df$`cb2:1`,
  beta3 = res_df$`cb3:1`, beta4 = res_df$`cb4:1`)%>%as.matrix()

logRR_mat<- X_sampled %*% t(nationwide_coef)
nationwide_df<- data.frame(temp03 = X_basis12_sampled$temp03,
                           logRR_med = apply(logRR_mat, 1, median),#by row
                           logRR_LL = apply(logRR_mat, 1, function(x) quantile(x, probs = 0.025)),
                           logRR_UL = apply(logRR_mat, 1, function(x) quantile(x, probs = 0.975)))

# identify MMT within 25-90th percentile of temperature during summers
range4MMT<-quantile(data_65$lag_t_mean, probs = c(25,90)/100)
df4MMT<-nationwide_df%>%
  filter(temp03>=range4MMT[1]& temp03<=range4MMT[2])

MMT_nationwide<-df4MMT$temp03[which.min(df4MMT$logRR_med)] 
MMT_nationwide
ecdf(data_65$lag_t_mean)(MMT_nationwide)*100 

rm(df4MMT,nationwide_df)

MMT_idx <- which.min(abs(X_basis12_sampled$temp03-MMT_nationwide))
X_basis12_sampled$temp03[MMT_idx]#15.87144
logRR_mat<- X_sampled %*% t(nationwide_coef) #200*4*4*1000

logRR_MMT_nationwide<-logRR_mat[MMT_idx,]
# rescale logRR to MMT 
logRR_mat_atMMT <- sweep(logRR_mat, 2, logRR_MMT_nationwide, "-")
logRR_atMMT<-cbind(temp03= X_basis12_sampled$temp03,
                   logRR_mat_atMMT)%>%as.data.frame()
colnames(logRR_atMMT)<- c("temp03", paste0("sample",1:1000))
nationwide_df_2<- data.frame(temp03 = X_basis12_sampled$temp03,
                             logRR_med = apply(logRR_mat_atMMT, 1, median),#by row
                             logRR_LL = apply(logRR_mat_atMMT, 1, function(x) quantile(x, probs = 0.025)),
                             logRR_UL = apply(logRR_mat_atMMT, 1, function(x) quantile(x, probs = 0.975)))


## per region (spag)-----
# identify the median value of coefficients on each of the basis functions 
coefX_med_byReg_df <- do.call(rbind, lapply(coefX_list_byReg, function(df) {
  apply(df, 2, median) #by col
}))
colnames(coefX_med_byReg_df) <- paste0("X", 1:4, "_med")  
spag_df <- as.data.frame( 
  as.matrix(X_basis12_sampled[,c("b1","b2","b3","b4")])%*%t(coefX_med_byReg_df))
colnames(spag_df)<-1:n_region
spag_df$temp03 <- X_basis12_sampled$temp03

spag_df_melt <- melt(spag_df, id.vars = "temp03", 
                     variable.name = "id_region", value.name = "logRR_med")
# range of MMT for each region (25-90th percentile of temperature in each region)
range4MMT_byReg<- data_65%>%group_by(id_region)%>%
  select(id_region,lag_t_mean)%>%
  mutate(MMT_Lrange = quantile(lag_t_mean, probs = 25/100),
         MMT_Urange = quantile(lag_t_mean, probs = 90/100))%>%
  select(-lag_t_mean)%>%unique()

range4MMT_byReg$id_region<- range4MMT_byReg$id_region%>%as.factor()


spag_df4MMT<- spag_df_melt%>%left_join(range4MMT_byReg, by = "id_region")%>%
  filter(temp03>=MMT_Lrange & temp03<=MMT_Urange)
rm(range4MMT_byReg)

spag_df4MMT<- spag_df4MMT%>%group_by(id_region)%>%
  mutate(MMT=temp03[which.min(logRR_med)],
         logRR_MMT = min(logRR_med))

spag_df_melt_2<-spag_df_melt%>%
  left_join(spag_df4MMT%>%select(id_region,MMT,logRR_MMT)%>%unique(), 
            by = "id_region")%>%
  mutate(logRR_atMMT = logRR_med - logRR_MMT)



# ------ epi metric(samples)--------
##J curve samples - for metric 
#natural cubic spline and recaling basis for temperature
ref_temp <- 12
# without intercept
onebasis_temp <- onebasis(data_65$lag_t_mean, fun = "ns", 
                          knots=quantile(data_65$lag_t_mean, 
                                         c(10,75,90)/100, na.rm=T))

# change the reference temperature
match1<- c("fun",names(formals(attr(onebasis_temp, "fun"))))
match2<- names(attributes(onebasis_temp))

ind<- match(match1, match2, nomatch = 0) 

varvec <- as.numeric(data_65$lag_t_mean)
basisvar <- do.call("onebasis", c(list(x=varvec), attributes(onebasis_temp)[ind]))

cen <- ref_temp # reference temp
basiscen <- do.call("onebasis", c(list(x=cen), attributes(onebasis_temp)[ind]))

# define basis for prediction
Xpred <- scale(basisvar, center = basiscen, scale = FALSE)
## Xpred = basisvar - basiscen

rm(match1, match2, ind, varvec, basisvar, basiscen)

# and add to the data
data_65$Xpred1 <- Xpred[,1]
data_65$Xpred2 <- Xpred[,2]
data_65$Xpred3 <- Xpred[,3]
data_65$Xpred4 <- Xpred[,4]


colnames(data_65)
df <-data_65[, c("date","pop", "deaths",
                 "id_region","id.year",
                 "lag_t_mean", "Xpred1", 
                 "Xpred2", "Xpred3", "Xpred4")]

# split the dataset by id_region - 1 region for each list, to improve the effciency in deriving results                                             
split_data2list_byReg <- function(df,n_region) {
  reg_ids <- unique(df$id_region)
  reg_list <-vector("list", n_region)
  names(reg_list)<-paste0("id_region",1:n_region)
  
  for (r in reg_ids){
    df_reg <- df[df$id_region == r,]
    reg_list[[r]]<-df_reg
  }
  return(reg_list)
  
}


shp <- read_sf("~/Shapefile_modificato")

shp <- shp %>%
  mutate(id_region = row_number())


data2list_byReg<- split_data2list_byReg(df,
                                        n_region = length(unique(shp$id_region))) 

# function to calculate the logRR curve samples, the curves are scaled at original references of the basis functions
calc_logRR_byReg <- function(data2list_byReg, coefX_list_byReg) {
  n_region <- length(data2list_byReg)
  logRR_list <- vector("list",n_region)
  names(logRR_list)<- names(data2list_byReg)
  for (reg_name in names(data2list_byReg)){
    df_r <- data2list_byReg[[reg_name]]
    coef_r <- coefX_list_byReg[[reg_name]]
    basis_mat <- as.matrix(df_r[, paste0("Xpred", 1:4)])
    coef_mat <- as.matrix(coef_r[, paste0("coef_X", 1:4)])
    logRR_mat <- basis_mat %*% t(coef_mat) #n_obs* n_sample (logRR samples)
    n_sample <- ncol(logRR_mat)
    logRR_wTemp<- cbind(df_r$lag_t_mean, logRR_mat)
    colnames(logRR_wTemp)<- c("temp03", paste0("sample", 1:n_sample))
    logRR_list[[reg_name]]<- logRR_wTemp
  }
  return (logRR_list)
}


calc_logRR_byReg_optimized <- function(data2list_byReg, 
                                       coefX_list_byReg, 
                                       region_index,
                                       chunk_size = 500) {
  
  sub_data  <- data2list_byReg[region_index]
  sub_coef  <- coefX_list_byReg[region_index]
  
  n_region <- length(sub_data)
  logRR_list <- vector("list", n_region)
  names(logRR_list) <- names(sub_data)
  
  for (r in seq_along(sub_data)) {
    
    reg_name <- names(sub_data)[r]
    cat("Processing:", reg_name, "\n")
    
    df_r   <- sub_data[[r]]
    coef_r <- sub_coef[[r]]
    
    basis_mat <- as.matrix(df_r[, paste0("Xpred", 1:4)])
    coef_mat  <- as.matrix(coef_r[, paste0("coef_X", 1:4)])
    
    n_obs    <- nrow(basis_mat)
    n_sample <- nrow(coef_mat)
    
    logRR_mat <- matrix(NA_real_, n_obs, n_sample)
    
    col_index <- 1
    
    for (i in seq(1, n_sample, by = chunk_size)) {
      
      idx <- i:min(i + chunk_size - 1, n_sample)
      chunk <- basis_mat %*% t(coef_mat[idx, , drop = FALSE])
      
      logRR_mat[, col_index:(col_index + ncol(chunk) - 1)] <- chunk
      col_index <- col_index + ncol(chunk)
      
      rm(chunk)
      gc(FALSE)
    }
    
    result_mat <- cbind(temp03 = df_r$lag_t_mean, logRR_mat)
    
    colnames(result_mat) <- c(
      "temp03",
      paste0("sample", 1:n_sample)
    )
    
    logRR_list[[r]] <- result_mat
    
    rm(basis_mat, coef_mat, logRR_mat, result_mat)
    gc(FALSE)
  }
  
  return(logRR_list)
}

n_tot <- length(data2list_byReg)

# Prima metà
part1 <- calc_logRR_byReg_optimized(
  data2list_byReg,
  coefX_list_byReg,
  region_index = 1:4000
)

gc()


# Second part
part2 <- calc_logRR_byReg_optimized(
  data2list_byReg,
  coefX_list_byReg,
  region_index = 4001:n_tot
)

logRR_list_byReg <- c(part1, part2)

n <- length(data2list_byReg)

data2list_byReg  <- data2list_byReg[1:4000] #first part
data2list_byReg <- data2list_byReg[4001:length(data2list_byReg)] #second part

logRR_list_byReg <- part1 #first part
logRR_list_byReg <- part2 #second part

t_0<-Sys.time()
t_1<-Sys.time()
t_1-t_0 #10mins


# calculate MMT based on the logRR samples across regions
calc_MMT_byReg <- function(logRR_list_byReg){
  MMT_list <- vector("list", length(logRR_list_byReg))
  names(MMT_list)<- names(logRR_list_byReg)
  for(reg_name in names(logRR_list_byReg)){
    logRR_df <-logRR_list_byReg[[reg_name]]%>%as.data.frame()
    temp_vec <-logRR_df[,1]
    range4MMT <- quantile(temp_vec, probs = c(25,90)/100) # range for MMT in summer months
    logRR_samples<- logRR_df[, -1]
    n_sample <- ncol(logRR_samples)
    MMT_vec <- numeric(n_sample)
    logRR_MMTvec <- numeric(n_sample)
    logRR4MMT <-logRR_df%>%
      filter(temp03>=range4MMT[1] & temp03<=range4MMT[2])
    temp4MMT <- logRR4MMT[, 1]
    logRR4MMT_samples <- logRR4MMT[, -1]
    MMT_df <- data.frame(MMT = numeric(n_sample),
                         logRR_at_MMT = numeric(n_sample))
    
    for (s in 1:n_sample) {
      sample_s <- logRR4MMT_samples[, s]
      idx_min <- which.min(sample_s)
      MMT_df$logRR_at_MMT[s] <- sample_s[idx_min]
      MMT_df$MMT[s] <- temp4MMT[idx_min]
    }
    
    MMT_list[[reg_name]] <- MMT_df # include MMT samples for each region
  }
  
  return(MMT_list)
}
t_0 <-Sys.time()
MMT_list_byReg<-calc_MMT_byReg(logRR_list_byReg)
t_1 <-Sys.time()
t_1-t_0 


# calculate AFH, ECH, ERH metric with the following function
calc_AFH_list_byReg <- function(MMT_list_byReg, data2list_byReg, logRR_list_byReg) {
  AFH_list <- vector("list", length(MMT_list_byReg))
  names(AFH_list) <- names(MMT_list_byReg)
  
  for (reg_name in names(MMT_list_byReg)) {
    MMT_df <- MMT_list_byReg[[reg_name]] # contain MMT and logRR_at_MMT samples for each region
    df_r <- data2list_byReg[[reg_name]]
    logRR_mat <- logRR_list_byReg[[reg_name]]
    temp_vec <- logRR_mat[, "temp03"]
    logRR_samples <- logRR_mat[, -1]
    n_sample <- ncol(logRR_samples)
    res_mat_Heat <- matrix(0, nrow = n_sample, ncol = 3) # matrix to save results
    colnames(res_mat_Heat) <- c("ECH", "AFH", "ERH")
    
    valid_idx <- which(df_r$deaths !=0) # filter data with at least 1 death, which will contribute to metric calculation
    
    if (length(valid_idx) == 0) {# some regions might have no valid data, due to no death
      AFH_list[[reg_name]] <- list(res_mat_Heat = res_mat_Heat) #assign 0 for metric for these regions
      next
    }
    # filter valid data for metric calculation (and also improve efficiency)
    logRR_samples <- logRR_samples[valid_idx, , drop = FALSE]
    temp_vec <- temp_vec[valid_idx]
    deaths_x <- df_r$deaths[valid_idx]
    pop_x <- df_r$pop[valid_idx]
    
    sum_death <- sum(deaths_x) # sum of death in one region
    mean_pop <- mean(pop_x) # mean population in one region
    
    for (s in 1:n_sample) {
      logRR_x <- logRR_samples[, s]
      MMT_s <- MMT_df$MMT[s] #value of MMT sample s in one region
      
      logRR_MMT <-  MMT_df$logRR_at_MMT[s] #for sample s, logRR at MMT
      logRR_new <- logRR_x - logRR_MMT #rescale logRR
      RR_x <- exp(logRR_new)
      AF_x <- 1 - 1 / RR_x #daily AF attributable to temperature x exposure
      EC_x <- AF_x * deaths_x #daily excess death counts (also called attributable number)
      
      heat_index <- (temp_vec >= MMT_s) # identify heat
      ECH <- sum(EC_x[heat_index]) #excess death counts due to heat
      AFH <- if (sum_death != 0) ECH/sum_death else 0 #some regions might have no death cases, assign 0
      ERH <- ECH / mean_pop
      
      res_mat_Heat[s, ] <- c(ECH, AFH, ERH)
    }
    
    AFH_list[[reg_name]] <- list(res_mat_Heat = res_mat_Heat)
  }
  
  return(AFH_list)
}

t_0 <-Sys.time()
AFH_list_byReg <- calc_AFH_list_byReg(MMT_list_byReg, 
                                      data2list_byReg, 
                                      logRR_list_byReg)

t_1 <-Sys.time()
t_1-t_0 


#Create AFH_list_byReg
AFH_list_byReg <- c(AFH_list_byReg_1, AFH_list_byReg)

#-----summary res -----
# MMT, MMP
#MMT_list_byReg<- readRDS("main_res_upd/MMT_list_byReg.rds")
n_region <- 7895
#n_region <- 4000 #part 1
#n_region <- 3895 #part 2
res_MMT <- data.frame(id_region = 1:n_region,
                      MMT = NA,
                      MMP = NA)
for (i in 1:n_region) {
  MMT_r <- MMT_list_byReg[[i]]
  temp_r <- data2list_byReg[[i]]$lag_t_mean
  res_MMT$MMT[i] <- median(MMT_r$MMT)
  res_MMT$MMT_mean[i] <- mean(MMT_r$MMT)
  res_MMT$MMP[i] <-ecdf(temp_r)(res_MMT$MMT[i])*100
}
mean(res_MMT$MMT_mean)#16.54996(main)
res_MMT$MMT_cut<- cut(res_MMT$MMT, breaks = c(-Inf,12,15,18,21,24,Inf),
                      labels = c("<12","12-15","15-18","18-21","21-24",">24"))


res_MMT$MMP_cut<-cut(res_MMT$MMP,breaks = c(-Inf, 25,35,50,70,Inf),
                     labels = c("<25th", "25-35th","35-50th","50-70th",">70th"))

# saveRDS(res_MMT,"main_res_upd/res_MMT.rds")

# ECH
ECH_df <- do.call(rbind, lapply(AFH_list_byReg, function(list) list$res_mat_Heat[,"ECH"]))%>%
  as.data.frame()
ECH_CH <- colSums(ECH_df)
quantile(ECH_CH, probs = c(0.025,0.5,0.975))

ECH_draws <- sapply(AFH_list_byReg, function(x) x$res_mat_Heat[, "ECH"])
ECH_total <- rowSums(ECH_draws)

c(median = median(ECH_total),
  lower = quantile(ECH_total, 0.025),
  upper = quantile(ECH_total, 0.975))



# ERH, AFH
mean_AFH <-lapply(AFH_list_byReg, function(region_res) {
  region_res$res_mat_Heat[,"AFH"]
})%>%unlist()%>%mean()
mean_AFH 

mean_ERH <-lapply(AFH_list_byReg, function(region_res) {
  region_res$res_mat_Heat[,"ERH"]
})%>%unlist()%>%mean()
mean_ERH 


res_AFH_list <- vector("list", length(AFH_list_byReg))
names(res_AFH_list) <- names(AFH_list_byReg)
for (i_reg in seq_len(n_region)) {
  res_heat <- AFH_list_byReg[[i_reg]]$res_mat_Heat
  
  res_AFH_list[[i_reg]] <- data.frame(
    AFH_med = median(res_heat[,"AFH"]),
    ERH_med = median(res_heat[,"ERH"]),
    AFH_post = mean(res_heat[,"AFH"]>mean_AFH),
    ERH_post = mean(res_heat[,"ERH"]>mean_ERH)
  )
}

res_ERH  <- data.frame(id_region = 1:n_region, 
                       AFH_med =sapply(res_AFH_list, function(df) df$AFH_med), 
                       ERH_med = sapply(res_AFH_list, function(df) df$ERH_med),
                       AFH_post =  sapply(res_AFH_list, function(df) df$AFH_post), 
                       ERH_post = sapply(res_AFH_list, function(df) df$ERH_post))

quantile(res_ERH$AFH_med, na.rm = TRUE)

#res_ERH$id_region <- res_ERH$id_region + 4000
#res_MMT$id_region <- res_MMT$id_region + 4000

saveRDS(res_ERH, "~ERH_part1.rds")
saveRDS(res_MMT, "~MMT_part1.rds")

#Now we have part1 and part2, merge and obtain res_ERH

res_ERH <- rbind(ERH_part1, ERH_part2)
res_MMT <- rbind(MMT_part1, MMT_part2)


sum(is.na(res_ERH))#0

