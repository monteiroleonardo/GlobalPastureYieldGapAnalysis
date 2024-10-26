# _______________________________________________________________
# Rscript 36_Climate_bin_Ramankutty_Occ_Sig.R
# _______________________________________________________________

# This script established climate bins based on equal area in a 10x10 climate space 
# defined by growing degree days and annual precipitation. 
# In this second binning exercise (see Rscript 35 for the first binning exercise), 
# the Ramankutty data is filtered to include only 
# significant pasture occupied by ruminants

# RSCRIPT OUTLINE

# CREATE DATA FRAME CONTAINING CLIMATE AND PASTURE AREA DATA
# 1. DIVIDE PASTURE AREA INTO N SWATHS ACROSS GDD RANGE
# 2. DIVIDE EACH SWATH INTO BINS BASED ON N RANGES OF PRECIPATION (TAP)
# 3  CREATE A DATAFRAME WITH BIN IDs, GDD INDEX, AND TAP INDEX BASED ON BIN BOUNDARIES
# CONVERT TAPi, GDDi, AND BINID DATA FRAMES TO RASTERS AND SAVE AS GEOTIFF FILES
# CREATE A MAP OF CLIMATE BINS WITH BIVARIATE COLOR MATRIX

# Ramankutty data available at http://www.earthstat.org/data-download/
# Described in the publication, Ramankutty et al. (2008), 
# "Farming the planet: 1. Geographic distribution of global 
# agricultural lands in the year 2000", 
# Global Biogeochemical Cycles, Vol. 22, GB1003, 
# doi:10.1029/2007GB002952.


# Assign directory ----
rm(list = ls())
setwd("c:/users/leonardo.monteiro/Documents/")
pkgs = c('rgdal', 'raster', 'maptools', 'RColorBrewer',
         'fields', 'ggplot2', 'sp', 'rworldmap', 'dplyr', 'data.table', 'tidyr', 'reshape2')
lapply(pkgs, require, character.only = T)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                         INPUT FILES 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#==========================
# CLIMATE FILES (2)
#==========================
r_GDD0 <- raster(paste0(getwd(),'/FAPESP_pasture/Resampled_files/GDD0_resample.tif')); names(r_GDD0) <- "GDD0" # growing degree-days
r_TAP <- raster(paste0(getwd(),'/FAPESP_pasture/Resampled_files/TAP_resample.tif')); names(r_TAP) <- "TAP" # precipitation

#==========================
# FRACTION PASTURE FILES (4)
#==========================
r_fpasture_total = raster(paste0(getwd(), '/FAPESP_pasture/Resampled_files/fpasture_resample.tif')) # total pasture frac Ramankutty et al. (2008)
cellStats(r_fpasture_total*area(r_fpasture_total),sum)

r_fpasture_occ = raster(paste0(getwd(), '/FAPESP_pasture/Data_filtered/fpasture_nonzero_resample.tif'))
cellStats(r_fpasture_occ*area(r_fpasture_occ),sum) # same area, as should be

r_fpasture_signif = raster(paste0(getwd(), '/FAPESP_pasture/Data_filtered/fpasture_signif_resample.tif')) # total pasture frac Ramankutty signif (>5% bottom area)
cellStats(r_fpasture_signif*area(r_fpasture_signif),sum)

r_fpasture_occ_signif = raster(paste0(getwd(), '/FAPESP_pasture/Data_filtered/fpasture_occ_signif_resample.tif')) # signif and occ pasture frac Ramankutty
cellStats(r_fpasture_occ_signif*area(r_fpasture_occ_signif),sum)

#==========================
# RUMINANTS FILES (2)
#==========================
r_LGcattle = raster(paste0(getwd(), '/FAPESP_pasture/Resampled_files/LGcattle_resample.tif'))
r_LGshoats = raster(paste0(getwd(), '/FAPESP_pasture/Resampled_files/LGshoats_resample.tif'))

r_LGruminants <- r_LGcattle + 0.2*r_LGshoats
names(r_LGruminants)<-"LGruminants_resample"

r_LGruminants_occ = r_LGruminants # create a raster of ruminants in occupied lands
r_LGruminants_occ[r_LGruminants_occ==0] <- NA
cellStats(r_LGruminants_occ*area(r_LGruminants_occ), sum) # total ruminants in the world

#==========================
# RUMINANTS FILES CORRECTED BY OCCUPIED PASTURE FRACTION
#==========================
r_LGruminants_occ_fpast <- r_LGruminants_occ/r_fpasture_occ
names(r_LGruminants_occ_fpast) <- "Ruminant_density_occ_corrected_pasture"
r_LGruminants_occ_fpast
cellStats(r_LGruminants_occ_fpast*area(r_fpasture_occ)*r_fpasture_occ, sum)

#==========================
# RUMINANTS FILES CORRECTED BY OCC and SIGNIF PASTURE FRACTION (bottom 5% removed)
#==========================
r_LGruminants_occ_signif_fpast <- r_LGruminants_occ
r_LGruminants_occ_signif_fpast <- r_LGruminants_occ_signif_fpast/r_fpasture_occ_signif
names(r_LGruminants_occ_signif_fpast) <- "Ruminant_density_occ_signif_fpast_corrected"; r_LGruminants_occ_signif_fpast
cellStats(r_LGruminants_occ_signif_fpast*area(r_fpasture_occ_signif)*r_fpasture_occ_signif, sum)

#==========================
# PASTURE PRODUCTIVITY - general (all pasture) - TOTAL KCAL ACCORDING TO HERRERO ET AL. (2013)
#==========================
r_totmeat_MMkcal_resample <- raster(paste0(getwd(),'/FAPESP_pasture/Resampled_files/totmeat_MMkcal_resample.tif'))
r_totmilk_MMkcal_resample <- raster(paste0(getwd(),'/FAPESP_pasture/Resampled_files/totmilk_MMkcal_resample.tif'))

r_tot_MMkcal_food_resample <- r_totmeat_MMkcal_resample + r_totmilk_MMkcal_resample
names(r_tot_MMkcal_food_resample)<-"tot_MMkcal_food_resample"; r_tot_MMkcal_food_resample
cellStats(r_tot_MMkcal_food_resample*area(r_tot_MMkcal_food_resample),sum) 

#==========================
# PASTURE PRODUCTIVITY - occupied areas
#==========================
r_tot_MMkcal_food_occ <- raster(paste0(getwd(), '/FAPESP_pasture/Resampled_files/tot_MMkcal_food_resample.tif'))
names(r_tot_MMkcal_food_occ)<-"tot_MMkcal_food_occ_resample"
r_tot_MMkcal_food_occ[r_tot_MMkcal_food_occ==0] <- NA; r_tot_MMkcal_food_occ
cellStats(r_tot_MMkcal_food_occ*area(r_tot_MMkcal_food_occ), sum) # total kcal in livestock according to Herrero (like above)

#==========================
# PASTURE PRODUCTIVITY - occupied areas and adj by pasture fraction (nonzero)
#==========================
r_tot_MMkcal_food_occ <- raster(paste0(getwd(), '/FAPESP_pasture/Resampled_files/tot_MMkcal_food_resample.tif'))
r_tot_MMkcal_food_occ[r_tot_MMkcal_food_occ==0] <- NA; r_tot_MMkcal_food_occ

r_tot_MMkcal_food_occ_fpast <- r_tot_MMkcal_food_occ/r_fpasture_occ
names(r_tot_MMkcal_food_occ_fpast) <- "tot_MMkcal_food_occ_corrected_pasture"; r_tot_MMkcal_food_occ_fpast
cellStats(r_tot_MMkcal_food_occ_fpast*area(r_fpasture_occ)*r_fpasture_occ, sum) 
# total food production where is considered pasture by Ramankutty(considering all pasture, not only where is signif)

#==========================
# PASTURE PRODUCTIVITY - occupied areas and adj by pasture fraction (nonzero) and signif (remove bottom 5% of total area)
#==========================
r_tot_MMkcal_food_occ_signif_fpast <- raster(paste0(getwd(), '/FAPESP_pasture/Data_filtered/tot_MMkcal_food_occ_signif_fpast.tif'))
names(r_tot_MMkcal_food_occ_signif_fpast)<-"tot_MMkcal_food_occ_signif_fpast"
cellStats(r_tot_MMkcal_food_occ_signif_fpast*area(r_fpasture_occ_signif)*r_fpasture_occ_signif, sum)

#==========================
# FILTERING SKINS FOR FILLING TABLE - USING LGP CLASSES
#==========================
r_lgp = raster(paste0(getwd(), '/FAPESP_pasture/shapefiles_manuscript/LGP.tif'))

# 1 = hyper-arid (0 days)
# 2 and 3 = arid (1 to 59 days)
# 4 and 5 = dry semi-arid (60 to 119 days)
# 6 and 7 = moist semi-arid (120 to 179 days)
# 8, 9 and 10 = sub humid (180 to 269 days)
# 11 to 15 = humid (270 to 365 days)
# 16 - per-humid (more than 365 days)

#==========================
# FILTERING SKINS FOR FILLING TABLE - USING PSI CUMMULATIVE CLASSES
#==========================
r_psi = raster(paste0(getwd(), '/FAPESP_pasture/shapefiles_manuscript/PSI.tif'))
r_psi[r_psi<0]<- NA

# -99 = no data
# 0 = Very low (0 - 10%)
# 1 = Low (10 - 20%)
# 2 = Medium low (20 - 35%)
# 3 = Medium (35 - 50%)
# 4 = Medium High (50 - 65%)
# 5 = High (65 - 80%)
# 6 = Very High (> 80%)

# LGP CLASSES ----
# 1 = hyper arid
# 2-3 = arid
# 4-5 = dry semi-arid
# 6-7 = moist semi-arid
# 8-10 = subhumid 
# 11-15 = humid


r_lgp_filt = r_lgp
r_lgp_filt[r_lgp_filt >9] <- NA
r_lgp_filt_fpasture = r_fpasture_occ_signif
r_lgp_filt_fpasture[is.na(r_lgp_filt)] <- NA
cellStats(r_lgp_filt_fpasture*area(r_lgp_filt_fpasture), sum)/10^6 
# pasture area into the LGP filtering scheme (million sq km)
r_lgp_total_food_filt = r_tot_MMkcal_food_occ_signif_fpast
r_lgp_total_food_filt[is.na(r_lgp_filt_fpasture)] <- NA
cellStats(r_lgp_total_food_filt*area(r_lgp_filt_fpasture)*r_lgp_filt_fpasture, sum)
# total food production (kcal sq km yr)

# PSI CLASSES (unique classes) ----
r_psi_filt = r_psi
r_psi_filt[r_psi_filt != 6] <- NA
r_psi_filt_fpasture = r_fpasture_occ_signif
r_psi_filt_fpasture[is.na(r_psi_filt)] <- NA
cellStats(r_psi_filt_fpasture*area(r_psi_filt_fpasture), sum)/10^6
# pasture area into PSI  filtering scheme (million sq km)
r_psi_filt_total_food = r_tot_MMkcal_food_occ_signif_fpast
r_psi_filt_total_food[is.na(r_psi_filt)] <- NA
cellStats(r_psi_filt_total_food*area(r_psi_filt_fpasture)*r_psi_filt_fpasture, sum)
# total food production (kcal per sq km per yr)

#=============================================================
# HERE BEGINING THE PROCESS TO GENERATE THE RASTERS FOR BINNING
#=============================================================

# Remove subset of significant pasture raster where total ruminants are not present (NA)

#-----------------------------------
# here in filters, the options are:
#-----------------------------------
# all  = all pastureland (occ and signif)
# lgp and psi classes
#=========================================
r_fpasture_occ_signif = raster('c:/users/leonardo.monteiro/Documents/FAPESP_pasture/shapefiles_manuscript/total_fpasture_occ_signif.tif')
filter = 'psi_veryhigh'

if(filter == 'total_pasture'){
  r_fpast = r_fpasture_occ_signif
  }else{
    if(filter == 'humid'){
      r_filt = r_lgp; r_filt[r_filt < 11 | r_filt > 15]<- NA
      r_fpast = r_fpasture_occ_signif
      r_fpast[is.na(r_filt)]<- NA
      }else{
        if(filter == 'subhumid'){
          r_filt = r_lgp; r_filt[r_filt < 8 | r_filt > 10]<- NA
          r_fpast = r_fpasture_occ_signif
          r_fpast[is.na(r_filt)]<- NA
          }else{
            if(filter == 'moistsemi'){
              r_filt = r_lgp; r_filt[r_filt < 6 | r_filt > 7]<- NA
              r_fpast = r_fpasture_occ_signif
              r_fpast[is.na(r_filt)]<- NA
            }else{
              if(filter == 'drysemi'){
                r_filt = r_lgp; r_filt[r_filt < 4 | r_filt > 5]<- NA
                r_fpast = r_fpasture_occ_signif
                r_fpast[is.na(r_filt)]<- NA
              }else{
                if(filter == 'arid'){
                  r_filt = r_lgp; r_filt[r_filt < 2 | r_filt > 3]<- NA
                  r_fpast = r_fpasture_occ_signif
                  r_fpast[is.na(r_filt)]<- NA
                }else{
                  if(filter == 'hyperarid'){
                    r_filt = r_lgp; r_filt[r_filt != 1]<- NA
                    r_fpast = r_fpasture_occ_signif
                    r_fpast[is.na(r_filt)]<- NA
                  }else{
if(filter == 'psi_verylow'){
  r_filt = r_psi; r_filt[r_filt != 0]<- NA
  r_fpast = r_fpasture_occ_signif
  r_fpast[is.na(r_filt)]<- NA
}else{
  if(filter == 'psi_low'){
    r_filt = r_psi; r_filt[r_filt != 1]<- NA
    r_fpast = r_fpasture_occ_signif
    r_fpast[is.na(r_filt)]<- NA
  }else{
    if(filter == 'psi_midlow'){
      r_filt = r_psi; r_filt[r_filt != 2]<- NA
      r_fpast = r_fpasture_occ_signif
      r_fpast[is.na(r_filt)]<- NA
    }else{
      if(filter == 'psi_mid'){
        r_filt = r_psi; r_filt[r_filt != 3]<- NA
        r_fpast = r_fpasture_occ_signif
        r_fpast[is.na(r_filt)]<- NA
      }else{
        if(filter == 'psi_midhigh'){
          r_filt = r_psi; r_filt[r_filt != 4]<- NA
          r_fpast = r_fpasture_occ_signif
          r_fpast[is.na(r_filt)]<- NA
        }else{
          if(filter == 'psi_high'){
            r_filt = r_psi; r_filt[r_filt != 5]<- NA
            r_fpast = r_fpasture_occ_signif
            r_fpast[is.na(r_filt)]<- NA}
          else{
            if(filter == 'psi_veryhigh'){
              r_filt = r_psi; r_filt[r_filt != 6]<- NA
              r_fpast = r_fpasture_occ_signif
              r_fpast[is.na(r_filt)]<- NA
            }}}}}}}}}}}}}}

r_fpasture_occ_signif <- r_fpast
r_fpasture_occ_signif[is.na(r_LGruminants_occ_signif_fpast)] <- NA
names(r_fpasture_occ_signif) <- "f_pasture_occ_sig"
cellStats(area(r_fpasture_occ_signif)*r_fpasture_occ_signif,sum)
# [1] 15,207,080 sq kn of occupied significant pasture. This matches the estimate
# made in "Rscript 7 Total ruminants.R"

# Save as GeoTIFF file
#writeRaster(r_fpasture_occ_sig, paste0(getwd(),
#                                         "FAPESP_pasture",
#                                         "Data_filtered", 
#                                         "fpasture_occ_signif_resample.tif", 
#                                         sep = "/"), 
#            format = "GTiff",
#            overwrite = TRUE)

# Create a raster containing total pasture area
r_area_Pasture_occ_sig <- area(r_fpasture_occ_signif)*r_fpasture_occ_signif
r_area_Pasture_occ_sig[is.na(r_area_Pasture_occ_sig)] <- 0 # set missing values to zero
names(r_area_Pasture_occ_sig) <- "P_area_occ_sig"
cellStats(r_area_Pasture_occ_sig,sum)
# This is the same calculation shown in previous cellStat sum
# [1] 15,207,080 sq km occupied signficant pasture

# Combine the climate and pasture area data in a raster stack
st <- stack(r_GDD0, r_TAP, r_area_Pasture_occ_sig) #create raster stack
df_climate_occ_sig <- as.data.frame(st, xy = TRUE) # create data frame with geo coordinates
df_climate_occ_sig$GDD0[is.na(df_climate_occ_sig$GDD0)] <- 0 # set missing GDD vals to zero
df_climate_occ_sig$TAP[is.na(df_climate_occ_sig$TAP)] <- 0 # set missing TPP vals to zero
df_climate_occ_sig$TAP[is.nan(df_climate_occ_sig$TAP)] <- 0 # set missing TPP vals to zero

# Sort the data frame in ascending order of GDD0
df_ordered <- df_climate_occ_sig[order(df_climate_occ_sig$GDD0),]
df_ordered$cumulArea <- cumsum(df_ordered$P_area_occ_sig)
head(df_ordered)

# 1. DIVIDE PASTURE AREA INTO N SWATHS ACROSS GDD RANGE ====

GDDcutoff <- 1
N <- 10

# Must have equal area representation across  swaths. So, goal is 
# to have a swath area equal to total area divided by the number of swaths:
P_swath_area <- max(df_ordered$cumulArea)/N

# Threshold level of cumulative P.area for each GDD column
# is (i-1) * the desired area size of each swath for the ith column
# Desired area size is a function of total area in pasture divided by N
# where N is the number divisions on each axis. (Typicy 10 for 100 bns)
# 
threshold <- length(N+1)
GDDedge_index <- length(N+1)
GDDedge_Value <- length(N+1)
for (c in 1:(N+1)){
  # The GDD cutoff is used only on the last (highest GDD range) swath.
  #  if(c==N+1) {Gcutoff <- GDDcutoff} else{Gcutoff <- 1}
  v1 <- df_ordered$cumulArea[df_ordered$cumulArea>=P_swath_area*GDDcutoff*(c-1)]
  threshold[c] <- min(v1)
  # The min value in v1 is upper edge of the current pasture swath
  # Find the index value corresponding to the threshold(c) cumulative area value
  # The R function "match" finds the FIRST value equal to to threshold(c)
  GDDedge_index[c] <- match(threshold[c], df_ordered$cumulArea)
  # Extract the value of GDD corresponding to the upper value for this swath
  GDDedge_Value[c] <- df_ordered$GDD0[GDDedge_index[c]]
  # Make sure that GDD has a numeric value. 
  # GDDedge_Value[is.na(GDDedge_Value)] <- 0
  print(paste(GDDedge_Value[c], "at index ", GDDedge_index[c]))
#  print(paste("Cutoff value for",c-1,"th level is",Gcutoff))
}

# 2. DIVIDE EACH GDD0 SWATH INTO BINS BASED ON N RANGES OF PRECIPATION (TAP) ====
# As before, each bin must contain equal number amount of pasture area
df_GDD_swath <- list()
df_GDD_swath_sorted <- list()
Area_bin <- length(N)
area_cumul_binN <- list()
threshold_TAP <- matrix(nrow = N+1,  ncol = N)
Areamod <- vector(length = 100)
TAPedge_index <- matrix(nrow = N+1,  ncol = N)
TAPedge_value <- matrix(nrow = N+1,  ncol = N)

# ____ Set TAP max cutoff ====
# This parameter sets the perecent of area in the highest extremes of precip
# that are excluded from the binning calculation
TAPcutoff <- 0.995

v2 <- list()
for (n in 1:N) {
  df_GDD_swath[[n]] <-subset(df_ordered[df_ordered$GDD0 >= GDDedge_Value[n] & df_ordered$GDD0 < GDDedge_Value[n+1],1:5])
  df_GDD_swath_sorted[[n]] <- df_GDD_swath[[n]][order(df_GDD_swath[[n]]$TAP),]
  df_GDD_swath_sorted[[n]]$cumulArea <- cumsum(df_GDD_swath_sorted[[n]]$P_area_occ_sig)
  Area_bin[n] <- max(df_GDD_swath_sorted[[n]]$cumulArea)/N
  for (m in (1:N+1)) {
    # For highest level of TAP, remove fraction of pasture land from extreme values of TAP
    #    if(m == N+1) {cutoff = TAPcutoff} else{cutoff = 1}
    v2[[m]] <- df_GDD_swath_sorted[[n]]$cumulArea[df_GDD_swath_sorted[[n]]$cumulArea >= Area_bin[n]*TAPcutoff*(m-1)]
    threshold_TAP[m,n] <- min(v2[[m]])
    TAPedge_index[m,n] <- match(threshold_TAP[m,n], df_GDD_swath_sorted[[n]]$cumulArea)
    TAPedge_value[m,n] <- df_GDD_swath_sorted[[n]]$TAP[TAPedge_index[m,n]]
  }
}

TAPedge_value[is.na(TAPedge_value)] <- 0

# Assign limiting values of TAP and GDD to climate bins
BinSize <- N^2
BinNum <- length(BinSize)
GDDmin <- length(BinSize)
GDDmax <- length(BinSize)
TAPmin <- length(BinSize)
TAPmax <- length(BinSize)
for (mTAP in 1:N){
  for (mGDD in 1:N) {
    BinNum[mTAP + N*(mGDD-1)] = mTAP + N*(mGDD-1)
    GDDmin[mTAP + N*(mGDD-1)] <- GDDedge_Value[mGDD]
    GDDmax[mTAP + N*(mGDD-1)] <- GDDedge_Value[mGDD + 1]
    TAPmin[mTAP + N*(mGDD-1)] <- TAPedge_value[mTAP,mGDD]
    TAPmax[mTAP + N*(mGDD-1)] <- TAPedge_value[mTAP+1,mGDD]
  }
}

# 3  CREATE A DATAFRAME WITH BIN IDs, GDD INDEX, AND TAP INDEX BASED ON BIN BOUNDARIES ====
df_bin_occ_sig <- data.frame(BinNum,GDDmin,GDDmax,TAPmin, TAPmax)
write.csv(df_bin_occ_sig, paste0(getwd(),'/FAPESP_pasture/tables_manuscript/',
                                filter, '_bin_table_occ_sig.csv')) 
                            

# 4. PLOT 5 MIN X 5 MIN GRID PIXELS IN CLIMATE SPACE (TAP X GDD0) ====

# Assign bin IDs to subsets of data frame meeting bin definitions
for (K in 1:N^2) {
  df_climate_occ_sig$BinID[
    df_climate_occ_sig$GDD0 >= GDDmin[K]
    & df_climate_occ_sig$GDD0 <= GDDmax[K]
    & df_climate_occ_sig$TAP >= TAPmin[K] 
    & df_climate_occ_sig$TAP <= TAPmax[K]] <- K
print (K)
  }
df_climate_occ_sig$BinID[df_climate_occ_sig$GDD0 == 0 & df_climate_occ_sig$TAP == 0] <- NA

# Assign GDD index to subsets of data frame meeting GDD index definition
vGDD <- as.vector(GDDedge_Value)
df_climate_occ_sig$GDDi <- as.vector(findInterval(df_climate_occ_sig$GDD0,vGDD))
df_climate_occ_sig$GDDi[df_climate_occ_sig$GDD0 == 0 & df_climate_occ_sig$TAP == 0] <- NA
df_climate_occ_sig$GDDi[df_climate_occ_sig$GDDi > 10] <- NA # The vector has 11 values. Get rid of 11th
df_climate_occ_sig$GDDi[df_climate_occ_sig$P_area_occ_sig==0 | is.na(df_climate_occ_sig$P_area_occ_sig)] <- NA

# Create TAP index
df_climate_occ_sig$TAPi <- df_climate_occ_sig$BinID - N*(df_climate_occ_sig$GDDi - 1)
# df_climate_occ_sig$TAPi[df_climate_occ_sig$P_area_occ_sig==0 | is.na(df_climate_occ_sig$P_area_occ_sig)] <- NA

# Need to make separate rasters of climate binID, GDD and TAP index
# In the following two pieces of code, a eliminate  of the data frame columns except xy coordinates 
# and the variable to be rastered. Not sure if this is necessary.

# Create a BinID data frame ----
df_GDDi_occ_sig <-df_climate_occ_sig[,c('x', 'y', 'GDDi')]
df_TAPi_occ_sig <-df_climate_occ_sig[,c('x', 'y', 'TAPi')]
df_BinID_occ_sig <-df_climate_occ_sig[,c('x', 'y', 'BinID')]

r_GDDi = rasterFromXYZ(df_GDDi_occ_sig)
r_TAPi = rasterFromXYZ(df_TAPi_occ_sig)
r_BinID = rasterFromXYZ(df_BinID_occ_sig)

# saving respective rasters ----
writeRaster(r_GDDi, paste0(getwd(),'/FAPESP_pasture/binsfiles_manuscript/',filter,'_GDDi.tif'),overwrite =TRUE)
writeRaster(r_TAPi, paste0(getwd(),'/FAPESP_pasture/binsfiles_manuscript/', filter, '_TAPi.tif'),overwrite =TRUE)
writeRaster(r_BinID, paste0(getwd(),'/FAPESP_pasture/binsfiles_manuscript/', filter, '_BinID.tif'),overwrite =TRUE)

#=============================================================
# HERE BEGINING THE PROCESS TO CALCULATE PASTURE YIELD GAP
#=============================================================

# SET PARAMETERS FOR HIGH AND LOW CUTOFFS ====
Alow <- 0.0
Ahigh <-  0.95
LUmin <- 0

r_ruminants <- raster(paste0(getwd(),'/FAPESP_pasture/Data_filtered/LGruminants_occ_signif_fpast.tif'))
r_MMkcal = r_tot_MMkcal_food_occ_signif_fpast;names(r_MMkcal) <- "MMkcal_per_sq_km_per_y"

r_area <- r_fpast*area(r_fpast)
AreaTotal <- cellStats(r_area,sum); AreaTotal
names(r_area) <- "area"
r_total_MMkcal <- r_area*r_MMkcal
MMkcalTot <- cellStats(r_total_MMkcal,sum); MMkcalTot
MMkcalAreaMean <- MMkcalTot/AreaTotal; MMkcalAreaMean
RumTot <- cellStats(r_ruminants*area(r_ruminants),sum); RumTot
r_rum_99 <- r_ruminants
r_rum_99[r_rum_99 > quantile(r_rum_99,0.99)] <- NA

# CREATE DATAFRAME CONTAINING BIN, AREA, PRODUCTIVITY DATA AND GEOCOORDINATES =====
# Combine raster data for ID, kcal, area
st <- stack(r_BinID, r_MMkcal,r_ruminants,r_area)
df <- as.data.frame(st, xy = TRUE) # Keep geocoordinates
head(df)
colnames(df)[3] = 'BinID_Pasture_occ_sig'
MMkcalTot <- sum(df$MMkcal_per_sq_km_per_y*df$area);MMkcalTot
AreaTotal<- sum(df$area); AreaTotal

# Eliminate pixels with less than 1 animal per square kilometer
df$LGruminants_occ_signif_fpast[df$LGruminants_occ_signif_fpast < LUmin] <- NA
df <- na.omit(df); head(df)

MMkcalTot <- sum(df$MMkcal_per_sq_km_per_y*df$area);MMkcalTot
AreaTotal<- sum(df$area);AreaTotal
MMkcalAreaMean <- MMkcalTot/AreaTotal; MMkcalAreaMean
df_ordered <- df[order(df$BinID_Pasture_occ_sig,df$MMkcal_per_sq_km_per_y),]
MMkcalTot <- sum(df_ordered$MMkcal_per_sq_km_per_y*df_ordered$area); MMkcalTot
MMkcalAreaMean <- MMkcalTot/AreaTotal; MMkcalAreaMean

# Create separate data frames for each bin with calculated area percentiles
# Also create modified area percentile based on exclusion of top 5% in each bin
N = 100; df_kcal<- list(); df_kcal_bins <- list(); df_kcal_bins_95 <- list()
binVal <- list(); binVal95 <- list(); binID95 <- list(); binApctle <- list()
head(df)

for (n in 1:N) {

  df_kcal_bins[[n]] <- df_ordered[df_ordered$BinID_Pasture_occ_sig == n,]
  df_kcal_bins[[n]]$cumularea <- cumsum(df_kcal_bins[[n]]$area)
  df_kcal_bins[[n]]$Apctle <- df_kcal_bins[[n]]$cumularea/max(df_kcal_bins[[n]]$cumularea)
  df_kcal_bins[[n]]$Apctle[df_kcal_bins[[n]]$Apctle>Ahigh]<-NA
  df_kcal_bins[[n]]$Apctle[df_kcal_bins[[n]]$Apctle<Alow]<-NA
  df_kcal_bins[[n]]<-na.omit(df_kcal_bins[[n]])
  df_kcal_bins[[n]]$cumularea <- cumsum(df_kcal_bins[[n]]$area)
  df_kcal_bins[[n]]$ApctleMod <-df_kcal_bins[[n]]$cumularea/max(df_kcal_bins[[n]]$cumularea)
  pctle <- c(seq(0.01,1,by=0.01))
  binValIndex <- findInterval(pctle,df_kcal_bins[[n]]$ApctleMod)
  binVal[[n]] <- df_kcal_bins[[n]]$MMkcal_per_sq_km_per_y[binValIndex]
  if(length(binVal[[n]] <100)){
    binVal[[n]] = c(rep(0,times = 100-length(binVal[[n]])), binVal[[n]])
    }
}


df_bin_kcal_pctle <- data.frame(matrix(unlist(binVal), nrow = N, ncol = length(pctle), byrow = TRUE))
names(df_bin_kcal_pctle) <- as.character(pctle)
write.csv(df_bin_kcal_pctle,  
          paste0(getwd(),'/FAPESP_pasture/tables_manuscript/', filter, '_kcal_pctle_matrix.csv'))
                          
df_bin_kcal_pctle$BinID <- as.character(c(seq(1,100,by=1)))
df_bin_kcal_pctle$x <- paste("00",as.character(df_bin_kcal_pctle$BinID),sep='');df_bin_kcal_pctle$x

char <- 3
# The following code reads three characters back from the right end of the string
df_bin_kcal_pctle$BinCode <- substr(df_bin_kcal_pctle$x, nchar(df_bin_kcal_pctle$x)-(char-1), nchar(df_bin_kcal_pctle$x))

#Create a precipitation code called TAPcode that tracks bin precipitation levels
char <- 1
df_bin_kcal_pctle$TAP <- paste("00",substr(df_bin_kcal_pctle$BinCode, nchar(df_bin_kcal_pctle$BinCode)-(char-1), nchar(df_bin_kcal_pctle$BinCode)),sep = "")
df_bin_kcal_pctle$TAP[df_bin_kcal_pctle$TAP=="000"] <- "010"
df_bin_kcal_pctle$TAP

df_bin_kcal_pctle$GDD[df_bin_kcal_pctle$BinCode >="001" & df_bin_kcal_pctle$BinCode <= "010"] <- "010"
df_bin_kcal_pctle$GDD[df_bin_kcal_pctle$BinCode >="011" & df_bin_kcal_pctle$BinCode <= "020"] <- "020"
df_bin_kcal_pctle$GDD[df_bin_kcal_pctle$BinCode >="021" & df_bin_kcal_pctle$BinCode <= "030"] <- "030"
df_bin_kcal_pctle$GDD[df_bin_kcal_pctle$BinCode >="031" & df_bin_kcal_pctle$BinCode <= "040"] <- "040"
df_bin_kcal_pctle$GDD[df_bin_kcal_pctle$BinCode >="041" & df_bin_kcal_pctle$BinCode <= "050"] <- "050"
df_bin_kcal_pctle$GDD[df_bin_kcal_pctle$BinCode >="051" & df_bin_kcal_pctle$BinCode <= "060"] <- "060"
df_bin_kcal_pctle$GDD[df_bin_kcal_pctle$BinCode >="061" & df_bin_kcal_pctle$BinCode <= "070"] <- "070"
df_bin_kcal_pctle$GDD[df_bin_kcal_pctle$BinCode >="071" & df_bin_kcal_pctle$BinCode <= "080"] <- "080"
df_bin_kcal_pctle$GDD[df_bin_kcal_pctle$BinCode >="081" & df_bin_kcal_pctle$BinCode <= "090"] <- "090"
df_bin_kcal_pctle$GDD[df_bin_kcal_pctle$BinCode >="091" & df_bin_kcal_pctle$BinCode <= "100"] <- "100"
df_bin_kcal_pctle$GDD

df_bin_kcal_pctle <- df_bin_kcal_pctle %>% select(-x)
write.csv(df_bin_kcal_pctle, 
          paste0(getwd(),'/FAPESP_pasture/tables_manuscript/', filter, '_kcal_pctle_matrix.csv'))

# Combine the list of df_kcal_bins dataframes into one long form data frame.
# This will be used to calculate yield gaps for each data point and replaces the
# Excel based method that starts with the 100 x 100 matrices generated previously.
df_kcal_bins_combined <- do.call("rbind",df_kcal_bins); head(df_kcal_bins_combined)
df_kcal_bins_combined <- df_kcal_bins_combined %>% rename(BinID = BinID_Pasture_occ_sig); head(df_kcal_bins_combined)

MMkcalTot <- sum(df_kcal_bins_combined$MMkcal_per_sq_km_per_y*df_kcal_bins_combined$area); MMkcalTot
AreaTotal <- sum(df_kcal_bins_combined$area); AreaTotal
MMkcalAreaMean <- MMkcalTot/AreaTotal; MMkcalAreaMean

write.csv(df_kcal_bins_combined, 
          paste0(getwd(),'/FAPESP_pasture/tables_manuscript/', filter, '_kcal_bins_combined.csv'))

# Create a long form dataframe and CSV file with BinCode, BinID, TAP and GDD index, percentile and MMkcal per sqkm
gathercols <- as.character(pctle)
keycol <- "percentile"
valcol <- "MMkcal_per_sqkm"
df_bin_kcal_pctle_long <- gather_(df_bin_kcal_pctle, keycol, valcol, gathercols)
head(df_bin_kcal_pctle_long)

# Rename MMkcal_per_sqkm to MMkcalPctle
df_bin_kcal_pctle_long <- df_bin_kcal_pctle_long %>% rename(MMkcalPctle = MMkcal_per_sqkm)
head(df_bin_kcal_pctle_long)
df_bin_kcal_pctle_long$MMkcalPctle_rel = NA

for(k in 1:100){
  A1 = df_bin_kcal_pctle_long[which(df_bin_kcal_pctle_long$BinID == k),]  
  A1$MMkcalPctle_rel = 100*(A1$MMkcalPctle/max(A1$MMkcalPctle))
  df_bin_kcal_pctle_long[as.numeric(rownames(A1)),ncol(df_bin_kcal_pctle_long)] <- A1$MMkcalPctle_rel
}
write.csv(df_bin_kcal_pctle_long, 
          paste0(getwd(),'/FAPESP_pasture/tables_manuscript/', filter, '_kcal_pctle_matrix_long.csv'))

#=============
# first stacked area plot - all bins
#=============

# BinColor is a vector of color values that match the seven colors used in Herrero et al maps
binColor <- as.vector(read.csv(paste0(getwd(),'/FAPESP_pasture/tables_manuscript/BinColors.csv'), header = F)[,1])
binColor <- as.character(binColor)

tiff(paste0(getwd(),'/FAPESP_pasture/graphics_manuscript/', filter,
               '_kcal_distributions_by_bin_stacked area.tif'), width = 2000, height = 2000, 
    res = 300, compression = 'lzw', family = 'serif')
ggplot(df_bin_kcal_pctle_long, aes(as.numeric(percentile),MMkcalPctle)) +
  geom_area(aes(fill = factor(BinCode)))  + 
  scale_fill_manual(values = binColor)+ 
  xlab('Ranked area percentile as fraction') +
  ylab('Productivity (Million kcal/sq km/yr)') +
  theme(legend.position = 'none',
        text = element_text(size = 15)) +
  ggtitle('Productivity vs ranked global area')
dev.off()

head(df_bin_kcal_pctle_long)


#=============
# second stacked area plot - each bin - absolute terms
#=============
tiff(paste0(getwd(),'/FAPESP_pasture/graphics_manuscript/', filter,
            '_ABSOLUTE_kcal_distributions_by_bin_stacked area.tif'), width = 4000, height = 3500, 
     res = 300, compression = 'lzw', family = 'serif')
ggplot(df_bin_kcal_pctle_long, aes(as.numeric(percentile), 
                                   MMkcalPctle, 
                                   fill=factor(BinCode))) + 
  ggtitle('Absolute ranked productivity in each climate bin') +
  xlab("Increasing growing degree days") +
  ylab("Increasing precipitation") +
  geom_area() +
  geom_area(color = "black", size = 0.25) +
  facet_grid(TAP ~ GDD, as.table = FALSE) +
  scale_fill_manual(values = binColor) +
  theme(axis.text.x  = element_blank(),
        text = element_text(size = 15),
        legend.position = 'none')
dev.off()

#=============
# second stacked area plot - each bin - relative terms
#=============
tiff(paste0(getwd(),'/FAPESP_pasture/graphics_manuscript/', filter,
            '_RELATIVE_kcal_distributions_by_bin_stacked area.tif'), width = 4000, height = 3500, 
     res = 300, compression = 'lzw', family = 'serif')
ggplot(df_bin_kcal_pctle_long, aes(as.numeric(percentile), 
                                   MMkcalPctle_rel, 
                                   fill=factor(BinCode))) + 
  ggtitle('Relative ranked productivity in each climate bin') +
  xlab("Increasing growing degree days") +
  ylab("Increasing precipitation") +
  geom_area() +
  geom_area(color = "black", size = 0.25) +
  facet_grid(TAP ~ GDD, as.table = FALSE) +
  scale_fill_manual(values = binColor) +
  theme(axis.text.x  = element_blank(),
        text = element_text(size = 15),
        legend.position = 'none')
dev.off()


# CREATE DATAFRAMES FOR DETAILED YIELD GAP CALCULATIONS ====
df_bin_kcal_100 <- df_bin_kcal_pctle_long %>% filter(percentile == 1)
df_bin_kcal_100$Alow <- Alow
df_bin_kcal_100$Ahigh <- Ahigh
df_bin_kcal_100$LUmin <- LUmin
df_bin_kcal_100

write.csv(df_bin_kcal_100,
          paste0(getwd(),'/FAPESP_pasture/tables_manuscript/', filter, '_kcal_100pctle_long.csv'))
# Retrieve complete geographic data set for MMkcal and BinID
df1 <- as.data.frame(read.csv(
      paste0(getwd(),'/FAPESP_pasture/tables_manuscript/',filter, '_kcal_bins_combined.csv')))
df1 <- df1 %>% select(x,y,BinID,area, MMkcal_per_sq_km_per_y); head(df1)

df2 <- as.data.frame(read.csv(
       paste0(getwd(), '/FAPESP_pasture/tables_manuscript/', filter, '_kcal_100pctle_long.csv'))) %>%
  select(BinID,MMkcalPctle); head(df2)

df3 <- left_join(df1,df2,by="BinID"); head(df3)

write.csv(df3, paste0(getwd(),'/FAPESP_pasture/tables_manuscript/', filter, 
                     '_kcal_bins_combined_maxMMkcal.csv'))

# Add calculated variables for percent of max attainable yield (PctMaxAtt)
# and percent yield gap (PctGap)
df4 <- df3 %>% 
  mutate(PctGap = (MMkcalPctle - MMkcal_per_sq_km_per_y )/MMkcalPctle*100,
         PctMaxAtt = MMkcal_per_sq_km_per_y/MMkcalPctle*100,
         DeltaMax = MMkcalPctle-MMkcal_per_sq_km_per_y)
head(df4)

# MAP YIELD GAP CALCULATIONS ONTO GEOGRAPHIC SPACE
# Eliminate all but coordinates and percent max attainable
df5 <- df4 %>% select(x,y,PctMaxAtt)
head(df5)

df6 <- rasterFromXYZ(df5)
r_PctMax <- df6
writeRaster(r_PctMax, paste0(getwd(), '/FAPESP_pasture/raster_files/', filter, '_PctMax.tif'),
            overwrite =T)

# Map Potential productivity gain ====
# Eliminate all but coordinates and percent max attainable
df7 <- df4 %>% select(x,y,DeltaMax)
df8 = rasterFromXYZ(df7)

r_deltaMax = df8
writeRaster(r_deltaMax, paste0(getwd(), '/FAPESP_pasture/raster_files/', filter, '_DeltaMax.tif'),
            overwrite = T)

MMkcalAreaMean <- sum(df3$MMkcal_per_sq_km_per_y*df3$area)/sum(df3$area);MMkcalAreaMean
MMkcalTot <- sum(df3$MMkcal_per_sq_km_per_y*df3$area);MMkcalTot
MMkcalMaxTot <- sum(df3$area*df3$MMkcalPctle);MMkcalMaxTot
IP100 <- MMkcalMaxTot/MMkcalTot; IP100

# INTENSIFICATION POTENTIAL CALCULATIONS ====

df7 <- as.data.frame(read.csv(
    paste0(getwd(),'/FAPESP_pasture/tables_manuscript/', 
           filter, '_kcal_bins_combined_maxMMkcal.csv')))

df7 <- df7 %>% mutate(Max90 = 0.9*MMkcalPctle,
                Max75=0.75*MMkcalPctle, 
                Max50=0.5*MMkcalPctle,
                MMkcal90 = Max90*(MMkcal_per_sq_km_per_y <= Max90) + MMkcal_per_sq_km_per_y*(MMkcal_per_sq_km_per_y > Max90),
                MMkcal75 = Max75*(MMkcal_per_sq_km_per_y <= Max75) + MMkcal_per_sq_km_per_y*(MMkcal_per_sq_km_per_y > Max75),
                MMkcal50 = Max50*(MMkcal_per_sq_km_per_y <= Max50) + MMkcal_per_sq_km_per_y*(MMkcal_per_sq_km_per_y > Max50),
                PctGap50 = (MMkcalPctle - MMkcal50 )/MMkcalPctle*100,
                PctMaxAtt50 = MMkcal50/MMkcalPctle*100)

IP50 <- sum(df7$MMkcal50*df7$area)/sum(df7$MMkcal_per_sq_km_per_y*df7$area); IP50
IP75 <- sum(df7$MMkcal75*df7$area)/sum(df7$MMkcal_per_sq_km_per_y*df7$area); IP75
IP90 <- sum(df7$MMkcal90*df7$area)/sum(df7$MMkcal_per_sq_km_per_y*df7$area); IP90
IP100 <- MMkcalMaxTot/MMkcalTot; IP100

ip_total = c(IP100, IP90, IP75, IP50)

