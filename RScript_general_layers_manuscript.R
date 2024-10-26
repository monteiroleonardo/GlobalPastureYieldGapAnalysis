# This script was wrote to calculation of all pasture areas 
# main idea: fill the spreadsheet for pasture yield gap manuscript

# Ramankutty data available at http://www.earthstat.org/data-download/
# Described in the publication, Ramankutty et al. (2008), 
# "Farming the planet: 1. Geographic distribution of global 
# agricultural lands in the year 2000", 
# Global Biogeochemical Cycles, Vol. 22, GB1003, 
# doi:10.1029/2007GB002952.

# Assign directory ----
rm(list = ls())
setwd("c:/users/leonardo.monteiro/Documents/")
pkgs = c('rgdal', 'raster', 'maptools', 'RColorBrewer', 'fields', 'ggplot2', 'sp', 'rworldmap')
lapply(pkgs, require, character.only = T)


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                         INPUT FILES 
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#==========================
# CLIMATE FILES (2)
#==========================
r_GDD0 <- raster(paste0(getwd(),'/FAPESP_pasture/Resampled_files/GDD0.tif')); names(r_GDD0) <- "GDD0" # growing degree-days
r_TAP <- raster(paste0(getwd(),'/FAPESP_pasture/Resampled_files/TAP.tif')); names(r_TAP) <- "TAP" # precipitation
if(!file.exists(paste0(getwd(), '/FAPESP_pasture/shapefiles_manuscript/GDD0.tif'))){
  writeRaster(r_GDD0, paste0(getwd(), '/FAPESP_pasture/shapefiles_manuscript/GDD0.tif'))
}
if(!file.exists(paste0(getwd(), '/FAPESP_pasture/shapefiles_manuscript/TAP.tif'))){
  writeRaster(r_TAP, paste0(getwd(), '/FAPESP_pasture/shapefiles_manuscript/TAP.tif'))
}

#==========================
# FRACTION PASTURE FILES (4)
#==========================
r_fpasture_total = raster(paste0(getwd(), '/FAPESP_pasture/Resampled_files/total_fpasture_ramankutty.tif')) # total pasture frac Ramankutty et al. (2008)
cellStats(r_fpasture_total*area(r_fpasture_total),sum)
if(!file.exists(paste0(getwd(), '/FAPESP_pasture/shapefiles_manuscript/total_fpasture_ramankutty.tif'))){
  writeRaster(r_fpasture_total, paste0(getwd(), '/FAPESP_pasture/shapefiles_manuscript/total_fpasture_ramankutty.tif'))
}

r_fpasture_occ = raster(paste0(getwd(), '/FAPESP_pasture/Data_filtered/total_fpasture_occ.tif'))
cellStats(r_fpasture_occ*area(r_fpasture_occ),sum) # same area, as should be
if(!file.exists(paste0(getwd(), '/FAPESP_pasture/shapefiles_manuscript/total_fpasture_occ.tif'))){
  writeRaster(r_fpasture_occ, paste0(getwd(), '/FAPESP_pasture/shapefiles_manuscript/total_fpasture_occ.tif'))
}

r_fpasture_signif = raster(paste0(getwd(), '/FAPESP_pasture/Data_filtered/total_fpasture_signif.tif')) # total pasture frac Ramankutty signif (>5% bottom area)
cellStats(r_fpasture_signif*area(r_fpasture_signif),sum)
if(!file.exists(paste0(getwd(), '/FAPESP_pasture/shapefiles_manuscript/total_fpasture_signif.tif'))){
  writeRaster(r_fpasture_signif, paste0(getwd(), '/FAPESP_pasture/shapefiles_manuscript/total_fpasture_signif.tif'))
}

r_fpasture_occ_signif = raster(paste0(getwd(), '/FAPESP_pasture/Data_filtered/total_fpasture_occ_signif.tif')) # signif and occ pasture frac Ramankutty
cellStats(r_fpasture_occ_signif*area(r_fpasture_occ_signif),sum)
if(!file.exists(paste0(getwd(), '/FAPESP_pasture/shapefiles_manuscript/total_fpasture_occ_signif.tif'))){
  writeRaster(r_fpasture_occ_signif, paste0(getwd(), '/FAPESP_pasture/shapefiles_manuscript/total_fpasture_occ_signif.tif'))
}

#==========================
# RUMINANTS FILES (2)
#==========================
r_LGcattle = raster(paste0(getwd(), '/FAPESP_pasture/shapefiles_manuscript/LGcattle_resample.tif'))
r_LGshoats = raster(paste0(getwd(), '/FAPESP_pasture/shapefiles_manuscript/LGshoats_resample.tif'))
r_LGruminants <- r_LGcattle + 0.2*r_LGshoats
names(r_LGruminants)<-"LGruminants_resample"

r_LGruminants_occ = r_LGruminants # create a raster of ruminants in occupied lands
r_LGruminants_occ[r_LGruminants_occ==0] <- NA
cellStats(r_LGruminants_occ*area(r_LGruminants_occ), sum) # total ruminants in the world

#==========================
# RUMINANTS FILES CORRECTED BY OCCUPIED PASTURE FRACTION
#==========================
r_LGruminants_occ_fpast <- r_LGruminants_occ
r_LGruminants_occ_fpast <- r_LGruminants_occ/r_fpasture_occ
names(r_LGruminants_occ_fpast) <- "Ruminant_density_occ_corrected_pasture"; r_LGruminants_occ_fpast
cellStats(r_LGruminants_occ_fpast*area(r_fpasture_occ)*r_fpasture_occ, sum)

#==========================
# RUMINANTS FILES CORRECTED BY OCC and SIGNIF PASTURE FRACTION (bottom 5% removed)
#==========================
r_LGruminants_occ_signif_fpast <- raster(paste0(getwd(), '/FAPESP_pasture/shapefiles_manuscript/LGruminants_occ_signif_fpast.tif'))
r_LGruminants_occ_signif_fpast <- r_LGruminants_occ_signif_fpast/r_fpasture_occ
names(r_LGruminants_occ_signif_fpast) <- "Ruminant_density_occ_signif_fpast_corrected"; r_LGruminants_occ_signif_fpast
cellStats(r_LGruminants_occ_signif_fpast*area(r_fpasture_occ_signif)*r_fpasture_occ_signif, sum)

#==========================
# PASTURE PRODUCTIVITY - general (all pasture) - TOTAL KCAL ACCORDING TO HERRERO ET AL. (2013)
#==========================
r_totmeat_MMkcal <- raster(paste0(getwd(),'/FAPESP_pasture/shapefiles_manuscript/r_totmeat_MMkcal.tif'))
r_totmilk_MMkcal <- raster(paste0(getwd(),'/FAPESP_pasture/shapefiles_manuscript/r_totmilk_MMkcal.tif'))

r_tot_MMkcal_food <- r_totmeat_MMkcal + r_totmilk_MMkcal
names(r_tot_MMkcal_food)<-"tot_MMkcal_food"; r_tot_MMkcal_food
cellStats(r_tot_MMkcal_food*area(r_tot_MMkcal_food),sum) 

if(!file.exists(paste0(getwd(), '/FAPESP_pasture/shapefiles_manuscript/r_tot_MMkcal_food.tif'))){
  writeRaster(r_tot_MMkcal_food, 
              paste0(getwd(), '/FAPESP_pasture/shapefiles_manuscript/r_tot_MMkcal_food.tif'))
}

#==========================
# PASTURE PRODUCTIVITY - occupied areas
#==========================
r_tot_MMkcal_food_occ <- raster(paste0(getwd(), '/FAPESP_pasture/shapefiles_manuscript/r_tot_MMkcal_food.tif'))
names(r_tot_MMkcal_food_occ)<-"tot_MMkcal_food_occ"
r_tot_MMkcal_food_occ[r_tot_MMkcal_food_occ==0] <- NA; r_tot_MMkcal_food_occ
cellStats(r_tot_MMkcal_food_occ*area(r_tot_MMkcal_food_occ), sum) # total kcal in livestock according to Herrero (like above)

#==========================
# PASTURE PRODUCTIVITY - occupied areas and adj by pasture fraction (nonzero)
#==========================
r_tot_MMkcal_food_occ <- raster(paste0(getwd(), '/FAPESP_pasture/shapefiles_manuscript/r_tot_MMkcal_food_occ.tif'))
r_tot_MMkcal_food_occ[r_tot_MMkcal_food_occ==0] <- NA; r_tot_MMkcal_food_occ

r_tot_MMkcal_food_occ_fpast <- r_tot_MMkcal_food_occ
r_tot_MMkcal_food_occ_fpast <- r_tot_MMkcal_food_occ/r_fpasture_occ
names(r_tot_MMkcal_food_occ_fpast) <- "tot_MMkcal_food_occ_corrected_pasture"; r_tot_MMkcal_food_occ_fpast
cellStats(r_tot_MMkcal_food_occ_fpast*area(r_fpasture_occ)*r_fpasture_occ, sum) 
# total food production where is considered pasture by Ramankutty(considering all pasture, not only where is signif)

#==========================
# PASTURE PRODUCTIVITY - occupied areas and adj by pasture fraction (nonzero) and signif (remove bottom 5% of total area)
#==========================
r_tot_MMkcal_food_occ_signif_fpast <- raster(paste0(getwd(), '/FAPESP_pasture/shapefiles_manuscript/r_tot_MMkcal_food_occ_signif_fpast.tif'))
names(r_tot_MMkcal_food_occ_signif_fpast)<-"tot_MMkcal_food_occ_signif_fpast"
cellStats(r_tot_MMkcal_food_occ_signif_fpast*area(r_fpasture_occ_signif)*r_fpasture_occ_signif, sum)

#==========================
# FILTERING SKINS FOR FILLING TABLE - USING LGP CLASSES
#==========================
r_lgp = raster(paste0(getwd(), '/FAPESP_pasture/Resampled_files/LGP_resample.tif'))
if(!file.exists(paste0(getwd(), '/FAPESP_pasture/shapefiles_manuscript/LGP.tif'))){
  writeRaster(r_lgp, 
              paste0(getwd(), '/FAPESP_pasture/shapefiles_manuscript/LGP.tif'))
}

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
r_psi = raster(paste0(getwd(), '/FAPESP_pasture/Resampled_files/psi_low_resample.tif'))
if(!file.exists(paste0(getwd(), '/FAPESP_pasture/shapefiles_manuscript/PSI.tif'))){
  writeRaster(r_psi, 
              paste0(getwd(), '/FAPESP_pasture/shapefiles_manuscript/PSI.tif'))
}

# -99 = no data
# 0 = Very low (0 - 10%)
# 1 = Low (10 - 20%)
# 2 = Medium low (20 - 35%)
# 3 = Medium (35 - 50%)
# 4 = Medium High (50 - 65%)
# 5 = High (65 - 80%)
# 6 = Very High (> 80%)

# LGP CLASSES ----
r_lgp_filt = r_lgp
r_lgp_filt[r_lgp_filt < 11 | r_lgp_filt > 15] <- NA
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
cellStats(r_psi_filt_fpasture*area(r_psi_filt_fpasture), sum)
# pasture area into PSI  filtering scheme (million sq km)
r_psi_filt_total_food = r_tot_MMkcal_food_occ_signif_fpast
r_psi_filt_total_food[is.na(r_psi_filt)] <- NA
cellStats(r_psi_filt_total_food*area(r_psi_filt_fpasture)*r_psi_filt_fpasture, sum)
# total food production (kcal per sq km per yr)
