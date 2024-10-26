# ----------------------------------------------------------------------
# this script calculate area cover by ruminants under many aspects
# the outputs are highlighted as double dashed line
# ----------------------------------------------------------------------
rm(list = ls())
pkgs = c('raster', 'fields')
lapply(pkgs, require, character.only = T)

path = 'c:/users/leonardo.monteiro/Documents/FAPESP_pasture'
tap = raster(paste0(path, '/Resampled_files/TAP_resample.tif'))

# pasture areas from Ramankutty et al (2008) ----
r_fpasture = raster(paste0(path, '/rum_and_pasture/ramankutty_fracpasture.tif')) 
r_fpasture_resampled = resample(r_fpasture, tap)

plot(r_fpasture_resampled); world(add=T)
(total_pasture_area = cellStats(r_fpasture_resampled*area(r_fpasture_resampled),sum))
#=========================================================================================================================
writeRaster(r_fpasture_resampled, paste0(path, '/Resampled_files/fpasture_total_resampled.tif'), overwrite = T)
#=========================================================================================================================

# REPLACE ZERO WITH NAs AND CREATE "Data_filtered/fpasture_nonzero_resample.tif" ----
r_fpasture_resampled[r_fpasture_resampled == 0] <- NA
r_fpasture_resampled_nonzero <- r_fpasture_resampled
names(r_fpasture_resampled_nonzero)<-"fPasture_nonzero_resample"
r_fpasture_resampled_nonzero

# REMOVE PIXELS WITH PASTURE AREA BELOW 5% THRESHOLD OF SIGNIFICANCE AND CREATE "fpasture_signif_resample.tif" ----
r_p_area <- area(r_fpasture_resampled_nonzero)*r_fpasture_resampled_nonzero
names(r_p_area) <- "p_area"; r_p_area

# create a dataframe of area and fractional rasters ----
st_area <- stack(r_fpasture_resampled_nonzero,r_p_area)
df_area <- as.data.frame(st_area,xy = TRUE) # xy parameter passes coordinates into data frame
head(df_area)

# Sort dataframe from lowest to highest area per grid cell ----
df_area_ordered <- df_area[order(df_area$p_area),]
head(df_area_ordered)

# Determine the value of the area cutoff corresponding to 5% of cumulative area in dataframe ----
df_area_ordered$cumularea <- cumsum(df_area_ordered$p_area)
area_cutoff <- 0.05*max(df_area_ordered$cumularea, na.rm = TRUE); area_cutoff

# Remove all grid cells below the area cutoff ----
df_area_ordered$fPasture_nonzero_resample[df_area_ordered$cumularea<area_cutoff] <- NA
df_area_ordered$p_area[df_area_ordered$cumularea<area_cutoff] <- NA
head(df_area_ordered)

# remove all but fpasture column. Just want a raster of fraction data. ----
df_area_ordered$p_area <- NULL
df_area_ordered$cumularea <- NULL
df_area_ordered$fPasture_resample <- NULL
head(df_area_ordered)

# convert to spatialPixelsDataframe ----
r_fpasture_signif <- rasterFromXYZ(df_area_ordered)
crs(r_fpasture_signif) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
(total_pasture_area_nonzero = cellStats((area(r_fpasture_signif)*r_fpasture_signif),sum))

# create a GeoTiff files with nonzero and significant Ramankutty pasture fraction data
#=========================================================================================================================
writeRaster(r_fpasture_signif, paste0(path, '/Data_filtered/fpasture_signif_resampled.tif'), overwrite = TRUE)
#=========================================================================================================================

# calculating total signif occupied pasture ----
r_LGruminants_total = raster(paste0(path, '/Resampled_files/LGruminants_resample.tif'))
r_LGruminants_total[r_LGruminants_total == 0 ] <- NA
plot(r_LGruminants_total)

# pasture area covered by ruminants total ----
r_fpasture_ruminants_total = r_fpasture_signif
r_fpasture_ruminants_total[is.na(r_LGruminants_total)]<- NA
(total_pasture_area_signif_occ = cellStats(r_fpasture_ruminants_total*area(r_fpasture_ruminants_total), sum))

# total pasture unoccupied by ruminants ----
r_fpasture_unoccupied_ruminants_total = r_fpasture_signif
r_fpasture_unoccupied_ruminants_total[r_LGruminants_total] <- NA

# create a GeoTiff files with total pasture non occupied fraction area by ruminants
#=========================================================================================================================
writeRaster(r_fpasture_ruminants_total, paste0(path, '/Data_filtered/fpasture_ruminants_total_resampled.tif'), overwrite = TRUE)
#=========================================================================================================================





# create a GeoTiff files with total pasture fraction area occupied by ruminants
#=========================================================================================================================
writeRaster(r_fpasture_ruminants_total, paste0(path, '/Data_filtered/fpasture_ruminants_total_resampled.tif'), overwrite = TRUE)
#=========================================================================================================================

# create a GeoTiff files with total non zero pasture fraction area occupied by ruminants
r_LGruminants_total_nonzero = r_LGruminants_total
r_LGruminants_total_nonzero[r_LGruminants_total_nonzero == 0]<- NA
r_LGruminants_total_nonzero[is.na(r_LGruminants_total)]<- NA

r_fpasture_ruminants_nonzero = r_fpasture_resampled
r_fpasture_ruminants_nonzero[is.na(r_LGruminants_total_nonzero)] <- NA
(total_pasture_area_ruminants_nonzero = cellStats(r_fpasture_ruminants_nonzero*area(r_fpasture_ruminants_nonzero), sum))

# create a GeoTiff files with total pasture fraction area occupied by ruminants
#=========================================================================================================================
writeRaster(r_LGruminants_total_nonzero, paste0(path, '/Data_filtered/fpasture_ruminants_total_resampled.tif'), overwrite = TRUE)
#=========================================================================================================================



# total pasture area occupied by ruminants ----
r_fpasture_LGruminants = r_fpasture
plot(r_fpasture_LGruminants); world(add=T)

r_fpasture_LGruminants[is.na(r_LGruminants_occ_sig)] <- NA
plot(r_fpasture_LGruminants, col = 'green')
(total_pasture_area_cover_occ_rum = cellStats(r_fpasture_LGruminants*area(r_fpasture_LGruminants), sum))
#=========================================================================================================================
writeRaster(r_fpasture_LGruminants, paste0(path, '/Data_filtered/fpasture_occ_occupied.tif'), overwrite = T)
#=========================================================================================================================

# total pasture area not occupied by ruminants ----
r_fpasture_non_occ_LGruminants = r_fpasture
r_fpasture_non_occ_LGruminants[r_fpasture_LGruminants] <- NA
(total_fpasture_area_cover_non_occ_rum = cellStats(r_fpasture_non_occ_LGruminants*area(r_fpasture_non_occ_LGruminants), sum))
plot(r_fpasture_non_occ_LGruminants, col = 'red', add=T)
#=========================================================================================================================
writeRaster(r_fpasture_non_occ_LGruminants, paste0(path, '/Data_filtered/fpasture_non_occupied.tif'), overwrite = T)
#=========================================================================================================================




















