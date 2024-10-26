# _______________________________________________________________
# Rscript 36_Climate_bin_Ramankutty_Occ_Sig.R
# _______________________________________________________________

# This script established climate bins based on equal area in a 10x10 climate space 
# defined by growing degree days and annual precipitation. 
rm(list=ls())
setwd('c:/users/leonardo.monteiro/Documents/FAPESP_pasture')


# 2 - loading packages ----
pkgs <- c('rgdal', 'raster', 'maps', 'maptools', 'RColorBrewer', 'fields', 'ggplot2',
          'sp', 'rworldmap', 'fields')
lapply(pkgs, require, character.only = T)

# 3 - climate data ----
r_GDD0 = raster(paste0(getwd(),"/Resampled_files",
                               "/GDD0_resample.tif"))
names(r_GDD0) <- "GDD0"

r_TAP <- raster(paste0(getwd(), "/Resampled_files",
                                "/TAP_resample.tif"))
names(r_TAP) = "TAP"

# 4 - ruminants data ----
r_LGruminants_occ_sig <- raster(paste0(getwd(),"/Data_filtered", 
                                               "/LGruminants_occ_signif_fpast.tif"))
                                       
#r_fracPasture_occ_sig <- raster(paste0(getwd(), "/Data_filtered", 
#                                                "/fpasture_signif_resample.tif"))
                                
r_fracPasture_occ_sig <- raster(paste0(getwd(), '/Data_filtered/pasture_area_above_psi_midlow_filtered.tif'))

cellStats(r_fracPasture_occ_sig*area(r_fracPasture_occ_sig),sum)

# +++ 4.1 removing where dont have animals ----
r_fracPasture_occ_sig[is.na(r_LGruminants_occ_sig)] <- NA
names(r_fracPasture_occ_sig) <- "f_pasture_occ_sig"
cellStats(area(r_fracPasture_occ_sig)*r_fracPasture_occ_sig,sum)  

#r_fracPasture_occ_sig = pasture_lgp_humid

r_area_Pasture_occ_sig = r_fracPasture_occ_sig #pasture_lgp_humid#
r_area_Pasture = r_area_Pasture_occ_sig*area(r_area_Pasture_occ_sig)
names(r_area_Pasture) = 'r_area_Pasture'

# 5 - Combine the climate and pasture area data in a raster stack ----
st <- stack(r_GDD0, r_TAP, r_area_Pasture) #create raster stack
df_climate_occ_sig <- as.data.frame(st, xy = TRUE) # create data frame with geo coordinates
df_climate_occ_sig$GDD0[is.na(df_climate_occ_sig$GDD0)] <- 0 # set missing GDD vals to zero
df_climate_occ_sig$TAP[is.na(df_climate_occ_sig$TAP)] <- 0 # set missing TPP vals to zero
df_climate_occ_sig$TAP[is.nan(df_climate_occ_sig$TAP)] <- 0 # set missing TPP vals to zero

# 6 - Sort the data frame in ascending order of GDD0 ----
df_ordered <- df_climate_occ_sig[order(df_climate_occ_sig$GDD0),]
df_ordered$r_area_Pasture[is.na(df_ordered$r_area_Pasture)]<- 0
df_ordered$cumulArea <- cumsum(df_ordered$r_area_Pasture)
head(df_ordered)
# 7 - setting the cutoff points for GDD ----
GDDcutoff <- 1

# 8 - N is number of swaths along GDD0 x-axis in TAPvGDD0 climate grid ----
N <- 10

# 9 - Must have equal area representation across  swaths. So, goal is 
# to have a swath area equal to total area divided by the number of swaths:
P_swath_area <- max(df_ordered$cumulArea)/N
threshold <- GDDedge_index <- GDDedge_Value <- NA
length(threshold) <- length(GDDedge_index) <- length(GDDedge_Value) <- N+1

for (c in 1:(N+1)){
  v1 <- df_ordered$cumulArea[df_ordered$cumulArea>=P_swath_area*GDDcutoff*(c-1)]
  threshold[c] <- min(v1)
  GDDedge_index[c] <- match(threshold[c], df_ordered$cumulArea)
  GDDedge_Value[c] <- df_ordered$GDD0[GDDedge_index[c]]
  print(paste(GDDedge_Value[c], "at index ", GDDedge_index[c]))
}

df_GDD_swath <- df_GDD_swath_sorted <- area_cumul_binN <- list()
Area_bin <- NA
length(Area_bin) <- N
Areamod <- vector(length = 100)
threshold_TAP<- TAPedge_index <- TAPedge_value <- matrix(nrow = N+1,  ncol = N)

# 10 - the same for rainfall ----

TAPcutoff <- 0.95

v2 <- list()
for (n in 1:N) {
  df_GDD_swath[[n]] <-subset(df_ordered[df_ordered$GDD0 >= GDDedge_Value[n] & df_ordered$GDD0 < GDDedge_Value[n+1],1:5])
  df_GDD_swath_sorted[[n]] <- df_GDD_swath[[n]][order(df_GDD_swath[[n]]$TAP),]
  df_GDD_swath_sorted[[n]]$cumulArea <- cumsum(df_GDD_swath_sorted[[n]]$r_area_Pasture)
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

# 11 - Assign limiting values of TAP and GDD to climate bins ----
BinSize <- BinNum <- GDDmin <- GDDmax <- TAPmin <- TAPmax <- NA
length(BinNum) <- length(GDDmin) <- length(GDDmax) <- length(TAPmin) <- length(TAPmax)<- N^2

for (mTAP in 1:N){
  for (mGDD in 1:N) {
    BinNum[mTAP + N*(mGDD-1)] = mTAP + N*(mGDD-1)
    GDDmin[mTAP + N*(mGDD-1)] <- GDDedge_Value[mGDD]
    GDDmax[mTAP + N*(mGDD-1)] <- GDDedge_Value[mGDD + 1]
    TAPmin[mTAP + N*(mGDD-1)] <- TAPedge_value[mTAP,mGDD]
    TAPmax[mTAP + N*(mGDD-1)] <- TAPedge_value[mTAP+1,mGDD]
  }
}

# 12 - DF of BinID, GDDi, TAPi based on bin boundaries ----
df_bin_occ_sig <- data.frame(BinNum,GDDmin,GDDmax,TAPmin, TAPmax)

write.csv(df_bin_occ_sig, paste0(getwd(), '/Climate_bins/midlow_table_all.csv'))

write.csv(df_bin_occ_sig, paste0(getwd(),
                                "/Climate_bins", '/',
                                climates, '_bin_table_all_04-12-17.csv'))

# 13 - Stacked Bar graphic ----

tiff(paste0(getwd(), '/graphics/', 'PSI_midlow_stacked_bars.tif'),#climates, '_stacked_bars.tif'),
     height = 2500, width = 4000, res = 300, compression = 'lzw')
  par(mar = c(2,15,2,2))
binColor = read.csv(paste0(getwd(), '/Climate_Bins', '/BinColors.csv'), header = F)[,1]
x_axis_scale = round(c(0,unique(df_bin_occ_sig$GDDmax)),0)
p1<- ggplot(df_bin_occ_sig)
p2 <- geom_rect(aes(xmin = df_bin_occ_sig$GDDmin, xmax = df_bin_occ_sig$GDDmax,
                    ymin = df_bin_occ_sig$TAPmin, ymax = df_bin_occ_sig$TAPmax),
                color = 'black',fill = binColor, alpha = .5)

p1+p2 + scale_x_continuous('GDD', breaks = x_axis_scale) +
  scale_y_continuous ('TAP')+ylim(0,3000)+
  ggtitle(paste0('Climate Bins stacked bars - PSI above 35%'))# \n (', climates, ' climates)'))

dev.off()

# 14 - PLOT 5 MIN X 5 MIN GRID PIXELS IN CLIMATE SPACE (TAP X GDD0) ----
for (K in 1:N^2) {
  df_climate_occ_sig$BinID[
    df_climate_occ_sig$GDD0 >= GDDmin[K]
    & df_climate_occ_sig$GDD0 <= GDDmax[K]
    & df_climate_occ_sig$TAP >= TAPmin[K] 
    & df_climate_occ_sig$TAP <= TAPmax[K]] <- K
  print (K)
}

df_climate_occ_sig$BinID[df_climate_occ_sig$GDD0 == 0 & df_climate_occ_sig$TAP == 0] <- NA
vGDD <- as.vector(GDDedge_Value)
df_climate_occ_sig$GDDi <- as.vector(findInterval(df_climate_occ_sig$GDD0,vGDD))
df_climate_occ_sig$GDDi[df_climate_occ_sig$GDD0 == 0 & df_climate_occ_sig$TAP == 0] <- NA
df_climate_occ_sig$GDDi[df_climate_occ_sig$GDDi > 10] <- NA # The vector has 11 values. Get rid of 11th
df_climate_occ_sig$GDDi[df_climate_occ_sig$P_area_occ_sig==0 | is.na(df_climate_occ_sig$r_area_Pasture)] <- NA

# Create TAP index ----
df_climate_occ_sig$TAPi <- df_climate_occ_sig$BinID - N*(df_climate_occ_sig$GDDi - 1)

# Create a BinID data frame ----
df_BinID_occ_sig <-df_climate_occ_sig[,c(1,2,6)]
df_TAPi_occ_sig <- df_climate_occ_sig[,c(1,2,8)]
df_GDDi_occ_sig <- df_climate_occ_sig[,c(1,2,7)]

# CONVERT TAPi, GDDi, AND BINID DATA FRAMES TO RASTERS----
#AND SAVE AS GEOTIFF FILES

r_TAPi <- rasterFromXYZ(df_TAPi_occ_sig)
r_GDDi <- rasterFromXYZ(df_GDDi_occ_sig)
r_BinID <- rasterFromXYZ(df_BinID_occ_sig)

r_TAPi[is.na(r_area_Pasture)]<- NA
r_GDDi[is.na(r_area_Pasture)]<- NA
r_BinID[is.na(r_area_Pasture)] <- NA

# Save rasters as GTiff files
writeRaster(r_GDDi, paste0(getwd(),
                           '/Climate_bins', 
                           '/GDDi_psi_above_midlow.tif'),#, climates, '.tif'), 
            format = "GTiff", overwrite =TRUE)

writeRaster(r_TAPi, paste0(getwd(), 
                           "/Climate_bins", 
                           '/TAPi_psi_above_midlow.tif'),#', climates, '.tif'),format = "GTiff",
            overwrite = TRUE)

writeRaster(r_BinID, paste0(getwd(), 
                            '/Climate_bins', 
                            '/BinID_psi_above_midlow.tif'),#', climates, '.tif'),format = "GTiff",
            overwrite = TRUE)

# CREATE ROBINSON PROJECTION MAP OF CLIMATE BINS ====
r_binID <- raster(paste0(getwd(),
                         "/Climate_bins",
                         '/BinID_psi_above_midlow.tif'))#', climates, '.tif'))

# Load bin ID colors for mapping
binColor <- as.vector(read.csv(paste0(getwd(),"/Climate_bins",
                                      "/BinColors.csv"),header = FALSE))$V1
tiff(paste0(getwd(), '/Maps/General_climate_bins_psi_above_midlow.tif'),#, climates, '_General_climate_bins.tif'), 
     height = 2500, width = 4000, res = 300, compression = 'lzw', family = 'serif')
rplot <- r_binID
rplot <- na.omit(rplot)
colsBins <- binColor
palBins<-colorRampPalette(colsBins)
world(fill = T, col = 'gray90')
image.plot(rplot, 
           col = palBins(100),
           breaks = c(seq(0,100,1)),
           horizontal = TRUE,
           legend.shrink = 1.0,
           axes = FALSE, add=T, legend.width = .75, axis.args = list(cex.axis = 1.75))
#wrld_robin <- spTransform(getMap(), CRS("+proj=robin +over"))
#map(wrld_robin, add = T, lwd = 0.5)
world(add=T)
title(paste0('Climate bins based on 10 x 10 grid space of GDD and annual precipitation\n (PSI > 35%)'), cex.main = 1.5)#for occupied and signficant pasture \n (', 
             #climates, ' climates)'),cex.main = 1.5)
dev.off()
