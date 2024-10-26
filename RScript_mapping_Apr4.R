# this script allows to create maps for manuscript in Robinson projection
rm(list = ls())
# packages to be loaded ----
pkgs = c('maptools', 'fields', 'ggplot2', 'rworldmap', 'rgdal', 'raster', 'RColorBrewer')
lapply(pkgs, require, character.only  = T)

# LOADING COUNTRIES AROUND THE WORLD ----
PROJ = '+proj=longlat +over'

world_countries = readShapeSpatial('c:/users/leonardo.monteiro/Documents/FAPESP_pasture/shapefiles_manuscript/world_countries.shp')
world_final = world_countries[-which(world_countries@data$continent == 'Antarctica'),]

# projecting all rasters for a given projection ----
#world_box_proj = spTransform(world_box, CRSobj = PROJ)
#world_grid_proj = spTransform(world_grid, CRSobj = PROJ)
#world_countries_proj = spTransform(world_countries, CRSobj = PROJ)

# all raster files to generate the histograms and maps ----
filter = 'humid'

total_food = raster('c:/users/leonardo.monteiro/Documents/FAPESP_pasture/shapefiles_manuscript/r_tot_MMkcal_food_occ_signif_fpast.tif')
r_PctMax = raster(paste0('c:/users/leonardo.monteiro/Documents/FAPESP_pasture/raster_files/',
                         filter, '_PctMax.tif'))
r_DeltaMax = raster(paste0('c:/users/leonardo.monteiro/Documents/FAPESP_pasture/raster_files/',
                         filter, '_DeltaMax.tif'))

#============================================================================================
# FIGURE 1 - creating the histogram (milk and meat productivity in signif and occ pasturelands)
#============================================================================================
tiff(paste0(main_path_graphics,'/histogram_milk_meat_productivity.tif'),
        res = 300,
        width = 2000,
        height = 2000,
        compression = 'lzw', family = 'serif')
par(mar = c(5,7,3,3))
hist(values(),
     col = 'wheat', 
     xlab = 'Million kcal/sq km/yr', 
     ylab = '',
     xlim = c(0,90), axes = F, main = '')
axis(1, seq(0,90,10), adj = 0, pos = -1)
axis(2, seq(0,250000, 50000), las = 2, pos = 0)
mtext(side = 2, text = 'Frequency', padj = -7)
legend(x = 60, y = 50000, legend = c(paste0('Mean = ',round(mean(values(rplot), na.rm = T),2)),
                              paste0('Median = ',round(median(values(rplot), na.rm = T),2)),
                              paste0('Max = ',round(max(values(rplot), na.rm = T),2))), 
       col = NA, bty = 'n')
dev.off()
#============================================================================================


#-------------------------
colsHerrero <- brewer.pal(5, 'BrBG')#c("#FFD280","#EBE79D","#D3FFBF","#9ED99F","#6DB581","#3E9466","#00734C")

#============================================================================================
# FIGURE 1A - creating the map (milk and meat productivity in signif and occ pasturelands)
#============================================================================================

tiff(
    paste0('c:/users/leonardo.monteiro/Documents/FAPESP_pasture/Maps_manuscript/F1_', 
  filter, '_global_food_production.tif'),
  width = 4000, height = 2500, res = 300, compression = 'lzw', family = 'serif')

par(mar = c(2,.5,4,1), mai = c(0,.25,1,.25))
plot(world_final, col = 'gray90', ylim = c(-40, 30), xlim = c(-115, 145))

image.plot(total_food, legend.mar = 5,
     col=colsHerrero,
     breaks = c(0,1, 2.5,5,10, 25), 
     horizontal = TRUE,
     axes=FALSE,
     legend.shrink = .75, 
     legend.width =.5,
     axis.args = list(at = c(0,1,2.5,5,10,25), 
                      labels = c('0','1.0','2.5','5.0','10.0','>25.0')), add=T)
plot(world_final, add=T)

par(cex = 1)
if(filter != 'total_pasture'){
  title('Spatial variability of milk and meat productivity in occupied and significant pastureland \n (Million kcal/sq km/yr)')
}else{
  title(paste0('Spatial variability of milk and meat productivity in occupied and significant pastureland in ',
               toupper(filter), ' climates, \n (Million kcal/sq km/yr)'))
}
dev.off()
#============================================================================================


#============================================================================================
# FIGURE 2A1 - creating the map (LGP DISTRIBUTION in signif and occ pasturelands)
#============================================================================================

lgp = raster('c:/users/leonardo.monteiro/Documents/FAPESP_pasture/shapefiles_manuscript/LGP.tif')
crs(lgp) = '+proj=longlat +over'
fpasture = raster('c:/users/leonardo.monteiro/Documents/FAPESP_pasture/shapefiles_manuscript/r_tot_MMkcal_food_occ_signif_fpast.tif')
lgp_pasture = lgp
lgp_pasture[is.na(fpasture)] <- NA
lgp_pasture = projectRaster(lgp_pasture, crs = '+proj=longlat +over')

values(lgp_pasture) = round(values(lgp_pasture),0)

values(lgp_pasture) <- ifelse(values(lgp_pasture) <=1, 1, ifelse(
  values(lgp_pasture) <= 3, 2,ifelse(
    values(lgp_pasture) <= 5, 3,ifelse(
      values(lgp_pasture) <= 7, 4,ifelse(
        values(lgp_pasture) <= 10, 5,6)))))
cols_lgp = c('darkred', 'red', 'orange', 'yellow', 'lightblue', 'navy')

tiff('c:/users/leonardo.monteiro/Documents/FAPESP_pasture/Maps_manuscript/F2A_LGP_pasture_distribution.tif',
     width = 4000, height = 2500, res = 300, compression = 'lzw', family = 'serif')

par(mar = c(2,.5,4,1), mai = c(0,.25,1,.25))
plot(world_final, col = 'gray90', ylim = c(-40, 30), xlim = c(-115, 145))

image.plot(lgp_pasture,
           col=cols_lgp,
           breaks = 0:6, 
           horizontal = TRUE,
           axes=FALSE,
           legend.shrink = .75, 
           legend.width =.5, legend.mar = 5,
           axis.args = list(at = seq(.5,5.5), 
                            labels = c('Hyper arid \n (0 days)',
                                       'Arid \n (1-59 days)',
                                       'Dry semi-arid \n (60-119 days)',
                                       'Moist semi-arid \n (120-179 days)',
                                       'Sub humid \n (180-269 days)',
                                       'Humid \n (270-365 days)'),padj = .4), add=T)
plot(world_final, add=T)

par(cex = 1)
title('Spatial variability of Length of Growing Period (LGP) in occupied and significant pastureland')
dev.off()

#============================================================================================
# FIGURE 2A2 - creating the lgp-histogram (LGP DISTRIBUTION in signif and occ pasturelands)
#============================================================================================

dd = data.frame(obs = as.matrix(table(values(lgp_pasture))), 
                classes = c('Hyper arid', 'Arid', 'Dry semi-arid', 'Moist semi-arid', 'Sub humid', 'Humid')
);
dd

tiff('c:/users/leonardo.monteiro/Documents/FAPESP_pasture/graphics_manuscript/barplot_LGP_classes.tif',
     res = 300,
     width = 2000,
     height = 2000,
     compression = 'lzw', family = 'serif')
par(mar = c(7,7,3,1), cex = 1)

barplot(dd[,1],1, axes = F, ylim = c(0,120000), col = 'wheat')
axis(1, at = seq(.7, 7,1.2), labels = c('Hyper\n arid', 'Arid','Dry \n semi-arid', 
                                        'Moist \n semi-arid', 'Sub \n humid', 'Humid'), padj = .5)
axis(2, at = seq(0,120000, 20000), las = 2); abline(h = 0)
par(cex = 1.5)
mtext(side = 1, text = 'LGP classes', padj = 7)
mtext(side = 2, text = 'Number of grid cells', padj = -7)
dev.off()

#============================================================================================
# FIGURE 2B1 - creating the map (PSI DISTRIBUTION in signif and occ pasturelands)
#============================================================================================

psi = raster('c:/users/leonardo.monteiro/Documents/FAPESP_pasture/shapefiles_manuscript/PSI.tif')
fpasture = raster('c:/users/leonardo.monteiro/Documents/FAPESP_pasture/shapefiles_manuscript/r_tot_MMkcal_food_occ_signif_fpast.tif')
psi_pasture = psi
psi_pasture[is.na(fpasture)] <- NA
psi_pasture = projectRaster(psi_pasture, crs = PROJ)
psi_pasture[psi_pasture<0] <- NA
values(psi_pasture) = round(values(psi_pasture), 0)
table(values(psi_pasture))

cols_psi = brewer.pal(7, 'Greens')#c('darkred', 'red', 'orange', 'yellow', 'lightblue', 'navy')

tiff('c:/users/leonardo.monteiro/Documents/FAPESP_pasture/Maps_manuscript/F2B_PSI_pasture_distribution.tif',
     width = 4000, height = 2500, res = 300, compression = 'lzw', family = 'serif')

par(mar = c(2,.5,4,1), mai = c(0,.25,1,.25))
plot(world_final, col = 'gray90', ylim = c(-40, 30), xlim = c(-115, 145))
image.plot(psi_pasture,
           col=cols_psi,
           breaks = 0:7, 
           horizontal = TRUE,
           axes=FALSE,
           legend.shrink = .75, 
           legend.width =.5, legend.mar = 5,
           axis.args = list(at = seq(.5,6.5), 
                            labels = c('Very Low \n (0-10%)',
                                       'Low \n (10-20%)',
                                       'Medium Low \n (20-35%)',
                                       'Medium \n (35-50%)',
                                       'Medium High \n (50-65%)',
                                       'High \n (65-80%)',
                                       'Very High \n (80-100%)'), padj = .4), add=T)
plot(world_final, add=T)

par(cex = 1)
title('Spatial variability of Pasture Suitability Index (PSI) in occupied and significant pastureland')
dev.off()

#============================================================================================
# FIGURE 2B2 - creating the histogram (PSI DISTRIBUTION in signif and occ pasturelands)
#============================================================================================

dd_psi = data.frame(obs = as.matrix(table(values(psi_pasture))), 
                classes = c('Very \n low', 'Low', 'Medium \n low', 'Medium', 'Medium \n high',
                            'High', 'Very \n high'))
dd_psi

tiff('c:/users/leonardo.monteiro/Documents/FAPESP_pasture/graphics_manuscript/barplot_PSI_classes.tif',
     res = 300,
     width = 2500,
     height = 2000,
     compression = 'lzw', family = 'serif')
par(mar = c(7,7,3,1), cex = 1)

barplot(dd_psi[,1],1, axes = F, ylim = c(0,120000), col = 'wheat')
axis(1, at = seq(.7, 8,1.2), labels = dd_psi[,2], padj = .5)
axis(2, at = seq(0,120000, 20000), las = 2); abline(h = 0)
par(cex = 1.5)
mtext(side = 1, text = 'PSI classes', padj = 7)
mtext(side = 2, text = 'Number of grid cells', padj = -7)
dev.off()

#===
#  FIGURE 2B CONTAINING ONLY LOW, MID AND HIGH PSI CLASSES
#===

values(psi_pasture) = ifelse(values(psi_pasture) <= 1,1,ifelse(
                             values(psi_pasture) <=4, 2,3))
table(values(psi_pasture))

tiff('c:/users/leonardo.monteiro/Documents/FAPESP_pasture/Maps_manuscript/F2B_PSI_LOW+MID+HIGH_pasture_distribution.tif',
     width = 4000, height = 2500, res = 300, compression = 'lzw', family = 'serif')

par(mar = c(2,.5,4,1), mai = c(0,.25,1,.25))
plot(world_final, col = 'gray90', ylim = c(-40, 30), xlim = c(-115, 145))
image.plot(psi_pasture,
           col= c('lightgreen', 'aquamarine4', 'darkgreen'),# 'darkolivegreen'),#brewer.pal(3, 'Greens'),#c('lightgreen', 'green', 'darkgreen'),
           breaks = 0:3, 
           horizontal = TRUE,
           axes=FALSE,
           legend.shrink = .5, 
           legend.width =.5, legend.mar = 5,
           axis.args = list(at = seq(.5,2.5), 
                            labels = c('Low \n (0-20%)',
                                       'Medium \n (20-65%)',
                                       'High \n (65-100%)'), padj = .4), add=T)
plot(world_final, add=T)

par(cex = 1)
title('Spatial variability of Pasture Suitability Index (PSI) in occupied and significant pastureland')
dev.off()

#==============
# HIST - PERC MAX ACHIEVED
#==============

tiff(paste0('c:/users/leonardo.monteiro/Documents//FAPESP_pasture/graphics_manuscript/', 
     filter, '_hist_PctMax.tif'),
     width = 2000, height = 2000, compression = 'lzw', family = 'serif', res = 300)
par(mar=c(5,7,3,3))
hist(values(r_PctMax), 
     col = 'wheat',
     xlab = 'Max percentile achieved', main = '',
     ylab = '',
     xlim = c(0,100), axes = F, ylim = c(0,100000))
axis(1, seq(0,100,20), pos = -1)
axis(2, at = seq(0,80000, 20000),labels = seq(0,80000, 20000), las = 2, pos = 0)
mtext(side = 2, text = 'Frequency', padj = -7)
legend(x = 60, y = 50000, 
       legend = c(paste0('Mean = ',round(mean(values(r_PctMax), na.rm = T),2)),
                  paste0('Median = ',round(median(values(r_PctMax), na.rm = T),2)),
                  paste0('Max = ',round(max(values(r_PctMax), na.rm = T),2))), bty = 'n')
dev.off()

#==============
# MAP - PERC MAX ACHIEVED
#==============

tiff(
  paste0('c:/users/leonardo.monteiro/Documents/FAPESP_pasture/Maps_manuscript/F3_',
  filter, '_PctleMax_pasture_distribution.tif'),
     width = 4000, height = 2500, res = 300, compression = 'lzw', family = 'serif')

par(mar = c(2,.5,4,1), mai = c(0,.25,1,.25))
plot(world_final, col = 'gray90', ylim = c(-40, 30), xlim = c(-115, 145))
image.plot(r_PctMax,
           col=colorRampPalette(c('darkred', 'yellow', 'darkgreen'))(10),#c('lightgreen', 'green', 'darkgreen'),
           breaks = seq(0,100,10), 
           horizontal = TRUE,
           axes=FALSE,
           legend.shrink = .5, 
           legend.width =.5, legend.mar = 5,
           axis.args = list(at = seq(0,100, 10), 
                            labels = seq(0,100,10)), add=T)
plot(world_final, add=T)

par(cex = 1)
if(filter != 'total_pasture'){
  title(paste0('Spatial variability of percentage kcal productivity achieved in occupied and significant pastureland in "', 
  filter, '" climates'))
  }else{
  title(paste0('Spatial variability of percentage kcal achieved in occupied and significant pastureland'))
}
dev.off()

#==============
# HIST - DELTA MAX ACHIEVED
#==============

tiff(paste0(path,'/FAPESP_pasture/graphics_manuscript/', filter, '_hist_DeltaMax.tif'),
     width = 2000, height = 2000, compression = 'lzw', family = 'serif', res = 300)
par(mar=c(5,7,3,3))
hist(values(r_DeltaMax), 
     col = 'wheat',
     xlab = 'Delta Max achieved (Million kcal/sq km/yr)', main = '',
     ylab = '',
     xlim = c(0,210), axes = F, ylim = c(0,210000))
axis(1, seq(0,210,30), pos = -1)
axis(2, at = seq(0,180000, 30000),labels = seq(0,180000, 30000), las = 2, pos = 0)
mtext(side = 2, text = 'Frequency', padj = -7)
legend(x = 150, y = 80000, 
       legend = c(paste0('Mean = ',round(mean(values(r_DeltaMax), na.rm = T),2)),
                  paste0('Median = ',round(median(values(r_DeltaMax), na.rm = T),2)),
                  paste0('Max = ',round(max(values(r_DeltaMax), na.rm = T),2))), bty = 'n')
dev.off()


#==============
# MAP - Delta Max ACHIEVED
#==============

tiff(
  paste0('c:/users/leonardo.monteiro/Documents/FAPESP_pasture/Maps_manuscript/F4_', filter, '_DeltaMax_',
         'distribution.tif'),
     width = 4000, height = 2500, res = 300, compression = 'lzw', family = 'serif')

par(mar = c(2,.5,4,1), mai = c(0,.25,1,.25))
plot(world_final, col = 'gray90', ylim = c(-40, 30), xlim = c(-115, 145))

image.plot(r_DeltaMax,
           col= c('darkred', 'orange', 'yellow', 'lightblue', 'darkblue'),#c('lightgreen', 'green', 'darkgreen'),
           breaks = c(0,10, 25, 50, 100, 200), 
           horizontal = TRUE,
           axes=FALSE,
           legend.shrink = .75, 
           legend.width =.5, legend.mar = 5,
           axis.args = list(at = c(0,10, 25, 50, 100, 200), 
                            labels = c('0','10', '25', '50', '100', '>200')), add=T)
plot(world_final, add=T)
par(cex = 1)
if(filter != 'total_pasture'){
  title(paste0('Spatial variability of maximum yield gap in occupied and significant pastureland in "', 
               filter, '" climates \n (Million kcal/sq km/yr)'))
}else{
  title('Spatial variability of maximum yield gap in occupied and significant pastureland \n (Million kcal/sq km/yr)')
}
dev.off()













