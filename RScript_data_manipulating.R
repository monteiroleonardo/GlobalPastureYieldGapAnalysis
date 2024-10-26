# --------------------------------------------------------
# THIS R SCRIPT HAS BEEN DEVELOPED FOR FILTERING ARID AND SEMI-ARID PLACES
# ====================================================================================
rm(list = ls())
# setting work directory ----
path = 'c:/Users/leonardo.monteiro/Documents/FAPESP_pasture'
setwd(path)

# loading main packages ----
pkgs = c('raster', 'maptools', 'fields')
lapply(pkgs, require, character.only = T)

# loading files ----

# pasture ----
pasture = raster(paste0(path, '/Data_filtered/fpasture_occ_signif_resample.tif'))
total_area_pasture = cellStats(pasture*area(pasture), sum)
plot(pasture, col = 'black', legend = F)

#pasture suitability index----

# after filtering by psi classes
# -99 -> no idt
# 0 -> very low
# 1 -> low
# 2 - medium low
# 3 - medium
# 4 - medium high
# 5 - high
# 6 - very high

pasture_index = 'low'
psi = raster(paste0(path, '/shapefiles/', pasture_index, '_inputs/output/', pasture_index, '_resampled.tif'))
psi[is.na(pasture) | psi<0]<- NA

# mapping general psi ----

tiff(paste0(path, '/Maps/intensification suitability mask_', pasture_index, '_input.tif'), 
            height = 2500, width = 4000, res = 300, compression = 'lzw', family = 'serif')
world(fill = T, col = 'gray90')
image.plot(psi,col = c('darkred', 'red', 'orange', 'yellow', 'lightgreen', 'forestgreen', 'darkgreen'),
           breaks = c(seq(0,7)),
           horizontal = TRUE, 
           legend.shrink = .8,
           axes = FALSE, add=T,
           legend.width = .5, axis.args = list(cex.axis = 1,at = seq(0.5, 6.5), labels = c('Very Low \n (PSI = 0-10%)', 'Low \n (PSI = 10-20%)',
                                                                         'Medium-Low \n (PSI = 20-35%)',
                                                                         'Medium \n (PSI = 35-50%)', 
                                                                         'Medium High \n (PSI = 50-65%)',
                                                                         'High \n (PSI = 65-80%)',
                                                                       'Very High \n (PSI = 80-100%)'), padj=.5))
world(add=T)
par(cex = 1.5)
title('Global distribution of pasture suitability index (PSI) \n - Low Input levels -')
dev.off()


# filtering by psi ----
above_psi = psi
values(above_psi) <- ifelse(values(above_psi) <= 5, NA, values(above_psi))
plot(above_psi)

# converting the filtered psi raster for pasture fraction ----
pasture_above_psi_remaining = pasture
pasture_above_psi_remaining[is.na(above_psi)]<- NA
plot(pasture_above_psi_remaining, legend= F); world(add=T)
total_pasture_area_above_psi = cellStats(pasture_above_psi_remaining*area(pasture_above_psi_remaining), sum)

# writing the raster filtered above ----
writeRaster(pasture_above_psi_remaining, 
            paste0(path, '/Data_filtered/pasture_above_psi_removed_upto_mid_LOW_Inputs.tif'), overwrite = T)

# mapping general psi ----
tiff(paste0(path, '/Maps/intensification suitability mask - PSI_mid (low input).tif'), height = 2500, width = 4000, res = 300, compression = 'lzw', family = 'serif')
world(fill = T, col = 'gray90')
image.plot(above_psi,col = c('darkred', 'red', 'orange', 'yellow', 'lightgreen', 'forestgreen', 'darkgreen'),
           breaks = c(seq(0,7)),
           horizontal = TRUE, 
           legend.shrink = .8,
           axes = FALSE, add=T,
           legend.width = .5, axis.args = list(cex.axis = 1,at = seq(0.5, 6.5), labels = c('Very Low \n (PSI = 0-10%)', 'Low \n (PSI = 10-20%)',
                                                                                           'Medium-Low \n (PSI = 20-35%)',
                                                                                           'Medium \n (PSI = 35-50%)', 
                                                                                           'Medium High \n (PSI = 50-65%)',
                                                                                           'High \n (PSI = 65-80%)',
                                                                                           'Very High \n (PSI = 80-100%)'), padj=.5))
world(add=T)
par(cex = 1.5)
title('Global distribution of pasture suitability index (PSI) > 50% \n - Low Input levels -')
dev.off()

#LGP raster ----
lgp = raster(paste0(path, '/Data_filtered/LGP_all.tif'))

# land area covered by arid and semi arid LGP classes ----
# before filtering by psi classes
lgp_filter = lgp

#lgp classes ----
# 1 -> hyper-arid
# 2 and 3 -> arid
# 4 and 5 -> dry semi-arid
# 6 and 7 -> moist semi-arid
# 8-10 -> sub-humid 
# 11-15 -> humid
# 16 -> per-humid (Precip > Evapotr in all months of the year)  

# selecting LGP areas ----   
lgp_filter[lgp_filter !=1] <-  NA
writeRaster(lgp_filter, paste0(path, '/Data_filtered/LGP_hyperarid.tif'), overwrite = T)
plot(lgp_filter)

values(lgp_filter) <- ifelse(values(lgp_filter) == 1, 1, ifelse(
                    values(lgp) <= 3, 2, ifelse(
                    values(lgp) <= 5, 3, ifelse(
                    values(lgp) <= 7, 4,ifelse(
                    values(lgp) <= 10,6, ifelse(
                    values(lgp) < 16, 7, 8))))))


# here, remains all lgp classes below the cutting point

# calculation of area cover by pasture -> arid and semi arid categories
pasture_lgp_remaining = pasture
pasture_lgp_remaining[is.na(lgp_filter)]<- NA
total_lgp_area_remaining = cellStats(pasture_lgp_remaining*area(pasture_lgp_remaining),sum)
plot(pasture_lgp_remaining)

world(add=T)

tiff(paste0(path, '/Maps/Hyper-arid_Arid_Semi-arid_LGP.tiff'), res = 300,
     height = 2500, width = 4000, compression = 'lzw', family = 'serif')
world(fill = T, col = 'gray90')
image.plot(lgp_filter, col = c('darkred', 'red', 'orange', 'yellow', 'lightgreen', 'blue', 'darkblue'),
                       breaks = c(seq(0,7)),
                       horizontal = TRUE,
                       legend.shrink = .75,
                       axes = FALSE, add=T, 
                       legend.width = .75, 
                       axis.args = list(cex.axis = 1, at = seq(0.5, 6.5), labels = c('Hyper-Arid', 'Arid',
                                                                                        'Dry Semi-Arid',
                                                                                        'Moist Semi-Arid', 
                                                                                        'Sub humid', 'Humid',
                                                                                        'Per-humid')))
world(add=T)
dev.off()          
    

  # filtering according to the psi classes












pasture_arid_semi_overl = pasture_arid_semi
pasture_arid_semi_overl[is.na(psi_pasture)] <- NA
plot(psi_pasture)

total_pasture_arid_semi_overl_area = cellStats(pasture_arid_semi_overl*area(pasture_arid_semi_overl), sum)

tiff(paste0(path, '/Maps/overlapping.tif'), height = 2500, width = 4000, res = 300, compression = 'lzw', family = 'serif')
#world(fill = T, col = 'gray90')
plot(pasture, col = 'darkgreen', legend=F)
plot(pasture_arid_semi_overl, col = 'red', legend = F, add=T)
title('Gridcells where Arid and Semi arid regions overlaps gridcells labeled as <35% PSI \n (total overlapped area = 10.8 sq km)')
world(add=T)
legend('top', legend = c('Pasture', 'overlapping grid cells'), fill = c('darkgreen', 'red'), bty = 'n')
dev.off()

low_input = raster(paste0(path, '/shapefiles/low_inputs/output/low_reclass.tif'))
low_resampled = resample(low_input, pasture)
low_resampled_med_low = low_resampled
low_resampled_med_low[low_resampled_med_low > 2 | low_resampled_med_low < 0]<- NA
pasture_low_midlow = pasture
pasture_low_midlow[is.na(low_resampled_med_low)]<- NA

total_area_low_resampled_midlow = cellStats(pasture_low_midlow*area(pasture_low_midlow), sum)

plot(low_resampled_med_low)

lgp_arid_semi = lgp
lgp_arid_semi[lgp_arid_semi > 7] <- NA
plot(lgp, col = 'darkgreen', legend = F)
plot(lgp_arid_semi, legend = F, col = 'red', add=T)

overlap <- low_resampled_med_low
overlap[is.na(lgp_arid_semi)]<- NA
dev.off()
plot(overlap)

pasture_over = pasture
pasture_over[is.na(overlap)]<- NA


total_area_overlap = cellStats(pasture_over*area(pasture_over), sum)

low_pasture = pasture
low_pasture[is.na(pasture)] <- NA


