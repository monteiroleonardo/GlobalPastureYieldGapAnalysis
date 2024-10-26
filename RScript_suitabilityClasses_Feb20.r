# --------------------------------------------------------
# THIS R SCRIPT HAS BEEN DEVELOPED FOR FILTERING GLOBAL SUITABILITY LANDS FOR PASTURE AND CROPS 
# ====================================================================================
#The FGGD combined suitability maps at low, intermediate and high levels of inputs
#under rainfed conditions on global land area are global raster datalayers with
#a resolution of 5 arc-minutes. Each pixel contains a combined suitability class 
#value for rainfed agriculture, based on the 2005 version of the suitability indexes
#for rainfed production of all crops plus pasture. The method is described in FAO and IIASA, 2007,
#Mapping biophysical factors that influence agricultural production and rural vulnerability,
#by H. von Velthuizen et al.
# ====================================================================================


# clearing R memory ----
rm(list = ls())

# your path ----
path = 'c:/Users/leonardo.monteiro/Documents/FAPESP_pasture/'

# loading libraries ----
pkgs = c('maptools', 'raster', 'rworldmap', 'fields', 'rgdal')
lapply(pkgs, require, character.only = T)

# setting working directory ----
setwd(path)
print (paste0('your current working directory is: ', getwd()))

# land suitability classes (high, intermediate and low) ----
land_class = 'high' # select here the follow classes: high, interm or low

# loading main files ----
data = raster(paste0(path, 'shapefiles/', land_class, '_inputs/output/', land_class, '_reclass.tif'))
pasture = raster(paste0(path, 'Data_filtered/fpasture_occ_signif_resample.tif'))

# total area cover by pasture (in million sq km) ----
total_pasture_area = cellStats(pasture*area(pasture), sum)
print (paste0('Total pasture area is: ', round(total_pasture_area/10^6,2), ' million sq km'))

if(!file.exists(paste0(path, 'shapefiles/', land_class, '_inputs/output/', land_class, '_reclass.tif')))
  {
    # resampling data file ----
    data_res = resample(data, pasture);
    # defining cutting intervals after resampling proc ----
    values(data_res) = ifelse(values(data_res) < 0 & !is.na(values(data_res)), -999, ifelse(
                            values(data_res) < .5 & !is.na(values(data_res)), 0, ifelse(
                            values(data_res) < 1.5 & !is.na(values(data_res)), 1, ifelse(
                            values(data_res) < 2.5 & !is.na(values(data_res)), 2, ifelse(
                            values(data_res) < 3.5 & !is.na(values(data_res)), 3, ifelse(
                            values(data_res) < 4.5 & !is.na(values(data_res)), 4, ifelse(
                            values(data_res) < 5.5 & !is.na(values(data_res)), 5, 6)))))))
    # removing all values respective to water bodies ----
      data_res[data_res < 0] = NA
    # filtering suitability classes where there is pasturelands ----
      data_res[is.na(pasture)] = NA
    # generating suitablity GeoTiff map for suitability classes
    writeRaster(data_res, paste0(path, 'Data_filtered/', land_class, '_inputs.tif'), overwrite = T)
  }else{
    data_res = raster(paste0(path, 'Data_filtered/', land_class, '_inputs.tif'))
  }

# suitability classes for filtering----
# 0 >> very low
# 1 >> low
# 2 >> medium low
# 3 >> medium
# 4 >> medium high
# 5 >> high
# 6 >> very high

suitability_classes = data.frame(suit_classes = c(0,1,2,3,4,5,6), 
                                 classes_name = c('Very Low', 
                                                  'Low',
                                                  'Medium Low',
                                                  'Medium', 
                                                  'Medium High',
                                                  'High',
                                                  'Very High'),
                                 classes_colors = c('darkred','red', 'orange', 'yellow', 'lightgreen', 'mediumseagreen' ,'darkgreen'))
# filtering by suitability classes ----
for(i in 0:6){
data_filtered = data_res
pasture_filtered = pasture

# here, please select one chasse to be filtered (see in suitability classes or NA)
classes_filtered = i


if(is.na(classes_filtered)){
  print ('you are not removing classes from total pasture')
}else{
  if(classes_filtered == 0){
    print ('you are removing very low classes')
    data_filtered = data_res
    data_filtered[data_filtered <= classes_filtered ] = NA
    pasture_filtered[is.na(data_filtered)] <- NA
    total_pasture_area_remain = cellStats(pasture_filtered*area(pasture_filtered), sum)
    print (paste0('Your total pasture area after filtering is: ', 
                  round(total_pasture_area_remain/10^6,2),' million sq km or ', 
                  round(100*total_pasture_area_remain/total_pasture_area,1), '% of total global pasture remained'))  
  }else{
    if(classes_filtered == 1){
      print (paste0('you are removing classes below LOW suitability'))
      data_filtered = data_res
      data_filtered[data_filtered <= classes_filtered ] = NA
      pasture_filtered[is.na(data_filtered)] <- NA
      total_pasture_area_remain = cellStats(pasture_filtered*area(pasture_filtered), sum)
      print (paste0('Your total pasture area after filtering is: ', 
                    round(total_pasture_area_remain/10^6,2),' million sq km or ', 
                    round(100*total_pasture_area_remain/total_pasture_area,1), '% of total global pasture remained'))  
    }else{  
      if(classes_filtered == 2){
        print ('you are removing classes below medium-low suitability')
        data_filtered = data_res
        data_filtered[data_filtered <= classes_filtered ] = NA
        pasture_filtered[is.na(data_filtered)] <- NA
        total_pasture_area_remain = cellStats(pasture_filtered*area(pasture_filtered), sum)
        print (paste0('Your total pasture area after filtering is: ', 
                      round(total_pasture_area_remain/10^6,2),' million sq km or ', 
                      round(100*total_pasture_area_remain/total_pasture_area,1), '% of total global pasture remained')) 
      }else{  
        if(classes_filtered == 3){
          print ('you are removing classes below medium suitability')
          data_filtered = data_res
          data_filtered[data_filtered <= classes_filtered ] = NA
          pasture_filtered[is.na(data_filtered)] <- NA
          total_pasture_area_remain = cellStats(pasture_filtered*area(pasture_filtered), sum)
          print (paste0('Your total pasture area after filtering is: ', 
                        round(total_pasture_area_remain/10^6,2),' million sq km or ', 
                        round(100*total_pasture_area_remain/total_pasture_area,1), '% of total global pasture remained')) 
        }else{  
          if(classes_filtered == 4){
            print ('you are removing classes below medium-high suitability')
            data_filtered = data_res
            data_filtered[data_filtered <= classes_filtered ] = NA
            pasture_filtered[is.na(data_filtered)] <- NA
            total_pasture_area_remain = cellStats(pasture_filtered*area(pasture_filtered), sum)
            print (paste0('Your total pasture area after filtering is: ', 
                          round(total_pasture_area_remain/10^6,2),' million sq km or ', 
                          round(100*total_pasture_area_remain/total_pasture_area,1), '% of total global pasture remained')) 
          }else{
            if(classes_filtered == 5){
              print ('you are removing classes below high suitability')
              data_filtered = data_res
              data_filtered[data_filtered <= classes_filtered ] = NA
              pasture_filtered[is.na(data_filtered)] <- NA
              total_pasture_area_remain = cellStats(pasture_filtered*area(pasture_filtered), sum)
              print (paste0('Your total pasture area after filtering is: ', 
                            round(total_pasture_area_remain/10^6,2),' million sq km or ', 
                            round(100*total_pasture_area_remain/total_pasture_area,1), '% of total global pasture remained')) 
            }else{  
              print ('you are leaving only very-high suitability')
              data_filtered = data_res
              data_filtered[data_filtered != classes_filtered ] = NA
              pasture_filtered[is.na(data_filtered)] <- NA
              total_pasture_area_remain = cellStats(pasture_filtered*area(pasture_filtered), sum)
              print (paste0('Your total pasture area after filtering is: ', 
                            round(total_pasture_area_remain/10^6,2),' million sq km or ', 
                            round(100*total_pasture_area_remain/total_pasture_area,1), '% of total global pasture remained')) 
            }
          }
        }
      }
    }
  }
}

# mapping
if(is.na(classes_filtered)){excluding <- 'all_classes'}else{
  if(classes_filtered == 0){excluding <- 'excluding_uptoVERY-LOW'}else{
    if(classes_filtered == 1){excluding <- 'excluding_uptoLOW'}else{
      if(classes_filtered == 2){excluding <- 'excluding_uptoMEDIUM-LOW'}else{
        if(classes_filtered == 3){excluding <- 'excluding_uptoMEDIUM'}else{
          if(classes_filtered == 4){excluding <- 'excluding_uptoMEDIUM-HIGH'}else{
            if(classes_filtered == 5){excluding <- 'excluding_uptoHIGH'}else{
              excluding <- 'Only Very High classes'}}}}}}}

tiff(paste0(path, 'Maps/','26Feb - Global_pasture_lands_suitability_land_classes_',  land_class, ' -EXCLUDING - ',excluding, '.tif'),
      res = 300, compression = 'lzw', height = 3500, width = 5000, family = 'serif')
#x11()
#par(mar = c(10,1,3, 10))
#lay = layout(rbind(1,1), heights=c(7,1)) 
#layout.show(lay)
#par (mar = c(10, 2, 5,1))
#dev.off()
par(cex = 2)
world(fill = T, col = 'gray90')
if(is.na(classes_filtered)){
  title (paste0('Global pasture distribution according to suitability land classes \n (', land_class, ' input level)'))
}else{
  if(classes_filtered == 0){
    title(paste0('Global pasture distribution according to suitability land excluding "Very Low" classes \n (', land_class, ' input level)'))
    }else{
    if(classes_filtered == 1){
      title(paste0('Global pasture distribution according to suitability land excluding until "Low" classes \n (', land_class, ' input level)'))
    }else{
      if(classes_filtered == 2){
        title(paste0('Global pasture distribution according to suitability land excluding until "Medium Low" classes \n (', land_class, ' input level)'))
      }else{
        if(classes_filtered == 3){
          title(paste0('Global pasture distribution according to suitability land excluding until "Medium" classes \n (', land_class, ' input level)'))
        }else{
          if(classes_filtered == 4){
            title(paste0('Global pasture distribution according to suitability land excluding until "Medium High" classes \n (', land_class, ' input level)'))
          }else{
            title(paste0('Global pasture distribution according to suitability land excluding until "High" classes \n (', land_class, ' input level)'))
            }
          }
        }
      }
    }
  }


colors = paste(suitability_classes[,3])
legend_labels = paste(suitability_classes[,2])
           
#if(is.na(classes_filtered)){
#  colors = paste(suitability_classes[,3])
#  legend_labels = paste(suitability_classes[,2])
  
#}else{ 
#  if(is.na(classes_filtered)){
#    colors = paste(suitability_classes[,3])
#    legend_labels = paste(suitability_classes[,2])
#  }else{
#    if(classes_filtered == 0){
#      colors = paste(suitability_classes[-1,3])
#      legend_labels = paste(suitability_classes[-1,2])
#    }else{
#      colors = paste(suitability_classes[-seq(0,classes_filtered), 3])
#      legend_labels = paste(suitability_classes[-seq(0,classes_filtered), 2])  
#    }
#  }
#}

brks <- 0.5:7.5
par(cex=1.5)
image.plot(data_filtered,legend.mar = 9,#data_filtered[data_filtered == 6],
     col=colors, breaks = brks-1,xpd = T,
     horizontal = T,
     axes=FALSE,
     box=TRUE,
     legend.shrink = .5, add=T, legend.width = .5,
     axis.args = list(at = 0:6, labels = legend_labels, las = 2),
     legend.args = list(text = '',side = 3, font = 2, cex = 3))
world(add=T)
dev.off()


print (i)
}

