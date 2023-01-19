#######################################
### Paper: Wild bee taxonomic and functional metrics reveal a spatial mismatch between α- and ß-diversity in Switzerland
### Script to produce Figure S7: correlations predictors
### Author: Joan Casanelles-Abella & Bertrand Fournier
### Date: 19.01.2023
#######################################
# Remove all R objects in the workspace
rm(list = ls())

require(raster)
require(viridis)
require(ggplot2)

### load the data -----------------------------------------------------------------------
setwd("input/")

### All descriptors
dat.cor.des.all = sampleRandom(stack(clim, plant_19, LU_all, LUI, hive), 100000)
mat.cor.des.all = cor(dat.cor.des.all)

library(ggcorrplot)
p_cor <- ggcorrplot(round(mat.cor.des.all,1), 
                    hc.order = FALSE, 
                    show.legend=FALSE,
                    type = "lower",
                    lab = TRUE) +
  ggtitle("Correlation matrix")


p_cor %>% ggexport(filename = "output/Descriptors_correlation_matrix.png",
                    width = 700, height = 700)
p_cor %>% ggexport(filename = "output/Descriptors_correlation_matrix.pdf",
                    width = 7, height = 7)


### Selected descriptors
dat.cor.des.sel = sampleRandom(stack(clim, plant_19$plant_PC2, plant_19$plant_PC3, 
                                 plant_19$plant_PC4, LU_all$urb2500, 
                                 LU_all$agri2500, LU_all$forest2500, 
                                 LUI$LUI_2500, hive$hive_2018_2500), 100000)
mat.cor.des.sel = cor(dat.cor.des.sel)


p_cor <- ggcorrplot(round(mat.cor.des.sel,1), 
                    hc.order = FALSE, 
                    show.legend=FALSE,
                    type = "lower",
                    lab = TRUE) +
  ggtitle("Correlation matrix")


p_cor %>% ggexport(filename = "output/Descriptors_correlation_matrix.png",
                    width = 700, height = 700)
p_cor %>% ggexport(filename = "output/Descriptors_correlation_matrix.pdf",
                    width = 7, height = 7)
