#######################################
### Paper: Wild bee diversity in Switzerland
### Script: Figure 3
### Author: Joan Casanelles-Abella & Bertrand Fournier
### Date: 11.2022
#######################################
### ===================================
###  Initialise the system
### ===================================
# Remove all R objects in the workspace
rm(list = ls())
setwd("PATH")
# Packages
library(raster)
library(viridis)
library(ggplot2)
library(scales) 
library(magrittr)
library(egg)
library(ggpubr)
library(ggplot2)
library(vegan)
### load the data -----------------------------------------------------------------------
pdp.TOP <- read.delim("DATA/OP_pdp.txt")
pdp.TOP$valueY=pdp.TOP$valueY/100
pdp.TED <- read.delim("DATA/TEd_pdp.txt")
pdp.FDis <- read.delim("DATA/FDis_pdp.txt")
pdp.rich <- read.delim("DATA/Richness_pdp.txt")
pdp.rich$valueY=pdp.rich$valueY/100
pdp.sha <- read.delim("DATA/Shannon_pdp.txt")
pdp.betataxo <- read.delim("DATA/LCBD_taxo_pdp.txt")
pdp.betataxo$valueY=pdp.betataxo$valueY*10000
pdp.betafun <- read.delim("DATA/LCBD_fun_pdp.txt")
pdp.betafun$valueY=pdp.betafun$valueY*1000
### ===================================
###  Ploz
### ===================================
## decimal places
scaleFUN <- function(x) sprintf("%.2f", x)
### PDPs plots hive
Make_PDP_plot_all <- function(dat, ylabel, labels){
  ## Climate
  palette_predictors_climate=c( "#1E1F26","#283655","#4D648D","#D0E1F9")
  var.list = c("clim_PC1","clim_PC2","clim_PC3","clim_PC4")
  dat.clim <- dat[dat$Var %in% var.list,]  
  
  p.climate <- ggplot(data = dat.clim, aes(y=valueY, x = valueX, 
                                         group = Var, colour = Var)) + 
    geom_smooth(method="lm", span = 1, aes(fill=Var)) +
    scale_color_manual(values =palette_predictors_climate) +
    scale_fill_manual(values = palette_predictors_climate) +
    scale_y_continuous(name = ylabel, breaks =labels , labels=scaleFUN, limits = c(min(labels), max(labels))) + 
    theme_classic(base_size = 20) +
    theme(legend.title = element_blank()) +
    xlab("")
  ## Vegetation
  palette_predictors_vegetation=c(  "#6fb98f","#4B7447","#7CAA2D")
  var.list = c( "plant_PC2", "plant_PC3", "plant_PC4")
  dat.veg <- dat[dat$Var %in% var.list,]  
  
  #for(i in var.list){ dat.veg$valueX[dat.veg$Var==i] = decostand(dat.veg$valueX[dat.veg$Var==i], "range") }
  
  p.vegetation <- ggplot(data = dat.veg, aes(y=valueY, x = valueX, 
                                      group = Var, colour = Var)) + 
    geom_smooth(method="lm", span = 1, aes(fill=Var)) +
    scale_color_manual(values =palette_predictors_vegetation) +
    scale_fill_manual(values = palette_predictors_vegetation) +
    scale_y_continuous(name = ylabel, breaks =labels , labels=scaleFUN, limits = c(min(labels), max(labels))) + 
    theme_classic(base_size = 20) +
    theme(legend.title = element_blank()) +
    xlab("")
  ## Land-use
  palette_predictors_landuse=c("#DC267F",  "#B38867",  "#CDCDC0")
  var.list = c( "agri2500", "forest2500", "urb2500")
  dat.lu <- dat[dat$Var %in% var.list,]  
  
 # for(i in var.list){ dat.lu$valueX[dat.lu$Var==i] = decostand(dat.lu$valueX[dat.lu$Var==i], "range")   }
  
  p.landuse <- ggplot(data = dat.lu, aes(y=valueY, x = valueX, 
                                         group = Var, colour = Var)) + 
    geom_smooth(method="lm", span = 1, aes(fill=Var)) +
    scale_color_manual(values =palette_predictors_landuse) +
    scale_fill_manual(values = palette_predictors_landuse) +
    scale_y_continuous(name = ylabel, breaks =labels , labels=scaleFUN, limits = c(min(labels), max(labels))) + 
    theme_classic(base_size = 20) +
    theme(legend.title = element_blank()) +
    xlab("")
  ## Beekeeping
  palette_predictors_hive=c( "#F4CC70")
  var.list = c("hive_2500")
  dat.be <- dat[dat$Var %in% var.list,]  
  
 # for(i in var.list){dat.be$valueX[dat.be$Var==i] = decostand(dat.be$valueX[dat.be$Var==i], "range") }
  
  p.beekeeping <- ggplot(data = dat.be, aes(y=valueY, x = valueX, 
                                  group = Var, colour = Var)) + 
    geom_smooth(method="lm", span = 1, aes(fill=Var)) +
    scale_color_manual(values =palette_predictors_hive) +
    scale_fill_manual(values = palette_predictors_hive) +
    scale_y_continuous(name = ylabel, breaks =labels , labels=scaleFUN, limits = c(min(labels), max(labels))) + 
    theme_classic(base_size = 20) +
    theme(legend.title = element_blank()) +
    xlab("")
  
  plot.arranged=ggarrange(p.climate,p.vegetation,p.landuse,p.beekeeping,
            nrow = 1, ncol=4)
  #return(plot.arranged)
  
}

p.TOP <- Make_PDP_plot_all(dat=pdp.TOP, ylabel="", labels=c(0.20,0.25,0.30, 0.35))
p.TED<- Make_PDP_plot_all(dat=pdp.TED, ylabel="", labels=c(0.92, 0.93, 0.94))
p.FDis <- Make_PDP_plot_all(dat=pdp.FDis, ylabel="", labels=c(1.20, 1.40, 1.60, 1.80))
p.rich <- Make_PDP_plot_all(dat=pdp.rich, ylabel="", labels=c(0.08,0.10,0.12,0.14,0.16,0.18))
p.sha<- Make_PDP_plot_all(dat=pdp.sha, ylabel="", labels=c(1.40,1.50,1.60,1.70,1.80))
p.betataxo<- Make_PDP_plot_all(dat=pdp.betataxo, ylabel="", labels=c(1.59, 1.61, 1.63, 1.65, 1.67))
p.beta.fun<- Make_PDP_plot_all(dat=pdp.betafun, ylabel="", labels=c(0.15, 0.20, 0.25, 0.30, 0.35))


figure.all <- ggpubr:: ggarrange(p.rich,p.sha,p.TOP,p.TED,p.FDis,p.betataxo,p.beta.fun,
                    nrow = 7, ncol=1) 
                    

figure.all %>% ggexport(filename = "Fig5_PDP_new.png",
                    width = 1500, height = 1200)

figure.all %>% ggexport(filename = "Fig5_PDP_new.pdf",
                    width = 21, height = 17)
