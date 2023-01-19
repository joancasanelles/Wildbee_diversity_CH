#######################################
### Paper: Wild bee taxonomic and functional metrics reveal a spatial mismatch between α- and ß-diversity in Switzerland
### Script to produce Figure S13-S14 Elevation vs. responses
### Author: Joan Casanelles-Abella & Bertrand Fournier
### Date: 19.01.2023
#######################################
### ===================================
###  Initialise the system
### ===================================
# Remove all R objects in the workspace
rm(list = ls())
setwd("input")
# Packages
require(raster)
require(viridis)
require(ggplot2)
library(sp)
library(sf)
### ===================================
###  Data
### ===================================
### load the data -----------------------------------------------------------------------
div <- stack("Diversity_stack_revised_Selected_descriptors.tif")
names(div) = c("belowgound","cleptoparasite","FDis", "feeding_specialization", 
               "FEve", "FRic", "InvSimpson", "ITD","LCBD_fun" ,"LCBD_taxo","phenoduration",
               "phenostart", "Rao", "Richness", "Shannon",  "Simpson",
               "solitary", "TED", "tong_length", "TOP")
## Community attributes
TOP <- div$TOP
TED <- div$TED
FDis <- div$FDis
rich <- div$Richness
shannon <- div$Shannon
LCBD_fun <- div$LCBD_fun*1000
LCBD_taxo<- div$LCBD_taxo*1000
## CWM traits
belowgound <- div$belowgound
cleptoparasite <- div$cleptoparasite
feeding_specialization <- div$feeding_specialization
phenoduration <- div$phenoduration
phenostart <- div$phenostart
ITD <- div$ITD
solitary <- div$solitary
tong_length<- div$tong_length
### water bodies
water_bodies=raster("water_bodies1.tif")
### Elevation
elevation = raster("dhm_25.tif")

### Extract community attributes
TOP.extr=data.frame(TOP=raster::extract(rasterstack.responses$TOP, coordinates(water_bodies)), coordinates(water_bodies))
TOP.extr=na.omit(TOP.extr)
TED.extr=data.frame(TED=raster::extract(rasterstack.responses$TED, coordinates(water_bodies)), coordinates(water_bodies))
TED.extr=na.omit(TED.extr)
FDis.extr=data.frame(FDis=raster::extract(rasterstack.responses$FDis, coordinates(water_bodies)), coordinates(water_bodies))
FDis.extr=na.omit(FDis.extr)
rich.extr=data.frame(rich=raster::extract(rasterstack.responses$rich, coordinates(water_bodies)), coordinates(water_bodies))
rich.extr=na.omit(rich.extr)
shannon.extr=data.frame(shannon=raster::extract(rasterstack.responses$shannon, coordinates(water_bodies)), coordinates(water_bodies))
shannon.extr=na.omit(shannon.extr)
LCBD_taxo.extr=data.frame(LCBD_taxo=raster::extract(rasterstack.responses$LCBD_taxo, coordinates(water_bodies)), coordinates(water_bodies))
LCBD_taxo.extr=na.omit(LCBD_taxo.extr)
LCBD_fun.extr=data.frame(LCBD_fun=raster::extract(rasterstack.responses$LCBD_fun, coordinates(water_bodies)), coordinates(water_bodies))
LCBD_fun.extr=na.omit(LCBD_fun.extr)
### Extract CWM traits
belowgound.extr=data.frame(belowgound=raster::extract(belowgound, coordinates(water_bodies)), coordinates(water_bodies))
belowgound.extr=na.omit(belowgound.extr)
cleptoparasite.extr=data.frame(cleptoparasite=raster::extract(cleptoparasite, coordinates(water_bodies)), coordinates(water_bodies))
cleptoparasite.extr=na.omit(cleptoparasite.extr)
feeding_specialization.extr=data.frame(feeding_specialization=raster::extract(feeding_specialization, coordinates(water_bodies)), coordinates(water_bodies))
feeding_specialization.extr=na.omit(feeding_specialization.extr)
phenoduration.extr=data.frame(phenoduration=raster::extract(phenoduration, coordinates(water_bodies)), coordinates(water_bodies))
phenoduration.extr=na.omit(phenoduration.extr)
phenostart.extr=data.frame(phenostart=raster::extract(phenostart, coordinates(water_bodies)), coordinates(water_bodies))
phenostart.extr=na.omit(phenostart.extr)
ITD.extr=data.frame(ITD=raster::extract(ITD, coordinates(water_bodies)), coordinates(water_bodies))
ITD.extr=na.omit(ITD.extr)
solitary.extr=data.frame(solitary=raster::extract(solitary, coordinates(water_bodies)), coordinates(water_bodies))
solitary.extr=na.omit(solitary.extr)
tong_length.extr=data.frame(tong_length=raster::extract(tong_length, coordinates(water_bodies)), coordinates(water_bodies))
tong_length.extr=na.omit(tong_length.extr)
### Extract elevation
elevation.extr=data.frame(elevation=raster::extract(elevation, coordinates(water_bodies)), coordinates(water_bodies))
elevation.extr=na.omit(elevation.extr)
elevation.extr$coordsmerge=paste(elevation.extr$x, elevation.extr$y)


list.responses=list(TOP.extr,TED.extr,FDis.extr,rich.extr,shannon.extr,LCBD_taxo.extr,LCBD_fun.extr)

for(r in 1:length(list.responses)){
  response.dat = list.responses[[r]]
  response.dat$coordsmerge=paste(response.dat$x, response.dat$y)
  response.dat.random=  response.dat[ response.dat$coordsmerge %in% random.coordsmerge,]
  response.dat.random.ele=merge(response.dat.random, elevation.extr.random, by=c("x", "y"))
  plot.elevation=ggplot(response.dat.random.ele, aes(x=elevation, y=get(colnames(response.dat.random.ele)[3]))) + 
    scale_fill_manual(values = "blue") +
    geom_point(alpha=0.1) +
    geom_smooth(aes(fill=paste(colnames(response.dat.random.ele)[3])),  method="loess") +
    theme_classic(base_size = 15) +
    scale_x_continuous(name="Elevation (m)", breaks = c(0, 1000, 2000, 3000), labels = c(0, 1000, 2000, 3000),limits=c(197, 3500)) +
    ylab(paste(colnames(response.dat.random.ele)[3])) + 
    theme(
      legend.title = element_blank(), legend.position = "none")
   ggsave(plot = plot.elevation, filename = paste("output/Elevation_", colnames(response.dat.random.ele)[3], ".png", sep=""), device = "png", width = 4, height = 4) 
}


list.traits=list(belowgound.extr,cleptoparasite.extr,feeding_specialization.extr,phenoduration.extr,phenostart.extr,ITD.extr,solitary.extr,tong_length.extr)

for(r in 1:length(list.traits)){
  response.dat = list.traits[[r]]
  response.dat$coordsmerge=paste(response.dat$x, response.dat$y)
  random.coordsmerge=sample(response.dat$coordsmerge, size = 10000)
  elevation.extr.random=elevation.extr[elevation.extr$coordsmerge %in% random.coordsmerge,]
  response.dat.random=  response.dat[ response.dat$coordsmerge %in% random.coordsmerge,]
  response.dat.random.ele=merge(response.dat.random, elevation.extr.random, by=c("x", "y"))
  plot.elevation=ggplot(response.dat.random.ele, aes(x=elevation, y=get(colnames(response.dat.random.ele)[3]))) + 
    scale_fill_manual(values = "blue") +
    theme_classic(base_size = 15) +
    geom_point(alpha=0.15) +
    geom_smooth(aes(fill=paste(colnames(response.dat.random.ele)[3])), method = "loess") +
    scale_x_continuous(name="Elevation (m)", breaks = c(0, 1000, 2000, 3000), labels = c(0, 1000, 2000, 3000),limits=c(197, 3500)) +
    ylab(paste(colnames(response.dat.random.ele)[3])) + 
    theme(
      legend.title = element_blank(), legend.position = "none")
  ggsave(plot = plot.elevation, filename = paste("output/Elevation_", colnames(response.dat.random.ele)[3], ".png", sep=""), device = "png", width = 4, height = 4) 
}