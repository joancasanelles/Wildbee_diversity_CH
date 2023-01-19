# Remove all R objects in the workspace
rm(list = ls())

# load packages
library(sdm)
library(raster)
library(rgdal)
library(viridis)
require(ggplotify)
require(caret)

### load the LC data -----------------------------------------------------------------------
setwd("input/")
urb_18 <- stack(x = "Urban_perc_18.tif")
names(urb_18) <- paste("Urb",c(200, 500, 1000, 2500),sep="_")
urb_18

LUI <- stack(x = "LUI_buffer.tif")
names(LUI) <- paste("LUI",c(200, 500, 1000, 2500),sep="_")
LUI$LUI_200[is.na(values(LUI$LUI_200))] = 0
LUI$LUI_200[is.na(values(urb_18$Urb_200))] = NA

LUI$LUI_500[is.na(values(LUI$LUI_500))] = 0
LUI$LUI_500[is.na(values(urb_18$Urb_200))] = NA

LUI$LUI_1000[is.na(values(LUI$LUI_1000))] = 0
LUI$LUI_1000[is.na(values(urb_18$Urb_200))] = NA

LUI$LUI_2500[is.na(values(LUI$LUI_2500))] = 0
LUI$LUI_2500[is.na(values(urb_18$Urb_200))] = NA

plot(LUI)

### load the bee hivedata
hive <- stack(x = "beehive-2012-2018.tif") 
names(hive) <- c(paste("hive_2012",c(200, 500, 1000, 2500),sep="_"), 
                 paste("hive_2018",c(200, 500, 1000, 2500),sep="_"))
hive <- stack(hive$hive_2018_200, hive$hive_2018_500, 
              hive$hive_2018_1000, hive$hive_2018_2500)

# replace NAs with 0
values(hive$hive_2018_200)[is.na(values(hive$hive_2018_200))] = 0 
hive$hive_2018_200[is.na(values(urb_18$Urb_200))] = NA
plot(hive$hive_2018_200, col=viridis(100))

values(hive$hive_2018_500)[is.na(values(hive$hive_2018_500))] = 0 
hive$hive_2018_500[is.na(values(urb_18$Urb_200))] = NA
plot(hive$hive_2018_500, col=viridis(100))

values(hive$hive_2018_1000)[is.na(values(hive$hive_2018_1000))] = 0 
hive$hive_2018_1000[is.na(values(urb_18$Urb_1000))] = NA
plot(hive$hive_2018_1000, col=viridis(100))

values(hive$hive_2018_2500)[is.na(values(hive$hive_2018_2500))] = 0 
hive$hive_2018_2500[is.na(values(urb_18$Urb_2500))] = NA
plot(hive$hive_2018_2500, col=viridis(100))


### Load climate data
clim <- stack(x = "Climate_PCA_CH_stack.tif") 
names(clim) <- c(paste("clim_PC", 1:4,sep=""))

### Load plant pca data
plant_19 <- stack(x = "Plant_PC_19_revised.tif") 
names(plant_19) <- c(paste("plant_PC", 1:4,sep=""))


### ------------------------------------------------------------
### prepare for MAchine learning
setwd("input/")
div.dat = read.delim("Diversity_data_revised.txt")
vec.sel = div.dat$Richness >= 5 & div.dat$Richness < quantile(div.dat$Richness, probs = 0.98)
div.dat=div.dat[vec.sel,]
coord = div.dat[,1:2]
div.dat=div.dat[,-c(1:2)]

# 2019
preds_19 <- stack(clim, urb_18, LUI, plant_19, hive)
names(preds_19) = c("clim_PC1", "clim_PC2", "clim_PC3", "clim_PC4", 
                    "urb_200", "urb_500", "urb_1000","urb_2500", 
                    "LUI_200", "LUI_500", "LUI_1000","LUI_2500", 
                    "plant_PC1", "plant_PC2", "plant_PC3", "plant_PC4",
                    "hive_200", "hive_500", "hive_1000", "hive_2500")
preds.df = as.data.frame(preds_19)
preds.df = na.omit(preds.df)

### make a data frame with all data used in Machine learning
preds.dat=extract(preds_19, coord) # Klima und LU Daten werden für Species Beprobungsorte(koordinaten) extrahiert
preds = as.data.frame(preds.dat)
div.dat = div.dat[-which(is.na(rowSums(preds.dat))),]
preds = na.omit(preds)

### parallel --------------------------------------------------------------
# setup parallel backend to use many processors
library(foreach)
library(doSNOW)
library(snow)

registerDoSEQ()
cl <- snow::makeCluster(3, type="SOCK")
registerDoSNOW(cl)
foreach::getDoParWorkers()

finalMatrix = data.frame(NULL)
finalMatrix <- foreach(k = 1:ncol(div.dat),.packages = c("caret")) %dopar% {
 
   ### Step 1: prepare data for machine learing analyses
   dat <- data.frame(resp = div.dat[,k], preds)
   dat=na.omit(dat)

   ### Step 2: Create the training and test datasets
   set.seed(1)
   trainIndex <- createDataPartition(dat$resp, p = .8, 
                                     list = FALSE, 
                                     times = 1)
   train <- dat[ trainIndex,]
   test  <- dat[-trainIndex,]
   
   #### Step 3: Train the model
   require(caretEnsemble)
   fitControl <- trainControl(## 10-fold CV
     method = "repeatedcv",
     number = 3,
     repeats = 3)
   
   model_list <- caretList(
     resp~., data=train,
     # preprocess = c("scale", "center"),
     methodList=c("rf", "nnet", "glm"),
     trControl=fitControl)
   
   ### Step 4: predict on the test data
   w = extractPrediction(model_list, testX = test)
   w.rf = w[w$model=="rf",]
   w.nnet = w[w$model=="nnet",]
   w.glm = w[w$model=="glm",]
   
   ### Step 5: evaluation metrics
   eval.rf = postResample(w.rf$pred, w.rf$obs)
   eval.glm = postResample(w.glm$pred, w.glm$obs)
   eval.nnet = postResample(w.nnet$pred, w.nnet$obs)
   res.temp <- as.data.frame(rbind(eval.rf, eval.nnet, eval.glm))
   res.temp$variable = rep(colnames(div.dat)[k],3)
   res.temp$model = c("rf", "nnet", "glm")
   
   # variable importance
   varimp.rf <- varImp(model_list$rf$finalModel, scale=T)
   varimp.nnet <- varImp(model_list$nnet$finalModel)
   varimp.glm <- varImp(model_list$glm$finalModel, scale=T)
   varimp.dat <- data.frame(var_rf = varimp.rf, var_nnet = varimp.nnet,
                            var_glm = varimp.glm)
   colnames(varimp.dat) = c("varimp_rf", "varimp_nnet", "varimp_glm")
   varimp.dat$variable= rep(colnames(div.dat)[k], 20)
   
   ###  Step 6: predictions for all Switzerland
   p = predict(model_list$rf$finalModel, preds.df,
               type = "response", na.rm = na.pas)
   ras.pred <- preds_19$clim_PC1  
   values(ras.pred) = NA
   ras.pred[as.numeric(rownames(preds.df))] = p
   
   ### Step 7: save the data
   setwd("~/Dropbox/Projects/City4Bees/Results_Diversity_Modelling")
   writeRaster(ras.pred, paste(colnames(div.dat)[k], ".tif", sep=""), overwrite=T)
   write.table(eval, paste(colnames(div.dat)[k], "_eval", ".txt", sep=""),sep="\t")
   write.table(varimp.dat, paste(colnames(div.dat)[k], "_varimp", ".txt", sep=""),sep="\t")
   
   res.final = list(res.temp, varimp.dat)
   return(res.final)
} 

snow::stopCluster(cl)


### reassemble data
eval.all <- as.data.frame(finalMatrix[[1]][[1]])
for(i in 2:ncol(div.dat)) eval.all = rbind(eval.all, finalMatrix[[i]][[1]])


varimp.all <- finalMatrix[[1]][[2]]
for(i in 2:ncol(div.dat)) varimp.all = rbind(varimp.all, finalMatrix[[i]][[2]])
varimp.all$model = rep(c("clim_PC1", "clim_PC2", "clim_PC3", "clim_PC4", 
                         "urb_200", "urb_500", "urb_1000","urb_2500", 
                         "LUI_200", "LUI_500", "LUI_1000","LUI_2500", 
                         "plant_PC1", "plant_PC2", "plant_PC3", "plant_PC4",
                         "hive_200", "hive_500", "hive_1000", "hive_2500"), 
                       ncol(div.dat))

setwd("output/")
write.table(eval.all, "Diversity_evaluation_metrics.txt",sep="\t")
write.table(varimp.all, "Diversity_VariableImportance_metrics.txt",sep="\t")


### make raster stack
setwd("output/")
rastlist <- list.files(path = "output/", 
                       pattern='.tif$', all.files=TRUE, full.names=FALSE)

ras_1 <- stack(rastlist)

plot_raster <- function(plotVar, nColor){
   library(raster)
   library(rasterVis)
   library(classInt)
   library(viridis)
   
   break1 <- classIntervals(plotVar[!is.na(plotVar)], 
                            n = nColor, 
                            style = "kmeans")
   lvp <- levelplot(plotVar, 
                    col.regions = cividis(nColor), 
                    at = break1$brks, margin = FALSE)
   return(lvp) 
}


p_TOP <- plot_raster(plotVar=ras_1$TOP, nColor=10)
p_TED <- plot_raster(plotVar=ras_1$TED, nColor=10)
p_FDis <- plot_raster(plotVar=ras_1$FDis, nColor=10)

p_ITD <- plot_raster(plotVar=ras_1$ITD, nColor=10)
p_below <- plot_raster(plotVar=ras_1$belowgound, nColor=10)
p_pdur <- plot_raster(plotVar=ras_1$phenoduration, nColor=10)
p_pstart <- plot_raster(plotVar=ras_1$phenostart, nColor=10)

require(cowplot)
plot_grid(p_TOP, p_TED, p_FDis, 
          labels = c('A', 'B', "C"), 
          nrow=2,
          align="hv",
          label_size = 10)


plot_grid(p_ITD, p_below, p_pdur, p_pstart,
          labels = c('A', 'B', "C", "D"), 
          nrow=2,
          align="hv",
          label_size = 10)



library(raster)
library(rasterVis)
library(classInt)
library(viridis)

plotVar <- ras_1$ITD

nColor <- 10
break1 <- classIntervals(plotVar[!is.na(plotVar)], 
                         n = nColor, 
                         style = "kmeans")
lvp <- levelplot(plotVar, 
                 col.regions = cividis(nColor), 
                 at = break1$brks, margin = FALSE)
lvp 



par(mfrow=c(3,2))
plot(ras_1$Richness, col=magma(100), main="Richness")
plot(ras_1$Shannon, col=magma(100), main="Shannon")
plot(ras_1$TOP, col=magma(100), main="TOP")
plot(ras_1$FDis, col=magma(100), main="FDis")
plot(ras_1$TED, col=magma(100), main="TED")

par(mfrow=c(4,2))
plot(ras_1$ITD, col=magma(100))
plot(ras_1$feeding_specialization, col=magma(100))
plot(ras_1$tong_length, col=magma(100))
plot(ras_1$solitary, col=magma(100))
plot(ras_1$cleptoparasite, col=magma(100))
plot(ras_1$phenoduration, col=magma(100))
plot(ras_1$phenostart, col=magma(100))

writeRaster(ras_1, "Diversity_stack.tif", overwrite=T)



