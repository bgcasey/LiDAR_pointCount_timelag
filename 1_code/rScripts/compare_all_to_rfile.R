## ----setup, include=FALSE, cache=FALSE----------------------------------
#Set root directory to R project root
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())


## ----child = '1_code/rNotebooks/3_compare/compare_AMRE.Rmd'-------------

## ----include=FALSE, cache=FALSE-----------------------------------------
#Set root directory to R project root
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())


## # set max height of code chunks using the following css styling

## 
## pre {

##   max-height: 300px;

##   overflow-y: auto;

## }

## 
## pre[class] {

##   max-height: 500px;

## }


## ----message = FALSE----------------------------------------------------
library(dplyr)
library(ggplot2)
library(raster)
library(spatialEco)
library(sf)
library(purrr)
library(broom)
library(viridis)
library(ggeffects)

## -----------------------------------------------------------------------
load("2_pipeline/store/models/AMRE_models_AUC_4.rData")

AMRE_models_AUC_4<-AMRE_models_AUC_4%>%
  rename(LiDAR_timelag=lag)

ggplot(AMRE_models_AUC_4, aes(x=LiDAR_timelag, y=AUC_mean)) + 
  geom_errorbar(aes(ymin=AUC_mean-1.96*AUC_stdErr, ymax=AUC_mean+ 1.96*AUC_stdErr, width=.2))+
  geom_point(size=1) + 
  geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
  geom_smooth(method = lm)+
  theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none")
 ggsave(filename=paste0("3_output/figures/timelag_stats/AMRE_AUC_lm_test.png"), width =6, height=5)
  
AMRE_AUC_lm_test<-lm(AUC_mean~ LiDAR_timelag, data=AMRE_models_AUC_4)
save(AMRE_AUC_lm_test,file="2_pipeline/store/models/AMRE_AUC_lm_test.rData")

AUC_cor_test <- cor.test(AMRE_models_AUC_4$AUC_mean, AMRE_models_AUC_4$LiDAR_timelag, method = "pearson")

#########################

ggplot(AMRE_models_AUC_4, aes(x=LiDAR_timelag, y=R2m_mean)) + 
  geom_errorbar(aes(ymin=R2m_mean- 1.96* R2m_stdErr, ymax=R2m_mean+ 1.96* R2m_stdErr, width=.2))+
  geom_point() + 
  geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
  geom_smooth(method = lm) +
  theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none")
 ggsave(filename=paste0("3_output/figures/timelag_stats/AMRE_R2m_test.png"), width =6, height=5)
  
AMRE_R2m_test<-lm(R2m_mean~ LiDAR_timelag, data=AMRE_models_AUC_4)
save(AMRE_R2m_test,file="2_pipeline/store/models/AMRE_R2m_test.rData")

########################
ggplot(AMRE_models_AUC_4, aes(x=LiDAR_timelag, y=R2c_mean)) + 
  geom_errorbar(aes(ymin=R2c_mean- 1.96* R2c_stdErr, ymax=R2c_mean+ 1.96* R2c_stdErr, width=.2))+
  geom_point() + 
  geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
  geom_smooth(method = lm)+
  theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none")
 ggsave(filename=paste0("3_output/figures/timelag_stats/AMRE_R2c_test.png"), width =6, height=5)
  
AMRE_R2c_test<-lm(R2c_mean~ LiDAR_timelag, data=AMRE_models_AUC_4)
save(AMRE_R2c_test,file="2_pipeline/store/models/AMRE_R2c_test.rData")



## ----echo=FALSE, out.width = '100%', fig.cap="AMRE AUC time lag"--------
knitr::include_graphics("3_output/figures/timelag_stats/AMRE_AUC_lm_test.png", dpi = 300)


## ----echo=FALSE---------------------------------------------------------
load("2_pipeline/store/models/AMRE_AUC_lm_test.rData")
print(summary(AMRE_AUC_lm_test))


## ----echo=FALSE, out.width = '100%', fig.cap="AMRE R2m time lag"--------
knitr::include_graphics("3_output/figures/timelag_stats/AMRE_R2m_test.png", dpi = 300)


## ----echo=FALSE---------------------------------------------------------
load("2_pipeline/store/models/AMRE_R2m_test.rData")
print(summary(AMRE_R2m_test))


## ----echo=FALSE, out.width = '100%', fig.cap="AMRE R2c time lag"--------
knitr::include_graphics("3_output/figures/timelag_stats/AMRE_R2c_test.png", dpi = 300)


## ----echo=FALSE---------------------------------------------------------
load("2_pipeline/store/models/AMRE_R2c_test.rData")
print(summary(AMRE_R2c_test))


## -----------------------------------------------------------------------
# Load stack of predictive rasters
AMRE_stack<-stackOpen("3_output/maps/predictedDistributions/AMRE/AMRE_stack.stk")

# reclassify to a binary above and below 50% occupancy probability

reclass_df <- c(0, .5, 0,
              .5, 1, 1)

reclass_m <- matrix(reclass_df,
                ncol = 3,
                byrow = TRUE)

AMRE_stack_reclass <- reclassify(AMRE_stack,
                     reclass_m)

# save reclass raster brick (save as .grd to preserve layer names)
writeRaster(AMRE_stack_reclass, "3_output/maps/predictedDistributions/AMRE/AMRE_stack_reclass.grd", overwrite=TRUE, format="raster") 

# load reclass rasters as a brick
AMRE_stack_reclass<-brick("3_output/maps/predictedDistributions/AMRE/AMRE_stack_reclass.grd")



## -----------------------------------------------------------------------
AMRE_stack<-stackOpen("3_output/maps/predictedDistributions/AMRE/AMRE_stack.stk")

predCor_0_01<-rasterCorrelation(AMRE_stack$AMRE_predictMap_m_00, AMRE_stack$AMRE_predictMap_m_01, s = 3, type = "pearson")
predCor_0_02<-rasterCorrelation(AMRE_stack$AMRE_predictMap_m_00, AMRE_stack$AMRE_predictMap_m_02, s = 3, type = "pearson")
predCor_0_03<-rasterCorrelation(AMRE_stack$AMRE_predictMap_m_00, AMRE_stack$AMRE_predictMap_m_03, s = 3, type = "pearson")
predCor_0_04<-rasterCorrelation(AMRE_stack$AMRE_predictMap_m_00, AMRE_stack$AMRE_predictMap_m_04, s = 3, type = "pearson")
predCor_0_05<-rasterCorrelation(AMRE_stack$AMRE_predictMap_m_00, AMRE_stack$AMRE_predictMap_m_05, s = 3, type = "pearson")
predCor_0_06<-rasterCorrelation(AMRE_stack$AMRE_predictMap_m_00, AMRE_stack$AMRE_predictMap_m_06, s = 3, type = "pearson")
predCor_0_07<-rasterCorrelation(AMRE_stack$AMRE_predictMap_m_00, AMRE_stack$AMRE_predictMap_m_07, s = 3, type = "pearson")
predCor_0_08<-rasterCorrelation(AMRE_stack$AMRE_predictMap_m_00, AMRE_stack$AMRE_predictMap_m_08, s = 3, type = "pearson")
predCor_0_09<-rasterCorrelation(AMRE_stack$AMRE_predictMap_m_00, AMRE_stack$AMRE_predictMap_m_09, s = 3, type = "pearson")
predCor_0_10<-rasterCorrelation(AMRE_stack$AMRE_predictMap_m_00, AMRE_stack$AMRE_predictMap_m_10, s = 3, type = "pearson")
predCor_0_11<-rasterCorrelation(AMRE_stack$AMRE_predictMap_m_00, AMRE_stack$AMRE_predictMap_m_11, s = 3, type = "pearson")
predCor_0_12<-rasterCorrelation(AMRE_stack$AMRE_predictMap_m_00, AMRE_stack$AMRE_predictMap_m_12, s = 3, type = "pearson")
predCor_0_13<-rasterCorrelation(AMRE_stack$AMRE_predictMap_m_00, AMRE_stack$AMRE_predictMap_m_13, s = 3, type = "pearson")
predCor_0_14<-rasterCorrelation(AMRE_stack$AMRE_predictMap_m_00, AMRE_stack$AMRE_predictMap_m_14, s = 3, type = "pearson")
predCor_0_15<-rasterCorrelation(AMRE_stack$AMRE_predictMap_m_00, AMRE_stack$AMRE_predictMap_m_15, s = 3, type = "pearson")


writeRaster(predCor_0_01, file="3_output/maps/predictedDistributions/AMRE/predCor/predCor_0_01.tif", overwrite=TRUE)
writeRaster(predCor_0_02, file="3_output/maps/predictedDistributions/AMRE/predCor/predCor_0_02.tif", overwrite=TRUE)
writeRaster(predCor_0_03, file="3_output/maps/predictedDistributions/AMRE/predCor/predCor_0_03.tif", overwrite=TRUE)
writeRaster(predCor_0_04, file="3_output/maps/predictedDistributions/AMRE/predCor/predCor_0_04.tif", overwrite=TRUE)
writeRaster(predCor_0_05, file="3_output/maps/predictedDistributions/AMRE/predCor/predCor_0_05.tif", overwrite=TRUE)
writeRaster(predCor_0_06, file="3_output/maps/predictedDistributions/AMRE/predCor/predCor_0_06.tif", overwrite=TRUE)
writeRaster(predCor_0_07, file="3_output/maps/predictedDistributions/AMRE/predCor/predCor_0_07.tif", overwrite=TRUE)
writeRaster(predCor_0_08, file="3_output/maps/predictedDistributions/AMRE/predCor/predCor_0_08.tif", overwrite=TRUE)
writeRaster(predCor_0_09, file="3_output/maps/predictedDistributions/AMRE/predCor/predCor_0_09.tif", overwrite=TRUE)
writeRaster(predCor_0_10, file="3_output/maps/predictedDistributions/AMRE/predCor/predCor_0_10.tif", overwrite=TRUE)
writeRaster(predCor_0_11, file="3_output/maps/predictedDistributions/AMRE/predCor/predCor_0_11.tif", overwrite=TRUE)
writeRaster(predCor_0_12, file="3_output/maps/predictedDistributions/AMRE/predCor/predCor_0_12.tif", overwrite=TRUE)
writeRaster(predCor_0_13, file="3_output/maps/predictedDistributions/AMRE/predCor/predCor_0_13.tif", overwrite=TRUE)
writeRaster(predCor_0_14, file="3_output/maps/predictedDistributions/AMRE/predCor/predCor_0_14.tif", overwrite=TRUE)
writeRaster(predCor_0_15, file="3_output/maps/predictedDistributions/AMRE/predCor/predCor_0_15.tif", overwrite=TRUE)



map_list<-list.files(path='3_output/maps/predictedDistributions/AMRE/predCor', pattern = "*.tif$", full.names = TRUE)
AMRE_predCor_stack<-raster::stack(map_list)

#save stack as r object
stackSave(AMRE_predCor_stack,file="3_output/maps/predictedDistributions/AMRE/predCor/AMRE_predCor_stack.stk")



## -----------------------------------------------------------------------


AMRE_predCor_stack<-stackOpen("3_output/maps/predictedDistributions/AMRE/predCor/AMRE_predCor_stack.stk")

lidar_forestAge_raster<-raster("0_data/manual/spatialCov/CASFRI/lidar_forestAge_raster.tif")
ageClass_00<-raster("0_data/manual/spatialCov/CASFRI/ageClassRasters/ageClass_00.tif")

lidar_forestAge_raster_pr<-projectRaster(lidar_forestAge_raster, crs=AMRE_predCor_stack)
ageClass_00_pr<-projectRaster(ageClass_00, method='ngb',crs=AMRE_predCor_stack)

#study area
load("0_data/manual/bird/studyarea_big.rData")

pc<- raster::crop(AMRE_predCor_stack, as_Spatial(st_geometry(c_bb)))
fa<- raster::crop(lidar_forestAge_raster_pr, as_Spatial(st_geometry(c_bb)))
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))
#resample so they have the same resolution
pc_rs<-resample(pc, fa)

# Stack covariates
fa_co_ac <- stack(pc_rs, fa, ac)

AMRE_fa_co_ac_df<-as.data.frame(na.omit(values(fa_co_ac)))%>%
  dplyr::rename(forest_age=lidar_forestAge_raster)%>%
  dplyr::rename(forest_age_class=ageClass_00)

AMRE_fa_co_ac_df$forest_age_class<-ordered(as.factor(AMRE_fa_co_ac_df$forest_age_class))

save(AMRE_fa_co_ac_df, file="2_pipeline/store/AMRE_fa_co_ac_df.rData")

load("2_pipeline/store/AMRE_fa_co_ac_df.rData")


#######################################################################################
############### Plot Effect ######################################################
#######################################################################################
lm_fa_00to01 <- lm(predCor_0_01 ~ forest_age, data=AMRE_fa_co_ac_df)
t<-ggpredict(lm_fa_00to01, terms="forest_age [all]", data=AMRE_fa_co_ac_df)
png("3_output/figures/timelag_stats/AMRE_lm_fa_00to01.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to01 <- lm(predCor_0_01 ~ forest_age_class, data=AMRE_fa_co_ac_df)
ggplot(AMRE_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_01) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/AMRE_lm_fac_00to01.png"), width =8, height=5)

#######################################################################################

lm_fa_00to02 <- lm(predCor_0_02 ~ forest_age, data=AMRE_fa_co_ac_df)
t<-ggpredict(lm_fa_00to02, terms="forest_age [all]", data=AMRE_fa_co_ac_df)
png("3_output/figures/timelag_stats/AMRE_lm_fa_00to02.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# age class
lm_fac_00to02 <- lm(predCor_0_02 ~ forest_age_class, data=AMRE_fa_co_ac_df)
ggplot(AMRE_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_02) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
ggsave(filename=paste0("3_output/figures/timelag_stats/AMRE_lm_fac_00to02.png"), width =8, height=5)  
  
#######################################################################################
lm_fa_00to03 <- lm(predCor_0_03 ~ forest_age, data=AMRE_fa_co_ac_df)
t<-ggpredict(lm_fa_00to03, terms="forest_age [all]", data=AMRE_fa_co_ac_df)
png("3_output/figures/timelag_stats/AMRE_lm_fa_00to03.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to03 <- lm(predCor_0_03 ~ forest_age_class, data=AMRE_fa_co_ac_df)
ggplot(AMRE_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_03) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/AMRE_lm_fac_00to03.png"), width =8, height=5)


#######################################################################################
lm_fa_00to04 <- lm(predCor_0_04 ~ forest_age, data=AMRE_fa_co_ac_df)
t<-ggpredict(lm_fa_00to04, terms="forest_age [all]", data=AMRE_fa_co_ac_df)
png("3_output/figures/timelag_stats/AMRE_lm_fa_00to04.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to04 <- lm(predCor_0_04 ~ forest_age_class, data=AMRE_fa_co_ac_df)
ggplot(AMRE_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_04) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())  
  ggsave(filename=paste0("3_output/figures/timelag_stats/AMRE_lm_fac_00to04.png"), width =8, height=5)    
  
#######################################################################################
lm_fa_00to05 <- lm(predCor_0_05 ~ forest_age, data=AMRE_fa_co_ac_df)
t<-ggpredict(lm_fa_00to05, terms="forest_age [all]", data=AMRE_fa_co_ac_df)
png("3_output/figures/timelag_stats/AMRE_lm_fa_00to05.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to05 <- lm(predCor_0_05 ~ forest_age_class, data=AMRE_fa_co_ac_df)
ggplot(AMRE_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_05) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/AMRE_lm_fac_00to05.png"), width =8, height=5)
  
  
  
#######################################################################################
lm_fa_00to06 <- lm(predCor_0_06 ~ forest_age, data=AMRE_fa_co_ac_df)
t<-ggpredict(lm_fa_00to06, terms="forest_age [all]", data=AMRE_fa_co_ac_df)
png("3_output/figures/timelag_stats/AMRE_lm_fa_00to06.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to06 <- lm(predCor_0_06 ~ forest_age_class, data=AMRE_fa_co_ac_df)
ggplot(AMRE_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_06) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/AMRE_lm_fac_00to06.png"), width =8, height=5)
  
  
  
  
  
  
  #######################################################################################
lm_fa_00to07 <- lm(predCor_0_07 ~ forest_age, data=AMRE_fa_co_ac_df)
t<-ggpredict(lm_fa_00to07, terms="forest_age [all]", data=AMRE_fa_co_ac_df)
png("3_output/figures/timelag_stats/AMRE_lm_fa_00to07.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to07 <- lm(predCor_0_07 ~ forest_age_class, data=AMRE_fa_co_ac_df)
ggplot(AMRE_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_07) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/AMRE_lm_fac_00to07.png"), width =8, height=5)
  
  
  
  
  
  #######################################################################################
lm_fa_00to08 <- lm(predCor_0_08 ~ forest_age, data=AMRE_fa_co_ac_df)
t<-ggpredict(lm_fa_00to08, terms="forest_age [all]", data=AMRE_fa_co_ac_df)
png("3_output/figures/timelag_stats/AMRE_lm_fa_00to08.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to08 <- lm(predCor_0_08 ~ forest_age_class, data=AMRE_fa_co_ac_df)
ggplot(AMRE_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_08) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/AMRE_lm_fac_00to08.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to09 <- lm(predCor_0_09 ~ forest_age, data=AMRE_fa_co_ac_df)
t<-ggpredict(lm_fa_00to09, terms="forest_age [all]", data=AMRE_fa_co_ac_df)
png("3_output/figures/timelag_stats/AMRE_lm_fa_00to09.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to09 <- lm(predCor_0_09 ~ forest_age_class, data=AMRE_fa_co_ac_df)
ggplot(AMRE_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_09) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/AMRE_lm_fac_00to09.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to10 <- lm(predCor_0_10 ~ forest_age, data=AMRE_fa_co_ac_df)
t<-ggpredict(lm_fa_00to10, terms="forest_age [all]", data=AMRE_fa_co_ac_df)
png("3_output/figures/timelag_stats/AMRE_lm_fa_00to10.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to10 <- lm(predCor_0_10 ~ forest_age_class, data=AMRE_fa_co_ac_df)
ggplot(AMRE_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_10) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/AMRE_lm_fac_00to10.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to11 <- lm(predCor_0_11 ~ forest_age, data=AMRE_fa_co_ac_df)
t<-ggpredict(lm_fa_00to11, terms="forest_age [all]", data=AMRE_fa_co_ac_df)
png("3_output/figures/timelag_stats/AMRE_lm_fa_00to11.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to11 <- lm(predCor_0_11 ~ forest_age_class, data=AMRE_fa_co_ac_df)
ggplot(AMRE_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_11) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/AMRE_lm_fac_00to11.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to12 <- lm(predCor_0_12 ~ forest_age, data=AMRE_fa_co_ac_df)
t<-ggpredict(lm_fa_00to12, terms="forest_age [all]", data=AMRE_fa_co_ac_df)
png("3_output/figures/timelag_stats/AMRE_lm_fa_00to12.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to12 <- lm(predCor_0_12 ~ forest_age_class, data=AMRE_fa_co_ac_df)
ggplot(AMRE_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_12) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/AMRE_lm_fac_00to12.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to13 <- lm(predCor_0_13 ~ forest_age, data=AMRE_fa_co_ac_df)
t<-ggpredict(lm_fa_00to13, terms="forest_age [all]", data=AMRE_fa_co_ac_df)
png("3_output/figures/timelag_stats/AMRE_lm_fa_00to13.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to13 <- lm(predCor_0_13 ~ forest_age_class, data=AMRE_fa_co_ac_df)
ggplot(AMRE_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_13) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/AMRE_lm_fac_00to13.png"), width =8, height=5)

  
  
  #######################################################################################
lm_fa_00to14 <- lm(predCor_0_14 ~ forest_age, data=AMRE_fa_co_ac_df)
t<-ggpredict(lm_fa_00to14, terms="forest_age [all]", data=AMRE_fa_co_ac_df)
png("3_output/figures/timelag_stats/AMRE_lm_fa_00to14.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to14 <- lm(predCor_0_14 ~ forest_age_class, data=AMRE_fa_co_ac_df)
ggplot(AMRE_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_14) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/AMRE_lm_fac_00to14.png"), width =8, height=5)
  
  
  
  #######################################################################################
lm_fa_00to15 <- lm(predCor_0_15 ~ forest_age, data=AMRE_fa_co_ac_df)
t<-ggpredict(lm_fa_00to15, terms="forest_age [all]", data=AMRE_fa_co_ac_df)
png("3_output/figures/timelag_stats/AMRE_lm_fa_00to15.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to15 <- lm(predCor_0_15 ~ forest_age_class, data=AMRE_fa_co_ac_df)
ggplot(AMRE_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_15) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/AMRE_lm_fac_00to15.png"), width =8, height=5)
  

#######################################################################################
#### save models
#######################################################################################

# save models

# forest age
AMRE_predCor_to_forestAge <-list(lm_fa_00to01, lm_fa_00to02, lm_fa_00to03, lm_fa_00to04, lm_fa_00to05, lm_fa_00to06, lm_fa_00to07, lm_fa_00to08, lm_fa_00to09, lm_fa_00to10, lm_fa_00to11, lm_fa_00to12, lm_fa_00to13, lm_fa_00to14, lm_fa_00to15)
names(AMRE_predCor_to_forestAge) <- c("lm_fa_00to01", "lm_fa_00to02", "lm_fa_00to03", "lm_fa_00to04", "lm_fa_00to05", "lm_fa_00to06", "lm_fa_00to07", "lm_fa_00to08", "lm_fa_00to09", "lm_fa_00to10", "lm_fa_00to11", "lm_fa_00to12", "lm_fa_00to13", "lm_fa_00to14", "lm_fa_00to15")
save(AMRE_predCor_to_forestAge, file="2_pipeline/store/models/AMRE_predCor_to_forestAge.rData")




AMRE_predCor_to_forestAge_df<-purrr::map_df(AMRE_predCor_to_forestAge, broom::glance, .id = 'formula')
save(AMRE_predCor_to_forestAge_df, file="2_pipeline/store/models/AMRE_predCor_to_forestAge_df.rData")

# forest age class
AMRE_predCor_to_forestAgeClass<-list(lm_fac_00to01, lm_fac_00to02, lm_fac_00to03, lm_fac_00to04, lm_fac_00to05, lm_fac_00to06, lm_fac_00to07, lm_fac_00to08, lm_fac_00to09, lm_fac_00to10, lm_fac_00to11, lm_fac_00to12, lm_fac_00to13, lm_fac_00to14, lm_fac_00to15)
names(AMRE_predCor_to_forestAgeClass) <- c("lm_fac_00to01", "lm_fac_00to02", "lm_fac_00to03", "lm_fac_00to04", "lm_fac_00to05", "lm_fac_00to06", "lm_fac_00to07", "lm_fac_00to08", "lm_fac_00to09", "lm_fac_00to10", "lm_fac_00to11", "lm_fac_00to12", "lm_fac_00to13", "lm_fac_00to14", "lm_fac_00to15")
save(AMRE_predCor_to_forestAgeClass, file="2_pipeline/store/models/AMRE_predCor_to_forestAgeClass.rData")




AMRE_predCor_to_forestAgeClass_df<-purrr::map_df(AMRE_predCor_to_forestAgeClass, broom::glance, .id = 'formula')
save(AMRE_predCor_to_forestAgeClass_df, file="2_pipeline/store/models/AMRE_predCor_to_forestAgeClass_df.rData")


#######################################################################################
#### Plot all on the same plot

ggplot() +
  geom_smooth(aes(x = forest_age, y = predCor_0_02, col = "0 and 02 years"), data = AMRE_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_04, col = "0 and 04 years"), data = AMRE_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_06, col = "0 and 06 years"), data = AMRE_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_08, col = "0 and 08 years"), data = AMRE_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_10, col = "0 and 10 years"), data = AMRE_fa_co_df, 
              method = "lm", se = FALSE)+
    scale_colour_manual(name="", 
                      values = c("#fa9fb5","#f768a1","#dd3497","#ae017e","#7a0177")) +
  labs(
    x = "Forest age when LiDAR was acquired", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  ggsave(filename=paste0("3_output/figures/timelag_stats/AMRE_lm_fa_all.png"), width =8, height=5)
  
  
  
ggplot() +
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_01, col = "0 and 01 years"), data = AMRE_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_02, col = "0 and 02 years"), data = AMRE_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_03, col = "0 and 03 years"), data = AMRE_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_04, col = "0 and 04 years"), data = AMRE_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_05, col = "0 and 05 years"), data = AMRE_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_06, col = "0 and 06 years"), data = AMRE_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_07, col = "0 and 07 years"), data = AMRE_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_08, col = "0 and 08 years"), data = AMRE_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_09, col = "0 and 09 years"), data = AMRE_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_10, col = "0 and 10 years"), data = AMRE_fa_co_ac_df, 
              method = "lm", se = FALSE)+
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_11, col = "0 and 11 years"), data = AMRE_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_12, col = "0 and 12 years"), data = AMRE_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_13, col = "0 and 13 years"), data = AMRE_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_14, col = "0 and 14 years"), data = AMRE_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_15, col = "0 and 15 years"), data = AMRE_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  scale_colour_viridis_d()+  
  # scale_colour_manual(name="", 
    #                   values = c("#fa9fb5","#f768a1","#dd3497","#ae017e","#7a0177")) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  ggsave(filename=paste0("3_output/figures/timelag_stats/AMRE_lm_fac_all.png"), width =8, height=5)
  


## ----echo=FALSE, out.width = '100%'-------------------------------------
knitr::include_graphics("3_output/figures/timelag_stats/AMRE_lm_fa_all.png", dpi = 300)


## ----echo=FALSE, message = FALSE, results="asis"------------------------
load("2_pipeline/store/models/AMRE_predCor_to_forestAge_df.rData")

knitr::kable(AMRE_predCor_to_forestAge_df) %>%
  kable_styling(font_size = 10, bootstrap_options= c("striped", "hover", "condensed" ), full_width= F, position="center") %>%
  column_spec(1, width= "20em")%>%
  scroll_box(width = "100%", height = "400px")


## -----------------------------------------------------------------------


AMRE_predCor_stack<-stackOpen("3_output/maps/predictedDistributions/AMRE/predCor/AMRE_predCor_stack.stk")

lidar_forestAge_raster<-raster("0_data/manual/spatialCov/CASFRI/lidar_forestAge_raster.tif")
ageClass_00<-raster("0_data/manual/spatialCov/CASFRI/ageClassRasters/ageClass_00.tif")

lidar_forestAge_raster_pr<-projectRaster(lidar_forestAge_raster, crs=AMRE_predCor_stack)
ageClass_00_pr<-projectRaster(ageClass_00, method='ngb',crs=AMRE_predCor_stack)

#study area
load("0_data/manual/bird/studyarea_big.rData")

pc<- raster::crop(AMRE_predCor_stack, as_Spatial(st_geometry(c_bb)))
fa<- raster::crop(lidar_forestAge_raster_pr, as_Spatial(st_geometry(c_bb)))
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))
#resample so they have the same resolution

pc_rs<-resample(pc, fa)

# Stack covariates
fa_co_ac <- stack(pc_rs, fa, ac)

AMRE_fa_co_ac_df<-as.data.frame(na.omit(values(fa_co_ac)))%>%
  dplyr::rename(forest_age=lidar_forestAge_raster)%>%
  dplyr::rename(forest_age_class=ageClass_00)

AMRE_fa_co_ac_df$forest_age_class<-ordered(as.factor(AMRE_fa_co_ac_df$forest_age_class))

save(AMRE_fa_co_ac_df, file="2_pipeline/store/AMRE_fa_co_ac_df.rData")

load("2_pipeline/store/AMRE_fa_co_ac_df.rData")



AMRE_stack<-stackOpen("3_output/maps/predictedDistributions/AMRE/AMRE_stack.stk")
lidar_forestAge_raster<-raster("0_data/manual/spatialCov/CASFRI/lidar_forestAge_raster.tif")
ageClass_00<-raster("0_data/manual/spatialCov/CASFRI/ageClassRasters/ageClass_00.tif")



lidar_forestAge_raster_pr<-projectRaster(lidar_forestAge_raster, crs=AMRE_stack)
ageClass_00_pr<-projectRaster(ageClass_00, method='ngb',crs=AMRE_stack)

#study area
load("0_data/manual/bird/studyarea_big.rData")
 
pd<- raster::crop(AMRE_stack, as_Spatial(st_geometry(c_bb))) 
fa<- raster::crop(lidar_forestAge_raster_pr, as_Spatial(st_geometry(c_bb))) 
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))

#resample so they have the same resolution
pd_rs<-resample(pd, fa)

# Stack covariates
pd_fa <- stack(pd_rs, fa, ac)

AMRE_pd_fa_df<-as.data.frame(na.omit(values(pd_fa)))%>%
  dplyr::rename(forest_age=lidar_forestAge_raster)
          
save(AMRE_pd_fa_df, file="2_pipeline/store/AMRE_pd_fa_df.rData")

load("2_pipeline/store/AMRE_pd_fa_df.rData")




AMRE_pd_fa_df %>%
  ggplot(aes(AMRE_predictMap_m_00, AMRE_predictMap_m_15, color=forest_age)) +
  geom_point(alpha=0.4, size=.1) +
  scale_color_viridis() +
  labs(y="occupancy probability (15 year time lag)", x="occupancy probability (no time lag)", color="forest age (years)") +
   theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
ggsave(filename=paste0("3_output/figures/timelag_stats/AMRE_scatter_00to15.png"), width = 7, height=5)



AMRE_pd_fa_df %>%
  ggplot(aes(AMRE_predictMap_m_00, AMRE_predictMap_m_15, color=ageClass_00)) +
  geom_point(alpha=0.4, size=.1) +
  scale_color_viridis() +
  labs(y="occupancy probability (15 year time lag)", x="occupancy probability (no time lag)", color="forest age class") +
   theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
ggsave(filename=paste0("3_output/figures/timelag_stats/AMRE_scatter_00to15_class.png"), width = 7, height=5)


## ----echo=FALSE, out.width = '100%'-------------------------------------
knitr::include_graphics("3_output/figures/timelag_stats/AMRE_scatter_00to15.png", dpi = 300)


## ----echo=FALSE, out.width = '100%'-------------------------------------
knitr::include_graphics("3_output/figures/timelag_stats/AMRE_scatter_00to15_class.png", dpi = 300)



## ----child = '1_code/rNotebooks/3_compare/compare_BTNW.Rmd'-------------

## ----include=FALSE, cache=FALSE-----------------------------------------
#Set root directory to R project root
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())


## # set max height of code chunks using the following css styling

## 
## pre {

##   max-height: 300px;

##   overflow-y: auto;

## }

## 
## pre[class] {

##   max-height: 500px;

## }


## ---- message = FALSE---------------------------------------------------


## -----------------------------------------------------------------------
load("2_pipeline/store/models/BTNW_models_AUC_4.rData")

BTNW_models_AUC_4<-BTNW_models_AUC_4%>%
  rename(LiDAR_timelag=lag)

ggplot(BTNW_models_AUC_4, aes(x=LiDAR_timelag, y=AUC_mean)) + 
  geom_errorbar(aes(ymin=AUC_mean-1.96*AUC_stdErr, ymax=AUC_mean+ 1.96*AUC_stdErr, width=.2))+
  geom_point(size=1) + 
  geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
  geom_smooth(method = lm)+
  theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none")
 ggsave(filename=paste0("3_output/figures/timelag_stats/BTNW_AUC_lm_test.png"), width =6, height=5)
  
BTNW_AUC_lm_test<-lm(AUC_mean~ LiDAR_timelag, data=BTNW_models_AUC_4)
save(BTNW_AUC_lm_test,file="2_pipeline/store/models/BTNW_AUC_lm_test.rData")

AUC_cor_test <- cor.test(BTNW_models_AUC_4$AUC_mean, BTNW_models_AUC_4$LiDAR_timelag, method = "pearson")

#########################

ggplot(BTNW_models_AUC_4, aes(x=LiDAR_timelag, y=R2m_mean)) + 
  geom_errorbar(aes(ymin=R2m_mean- 1.96* R2m_stdErr, ymax=R2m_mean+ 1.96* R2m_stdErr, width=.2))+
  geom_point() + 
  geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
  geom_smooth(method = lm) +
  theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none")
 ggsave(filename=paste0("3_output/figures/timelag_stats/BTNW_R2m_test.png"), width =6, height=5)
  
BTNW_R2m_test<-lm(R2m_mean~ LiDAR_timelag, data=BTNW_models_AUC_4)
save(BTNW_R2m_test,file="2_pipeline/store/models/BTNW_R2m_test.rData")

########################
ggplot(BTNW_models_AUC_4, aes(x=LiDAR_timelag, y=R2c_mean)) + 
  geom_errorbar(aes(ymin=R2c_mean- 1.96* R2c_stdErr, ymax=R2c_mean+ 1.96* R2c_stdErr, width=.2))+
  geom_point() + 
  geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
  geom_smooth(method = lm)+
  theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none")
 ggsave(filename=paste0("3_output/figures/timelag_stats/BTNW_R2c_test.png"), width =6, height=5)
  
BTNW_R2c_test<-lm(R2c_mean~ LiDAR_timelag, data=BTNW_models_AUC_4)
save(BTNW_R2c_test,file="2_pipeline/store/models/BTNW_R2c_test.rData")



## ----echo=FALSE, out.width = '100%', fig.cap="BTNW AUC time lag"--------
knitr::include_graphics("3_output/figures/timelag_stats/BTNW_AUC_lm_test.png", dpi = 300)


## ----echo=FALSE---------------------------------------------------------
load("2_pipeline/store/models/BTNW_AUC_lm_test.rData")
print(summary(BTNW_AUC_lm_test))


## ----echo=FALSE, out.width = '100%', fig.cap="BTNW R2m time lag"--------
knitr::include_graphics("3_output/figures/timelag_stats/BTNW_R2m_test.png", dpi = 300)


## ----echo=FALSE---------------------------------------------------------
load("2_pipeline/store/models/BTNW_R2m_test.rData")
print(summary(BTNW_R2m_test))


## ----echo=FALSE, out.width = '100%', fig.cap="BTNW R2c time lag"--------
knitr::include_graphics("3_output/figures/timelag_stats/BTNW_R2c_test.png", dpi = 300)


## ----echo=FALSE---------------------------------------------------------
load("2_pipeline/store/models/BTNW_R2c_test.rData")
print(summary(BTNW_R2c_test))


## -----------------------------------------------------------------------
# Load stack of predictive rasters
BTNW_stack<-stackOpen("3_output/maps/predictedDistributions/BTNW/BTNW_stack.stk")

# reclassify to a binary above and below 50% occupancy probability

reclass_df <- c(0, .5, 0,
              .5, 1, 1)

reclass_m <- matrix(reclass_df,
                ncol = 3,
                byrow = TRUE)

BTNW_stack_reclass <- reclassify(BTNW_stack,
                     reclass_m)

# save reclass raster brick (save as .grd to preserve layer names)
writeRaster(BTNW_stack_reclass, "3_output/maps/predictedDistributions/BTNW/BTNW_stack_reclass.grd", overwrite=TRUE, format="raster") 

# load reclass rasters as a brick
BTNW_stack_reclass<-brick("3_output/maps/predictedDistributions/BTNW/BTNW_stack_reclass.grd")



## -----------------------------------------------------------------------
BTNW_stack<-stackOpen("3_output/maps/predictedDistributions/BTNW/BTNW_stack.stk")

predCor_0_01<-rasterCorrelation(BTNW_stack$BTNW_predictMap_m_00, BTNW_stack$BTNW_predictMap_m_01, s = 3, type = "pearson")
predCor_0_02<-rasterCorrelation(BTNW_stack$BTNW_predictMap_m_00, BTNW_stack$BTNW_predictMap_m_02, s = 3, type = "pearson")
predCor_0_03<-rasterCorrelation(BTNW_stack$BTNW_predictMap_m_00, BTNW_stack$BTNW_predictMap_m_03, s = 3, type = "pearson")
predCor_0_04<-rasterCorrelation(BTNW_stack$BTNW_predictMap_m_00, BTNW_stack$BTNW_predictMap_m_04, s = 3, type = "pearson")
predCor_0_05<-rasterCorrelation(BTNW_stack$BTNW_predictMap_m_00, BTNW_stack$BTNW_predictMap_m_05, s = 3, type = "pearson")
predCor_0_06<-rasterCorrelation(BTNW_stack$BTNW_predictMap_m_00, BTNW_stack$BTNW_predictMap_m_06, s = 3, type = "pearson")
predCor_0_07<-rasterCorrelation(BTNW_stack$BTNW_predictMap_m_00, BTNW_stack$BTNW_predictMap_m_07, s = 3, type = "pearson")
predCor_0_08<-rasterCorrelation(BTNW_stack$BTNW_predictMap_m_00, BTNW_stack$BTNW_predictMap_m_08, s = 3, type = "pearson")
predCor_0_09<-rasterCorrelation(BTNW_stack$BTNW_predictMap_m_00, BTNW_stack$BTNW_predictMap_m_09, s = 3, type = "pearson")
predCor_0_10<-rasterCorrelation(BTNW_stack$BTNW_predictMap_m_00, BTNW_stack$BTNW_predictMap_m_10, s = 3, type = "pearson")
predCor_0_11<-rasterCorrelation(BTNW_stack$BTNW_predictMap_m_00, BTNW_stack$BTNW_predictMap_m_11, s = 3, type = "pearson")
predCor_0_12<-rasterCorrelation(BTNW_stack$BTNW_predictMap_m_00, BTNW_stack$BTNW_predictMap_m_12, s = 3, type = "pearson")
predCor_0_13<-rasterCorrelation(BTNW_stack$BTNW_predictMap_m_00, BTNW_stack$BTNW_predictMap_m_13, s = 3, type = "pearson")
predCor_0_14<-rasterCorrelation(BTNW_stack$BTNW_predictMap_m_00, BTNW_stack$BTNW_predictMap_m_14, s = 3, type = "pearson")
predCor_0_15<-rasterCorrelation(BTNW_stack$BTNW_predictMap_m_00, BTNW_stack$BTNW_predictMap_m_15, s = 3, type = "pearson")


writeRaster(predCor_0_01, file="3_output/maps/predictedDistributions/BTNW/predCor/predCor_0_01.tif", overwrite=TRUE)
writeRaster(predCor_0_02, file="3_output/maps/predictedDistributions/BTNW/predCor/predCor_0_02.tif", overwrite=TRUE)
writeRaster(predCor_0_03, file="3_output/maps/predictedDistributions/BTNW/predCor/predCor_0_03.tif", overwrite=TRUE)
writeRaster(predCor_0_04, file="3_output/maps/predictedDistributions/BTNW/predCor/predCor_0_04.tif", overwrite=TRUE)
writeRaster(predCor_0_05, file="3_output/maps/predictedDistributions/BTNW/predCor/predCor_0_05.tif", overwrite=TRUE)
writeRaster(predCor_0_06, file="3_output/maps/predictedDistributions/BTNW/predCor/predCor_0_06.tif", overwrite=TRUE)
writeRaster(predCor_0_07, file="3_output/maps/predictedDistributions/BTNW/predCor/predCor_0_07.tif", overwrite=TRUE)
writeRaster(predCor_0_08, file="3_output/maps/predictedDistributions/BTNW/predCor/predCor_0_08.tif", overwrite=TRUE)
writeRaster(predCor_0_09, file="3_output/maps/predictedDistributions/BTNW/predCor/predCor_0_09.tif", overwrite=TRUE)
writeRaster(predCor_0_10, file="3_output/maps/predictedDistributions/BTNW/predCor/predCor_0_10.tif", overwrite=TRUE)
writeRaster(predCor_0_11, file="3_output/maps/predictedDistributions/BTNW/predCor/predCor_0_11.tif", overwrite=TRUE)
writeRaster(predCor_0_12, file="3_output/maps/predictedDistributions/BTNW/predCor/predCor_0_12.tif", overwrite=TRUE)
writeRaster(predCor_0_13, file="3_output/maps/predictedDistributions/BTNW/predCor/predCor_0_13.tif", overwrite=TRUE)
writeRaster(predCor_0_14, file="3_output/maps/predictedDistributions/BTNW/predCor/predCor_0_14.tif", overwrite=TRUE)
writeRaster(predCor_0_15, file="3_output/maps/predictedDistributions/BTNW/predCor/predCor_0_15.tif", overwrite=TRUE)



map_list<-list.files(path='3_output/maps/predictedDistributions/BTNW/predCor', pattern = "*.tif$", full.names = TRUE)
BTNW_predCor_stack<-raster::stack(map_list)

#save stack as r object
stackSave(BTNW_predCor_stack,file="3_output/maps/predictedDistributions/BTNW/predCor/BTNW_predCor_stack.stk")



## -----------------------------------------------------------------------


BTNW_predCor_stack<-stackOpen("3_output/maps/predictedDistributions/BTNW/predCor/BTNW_predCor_stack.stk")

lidar_forestAge_raster<-raster("0_data/manual/spatialCov/CASFRI/lidar_forestAge_raster.tif")
ageClass_00<-raster("0_data/manual/spatialCov/CASFRI/ageClassRasters/ageClass_00.tif")

lidar_forestAge_raster_pr<-projectRaster(lidar_forestAge_raster, crs=BTNW_predCor_stack)
ageClass_00_pr<-projectRaster(ageClass_00, method='ngb',crs=BTNW_predCor_stack)

#study area
load("0_data/manual/bird/studyarea_big.rData")

pc<- raster::crop(BTNW_predCor_stack, as_Spatial(st_geometry(c_bb)))
fa<- raster::crop(lidar_forestAge_raster_pr, as_Spatial(st_geometry(c_bb)))
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))
#resample so they have the same resolution
pc_rs<-resample(pc, fa)

# Stack covariates
fa_co_ac <- stack(pc_rs, fa, ac)

BTNW_fa_co_ac_df<-as.data.frame(na.omit(values(fa_co_ac)))%>%
  dplyr::rename(forest_age=lidar_forestAge_raster)%>%
  dplyr::rename(forest_age_class=ageClass_00)

BTNW_fa_co_ac_df$forest_age_class<-ordered(as.factor(BTNW_fa_co_ac_df$forest_age_class))

save(BTNW_fa_co_ac_df, file="2_pipeline/store/BTNW_fa_co_ac_df.rData")

load("2_pipeline/store/BTNW_fa_co_ac_df.rData")


#######################################################################################
############### Plot Effect ######################################################
#######################################################################################
lm_fa_00to01 <- lm(predCor_0_01 ~ forest_age, data=BTNW_fa_co_ac_df)
t<-ggpredict(lm_fa_00to01, terms="forest_age [all]", data=BTNW_fa_co_ac_df)
png("3_output/figures/timelag_stats/BTNW_lm_fa_00to01.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to01 <- lm(predCor_0_01 ~ forest_age_class, data=BTNW_fa_co_ac_df)
ggplot(BTNW_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_01) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/BTNW_lm_fac_00to01.png"), width =8, height=5)

#######################################################################################

lm_fa_00to02 <- lm(predCor_0_02 ~ forest_age, data=BTNW_fa_co_ac_df)
t<-ggpredict(lm_fa_00to02, terms="forest_age [all]", data=BTNW_fa_co_ac_df)
png("3_output/figures/timelag_stats/BTNW_lm_fa_00to02.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# age class
lm_fac_00to02 <- lm(predCor_0_02 ~ forest_age_class, data=BTNW_fa_co_ac_df)
ggplot(BTNW_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_02) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
ggsave(filename=paste0("3_output/figures/timelag_stats/BTNW_lm_fac_00to02.png"), width =8, height=5)  
  
#######################################################################################
lm_fa_00to03 <- lm(predCor_0_03 ~ forest_age, data=BTNW_fa_co_ac_df)
t<-ggpredict(lm_fa_00to03, terms="forest_age [all]", data=BTNW_fa_co_ac_df)
png("3_output/figures/timelag_stats/BTNW_lm_fa_00to03.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to03 <- lm(predCor_0_03 ~ forest_age_class, data=BTNW_fa_co_ac_df)
ggplot(BTNW_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_03) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/BTNW_lm_fac_00to03.png"), width =8, height=5)


#######################################################################################
lm_fa_00to04 <- lm(predCor_0_04 ~ forest_age, data=BTNW_fa_co_ac_df)
t<-ggpredict(lm_fa_00to04, terms="forest_age [all]", data=BTNW_fa_co_ac_df)
png("3_output/figures/timelag_stats/BTNW_lm_fa_00to04.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to04 <- lm(predCor_0_04 ~ forest_age_class, data=BTNW_fa_co_ac_df)
ggplot(BTNW_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_04) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())  
  ggsave(filename=paste0("3_output/figures/timelag_stats/BTNW_lm_fac_00to04.png"), width =8, height=5)    
  
#######################################################################################
lm_fa_00to05 <- lm(predCor_0_05 ~ forest_age, data=BTNW_fa_co_ac_df)
t<-ggpredict(lm_fa_00to05, terms="forest_age [all]", data=BTNW_fa_co_ac_df)
png("3_output/figures/timelag_stats/BTNW_lm_fa_00to05.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to05 <- lm(predCor_0_05 ~ forest_age_class, data=BTNW_fa_co_ac_df)
ggplot(BTNW_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_05) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/BTNW_lm_fac_00to05.png"), width =8, height=5)
  
  
  
#######################################################################################
lm_fa_00to06 <- lm(predCor_0_06 ~ forest_age, data=BTNW_fa_co_ac_df)
t<-ggpredict(lm_fa_00to06, terms="forest_age [all]", data=BTNW_fa_co_ac_df)
png("3_output/figures/timelag_stats/BTNW_lm_fa_00to06.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to06 <- lm(predCor_0_06 ~ forest_age_class, data=BTNW_fa_co_ac_df)
ggplot(BTNW_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_06) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/BTNW_lm_fac_00to06.png"), width =8, height=5)
  
  
  
  
  
  
  #######################################################################################
lm_fa_00to07 <- lm(predCor_0_07 ~ forest_age, data=BTNW_fa_co_ac_df)
t<-ggpredict(lm_fa_00to07, terms="forest_age [all]", data=BTNW_fa_co_ac_df)
png("3_output/figures/timelag_stats/BTNW_lm_fa_00to07.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to07 <- lm(predCor_0_07 ~ forest_age_class, data=BTNW_fa_co_ac_df)
ggplot(BTNW_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_07) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/BTNW_lm_fac_00to07.png"), width =8, height=5)
  
  
  
  
  
  #######################################################################################
lm_fa_00to08 <- lm(predCor_0_08 ~ forest_age, data=BTNW_fa_co_ac_df)
t<-ggpredict(lm_fa_00to08, terms="forest_age [all]", data=BTNW_fa_co_ac_df)
png("3_output/figures/timelag_stats/BTNW_lm_fa_00to08.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to08 <- lm(predCor_0_08 ~ forest_age_class, data=BTNW_fa_co_ac_df)
ggplot(BTNW_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_08) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/BTNW_lm_fac_00to08.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to09 <- lm(predCor_0_09 ~ forest_age, data=BTNW_fa_co_ac_df)
t<-ggpredict(lm_fa_00to09, terms="forest_age [all]", data=BTNW_fa_co_ac_df)
png("3_output/figures/timelag_stats/BTNW_lm_fa_00to09.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to09 <- lm(predCor_0_09 ~ forest_age_class, data=BTNW_fa_co_ac_df)
ggplot(BTNW_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_09) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/BTNW_lm_fac_00to09.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to10 <- lm(predCor_0_10 ~ forest_age, data=BTNW_fa_co_ac_df)
t<-ggpredict(lm_fa_00to10, terms="forest_age [all]", data=BTNW_fa_co_ac_df)
png("3_output/figures/timelag_stats/BTNW_lm_fa_00to10.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to10 <- lm(predCor_0_10 ~ forest_age_class, data=BTNW_fa_co_ac_df)
ggplot(BTNW_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_10) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/BTNW_lm_fac_00to10.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to11 <- lm(predCor_0_11 ~ forest_age, data=BTNW_fa_co_ac_df)
t<-ggpredict(lm_fa_00to11, terms="forest_age [all]", data=BTNW_fa_co_ac_df)
png("3_output/figures/timelag_stats/BTNW_lm_fa_00to11.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to11 <- lm(predCor_0_11 ~ forest_age_class, data=BTNW_fa_co_ac_df)
ggplot(BTNW_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_11) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/BTNW_lm_fac_00to11.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to12 <- lm(predCor_0_12 ~ forest_age, data=BTNW_fa_co_ac_df)
t<-ggpredict(lm_fa_00to12, terms="forest_age [all]", data=BTNW_fa_co_ac_df)
png("3_output/figures/timelag_stats/BTNW_lm_fa_00to12.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to12 <- lm(predCor_0_12 ~ forest_age_class, data=BTNW_fa_co_ac_df)
ggplot(BTNW_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_12) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/BTNW_lm_fac_00to12.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to13 <- lm(predCor_0_13 ~ forest_age, data=BTNW_fa_co_ac_df)
t<-ggpredict(lm_fa_00to13, terms="forest_age [all]", data=BTNW_fa_co_ac_df)
png("3_output/figures/timelag_stats/BTNW_lm_fa_00to13.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to13 <- lm(predCor_0_13 ~ forest_age_class, data=BTNW_fa_co_ac_df)
ggplot(BTNW_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_13) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/BTNW_lm_fac_00to13.png"), width =8, height=5)

  
  
  #######################################################################################
lm_fa_00to14 <- lm(predCor_0_14 ~ forest_age, data=BTNW_fa_co_ac_df)
t<-ggpredict(lm_fa_00to14, terms="forest_age [all]", data=BTNW_fa_co_ac_df)
png("3_output/figures/timelag_stats/BTNW_lm_fa_00to14.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to14 <- lm(predCor_0_14 ~ forest_age_class, data=BTNW_fa_co_ac_df)
ggplot(BTNW_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_14) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/BTNW_lm_fac_00to14.png"), width =8, height=5)
  
  
  
  #######################################################################################
lm_fa_00to15 <- lm(predCor_0_15 ~ forest_age, data=BTNW_fa_co_ac_df)
t<-ggpredict(lm_fa_00to15, terms="forest_age [all]", data=BTNW_fa_co_ac_df)
png("3_output/figures/timelag_stats/BTNW_lm_fa_00to15.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to15 <- lm(predCor_0_15 ~ forest_age_class, data=BTNW_fa_co_ac_df)
ggplot(BTNW_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_15) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/BTNW_lm_fac_00to15.png"), width =8, height=5)
  

#######################################################################################
#### save models
#######################################################################################

# save models

# forest age
BTNW_predCor_to_forestAge <-list(lm_fa_00to01, lm_fa_00to02, lm_fa_00to03, lm_fa_00to04, lm_fa_00to05, lm_fa_00to06, lm_fa_00to07, lm_fa_00to08, lm_fa_00to09, lm_fa_00to10, lm_fa_00to11, lm_fa_00to12, lm_fa_00to13, lm_fa_00to14, lm_fa_00to15)
names(BTNW_predCor_to_forestAge) <- c("lm_fa_00to01", "lm_fa_00to02", "lm_fa_00to03", "lm_fa_00to04", "lm_fa_00to05", "lm_fa_00to06", "lm_fa_00to07", "lm_fa_00to08", "lm_fa_00to09", "lm_fa_00to10", "lm_fa_00to11", "lm_fa_00to12", "lm_fa_00to13", "lm_fa_00to14", "lm_fa_00to15")
save(BTNW_predCor_to_forestAge, file="2_pipeline/store/models/BTNW_predCor_to_forestAge.rData")




BTNW_predCor_to_forestAge_df<-purrr::map_df(BTNW_predCor_to_forestAge, broom::glance, .id = 'formula')
save(BTNW_predCor_to_forestAge_df, file="2_pipeline/store/models/BTNW_predCor_to_forestAge_df.rData")

# forest age class
BTNW_predCor_to_forestAgeClass<-list(lm_fac_00to01, lm_fac_00to02, lm_fac_00to03, lm_fac_00to04, lm_fac_00to05, lm_fac_00to06, lm_fac_00to07, lm_fac_00to08, lm_fac_00to09, lm_fac_00to10, lm_fac_00to11, lm_fac_00to12, lm_fac_00to13, lm_fac_00to14, lm_fac_00to15)
names(BTNW_predCor_to_forestAgeClass) <- c("lm_fac_00to01", "lm_fac_00to02", "lm_fac_00to03", "lm_fac_00to04", "lm_fac_00to05", "lm_fac_00to06", "lm_fac_00to07", "lm_fac_00to08", "lm_fac_00to09", "lm_fac_00to10", "lm_fac_00to11", "lm_fac_00to12", "lm_fac_00to13", "lm_fac_00to14", "lm_fac_00to15")
save(BTNW_predCor_to_forestAgeClass, file="2_pipeline/store/models/BTNW_predCor_to_forestAgeClass.rData")




BTNW_predCor_to_forestAgeClass_df<-purrr::map_df(BTNW_predCor_to_forestAgeClass, broom::glance, .id = 'formula')
save(BTNW_predCor_to_forestAgeClass_df, file="2_pipeline/store/models/BTNW_predCor_to_forestAgeClass_df.rData")


#######################################################################################
#### Plot all on the same plot

ggplot() +
  geom_smooth(aes(x = forest_age, y = predCor_0_02, col = "0 and 02 years"), data = BTNW_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_04, col = "0 and 04 years"), data = BTNW_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_06, col = "0 and 06 years"), data = BTNW_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_08, col = "0 and 08 years"), data = BTNW_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_10, col = "0 and 10 years"), data = BTNW_fa_co_df, 
              method = "lm", se = FALSE)+
    scale_colour_manual(name="", 
                      values = c("#fa9fb5","#f768a1","#dd3497","#ae017e","#7a0177")) +
  labs(
    x = "Forest age when LiDAR was acquired", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  ggsave(filename=paste0("3_output/figures/timelag_stats/BTNW_lm_fa_all.png"), width =8, height=5)
  
  
  
ggplot() +
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_01, col = "0 and 01 years"), data = BTNW_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_02, col = "0 and 02 years"), data = BTNW_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_03, col = "0 and 03 years"), data = BTNW_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_04, col = "0 and 04 years"), data = BTNW_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_05, col = "0 and 05 years"), data = BTNW_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_06, col = "0 and 06 years"), data = BTNW_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_07, col = "0 and 07 years"), data = BTNW_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_08, col = "0 and 08 years"), data = BTNW_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_09, col = "0 and 09 years"), data = BTNW_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_10, col = "0 and 10 years"), data = BTNW_fa_co_ac_df, 
              method = "lm", se = FALSE)+
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_11, col = "0 and 11 years"), data = BTNW_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_12, col = "0 and 12 years"), data = BTNW_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_13, col = "0 and 13 years"), data = BTNW_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_14, col = "0 and 14 years"), data = BTNW_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_15, col = "0 and 15 years"), data = BTNW_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  scale_colour_viridis_d()+  
  # scale_colour_manual(name="", 
    #                   values = c("#fa9fb5","#f768a1","#dd3497","#ae017e","#7a0177")) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  ggsave(filename=paste0("3_output/figures/timelag_stats/BTNW_lm_fac_all.png"), width =8, height=5)
  


## ----echo=FALSE, out.width = '100%'-------------------------------------
knitr::include_graphics("3_output/figures/timelag_stats/BTNW_lm_fa_all.png", dpi = 300)


## ----echo=FALSE, message = FALSE, results="asis"------------------------
load("2_pipeline/store/models/BTNW_predCor_to_forestAge_df.rData")

knitr::kable(BTNW_predCor_to_forestAge_df) %>%
  kable_styling(font_size = 10, bootstrap_options= c("striped", "hover", "condensed" ), full_width= F, position="center") %>%
  column_spec(1, width= "20em")%>%
  scroll_box(width = "100%", height = "400px")


## -----------------------------------------------------------------------


BTNW_predCor_stack<-stackOpen("3_output/maps/predictedDistributions/BTNW/predCor/BTNW_predCor_stack.stk")

lidar_forestAge_raster<-raster("0_data/manual/spatialCov/CASFRI/lidar_forestAge_raster.tif")
ageClass_00<-raster("0_data/manual/spatialCov/CASFRI/ageClassRasters/ageClass_00.tif")

lidar_forestAge_raster_pr<-projectRaster(lidar_forestAge_raster, crs=BTNW_predCor_stack)
ageClass_00_pr<-projectRaster(ageClass_00, method='ngb',crs=BTNW_predCor_stack)

#study area
load("0_data/manual/bird/studyarea_big.rData")

pc<- raster::crop(BTNW_predCor_stack, as_Spatial(st_geometry(c_bb)))
fa<- raster::crop(lidar_forestAge_raster_pr, as_Spatial(st_geometry(c_bb)))
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))
#resample so they have the same resolution

pc_rs<-resample(pc, fa)

# Stack covariates
fa_co_ac <- stack(pc_rs, fa, ac)

BTNW_fa_co_ac_df<-as.data.frame(na.omit(values(fa_co_ac)))%>%
  dplyr::rename(forest_age=lidar_forestAge_raster)%>%
  dplyr::rename(forest_age_class=ageClass_00)

BTNW_fa_co_ac_df$forest_age_class<-ordered(as.factor(BTNW_fa_co_ac_df$forest_age_class))

save(BTNW_fa_co_ac_df, file="2_pipeline/store/BTNW_fa_co_ac_df.rData")

load("2_pipeline/store/BTNW_fa_co_ac_df.rData")



BTNW_stack<-stackOpen("3_output/maps/predictedDistributions/BTNW/BTNW_stack.stk")
lidar_forestAge_raster<-raster("0_data/manual/spatialCov/CASFRI/lidar_forestAge_raster.tif")
ageClass_00<-raster("0_data/manual/spatialCov/CASFRI/ageClassRasters/ageClass_00.tif")



lidar_forestAge_raster_pr<-projectRaster(lidar_forestAge_raster, crs=BTNW_stack)
ageClass_00_pr<-projectRaster(ageClass_00, method='ngb',crs=BTNW_stack)

#study area
load("0_data/manual/bird/studyarea_big.rData")
 
pd<- raster::crop(BTNW_stack, as_Spatial(st_geometry(c_bb))) 
fa<- raster::crop(lidar_forestAge_raster_pr, as_Spatial(st_geometry(c_bb))) 
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))

#resample so they have the same resolution
pd_rs<-resample(pd, fa)

# Stack covariates
pd_fa <- stack(pd_rs, fa, ac)

BTNW_pd_fa_df<-as.data.frame(na.omit(values(pd_fa)))%>%
  dplyr::rename(forest_age=lidar_forestAge_raster)
          
save(BTNW_pd_fa_df, file="2_pipeline/store/BTNW_pd_fa_df.rData")

load("2_pipeline/store/BTNW_pd_fa_df.rData")




BTNW_pd_fa_df %>%
  ggplot(aes(BTNW_predictMap_m_00, BTNW_predictMap_m_15, color=forest_age)) +
  geom_point(alpha=0.4, size=.1) +
  scale_color_viridis() +
  labs(y="occupancy probability (15 year time lag)", x="occupancy probability (no time lag)", color="forest age (years)") +
   theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
ggsave(filename=paste0("3_output/figures/timelag_stats/BTNW_scatter_00to15.png"), width = 7, height=5)



BTNW_pd_fa_df %>%
  ggplot(aes(BTNW_predictMap_m_00, BTNW_predictMap_m_15, color=ageClass_00)) +
  geom_point(alpha=0.4, size=.1) +
  scale_color_viridis() +
  labs(y="occupancy probability (15 year time lag)", x="occupancy probability (no time lag)", color="forest age class") +
   theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
ggsave(filename=paste0("3_output/figures/timelag_stats/BTNW_scatter_00to15_class.png"), width = 7, height=5)


## ----echo=FALSE, out.width = '100%'-------------------------------------
knitr::include_graphics("3_output/figures/timelag_stats/BTNW_scatter_00to15.png", dpi = 300)


## ----echo=FALSE, out.width = '100%'-------------------------------------
knitr::include_graphics("3_output/figures/timelag_stats/BTNW_scatter_00to15_class.png", dpi = 300)



## ----child = '1_code/rNotebooks/3_compare/compare_MOWA.Rmd'-------------

## ----include=FALSE, cache=FALSE-----------------------------------------
#Set root directory to R project root
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())


## # set max height of code chunks using the following css styling

## 
## pre {

##   max-height: 300px;

##   overflow-y: auto;

## }

## 
## pre[class] {

##   max-height: 500px;

## }


## ---- message = FALSE---------------------------------------------------


## -----------------------------------------------------------------------
load("2_pipeline/store/models/MOWA_models_AUC_4.rData")

MOWA_models_AUC_4<-MOWA_models_AUC_4%>%
  rename(LiDAR_timelag=lag)

ggplot(MOWA_models_AUC_4, aes(x=LiDAR_timelag, y=AUC_mean)) + 
  geom_errorbar(aes(ymin=AUC_mean-1.96*AUC_stdErr, ymax=AUC_mean+ 1.96*AUC_stdErr, width=.2))+
  geom_point(size=1) + 
  geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
  geom_smooth(method = lm)+
  theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none")
 ggsave(filename=paste0("3_output/figures/timelag_stats/MOWA_AUC_lm_test.png"), width =6, height=5)
  
MOWA_AUC_lm_test<-lm(AUC_mean~ LiDAR_timelag, data=MOWA_models_AUC_4)
save(MOWA_AUC_lm_test,file="2_pipeline/store/models/MOWA_AUC_lm_test.rData")

AUC_cor_test <- cor.test(MOWA_models_AUC_4$AUC_mean, MOWA_models_AUC_4$LiDAR_timelag, method = "pearson")

#########################

ggplot(MOWA_models_AUC_4, aes(x=LiDAR_timelag, y=R2m_mean)) + 
  geom_errorbar(aes(ymin=R2m_mean- 1.96* R2m_stdErr, ymax=R2m_mean+ 1.96* R2m_stdErr, width=.2))+
  geom_point() + 
  geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
  geom_smooth(method = lm) +
  theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none")
 ggsave(filename=paste0("3_output/figures/timelag_stats/MOWA_R2m_test.png"), width =6, height=5)
  
MOWA_R2m_test<-lm(R2m_mean~ LiDAR_timelag, data=MOWA_models_AUC_4)
save(MOWA_R2m_test,file="2_pipeline/store/models/MOWA_R2m_test.rData")

########################
ggplot(MOWA_models_AUC_4, aes(x=LiDAR_timelag, y=R2c_mean)) + 
  geom_errorbar(aes(ymin=R2c_mean- 1.96* R2c_stdErr, ymax=R2c_mean+ 1.96* R2c_stdErr, width=.2))+
  geom_point() + 
  geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
  geom_smooth(method = lm)+
  theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none")
 ggsave(filename=paste0("3_output/figures/timelag_stats/MOWA_R2c_test.png"), width =6, height=5)
  
MOWA_R2c_test<-lm(R2c_mean~ LiDAR_timelag, data=MOWA_models_AUC_4)
save(MOWA_R2c_test,file="2_pipeline/store/models/MOWA_R2c_test.rData")



## ----echo=FALSE, out.width = '100%', fig.cap="MOWA AUC time lag"--------
knitr::include_graphics("3_output/figures/timelag_stats/MOWA_AUC_lm_test.png", dpi = 300)


## ----echo=FALSE---------------------------------------------------------
load("2_pipeline/store/models/MOWA_AUC_lm_test.rData")
print(summary(MOWA_AUC_lm_test))


## ----echo=FALSE, out.width = '100%', fig.cap="MOWA R2m time lag"--------
knitr::include_graphics("3_output/figures/timelag_stats/MOWA_R2m_test.png", dpi = 300)


## ----echo=FALSE---------------------------------------------------------
load("2_pipeline/store/models/MOWA_R2m_test.rData")
print(summary(MOWA_R2m_test))


## ----echo=FALSE, out.width = '100%', fig.cap="MOWA R2c time lag"--------
knitr::include_graphics("3_output/figures/timelag_stats/MOWA_R2c_test.png", dpi = 300)


## ----echo=FALSE---------------------------------------------------------
load("2_pipeline/store/models/MOWA_R2c_test.rData")
print(summary(MOWA_R2c_test))


## -----------------------------------------------------------------------
# Load stack of predictive rasters
MOWA_stack<-stackOpen("3_output/maps/predictedDistributions/MOWA/MOWA_stack.stk")

# reclassify to a binary above and below 50% occupancy probability

reclass_df <- c(0, .5, 0,
              .5, 1, 1)

reclass_m <- matrix(reclass_df,
                ncol = 3,
                byrow = TRUE)

MOWA_stack_reclass <- reclassify(MOWA_stack,
                     reclass_m)

# save reclass raster brick (save as .grd to preserve layer names)
writeRaster(MOWA_stack_reclass, "3_output/maps/predictedDistributions/MOWA/MOWA_stack_reclass.grd", overwrite=TRUE, format="raster") 

# load reclass rasters as a brick
MOWA_stack_reclass<-brick("3_output/maps/predictedDistributions/MOWA/MOWA_stack_reclass.grd")



## -----------------------------------------------------------------------
MOWA_stack<-stackOpen("3_output/maps/predictedDistributions/MOWA/MOWA_stack.stk")

predCor_0_01<-rasterCorrelation(MOWA_stack$MOWA_predictMap_m_00, MOWA_stack$MOWA_predictMap_m_01, s = 3, type = "pearson")
predCor_0_02<-rasterCorrelation(MOWA_stack$MOWA_predictMap_m_00, MOWA_stack$MOWA_predictMap_m_02, s = 3, type = "pearson")
predCor_0_03<-rasterCorrelation(MOWA_stack$MOWA_predictMap_m_00, MOWA_stack$MOWA_predictMap_m_03, s = 3, type = "pearson")
predCor_0_04<-rasterCorrelation(MOWA_stack$MOWA_predictMap_m_00, MOWA_stack$MOWA_predictMap_m_04, s = 3, type = "pearson")
predCor_0_05<-rasterCorrelation(MOWA_stack$MOWA_predictMap_m_00, MOWA_stack$MOWA_predictMap_m_05, s = 3, type = "pearson")
predCor_0_06<-rasterCorrelation(MOWA_stack$MOWA_predictMap_m_00, MOWA_stack$MOWA_predictMap_m_06, s = 3, type = "pearson")
predCor_0_07<-rasterCorrelation(MOWA_stack$MOWA_predictMap_m_00, MOWA_stack$MOWA_predictMap_m_07, s = 3, type = "pearson")
predCor_0_08<-rasterCorrelation(MOWA_stack$MOWA_predictMap_m_00, MOWA_stack$MOWA_predictMap_m_08, s = 3, type = "pearson")
predCor_0_09<-rasterCorrelation(MOWA_stack$MOWA_predictMap_m_00, MOWA_stack$MOWA_predictMap_m_09, s = 3, type = "pearson")
predCor_0_10<-rasterCorrelation(MOWA_stack$MOWA_predictMap_m_00, MOWA_stack$MOWA_predictMap_m_10, s = 3, type = "pearson")
predCor_0_11<-rasterCorrelation(MOWA_stack$MOWA_predictMap_m_00, MOWA_stack$MOWA_predictMap_m_11, s = 3, type = "pearson")
predCor_0_12<-rasterCorrelation(MOWA_stack$MOWA_predictMap_m_00, MOWA_stack$MOWA_predictMap_m_12, s = 3, type = "pearson")
predCor_0_13<-rasterCorrelation(MOWA_stack$MOWA_predictMap_m_00, MOWA_stack$MOWA_predictMap_m_13, s = 3, type = "pearson")
predCor_0_14<-rasterCorrelation(MOWA_stack$MOWA_predictMap_m_00, MOWA_stack$MOWA_predictMap_m_14, s = 3, type = "pearson")
predCor_0_15<-rasterCorrelation(MOWA_stack$MOWA_predictMap_m_00, MOWA_stack$MOWA_predictMap_m_15, s = 3, type = "pearson")


writeRaster(predCor_0_01, file="3_output/maps/predictedDistributions/MOWA/predCor/predCor_0_01.tif", overwrite=TRUE)
writeRaster(predCor_0_02, file="3_output/maps/predictedDistributions/MOWA/predCor/predCor_0_02.tif", overwrite=TRUE)
writeRaster(predCor_0_03, file="3_output/maps/predictedDistributions/MOWA/predCor/predCor_0_03.tif", overwrite=TRUE)
writeRaster(predCor_0_04, file="3_output/maps/predictedDistributions/MOWA/predCor/predCor_0_04.tif", overwrite=TRUE)
writeRaster(predCor_0_05, file="3_output/maps/predictedDistributions/MOWA/predCor/predCor_0_05.tif", overwrite=TRUE)
writeRaster(predCor_0_06, file="3_output/maps/predictedDistributions/MOWA/predCor/predCor_0_06.tif", overwrite=TRUE)
writeRaster(predCor_0_07, file="3_output/maps/predictedDistributions/MOWA/predCor/predCor_0_07.tif", overwrite=TRUE)
writeRaster(predCor_0_08, file="3_output/maps/predictedDistributions/MOWA/predCor/predCor_0_08.tif", overwrite=TRUE)
writeRaster(predCor_0_09, file="3_output/maps/predictedDistributions/MOWA/predCor/predCor_0_09.tif", overwrite=TRUE)
writeRaster(predCor_0_10, file="3_output/maps/predictedDistributions/MOWA/predCor/predCor_0_10.tif", overwrite=TRUE)
writeRaster(predCor_0_11, file="3_output/maps/predictedDistributions/MOWA/predCor/predCor_0_11.tif", overwrite=TRUE)
writeRaster(predCor_0_12, file="3_output/maps/predictedDistributions/MOWA/predCor/predCor_0_12.tif", overwrite=TRUE)
writeRaster(predCor_0_13, file="3_output/maps/predictedDistributions/MOWA/predCor/predCor_0_13.tif", overwrite=TRUE)
writeRaster(predCor_0_14, file="3_output/maps/predictedDistributions/MOWA/predCor/predCor_0_14.tif", overwrite=TRUE)
writeRaster(predCor_0_15, file="3_output/maps/predictedDistributions/MOWA/predCor/predCor_0_15.tif", overwrite=TRUE)



map_list<-list.files(path='3_output/maps/predictedDistributions/MOWA/predCor', pattern = "*.tif$", full.names = TRUE)
MOWA_predCor_stack<-raster::stack(map_list)

#save stack as r object
stackSave(MOWA_predCor_stack,file="3_output/maps/predictedDistributions/MOWA/predCor/MOWA_predCor_stack.stk")



## -----------------------------------------------------------------------


MOWA_predCor_stack<-stackOpen("3_output/maps/predictedDistributions/MOWA/predCor/MOWA_predCor_stack.stk")

lidar_forestAge_raster<-raster("0_data/manual/spatialCov/CASFRI/lidar_forestAge_raster.tif")
ageClass_00<-raster("0_data/manual/spatialCov/CASFRI/ageClassRasters/ageClass_00.tif")

lidar_forestAge_raster_pr<-projectRaster(lidar_forestAge_raster, crs=MOWA_predCor_stack)
ageClass_00_pr<-projectRaster(ageClass_00, method='ngb',crs=MOWA_predCor_stack)

#study area
load("0_data/manual/bird/studyarea_big.rData")

pc<- raster::crop(MOWA_predCor_stack, as_Spatial(st_geometry(c_bb)))
fa<- raster::crop(lidar_forestAge_raster_pr, as_Spatial(st_geometry(c_bb)))
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))
#resample so they have the same resolution
pc_rs<-resample(pc, fa)

# Stack covariates
fa_co_ac <- stack(pc_rs, fa, ac)

MOWA_fa_co_ac_df<-as.data.frame(na.omit(values(fa_co_ac)))%>%
  dplyr::rename(forest_age=lidar_forestAge_raster)%>%
  dplyr::rename(forest_age_class=ageClass_00)

MOWA_fa_co_ac_df$forest_age_class<-ordered(as.factor(MOWA_fa_co_ac_df$forest_age_class))

save(MOWA_fa_co_ac_df, file="2_pipeline/store/MOWA_fa_co_ac_df.rData")

load("2_pipeline/store/MOWA_fa_co_ac_df.rData")


#######################################################################################
############### Plot Effect ######################################################
#######################################################################################
lm_fa_00to01 <- lm(predCor_0_01 ~ forest_age, data=MOWA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to01, terms="forest_age [all]", data=MOWA_fa_co_ac_df)
png("3_output/figures/timelag_stats/MOWA_lm_fa_00to01.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to01 <- lm(predCor_0_01 ~ forest_age_class, data=MOWA_fa_co_ac_df)
ggplot(MOWA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_01) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/MOWA_lm_fac_00to01.png"), width =8, height=5)

#######################################################################################

lm_fa_00to02 <- lm(predCor_0_02 ~ forest_age, data=MOWA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to02, terms="forest_age [all]", data=MOWA_fa_co_ac_df)
png("3_output/figures/timelag_stats/MOWA_lm_fa_00to02.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# age class
lm_fac_00to02 <- lm(predCor_0_02 ~ forest_age_class, data=MOWA_fa_co_ac_df)
ggplot(MOWA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_02) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
ggsave(filename=paste0("3_output/figures/timelag_stats/MOWA_lm_fac_00to02.png"), width =8, height=5)  
  
#######################################################################################
lm_fa_00to03 <- lm(predCor_0_03 ~ forest_age, data=MOWA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to03, terms="forest_age [all]", data=MOWA_fa_co_ac_df)
png("3_output/figures/timelag_stats/MOWA_lm_fa_00to03.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to03 <- lm(predCor_0_03 ~ forest_age_class, data=MOWA_fa_co_ac_df)
ggplot(MOWA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_03) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/MOWA_lm_fac_00to03.png"), width =8, height=5)


#######################################################################################
lm_fa_00to04 <- lm(predCor_0_04 ~ forest_age, data=MOWA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to04, terms="forest_age [all]", data=MOWA_fa_co_ac_df)
png("3_output/figures/timelag_stats/MOWA_lm_fa_00to04.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to04 <- lm(predCor_0_04 ~ forest_age_class, data=MOWA_fa_co_ac_df)
ggplot(MOWA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_04) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())  
  ggsave(filename=paste0("3_output/figures/timelag_stats/MOWA_lm_fac_00to04.png"), width =8, height=5)    
  
#######################################################################################
lm_fa_00to05 <- lm(predCor_0_05 ~ forest_age, data=MOWA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to05, terms="forest_age [all]", data=MOWA_fa_co_ac_df)
png("3_output/figures/timelag_stats/MOWA_lm_fa_00to05.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to05 <- lm(predCor_0_05 ~ forest_age_class, data=MOWA_fa_co_ac_df)
ggplot(MOWA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_05) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/MOWA_lm_fac_00to05.png"), width =8, height=5)
  
  
  
#######################################################################################
lm_fa_00to06 <- lm(predCor_0_06 ~ forest_age, data=MOWA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to06, terms="forest_age [all]", data=MOWA_fa_co_ac_df)
png("3_output/figures/timelag_stats/MOWA_lm_fa_00to06.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to06 <- lm(predCor_0_06 ~ forest_age_class, data=MOWA_fa_co_ac_df)
ggplot(MOWA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_06) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/MOWA_lm_fac_00to06.png"), width =8, height=5)
  
  
  
  
  
  
  #######################################################################################
lm_fa_00to07 <- lm(predCor_0_07 ~ forest_age, data=MOWA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to07, terms="forest_age [all]", data=MOWA_fa_co_ac_df)
png("3_output/figures/timelag_stats/MOWA_lm_fa_00to07.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to07 <- lm(predCor_0_07 ~ forest_age_class, data=MOWA_fa_co_ac_df)
ggplot(MOWA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_07) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/MOWA_lm_fac_00to07.png"), width =8, height=5)
  
  
  
  
  
  #######################################################################################
lm_fa_00to08 <- lm(predCor_0_08 ~ forest_age, data=MOWA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to08, terms="forest_age [all]", data=MOWA_fa_co_ac_df)
png("3_output/figures/timelag_stats/MOWA_lm_fa_00to08.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to08 <- lm(predCor_0_08 ~ forest_age_class, data=MOWA_fa_co_ac_df)
ggplot(MOWA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_08) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/MOWA_lm_fac_00to08.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to09 <- lm(predCor_0_09 ~ forest_age, data=MOWA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to09, terms="forest_age [all]", data=MOWA_fa_co_ac_df)
png("3_output/figures/timelag_stats/MOWA_lm_fa_00to09.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to09 <- lm(predCor_0_09 ~ forest_age_class, data=MOWA_fa_co_ac_df)
ggplot(MOWA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_09) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/MOWA_lm_fac_00to09.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to10 <- lm(predCor_0_10 ~ forest_age, data=MOWA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to10, terms="forest_age [all]", data=MOWA_fa_co_ac_df)
png("3_output/figures/timelag_stats/MOWA_lm_fa_00to10.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to10 <- lm(predCor_0_10 ~ forest_age_class, data=MOWA_fa_co_ac_df)
ggplot(MOWA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_10) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/MOWA_lm_fac_00to10.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to11 <- lm(predCor_0_11 ~ forest_age, data=MOWA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to11, terms="forest_age [all]", data=MOWA_fa_co_ac_df)
png("3_output/figures/timelag_stats/MOWA_lm_fa_00to11.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to11 <- lm(predCor_0_11 ~ forest_age_class, data=MOWA_fa_co_ac_df)
ggplot(MOWA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_11) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/MOWA_lm_fac_00to11.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to12 <- lm(predCor_0_12 ~ forest_age, data=MOWA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to12, terms="forest_age [all]", data=MOWA_fa_co_ac_df)
png("3_output/figures/timelag_stats/MOWA_lm_fa_00to12.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to12 <- lm(predCor_0_12 ~ forest_age_class, data=MOWA_fa_co_ac_df)
ggplot(MOWA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_12) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/MOWA_lm_fac_00to12.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to13 <- lm(predCor_0_13 ~ forest_age, data=MOWA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to13, terms="forest_age [all]", data=MOWA_fa_co_ac_df)
png("3_output/figures/timelag_stats/MOWA_lm_fa_00to13.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to13 <- lm(predCor_0_13 ~ forest_age_class, data=MOWA_fa_co_ac_df)
ggplot(MOWA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_13) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/MOWA_lm_fac_00to13.png"), width =8, height=5)

  
  
  #######################################################################################
lm_fa_00to14 <- lm(predCor_0_14 ~ forest_age, data=MOWA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to14, terms="forest_age [all]", data=MOWA_fa_co_ac_df)
png("3_output/figures/timelag_stats/MOWA_lm_fa_00to14.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to14 <- lm(predCor_0_14 ~ forest_age_class, data=MOWA_fa_co_ac_df)
ggplot(MOWA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_14) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/MOWA_lm_fac_00to14.png"), width =8, height=5)
  
  
  
  #######################################################################################
lm_fa_00to15 <- lm(predCor_0_15 ~ forest_age, data=MOWA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to15, terms="forest_age [all]", data=MOWA_fa_co_ac_df)
png("3_output/figures/timelag_stats/MOWA_lm_fa_00to15.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to15 <- lm(predCor_0_15 ~ forest_age_class, data=MOWA_fa_co_ac_df)
ggplot(MOWA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_15) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/MOWA_lm_fac_00to15.png"), width =8, height=5)
  

#######################################################################################
#### save models
#######################################################################################

# save models

# forest age
MOWA_predCor_to_forestAge <-list(lm_fa_00to01, lm_fa_00to02, lm_fa_00to03, lm_fa_00to04, lm_fa_00to05, lm_fa_00to06, lm_fa_00to07, lm_fa_00to08, lm_fa_00to09, lm_fa_00to10, lm_fa_00to11, lm_fa_00to12, lm_fa_00to13, lm_fa_00to14, lm_fa_00to15)
names(MOWA_predCor_to_forestAge) <- c("lm_fa_00to01", "lm_fa_00to02", "lm_fa_00to03", "lm_fa_00to04", "lm_fa_00to05", "lm_fa_00to06", "lm_fa_00to07", "lm_fa_00to08", "lm_fa_00to09", "lm_fa_00to10", "lm_fa_00to11", "lm_fa_00to12", "lm_fa_00to13", "lm_fa_00to14", "lm_fa_00to15")
save(MOWA_predCor_to_forestAge, file="2_pipeline/store/models/MOWA_predCor_to_forestAge.rData")




MOWA_predCor_to_forestAge_df<-purrr::map_df(MOWA_predCor_to_forestAge, broom::glance, .id = 'formula')
save(MOWA_predCor_to_forestAge_df, file="2_pipeline/store/models/MOWA_predCor_to_forestAge_df.rData")

# forest age class
MOWA_predCor_to_forestAgeClass<-list(lm_fac_00to01, lm_fac_00to02, lm_fac_00to03, lm_fac_00to04, lm_fac_00to05, lm_fac_00to06, lm_fac_00to07, lm_fac_00to08, lm_fac_00to09, lm_fac_00to10, lm_fac_00to11, lm_fac_00to12, lm_fac_00to13, lm_fac_00to14, lm_fac_00to15)
names(MOWA_predCor_to_forestAgeClass) <- c("lm_fac_00to01", "lm_fac_00to02", "lm_fac_00to03", "lm_fac_00to04", "lm_fac_00to05", "lm_fac_00to06", "lm_fac_00to07", "lm_fac_00to08", "lm_fac_00to09", "lm_fac_00to10", "lm_fac_00to11", "lm_fac_00to12", "lm_fac_00to13", "lm_fac_00to14", "lm_fac_00to15")
save(MOWA_predCor_to_forestAgeClass, file="2_pipeline/store/models/MOWA_predCor_to_forestAgeClass.rData")




MOWA_predCor_to_forestAgeClass_df<-purrr::map_df(MOWA_predCor_to_forestAgeClass, broom::glance, .id = 'formula')
save(MOWA_predCor_to_forestAgeClass_df, file="2_pipeline/store/models/MOWA_predCor_to_forestAgeClass_df.rData")


#######################################################################################
#### Plot all on the same plot

ggplot() +
  geom_smooth(aes(x = forest_age, y = predCor_0_02, col = "0 and 02 years"), data = MOWA_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_04, col = "0 and 04 years"), data = MOWA_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_06, col = "0 and 06 years"), data = MOWA_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_08, col = "0 and 08 years"), data = MOWA_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_10, col = "0 and 10 years"), data = MOWA_fa_co_df, 
              method = "lm", se = FALSE)+
    scale_colour_manual(name="", 
                      values = c("#fa9fb5","#f768a1","#dd3497","#ae017e","#7a0177")) +
  labs(
    x = "Forest age when LiDAR was acquired", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  ggsave(filename=paste0("3_output/figures/timelag_stats/MOWA_lm_fa_all.png"), width =8, height=5)
  
  
  
ggplot() +
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_01, col = "0 and 01 years"), data = MOWA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_02, col = "0 and 02 years"), data = MOWA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_03, col = "0 and 03 years"), data = MOWA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_04, col = "0 and 04 years"), data = MOWA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_05, col = "0 and 05 years"), data = MOWA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_06, col = "0 and 06 years"), data = MOWA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_07, col = "0 and 07 years"), data = MOWA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_08, col = "0 and 08 years"), data = MOWA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_09, col = "0 and 09 years"), data = MOWA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_10, col = "0 and 10 years"), data = MOWA_fa_co_ac_df, 
              method = "lm", se = FALSE)+
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_11, col = "0 and 11 years"), data = MOWA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_12, col = "0 and 12 years"), data = MOWA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_13, col = "0 and 13 years"), data = MOWA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_14, col = "0 and 14 years"), data = MOWA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_15, col = "0 and 15 years"), data = MOWA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  scale_colour_viridis_d()+  
  # scale_colour_manual(name="", 
    #                   values = c("#fa9fb5","#f768a1","#dd3497","#ae017e","#7a0177")) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  ggsave(filename=paste0("3_output/figures/timelag_stats/MOWA_lm_fac_all.png"), width =8, height=5)
  


## ----echo=FALSE, out.width = '100%'-------------------------------------
knitr::include_graphics("3_output/figures/timelag_stats/MOWA_lm_fa_all.png", dpi = 300)


## ----echo=FALSE, message = FALSE, results="asis"------------------------
load("2_pipeline/store/models/MOWA_predCor_to_forestAge_df.rData")

knitr::kable(MOWA_predCor_to_forestAge_df) %>%
  kable_styling(font_size = 10, bootstrap_options= c("striped", "hover", "condensed" ), full_width= F, position="center") %>%
  column_spec(1, width= "20em")%>%
  scroll_box(width = "100%", height = "400px")


## -----------------------------------------------------------------------


MOWA_predCor_stack<-stackOpen("3_output/maps/predictedDistributions/MOWA/predCor/MOWA_predCor_stack.stk")

lidar_forestAge_raster<-raster("0_data/manual/spatialCov/CASFRI/lidar_forestAge_raster.tif")
ageClass_00<-raster("0_data/manual/spatialCov/CASFRI/ageClassRasters/ageClass_00.tif")

lidar_forestAge_raster_pr<-projectRaster(lidar_forestAge_raster, crs=MOWA_predCor_stack)
ageClass_00_pr<-projectRaster(ageClass_00, method='ngb',crs=MOWA_predCor_stack)

#study area
load("0_data/manual/bird/studyarea_big.rData")

pc<- raster::crop(MOWA_predCor_stack, as_Spatial(st_geometry(c_bb)))
fa<- raster::crop(lidar_forestAge_raster_pr, as_Spatial(st_geometry(c_bb)))
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))
#resample so they have the same resolution

pc_rs<-resample(pc, fa)

# Stack covariates
fa_co_ac <- stack(pc_rs, fa, ac)

MOWA_fa_co_ac_df<-as.data.frame(na.omit(values(fa_co_ac)))%>%
  dplyr::rename(forest_age=lidar_forestAge_raster)%>%
  dplyr::rename(forest_age_class=ageClass_00)

MOWA_fa_co_ac_df$forest_age_class<-ordered(as.factor(MOWA_fa_co_ac_df$forest_age_class))

save(MOWA_fa_co_ac_df, file="2_pipeline/store/MOWA_fa_co_ac_df.rData")

load("2_pipeline/store/MOWA_fa_co_ac_df.rData")



MOWA_stack<-stackOpen("3_output/maps/predictedDistributions/MOWA/MOWA_stack.stk")
lidar_forestAge_raster<-raster("0_data/manual/spatialCov/CASFRI/lidar_forestAge_raster.tif")
ageClass_00<-raster("0_data/manual/spatialCov/CASFRI/ageClassRasters/ageClass_00.tif")



lidar_forestAge_raster_pr<-projectRaster(lidar_forestAge_raster, crs=MOWA_stack)
ageClass_00_pr<-projectRaster(ageClass_00, method='ngb',crs=MOWA_stack)

#study area
load("0_data/manual/bird/studyarea_big.rData")
 
pd<- raster::crop(MOWA_stack, as_Spatial(st_geometry(c_bb))) 
fa<- raster::crop(lidar_forestAge_raster_pr, as_Spatial(st_geometry(c_bb))) 
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))

#resample so they have the same resolution
pd_rs<-resample(pd, fa)

# Stack covariates
pd_fa <- stack(pd_rs, fa, ac)

MOWA_pd_fa_df<-as.data.frame(na.omit(values(pd_fa)))%>%
  dplyr::rename(forest_age=lidar_forestAge_raster)
          
save(MOWA_pd_fa_df, file="2_pipeline/store/MOWA_pd_fa_df.rData")

load("2_pipeline/store/MOWA_pd_fa_df.rData")




MOWA_pd_fa_df %>%
  ggplot(aes(MOWA_predictMap_m_00, MOWA_predictMap_m_15, color=forest_age)) +
  geom_point(alpha=0.4, size=.1) +
  scale_color_viridis() +
  labs(y="occupancy probability (15 year time lag)", x="occupancy probability (no time lag)", color="forest age (years)") +
   theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
ggsave(filename=paste0("3_output/figures/timelag_stats/MOWA_scatter_00to15.png"), width = 7, height=5)



MOWA_pd_fa_df %>%
  ggplot(aes(MOWA_predictMap_m_00, MOWA_predictMap_m_15, color=ageClass_00)) +
  geom_point(alpha=0.4, size=.1) +
  scale_color_viridis() +
  labs(y="occupancy probability (15 year time lag)", x="occupancy probability (no time lag)", color="forest age class") +
   theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
ggsave(filename=paste0("3_output/figures/timelag_stats/MOWA_scatter_00to15_class.png"), width = 7, height=5)


## ----echo=FALSE, out.width = '100%'-------------------------------------
knitr::include_graphics("3_output/figures/timelag_stats/MOWA_scatter_00to15.png", dpi = 300)


## ----echo=FALSE, out.width = '100%'-------------------------------------
knitr::include_graphics("3_output/figures/timelag_stats/MOWA_scatter_00to15_class.png", dpi = 300)



## ----child = '1_code/rNotebooks/3_compare/compare_SWTH.Rmd'-------------

## ----include=FALSE, cache=FALSE-----------------------------------------
#Set root directory to R project root
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())


## # set max height of code chunks using the following css styling

## 
## pre {

##   max-height: 300px;

##   overflow-y: auto;

## }

## 
## pre[class] {

##   max-height: 500px;

## }


## ----message = FALSE----------------------------------------------------


## -----------------------------------------------------------------------
load("2_pipeline/store/models/SWTH_models_AUC_4.rData")

SWTH_models_AUC_4<-SWTH_models_AUC_4%>%
  rename(LiDAR_timelag=lag)

ggplot(SWTH_models_AUC_4, aes(x=LiDAR_timelag, y=AUC_mean)) + 
  geom_errorbar(aes(ymin=AUC_mean-1.96*AUC_stdErr, ymax=AUC_mean+ 1.96*AUC_stdErr, width=.2))+
  geom_point(size=1) + 
  geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
  geom_smooth(method = lm)+
  theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none")
 ggsave(filename=paste0("3_output/figures/timelag_stats/SWTH_AUC_lm_test.png"), width =6, height=5)
  
SWTH_AUC_lm_test<-lm(AUC_mean~ LiDAR_timelag, data=SWTH_models_AUC_4)
save(SWTH_AUC_lm_test,file="2_pipeline/store/models/SWTH_AUC_lm_test.rData")

AUC_cor_test <- cor.test(SWTH_models_AUC_4$AUC_mean, SWTH_models_AUC_4$LiDAR_timelag, method = "pearson")

#########################

ggplot(SWTH_models_AUC_4, aes(x=LiDAR_timelag, y=R2m_mean)) + 
  geom_errorbar(aes(ymin=R2m_mean- 1.96* R2m_stdErr, ymax=R2m_mean+ 1.96* R2m_stdErr, width=.2))+
  geom_point() + 
  geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
  geom_smooth(method = lm) +
  theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none")
 ggsave(filename=paste0("3_output/figures/timelag_stats/SWTH_R2m_test.png"), width =6, height=5)
  
SWTH_R2m_test<-lm(R2m_mean~ LiDAR_timelag, data=SWTH_models_AUC_4)
save(SWTH_R2m_test,file="2_pipeline/store/models/SWTH_R2m_test.rData")

########################
ggplot(SWTH_models_AUC_4, aes(x=LiDAR_timelag, y=R2c_mean)) + 
  geom_errorbar(aes(ymin=R2c_mean- 1.96* R2c_stdErr, ymax=R2c_mean+ 1.96* R2c_stdErr, width=.2))+
  geom_point() + 
  geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
  geom_smooth(method = lm)+
  theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none")
 ggsave(filename=paste0("3_output/figures/timelag_stats/SWTH_R2c_test.png"), width =6, height=5)
  
SWTH_R2c_test<-lm(R2c_mean~ LiDAR_timelag, data=SWTH_models_AUC_4)
save(SWTH_R2c_test,file="2_pipeline/store/models/SWTH_R2c_test.rData")



## ----echo=FALSE, out.width = '100%', fig.cap="SWTH AUC time lag"--------
knitr::include_graphics("3_output/figures/timelag_stats/SWTH_AUC_lm_test.png", dpi = 300)


## ----echo=FALSE---------------------------------------------------------
load("2_pipeline/store/models/SWTH_AUC_lm_test.rData")
print(summary(SWTH_AUC_lm_test))


## ----echo=FALSE, out.width = '100%', fig.cap="SWTH R2m time lag"--------
knitr::include_graphics("3_output/figures/timelag_stats/SWTH_R2m_test.png", dpi = 300)


## ----echo=FALSE---------------------------------------------------------
load("2_pipeline/store/models/SWTH_R2m_test.rData")
print(summary(SWTH_R2m_test))


## ----echo=FALSE, out.width = '100%', fig.cap="SWTH R2c time lag"--------
knitr::include_graphics("3_output/figures/timelag_stats/SWTH_R2c_test.png", dpi = 300)


## ----echo=FALSE---------------------------------------------------------
load("2_pipeline/store/models/SWTH_R2c_test.rData")
print(summary(SWTH_R2c_test))


## -----------------------------------------------------------------------
# Load stack of predictive rasters
SWTH_stack<-stackOpen("3_output/maps/predictedDistributions/SWTH/SWTH_stack.stk")

# reclassify to a binary above and below 50% occupancy probability

reclass_df <- c(0, .5, 0,
              .5, 1, 1)

reclass_m <- matrix(reclass_df,
                ncol = 3,
                byrow = TRUE)

SWTH_stack_reclass <- reclassify(SWTH_stack,
                     reclass_m)

# save reclass raster brick (save as .grd to preserve layer names)
writeRaster(SWTH_stack_reclass, "3_output/maps/predictedDistributions/SWTH/SWTH_stack_reclass.grd", overwrite=TRUE, format="raster") 

# load reclass rasters as a brick
SWTH_stack_reclass<-brick("3_output/maps/predictedDistributions/SWTH/SWTH_stack_reclass.grd")



## -----------------------------------------------------------------------
SWTH_stack<-stackOpen("3_output/maps/predictedDistributions/SWTH/SWTH_stack.stk")

predCor_0_01<-rasterCorrelation(SWTH_stack$SWTH_predictMap_m_00, SWTH_stack$SWTH_predictMap_m_01, s = 3, type = "pearson")
predCor_0_02<-rasterCorrelation(SWTH_stack$SWTH_predictMap_m_00, SWTH_stack$SWTH_predictMap_m_02, s = 3, type = "pearson")
predCor_0_03<-rasterCorrelation(SWTH_stack$SWTH_predictMap_m_00, SWTH_stack$SWTH_predictMap_m_03, s = 3, type = "pearson")
predCor_0_04<-rasterCorrelation(SWTH_stack$SWTH_predictMap_m_00, SWTH_stack$SWTH_predictMap_m_04, s = 3, type = "pearson")
predCor_0_05<-rasterCorrelation(SWTH_stack$SWTH_predictMap_m_00, SWTH_stack$SWTH_predictMap_m_05, s = 3, type = "pearson")
predCor_0_06<-rasterCorrelation(SWTH_stack$SWTH_predictMap_m_00, SWTH_stack$SWTH_predictMap_m_06, s = 3, type = "pearson")
predCor_0_07<-rasterCorrelation(SWTH_stack$SWTH_predictMap_m_00, SWTH_stack$SWTH_predictMap_m_07, s = 3, type = "pearson")
predCor_0_08<-rasterCorrelation(SWTH_stack$SWTH_predictMap_m_00, SWTH_stack$SWTH_predictMap_m_08, s = 3, type = "pearson")
predCor_0_09<-rasterCorrelation(SWTH_stack$SWTH_predictMap_m_00, SWTH_stack$SWTH_predictMap_m_09, s = 3, type = "pearson")
predCor_0_10<-rasterCorrelation(SWTH_stack$SWTH_predictMap_m_00, SWTH_stack$SWTH_predictMap_m_10, s = 3, type = "pearson")
predCor_0_11<-rasterCorrelation(SWTH_stack$SWTH_predictMap_m_00, SWTH_stack$SWTH_predictMap_m_11, s = 3, type = "pearson")
predCor_0_12<-rasterCorrelation(SWTH_stack$SWTH_predictMap_m_00, SWTH_stack$SWTH_predictMap_m_12, s = 3, type = "pearson")
predCor_0_13<-rasterCorrelation(SWTH_stack$SWTH_predictMap_m_00, SWTH_stack$SWTH_predictMap_m_13, s = 3, type = "pearson")
predCor_0_14<-rasterCorrelation(SWTH_stack$SWTH_predictMap_m_00, SWTH_stack$SWTH_predictMap_m_14, s = 3, type = "pearson")
predCor_0_15<-rasterCorrelation(SWTH_stack$SWTH_predictMap_m_00, SWTH_stack$SWTH_predictMap_m_15, s = 3, type = "pearson")


writeRaster(predCor_0_01, file="3_output/maps/predictedDistributions/SWTH/predCor/predCor_0_01.tif", overwrite=TRUE)
writeRaster(predCor_0_02, file="3_output/maps/predictedDistributions/SWTH/predCor/predCor_0_02.tif", overwrite=TRUE)
writeRaster(predCor_0_03, file="3_output/maps/predictedDistributions/SWTH/predCor/predCor_0_03.tif", overwrite=TRUE)
writeRaster(predCor_0_04, file="3_output/maps/predictedDistributions/SWTH/predCor/predCor_0_04.tif", overwrite=TRUE)
writeRaster(predCor_0_05, file="3_output/maps/predictedDistributions/SWTH/predCor/predCor_0_05.tif", overwrite=TRUE)
writeRaster(predCor_0_06, file="3_output/maps/predictedDistributions/SWTH/predCor/predCor_0_06.tif", overwrite=TRUE)
writeRaster(predCor_0_07, file="3_output/maps/predictedDistributions/SWTH/predCor/predCor_0_07.tif", overwrite=TRUE)
writeRaster(predCor_0_08, file="3_output/maps/predictedDistributions/SWTH/predCor/predCor_0_08.tif", overwrite=TRUE)
writeRaster(predCor_0_09, file="3_output/maps/predictedDistributions/SWTH/predCor/predCor_0_09.tif", overwrite=TRUE)
writeRaster(predCor_0_10, file="3_output/maps/predictedDistributions/SWTH/predCor/predCor_0_10.tif", overwrite=TRUE)
writeRaster(predCor_0_11, file="3_output/maps/predictedDistributions/SWTH/predCor/predCor_0_11.tif", overwrite=TRUE)
writeRaster(predCor_0_12, file="3_output/maps/predictedDistributions/SWTH/predCor/predCor_0_12.tif", overwrite=TRUE)
writeRaster(predCor_0_13, file="3_output/maps/predictedDistributions/SWTH/predCor/predCor_0_13.tif", overwrite=TRUE)
writeRaster(predCor_0_14, file="3_output/maps/predictedDistributions/SWTH/predCor/predCor_0_14.tif", overwrite=TRUE)
writeRaster(predCor_0_15, file="3_output/maps/predictedDistributions/SWTH/predCor/predCor_0_15.tif", overwrite=TRUE)



map_list<-list.files(path='3_output/maps/predictedDistributions/SWTH/predCor', pattern = "*.tif$", full.names = TRUE)
SWTH_predCor_stack<-raster::stack(map_list)

#save stack as r object
stackSave(SWTH_predCor_stack,file="3_output/maps/predictedDistributions/SWTH/predCor/SWTH_predCor_stack.stk")



## -----------------------------------------------------------------------


SWTH_predCor_stack<-stackOpen("3_output/maps/predictedDistributions/SWTH/predCor/SWTH_predCor_stack.stk")

lidar_forestAge_raster<-raster("0_data/manual/spatialCov/CASFRI/lidar_forestAge_raster.tif")
ageClass_00<-raster("0_data/manual/spatialCov/CASFRI/ageClassRasters/ageClass_00.tif")

lidar_forestAge_raster_pr<-projectRaster(lidar_forestAge_raster, crs=SWTH_predCor_stack)
ageClass_00_pr<-projectRaster(ageClass_00, method='ngb',crs=SWTH_predCor_stack)

#study area
load("0_data/manual/bird/studyarea_big.rData")

pc<- raster::crop(SWTH_predCor_stack, as_Spatial(st_geometry(c_bb)))
fa<- raster::crop(lidar_forestAge_raster_pr, as_Spatial(st_geometry(c_bb)))
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))
#resample so they have the same resolution
pc_rs<-resample(pc, fa)

# Stack covariates
fa_co_ac <- stack(pc_rs, fa, ac)

SWTH_fa_co_ac_df<-as.data.frame(na.omit(values(fa_co_ac)))%>%
  dplyr::rename(forest_age=lidar_forestAge_raster)%>%
  dplyr::rename(forest_age_class=ageClass_00)

SWTH_fa_co_ac_df$forest_age_class<-ordered(as.factor(SWTH_fa_co_ac_df$forest_age_class))

save(SWTH_fa_co_ac_df, file="2_pipeline/store/SWTH_fa_co_ac_df.rData")

load("2_pipeline/store/SWTH_fa_co_ac_df.rData")


#######################################################################################
############### Plot Effect ######################################################
#######################################################################################
lm_fa_00to01 <- lm(predCor_0_01 ~ forest_age, data=SWTH_fa_co_ac_df)
t<-ggpredict(lm_fa_00to01, terms="forest_age [all]", data=SWTH_fa_co_ac_df)
png("3_output/figures/timelag_stats/SWTH_lm_fa_00to01.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to01 <- lm(predCor_0_01 ~ forest_age_class, data=SWTH_fa_co_ac_df)
ggplot(SWTH_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_01) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/SWTH_lm_fac_00to01.png"), width =8, height=5)

#######################################################################################

lm_fa_00to02 <- lm(predCor_0_02 ~ forest_age, data=SWTH_fa_co_ac_df)
t<-ggpredict(lm_fa_00to02, terms="forest_age [all]", data=SWTH_fa_co_ac_df)
png("3_output/figures/timelag_stats/SWTH_lm_fa_00to02.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# age class
lm_fac_00to02 <- lm(predCor_0_02 ~ forest_age_class, data=SWTH_fa_co_ac_df)
ggplot(SWTH_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_02) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
ggsave(filename=paste0("3_output/figures/timelag_stats/SWTH_lm_fac_00to02.png"), width =8, height=5)  
  
#######################################################################################
lm_fa_00to03 <- lm(predCor_0_03 ~ forest_age, data=SWTH_fa_co_ac_df)
t<-ggpredict(lm_fa_00to03, terms="forest_age [all]", data=SWTH_fa_co_ac_df)
png("3_output/figures/timelag_stats/SWTH_lm_fa_00to03.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to03 <- lm(predCor_0_03 ~ forest_age_class, data=SWTH_fa_co_ac_df)
ggplot(SWTH_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_03) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/SWTH_lm_fac_00to03.png"), width =8, height=5)


#######################################################################################
lm_fa_00to04 <- lm(predCor_0_04 ~ forest_age, data=SWTH_fa_co_ac_df)
t<-ggpredict(lm_fa_00to04, terms="forest_age [all]", data=SWTH_fa_co_ac_df)
png("3_output/figures/timelag_stats/SWTH_lm_fa_00to04.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to04 <- lm(predCor_0_04 ~ forest_age_class, data=SWTH_fa_co_ac_df)
ggplot(SWTH_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_04) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())  
  ggsave(filename=paste0("3_output/figures/timelag_stats/SWTH_lm_fac_00to04.png"), width =8, height=5)    
  
#######################################################################################
lm_fa_00to05 <- lm(predCor_0_05 ~ forest_age, data=SWTH_fa_co_ac_df)
t<-ggpredict(lm_fa_00to05, terms="forest_age [all]", data=SWTH_fa_co_ac_df)
png("3_output/figures/timelag_stats/SWTH_lm_fa_00to05.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to05 <- lm(predCor_0_05 ~ forest_age_class, data=SWTH_fa_co_ac_df)
ggplot(SWTH_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_05) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/SWTH_lm_fac_00to05.png"), width =8, height=5)
  
  
  
#######################################################################################
lm_fa_00to06 <- lm(predCor_0_06 ~ forest_age, data=SWTH_fa_co_ac_df)
t<-ggpredict(lm_fa_00to06, terms="forest_age [all]", data=SWTH_fa_co_ac_df)
png("3_output/figures/timelag_stats/SWTH_lm_fa_00to06.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to06 <- lm(predCor_0_06 ~ forest_age_class, data=SWTH_fa_co_ac_df)
ggplot(SWTH_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_06) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/SWTH_lm_fac_00to06.png"), width =8, height=5)
  
  
  
  
  
  
  #######################################################################################
lm_fa_00to07 <- lm(predCor_0_07 ~ forest_age, data=SWTH_fa_co_ac_df)
t<-ggpredict(lm_fa_00to07, terms="forest_age [all]", data=SWTH_fa_co_ac_df)
png("3_output/figures/timelag_stats/SWTH_lm_fa_00to07.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to07 <- lm(predCor_0_07 ~ forest_age_class, data=SWTH_fa_co_ac_df)
ggplot(SWTH_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_07) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/SWTH_lm_fac_00to07.png"), width =8, height=5)
  
  
  
  
  
  #######################################################################################
lm_fa_00to08 <- lm(predCor_0_08 ~ forest_age, data=SWTH_fa_co_ac_df)
t<-ggpredict(lm_fa_00to08, terms="forest_age [all]", data=SWTH_fa_co_ac_df)
png("3_output/figures/timelag_stats/SWTH_lm_fa_00to08.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to08 <- lm(predCor_0_08 ~ forest_age_class, data=SWTH_fa_co_ac_df)
ggplot(SWTH_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_08) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/SWTH_lm_fac_00to08.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to09 <- lm(predCor_0_09 ~ forest_age, data=SWTH_fa_co_ac_df)
t<-ggpredict(lm_fa_00to09, terms="forest_age [all]", data=SWTH_fa_co_ac_df)
png("3_output/figures/timelag_stats/SWTH_lm_fa_00to09.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to09 <- lm(predCor_0_09 ~ forest_age_class, data=SWTH_fa_co_ac_df)
ggplot(SWTH_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_09) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/SWTH_lm_fac_00to09.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to10 <- lm(predCor_0_10 ~ forest_age, data=SWTH_fa_co_ac_df)
t<-ggpredict(lm_fa_00to10, terms="forest_age [all]", data=SWTH_fa_co_ac_df)
png("3_output/figures/timelag_stats/SWTH_lm_fa_00to10.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to10 <- lm(predCor_0_10 ~ forest_age_class, data=SWTH_fa_co_ac_df)
ggplot(SWTH_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_10) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/SWTH_lm_fac_00to10.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to11 <- lm(predCor_0_11 ~ forest_age, data=SWTH_fa_co_ac_df)
t<-ggpredict(lm_fa_00to11, terms="forest_age [all]", data=SWTH_fa_co_ac_df)
png("3_output/figures/timelag_stats/SWTH_lm_fa_00to11.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to11 <- lm(predCor_0_11 ~ forest_age_class, data=SWTH_fa_co_ac_df)
ggplot(SWTH_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_11) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/SWTH_lm_fac_00to11.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to12 <- lm(predCor_0_12 ~ forest_age, data=SWTH_fa_co_ac_df)
t<-ggpredict(lm_fa_00to12, terms="forest_age [all]", data=SWTH_fa_co_ac_df)
png("3_output/figures/timelag_stats/SWTH_lm_fa_00to12.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to12 <- lm(predCor_0_12 ~ forest_age_class, data=SWTH_fa_co_ac_df)
ggplot(SWTH_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_12) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/SWTH_lm_fac_00to12.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to13 <- lm(predCor_0_13 ~ forest_age, data=SWTH_fa_co_ac_df)
t<-ggpredict(lm_fa_00to13, terms="forest_age [all]", data=SWTH_fa_co_ac_df)
png("3_output/figures/timelag_stats/SWTH_lm_fa_00to13.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to13 <- lm(predCor_0_13 ~ forest_age_class, data=SWTH_fa_co_ac_df)
ggplot(SWTH_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_13) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/SWTH_lm_fac_00to13.png"), width =8, height=5)

  
  
  #######################################################################################
lm_fa_00to14 <- lm(predCor_0_14 ~ forest_age, data=SWTH_fa_co_ac_df)
t<-ggpredict(lm_fa_00to14, terms="forest_age [all]", data=SWTH_fa_co_ac_df)
png("3_output/figures/timelag_stats/SWTH_lm_fa_00to14.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to14 <- lm(predCor_0_14 ~ forest_age_class, data=SWTH_fa_co_ac_df)
ggplot(SWTH_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_14) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/SWTH_lm_fac_00to14.png"), width =8, height=5)
  
  
  
  #######################################################################################
lm_fa_00to15 <- lm(predCor_0_15 ~ forest_age, data=SWTH_fa_co_ac_df)
t<-ggpredict(lm_fa_00to15, terms="forest_age [all]", data=SWTH_fa_co_ac_df)
png("3_output/figures/timelag_stats/SWTH_lm_fa_00to15.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to15 <- lm(predCor_0_15 ~ forest_age_class, data=SWTH_fa_co_ac_df)
ggplot(SWTH_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_15) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/SWTH_lm_fac_00to15.png"), width =8, height=5)
  

#######################################################################################
#### save models
#######################################################################################

# save models

# forest age
SWTH_predCor_to_forestAge <-list(lm_fa_00to01, lm_fa_00to02, lm_fa_00to03, lm_fa_00to04, lm_fa_00to05, lm_fa_00to06, lm_fa_00to07, lm_fa_00to08, lm_fa_00to09, lm_fa_00to10, lm_fa_00to11, lm_fa_00to12, lm_fa_00to13, lm_fa_00to14, lm_fa_00to15)
names(SWTH_predCor_to_forestAge) <- c("lm_fa_00to01", "lm_fa_00to02", "lm_fa_00to03", "lm_fa_00to04", "lm_fa_00to05", "lm_fa_00to06", "lm_fa_00to07", "lm_fa_00to08", "lm_fa_00to09", "lm_fa_00to10", "lm_fa_00to11", "lm_fa_00to12", "lm_fa_00to13", "lm_fa_00to14", "lm_fa_00to15")
save(SWTH_predCor_to_forestAge, file="2_pipeline/store/models/SWTH_predCor_to_forestAge.rData")




SWTH_predCor_to_forestAge_df<-purrr::map_df(SWTH_predCor_to_forestAge, broom::glance, .id = 'formula')
save(SWTH_predCor_to_forestAge_df, file="2_pipeline/store/models/SWTH_predCor_to_forestAge_df.rData")

# forest age class
SWTH_predCor_to_forestAgeClass<-list(lm_fac_00to01, lm_fac_00to02, lm_fac_00to03, lm_fac_00to04, lm_fac_00to05, lm_fac_00to06, lm_fac_00to07, lm_fac_00to08, lm_fac_00to09, lm_fac_00to10, lm_fac_00to11, lm_fac_00to12, lm_fac_00to13, lm_fac_00to14, lm_fac_00to15)
names(SWTH_predCor_to_forestAgeClass) <- c("lm_fac_00to01", "lm_fac_00to02", "lm_fac_00to03", "lm_fac_00to04", "lm_fac_00to05", "lm_fac_00to06", "lm_fac_00to07", "lm_fac_00to08", "lm_fac_00to09", "lm_fac_00to10", "lm_fac_00to11", "lm_fac_00to12", "lm_fac_00to13", "lm_fac_00to14", "lm_fac_00to15")
save(SWTH_predCor_to_forestAgeClass, file="2_pipeline/store/models/SWTH_predCor_to_forestAgeClass.rData")




SWTH_predCor_to_forestAgeClass_df<-purrr::map_df(SWTH_predCor_to_forestAgeClass, broom::glance, .id = 'formula')
save(SWTH_predCor_to_forestAgeClass_df, file="2_pipeline/store/models/SWTH_predCor_to_forestAgeClass_df.rData")


#######################################################################################
#### Plot all on the same plot

ggplot() +
  geom_smooth(aes(x = forest_age, y = predCor_0_02, col = "0 and 02 years"), data = SWTH_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_04, col = "0 and 04 years"), data = SWTH_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_06, col = "0 and 06 years"), data = SWTH_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_08, col = "0 and 08 years"), data = SWTH_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_10, col = "0 and 10 years"), data = SWTH_fa_co_df, 
              method = "lm", se = FALSE)+
    scale_colour_manual(name="", 
                      values = c("#fa9fb5","#f768a1","#dd3497","#ae017e","#7a0177")) +
  labs(
    x = "Forest age when LiDAR was acquired", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  ggsave(filename=paste0("3_output/figures/timelag_stats/SWTH_lm_fa_all.png"), width =8, height=5)
  
  
  
ggplot() +
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_01, col = "0 and 01 years"), data = SWTH_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_02, col = "0 and 02 years"), data = SWTH_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_03, col = "0 and 03 years"), data = SWTH_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_04, col = "0 and 04 years"), data = SWTH_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_05, col = "0 and 05 years"), data = SWTH_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_06, col = "0 and 06 years"), data = SWTH_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_07, col = "0 and 07 years"), data = SWTH_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_08, col = "0 and 08 years"), data = SWTH_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_09, col = "0 and 09 years"), data = SWTH_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_10, col = "0 and 10 years"), data = SWTH_fa_co_ac_df, 
              method = "lm", se = FALSE)+
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_11, col = "0 and 11 years"), data = SWTH_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_12, col = "0 and 12 years"), data = SWTH_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_13, col = "0 and 13 years"), data = SWTH_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_14, col = "0 and 14 years"), data = SWTH_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_15, col = "0 and 15 years"), data = SWTH_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  scale_colour_viridis_d()+  
  # scale_colour_manual(name="", 
    #                   values = c("#fa9fb5","#f768a1","#dd3497","#ae017e","#7a0177")) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  ggsave(filename=paste0("3_output/figures/timelag_stats/SWTH_lm_fac_all.png"), width =8, height=5)
  


## ----echo=FALSE, out.width = '100%'-------------------------------------
knitr::include_graphics("3_output/figures/timelag_stats/SWTH_lm_fa_all.png", dpi = 300)


## ----echo=FALSE, message = FALSE, results="asis"------------------------
load("2_pipeline/store/models/SWTH_predCor_to_forestAge_df.rData")

knitr::kable(SWTH_predCor_to_forestAge_df) %>%
  kable_styling(font_size = 10, bootstrap_options= c("striped", "hover", "condensed" ), full_width= F, position="center") %>%
  column_spec(1, width= "20em")%>%
  scroll_box(width = "100%", height = "400px")


## -----------------------------------------------------------------------


SWTH_predCor_stack<-stackOpen("3_output/maps/predictedDistributions/SWTH/predCor/SWTH_predCor_stack.stk")

lidar_forestAge_raster<-raster("0_data/manual/spatialCov/CASFRI/lidar_forestAge_raster.tif")
ageClass_00<-raster("0_data/manual/spatialCov/CASFRI/ageClassRasters/ageClass_00.tif")

lidar_forestAge_raster_pr<-projectRaster(lidar_forestAge_raster, crs=SWTH_predCor_stack)
ageClass_00_pr<-projectRaster(ageClass_00, method='ngb',crs=SWTH_predCor_stack)

#study area
load("0_data/manual/bird/studyarea_big.rData")

pc<- raster::crop(SWTH_predCor_stack, as_Spatial(st_geometry(c_bb)))
fa<- raster::crop(lidar_forestAge_raster_pr, as_Spatial(st_geometry(c_bb)))
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))
#resample so they have the same resolution

pc_rs<-resample(pc, fa)

# Stack covariates
fa_co_ac <- stack(pc_rs, fa, ac)

SWTH_fa_co_ac_df<-as.data.frame(na.omit(values(fa_co_ac)))%>%
  dplyr::rename(forest_age=lidar_forestAge_raster)%>%
  dplyr::rename(forest_age_class=ageClass_00)

SWTH_fa_co_ac_df$forest_age_class<-ordered(as.factor(SWTH_fa_co_ac_df$forest_age_class))

save(SWTH_fa_co_ac_df, file="2_pipeline/store/SWTH_fa_co_ac_df.rData")

load("2_pipeline/store/SWTH_fa_co_ac_df.rData")



SWTH_stack<-stackOpen("3_output/maps/predictedDistributions/SWTH/SWTH_stack.stk")
lidar_forestAge_raster<-raster("0_data/manual/spatialCov/CASFRI/lidar_forestAge_raster.tif")
ageClass_00<-raster("0_data/manual/spatialCov/CASFRI/ageClassRasters/ageClass_00.tif")



lidar_forestAge_raster_pr<-projectRaster(lidar_forestAge_raster, crs=SWTH_stack)
ageClass_00_pr<-projectRaster(ageClass_00, method='ngb',crs=SWTH_stack)

#study area
load("0_data/manual/bird/studyarea_big.rData")
 
pd<- raster::crop(SWTH_stack, as_Spatial(st_geometry(c_bb))) 
fa<- raster::crop(lidar_forestAge_raster_pr, as_Spatial(st_geometry(c_bb))) 
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))

#resample so they have the same resolution
pd_rs<-resample(pd, fa)

# Stack covariates
pd_fa <- stack(pd_rs, fa, ac)

SWTH_pd_fa_df<-as.data.frame(na.omit(values(pd_fa)))%>%
  dplyr::rename(forest_age=lidar_forestAge_raster)
          
save(SWTH_pd_fa_df, file="2_pipeline/store/SWTH_pd_fa_df.rData")

load("2_pipeline/store/SWTH_pd_fa_df.rData")




SWTH_pd_fa_df %>%
  ggplot(aes(SWTH_predictMap_m_00, SWTH_predictMap_m_15, color=forest_age)) +
  geom_point(alpha=0.4, size=.1) +
  scale_color_viridis() +
  labs(y="occupancy probability (15 year time lag)", x="occupancy probability (no time lag)", color="forest age (years)") +
   theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
ggsave(filename=paste0("3_output/figures/timelag_stats/SWTH_scatter_00to15.png"), width = 7, height=5)



SWTH_pd_fa_df %>%
  ggplot(aes(SWTH_predictMap_m_00, SWTH_predictMap_m_15, color=ageClass_00)) +
  geom_point(alpha=0.4, size=.1) +
  scale_color_viridis() +
  labs(y="occupancy probability (15 year time lag)", x="occupancy probability (no time lag)", color="forest age class") +
   theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
ggsave(filename=paste0("3_output/figures/timelag_stats/SWTH_scatter_00to15_class.png"), width = 7, height=5)


## ----echo=FALSE, out.width = '100%'-------------------------------------
knitr::include_graphics("3_output/figures/timelag_stats/SWTH_scatter_00to15.png", dpi = 300)


## ----echo=FALSE, out.width = '100%'-------------------------------------
knitr::include_graphics("3_output/figures/timelag_stats/SWTH_scatter_00to15_class.png", dpi = 300)



## ----child = '1_code/rNotebooks/3_compare/compare_WIWR.Rmd'-------------

## ----include=FALSE, cache=FALSE-----------------------------------------
#Set root directory to R project root
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())


## # set max height of code chunks using the following css styling

## 
## pre {

##   max-height: 300px;

##   overflow-y: auto;

## }

## 
## pre[class] {

##   max-height: 500px;

## }


## ---- message = FALSE---------------------------------------------------

## -----------------------------------------------------------------------
load("2_pipeline/store/models/WIWR_models_AUC_4.rData")

WIWR_models_AUC_4<-WIWR_models_AUC_4%>%
  rename(LiDAR_timelag=lag)

ggplot(WIWR_models_AUC_4, aes(x=LiDAR_timelag, y=AUC_mean)) + 
  geom_errorbar(aes(ymin=AUC_mean-1.96*AUC_stdErr, ymax=AUC_mean+ 1.96*AUC_stdErr, width=.2))+
  geom_point(size=1) + 
  geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
  geom_smooth(method = lm)+
  theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none")
 ggsave(filename=paste0("3_output/figures/timelag_stats/WIWR_AUC_lm_test.png"), width =6, height=5)
  
WIWR_AUC_lm_test<-lm(AUC_mean~ LiDAR_timelag, data=WIWR_models_AUC_4)
save(WIWR_AUC_lm_test,file="2_pipeline/store/models/WIWR_AUC_lm_test.rData")

AUC_cor_test <- cor.test(WIWR_models_AUC_4$AUC_mean, WIWR_models_AUC_4$LiDAR_timelag, method = "pearson")

#########################

ggplot(WIWR_models_AUC_4, aes(x=LiDAR_timelag, y=R2m_mean)) + 
  geom_errorbar(aes(ymin=R2m_mean- 1.96* R2m_stdErr, ymax=R2m_mean+ 1.96* R2m_stdErr, width=.2))+
  geom_point() + 
  geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
  geom_smooth(method = lm) +
  theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none")
 ggsave(filename=paste0("3_output/figures/timelag_stats/WIWR_R2m_test.png"), width =6, height=5)
  
WIWR_R2m_test<-lm(R2m_mean~ LiDAR_timelag, data=WIWR_models_AUC_4)
save(WIWR_R2m_test,file="2_pipeline/store/models/WIWR_R2m_test.rData")

########################
ggplot(WIWR_models_AUC_4, aes(x=LiDAR_timelag, y=R2c_mean)) + 
  geom_errorbar(aes(ymin=R2c_mean- 1.96* R2c_stdErr, ymax=R2c_mean+ 1.96* R2c_stdErr, width=.2))+
  geom_point() + 
  geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
  geom_smooth(method = lm)+
  theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none")
 ggsave(filename=paste0("3_output/figures/timelag_stats/WIWR_R2c_test.png"), width =6, height=5)
  
WIWR_R2c_test<-lm(R2c_mean~ LiDAR_timelag, data=WIWR_models_AUC_4)
save(WIWR_R2c_test,file="2_pipeline/store/models/WIWR_R2c_test.rData")



## ----echo=FALSE, out.width = '100%', fig.cap="WIWR AUC time lag"--------
knitr::include_graphics("3_output/figures/timelag_stats/WIWR_AUC_lm_test.png", dpi = 300)


## ----echo=FALSE---------------------------------------------------------
load("2_pipeline/store/models/WIWR_AUC_lm_test.rData")
print(summary(WIWR_AUC_lm_test))


## ----echo=FALSE, out.width = '100%', fig.cap="WIWR R2m time lag"--------
knitr::include_graphics("3_output/figures/timelag_stats/WIWR_R2m_test.png", dpi = 300)


## ----echo=FALSE---------------------------------------------------------
load("2_pipeline/store/models/WIWR_R2m_test.rData")
print(summary(WIWR_R2m_test))


## ----echo=FALSE, out.width = '100%', fig.cap="WIWR R2c time lag"--------
knitr::include_graphics("3_output/figures/timelag_stats/WIWR_R2c_test.png", dpi = 300)


## ----echo=FALSE---------------------------------------------------------
load("2_pipeline/store/models/WIWR_R2c_test.rData")
print(summary(WIWR_R2c_test))


## -----------------------------------------------------------------------
# Load stack of predictive rasters
WIWR_stack<-stackOpen("3_output/maps/predictedDistributions/WIWR/WIWR_stack.stk")

# reclassify to a binary above and below 50% occupancy probability

reclass_df <- c(0, .5, 0,
              .5, 1, 1)

reclass_m <- matrix(reclass_df,
                ncol = 3,
                byrow = TRUE)

WIWR_stack_reclass <- reclassify(WIWR_stack,
                     reclass_m)

# save reclass raster brick (save as .grd to preserve layer names)
writeRaster(WIWR_stack_reclass, "3_output/maps/predictedDistributions/WIWR/WIWR_stack_reclass.grd", overwrite=TRUE, format="raster") 

# load reclass rasters as a brick
WIWR_stack_reclass<-brick("3_output/maps/predictedDistributions/WIWR/WIWR_stack_reclass.grd")



## -----------------------------------------------------------------------
WIWR_stack<-stackOpen("3_output/maps/predictedDistributions/WIWR/WIWR_stack.stk")

predCor_0_01<-rasterCorrelation(WIWR_stack$WIWR_predictMap_m_00, WIWR_stack$WIWR_predictMap_m_01, s = 3, type = "pearson")
predCor_0_02<-rasterCorrelation(WIWR_stack$WIWR_predictMap_m_00, WIWR_stack$WIWR_predictMap_m_02, s = 3, type = "pearson")
predCor_0_03<-rasterCorrelation(WIWR_stack$WIWR_predictMap_m_00, WIWR_stack$WIWR_predictMap_m_03, s = 3, type = "pearson")
predCor_0_04<-rasterCorrelation(WIWR_stack$WIWR_predictMap_m_00, WIWR_stack$WIWR_predictMap_m_04, s = 3, type = "pearson")
predCor_0_05<-rasterCorrelation(WIWR_stack$WIWR_predictMap_m_00, WIWR_stack$WIWR_predictMap_m_05, s = 3, type = "pearson")
predCor_0_06<-rasterCorrelation(WIWR_stack$WIWR_predictMap_m_00, WIWR_stack$WIWR_predictMap_m_06, s = 3, type = "pearson")
predCor_0_07<-rasterCorrelation(WIWR_stack$WIWR_predictMap_m_00, WIWR_stack$WIWR_predictMap_m_07, s = 3, type = "pearson")
predCor_0_08<-rasterCorrelation(WIWR_stack$WIWR_predictMap_m_00, WIWR_stack$WIWR_predictMap_m_08, s = 3, type = "pearson")
predCor_0_09<-rasterCorrelation(WIWR_stack$WIWR_predictMap_m_00, WIWR_stack$WIWR_predictMap_m_09, s = 3, type = "pearson")
predCor_0_10<-rasterCorrelation(WIWR_stack$WIWR_predictMap_m_00, WIWR_stack$WIWR_predictMap_m_10, s = 3, type = "pearson")
predCor_0_11<-rasterCorrelation(WIWR_stack$WIWR_predictMap_m_00, WIWR_stack$WIWR_predictMap_m_11, s = 3, type = "pearson")
predCor_0_12<-rasterCorrelation(WIWR_stack$WIWR_predictMap_m_00, WIWR_stack$WIWR_predictMap_m_12, s = 3, type = "pearson")
predCor_0_13<-rasterCorrelation(WIWR_stack$WIWR_predictMap_m_00, WIWR_stack$WIWR_predictMap_m_13, s = 3, type = "pearson")
predCor_0_14<-rasterCorrelation(WIWR_stack$WIWR_predictMap_m_00, WIWR_stack$WIWR_predictMap_m_14, s = 3, type = "pearson")
predCor_0_15<-rasterCorrelation(WIWR_stack$WIWR_predictMap_m_00, WIWR_stack$WIWR_predictMap_m_15, s = 3, type = "pearson")


writeRaster(predCor_0_01, file="3_output/maps/predictedDistributions/WIWR/predCor/predCor_0_01.tif", overwrite=TRUE)
writeRaster(predCor_0_02, file="3_output/maps/predictedDistributions/WIWR/predCor/predCor_0_02.tif", overwrite=TRUE)
writeRaster(predCor_0_03, file="3_output/maps/predictedDistributions/WIWR/predCor/predCor_0_03.tif", overwrite=TRUE)
writeRaster(predCor_0_04, file="3_output/maps/predictedDistributions/WIWR/predCor/predCor_0_04.tif", overwrite=TRUE)
writeRaster(predCor_0_05, file="3_output/maps/predictedDistributions/WIWR/predCor/predCor_0_05.tif", overwrite=TRUE)
writeRaster(predCor_0_06, file="3_output/maps/predictedDistributions/WIWR/predCor/predCor_0_06.tif", overwrite=TRUE)
writeRaster(predCor_0_07, file="3_output/maps/predictedDistributions/WIWR/predCor/predCor_0_07.tif", overwrite=TRUE)
writeRaster(predCor_0_08, file="3_output/maps/predictedDistributions/WIWR/predCor/predCor_0_08.tif", overwrite=TRUE)
writeRaster(predCor_0_09, file="3_output/maps/predictedDistributions/WIWR/predCor/predCor_0_09.tif", overwrite=TRUE)
writeRaster(predCor_0_10, file="3_output/maps/predictedDistributions/WIWR/predCor/predCor_0_10.tif", overwrite=TRUE)
writeRaster(predCor_0_11, file="3_output/maps/predictedDistributions/WIWR/predCor/predCor_0_11.tif", overwrite=TRUE)
writeRaster(predCor_0_12, file="3_output/maps/predictedDistributions/WIWR/predCor/predCor_0_12.tif", overwrite=TRUE)
writeRaster(predCor_0_13, file="3_output/maps/predictedDistributions/WIWR/predCor/predCor_0_13.tif", overwrite=TRUE)
writeRaster(predCor_0_14, file="3_output/maps/predictedDistributions/WIWR/predCor/predCor_0_14.tif", overwrite=TRUE)
writeRaster(predCor_0_15, file="3_output/maps/predictedDistributions/WIWR/predCor/predCor_0_15.tif", overwrite=TRUE)



map_list<-list.files(path='3_output/maps/predictedDistributions/WIWR/predCor', pattern = "*.tif$", full.names = TRUE)
WIWR_predCor_stack<-raster::stack(map_list)

#save stack as r object
stackSave(WIWR_predCor_stack,file="3_output/maps/predictedDistributions/WIWR/predCor/WIWR_predCor_stack.stk")



## -----------------------------------------------------------------------


WIWR_predCor_stack<-stackOpen("3_output/maps/predictedDistributions/WIWR/predCor/WIWR_predCor_stack.stk")

lidar_forestAge_raster<-raster("0_data/manual/spatialCov/CASFRI/lidar_forestAge_raster.tif")
ageClass_00<-raster("0_data/manual/spatialCov/CASFRI/ageClassRasters/ageClass_00.tif")

lidar_forestAge_raster_pr<-projectRaster(lidar_forestAge_raster, crs=WIWR_predCor_stack)
ageClass_00_pr<-projectRaster(ageClass_00, method='ngb',crs=WIWR_predCor_stack)

#study area
load("0_data/manual/bird/studyarea_big.rData")

pc<- raster::crop(WIWR_predCor_stack, as_Spatial(st_geometry(c_bb)))
fa<- raster::crop(lidar_forestAge_raster_pr, as_Spatial(st_geometry(c_bb)))
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))
#resample so they have the same resolution
pc_rs<-resample(pc, fa)

# Stack covariates
fa_co_ac <- stack(pc_rs, fa, ac)

WIWR_fa_co_ac_df<-as.data.frame(na.omit(values(fa_co_ac)))%>%
  dplyr::rename(forest_age=lidar_forestAge_raster)%>%
  dplyr::rename(forest_age_class=ageClass_00)

WIWR_fa_co_ac_df$forest_age_class<-ordered(as.factor(WIWR_fa_co_ac_df$forest_age_class))

save(WIWR_fa_co_ac_df, file="2_pipeline/store/WIWR_fa_co_ac_df.rData")

load("2_pipeline/store/WIWR_fa_co_ac_df.rData")


#######################################################################################
############### Plot Effect ######################################################
#######################################################################################
lm_fa_00to01 <- lm(predCor_0_01 ~ forest_age, data=WIWR_fa_co_ac_df)
t<-ggpredict(lm_fa_00to01, terms="forest_age [all]", data=WIWR_fa_co_ac_df)
png("3_output/figures/timelag_stats/WIWR_lm_fa_00to01.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to01 <- lm(predCor_0_01 ~ forest_age_class, data=WIWR_fa_co_ac_df)
ggplot(WIWR_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_01) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WIWR_lm_fac_00to01.png"), width =8, height=5)

#######################################################################################

lm_fa_00to02 <- lm(predCor_0_02 ~ forest_age, data=WIWR_fa_co_ac_df)
t<-ggpredict(lm_fa_00to02, terms="forest_age [all]", data=WIWR_fa_co_ac_df)
png("3_output/figures/timelag_stats/WIWR_lm_fa_00to02.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# age class
lm_fac_00to02 <- lm(predCor_0_02 ~ forest_age_class, data=WIWR_fa_co_ac_df)
ggplot(WIWR_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_02) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
ggsave(filename=paste0("3_output/figures/timelag_stats/WIWR_lm_fac_00to02.png"), width =8, height=5)  
  
#######################################################################################
lm_fa_00to03 <- lm(predCor_0_03 ~ forest_age, data=WIWR_fa_co_ac_df)
t<-ggpredict(lm_fa_00to03, terms="forest_age [all]", data=WIWR_fa_co_ac_df)
png("3_output/figures/timelag_stats/WIWR_lm_fa_00to03.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to03 <- lm(predCor_0_03 ~ forest_age_class, data=WIWR_fa_co_ac_df)
ggplot(WIWR_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_03) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WIWR_lm_fac_00to03.png"), width =8, height=5)


#######################################################################################
lm_fa_00to04 <- lm(predCor_0_04 ~ forest_age, data=WIWR_fa_co_ac_df)
t<-ggpredict(lm_fa_00to04, terms="forest_age [all]", data=WIWR_fa_co_ac_df)
png("3_output/figures/timelag_stats/WIWR_lm_fa_00to04.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to04 <- lm(predCor_0_04 ~ forest_age_class, data=WIWR_fa_co_ac_df)
ggplot(WIWR_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_04) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WIWR_lm_fac_00to04.png"), width =8, height=5)    
  
#######################################################################################
lm_fa_00to05 <- lm(predCor_0_05 ~ forest_age, data=WIWR_fa_co_ac_df)
t<-ggpredict(lm_fa_00to05, terms="forest_age [all]", data=WIWR_fa_co_ac_df)
png("3_output/figures/timelag_stats/WIWR_lm_fa_00to05.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to05 <- lm(predCor_0_05 ~ forest_age_class, data=WIWR_fa_co_ac_df)
ggplot(WIWR_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_05) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WIWR_lm_fac_00to05.png"), width =8, height=5)
  
  
  
#######################################################################################
lm_fa_00to06 <- lm(predCor_0_06 ~ forest_age, data=WIWR_fa_co_ac_df)
t<-ggpredict(lm_fa_00to06, terms="forest_age [all]", data=WIWR_fa_co_ac_df)
png("3_output/figures/timelag_stats/WIWR_lm_fa_00to06.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to06 <- lm(predCor_0_06 ~ forest_age_class, data=WIWR_fa_co_ac_df)
ggplot(WIWR_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_06) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WIWR_lm_fac_00to06.png"), width =8, height=5)
  
  
  
  
  
  
  #######################################################################################
lm_fa_00to07 <- lm(predCor_0_07 ~ forest_age, data=WIWR_fa_co_ac_df)
t<-ggpredict(lm_fa_00to07, terms="forest_age [all]", data=WIWR_fa_co_ac_df)
png("3_output/figures/timelag_stats/WIWR_lm_fa_00to07.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to07 <- lm(predCor_0_07 ~ forest_age_class, data=WIWR_fa_co_ac_df)
ggplot(WIWR_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_07) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WIWR_lm_fac_00to07.png"), width =8, height=5)
  
  
  
  
  
  #######################################################################################
lm_fa_00to08 <- lm(predCor_0_08 ~ forest_age, data=WIWR_fa_co_ac_df)
t<-ggpredict(lm_fa_00to08, terms="forest_age [all]", data=WIWR_fa_co_ac_df)
png("3_output/figures/timelag_stats/WIWR_lm_fa_00to08.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to08 <- lm(predCor_0_08 ~ forest_age_class, data=WIWR_fa_co_ac_df)
ggplot(WIWR_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_08) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WIWR_lm_fac_00to08.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to09 <- lm(predCor_0_09 ~ forest_age, data=WIWR_fa_co_ac_df)
t<-ggpredict(lm_fa_00to09, terms="forest_age [all]", data=WIWR_fa_co_ac_df)
png("3_output/figures/timelag_stats/WIWR_lm_fa_00to09.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to09 <- lm(predCor_0_09 ~ forest_age_class, data=WIWR_fa_co_ac_df)
ggplot(WIWR_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_09) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WIWR_lm_fac_00to09.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to10 <- lm(predCor_0_10 ~ forest_age, data=WIWR_fa_co_ac_df)
t<-ggpredict(lm_fa_00to10, terms="forest_age [all]", data=WIWR_fa_co_ac_df)
png("3_output/figures/timelag_stats/WIWR_lm_fa_00to10.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to10 <- lm(predCor_0_10 ~ forest_age_class, data=WIWR_fa_co_ac_df)
ggplot(WIWR_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_10) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WIWR_lm_fac_00to10.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to11 <- lm(predCor_0_11 ~ forest_age, data=WIWR_fa_co_ac_df)
t<-ggpredict(lm_fa_00to11, terms="forest_age [all]", data=WIWR_fa_co_ac_df)
png("3_output/figures/timelag_stats/WIWR_lm_fa_00to11.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to11 <- lm(predCor_0_11 ~ forest_age_class, data=WIWR_fa_co_ac_df)
ggplot(WIWR_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_11) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WIWR_lm_fac_00to11.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to12 <- lm(predCor_0_12 ~ forest_age, data=WIWR_fa_co_ac_df)
t<-ggpredict(lm_fa_00to12, terms="forest_age [all]", data=WIWR_fa_co_ac_df)
png("3_output/figures/timelag_stats/WIWR_lm_fa_00to12.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to12 <- lm(predCor_0_12 ~ forest_age_class, data=WIWR_fa_co_ac_df)
ggplot(WIWR_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_12) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WIWR_lm_fac_00to12.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to13 <- lm(predCor_0_13 ~ forest_age, data=WIWR_fa_co_ac_df)
t<-ggpredict(lm_fa_00to13, terms="forest_age [all]", data=WIWR_fa_co_ac_df)
png("3_output/figures/timelag_stats/WIWR_lm_fa_00to13.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to13 <- lm(predCor_0_13 ~ forest_age_class, data=WIWR_fa_co_ac_df)
ggplot(WIWR_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_13) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WIWR_lm_fac_00to13.png"), width =8, height=5)

  
  
  #######################################################################################
lm_fa_00to14 <- lm(predCor_0_14 ~ forest_age, data=WIWR_fa_co_ac_df)
t<-ggpredict(lm_fa_00to14, terms="forest_age [all]", data=WIWR_fa_co_ac_df)
png("3_output/figures/timelag_stats/WIWR_lm_fa_00to14.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to14 <- lm(predCor_0_14 ~ forest_age_class, data=WIWR_fa_co_ac_df)
ggplot(WIWR_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_14) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WIWR_lm_fac_00to14.png"), width =8, height=5)
  
  
  
  #######################################################################################
lm_fa_00to15 <- lm(predCor_0_15 ~ forest_age, data=WIWR_fa_co_ac_df)
t<-ggpredict(lm_fa_00to15, terms="forest_age [all]", data=WIWR_fa_co_ac_df)
png("3_output/figures/timelag_stats/WIWR_lm_fa_00to15.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to15 <- lm(predCor_0_15 ~ forest_age_class, data=WIWR_fa_co_ac_df)
ggplot(WIWR_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_15) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WIWR_lm_fac_00to15.png"), width =8, height=5)
  

#######################################################################################
#### save models
#######################################################################################

# save models

# forest age
WIWR_predCor_to_forestAge <-list(lm_fa_00to01, lm_fa_00to02, lm_fa_00to03, lm_fa_00to04, lm_fa_00to05, lm_fa_00to06, lm_fa_00to07, lm_fa_00to08, lm_fa_00to09, lm_fa_00to10, lm_fa_00to11, lm_fa_00to12, lm_fa_00to13, lm_fa_00to14, lm_fa_00to15)
names(WIWR_predCor_to_forestAge) <- c("lm_fa_00to01", "lm_fa_00to02", "lm_fa_00to03", "lm_fa_00to04", "lm_fa_00to05", "lm_fa_00to06", "lm_fa_00to07", "lm_fa_00to08", "lm_fa_00to09", "lm_fa_00to10", "lm_fa_00to11", "lm_fa_00to12", "lm_fa_00to13", "lm_fa_00to14", "lm_fa_00to15")
save(WIWR_predCor_to_forestAge, file="2_pipeline/store/models/WIWR_predCor_to_forestAge.rData")




WIWR_predCor_to_forestAge_df<-purrr::map_df(WIWR_predCor_to_forestAge, broom::glance, .id = 'formula')
save(WIWR_predCor_to_forestAge_df, file="2_pipeline/store/models/WIWR_predCor_to_forestAge_df.rData")

# forest age class
WIWR_predCor_to_forestAgeClass<-list(lm_fac_00to01, lm_fac_00to02, lm_fac_00to03, lm_fac_00to04, lm_fac_00to05, lm_fac_00to06, lm_fac_00to07, lm_fac_00to08, lm_fac_00to09, lm_fac_00to10, lm_fac_00to11, lm_fac_00to12, lm_fac_00to13, lm_fac_00to14, lm_fac_00to15)
names(WIWR_predCor_to_forestAgeClass) <- c("lm_fac_00to01", "lm_fac_00to02", "lm_fac_00to03", "lm_fac_00to04", "lm_fac_00to05", "lm_fac_00to06", "lm_fac_00to07", "lm_fac_00to08", "lm_fac_00to09", "lm_fac_00to10", "lm_fac_00to11", "lm_fac_00to12", "lm_fac_00to13", "lm_fac_00to14", "lm_fac_00to15")
save(WIWR_predCor_to_forestAgeClass, file="2_pipeline/store/models/WIWR_predCor_to_forestAgeClass.rData")




WIWR_predCor_to_forestAgeClass_df<-purrr::map_df(WIWR_predCor_to_forestAgeClass, broom::glance, .id = 'formula')
save(WIWR_predCor_to_forestAgeClass_df, file="2_pipeline/store/models/WIWR_predCor_to_forestAgeClass_df.rData")


#######################################################################################
#### Plot all on the same plot

ggplot() +
  geom_smooth(aes(x = forest_age, y = predCor_0_02, col = "0 and 02 years"), data = WIWR_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_04, col = "0 and 04 years"), data = WIWR_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_06, col = "0 and 06 years"), data = WIWR_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_08, col = "0 and 08 years"), data = WIWR_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_10, col = "0 and 10 years"), data = WIWR_fa_co_df, 
              method = "lm", se = FALSE)+
    scale_colour_manual(name="", 
                      values = c("#fa9fb5","#f768a1","#dd3497","#ae017e","#7a0177")) +
  labs(
    x = "Forest age when LiDAR was acquired", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  ggsave(filename=paste0("3_output/figures/timelag_stats/WIWR_lm_fa_all.png"), width =8, height=5)
  
  
  
ggplot() +
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_01, col = "0 and 01 years"), data = WIWR_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_02, col = "0 and 02 years"), data = WIWR_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_03, col = "0 and 03 years"), data = WIWR_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_04, col = "0 and 04 years"), data = WIWR_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_05, col = "0 and 05 years"), data = WIWR_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_06, col = "0 and 06 years"), data = WIWR_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_07, col = "0 and 07 years"), data = WIWR_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_08, col = "0 and 08 years"), data = WIWR_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_09, col = "0 and 09 years"), data = WIWR_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_10, col = "0 and 10 years"), data = WIWR_fa_co_ac_df, 
              method = "lm", se = FALSE)+
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_11, col = "0 and 11 years"), data = WIWR_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_12, col = "0 and 12 years"), data = WIWR_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_13, col = "0 and 13 years"), data = WIWR_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_14, col = "0 and 14 years"), data = WIWR_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_15, col = "0 and 15 years"), data = WIWR_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  scale_colour_viridis_d()+  
  # scale_colour_manual(name="", 
    #                   values = c("#fa9fb5","#f768a1","#dd3497","#ae017e","#7a0177")) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WIWR_lm_fac_all.png"), width =8, height=5)
  


## ----echo=FALSE, out.width = '100%'-------------------------------------
knitr::include_graphics("3_output/figures/timelag_stats/WIWR_lm_fa_all.png", dpi = 300)


## ----echo=FALSE, message = FALSE, results="asis"------------------------
load("2_pipeline/store/models/WIWR_predCor_to_forestAge_df.rData")

knitr::kable(WIWR_predCor_to_forestAge_df) %>%
  kable_styling(font_size = 10, bootstrap_options= c("striped", "hover", "condensed" ), full_width= F, position="center") %>%
  column_spec(1, width= "20em")%>%
  scroll_box(width = "100%", height = "400px")


## -----------------------------------------------------------------------


WIWR_predCor_stack<-stackOpen("3_output/maps/predictedDistributions/WIWR/predCor/WIWR_predCor_stack.stk")

lidar_forestAge_raster<-raster("0_data/manual/spatialCov/CASFRI/lidar_forestAge_raster.tif")
ageClass_00<-raster("0_data/manual/spatialCov/CASFRI/ageClassRasters/ageClass_00.tif")

lidar_forestAge_raster_pr<-projectRaster(lidar_forestAge_raster, crs=WIWR_predCor_stack)
ageClass_00_pr<-projectRaster(ageClass_00, method='ngb',crs=WIWR_predCor_stack)

#study area
load("0_data/manual/bird/studyarea_big.rData")

pc<- raster::crop(WIWR_predCor_stack, as_Spatial(st_geometry(c_bb)))
fa<- raster::crop(lidar_forestAge_raster_pr, as_Spatial(st_geometry(c_bb)))
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))
#resample so they have the same resolution

pc_rs<-resample(pc, fa)

# Stack covariates
fa_co_ac <- stack(pc_rs, fa, ac)

WIWR_fa_co_ac_df<-as.data.frame(na.omit(values(fa_co_ac)))%>%
  dplyr::rename(forest_age=lidar_forestAge_raster)%>%
  dplyr::rename(forest_age_class=ageClass_00)

WIWR_fa_co_ac_df$forest_age_class<-ordered(as.factor(WIWR_fa_co_ac_df$forest_age_class))

save(WIWR_fa_co_ac_df, file="2_pipeline/store/WIWR_fa_co_ac_df.rData")

load("2_pipeline/store/WIWR_fa_co_ac_df.rData")



WIWR_stack<-stackOpen("3_output/maps/predictedDistributions/WIWR/WIWR_stack.stk")
lidar_forestAge_raster<-raster("0_data/manual/spatialCov/CASFRI/lidar_forestAge_raster.tif")
ageClass_00<-raster("0_data/manual/spatialCov/CASFRI/ageClassRasters/ageClass_00.tif")



lidar_forestAge_raster_pr<-projectRaster(lidar_forestAge_raster, crs=WIWR_stack)
ageClass_00_pr<-projectRaster(ageClass_00, method='ngb',crs=WIWR_stack)

#study area
load("0_data/manual/bird/studyarea_big.rData")
 
pd<- raster::crop(WIWR_stack, as_Spatial(st_geometry(c_bb))) 
fa<- raster::crop(lidar_forestAge_raster_pr, as_Spatial(st_geometry(c_bb))) 
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))

#resample so they have the same resolution
pd_rs<-resample(pd, fa)

# Stack covariates
pd_fa <- stack(pd_rs, fa, ac)

WIWR_pd_fa_df<-as.data.frame(na.omit(values(pd_fa)))%>%
  dplyr::rename(forest_age=lidar_forestAge_raster)
          
save(WIWR_pd_fa_df, file="2_pipeline/store/WIWR_pd_fa_df.rData")

load("2_pipeline/store/WIWR_pd_fa_df.rData")




WIWR_pd_fa_df %>%
  ggplot(aes(WIWR_predictMap_m_00, WIWR_predictMap_m_15, color=forest_age)) +
  geom_point(alpha=0.4, size=.1) +
  scale_color_viridis() +
  labs(y="occupancy probability (15 year time lag)", x="occupancy probability (no time lag)", color="forest age (years)") +
   theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
ggsave(filename=paste0("3_output/figures/timelag_stats/WIWR_scatter_00to15.png"), width = 7, height=5)



WIWR_pd_fa_df %>%
  ggplot(aes(WIWR_predictMap_m_00, WIWR_predictMap_m_15, color=ageClass_00)) +
  geom_point(alpha=0.4, size=.1) +
  scale_color_viridis() +
  labs(y="occupancy probability (15 year time lag)", x="occupancy probability (no time lag)", color="forest age class") +
   theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
ggsave(filename=paste0("3_output/figures/timelag_stats/WIWR_scatter_00to15_class.png"), width = 7, height=5)


## ----echo=FALSE, out.width = '100%'-------------------------------------
knitr::include_graphics("3_output/figures/timelag_stats/WIWR_scatter_00to15.png", dpi = 300)


## ----echo=FALSE, out.width = '100%'-------------------------------------
knitr::include_graphics("3_output/figures/timelag_stats/WIWR_scatter_00to15_class.png", dpi = 300)



## ----child = '1_code/rNotebooks/3_compare/compare_WTSP.Rmd'-------------

## ----include=FALSE, cache=FALSE-----------------------------------------
#Set root directory to R project root
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())


## # set max height of code chunks using the following css styling

## 
## pre {

##   max-height: 300px;

##   overflow-y: auto;

## }

## 
## pre[class] {

##   max-height: 500px;

## }


## ---- message = FALSE---------------------------------------------------


## -----------------------------------------------------------------------
load("2_pipeline/store/models/WTSP_models_AUC_4.rData")

WTSP_models_AUC_4<-WTSP_models_AUC_4%>%
  rename(LiDAR_timelag=lag)

ggplot(WTSP_models_AUC_4, aes(x=LiDAR_timelag, y=AUC_mean)) + 
  geom_errorbar(aes(ymin=AUC_mean-1.96*AUC_stdErr, ymax=AUC_mean+ 1.96*AUC_stdErr, width=.2))+
  geom_point(size=1) + 
  geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
  geom_smooth(method = lm)+
  theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none")
 ggsave(filename=paste0("3_output/figures/timelag_stats/WTSP_AUC_lm_test.png"), width =6, height=5)
  
WTSP_AUC_lm_test<-lm(AUC_mean~ LiDAR_timelag, data=WTSP_models_AUC_4)
save(WTSP_AUC_lm_test,file="2_pipeline/store/models/WTSP_AUC_lm_test.rData")

AUC_cor_test <- cor.test(WTSP_models_AUC_4$AUC_mean, WTSP_models_AUC_4$LiDAR_timelag, method = "pearson")

#########################

ggplot(WTSP_models_AUC_4, aes(x=LiDAR_timelag, y=R2m_mean)) + 
  geom_errorbar(aes(ymin=R2m_mean- 1.96* R2m_stdErr, ymax=R2m_mean+ 1.96* R2m_stdErr, width=.2))+
  geom_point() + 
  geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
  geom_smooth(method = lm) +
  theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none")
 ggsave(filename=paste0("3_output/figures/timelag_stats/WTSP_R2m_test.png"), width =6, height=5)
  
WTSP_R2m_test<-lm(R2m_mean~ LiDAR_timelag, data=WTSP_models_AUC_4)
save(WTSP_R2m_test,file="2_pipeline/store/models/WTSP_R2m_test.rData")

########################
ggplot(WTSP_models_AUC_4, aes(x=LiDAR_timelag, y=R2c_mean)) + 
  geom_errorbar(aes(ymin=R2c_mean- 1.96* R2c_stdErr, ymax=R2c_mean+ 1.96* R2c_stdErr, width=.2))+
  geom_point() + 
  geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
  geom_smooth(method = lm)+
  theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none")
 ggsave(filename=paste0("3_output/figures/timelag_stats/WTSP_R2c_test.png"), width =6, height=5)
  
WTSP_R2c_test<-lm(R2c_mean~ LiDAR_timelag, data=WTSP_models_AUC_4)
save(WTSP_R2c_test,file="2_pipeline/store/models/WTSP_R2c_test.rData")



## ----echo=FALSE, out.width = '100%', fig.cap="WTSP AUC time lag"--------
knitr::include_graphics("3_output/figures/timelag_stats/WTSP_AUC_lm_test.png", dpi = 300)


## ----echo=FALSE---------------------------------------------------------
load("2_pipeline/store/models/WTSP_AUC_lm_test.rData")
print(summary(WTSP_AUC_lm_test))


## ----echo=FALSE, out.width = '100%', fig.cap="WTSP R2m time lag"--------
knitr::include_graphics("3_output/figures/timelag_stats/WTSP_R2m_test.png", dpi = 300)


## ----echo=FALSE---------------------------------------------------------
load("2_pipeline/store/models/WTSP_R2m_test.rData")
print(summary(WTSP_R2m_test))


## ----echo=FALSE, out.width = '100%', fig.cap="WTSP R2c time lag"--------
knitr::include_graphics("3_output/figures/timelag_stats/WTSP_R2c_test.png", dpi = 300)


## ----echo=FALSE---------------------------------------------------------
load("2_pipeline/store/models/WTSP_R2c_test.rData")
print(summary(WTSP_R2c_test))


## -----------------------------------------------------------------------
# Load stack of predictive rasters
WTSP_stack<-stackOpen("3_output/maps/predictedDistributions/WTSP/WTSP_stack.stk")

# reclassify to a binary above and below 50% occupancy probability

reclass_df <- c(0, .5, 0,
              .5, 1, 1)

reclass_m <- matrix(reclass_df,
                ncol = 3,
                byrow = TRUE)

WTSP_stack_reclass <- reclassify(WTSP_stack,
                     reclass_m)

# save reclass raster brick (save as .grd to preserve layer names)
writeRaster(WTSP_stack_reclass, "3_output/maps/predictedDistributions/WTSP/WTSP_stack_reclass.grd", overwrite=TRUE, format="raster") 

# load reclass rasters as a brick
WTSP_stack_reclass<-brick("3_output/maps/predictedDistributions/WTSP/WTSP_stack_reclass.grd")



## -----------------------------------------------------------------------
WTSP_stack<-stackOpen("3_output/maps/predictedDistributions/WTSP/WTSP_stack.stk")

predCor_0_01<-rasterCorrelation(WTSP_stack$WTSP_predictMap_m_00, WTSP_stack$WTSP_predictMap_m_01, s = 3, type = "pearson")
predCor_0_02<-rasterCorrelation(WTSP_stack$WTSP_predictMap_m_00, WTSP_stack$WTSP_predictMap_m_02, s = 3, type = "pearson")
predCor_0_03<-rasterCorrelation(WTSP_stack$WTSP_predictMap_m_00, WTSP_stack$WTSP_predictMap_m_03, s = 3, type = "pearson")
predCor_0_04<-rasterCorrelation(WTSP_stack$WTSP_predictMap_m_00, WTSP_stack$WTSP_predictMap_m_04, s = 3, type = "pearson")
predCor_0_05<-rasterCorrelation(WTSP_stack$WTSP_predictMap_m_00, WTSP_stack$WTSP_predictMap_m_05, s = 3, type = "pearson")
predCor_0_06<-rasterCorrelation(WTSP_stack$WTSP_predictMap_m_00, WTSP_stack$WTSP_predictMap_m_06, s = 3, type = "pearson")
predCor_0_07<-rasterCorrelation(WTSP_stack$WTSP_predictMap_m_00, WTSP_stack$WTSP_predictMap_m_07, s = 3, type = "pearson")
predCor_0_08<-rasterCorrelation(WTSP_stack$WTSP_predictMap_m_00, WTSP_stack$WTSP_predictMap_m_08, s = 3, type = "pearson")
predCor_0_09<-rasterCorrelation(WTSP_stack$WTSP_predictMap_m_00, WTSP_stack$WTSP_predictMap_m_09, s = 3, type = "pearson")
predCor_0_10<-rasterCorrelation(WTSP_stack$WTSP_predictMap_m_00, WTSP_stack$WTSP_predictMap_m_10, s = 3, type = "pearson")
predCor_0_11<-rasterCorrelation(WTSP_stack$WTSP_predictMap_m_00, WTSP_stack$WTSP_predictMap_m_11, s = 3, type = "pearson")
predCor_0_12<-rasterCorrelation(WTSP_stack$WTSP_predictMap_m_00, WTSP_stack$WTSP_predictMap_m_12, s = 3, type = "pearson")
predCor_0_13<-rasterCorrelation(WTSP_stack$WTSP_predictMap_m_00, WTSP_stack$WTSP_predictMap_m_13, s = 3, type = "pearson")
predCor_0_14<-rasterCorrelation(WTSP_stack$WTSP_predictMap_m_00, WTSP_stack$WTSP_predictMap_m_14, s = 3, type = "pearson")
predCor_0_15<-rasterCorrelation(WTSP_stack$WTSP_predictMap_m_00, WTSP_stack$WTSP_predictMap_m_15, s = 3, type = "pearson")


writeRaster(predCor_0_01, file="3_output/maps/predictedDistributions/WTSP/predCor/predCor_0_01.tif", overwrite=TRUE)
writeRaster(predCor_0_02, file="3_output/maps/predictedDistributions/WTSP/predCor/predCor_0_02.tif", overwrite=TRUE)
writeRaster(predCor_0_03, file="3_output/maps/predictedDistributions/WTSP/predCor/predCor_0_03.tif", overwrite=TRUE)
writeRaster(predCor_0_04, file="3_output/maps/predictedDistributions/WTSP/predCor/predCor_0_04.tif", overwrite=TRUE)
writeRaster(predCor_0_05, file="3_output/maps/predictedDistributions/WTSP/predCor/predCor_0_05.tif", overwrite=TRUE)
writeRaster(predCor_0_06, file="3_output/maps/predictedDistributions/WTSP/predCor/predCor_0_06.tif", overwrite=TRUE)
writeRaster(predCor_0_07, file="3_output/maps/predictedDistributions/WTSP/predCor/predCor_0_07.tif", overwrite=TRUE)
writeRaster(predCor_0_08, file="3_output/maps/predictedDistributions/WTSP/predCor/predCor_0_08.tif", overwrite=TRUE)
writeRaster(predCor_0_09, file="3_output/maps/predictedDistributions/WTSP/predCor/predCor_0_09.tif", overwrite=TRUE)
writeRaster(predCor_0_10, file="3_output/maps/predictedDistributions/WTSP/predCor/predCor_0_10.tif", overwrite=TRUE)
writeRaster(predCor_0_11, file="3_output/maps/predictedDistributions/WTSP/predCor/predCor_0_11.tif", overwrite=TRUE)
writeRaster(predCor_0_12, file="3_output/maps/predictedDistributions/WTSP/predCor/predCor_0_12.tif", overwrite=TRUE)
writeRaster(predCor_0_13, file="3_output/maps/predictedDistributions/WTSP/predCor/predCor_0_13.tif", overwrite=TRUE)
writeRaster(predCor_0_14, file="3_output/maps/predictedDistributions/WTSP/predCor/predCor_0_14.tif", overwrite=TRUE)
writeRaster(predCor_0_15, file="3_output/maps/predictedDistributions/WTSP/predCor/predCor_0_15.tif", overwrite=TRUE)



map_list<-list.files(path='3_output/maps/predictedDistributions/WTSP/predCor', pattern = "*.tif$", full.names = TRUE)
WTSP_predCor_stack<-raster::stack(map_list)

#save stack as r object
stackSave(WTSP_predCor_stack,file="3_output/maps/predictedDistributions/WTSP/predCor/WTSP_predCor_stack.stk")



## -----------------------------------------------------------------------


WTSP_predCor_stack<-stackOpen("3_output/maps/predictedDistributions/WTSP/predCor/WTSP_predCor_stack.stk")

lidar_forestAge_raster<-raster("0_data/manual/spatialCov/CASFRI/lidar_forestAge_raster.tif")
ageClass_00<-raster("0_data/manual/spatialCov/CASFRI/ageClassRasters/ageClass_00.tif")

lidar_forestAge_raster_pr<-projectRaster(lidar_forestAge_raster, crs=WTSP_predCor_stack)
ageClass_00_pr<-projectRaster(ageClass_00, method='ngb',crs=WTSP_predCor_stack)

#study area
load("0_data/manual/bird/studyarea_big.rData")

pc<- raster::crop(WTSP_predCor_stack, as_Spatial(st_geometry(c_bb)))
fa<- raster::crop(lidar_forestAge_raster_pr, as_Spatial(st_geometry(c_bb)))
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))
#resample so they have the same resolution
pc_rs<-resample(pc, fa)

# Stack covariates
fa_co_ac <- stack(pc_rs, fa, ac)

WTSP_fa_co_ac_df<-as.data.frame(na.omit(values(fa_co_ac)))%>%
  dplyr::rename(forest_age=lidar_forestAge_raster)%>%
  dplyr::rename(forest_age_class=ageClass_00)

WTSP_fa_co_ac_df$forest_age_class<-ordered(as.factor(WTSP_fa_co_ac_df$forest_age_class))

save(WTSP_fa_co_ac_df, file="2_pipeline/store/WTSP_fa_co_ac_df.rData")

load("2_pipeline/store/WTSP_fa_co_ac_df.rData")


#######################################################################################
############### Plot Effect ######################################################
#######################################################################################
lm_fa_00to01 <- lm(predCor_0_01 ~ forest_age, data=WTSP_fa_co_ac_df)
t<-ggpredict(lm_fa_00to01, terms="forest_age [all]", data=WTSP_fa_co_ac_df)
png("3_output/figures/timelag_stats/WTSP_lm_fa_00to01.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to01 <- lm(predCor_0_01 ~ forest_age_class, data=WTSP_fa_co_ac_df)
ggplot(WTSP_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_01) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WTSP_lm_fac_00to01.png"), width =8, height=5)

#######################################################################################

lm_fa_00to02 <- lm(predCor_0_02 ~ forest_age, data=WTSP_fa_co_ac_df)
t<-ggpredict(lm_fa_00to02, terms="forest_age [all]", data=WTSP_fa_co_ac_df)
png("3_output/figures/timelag_stats/WTSP_lm_fa_00to02.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# age class
lm_fac_00to02 <- lm(predCor_0_02 ~ forest_age_class, data=WTSP_fa_co_ac_df)
ggplot(WTSP_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_02) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
ggsave(filename=paste0("3_output/figures/timelag_stats/WTSP_lm_fac_00to02.png"), width =8, height=5)  
  
#######################################################################################
lm_fa_00to03 <- lm(predCor_0_03 ~ forest_age, data=WTSP_fa_co_ac_df)
t<-ggpredict(lm_fa_00to03, terms="forest_age [all]", data=WTSP_fa_co_ac_df)
png("3_output/figures/timelag_stats/WTSP_lm_fa_00to03.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to03 <- lm(predCor_0_03 ~ forest_age_class, data=WTSP_fa_co_ac_df)
ggplot(WTSP_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_03) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WTSP_lm_fac_00to03.png"), width =8, height=5)


#######################################################################################
lm_fa_00to04 <- lm(predCor_0_04 ~ forest_age, data=WTSP_fa_co_ac_df)
t<-ggpredict(lm_fa_00to04, terms="forest_age [all]", data=WTSP_fa_co_ac_df)
png("3_output/figures/timelag_stats/WTSP_lm_fa_00to04.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to04 <- lm(predCor_0_04 ~ forest_age_class, data=WTSP_fa_co_ac_df)
ggplot(WTSP_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_04) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WTSP_lm_fac_00to04.png"), width =8, height=5)    
  
#######################################################################################
lm_fa_00to05 <- lm(predCor_0_05 ~ forest_age, data=WTSP_fa_co_ac_df)
t<-ggpredict(lm_fa_00to05, terms="forest_age [all]", data=WTSP_fa_co_ac_df)
png("3_output/figures/timelag_stats/WTSP_lm_fa_00to05.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to05 <- lm(predCor_0_05 ~ forest_age_class, data=WTSP_fa_co_ac_df)
ggplot(WTSP_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_05) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WTSP_lm_fac_00to05.png"), width =8, height=5)
  
  
  
#######################################################################################
lm_fa_00to06 <- lm(predCor_0_06 ~ forest_age, data=WTSP_fa_co_ac_df)
t<-ggpredict(lm_fa_00to06, terms="forest_age [all]", data=WTSP_fa_co_ac_df)
png("3_output/figures/timelag_stats/WTSP_lm_fa_00to06.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to06 <- lm(predCor_0_06 ~ forest_age_class, data=WTSP_fa_co_ac_df)
ggplot(WTSP_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_06) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WTSP_lm_fac_00to06.png"), width =8, height=5)
  
  
  
  
  
  
  #######################################################################################
lm_fa_00to07 <- lm(predCor_0_07 ~ forest_age, data=WTSP_fa_co_ac_df)
t<-ggpredict(lm_fa_00to07, terms="forest_age [all]", data=WTSP_fa_co_ac_df)
png("3_output/figures/timelag_stats/WTSP_lm_fa_00to07.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to07 <- lm(predCor_0_07 ~ forest_age_class, data=WTSP_fa_co_ac_df)
ggplot(WTSP_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_07) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WTSP_lm_fac_00to07.png"), width =8, height=5)
  
  
  
  
  
  #######################################################################################
lm_fa_00to08 <- lm(predCor_0_08 ~ forest_age, data=WTSP_fa_co_ac_df)
t<-ggpredict(lm_fa_00to08, terms="forest_age [all]", data=WTSP_fa_co_ac_df)
png("3_output/figures/timelag_stats/WTSP_lm_fa_00to08.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to08 <- lm(predCor_0_08 ~ forest_age_class, data=WTSP_fa_co_ac_df)
ggplot(WTSP_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_08) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WTSP_lm_fac_00to08.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to09 <- lm(predCor_0_09 ~ forest_age, data=WTSP_fa_co_ac_df)
t<-ggpredict(lm_fa_00to09, terms="forest_age [all]", data=WTSP_fa_co_ac_df)
png("3_output/figures/timelag_stats/WTSP_lm_fa_00to09.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to09 <- lm(predCor_0_09 ~ forest_age_class, data=WTSP_fa_co_ac_df)
ggplot(WTSP_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_09) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WTSP_lm_fac_00to09.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to10 <- lm(predCor_0_10 ~ forest_age, data=WTSP_fa_co_ac_df)
t<-ggpredict(lm_fa_00to10, terms="forest_age [all]", data=WTSP_fa_co_ac_df)
png("3_output/figures/timelag_stats/WTSP_lm_fa_00to10.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to10 <- lm(predCor_0_10 ~ forest_age_class, data=WTSP_fa_co_ac_df)
ggplot(WTSP_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_10) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WTSP_lm_fac_00to10.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to11 <- lm(predCor_0_11 ~ forest_age, data=WTSP_fa_co_ac_df)
t<-ggpredict(lm_fa_00to11, terms="forest_age [all]", data=WTSP_fa_co_ac_df)
png("3_output/figures/timelag_stats/WTSP_lm_fa_00to11.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to11 <- lm(predCor_0_11 ~ forest_age_class, data=WTSP_fa_co_ac_df)
ggplot(WTSP_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_11) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WTSP_lm_fac_00to11.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to12 <- lm(predCor_0_12 ~ forest_age, data=WTSP_fa_co_ac_df)
t<-ggpredict(lm_fa_00to12, terms="forest_age [all]", data=WTSP_fa_co_ac_df)
png("3_output/figures/timelag_stats/WTSP_lm_fa_00to12.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to12 <- lm(predCor_0_12 ~ forest_age_class, data=WTSP_fa_co_ac_df)
ggplot(WTSP_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_12) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WTSP_lm_fac_00to12.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to13 <- lm(predCor_0_13 ~ forest_age, data=WTSP_fa_co_ac_df)
t<-ggpredict(lm_fa_00to13, terms="forest_age [all]", data=WTSP_fa_co_ac_df)
png("3_output/figures/timelag_stats/WTSP_lm_fa_00to13.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to13 <- lm(predCor_0_13 ~ forest_age_class, data=WTSP_fa_co_ac_df)
ggplot(WTSP_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_13) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WTSP_lm_fac_00to13.png"), width =8, height=5)

  
  
  #######################################################################################
lm_fa_00to14 <- lm(predCor_0_14 ~ forest_age, data=WTSP_fa_co_ac_df)
t<-ggpredict(lm_fa_00to14, terms="forest_age [all]", data=WTSP_fa_co_ac_df)
png("3_output/figures/timelag_stats/WTSP_lm_fa_00to14.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to14 <- lm(predCor_0_14 ~ forest_age_class, data=WTSP_fa_co_ac_df)
ggplot(WTSP_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_14) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WTSP_lm_fac_00to14.png"), width =8, height=5)
  
  
  
  #######################################################################################
lm_fa_00to15 <- lm(predCor_0_15 ~ forest_age, data=WTSP_fa_co_ac_df)
t<-ggpredict(lm_fa_00to15, terms="forest_age [all]", data=WTSP_fa_co_ac_df)
png("3_output/figures/timelag_stats/WTSP_lm_fa_00to15.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to15 <- lm(predCor_0_15 ~ forest_age_class, data=WTSP_fa_co_ac_df)
ggplot(WTSP_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_15) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WTSP_lm_fac_00to15.png"), width =8, height=5)
  

#######################################################################################
#### save models
#######################################################################################

# save models

# forest age
WTSP_predCor_to_forestAge <-list(lm_fa_00to01, lm_fa_00to02, lm_fa_00to03, lm_fa_00to04, lm_fa_00to05, lm_fa_00to06, lm_fa_00to07, lm_fa_00to08, lm_fa_00to09, lm_fa_00to10, lm_fa_00to11, lm_fa_00to12, lm_fa_00to13, lm_fa_00to14, lm_fa_00to15)
names(WTSP_predCor_to_forestAge) <- c("lm_fa_00to01", "lm_fa_00to02", "lm_fa_00to03", "lm_fa_00to04", "lm_fa_00to05", "lm_fa_00to06", "lm_fa_00to07", "lm_fa_00to08", "lm_fa_00to09", "lm_fa_00to10", "lm_fa_00to11", "lm_fa_00to12", "lm_fa_00to13", "lm_fa_00to14", "lm_fa_00to15")
save(WTSP_predCor_to_forestAge, file="2_pipeline/store/models/WTSP_predCor_to_forestAge.rData")




WTSP_predCor_to_forestAge_df<-purrr::map_df(WTSP_predCor_to_forestAge, broom::glance, .id = 'formula')
save(WTSP_predCor_to_forestAge_df, file="2_pipeline/store/models/WTSP_predCor_to_forestAge_df.rData")

# forest age class
WTSP_predCor_to_forestAgeClass<-list(lm_fac_00to01, lm_fac_00to02, lm_fac_00to03, lm_fac_00to04, lm_fac_00to05, lm_fac_00to06, lm_fac_00to07, lm_fac_00to08, lm_fac_00to09, lm_fac_00to10, lm_fac_00to11, lm_fac_00to12, lm_fac_00to13, lm_fac_00to14, lm_fac_00to15)
names(WTSP_predCor_to_forestAgeClass) <- c("lm_fac_00to01", "lm_fac_00to02", "lm_fac_00to03", "lm_fac_00to04", "lm_fac_00to05", "lm_fac_00to06", "lm_fac_00to07", "lm_fac_00to08", "lm_fac_00to09", "lm_fac_00to10", "lm_fac_00to11", "lm_fac_00to12", "lm_fac_00to13", "lm_fac_00to14", "lm_fac_00to15")
save(WTSP_predCor_to_forestAgeClass, file="2_pipeline/store/models/WTSP_predCor_to_forestAgeClass.rData")




WTSP_predCor_to_forestAgeClass_df<-purrr::map_df(WTSP_predCor_to_forestAgeClass, broom::glance, .id = 'formula')
save(WTSP_predCor_to_forestAgeClass_df, file="2_pipeline/store/models/WTSP_predCor_to_forestAgeClass_df.rData")


#######################################################################################
#### Plot all on the same plot

ggplot() +
  geom_smooth(aes(x = forest_age, y = predCor_0_02, col = "0 and 02 years"), data = WTSP_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_04, col = "0 and 04 years"), data = WTSP_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_06, col = "0 and 06 years"), data = WTSP_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_08, col = "0 and 08 years"), data = WTSP_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_10, col = "0 and 10 years"), data = WTSP_fa_co_df, 
              method = "lm", se = FALSE)+
    scale_colour_manual(name="", 
                      values = c("#fa9fb5","#f768a1","#dd3497","#ae017e","#7a0177")) +
  labs(
    x = "Forest age when LiDAR was acquired", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  ggsave(filename=paste0("3_output/figures/timelag_stats/WTSP_lm_fa_all.png"), width =8, height=5)
  
  
  
ggplot() +
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_01, col = "0 and 01 years"), data = WTSP_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_02, col = "0 and 02 years"), data = WTSP_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_03, col = "0 and 03 years"), data = WTSP_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_04, col = "0 and 04 years"), data = WTSP_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_05, col = "0 and 05 years"), data = WTSP_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_06, col = "0 and 06 years"), data = WTSP_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_07, col = "0 and 07 years"), data = WTSP_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_08, col = "0 and 08 years"), data = WTSP_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_09, col = "0 and 09 years"), data = WTSP_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_10, col = "0 and 10 years"), data = WTSP_fa_co_ac_df, 
              method = "lm", se = FALSE)+
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_11, col = "0 and 11 years"), data = WTSP_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_12, col = "0 and 12 years"), data = WTSP_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_13, col = "0 and 13 years"), data = WTSP_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_14, col = "0 and 14 years"), data = WTSP_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_15, col = "0 and 15 years"), data = WTSP_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  scale_colour_viridis_d()+  
  # scale_colour_manual(name="", 
    #                   values = c("#fa9fb5","#f768a1","#dd3497","#ae017e","#7a0177")) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  ggsave(filename=paste0("3_output/figures/timelag_stats/WTSP_lm_fac_all.png"), width =8, height=5)
  


## ----echo=FALSE, out.width = '100%'-------------------------------------
knitr::include_graphics("3_output/figures/timelag_stats/WTSP_lm_fa_all.png", dpi = 300)


## ----echo=FALSE, message = FALSE, results="asis"------------------------
load("2_pipeline/store/models/WTSP_predCor_to_forestAge_df.rData")

knitr::kable(WTSP_predCor_to_forestAge_df) %>%
  kable_styling(font_size = 10, bootstrap_options= c("striped", "hover", "condensed" ), full_width= F, position="center") %>%
  column_spec(1, width= "20em")%>%
  scroll_box(width = "100%", height = "400px")


## -----------------------------------------------------------------------


WTSP_predCor_stack<-stackOpen("3_output/maps/predictedDistributions/WTSP/predCor/WTSP_predCor_stack.stk")

lidar_forestAge_raster<-raster("0_data/manual/spatialCov/CASFRI/lidar_forestAge_raster.tif")
ageClass_00<-raster("0_data/manual/spatialCov/CASFRI/ageClassRasters/ageClass_00.tif")

lidar_forestAge_raster_pr<-projectRaster(lidar_forestAge_raster, crs=WTSP_predCor_stack)
ageClass_00_pr<-projectRaster(ageClass_00, method='ngb',crs=WTSP_predCor_stack)

#study area
load("0_data/manual/bird/studyarea_big.rData")

pc<- raster::crop(WTSP_predCor_stack, as_Spatial(st_geometry(c_bb)))
fa<- raster::crop(lidar_forestAge_raster_pr, as_Spatial(st_geometry(c_bb)))
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))
#resample so they have the same resolution

pc_rs<-resample(pc, fa)

# Stack covariates
fa_co_ac <- stack(pc_rs, fa, ac)

WTSP_fa_co_ac_df<-as.data.frame(na.omit(values(fa_co_ac)))%>%
  dplyr::rename(forest_age=lidar_forestAge_raster)%>%
  dplyr::rename(forest_age_class=ageClass_00)

WTSP_fa_co_ac_df$forest_age_class<-ordered(as.factor(WTSP_fa_co_ac_df$forest_age_class))

save(WTSP_fa_co_ac_df, file="2_pipeline/store/WTSP_fa_co_ac_df.rData")

load("2_pipeline/store/WTSP_fa_co_ac_df.rData")



WTSP_stack<-stackOpen("3_output/maps/predictedDistributions/WTSP/WTSP_stack.stk")
lidar_forestAge_raster<-raster("0_data/manual/spatialCov/CASFRI/lidar_forestAge_raster.tif")
ageClass_00<-raster("0_data/manual/spatialCov/CASFRI/ageClassRasters/ageClass_00.tif")



lidar_forestAge_raster_pr<-projectRaster(lidar_forestAge_raster, crs=WTSP_stack)
ageClass_00_pr<-projectRaster(ageClass_00, method='ngb',crs=WTSP_stack)

#study area
load("0_data/manual/bird/studyarea_big.rData")
 
pd<- raster::crop(WTSP_stack, as_Spatial(st_geometry(c_bb))) 
fa<- raster::crop(lidar_forestAge_raster_pr, as_Spatial(st_geometry(c_bb))) 
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))

#resample so they have the same resolution
pd_rs<-resample(pd, fa)

# Stack covariates
pd_fa <- stack(pd_rs, fa, ac)

WTSP_pd_fa_df<-as.data.frame(na.omit(values(pd_fa)))%>%
  dplyr::rename(forest_age=lidar_forestAge_raster)
          
save(WTSP_pd_fa_df, file="2_pipeline/store/WTSP_pd_fa_df.rData")

load("2_pipeline/store/WTSP_pd_fa_df.rData")




WTSP_pd_fa_df %>%
  ggplot(aes(WTSP_predictMap_m_00, WTSP_predictMap_m_15, color=forest_age)) +
  geom_point(alpha=0.4, size=.1) +
  scale_color_viridis() +
  labs(y="occupancy probability (15 year time lag)", x="occupancy probability (no time lag)", color="forest age (years)") +
   theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
ggsave(filename=paste0("3_output/figures/timelag_stats/WTSP_scatter_00to15.png"), width = 7, height=5)



WTSP_pd_fa_df %>%
  ggplot(aes(WTSP_predictMap_m_00, WTSP_predictMap_m_15, color=ageClass_00)) +
  geom_point(alpha=0.4, size=.1) +
  scale_color_viridis() +
  labs(y="occupancy probability (15 year time lag)", x="occupancy probability (no time lag)", color="forest age class") +
   theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
ggsave(filename=paste0("3_output/figures/timelag_stats/WTSP_scatter_00to15_class.png"), width = 7, height=5)


## ----echo=FALSE, out.width = '100%'-------------------------------------
knitr::include_graphics("3_output/figures/timelag_stats/WTSP_scatter_00to15.png", dpi = 300)


## ----echo=FALSE, out.width = '100%'-------------------------------------
knitr::include_graphics("3_output/figures/timelag_stats/WTSP_scatter_00to15_class.png", dpi = 300)



## ----child = '1_code/rNotebooks/3_compare/compare_YBSA.Rmd'-------------

## ----include=FALSE, cache=FALSE-----------------------------------------
#Set root directory to R project root
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())


## # set max height of code chunks using the following css styling

## 
## pre {

##   max-height: 300px;

##   overflow-y: auto;

## }

## 
## pre[class] {

##   max-height: 500px;

## }


## ---- message = FALSE---------------------------------------------------



## -----------------------------------------------------------------------
load("2_pipeline/store/models/YBSA_models_AUC_4.rData")

YBSA_models_AUC_4<-YBSA_models_AUC_4%>%
  rename(LiDAR_timelag=lag)

ggplot(YBSA_models_AUC_4, aes(x=LiDAR_timelag, y=AUC_mean)) + 
  geom_errorbar(aes(ymin=AUC_mean-1.96*AUC_stdErr, ymax=AUC_mean+ 1.96*AUC_stdErr, width=.2))+
  geom_point(size=1) + 
  geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
  geom_smooth(method = lm)+
  theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none")
 ggsave(filename=paste0("3_output/figures/timelag_stats/YBSA_AUC_lm_test.png"), width =6, height=5)
  
YBSA_AUC_lm_test<-lm(AUC_mean~ LiDAR_timelag, data=YBSA_models_AUC_4)
save(YBSA_AUC_lm_test,file="2_pipeline/store/models/YBSA_AUC_lm_test.rData")

AUC_cor_test <- cor.test(YBSA_models_AUC_4$AUC_mean, YBSA_models_AUC_4$LiDAR_timelag, method = "pearson")

#########################

ggplot(YBSA_models_AUC_4, aes(x=LiDAR_timelag, y=R2m_mean)) + 
  geom_errorbar(aes(ymin=R2m_mean- 1.96* R2m_stdErr, ymax=R2m_mean+ 1.96* R2m_stdErr, width=.2))+
  geom_point() + 
  geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
  geom_smooth(method = lm) +
  theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none")
 ggsave(filename=paste0("3_output/figures/timelag_stats/YBSA_R2m_test.png"), width =6, height=5)
  
YBSA_R2m_test<-lm(R2m_mean~ LiDAR_timelag, data=YBSA_models_AUC_4)
save(YBSA_R2m_test,file="2_pipeline/store/models/YBSA_R2m_test.rData")

########################
ggplot(YBSA_models_AUC_4, aes(x=LiDAR_timelag, y=R2c_mean)) + 
  geom_errorbar(aes(ymin=R2c_mean- 1.96* R2c_stdErr, ymax=R2c_mean+ 1.96* R2c_stdErr, width=.2))+
  geom_point() + 
  geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
  geom_smooth(method = lm)+
  theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position="none")
 ggsave(filename=paste0("3_output/figures/timelag_stats/YBSA_R2c_test.png"), width =6, height=5)
  
YBSA_R2c_test<-lm(R2c_mean~ LiDAR_timelag, data=YBSA_models_AUC_4)
save(YBSA_R2c_test,file="2_pipeline/store/models/YBSA_R2c_test.rData")



## ----echo=FALSE, out.width = '100%', fig.cap="YBSA AUC time lag"--------
knitr::include_graphics("3_output/figures/timelag_stats/YBSA_AUC_lm_test.png", dpi = 300)


## ----echo=FALSE---------------------------------------------------------
load("2_pipeline/store/models/YBSA_AUC_lm_test.rData")
print(summary(YBSA_AUC_lm_test))


## ----echo=FALSE, out.width = '100%', fig.cap="YBSA R2m time lag"--------
knitr::include_graphics("3_output/figures/timelag_stats/YBSA_R2m_test.png", dpi = 300)


## ----echo=FALSE---------------------------------------------------------
load("2_pipeline/store/models/YBSA_R2m_test.rData")
print(summary(YBSA_R2m_test))


## ----echo=FALSE, out.width = '100%', fig.cap="YBSA R2c time lag"--------
knitr::include_graphics("3_output/figures/timelag_stats/YBSA_R2c_test.png", dpi = 300)


## ----echo=FALSE---------------------------------------------------------
load("2_pipeline/store/models/YBSA_R2c_test.rData")
print(summary(YBSA_R2c_test))


## -----------------------------------------------------------------------
# Load stack of predictive rasters
YBSA_stack<-stackOpen("3_output/maps/predictedDistributions/YBSA/YBSA_stack.stk")

# reclassify to a binary above and below 50% occupancy probability

reclass_df <- c(0, .5, 0,
              .5, 1, 1)

reclass_m <- matrix(reclass_df,
                ncol = 3,
                byrow = TRUE)

YBSA_stack_reclass <- reclassify(YBSA_stack,
                     reclass_m)

# save reclass raster brick (save as .grd to preserve layer names)
writeRaster(YBSA_stack_reclass, "3_output/maps/predictedDistributions/YBSA/YBSA_stack_reclass.grd", overwrite=TRUE, format="raster") 

# load reclass rasters as a brick
YBSA_stack_reclass<-brick("3_output/maps/predictedDistributions/YBSA/YBSA_stack_reclass.grd")



## -----------------------------------------------------------------------
YBSA_stack<-stackOpen("3_output/maps/predictedDistributions/YBSA/YBSA_stack.stk")

predCor_0_01<-rasterCorrelation(YBSA_stack$YBSA_predictMap_m_00, YBSA_stack$YBSA_predictMap_m_01, s = 3, type = "pearson")
predCor_0_02<-rasterCorrelation(YBSA_stack$YBSA_predictMap_m_00, YBSA_stack$YBSA_predictMap_m_02, s = 3, type = "pearson")
predCor_0_03<-rasterCorrelation(YBSA_stack$YBSA_predictMap_m_00, YBSA_stack$YBSA_predictMap_m_03, s = 3, type = "pearson")
predCor_0_04<-rasterCorrelation(YBSA_stack$YBSA_predictMap_m_00, YBSA_stack$YBSA_predictMap_m_04, s = 3, type = "pearson")
predCor_0_05<-rasterCorrelation(YBSA_stack$YBSA_predictMap_m_00, YBSA_stack$YBSA_predictMap_m_05, s = 3, type = "pearson")
predCor_0_06<-rasterCorrelation(YBSA_stack$YBSA_predictMap_m_00, YBSA_stack$YBSA_predictMap_m_06, s = 3, type = "pearson")
predCor_0_07<-rasterCorrelation(YBSA_stack$YBSA_predictMap_m_00, YBSA_stack$YBSA_predictMap_m_07, s = 3, type = "pearson")
predCor_0_08<-rasterCorrelation(YBSA_stack$YBSA_predictMap_m_00, YBSA_stack$YBSA_predictMap_m_08, s = 3, type = "pearson")
predCor_0_09<-rasterCorrelation(YBSA_stack$YBSA_predictMap_m_00, YBSA_stack$YBSA_predictMap_m_09, s = 3, type = "pearson")
predCor_0_10<-rasterCorrelation(YBSA_stack$YBSA_predictMap_m_00, YBSA_stack$YBSA_predictMap_m_10, s = 3, type = "pearson")
predCor_0_11<-rasterCorrelation(YBSA_stack$YBSA_predictMap_m_00, YBSA_stack$YBSA_predictMap_m_11, s = 3, type = "pearson")
predCor_0_12<-rasterCorrelation(YBSA_stack$YBSA_predictMap_m_00, YBSA_stack$YBSA_predictMap_m_12, s = 3, type = "pearson")
predCor_0_13<-rasterCorrelation(YBSA_stack$YBSA_predictMap_m_00, YBSA_stack$YBSA_predictMap_m_13, s = 3, type = "pearson")
predCor_0_14<-rasterCorrelation(YBSA_stack$YBSA_predictMap_m_00, YBSA_stack$YBSA_predictMap_m_14, s = 3, type = "pearson")
predCor_0_15<-rasterCorrelation(YBSA_stack$YBSA_predictMap_m_00, YBSA_stack$YBSA_predictMap_m_15, s = 3, type = "pearson")


writeRaster(predCor_0_01, file="3_output/maps/predictedDistributions/YBSA/predCor/predCor_0_01.tif", overwrite=TRUE)
writeRaster(predCor_0_02, file="3_output/maps/predictedDistributions/YBSA/predCor/predCor_0_02.tif", overwrite=TRUE)
writeRaster(predCor_0_03, file="3_output/maps/predictedDistributions/YBSA/predCor/predCor_0_03.tif", overwrite=TRUE)
writeRaster(predCor_0_04, file="3_output/maps/predictedDistributions/YBSA/predCor/predCor_0_04.tif", overwrite=TRUE)
writeRaster(predCor_0_05, file="3_output/maps/predictedDistributions/YBSA/predCor/predCor_0_05.tif", overwrite=TRUE)
writeRaster(predCor_0_06, file="3_output/maps/predictedDistributions/YBSA/predCor/predCor_0_06.tif", overwrite=TRUE)
writeRaster(predCor_0_07, file="3_output/maps/predictedDistributions/YBSA/predCor/predCor_0_07.tif", overwrite=TRUE)
writeRaster(predCor_0_08, file="3_output/maps/predictedDistributions/YBSA/predCor/predCor_0_08.tif", overwrite=TRUE)
writeRaster(predCor_0_09, file="3_output/maps/predictedDistributions/YBSA/predCor/predCor_0_09.tif", overwrite=TRUE)
writeRaster(predCor_0_10, file="3_output/maps/predictedDistributions/YBSA/predCor/predCor_0_10.tif", overwrite=TRUE)
writeRaster(predCor_0_11, file="3_output/maps/predictedDistributions/YBSA/predCor/predCor_0_11.tif", overwrite=TRUE)
writeRaster(predCor_0_12, file="3_output/maps/predictedDistributions/YBSA/predCor/predCor_0_12.tif", overwrite=TRUE)
writeRaster(predCor_0_13, file="3_output/maps/predictedDistributions/YBSA/predCor/predCor_0_13.tif", overwrite=TRUE)
writeRaster(predCor_0_14, file="3_output/maps/predictedDistributions/YBSA/predCor/predCor_0_14.tif", overwrite=TRUE)
writeRaster(predCor_0_15, file="3_output/maps/predictedDistributions/YBSA/predCor/predCor_0_15.tif", overwrite=TRUE)



map_list<-list.files(path='3_output/maps/predictedDistributions/YBSA/predCor', pattern = "*.tif$", full.names = TRUE)
YBSA_predCor_stack<-raster::stack(map_list)

#save stack as r object
stackSave(YBSA_predCor_stack,file="3_output/maps/predictedDistributions/YBSA/predCor/YBSA_predCor_stack.stk")



## -----------------------------------------------------------------------


YBSA_predCor_stack<-stackOpen("3_output/maps/predictedDistributions/YBSA/predCor/YBSA_predCor_stack.stk")

lidar_forestAge_raster<-raster("0_data/manual/spatialCov/CASFRI/lidar_forestAge_raster.tif")
ageClass_00<-raster("0_data/manual/spatialCov/CASFRI/ageClassRasters/ageClass_00.tif")

lidar_forestAge_raster_pr<-projectRaster(lidar_forestAge_raster, crs=YBSA_predCor_stack)
ageClass_00_pr<-projectRaster(ageClass_00, method='ngb',crs=YBSA_predCor_stack)

#study area
load("0_data/manual/bird/studyarea_big.rData")

pc<- raster::crop(YBSA_predCor_stack, as_Spatial(st_geometry(c_bb)))
fa<- raster::crop(lidar_forestAge_raster_pr, as_Spatial(st_geometry(c_bb)))
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))
#resample so they have the same resolution
pc_rs<-resample(pc, fa)

# Stack covariates
fa_co_ac <- stack(pc_rs, fa, ac)

YBSA_fa_co_ac_df<-as.data.frame(na.omit(values(fa_co_ac)))%>%
  dplyr::rename(forest_age=lidar_forestAge_raster)%>%
  dplyr::rename(forest_age_class=ageClass_00)

YBSA_fa_co_ac_df$forest_age_class<-ordered(as.factor(YBSA_fa_co_ac_df$forest_age_class))

save(YBSA_fa_co_ac_df, file="2_pipeline/store/YBSA_fa_co_ac_df.rData")

load("2_pipeline/store/YBSA_fa_co_ac_df.rData")


#######################################################################################
############### Plot Effect ######################################################
#######################################################################################
lm_fa_00to01 <- lm(predCor_0_01 ~ forest_age, data=YBSA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to01, terms="forest_age [all]", data=YBSA_fa_co_ac_df)
png("3_output/figures/timelag_stats/YBSA_lm_fa_00to01.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to01 <- lm(predCor_0_01 ~ forest_age_class, data=YBSA_fa_co_ac_df)
ggplot(YBSA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_01) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/YBSA_lm_fac_00to01.png"), width =8, height=5)

#######################################################################################

lm_fa_00to02 <- lm(predCor_0_02 ~ forest_age, data=YBSA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to02, terms="forest_age [all]", data=YBSA_fa_co_ac_df)
png("3_output/figures/timelag_stats/YBSA_lm_fa_00to02.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
# age class
lm_fac_00to02 <- lm(predCor_0_02 ~ forest_age_class, data=YBSA_fa_co_ac_df)
ggplot(YBSA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_02) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
ggsave(filename=paste0("3_output/figures/timelag_stats/YBSA_lm_fac_00to02.png"), width =8, height=5)  
  
#######################################################################################
lm_fa_00to03 <- lm(predCor_0_03 ~ forest_age, data=YBSA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to03, terms="forest_age [all]", data=YBSA_fa_co_ac_df)
png("3_output/figures/timelag_stats/YBSA_lm_fa_00to03.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to03 <- lm(predCor_0_03 ~ forest_age_class, data=YBSA_fa_co_ac_df)
ggplot(YBSA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_03) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/YBSA_lm_fac_00to03.png"), width =8, height=5)


#######################################################################################
lm_fa_00to04 <- lm(predCor_0_04 ~ forest_age, data=YBSA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to04, terms="forest_age [all]", data=YBSA_fa_co_ac_df)
png("3_output/figures/timelag_stats/YBSA_lm_fa_00to04.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to04 <- lm(predCor_0_04 ~ forest_age_class, data=YBSA_fa_co_ac_df)
ggplot(YBSA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_04) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())  
  ggsave(filename=paste0("3_output/figures/timelag_stats/YBSA_lm_fac_00to04.png"), width =8, height=5)    
  
#######################################################################################
lm_fa_00to05 <- lm(predCor_0_05 ~ forest_age, data=YBSA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to05, terms="forest_age [all]", data=YBSA_fa_co_ac_df)
png("3_output/figures/timelag_stats/YBSA_lm_fa_00to05.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to05 <- lm(predCor_0_05 ~ forest_age_class, data=YBSA_fa_co_ac_df)
ggplot(YBSA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_05) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/YBSA_lm_fac_00to05.png"), width =8, height=5)
  
  
  
#######################################################################################
lm_fa_00to06 <- lm(predCor_0_06 ~ forest_age, data=YBSA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to06, terms="forest_age [all]", data=YBSA_fa_co_ac_df)
png("3_output/figures/timelag_stats/YBSA_lm_fa_00to06.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to06 <- lm(predCor_0_06 ~ forest_age_class, data=YBSA_fa_co_ac_df)
ggplot(YBSA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_06) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/YBSA_lm_fac_00to06.png"), width =8, height=5)
  
  
  
  
  
  
  #######################################################################################
lm_fa_00to07 <- lm(predCor_0_07 ~ forest_age, data=YBSA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to07, terms="forest_age [all]", data=YBSA_fa_co_ac_df)
png("3_output/figures/timelag_stats/YBSA_lm_fa_00to07.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to07 <- lm(predCor_0_07 ~ forest_age_class, data=YBSA_fa_co_ac_df)
ggplot(YBSA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_07) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/YBSA_lm_fac_00to07.png"), width =8, height=5)
  
  
  
  
  
  #######################################################################################
lm_fa_00to08 <- lm(predCor_0_08 ~ forest_age, data=YBSA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to08, terms="forest_age [all]", data=YBSA_fa_co_ac_df)
png("3_output/figures/timelag_stats/YBSA_lm_fa_00to08.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to08 <- lm(predCor_0_08 ~ forest_age_class, data=YBSA_fa_co_ac_df)
ggplot(YBSA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_08) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/YBSA_lm_fac_00to08.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to09 <- lm(predCor_0_09 ~ forest_age, data=YBSA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to09, terms="forest_age [all]", data=YBSA_fa_co_ac_df)
png("3_output/figures/timelag_stats/YBSA_lm_fa_00to09.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to09 <- lm(predCor_0_09 ~ forest_age_class, data=YBSA_fa_co_ac_df)
ggplot(YBSA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_09) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/YBSA_lm_fac_00to09.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to10 <- lm(predCor_0_10 ~ forest_age, data=YBSA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to10, terms="forest_age [all]", data=YBSA_fa_co_ac_df)
png("3_output/figures/timelag_stats/YBSA_lm_fa_00to10.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to10 <- lm(predCor_0_10 ~ forest_age_class, data=YBSA_fa_co_ac_df)
ggplot(YBSA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_10) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/YBSA_lm_fac_00to10.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to11 <- lm(predCor_0_11 ~ forest_age, data=YBSA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to11, terms="forest_age [all]", data=YBSA_fa_co_ac_df)
png("3_output/figures/timelag_stats/YBSA_lm_fa_00to11.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to11 <- lm(predCor_0_11 ~ forest_age_class, data=YBSA_fa_co_ac_df)
ggplot(YBSA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_11) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/YBSA_lm_fac_00to11.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to12 <- lm(predCor_0_12 ~ forest_age, data=YBSA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to12, terms="forest_age [all]", data=YBSA_fa_co_ac_df)
png("3_output/figures/timelag_stats/YBSA_lm_fa_00to12.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to12 <- lm(predCor_0_12 ~ forest_age_class, data=YBSA_fa_co_ac_df)
ggplot(YBSA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_12) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/YBSA_lm_fac_00to12.png"), width =8, height=5)
  
  
  
  
  #######################################################################################
lm_fa_00to13 <- lm(predCor_0_13 ~ forest_age, data=YBSA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to13, terms="forest_age [all]", data=YBSA_fa_co_ac_df)
png("3_output/figures/timelag_stats/YBSA_lm_fa_00to13.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to13 <- lm(predCor_0_13 ~ forest_age_class, data=YBSA_fa_co_ac_df)
ggplot(YBSA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_13) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/YBSA_lm_fac_00to13.png"), width =8, height=5)

  
  
  #######################################################################################
lm_fa_00to14 <- lm(predCor_0_14 ~ forest_age, data=YBSA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to14, terms="forest_age [all]", data=YBSA_fa_co_ac_df)
png("3_output/figures/timelag_stats/YBSA_lm_fa_00to14.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to14 <- lm(predCor_0_14 ~ forest_age_class, data=YBSA_fa_co_ac_df)
ggplot(YBSA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_14) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/YBSA_lm_fac_00to14.png"), width =8, height=5)
  
  
  
  #######################################################################################
lm_fa_00to15 <- lm(predCor_0_15 ~ forest_age, data=YBSA_fa_co_ac_df)
t<-ggpredict(lm_fa_00to15, terms="forest_age [all]", data=YBSA_fa_co_ac_df)
png("3_output/figures/timelag_stats/YBSA_lm_fa_00to15.png", width = 800, height = 600, units = "px", pointsize = 12)
plot(t) + labs(
    x = "Forest age when LiDAR was acquired.", 
    y = "Pearson correlation (r)",
    title=""
  )
dev.off()

# age class
lm_fac_00to15 <- lm(predCor_0_15 ~ forest_age_class, data=YBSA_fa_co_ac_df)
ggplot(YBSA_fa_co_ac_df) +
  aes(x = forest_age_class, y = predCor_0_15) +
  geom_smooth(aes(x = unclass(forest_age_class), color = "1"), 
              formula = y ~ x, 
              method = lm, se = FALSE, show.legend = FALSE) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  
  ggsave(filename=paste0("3_output/figures/timelag_stats/YBSA_lm_fac_00to15.png"), width =8, height=5)
  

#######################################################################################
#### save models
#######################################################################################

# save models

# forest age
YBSA_predCor_to_forestAge <-list(lm_fa_00to01, lm_fa_00to02, lm_fa_00to03, lm_fa_00to04, lm_fa_00to05, lm_fa_00to06, lm_fa_00to07, lm_fa_00to08, lm_fa_00to09, lm_fa_00to10, lm_fa_00to11, lm_fa_00to12, lm_fa_00to13, lm_fa_00to14, lm_fa_00to15)
names(YBSA_predCor_to_forestAge) <- c("lm_fa_00to01", "lm_fa_00to02", "lm_fa_00to03", "lm_fa_00to04", "lm_fa_00to05", "lm_fa_00to06", "lm_fa_00to07", "lm_fa_00to08", "lm_fa_00to09", "lm_fa_00to10", "lm_fa_00to11", "lm_fa_00to12", "lm_fa_00to13", "lm_fa_00to14", "lm_fa_00to15")
save(YBSA_predCor_to_forestAge, file="2_pipeline/store/models/YBSA_predCor_to_forestAge.rData")




YBSA_predCor_to_forestAge_df<-purrr::map_df(YBSA_predCor_to_forestAge, broom::glance, .id = 'formula')
save(YBSA_predCor_to_forestAge_df, file="2_pipeline/store/models/YBSA_predCor_to_forestAge_df.rData")

# forest age class
YBSA_predCor_to_forestAgeClass<-list(lm_fac_00to01, lm_fac_00to02, lm_fac_00to03, lm_fac_00to04, lm_fac_00to05, lm_fac_00to06, lm_fac_00to07, lm_fac_00to08, lm_fac_00to09, lm_fac_00to10, lm_fac_00to11, lm_fac_00to12, lm_fac_00to13, lm_fac_00to14, lm_fac_00to15)
names(YBSA_predCor_to_forestAgeClass) <- c("lm_fac_00to01", "lm_fac_00to02", "lm_fac_00to03", "lm_fac_00to04", "lm_fac_00to05", "lm_fac_00to06", "lm_fac_00to07", "lm_fac_00to08", "lm_fac_00to09", "lm_fac_00to10", "lm_fac_00to11", "lm_fac_00to12", "lm_fac_00to13", "lm_fac_00to14", "lm_fac_00to15")
save(YBSA_predCor_to_forestAgeClass, file="2_pipeline/store/models/YBSA_predCor_to_forestAgeClass.rData")




YBSA_predCor_to_forestAgeClass_df<-purrr::map_df(YBSA_predCor_to_forestAgeClass, broom::glance, .id = 'formula')
save(YBSA_predCor_to_forestAgeClass_df, file="2_pipeline/store/models/YBSA_predCor_to_forestAgeClass_df.rData")


#######################################################################################
#### Plot all on the same plot

ggplot() +
  geom_smooth(aes(x = forest_age, y = predCor_0_02, col = "0 and 02 years"), data = YBSA_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_04, col = "0 and 04 years"), data = YBSA_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_06, col = "0 and 06 years"), data = YBSA_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_08, col = "0 and 08 years"), data = YBSA_fa_co_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = forest_age, y = predCor_0_10, col = "0 and 10 years"), data = YBSA_fa_co_df, 
              method = "lm", se = FALSE)+
    scale_colour_manual(name="", 
                      values = c("#fa9fb5","#f768a1","#dd3497","#ae017e","#7a0177")) +
  labs(
    x = "Forest age when LiDAR was acquired", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  ggsave(filename=paste0("3_output/figures/timelag_stats/YBSA_lm_fa_all.png"), width =8, height=5)
  
  
  
ggplot() +
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_01, col = "0 and 01 years"), data = YBSA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_02, col = "0 and 02 years"), data = YBSA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_03, col = "0 and 03 years"), data = YBSA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_04, col = "0 and 04 years"), data = YBSA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_05, col = "0 and 05 years"), data = YBSA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_06, col = "0 and 06 years"), data = YBSA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_07, col = "0 and 07 years"), data = YBSA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_08, col = "0 and 08 years"), data = YBSA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_09, col = "0 and 09 years"), data = YBSA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_10, col = "0 and 10 years"), data = YBSA_fa_co_ac_df, 
              method = "lm", se = FALSE)+
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_11, col = "0 and 11 years"), data = YBSA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_12, col = "0 and 12 years"), data = YBSA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_13, col = "0 and 13 years"), data = YBSA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_14, col = "0 and 14 years"), data = YBSA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
   geom_smooth(aes(x = unclass(forest_age_class), y = predCor_0_15, col = "0 and 15 years"), data = YBSA_fa_co_ac_df, 
              method = "lm", se = FALSE) + 
  scale_colour_viridis_d()+  
  # scale_colour_manual(name="", 
    #                   values = c("#fa9fb5","#f768a1","#dd3497","#ae017e","#7a0177")) +
  labs(
    x = "Forest age class", 
    y = "Pearson correlation (r)")+
    theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
         legend.title = element_blank())  
  ggsave(filename=paste0("3_output/figures/timelag_stats/YBSA_lm_fac_all.png"), width =8, height=5)
  


## ----echo=FALSE, out.width = '100%'-------------------------------------
knitr::include_graphics("3_output/figures/timelag_stats/YBSA_lm_fa_all.png", dpi = 300)


## ----echo=FALSE, message = FALSE, results="asis"------------------------
load("2_pipeline/store/models/YBSA_predCor_to_forestAge_df.rData")

knitr::kable(YBSA_predCor_to_forestAge_df) %>%
  kable_styling(font_size = 10, bootstrap_options= c("striped", "hover", "condensed" ), full_width= F, position="center") %>%
  column_spec(1, width= "20em")%>%
  scroll_box(width = "100%", height = "400px")


## -----------------------------------------------------------------------


YBSA_predCor_stack<-stackOpen("3_output/maps/predictedDistributions/YBSA/predCor/YBSA_predCor_stack.stk")

lidar_forestAge_raster<-raster("0_data/manual/spatialCov/CASFRI/lidar_forestAge_raster.tif")
ageClass_00<-raster("0_data/manual/spatialCov/CASFRI/ageClassRasters/ageClass_00.tif")

lidar_forestAge_raster_pr<-projectRaster(lidar_forestAge_raster, crs=YBSA_predCor_stack)
ageClass_00_pr<-projectRaster(ageClass_00, method='ngb',crs=YBSA_predCor_stack)

#study area
load("0_data/manual/bird/studyarea_big.rData")

pc<- raster::crop(YBSA_predCor_stack, as_Spatial(st_geometry(c_bb)))
fa<- raster::crop(lidar_forestAge_raster_pr, as_Spatial(st_geometry(c_bb)))
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))
#resample so they have the same resolution

pc_rs<-resample(pc, fa)

# Stack covariates
fa_co_ac <- stack(pc_rs, fa, ac)

YBSA_fa_co_ac_df<-as.data.frame(na.omit(values(fa_co_ac)))%>%
  dplyr::rename(forest_age=lidar_forestAge_raster)%>%
  dplyr::rename(forest_age_class=ageClass_00)

YBSA_fa_co_ac_df$forest_age_class<-ordered(as.factor(YBSA_fa_co_ac_df$forest_age_class))

save(YBSA_fa_co_ac_df, file="2_pipeline/store/YBSA_fa_co_ac_df.rData")

load("2_pipeline/store/YBSA_fa_co_ac_df.rData")



YBSA_stack<-stackOpen("3_output/maps/predictedDistributions/YBSA/YBSA_stack.stk")
lidar_forestAge_raster<-raster("0_data/manual/spatialCov/CASFRI/lidar_forestAge_raster.tif")
ageClass_00<-raster("0_data/manual/spatialCov/CASFRI/ageClassRasters/ageClass_00.tif")



lidar_forestAge_raster_pr<-projectRaster(lidar_forestAge_raster, crs=YBSA_stack)
ageClass_00_pr<-projectRaster(ageClass_00, method='ngb',crs=YBSA_stack)

#study area
load("0_data/manual/bird/studyarea_big.rData")
 
pd<- raster::crop(YBSA_stack, as_Spatial(st_geometry(c_bb))) 
fa<- raster::crop(lidar_forestAge_raster_pr, as_Spatial(st_geometry(c_bb))) 
ac<- raster::crop(ageClass_00_pr, as_Spatial(st_geometry(c_bb)))

#resample so they have the same resolution
pd_rs<-resample(pd, fa)

# Stack covariates
pd_fa <- stack(pd_rs, fa, ac)

YBSA_pd_fa_df<-as.data.frame(na.omit(values(pd_fa)))%>%
  dplyr::rename(forest_age=lidar_forestAge_raster)
          
save(YBSA_pd_fa_df, file="2_pipeline/store/YBSA_pd_fa_df.rData")

load("2_pipeline/store/YBSA_pd_fa_df.rData")




YBSA_pd_fa_df %>%
  ggplot(aes(YBSA_predictMap_m_00, YBSA_predictMap_m_15, color=forest_age)) +
  geom_point(alpha=0.4, size=.1) +
  scale_color_viridis() +
  labs(y="occupancy probability (15 year time lag)", x="occupancy probability (no time lag)", color="forest age (years)") +
   theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
ggsave(filename=paste0("3_output/figures/timelag_stats/YBSA_scatter_00to15.png"), width = 7, height=5)



YBSA_pd_fa_df %>%
  ggplot(aes(YBSA_predictMap_m_00, YBSA_predictMap_m_15, color=ageClass_00)) +
  geom_point(alpha=0.4, size=.1) +
  scale_color_viridis() +
  labs(y="occupancy probability (15 year time lag)", x="occupancy probability (no time lag)", color="forest age class") +
   theme(panel.background = element_blank(),
          text = element_text(size=10),
          axis.ticks=element_blank(),
          #strip.background =element_rect(fill="white"),
         panel.border = element_rect(color = "black", fill = NA, size = .2),
          axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
          #panel.background = element_rect(fill="white"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
ggsave(filename=paste0("3_output/figures/timelag_stats/YBSA_scatter_00to15_class.png"), width = 7, height=5)


## ----echo=FALSE, out.width = '100%'-------------------------------------
knitr::include_graphics("3_output/figures/timelag_stats/YBSA_scatter_00to15.png", dpi = 300)


## ----echo=FALSE, out.width = '100%'-------------------------------------
knitr::include_graphics("3_output/figures/timelag_stats/YBSA_scatter_00to15_class.png", dpi = 300)


