
library(dplyr)
library(lme4)
library(car)
library(MuMIn)
library(raster)
library(jtools)
library(effects)
library(DHARMa)
library(pROC)


load("2_pipeline/tmp/sdm_d3.rData")
d3<-sdm_d3

scaled_stack<-stackOpen("0_data/manual/spatialCov/scaled_cov/scaled_stack2.stk")
names(scaled_stack)[78:97]<-c('ndvi_lag_19', 'ndvi_lag_18', 'ndvi_lag_17', 'ndvi_lag_16', 'ndvi_lag_15', 'ndvi_lag_14', 'ndvi_lag_13', 'ndvi_lag_12', 'ndvi_lag_11', 'ndvi_lag_10', 'ndvi_lag_9', 'ndvi_lag_8', 'ndvi_lag_7', 'ndvi_lag_6', 'ndvi_lag_5', 'ndvi_lag_4', 'ndvi_lag_3', 'ndvi_lag_2', 'ndvi_lag_1', 'ndvi_lag_0')



####################################################################################################################################
##AMRE##############################################################################################################################
####################################################################################################################################
#stats function for AUC

#dd<-d3%>%filter(SS_lidar_timelag==0)

dd<-d3%>%filter(SS_lidar_timelag==0)


# AUCFun <- function(x) {
#   library(pROC)
#   pred<-predict(x, type="response", allow.new.levels=TRUE)
#   AUC<-as.numeric(auc(d_0$AMRE_OCC, pred))
# }


#both AUC and R2
# AUCFun <- function(fit) {
#   library(pROC)
#   pred<-predict(fit, type="response", allow.new.levels=TRUE)
#   R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$AMRE_OCC, pred))),3)
#   }
#   
AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$AMRE_OCC, pred))),3)
}


#models
md_0<-lme4::glmer(AMRE_OCC ~ ndvi_lag_0 +  elev_2pnt00_to_4pnt00_return_proportion +  elev_cv+ elev_maximum +  elev_p50 +  (1|SS) , data=dd, family = binomial, offset = AMRE_OFF,  na.action = na.fail, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)))


md_0_bootAUC<-bootMer(md_0, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

# meanAUC<-mean(md_0_bootAUC$t)
# std.err<-sd(md_0_bootAUC$t)/sqrt(length(md_0_bootAUC$t))
# CI.lower <- meanAUC - std.err*1.96
# CI.upper <- meanAUC + std.err*1.96
# 
# md_0_meanAUC<-data.frame(meanAUC, std.err, CI.lower, CI.upper)

R2m_mean<-mean(md_0_bootAUC$t[,1])
R2m_stdErr<-sd(md_0_bootAUC$t[,1])/sqrt(length(md_0_bootAUC$t[,1]))
R2c_mean<-mean(md_0_bootAUC$t[,2])
R2c_stdErr<-sd(md_0_bootAUC$t[,2])/sqrt(length(md_0_bootAUC$t[,2]))
AUC_mean<-mean(md_0_bootAUC$t[,3])
AUC_stdErr<-sd(md_0_bootAUC$t[,3])/sqrt(length(md_0_bootAUC$t[,3]))

R2m_sd<-sd(md_0_bootAUC$t[,1])
R2c_sd<-sd(md_0_bootAUC$t[,2])
AUC_sd<-sd(md_0_bootAUC$t[,3])

md_0_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)


AMRE_predictMap_m_0<-raster::predict(scaled_stack, md_0, re.form=NA, type='response')
writeRaster(AMRE_predictMap_m_0,file="3_output/maps/predictedDistributions/AMRE/AMRE_predictMap_m_00", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-1)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$AMRE_OCC, pred))),3)
}

md_1<-lme4::glmer(AMRE_OCC ~ ndvi_lag_1 +  elev_2pnt00_to_4pnt00_return_proportion +  elev_cv+ elev_maximum +  elev_p50 +  (1|SS) , data=dd, family = binomial, offset = AMRE_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_1_bootAUC<-bootMer(md_1, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_1_bootAUC$t[,1])
R2m_stdErr<-sd(md_1_bootAUC$t[,1])/sqrt(length(md_1_bootAUC$t[,1]))
R2m_sd<-sd(md_1_bootAUC$t[,1])
R2c_mean<-mean(md_1_bootAUC$t[,2])
R2c_stdErr<-sd(md_1_bootAUC$t[,2])/sqrt(length(md_1_bootAUC$t[,2]))
R2c_sd<-sd(md_1_bootAUC$t[,2])
AUC_mean<-mean(md_1_bootAUC$t[,3])
AUC_stdErr<-sd(md_1_bootAUC$t[,3])/sqrt(length(md_1_bootAUC$t[,3]))
AUC_sd<-sd(md_1_bootAUC$t[,3])

R2m_sd<-sd(md_1_bootAUC$t[,1])
R2c_sd<-sd(md_1_bootAUC$t[,2])
AUC_sd<-sd(md_1_bootAUC$t[,3])

md_1_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

AMRE_predictMap_m_01<-raster::predict(scaled_stack, md_1, re.form=NA, type='response')
writeRaster(AMRE_predictMap_m_01,file="3_output/maps/predictedDistributions/AMRE/AMRE_predictMap_m_01", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-2)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$AMRE_OCC, pred))),3)
}

md_2<-lme4::glmer(AMRE_OCC ~ ndvi_lag_2 +  elev_2pnt00_to_4pnt00_return_proportion +  elev_cv+ elev_maximum +  elev_p50 +  (1|SS) , data=dd, family = binomial, offset = AMRE_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_2_bootAUC<-bootMer(md_2, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_2_bootAUC$t[,1])
R2m_stdErr<-sd(md_2_bootAUC$t[,1])/sqrt(length(md_2_bootAUC$t[,1]))
R2c_mean<-mean(md_2_bootAUC$t[,2])
R2c_stdErr<-sd(md_2_bootAUC$t[,2])/sqrt(length(md_2_bootAUC$t[,2]))
AUC_mean<-mean(md_2_bootAUC$t[,3])
AUC_stdErr<-sd(md_2_bootAUC$t[,3])/sqrt(length(md_2_bootAUC$t[,3]))

R2m_sd<-sd(md_2_bootAUC$t[,1])
R2c_sd<-sd(md_2_bootAUC$t[,2])
AUC_sd<-sd(md_2_bootAUC$t[,3])

md_2_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

AMRE_predictMap_m_02<-raster::predict(scaled_stack, md_2, re.form=NA, type='response')
writeRaster(AMRE_predictMap_m_02,file="3_output/maps/predictedDistributions/AMRE/AMRE_predictMap_m_02", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-3)


AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$AMRE_OCC, pred))),3)
}

md_3<-lme4::glmer(AMRE_OCC ~ ndvi_lag_3 +  elev_2pnt00_to_4pnt00_return_proportion +  elev_cv+ elev_maximum +  elev_p50 +  (1|SS) , data=dd, family = binomial, offset = AMRE_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_3_bootAUC<-bootMer(md_3, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_3_bootAUC$t[,1])
R2m_stdErr<-sd(md_3_bootAUC$t[,1])/sqrt(length(md_3_bootAUC$t[,1]))
R2c_mean<-mean(md_3_bootAUC$t[,2])
R2c_stdErr<-sd(md_3_bootAUC$t[,2])/sqrt(length(md_3_bootAUC$t[,2]))
AUC_mean<-mean(md_3_bootAUC$t[,3])
AUC_stdErr<-sd(md_3_bootAUC$t[,3])/sqrt(length(md_3_bootAUC$t[,3]))

R2m_sd<-sd(md_3_bootAUC$t[,1])
R2c_sd<-sd(md_3_bootAUC$t[,2])
AUC_sd<-sd(md_3_bootAUC$t[,3])

md_3_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

AMRE_predictMap_m_03<-raster::predict(scaled_stack, md_3, re.form=NA, type='response')
writeRaster(AMRE_predictMap_m_03,file="3_output/maps/predictedDistributions/AMRE/AMRE_predictMap_m_03", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-4)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$AMRE_OCC, pred))),3)
}

md_4<-lme4::glmer(AMRE_OCC ~ ndvi_lag_4 +  elev_2pnt00_to_4pnt00_return_proportion +  elev_cv+ elev_maximum +  elev_p50 +  (1|SS) , data=dd, family = binomial, offset = AMRE_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_4_bootAUC<-bootMer(md_4, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_4_bootAUC$t[,1])
R2m_stdErr<-sd(md_4_bootAUC$t[,1])/sqrt(length(md_4_bootAUC$t[,1]))
R2c_mean<-mean(md_4_bootAUC$t[,2])
R2c_stdErr<-sd(md_4_bootAUC$t[,2])/sqrt(length(md_4_bootAUC$t[,2]))
AUC_mean<-mean(md_4_bootAUC$t[,3])
AUC_stdErr<-sd(md_4_bootAUC$t[,3])/sqrt(length(md_4_bootAUC$t[,3]))

R2m_sd<-sd(md_4_bootAUC$t[,1])
R2c_sd<-sd(md_4_bootAUC$t[,2])
AUC_sd<-sd(md_4_bootAUC$t[,3])

md_4_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

AMRE_predictMap_m_04<-raster::predict(scaled_stack, md_4, re.form=NA, type='response')
writeRaster(AMRE_predictMap_m_04,file="3_output/maps/predictedDistributions/AMRE/AMRE_predictMap_m_04", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-5)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$AMRE_OCC, pred))),3)
}

md_5<-lme4::glmer(AMRE_OCC ~ ndvi_lag_5 +  elev_2pnt00_to_4pnt00_return_proportion +  elev_cv+ elev_maximum +  elev_p50 +  (1|SS) , data=dd, family = binomial, offset = AMRE_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_5_bootAUC<-bootMer(md_5, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_5_bootAUC$t[,1])
R2m_stdErr<-sd(md_5_bootAUC$t[,1])/sqrt(length(md_5_bootAUC$t[,1]))
R2c_mean<-mean(md_5_bootAUC$t[,2])
R2c_stdErr<-sd(md_5_bootAUC$t[,2])/sqrt(length(md_5_bootAUC$t[,2]))
AUC_mean<-mean(md_5_bootAUC$t[,3])
AUC_stdErr<-sd(md_5_bootAUC$t[,3])/sqrt(length(md_5_bootAUC$t[,3]))

R2m_sd<-sd(md_5_bootAUC$t[,1])
R2c_sd<-sd(md_5_bootAUC$t[,2])
AUC_sd<-sd(md_5_bootAUC$t[,3])

md_5_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

AMRE_predictMap_m_05<-raster::predict(scaled_stack, md_5, re.form=NA, type='response')
writeRaster(AMRE_predictMap_m_05,file="3_output/maps/predictedDistributions/AMRE/AMRE_predictMap_m_05", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-6)


AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$AMRE_OCC, pred))),3)
}

md_6<-lme4::glmer(AMRE_OCC ~ ndvi_lag_6 +  elev_2pnt00_to_4pnt00_return_proportion +  elev_cv+ elev_maximum +  elev_p50 +  (1|SS) , data=dd, family = binomial, offset = AMRE_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_6_bootAUC<-bootMer(md_6, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_6_bootAUC$t[,1])
R2m_stdErr<-sd(md_6_bootAUC$t[,1])/sqrt(length(md_6_bootAUC$t[,1]))
R2c_mean<-mean(md_6_bootAUC$t[,2])
R2c_stdErr<-sd(md_6_bootAUC$t[,2])/sqrt(length(md_6_bootAUC$t[,2]))
AUC_mean<-mean(md_6_bootAUC$t[,3])
AUC_stdErr<-sd(md_6_bootAUC$t[,3])/sqrt(length(md_6_bootAUC$t[,3]))

R2m_sd<-sd(md_6_bootAUC$t[,1])
R2c_sd<-sd(md_6_bootAUC$t[,2])
AUC_sd<-sd(md_6_bootAUC$t[,3])

md_6_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)


AMRE_predictMap_m_06<-raster::predict(scaled_stack, md_6, re.form=NA, type='response')
writeRaster(AMRE_predictMap_m_06,file="3_output/maps/predictedDistributions/AMRE/AMRE_predictMap_m_06", format="GTiff", overwrite=T)

##########################
dd<-d3%>%filter(SS_lidar_timelag==-7)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$AMRE_OCC, pred))),3)
}

md_7<-lme4::glmer(AMRE_OCC ~ ndvi_lag_7 +  elev_2pnt00_to_4pnt00_return_proportion +  elev_cv+ elev_maximum +  elev_p50 +  (1|SS) , data=dd, family = binomial, offset = AMRE_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_7_bootAUC<-bootMer(md_7, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_7_bootAUC$t[,1])
R2m_stdErr<-sd(md_7_bootAUC$t[,1])/sqrt(length(md_7_bootAUC$t[,1]))
R2c_mean<-mean(md_7_bootAUC$t[,2])
R2c_stdErr<-sd(md_7_bootAUC$t[,2])/sqrt(length(md_7_bootAUC$t[,2]))
AUC_mean<-mean(md_7_bootAUC$t[,3])
AUC_stdErr<-sd(md_7_bootAUC$t[,3])/sqrt(length(md_7_bootAUC$t[,3]))

R2m_sd<-sd(md_7_bootAUC$t[,1])
R2c_sd<-sd(md_7_bootAUC$t[,2])
AUC_sd<-sd(md_7_bootAUC$t[,3])

md_7_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

AMRE_predictMap_m_07<-raster::predict(scaled_stack, md_7, re.form=NA, type='response')
writeRaster(AMRE_predictMap_m_07,file="3_output/maps/predictedDistributions/AMRE/AMRE_predictMap_m_07", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-8)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$AMRE_OCC, pred))),3)
}

md_8<-lme4::glmer(AMRE_OCC ~ ndvi_lag_8 +  elev_2pnt00_to_4pnt00_return_proportion +  elev_cv+ elev_maximum +  elev_p50 +  (1|SS) , data=dd, family = binomial, offset = AMRE_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_8_bootAUC<-bootMer(md_8, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_8_bootAUC$t[,1])
R2m_stdErr<-sd(md_8_bootAUC$t[,1])/sqrt(length(md_8_bootAUC$t[,1]))
R2c_mean<-mean(md_8_bootAUC$t[,2])
R2c_stdErr<-sd(md_8_bootAUC$t[,2])/sqrt(length(md_8_bootAUC$t[,2]))
AUC_mean<-mean(md_8_bootAUC$t[,3])
AUC_stdErr<-sd(md_8_bootAUC$t[,3])/sqrt(length(md_8_bootAUC$t[,3]))

R2m_sd<-sd(md_8_bootAUC$t[,1])
R2c_sd<-sd(md_8_bootAUC$t[,2])
AUC_sd<-sd(md_8_bootAUC$t[,3])

md_8_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

AMRE_predictMap_m_08<-raster::predict(scaled_stack, md_8, re.form=NA, type='response')
writeRaster(AMRE_predictMap_m_08,file="3_output/maps/predictedDistributions/AMRE/AMRE_predictMap_m_08", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-9)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$AMRE_OCC, pred))),3)
}

md_9<-lme4::glmer(AMRE_OCC ~ ndvi_lag_9 +  elev_2pnt00_to_4pnt00_return_proportion +  elev_cv+ elev_maximum +  elev_p50 +  (1|SS) , data=dd, family = binomial, offset = AMRE_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_9_bootAUC<-bootMer(md_9, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_9_bootAUC$t[,1])
R2m_stdErr<-sd(md_9_bootAUC$t[,1])/sqrt(length(md_9_bootAUC$t[,1]))
R2c_mean<-mean(md_9_bootAUC$t[,2])
R2c_stdErr<-sd(md_9_bootAUC$t[,2])/sqrt(length(md_9_bootAUC$t[,2]))
AUC_mean<-mean(md_9_bootAUC$t[,3])
AUC_stdErr<-sd(md_9_bootAUC$t[,3])/sqrt(length(md_9_bootAUC$t[,3]))

R2m_sd<-sd(md_9_bootAUC$t[,1])
R2c_sd<-sd(md_9_bootAUC$t[,2])
AUC_sd<-sd(md_9_bootAUC$t[,3])

md_9_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)


AMRE_predictMap_m_09<-raster::predict(scaled_stack, md_9, re.form=NA, type='response')
writeRaster(AMRE_predictMap_m_09,file="3_output/maps/predictedDistributions/AMRE/AMRE_predictMap_m_09", format="GTiff", overwrite=T)

##########################
dd<-d3%>%filter(SS_lidar_timelag==-10)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$AMRE_OCC, pred))),3)
}

md_10<-lme4::glmer(AMRE_OCC ~ ndvi_lag_10 +  elev_2pnt00_to_4pnt00_return_proportion +  elev_cv+ elev_maximum +  elev_p50 +  (1|SS) , data=dd, family = binomial, offset = AMRE_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_10_bootAUC<-bootMer(md_10, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_10_bootAUC$t[,1])
R2m_stdErr<-sd(md_10_bootAUC$t[,1])/sqrt(length(md_10_bootAUC$t[,1]))
R2c_mean<-mean(md_10_bootAUC$t[,2])
R2c_stdErr<-sd(md_10_bootAUC$t[,2])/sqrt(length(md_10_bootAUC$t[,2]))
AUC_mean<-mean(md_10_bootAUC$t[,3])
AUC_stdErr<-sd(md_10_bootAUC$t[,3])/sqrt(length(md_10_bootAUC$t[,3]))

R2m_sd<-sd(md_10_bootAUC$t[,1])
R2c_sd<-sd(md_10_bootAUC$t[,2])
AUC_sd<-sd(md_10_bootAUC$t[,3])

md_10_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

AMRE_predictMap_m_10<-raster::predict(scaled_stack, md_10, re.form=NA, type='response')
writeRaster(AMRE_predictMap_m_10,file="3_output/maps/predictedDistributions/AMRE/AMRE_predictMap_m_10", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-11)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$AMRE_OCC, pred))),3)
}

md_11<-lme4::glmer(AMRE_OCC ~ ndvi_lag_11 +  elev_2pnt00_to_4pnt00_return_proportion +  elev_cv+ elev_maximum +  elev_p50 +  (1|SS) , data=dd, family = binomial, offset = AMRE_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_11_bootAUC<-bootMer(md_11, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_11_bootAUC$t[,1])
R2m_stdErr<-sd(md_11_bootAUC$t[,1])/sqrt(length(md_11_bootAUC$t[,1]))
R2c_mean<-mean(md_11_bootAUC$t[,2])
R2c_stdErr<-sd(md_11_bootAUC$t[,2])/sqrt(length(md_11_bootAUC$t[,2]))
AUC_mean<-mean(md_11_bootAUC$t[,3])
AUC_stdErr<-sd(md_11_bootAUC$t[,3])/sqrt(length(md_11_bootAUC$t[,3]))

R2m_sd<-sd(md_11_bootAUC$t[,1])
R2c_sd<-sd(md_11_bootAUC$t[,2])
AUC_sd<-sd(md_11_bootAUC$t[,3])

md_11_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

AMRE_predictMap_m_11<-raster::predict(scaled_stack, md_11, re.form=NA, type='response')
writeRaster(AMRE_predictMap_m_11,file="3_output/maps/predictedDistributions/AMRE/AMRE_predictMap_m_11", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-12)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$AMRE_OCC, pred))),3)
}

md_12<-lme4::glmer(AMRE_OCC ~ ndvi_lag_12 +  elev_2pnt00_to_4pnt00_return_proportion +  elev_cv+ elev_maximum +  elev_p50 +  (1|SS) , data=dd, family = binomial, offset = AMRE_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_12_bootAUC<-bootMer(md_12, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_12_bootAUC$t[,1])
R2m_stdErr<-sd(md_12_bootAUC$t[,1])/sqrt(length(md_12_bootAUC$t[,1]))
R2c_mean<-mean(md_12_bootAUC$t[,2])
R2c_stdErr<-sd(md_12_bootAUC$t[,2])/sqrt(length(md_12_bootAUC$t[,2]))
AUC_mean<-mean(md_12_bootAUC$t[,3])
AUC_stdErr<-sd(md_12_bootAUC$t[,3])/sqrt(length(md_12_bootAUC$t[,3]))

R2m_sd<-sd(md_12_bootAUC$t[,1])
R2c_sd<-sd(md_12_bootAUC$t[,2])
AUC_sd<-sd(md_12_bootAUC$t[,3])

md_12_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

AMRE_predictMap_m_12<-raster::predict(scaled_stack, md_12, re.form=NA, type='response')
writeRaster(AMRE_predictMap_m_12,file="3_output/maps/predictedDistributions/AMRE/AMRE_predictMap_m_12", format="GTiff", overwrite=T)

##########################
dd<-d3%>%filter(SS_lidar_timelag==-13)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$AMRE_OCC, pred))),3)
}

md_13<-lme4::glmer(AMRE_OCC ~ ndvi_lag_13 +  elev_2pnt00_to_4pnt00_return_proportion +  elev_cv+ elev_maximum +  elev_p50 +  (1|SS) , data=dd, family = binomial, offset = AMRE_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_13_bootAUC<-bootMer(md_13, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_13_bootAUC$t[,1])
R2m_stdErr<-sd(md_13_bootAUC$t[,1])/sqrt(length(md_13_bootAUC$t[,1]))
R2c_mean<-mean(md_13_bootAUC$t[,2])
R2c_stdErr<-sd(md_13_bootAUC$t[,2])/sqrt(length(md_13_bootAUC$t[,2]))
AUC_mean<-mean(md_13_bootAUC$t[,3])
AUC_stdErr<-sd(md_13_bootAUC$t[,3])/sqrt(length(md_13_bootAUC$t[,3]))

R2m_sd<-sd(md_13_bootAUC$t[,1])
R2c_sd<-sd(md_13_bootAUC$t[,2])
AUC_sd<-sd(md_13_bootAUC$t[,3])

md_13_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

AMRE_predictMap_m_13<-raster::predict(scaled_stack, md_13, re.form=NA, type='response')
writeRaster(AMRE_predictMap_m_13,file="3_output/maps/predictedDistributions/AMRE/AMRE_predictMap_m_13", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-14)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$AMRE_OCC, pred))),3)
}

md_14<-lme4::glmer(AMRE_OCC ~ ndvi_lag_14 +  elev_2pnt00_to_4pnt00_return_proportion +  elev_cv+ elev_maximum +  elev_p50 +  (1|SS) , data=dd, family = binomial, offset = AMRE_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_14_bootAUC<-bootMer(md_14, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_14_bootAUC$t[,1])
R2m_stdErr<-sd(md_14_bootAUC$t[,1])/sqrt(length(md_14_bootAUC$t[,1]))
R2c_mean<-mean(md_14_bootAUC$t[,2])
R2c_stdErr<-sd(md_14_bootAUC$t[,2])/sqrt(length(md_14_bootAUC$t[,2]))
AUC_mean<-mean(md_14_bootAUC$t[,3])
AUC_stdErr<-sd(md_14_bootAUC$t[,3])/sqrt(length(md_14_bootAUC$t[,3]))

R2m_sd<-sd(md_14_bootAUC$t[,1])
R2c_sd<-sd(md_14_bootAUC$t[,2])
AUC_sd<-sd(md_14_bootAUC$t[,3])

md_14_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

AMRE_predictMap_m_14<-raster::predict(scaled_stack, md_14, re.form=NA, type='response')
writeRaster(AMRE_predictMap_m_14,file="3_output/maps/predictedDistributions/AMRE/AMRE_predictMap_m_14", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-15)
set.seed(1)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$AMRE_OCC, pred))),3)
}

md_15<-lme4::glmer(AMRE_OCC ~ ndvi_lag_15 +  elev_2pnt00_to_4pnt00_return_proportion +  elev_cv+ elev_maximum +  elev_p50 +  (1|SS) , data=dd, family = binomial, offset = AMRE_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_15_bootAUC<-bootMer(md_15, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_15_bootAUC$t[,1])
R2m_stdErr<-sd(md_15_bootAUC$t[,1])/sqrt(length(md_15_bootAUC$t[,1]))
R2c_mean<-mean(md_15_bootAUC$t[,2])
R2c_stdErr<-sd(md_15_bootAUC$t[,2])/sqrt(length(md_15_bootAUC$t[,2]))
AUC_mean<-mean(md_15_bootAUC$t[,3])
AUC_stdErr<-sd(md_15_bootAUC$t[,3])/sqrt(length(md_15_bootAUC$t[,3]))
R2m_range<-range(md_15_bootAUC$t[,1])


R2m_sd<-sd(md_15_bootAUC$t[,1])
R2c_sd<-sd(md_15_bootAUC$t[,2])
AUC_sd<-sd(md_15_bootAUC$t[,3])

md_15_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)


AMRE_predictMap_m_15<-raster::predict(scaled_stack, md_15, re.form=NA, type='response')
writeRaster(AMRE_predictMap_m_15,file="3_output/maps/predictedDistributions/AMRE/AMRE_predictMap_m_15", format="GTiff", overwrite=T)


####################################################################
# save models
AMRE_models_4<-list(md_0, md_1, md_2, md_3, md_4, md_5, md_6, md_7, md_8, md_9, md_10, md_11, md_12, md_13, md_14, md_15)
names(AMRE_models_4) <- c("md_0", "md_1", "md_2", "md_3", "md_4", "md_5", "md_6", "md_7", "md_8", "md_9", "md_10", "md_11", "md_12", "md_13", "md_14", "md_15")
save(AMRE_models_4, file="2_pipeline/store/models/AMRE_models_4.rData")

####################################################################
# create an AUC dataframe
lag<-c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
model<-c("md_0", "md_1", "md_2", "md_3", "md_4", "md_5", "md_6", "md_7", "md_8", "md_9", "md_10", "md_11", "md_12", "md_13", "md_14", "md_15")

AMRE_models_AUC_4<-as.data.frame(rbind(md_0_meanAUC, md_1_meanAUC, md_2_meanAUC, md_3_meanAUC, md_4_meanAUC, md_5_meanAUC, md_6_meanAUC, md_7_meanAUC, md_8_meanAUC, md_9_meanAUC, md_10_meanAUC, md_11_meanAUC, md_12_meanAUC, md_13_meanAUC, md_14_meanAUC, md_15_meanAUC))%>%
  cbind(lag)

# AMRE_AUC_lm_test<-lm(AUC_mean~ lag, data=AMRE_models_AUC_4)
# summary(AMRE_AUC_lm_test)
# 
# AMRE_r2c_lm_test<-lm(R2c_mean~ lag, data=AMRE_models_AUC_4)
# summary(AMRE_r2c_lm_test)
# 
# AMRE_r2m_lm_test<-lm(R2m_mean~ lag, data=AMRE_models_AUC_4)
# summary(AMRE_r2m_lm_test)
# 
# ggplot(AMRE_models_AUC_4, aes(x=lag, y=AUC_mean)) + 
#     geom_errorbar(aes(ymin=AUC_mean-1.96*AUC_stdErr, ymax=AUC_mean+ 1.96*AUC_stdErr, width=.2))+
#     geom_point(size=1) + 
#     geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
#     geom_smooth(method = lm)+
#     theme(panel.background = element_blank(),
#           text = element_text(size=10),
#           axis.ticks=element_blank(),
#           #strip.background =element_rect(fill="white"),
#           panel.border = element_rect(color = "black", fill = NA, size = .2),
#           axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
#           axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
#           #panel.background = element_rect(fill="white"),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           legend.position="none")

save(AMRE_models_AUC_4, file="2_pipeline/store/models/AMRE_models_AUC_4.rData")

##############################################################
## save bootmer results
##############################################################
AMRE_models_bootAUC_4<-list(md_0_bootAUC, md_1_bootAUC, md_2_bootAUC, md_3_bootAUC, md_4_bootAUC, md_5_bootAUC, md_6_bootAUC, md_7_bootAUC, md_8_bootAUC, md_9_bootAUC, md_10_bootAUC, md_11_bootAUC, md_12_bootAUC, md_13_bootAUC, md_14_bootAUC, md_15_bootAUC)
names(AMRE_models_bootAUC_4) <- c("md_0_bootAUC", "md_1_bootAUC", "md_2_bootAUC", "md_3_bootAUC", "md_4_bootAUC", "md_5_bootAUC", "md_6_bootAUC", "md_7_bootAUC", "md_8_bootAUC", "md_9_bootAUC", "md_10_bootAUC", "md_11_bootAUC", "md_12_bootAUC", "md_13_bootAUC", "md_14_bootAUC", "md_15_bootAUC")
save(AMRE_models_bootAUC_4, file="2_pipeline/store/models/AMRE_models_bootAUC_4.rData")

##############################################################
## stack rasters
##############################################################

map_list<-list.files(path='3_output/maps/predictedDistributions/AMRE/predictMap', pattern = "*.tif$", full.names = TRUE)
AMRE_stack<-raster::stack(map_list)

#save stack as r object
stackSave(AMRE_stack,file="3_output/maps/predictedDistributions/AMRE/AMRE_stack.stk")


######################################################################################################################################

####################################################################################################################################
##BTNW##############################################################################################################################
####################################################################################################################################

#stats function for AUC

#dd<-d3%>%filter(SS_lidar_timelag==0)

dd<-d3%>%filter(SS_lidar_timelag==0)


# AUCFun <- function(x) {
#   library(pROC)
#   pred<-predict(x, type="response", allow.new.levels=TRUE)
#   AUC<-as.numeric(auc(d_0$BTNW_OCC, pred))
# }


#both AUC and R2
# AUCFun <- function(fit) {
#   library(pROC)
#   pred<-predict(fit, type="response", allow.new.levels=TRUE)
#   R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$BTNW_OCC, pred))),3)
#   }
#   
AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$BTNW_OCC, pred))),3)
}


#models
md_0<-lme4::glmer(BTNW_OCC ~ canopy_relief_ratio + elev_maximum + total_all_returns +  (1|SS) , data=dd, family = binomial, offset = BTNW_OFF,  na.action = na.fail, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)))


md_0_bootAUC<-bootMer(md_0, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)


# meanAUC<-mean(md_0_bootAUC$t)
# std.err<-sd(md_0_bootAUC$t)/sqrt(length(md_0_bootAUC$t))
# CI.lower <- meanAUC - std.err*1.96
# CI.upper <- meanAUC + std.err*1.96
# 
# md_0_meanAUC<-data.frame(meanAUC, std.err, CI.lower, CI.upper)

R2m_mean<-mean(md_0_bootAUC$t[,1])
R2m_stdErr<-sd(md_0_bootAUC$t[,1])/sqrt(length(md_0_bootAUC$t[,1]))
R2c_mean<-mean(md_0_bootAUC$t[,2])
R2c_stdErr<-sd(md_0_bootAUC$t[,2])/sqrt(length(md_0_bootAUC$t[,2]))
AUC_mean<-mean(md_0_bootAUC$t[,3])
AUC_stdErr<-sd(md_0_bootAUC$t[,3])/sqrt(length(md_0_bootAUC$t[,3]))

R2m_sd<-sd(md_0_bootAUC$t[,1])
R2c_sd<-sd(md_0_bootAUC$t[,2])
AUC_sd<-sd(md_0_bootAUC$t[,3])

md_0_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)


BTNW_predictMap_m_0<-raster::predict(scaled_stack, md_0, re.form=NA, type='response')
writeRaster(BTNW_predictMap_m_0,file="3_output/maps/predictedDistributions/BTNW/BTNW_predictMap_m_00", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-1)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$BTNW_OCC, pred))),3)
}

md_1<-lme4::glmer(BTNW_OCC ~ canopy_relief_ratio + elev_maximum + total_all_returns +  (1|SS) , data=dd, family = binomial, offset = BTNW_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_1_bootAUC<-bootMer(md_1, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_1_bootAUC$t[,1])
R2m_stdErr<-sd(md_1_bootAUC$t[,1])/sqrt(length(md_1_bootAUC$t[,1]))
R2m_sd<-sd(md_1_bootAUC$t[,1])
R2c_mean<-mean(md_1_bootAUC$t[,2])
R2c_stdErr<-sd(md_1_bootAUC$t[,2])/sqrt(length(md_1_bootAUC$t[,2]))
R2c_sd<-sd(md_1_bootAUC$t[,2])
AUC_mean<-mean(md_1_bootAUC$t[,3])
AUC_stdErr<-sd(md_1_bootAUC$t[,3])/sqrt(length(md_1_bootAUC$t[,3]))
AUC_sd<-sd(md_1_bootAUC$t[,3])

R2m_sd<-sd(md_1_bootAUC$t[,1])
R2c_sd<-sd(md_1_bootAUC$t[,2])
AUC_sd<-sd(md_1_bootAUC$t[,3])

md_1_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

BTNW_predictMap_m_01<-raster::predict(scaled_stack, md_1, re.form=NA, type='response')
writeRaster(BTNW_predictMap_m_01,file="3_output/maps/predictedDistributions/BTNW/BTNW_predictMap_m_01", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-2)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$BTNW_OCC, pred))),3)
}

md_2<-lme4::glmer(BTNW_OCC ~ canopy_relief_ratio + elev_maximum + total_all_returns +  (1|SS) , data=dd, family = binomial, offset = BTNW_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_2_bootAUC<-bootMer(md_2, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_2_bootAUC$t[,1])
R2m_stdErr<-sd(md_2_bootAUC$t[,1])/sqrt(length(md_2_bootAUC$t[,1]))
R2c_mean<-mean(md_2_bootAUC$t[,2])
R2c_stdErr<-sd(md_2_bootAUC$t[,2])/sqrt(length(md_2_bootAUC$t[,2]))
AUC_mean<-mean(md_2_bootAUC$t[,3])
AUC_stdErr<-sd(md_2_bootAUC$t[,3])/sqrt(length(md_2_bootAUC$t[,3]))

R2m_sd<-sd(md_2_bootAUC$t[,1])
R2c_sd<-sd(md_2_bootAUC$t[,2])
AUC_sd<-sd(md_2_bootAUC$t[,3])

md_2_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

BTNW_predictMap_m_02<-raster::predict(scaled_stack, md_2, re.form=NA, type='response')
writeRaster(BTNW_predictMap_m_02,file="3_output/maps/predictedDistributions/BTNW/BTNW_predictMap_m_02", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-3)


AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$BTNW_OCC, pred))),3)
}

md_3<-lme4::glmer(BTNW_OCC ~ canopy_relief_ratio + elev_maximum + total_all_returns +  (1|SS) , data=dd, family = binomial, offset = BTNW_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_3_bootAUC<-bootMer(md_3, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_3_bootAUC$t[,1])
R2m_stdErr<-sd(md_3_bootAUC$t[,1])/sqrt(length(md_3_bootAUC$t[,1]))
R2c_mean<-mean(md_3_bootAUC$t[,2])
R2c_stdErr<-sd(md_3_bootAUC$t[,2])/sqrt(length(md_3_bootAUC$t[,2]))
AUC_mean<-mean(md_3_bootAUC$t[,3])
AUC_stdErr<-sd(md_3_bootAUC$t[,3])/sqrt(length(md_3_bootAUC$t[,3]))

R2m_sd<-sd(md_3_bootAUC$t[,1])
R2c_sd<-sd(md_3_bootAUC$t[,2])
AUC_sd<-sd(md_3_bootAUC$t[,3])

md_3_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

BTNW_predictMap_m_03<-raster::predict(scaled_stack, md_3, re.form=NA, type='response')
writeRaster(BTNW_predictMap_m_03,file="3_output/maps/predictedDistributions/BTNW/BTNW_predictMap_m_03", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-4)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$BTNW_OCC, pred))),3)
}

md_4<-lme4::glmer(BTNW_OCC ~ canopy_relief_ratio + elev_maximum + total_all_returns +  (1|SS) , data=dd, family = binomial, offset = BTNW_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_4_bootAUC<-bootMer(md_4, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_4_bootAUC$t[,1])
R2m_stdErr<-sd(md_4_bootAUC$t[,1])/sqrt(length(md_4_bootAUC$t[,1]))
R2c_mean<-mean(md_4_bootAUC$t[,2])
R2c_stdErr<-sd(md_4_bootAUC$t[,2])/sqrt(length(md_4_bootAUC$t[,2]))
AUC_mean<-mean(md_4_bootAUC$t[,3])
AUC_stdErr<-sd(md_4_bootAUC$t[,3])/sqrt(length(md_4_bootAUC$t[,3]))

R2m_sd<-sd(md_4_bootAUC$t[,1])
R2c_sd<-sd(md_4_bootAUC$t[,2])
AUC_sd<-sd(md_4_bootAUC$t[,3])

md_4_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

BTNW_predictMap_m_04<-raster::predict(scaled_stack, md_4, re.form=NA, type='response')
writeRaster(BTNW_predictMap_m_04,file="3_output/maps/predictedDistributions/BTNW/BTNW_predictMap_m_04", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-5)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$BTNW_OCC, pred))),3)
}

md_5<-lme4::glmer(BTNW_OCC ~ canopy_relief_ratio + elev_maximum + total_all_returns +  (1|SS) , data=dd, family = binomial, offset = BTNW_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_5_bootAUC<-bootMer(md_5, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_5_bootAUC$t[,1])
R2m_stdErr<-sd(md_5_bootAUC$t[,1])/sqrt(length(md_5_bootAUC$t[,1]))
R2c_mean<-mean(md_5_bootAUC$t[,2])
R2c_stdErr<-sd(md_5_bootAUC$t[,2])/sqrt(length(md_5_bootAUC$t[,2]))
AUC_mean<-mean(md_5_bootAUC$t[,3])
AUC_stdErr<-sd(md_5_bootAUC$t[,3])/sqrt(length(md_5_bootAUC$t[,3]))

R2m_sd<-sd(md_5_bootAUC$t[,1])
R2c_sd<-sd(md_5_bootAUC$t[,2])
AUC_sd<-sd(md_5_bootAUC$t[,3])

md_5_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

BTNW_predictMap_m_05<-raster::predict(scaled_stack, md_5, re.form=NA, type='response')
writeRaster(BTNW_predictMap_m_05,file="3_output/maps/predictedDistributions/BTNW/BTNW_predictMap_m_05", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-6)


AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$BTNW_OCC, pred))),3)
}

md_6<-lme4::glmer(BTNW_OCC ~ canopy_relief_ratio + elev_maximum + total_all_returns +  (1|SS) , data=dd, family = binomial, offset = BTNW_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_6_bootAUC<-bootMer(md_6, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_6_bootAUC$t[,1])
R2m_stdErr<-sd(md_6_bootAUC$t[,1])/sqrt(length(md_6_bootAUC$t[,1]))
R2c_mean<-mean(md_6_bootAUC$t[,2])
R2c_stdErr<-sd(md_6_bootAUC$t[,2])/sqrt(length(md_6_bootAUC$t[,2]))
AUC_mean<-mean(md_6_bootAUC$t[,3])
AUC_stdErr<-sd(md_6_bootAUC$t[,3])/sqrt(length(md_6_bootAUC$t[,3]))

R2m_sd<-sd(md_6_bootAUC$t[,1])
R2c_sd<-sd(md_6_bootAUC$t[,2])
AUC_sd<-sd(md_6_bootAUC$t[,3])

md_6_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)


BTNW_predictMap_m_06<-raster::predict(scaled_stack, md_6, re.form=NA, type='response')
writeRaster(BTNW_predictMap_m_06,file="3_output/maps/predictedDistributions/BTNW/BTNW_predictMap_m_06", format="GTiff", overwrite=T)

##########################
dd<-d3%>%filter(SS_lidar_timelag==-7)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$BTNW_OCC, pred))),3)
}

md_7<-lme4::glmer(BTNW_OCC ~ canopy_relief_ratio + elev_maximum + total_all_returns +  (1|SS) , data=dd, family = binomial, offset = BTNW_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_7_bootAUC<-bootMer(md_7, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_7_bootAUC$t[,1])
R2m_stdErr<-sd(md_7_bootAUC$t[,1])/sqrt(length(md_7_bootAUC$t[,1]))
R2c_mean<-mean(md_7_bootAUC$t[,2])
R2c_stdErr<-sd(md_7_bootAUC$t[,2])/sqrt(length(md_7_bootAUC$t[,2]))
AUC_mean<-mean(md_7_bootAUC$t[,3])
AUC_stdErr<-sd(md_7_bootAUC$t[,3])/sqrt(length(md_7_bootAUC$t[,3]))

R2m_sd<-sd(md_7_bootAUC$t[,1])
R2c_sd<-sd(md_7_bootAUC$t[,2])
AUC_sd<-sd(md_7_bootAUC$t[,3])

md_7_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

BTNW_predictMap_m_07<-raster::predict(scaled_stack, md_7, re.form=NA, type='response')
writeRaster(BTNW_predictMap_m_07,file="3_output/maps/predictedDistributions/BTNW/BTNW_predictMap_m_07", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-8)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$BTNW_OCC, pred))),3)
}

md_8<-lme4::glmer(BTNW_OCC ~ canopy_relief_ratio + elev_maximum + total_all_returns +  (1|SS) , data=dd, family = binomial, offset = BTNW_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_8_bootAUC<-bootMer(md_8, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_8_bootAUC$t[,1])
R2m_stdErr<-sd(md_8_bootAUC$t[,1])/sqrt(length(md_8_bootAUC$t[,1]))
R2c_mean<-mean(md_8_bootAUC$t[,2])
R2c_stdErr<-sd(md_8_bootAUC$t[,2])/sqrt(length(md_8_bootAUC$t[,2]))
AUC_mean<-mean(md_8_bootAUC$t[,3])
AUC_stdErr<-sd(md_8_bootAUC$t[,3])/sqrt(length(md_8_bootAUC$t[,3]))

R2m_sd<-sd(md_8_bootAUC$t[,1])
R2c_sd<-sd(md_8_bootAUC$t[,2])
AUC_sd<-sd(md_8_bootAUC$t[,3])

md_8_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

BTNW_predictMap_m_08<-raster::predict(scaled_stack, md_8, re.form=NA, type='response')
writeRaster(BTNW_predictMap_m_08,file="3_output/maps/predictedDistributions/BTNW/BTNW_predictMap_m_08", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-9)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$BTNW_OCC, pred))),3)
}

md_9<-lme4::glmer(BTNW_OCC ~ canopy_relief_ratio + elev_maximum + total_all_returns +  (1|SS) , data=dd, family = binomial, offset = BTNW_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_9_bootAUC<-bootMer(md_9, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_9_bootAUC$t[,1])
R2m_stdErr<-sd(md_9_bootAUC$t[,1])/sqrt(length(md_9_bootAUC$t[,1]))
R2c_mean<-mean(md_9_bootAUC$t[,2])
R2c_stdErr<-sd(md_9_bootAUC$t[,2])/sqrt(length(md_9_bootAUC$t[,2]))
AUC_mean<-mean(md_9_bootAUC$t[,3])
AUC_stdErr<-sd(md_9_bootAUC$t[,3])/sqrt(length(md_9_bootAUC$t[,3]))

R2m_sd<-sd(md_9_bootAUC$t[,1])
R2c_sd<-sd(md_9_bootAUC$t[,2])
AUC_sd<-sd(md_9_bootAUC$t[,3])

md_9_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)


BTNW_predictMap_m_09<-raster::predict(scaled_stack, md_9, re.form=NA, type='response')
writeRaster(BTNW_predictMap_m_09,file="3_output/maps/predictedDistributions/BTNW/BTNW_predictMap_m_09", format="GTiff", overwrite=T)

##########################
dd<-d3%>%filter(SS_lidar_timelag==-10)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$BTNW_OCC, pred))),3)
}

md_10<-lme4::glmer(BTNW_OCC ~ canopy_relief_ratio + elev_maximum + total_all_returns +  (1|SS) , data=dd, family = binomial, offset = BTNW_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_10_bootAUC<-bootMer(md_10, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_10_bootAUC$t[,1])
R2m_stdErr<-sd(md_10_bootAUC$t[,1])/sqrt(length(md_10_bootAUC$t[,1]))
R2c_mean<-mean(md_10_bootAUC$t[,2])
R2c_stdErr<-sd(md_10_bootAUC$t[,2])/sqrt(length(md_10_bootAUC$t[,2]))
AUC_mean<-mean(md_10_bootAUC$t[,3])
AUC_stdErr<-sd(md_10_bootAUC$t[,3])/sqrt(length(md_10_bootAUC$t[,3]))

R2m_sd<-sd(md_10_bootAUC$t[,1])
R2c_sd<-sd(md_10_bootAUC$t[,2])
AUC_sd<-sd(md_10_bootAUC$t[,3])

md_10_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

BTNW_predictMap_m_10<-raster::predict(scaled_stack, md_10, re.form=NA, type='response')
writeRaster(BTNW_predictMap_m_10,file="3_output/maps/predictedDistributions/BTNW/BTNW_predictMap_m_10", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-11)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$BTNW_OCC, pred))),3)
}

md_11<-lme4::glmer(BTNW_OCC ~ canopy_relief_ratio + elev_maximum + total_all_returns +  (1|SS) , data=dd, family = binomial, offset = BTNW_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_11_bootAUC<-bootMer(md_11, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_11_bootAUC$t[,1])
R2m_stdErr<-sd(md_11_bootAUC$t[,1])/sqrt(length(md_11_bootAUC$t[,1]))
R2c_mean<-mean(md_11_bootAUC$t[,2])
R2c_stdErr<-sd(md_11_bootAUC$t[,2])/sqrt(length(md_11_bootAUC$t[,2]))
AUC_mean<-mean(md_11_bootAUC$t[,3])
AUC_stdErr<-sd(md_11_bootAUC$t[,3])/sqrt(length(md_11_bootAUC$t[,3]))

R2m_sd<-sd(md_11_bootAUC$t[,1])
R2c_sd<-sd(md_11_bootAUC$t[,2])
AUC_sd<-sd(md_11_bootAUC$t[,3])

md_11_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

BTNW_predictMap_m_11<-raster::predict(scaled_stack, md_11, re.form=NA, type='response')
writeRaster(BTNW_predictMap_m_11,file="3_output/maps/predictedDistributions/BTNW/BTNW_predictMap_m_11", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-12)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$BTNW_OCC, pred))),3)
}

md_12<-lme4::glmer(BTNW_OCC ~ canopy_relief_ratio + elev_maximum + total_all_returns +  (1|SS) , data=dd, family = binomial, offset = BTNW_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_12_bootAUC<-bootMer(md_12, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_12_bootAUC$t[,1])
R2m_stdErr<-sd(md_12_bootAUC$t[,1])/sqrt(length(md_12_bootAUC$t[,1]))
R2c_mean<-mean(md_12_bootAUC$t[,2])
R2c_stdErr<-sd(md_12_bootAUC$t[,2])/sqrt(length(md_12_bootAUC$t[,2]))
AUC_mean<-mean(md_12_bootAUC$t[,3])
AUC_stdErr<-sd(md_12_bootAUC$t[,3])/sqrt(length(md_12_bootAUC$t[,3]))

R2m_sd<-sd(md_12_bootAUC$t[,1])
R2c_sd<-sd(md_12_bootAUC$t[,2])
AUC_sd<-sd(md_12_bootAUC$t[,3])

md_12_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

BTNW_predictMap_m_12<-raster::predict(scaled_stack, md_12, re.form=NA, type='response')
writeRaster(BTNW_predictMap_m_12,file="3_output/maps/predictedDistributions/BTNW/BTNW_predictMap_m_12", format="GTiff", overwrite=T)

##########################
dd<-d3%>%filter(SS_lidar_timelag==-13)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$BTNW_OCC, pred))),3)
}

md_13<-lme4::glmer(BTNW_OCC ~ canopy_relief_ratio + elev_maximum + total_all_returns +  (1|SS) , data=dd, family = binomial, offset = BTNW_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_13_bootAUC<-bootMer(md_13, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_13_bootAUC$t[,1])
R2m_stdErr<-sd(md_13_bootAUC$t[,1])/sqrt(length(md_13_bootAUC$t[,1]))
R2c_mean<-mean(md_13_bootAUC$t[,2])
R2c_stdErr<-sd(md_13_bootAUC$t[,2])/sqrt(length(md_13_bootAUC$t[,2]))
AUC_mean<-mean(md_13_bootAUC$t[,3])
AUC_stdErr<-sd(md_13_bootAUC$t[,3])/sqrt(length(md_13_bootAUC$t[,3]))

R2m_sd<-sd(md_13_bootAUC$t[,1])
R2c_sd<-sd(md_13_bootAUC$t[,2])
AUC_sd<-sd(md_13_bootAUC$t[,3])

md_13_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

BTNW_predictMap_m_13<-raster::predict(scaled_stack, md_13, re.form=NA, type='response')
writeRaster(BTNW_predictMap_m_13,file="3_output/maps/predictedDistributions/BTNW/BTNW_predictMap_m_13", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-14)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$BTNW_OCC, pred))),3)
}

md_14<-lme4::glmer(BTNW_OCC ~ canopy_relief_ratio + elev_maximum + total_all_returns +  (1|SS) , data=dd, family = binomial, offset = BTNW_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_14_bootAUC<-bootMer(md_14, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_14_bootAUC$t[,1])
R2m_stdErr<-sd(md_14_bootAUC$t[,1])/sqrt(length(md_14_bootAUC$t[,1]))
R2c_mean<-mean(md_14_bootAUC$t[,2])
R2c_stdErr<-sd(md_14_bootAUC$t[,2])/sqrt(length(md_14_bootAUC$t[,2]))
AUC_mean<-mean(md_14_bootAUC$t[,3])
AUC_stdErr<-sd(md_14_bootAUC$t[,3])/sqrt(length(md_14_bootAUC$t[,3]))

R2m_sd<-sd(md_14_bootAUC$t[,1])
R2c_sd<-sd(md_14_bootAUC$t[,2])
AUC_sd<-sd(md_14_bootAUC$t[,3])

md_14_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

BTNW_predictMap_m_14<-raster::predict(scaled_stack, md_14, re.form=NA, type='response')
writeRaster(BTNW_predictMap_m_14,file="3_output/maps/predictedDistributions/BTNW/BTNW_predictMap_m_14", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-15)
set.seed(1)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$BTNW_OCC, pred))),3)
}

md_15<-lme4::glmer(BTNW_OCC ~ canopy_relief_ratio + elev_maximum + total_all_returns +  (1|SS) , data=dd, family = binomial, offset = BTNW_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_15_bootAUC<-bootMer(md_15, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_15_bootAUC$t[,1])
R2m_stdErr<-sd(md_15_bootAUC$t[,1])/sqrt(length(md_15_bootAUC$t[,1]))
R2c_mean<-mean(md_15_bootAUC$t[,2])
R2c_stdErr<-sd(md_15_bootAUC$t[,2])/sqrt(length(md_15_bootAUC$t[,2]))
AUC_mean<-mean(md_15_bootAUC$t[,3])
AUC_stdErr<-sd(md_15_bootAUC$t[,3])/sqrt(length(md_15_bootAUC$t[,3]))
R2m_range<-range(md_15_bootAUC$t[,1])


R2m_sd<-sd(md_15_bootAUC$t[,1])
R2c_sd<-sd(md_15_bootAUC$t[,2])
AUC_sd<-sd(md_15_bootAUC$t[,3])

md_15_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

BTNW_predictMap_m_15<-raster::predict(scaled_stack, md_15, re.form=NA, type='response')
writeRaster(BTNW_predictMap_m_15,file="3_output/maps/predictedDistributions/BTNW/BTNW_predictMap_m_15", format="GTiff", overwrite=T)


####################################################################
# save models
BTNW_models_4<-list(md_0, md_1, md_2, md_3, md_4, md_5, md_6, md_7, md_8, md_9, md_10, md_11, md_12, md_13, md_14, md_15)
names(BTNW_models_4) <- c("md_0", "md_1", "md_2", "md_3", "md_4", "md_5", "md_6", "md_7", "md_8", "md_9", "md_10", "md_11", "md_12", "md_13", "md_14", "md_15")
save(BTNW_models_4, file="2_pipeline/store/models/BTNW_models_4.rData")

####################################################################
# create an AUC dataframe
lag<-c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
model<-c("md_0", "md_1", "md_2", "md_3", "md_4", "md_5", "md_6", "md_7", "md_8", "md_9", "md_10", "md_11", "md_12", "md_13", "md_14", "md_15")

BTNW_models_AUC_4<-as.data.frame(rbind(md_0_meanAUC, md_1_meanAUC, md_2_meanAUC, md_3_meanAUC, md_4_meanAUC, md_5_meanAUC, md_6_meanAUC, md_7_meanAUC, md_8_meanAUC, md_9_meanAUC, md_10_meanAUC, md_11_meanAUC, md_12_meanAUC, md_13_meanAUC, md_14_meanAUC, md_15_meanAUC))%>%
  cbind(lag)

BTNW_AUC_lm_test<-lm(AUC_mean~ lag, data=BTNW_models_AUC_4)
summary(BTNW_AUC_lm_test)

BTNW_r2c_lm_test<-lm(R2c_mean~ lag, data=BTNW_models_AUC_4)
summary(BTNW_r2c_lm_test)

BTNW_r2m_lm_test<-lm(R2m_mean~ lag, data=BTNW_models_AUC_4)
summary(BTNW_r2m_lm_test)

# ggplot(BTNW_models_AUC_4, aes(x=lag, y=AUC_mean)) + 
#     geom_errorbar(aes(ymin=AUC_mean-1.96*AUC_stdErr, ymax=AUC_mean+ 1.96*AUC_stdErr, width=.2))+
#     geom_point(size=1) + 
#     geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
#     geom_smooth(method = lm)+
#     theme(panel.background = element_blank(),
#           text = element_text(size=10),
#           axis.ticks=element_blank(),
#           #strip.background =element_rect(fill="white"),
#           panel.border = element_rect(color = "black", fill = NA, size = .2),
#           axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
#           axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
#           #panel.background = element_rect(fill="white"),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           legend.position="none")



save(BTNW_models_AUC_4, file="2_pipeline/store/models/BTNW_models_AUC_4.rData")

##############################################################
## save bootmer results
##############################################################
BTNW_models_bootAUC_4<-list(md_0_bootAUC, md_1_bootAUC, md_2_bootAUC, md_3_bootAUC, md_4_bootAUC, md_5_bootAUC, md_6_bootAUC, md_7_bootAUC, md_8_bootAUC, md_9_bootAUC, md_10_bootAUC, md_11_bootAUC, md_12_bootAUC, md_13_bootAUC, md_14_bootAUC, md_15_bootAUC)
names(BTNW_models_bootAUC_4) <- c("md_0_bootAUC", "md_1_bootAUC", "md_2_bootAUC", "md_3_bootAUC", "md_4_bootAUC", "md_5_bootAUC", "md_6_bootAUC", "md_7_bootAUC", "md_8_bootAUC", "md_9_bootAUC", "md_10_bootAUC", "md_11_bootAUC", "md_12_bootAUC", "md_13_bootAUC", "md_14_bootAUC", "md_15_bootAUC")
save(BTNW_models_bootAUC_4, file="2_pipeline/store/models/BTNW_models_bootAUC_4.rData")

##############################################################
## stack rasters
##############################################################

map_list<-list.files(path='3_output/maps/predictedDistributions/BTNW/predictMap', pattern = "*.tif$", full.names = TRUE)
BTNW_stack<-raster::stack(map_list)

#save stack as r object
stackSave(BTNW_stack,file="3_output/maps/predictedDistributions/BTNW/BTNW_stack.stk")


####################################################################################################################################
##MOWA##############################################################################################################################
####################################################################################################################################

#stats function for AUC

#dd<-d3%>%filter(SS_lidar_timelag==0)

dd<-d3%>%filter(SS_lidar_timelag==0)


# AUCFun <- function(x) {
#   library(pROC)
#   pred<-predict(x, type="response", allow.new.levels=TRUE)
#   AUC<-as.numeric(auc(d_0$MOWA_OCC, pred))
# }


#both AUC and R2
# AUCFun <- function(fit) {
#   library(pROC)
#   pred<-predict(fit, type="response", allow.new.levels=TRUE)
#   R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$MOWA_OCC, pred))),3)
#   }
#   
AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$MOWA_OCC, pred))),3)
}


#models
md_0<-lme4::glmer(MOWA_OCC ~ poly(ndvi_lag_0,2) +  elev_0pnt15_to_2pnt00_return_proportion+ elev_2pnt00_to_4pnt00_return_proportion + elev_maximum *elev_stddev+   percentage_first_returns_above_mean+  (1|SS) , data=dd, family = binomial, offset = MOWA_OFF,  na.action = na.fail, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)))


md_0_bootAUC<-bootMer(md_0, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)


# meanAUC<-mean(md_0_bootAUC$t)
# std.err<-sd(md_0_bootAUC$t)/sqrt(length(md_0_bootAUC$t))
# CI.lower <- meanAUC - std.err*1.96
# CI.upper <- meanAUC + std.err*1.96
# 
# md_0_meanAUC<-data.frame(meanAUC, std.err, CI.lower, CI.upper)

R2m_mean<-mean(md_0_bootAUC$t[,1])
R2m_stdErr<-sd(md_0_bootAUC$t[,1])/sqrt(length(md_0_bootAUC$t[,1]))
R2c_mean<-mean(md_0_bootAUC$t[,2])
R2c_stdErr<-sd(md_0_bootAUC$t[,2])/sqrt(length(md_0_bootAUC$t[,2]))
AUC_mean<-mean(md_0_bootAUC$t[,3])
AUC_stdErr<-sd(md_0_bootAUC$t[,3])/sqrt(length(md_0_bootAUC$t[,3]))

R2m_sd<-sd(md_0_bootAUC$t[,1])
R2c_sd<-sd(md_0_bootAUC$t[,2])
AUC_sd<-sd(md_0_bootAUC$t[,3])

md_0_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)


MOWA_predictMap_m_0<-raster::predict(scaled_stack, md_0, re.form=NA, type='response')
writeRaster(MOWA_predictMap_m_0,file="3_output/maps/predictedDistributions/MOWA/MOWA_predictMap_m_00", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-1)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$MOWA_OCC, pred))),3)
}

md_1<-lme4::glmer(MOWA_OCC ~ poly(ndvi_lag_1,2) +  elev_0pnt15_to_2pnt00_return_proportion+ elev_2pnt00_to_4pnt00_return_proportion + elev_maximum *elev_stddev+   percentage_first_returns_above_mean+  (1|SS) , data=dd, family = binomial, offset = MOWA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_1_bootAUC<-bootMer(md_1, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_1_bootAUC$t[,1])
R2m_stdErr<-sd(md_1_bootAUC$t[,1])/sqrt(length(md_1_bootAUC$t[,1]))
R2m_sd<-sd(md_1_bootAUC$t[,1])
R2c_mean<-mean(md_1_bootAUC$t[,2])
R2c_stdErr<-sd(md_1_bootAUC$t[,2])/sqrt(length(md_1_bootAUC$t[,2]))
R2c_sd<-sd(md_1_bootAUC$t[,2])
AUC_mean<-mean(md_1_bootAUC$t[,3])
AUC_stdErr<-sd(md_1_bootAUC$t[,3])/sqrt(length(md_1_bootAUC$t[,3]))
AUC_sd<-sd(md_1_bootAUC$t[,3])

R2m_sd<-sd(md_1_bootAUC$t[,1])
R2c_sd<-sd(md_1_bootAUC$t[,2])
AUC_sd<-sd(md_1_bootAUC$t[,3])

md_1_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

MOWA_predictMap_m_01<-raster::predict(scaled_stack, md_1, re.form=NA, type='response')
writeRaster(MOWA_predictMap_m_01,file="3_output/maps/predictedDistributions/MOWA/MOWA_predictMap_m_01", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-2)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$MOWA_OCC, pred))),3)
}

md_2<-lme4::glmer(MOWA_OCC ~ poly(ndvi_lag_2,2) +  elev_0pnt15_to_2pnt00_return_proportion+ elev_2pnt00_to_4pnt00_return_proportion + elev_maximum *elev_stddev+   percentage_first_returns_above_mean+  (1|SS) , data=dd, family = binomial, offset = MOWA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_2_bootAUC<-bootMer(md_2, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_2_bootAUC$t[,1])
R2m_stdErr<-sd(md_2_bootAUC$t[,1])/sqrt(length(md_2_bootAUC$t[,1]))
R2c_mean<-mean(md_2_bootAUC$t[,2])
R2c_stdErr<-sd(md_2_bootAUC$t[,2])/sqrt(length(md_2_bootAUC$t[,2]))
AUC_mean<-mean(md_2_bootAUC$t[,3])
AUC_stdErr<-sd(md_2_bootAUC$t[,3])/sqrt(length(md_2_bootAUC$t[,3]))

R2m_sd<-sd(md_2_bootAUC$t[,1])
R2c_sd<-sd(md_2_bootAUC$t[,2])
AUC_sd<-sd(md_2_bootAUC$t[,3])

md_2_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

MOWA_predictMap_m_02<-raster::predict(scaled_stack, md_2, re.form=NA, type='response')
writeRaster(MOWA_predictMap_m_02,file="3_output/maps/predictedDistributions/MOWA/MOWA_predictMap_m_02", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-3)


AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$MOWA_OCC, pred))),3)
}

md_3<-lme4::glmer(MOWA_OCC ~ poly(ndvi_lag_3,2) +  elev_0pnt15_to_2pnt00_return_proportion+ elev_2pnt00_to_4pnt00_return_proportion + elev_maximum *elev_stddev+   percentage_first_returns_above_mean+  (1|SS) , data=dd, family = binomial, offset = MOWA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_3_bootAUC<-bootMer(md_3, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_3_bootAUC$t[,1])
R2m_stdErr<-sd(md_3_bootAUC$t[,1])/sqrt(length(md_3_bootAUC$t[,1]))
R2c_mean<-mean(md_3_bootAUC$t[,2])
R2c_stdErr<-sd(md_3_bootAUC$t[,2])/sqrt(length(md_3_bootAUC$t[,2]))
AUC_mean<-mean(md_3_bootAUC$t[,3])
AUC_stdErr<-sd(md_3_bootAUC$t[,3])/sqrt(length(md_3_bootAUC$t[,3]))

R2m_sd<-sd(md_3_bootAUC$t[,1])
R2c_sd<-sd(md_3_bootAUC$t[,2])
AUC_sd<-sd(md_3_bootAUC$t[,3])

md_3_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

MOWA_predictMap_m_03<-raster::predict(scaled_stack, md_3, re.form=NA, type='response')
writeRaster(MOWA_predictMap_m_03,file="3_output/maps/predictedDistributions/MOWA/MOWA_predictMap_m_03", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-4)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$MOWA_OCC, pred))),3)
}

md_4<-lme4::glmer(MOWA_OCC ~ poly(ndvi_lag_4,2) +  elev_0pnt15_to_2pnt00_return_proportion+ elev_2pnt00_to_4pnt00_return_proportion + elev_maximum *elev_stddev+   percentage_first_returns_above_mean+  (1|SS) , data=dd, family = binomial, offset = MOWA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_4_bootAUC<-bootMer(md_4, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_4_bootAUC$t[,1])
R2m_stdErr<-sd(md_4_bootAUC$t[,1])/sqrt(length(md_4_bootAUC$t[,1]))
R2c_mean<-mean(md_4_bootAUC$t[,2])
R2c_stdErr<-sd(md_4_bootAUC$t[,2])/sqrt(length(md_4_bootAUC$t[,2]))
AUC_mean<-mean(md_4_bootAUC$t[,3])
AUC_stdErr<-sd(md_4_bootAUC$t[,3])/sqrt(length(md_4_bootAUC$t[,3]))

R2m_sd<-sd(md_4_bootAUC$t[,1])
R2c_sd<-sd(md_4_bootAUC$t[,2])
AUC_sd<-sd(md_4_bootAUC$t[,3])

md_4_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

MOWA_predictMap_m_04<-raster::predict(scaled_stack, md_4, re.form=NA, type='response')
writeRaster(MOWA_predictMap_m_04,file="3_output/maps/predictedDistributions/MOWA/MOWA_predictMap_m_04", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-5)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$MOWA_OCC, pred))),3)
}

md_5<-lme4::glmer(MOWA_OCC ~ poly(ndvi_lag_5,2) +  elev_0pnt15_to_2pnt00_return_proportion+ elev_2pnt00_to_4pnt00_return_proportion + elev_maximum *elev_stddev+   percentage_first_returns_above_mean+  (1|SS) , data=dd, family = binomial, offset = MOWA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_5_bootAUC<-bootMer(md_5, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_5_bootAUC$t[,1])
R2m_stdErr<-sd(md_5_bootAUC$t[,1])/sqrt(length(md_5_bootAUC$t[,1]))
R2c_mean<-mean(md_5_bootAUC$t[,2])
R2c_stdErr<-sd(md_5_bootAUC$t[,2])/sqrt(length(md_5_bootAUC$t[,2]))
AUC_mean<-mean(md_5_bootAUC$t[,3])
AUC_stdErr<-sd(md_5_bootAUC$t[,3])/sqrt(length(md_5_bootAUC$t[,3]))

R2m_sd<-sd(md_5_bootAUC$t[,1])
R2c_sd<-sd(md_5_bootAUC$t[,2])
AUC_sd<-sd(md_5_bootAUC$t[,3])

md_5_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

MOWA_predictMap_m_05<-raster::predict(scaled_stack, md_5, re.form=NA, type='response')
writeRaster(MOWA_predictMap_m_05,file="3_output/maps/predictedDistributions/MOWA/MOWA_predictMap_m_05", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-6)


AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$MOWA_OCC, pred))),3)
}

md_6<-lme4::glmer(MOWA_OCC ~ poly(ndvi_lag_6,2) +  elev_0pnt15_to_2pnt00_return_proportion+ elev_2pnt00_to_4pnt00_return_proportion + elev_maximum *elev_stddev+   percentage_first_returns_above_mean+  (1|SS) , data=dd, family = binomial, offset = MOWA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_6_bootAUC<-bootMer(md_6, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_6_bootAUC$t[,1])
R2m_stdErr<-sd(md_6_bootAUC$t[,1])/sqrt(length(md_6_bootAUC$t[,1]))
R2c_mean<-mean(md_6_bootAUC$t[,2])
R2c_stdErr<-sd(md_6_bootAUC$t[,2])/sqrt(length(md_6_bootAUC$t[,2]))
AUC_mean<-mean(md_6_bootAUC$t[,3])
AUC_stdErr<-sd(md_6_bootAUC$t[,3])/sqrt(length(md_6_bootAUC$t[,3]))

R2m_sd<-sd(md_6_bootAUC$t[,1])
R2c_sd<-sd(md_6_bootAUC$t[,2])
AUC_sd<-sd(md_6_bootAUC$t[,3])

md_6_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)


MOWA_predictMap_m_06<-raster::predict(scaled_stack, md_6, re.form=NA, type='response')
writeRaster(MOWA_predictMap_m_06,file="3_output/maps/predictedDistributions/MOWA/MOWA_predictMap_m_06", format="GTiff", overwrite=T)

##########################
dd<-d3%>%filter(SS_lidar_timelag==-7)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$MOWA_OCC, pred))),3)
}

md_7<-lme4::glmer(MOWA_OCC ~ poly(ndvi_lag_7,2) +  elev_0pnt15_to_2pnt00_return_proportion+ elev_2pnt00_to_4pnt00_return_proportion + elev_maximum *elev_stddev+   percentage_first_returns_above_mean+  (1|SS) , data=dd, family = binomial, offset = MOWA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_7_bootAUC<-bootMer(md_7, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_7_bootAUC$t[,1])
R2m_stdErr<-sd(md_7_bootAUC$t[,1])/sqrt(length(md_7_bootAUC$t[,1]))
R2c_mean<-mean(md_7_bootAUC$t[,2])
R2c_stdErr<-sd(md_7_bootAUC$t[,2])/sqrt(length(md_7_bootAUC$t[,2]))
AUC_mean<-mean(md_7_bootAUC$t[,3])
AUC_stdErr<-sd(md_7_bootAUC$t[,3])/sqrt(length(md_7_bootAUC$t[,3]))

R2m_sd<-sd(md_7_bootAUC$t[,1])
R2c_sd<-sd(md_7_bootAUC$t[,2])
AUC_sd<-sd(md_7_bootAUC$t[,3])

md_7_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

MOWA_predictMap_m_07<-raster::predict(scaled_stack, md_7, re.form=NA, type='response')
writeRaster(MOWA_predictMap_m_07,file="3_output/maps/predictedDistributions/MOWA/MOWA_predictMap_m_07", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-8)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$MOWA_OCC, pred))),3)
}

md_8<-lme4::glmer(MOWA_OCC ~ poly(ndvi_lag_8,2) +  elev_0pnt15_to_2pnt00_return_proportion+ elev_2pnt00_to_4pnt00_return_proportion + elev_maximum *elev_stddev+   percentage_first_returns_above_mean+  (1|SS) , data=dd, family = binomial, offset = MOWA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_8_bootAUC<-bootMer(md_8, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_8_bootAUC$t[,1])
R2m_stdErr<-sd(md_8_bootAUC$t[,1])/sqrt(length(md_8_bootAUC$t[,1]))
R2c_mean<-mean(md_8_bootAUC$t[,2])
R2c_stdErr<-sd(md_8_bootAUC$t[,2])/sqrt(length(md_8_bootAUC$t[,2]))
AUC_mean<-mean(md_8_bootAUC$t[,3])
AUC_stdErr<-sd(md_8_bootAUC$t[,3])/sqrt(length(md_8_bootAUC$t[,3]))

R2m_sd<-sd(md_8_bootAUC$t[,1])
R2c_sd<-sd(md_8_bootAUC$t[,2])
AUC_sd<-sd(md_8_bootAUC$t[,3])

md_8_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

MOWA_predictMap_m_08<-raster::predict(scaled_stack, md_8, re.form=NA, type='response')
writeRaster(MOWA_predictMap_m_08,file="3_output/maps/predictedDistributions/MOWA/MOWA_predictMap_m_08", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-9)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$MOWA_OCC, pred))),3)
}

md_9<-lme4::glmer(MOWA_OCC ~ poly(ndvi_lag_9,2) +  elev_0pnt15_to_2pnt00_return_proportion+ elev_2pnt00_to_4pnt00_return_proportion + elev_maximum *elev_stddev+   percentage_first_returns_above_mean+  (1|SS) , data=dd, family = binomial, offset = MOWA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_9_bootAUC<-bootMer(md_9, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_9_bootAUC$t[,1])
R2m_stdErr<-sd(md_9_bootAUC$t[,1])/sqrt(length(md_9_bootAUC$t[,1]))
R2c_mean<-mean(md_9_bootAUC$t[,2])
R2c_stdErr<-sd(md_9_bootAUC$t[,2])/sqrt(length(md_9_bootAUC$t[,2]))
AUC_mean<-mean(md_9_bootAUC$t[,3])
AUC_stdErr<-sd(md_9_bootAUC$t[,3])/sqrt(length(md_9_bootAUC$t[,3]))

R2m_sd<-sd(md_9_bootAUC$t[,1])
R2c_sd<-sd(md_9_bootAUC$t[,2])
AUC_sd<-sd(md_9_bootAUC$t[,3])

md_9_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)


MOWA_predictMap_m_09<-raster::predict(scaled_stack, md_9, re.form=NA, type='response')
writeRaster(MOWA_predictMap_m_09,file="3_output/maps/predictedDistributions/MOWA/MOWA_predictMap_m_09", format="GTiff", overwrite=T)

##########################
dd<-d3%>%filter(SS_lidar_timelag==-10)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$MOWA_OCC, pred))),3)
}

md_10<-lme4::glmer(MOWA_OCC ~ poly(ndvi_lag_10,2) +  elev_0pnt15_to_2pnt00_return_proportion+ elev_2pnt00_to_4pnt00_return_proportion + elev_maximum *elev_stddev+   percentage_first_returns_above_mean+  (1|SS) , data=dd, family = binomial, offset = MOWA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_10_bootAUC<-bootMer(md_10, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_10_bootAUC$t[,1])
R2m_stdErr<-sd(md_10_bootAUC$t[,1])/sqrt(length(md_10_bootAUC$t[,1]))
R2c_mean<-mean(md_10_bootAUC$t[,2])
R2c_stdErr<-sd(md_10_bootAUC$t[,2])/sqrt(length(md_10_bootAUC$t[,2]))
AUC_mean<-mean(md_10_bootAUC$t[,3])
AUC_stdErr<-sd(md_10_bootAUC$t[,3])/sqrt(length(md_10_bootAUC$t[,3]))

R2m_sd<-sd(md_10_bootAUC$t[,1])
R2c_sd<-sd(md_10_bootAUC$t[,2])
AUC_sd<-sd(md_10_bootAUC$t[,3])

md_10_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

MOWA_predictMap_m_10<-raster::predict(scaled_stack, md_10, re.form=NA, type='response')
writeRaster(MOWA_predictMap_m_10,file="3_output/maps/predictedDistributions/MOWA/MOWA_predictMap_m_10", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-11)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$MOWA_OCC, pred))),3)
}

md_11<-lme4::glmer(MOWA_OCC ~ poly(ndvi_lag_11,2) +  elev_0pnt15_to_2pnt00_return_proportion+ elev_2pnt00_to_4pnt00_return_proportion + elev_maximum *elev_stddev+   percentage_first_returns_above_mean+  (1|SS) , data=dd, family = binomial, offset = MOWA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_11_bootAUC<-bootMer(md_11, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_11_bootAUC$t[,1])
R2m_stdErr<-sd(md_11_bootAUC$t[,1])/sqrt(length(md_11_bootAUC$t[,1]))
R2c_mean<-mean(md_11_bootAUC$t[,2])
R2c_stdErr<-sd(md_11_bootAUC$t[,2])/sqrt(length(md_11_bootAUC$t[,2]))
AUC_mean<-mean(md_11_bootAUC$t[,3])
AUC_stdErr<-sd(md_11_bootAUC$t[,3])/sqrt(length(md_11_bootAUC$t[,3]))

R2m_sd<-sd(md_11_bootAUC$t[,1])
R2c_sd<-sd(md_11_bootAUC$t[,2])
AUC_sd<-sd(md_11_bootAUC$t[,3])

md_11_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

MOWA_predictMap_m_11<-raster::predict(scaled_stack, md_11, re.form=NA, type='response')
writeRaster(MOWA_predictMap_m_11,file="3_output/maps/predictedDistributions/MOWA/MOWA_predictMap_m_11", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-12)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$MOWA_OCC, pred))),3)
}

md_12<-lme4::glmer(MOWA_OCC ~ poly(ndvi_lag_12,2) +  elev_0pnt15_to_2pnt00_return_proportion+ elev_2pnt00_to_4pnt00_return_proportion + elev_maximum *elev_stddev+   percentage_first_returns_above_mean+  (1|SS) , data=dd, family = binomial, offset = MOWA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_12_bootAUC<-bootMer(md_12, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_12_bootAUC$t[,1])
R2m_stdErr<-sd(md_12_bootAUC$t[,1])/sqrt(length(md_12_bootAUC$t[,1]))
R2c_mean<-mean(md_12_bootAUC$t[,2])
R2c_stdErr<-sd(md_12_bootAUC$t[,2])/sqrt(length(md_12_bootAUC$t[,2]))
AUC_mean<-mean(md_12_bootAUC$t[,3])
AUC_stdErr<-sd(md_12_bootAUC$t[,3])/sqrt(length(md_12_bootAUC$t[,3]))

R2m_sd<-sd(md_12_bootAUC$t[,1])
R2c_sd<-sd(md_12_bootAUC$t[,2])
AUC_sd<-sd(md_12_bootAUC$t[,3])

md_12_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

MOWA_predictMap_m_12<-raster::predict(scaled_stack, md_12, re.form=NA, type='response')
writeRaster(MOWA_predictMap_m_12,file="3_output/maps/predictedDistributions/MOWA/MOWA_predictMap_m_12", format="GTiff", overwrite=T)

##########################
dd<-d3%>%filter(SS_lidar_timelag==-13)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$MOWA_OCC, pred))),3)
}

md_13<-lme4::glmer(MOWA_OCC ~ poly(ndvi_lag_13,2) +  elev_0pnt15_to_2pnt00_return_proportion+ elev_2pnt00_to_4pnt00_return_proportion + elev_maximum *elev_stddev+   percentage_first_returns_above_mean+  (1|SS) , data=dd, family = binomial, offset = MOWA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_13_bootAUC<-bootMer(md_13, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_13_bootAUC$t[,1])
R2m_stdErr<-sd(md_13_bootAUC$t[,1])/sqrt(length(md_13_bootAUC$t[,1]))
R2c_mean<-mean(md_13_bootAUC$t[,2])
R2c_stdErr<-sd(md_13_bootAUC$t[,2])/sqrt(length(md_13_bootAUC$t[,2]))
AUC_mean<-mean(md_13_bootAUC$t[,3])
AUC_stdErr<-sd(md_13_bootAUC$t[,3])/sqrt(length(md_13_bootAUC$t[,3]))

R2m_sd<-sd(md_13_bootAUC$t[,1])
R2c_sd<-sd(md_13_bootAUC$t[,2])
AUC_sd<-sd(md_13_bootAUC$t[,3])

md_13_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

MOWA_predictMap_m_13<-raster::predict(scaled_stack, md_13, re.form=NA, type='response')
writeRaster(MOWA_predictMap_m_13,file="3_output/maps/predictedDistributions/MOWA/MOWA_predictMap_m_13", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-14)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$MOWA_OCC, pred))),3)
}

md_14<-lme4::glmer(MOWA_OCC ~ poly(ndvi_lag_14,2) +  elev_0pnt15_to_2pnt00_return_proportion+ elev_2pnt00_to_4pnt00_return_proportion + elev_maximum *elev_stddev+   percentage_first_returns_above_mean+  (1|SS) , data=dd, family = binomial, offset = MOWA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_14_bootAUC<-bootMer(md_14, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_14_bootAUC$t[,1])
R2m_stdErr<-sd(md_14_bootAUC$t[,1])/sqrt(length(md_14_bootAUC$t[,1]))
R2c_mean<-mean(md_14_bootAUC$t[,2])
R2c_stdErr<-sd(md_14_bootAUC$t[,2])/sqrt(length(md_14_bootAUC$t[,2]))
AUC_mean<-mean(md_14_bootAUC$t[,3])
AUC_stdErr<-sd(md_14_bootAUC$t[,3])/sqrt(length(md_14_bootAUC$t[,3]))

R2m_sd<-sd(md_14_bootAUC$t[,1])
R2c_sd<-sd(md_14_bootAUC$t[,2])
AUC_sd<-sd(md_14_bootAUC$t[,3])

md_14_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

MOWA_predictMap_m_14<-raster::predict(scaled_stack, md_14, re.form=NA, type='response')
writeRaster(MOWA_predictMap_m_14,file="3_output/maps/predictedDistributions/MOWA/MOWA_predictMap_m_14", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-15)
set.seed(1)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$MOWA_OCC, pred))),3)
}

md_15<-lme4::glmer(MOWA_OCC ~ poly(ndvi_lag_15,2) +  elev_0pnt15_to_2pnt00_return_proportion+ elev_2pnt00_to_4pnt00_return_proportion + elev_maximum *elev_stddev+   percentage_first_returns_above_mean+  (1|SS) , data=dd, family = binomial, offset = MOWA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_15_bootAUC<-bootMer(md_15, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_15_bootAUC$t[,1])
R2m_stdErr<-sd(md_15_bootAUC$t[,1])/sqrt(length(md_15_bootAUC$t[,1]))
R2c_mean<-mean(md_15_bootAUC$t[,2])
R2c_stdErr<-sd(md_15_bootAUC$t[,2])/sqrt(length(md_15_bootAUC$t[,2]))
AUC_mean<-mean(md_15_bootAUC$t[,3])
AUC_stdErr<-sd(md_15_bootAUC$t[,3])/sqrt(length(md_15_bootAUC$t[,3]))
R2m_range<-range(md_15_bootAUC$t[,1])


R2m_sd<-sd(md_15_bootAUC$t[,1])
R2c_sd<-sd(md_15_bootAUC$t[,2])
AUC_sd<-sd(md_15_bootAUC$t[,3])

md_15_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)


MOWA_predictMap_m_15<-raster::predict(scaled_stack, md_15, re.form=NA, type='response')
writeRaster(MOWA_predictMap_m_15,file="3_output/maps/predictedDistributions/MOWA/MOWA_predictMap_m_15", format="GTiff", overwrite=T)

####################################################################
# save models
MOWA_models_4<-list(md_0, md_1, md_2, md_3, md_4, md_5, md_6, md_7, md_8, md_9, md_10, md_11, md_12, md_13, md_14, md_15)
names(MOWA_models_4) <- c("md_0", "md_1", "md_2", "md_3", "md_4", "md_5", "md_6", "md_7", "md_8", "md_9", "md_10", "md_11", "md_12", "md_13", "md_14", "md_15")
save(MOWA_models_4, file="2_pipeline/store/models/MOWA_models_4.rData")

####################################################################
# create an AUC dataframe
lag<-c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
model<-c("md_0", "md_1", "md_2", "md_3", "md_4", "md_5", "md_6", "md_7", "md_8", "md_9", "md_10", "md_11", "md_12", "md_13", "md_14", "md_15")

MOWA_models_AUC_4<-as.data.frame(rbind(md_0_meanAUC, md_1_meanAUC, md_2_meanAUC, md_3_meanAUC, md_4_meanAUC, md_5_meanAUC, md_6_meanAUC, md_7_meanAUC, md_8_meanAUC, md_9_meanAUC, md_10_meanAUC, md_11_meanAUC, md_12_meanAUC, md_13_meanAUC, md_14_meanAUC, md_15_meanAUC))%>%
  cbind(lag)

# MOWA_AUC_lm_test<-lm(AUC_mean~ lag, data=MOWA_models_AUC_4)
# summary(MOWA_AUC_lm_test)
# 
# MOWA_r2c_lm_test<-lm(R2c_mean~ lag, data=MOWA_models_AUC_4)
# summary(MOWA_r2c_lm_test)
# 
# MOWA_r2m_lm_test<-lm(R2m_mean~ lag, data=MOWA_models_AUC_4)
# summary(MOWA_r2m_lm_test)
# 
# ggplot(MOWA_models_AUC_4, aes(x=lag, y=AUC_mean)) + 
#     geom_errorbar(aes(ymin=AUC_mean-1.96*AUC_stdErr, ymax=AUC_mean+ 1.96*AUC_stdErr, width=.2))+
#     geom_point(size=1) + 
#     geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
#     geom_smooth(method = lm)+
#     theme(panel.background = element_blank(),
#           text = element_text(size=10),
#           axis.ticks=element_blank(),
#           #strip.background =element_rect(fill="white"),
#           panel.border = element_rect(color = "black", fill = NA, size = .2),
#           axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
#           axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
#           #panel.background = element_rect(fill="white"),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           legend.position="none")


save(MOWA_models_AUC_4, file="2_pipeline/store/models/MOWA_models_AUC_4.rData")

##############################################################
## save bootmer results
##############################################################
MOWA_models_bootAUC_4<-list(md_0_bootAUC, md_1_bootAUC, md_2_bootAUC, md_3_bootAUC, md_4_bootAUC, md_5_bootAUC, md_6_bootAUC, md_7_bootAUC, md_8_bootAUC, md_9_bootAUC, md_10_bootAUC, md_11_bootAUC, md_12_bootAUC, md_13_bootAUC, md_14_bootAUC, md_15_bootAUC)
names(MOWA_models_bootAUC_4) <- c("md_0_bootAUC", "md_1_bootAUC", "md_2_bootAUC", "md_3_bootAUC", "md_4_bootAUC", "md_5_bootAUC", "md_6_bootAUC", "md_7_bootAUC", "md_8_bootAUC", "md_9_bootAUC", "md_10_bootAUC", "md_11_bootAUC", "md_12_bootAUC", "md_13_bootAUC", "md_14_bootAUC", "md_15_bootAUC")
save(MOWA_models_bootAUC_4, file="2_pipeline/store/models/MOWA_models_bootAUC_4.rData")

##############################################################
## stack rasters
##############################################################

map_list<-list.files(path='3_output/maps/predictedDistributions/MOWA/predictMap', pattern = "*.tif$", full.names = TRUE)
MOWA_stack<-raster::stack(map_list)

#save stack as r object
stackSave(MOWA_stack,file="3_output/maps/predictedDistributions/MOWA/MOWA_stack.stk")

####################################################################################################################################
##SWTH##############################################################################################################################
####################################################################################################################################

#stats function for AUC

#dd<-d3%>%filter(SS_lidar_timelag==0)

dd<-d3%>%filter(SS_lidar_timelag==0)


# AUCFun <- function(x) {
#   library(pROC)
#   pred<-predict(x, type="response", allow.new.levels=TRUE)
#   AUC<-as.numeric(auc(d_0$SWTH_OCC, pred))
# }


#both AUC and R2
# AUCFun <- function(fit) {
#   library(pROC)
#   pred<-predict(fit, type="response", allow.new.levels=TRUE)
#   R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$SWTH_OCC, pred))),3)
#   }
#   
AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$SWTH_OCC, pred))),3)
}


#models
md_0<-lme4::glmer(SWTH_OCC ~ poly(ndvi_lag_0, 2) +  elev_maximum + elev_p50 + (1|SS) , data=dd, family = binomial, offset = SWTH_OFF,  na.action = na.fail, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)))


md_0_bootAUC<-bootMer(md_0, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

# meanAUC<-mean(md_0_bootAUC$t)
# std.err<-sd(md_0_bootAUC$t)/sqrt(length(md_0_bootAUC$t))
# CI.lower <- meanAUC - std.err*1.96
# CI.upper <- meanAUC + std.err*1.96
# 
# md_0_meanAUC<-data.frame(meanAUC, std.err, CI.lower, CI.upper)

R2m_mean<-mean(md_0_bootAUC$t[,1])
R2m_stdErr<-sd(md_0_bootAUC$t[,1])/sqrt(length(md_0_bootAUC$t[,1]))
R2c_mean<-mean(md_0_bootAUC$t[,2])
R2c_stdErr<-sd(md_0_bootAUC$t[,2])/sqrt(length(md_0_bootAUC$t[,2]))
AUC_mean<-mean(md_0_bootAUC$t[,3])
AUC_stdErr<-sd(md_0_bootAUC$t[,3])/sqrt(length(md_0_bootAUC$t[,3]))

R2m_sd<-sd(md_0_bootAUC$t[,1])
R2c_sd<-sd(md_0_bootAUC$t[,2])
AUC_sd<-sd(md_0_bootAUC$t[,3])

md_0_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)


SWTH_predictMap_m_0<-raster::predict(scaled_stack, md_0, re.form=NA, type='response')
writeRaster(SWTH_predictMap_m_0,file="3_output/maps/predictedDistributions/SWTH/SWTH_predictMap_m_00", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-1)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$SWTH_OCC, pred))),3)
}

md_1<-lme4::glmer(SWTH_OCC ~ poly(ndvi_lag_1, 2) +  elev_maximum + elev_p50 + (1|SS) , data=dd, family = binomial, offset = SWTH_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_1_bootAUC<-bootMer(md_1, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_1_bootAUC$t[,1])
R2m_stdErr<-sd(md_1_bootAUC$t[,1])/sqrt(length(md_1_bootAUC$t[,1]))
R2m_sd<-sd(md_1_bootAUC$t[,1])
R2c_mean<-mean(md_1_bootAUC$t[,2])
R2c_stdErr<-sd(md_1_bootAUC$t[,2])/sqrt(length(md_1_bootAUC$t[,2]))
R2c_sd<-sd(md_1_bootAUC$t[,2])
AUC_mean<-mean(md_1_bootAUC$t[,3])
AUC_stdErr<-sd(md_1_bootAUC$t[,3])/sqrt(length(md_1_bootAUC$t[,3]))
AUC_sd<-sd(md_1_bootAUC$t[,3])

R2m_sd<-sd(md_1_bootAUC$t[,1])
R2c_sd<-sd(md_1_bootAUC$t[,2])
AUC_sd<-sd(md_1_bootAUC$t[,3])

md_1_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

SWTH_predictMap_m_01<-raster::predict(scaled_stack, md_1, re.form=NA, type='response')
writeRaster(SWTH_predictMap_m_01,file="3_output/maps/predictedDistributions/SWTH/SWTH_predictMap_m_01", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-2)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$SWTH_OCC, pred))),3)
}

md_2<-lme4::glmer(SWTH_OCC ~ poly(ndvi_lag_2, 2) +  elev_maximum + elev_p50 + (1|SS) , data=dd, family = binomial, offset = SWTH_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_2_bootAUC<-bootMer(md_2, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_2_bootAUC$t[,1])
R2m_stdErr<-sd(md_2_bootAUC$t[,1])/sqrt(length(md_2_bootAUC$t[,1]))
R2c_mean<-mean(md_2_bootAUC$t[,2])
R2c_stdErr<-sd(md_2_bootAUC$t[,2])/sqrt(length(md_2_bootAUC$t[,2]))
AUC_mean<-mean(md_2_bootAUC$t[,3])
AUC_stdErr<-sd(md_2_bootAUC$t[,3])/sqrt(length(md_2_bootAUC$t[,3]))

R2m_sd<-sd(md_2_bootAUC$t[,1])
R2c_sd<-sd(md_2_bootAUC$t[,2])
AUC_sd<-sd(md_2_bootAUC$t[,3])

md_2_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

SWTH_predictMap_m_02<-raster::predict(scaled_stack, md_2, re.form=NA, type='response')
writeRaster(SWTH_predictMap_m_02,file="3_output/maps/predictedDistributions/SWTH/SWTH_predictMap_m_02", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-3)


AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$SWTH_OCC, pred))),3)
}

md_3<-lme4::glmer(SWTH_OCC ~ poly(ndvi_lag_3, 2) +  elev_maximum + elev_p50 + (1|SS) , data=dd, family = binomial, offset = SWTH_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_3_bootAUC<-bootMer(md_3, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_3_bootAUC$t[,1])
R2m_stdErr<-sd(md_3_bootAUC$t[,1])/sqrt(length(md_3_bootAUC$t[,1]))
R2c_mean<-mean(md_3_bootAUC$t[,2])
R2c_stdErr<-sd(md_3_bootAUC$t[,2])/sqrt(length(md_3_bootAUC$t[,2]))
AUC_mean<-mean(md_3_bootAUC$t[,3])
AUC_stdErr<-sd(md_3_bootAUC$t[,3])/sqrt(length(md_3_bootAUC$t[,3]))

R2m_sd<-sd(md_3_bootAUC$t[,1])
R2c_sd<-sd(md_3_bootAUC$t[,2])
AUC_sd<-sd(md_3_bootAUC$t[,3])

md_3_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

SWTH_predictMap_m_03<-raster::predict(scaled_stack, md_3, re.form=NA, type='response')
writeRaster(SWTH_predictMap_m_03,file="3_output/maps/predictedDistributions/SWTH/SWTH_predictMap_m_03", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-4)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$SWTH_OCC, pred))),3)
}

md_4<-lme4::glmer(SWTH_OCC ~ poly(ndvi_lag_4, 2) +  elev_maximum + elev_p50 + (1|SS) , data=dd, family = binomial, offset = SWTH_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_4_bootAUC<-bootMer(md_4, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_4_bootAUC$t[,1])
R2m_stdErr<-sd(md_4_bootAUC$t[,1])/sqrt(length(md_4_bootAUC$t[,1]))
R2c_mean<-mean(md_4_bootAUC$t[,2])
R2c_stdErr<-sd(md_4_bootAUC$t[,2])/sqrt(length(md_4_bootAUC$t[,2]))
AUC_mean<-mean(md_4_bootAUC$t[,3])
AUC_stdErr<-sd(md_4_bootAUC$t[,3])/sqrt(length(md_4_bootAUC$t[,3]))

R2m_sd<-sd(md_4_bootAUC$t[,1])
R2c_sd<-sd(md_4_bootAUC$t[,2])
AUC_sd<-sd(md_4_bootAUC$t[,3])

md_4_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

SWTH_predictMap_m_04<-raster::predict(scaled_stack, md_4, re.form=NA, type='response')
writeRaster(SWTH_predictMap_m_04,file="3_output/maps/predictedDistributions/SWTH/SWTH_predictMap_m_04", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-5)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$SWTH_OCC, pred))),3)
}

md_5<-lme4::glmer(SWTH_OCC ~ poly(ndvi_lag_5, 2) +  elev_maximum + elev_p50 + (1|SS) , data=dd, family = binomial, offset = SWTH_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_5_bootAUC<-bootMer(md_5, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_5_bootAUC$t[,1])
R2m_stdErr<-sd(md_5_bootAUC$t[,1])/sqrt(length(md_5_bootAUC$t[,1]))
R2c_mean<-mean(md_5_bootAUC$t[,2])
R2c_stdErr<-sd(md_5_bootAUC$t[,2])/sqrt(length(md_5_bootAUC$t[,2]))
AUC_mean<-mean(md_5_bootAUC$t[,3])
AUC_stdErr<-sd(md_5_bootAUC$t[,3])/sqrt(length(md_5_bootAUC$t[,3]))

R2m_sd<-sd(md_5_bootAUC$t[,1])
R2c_sd<-sd(md_5_bootAUC$t[,2])
AUC_sd<-sd(md_5_bootAUC$t[,3])

md_5_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

SWTH_predictMap_m_05<-raster::predict(scaled_stack, md_5, re.form=NA, type='response')
writeRaster(SWTH_predictMap_m_05,file="3_output/maps/predictedDistributions/SWTH/SWTH_predictMap_m_05", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-6)


AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$SWTH_OCC, pred))),3)
}

md_6<-lme4::glmer(SWTH_OCC ~ poly(ndvi_lag_6, 2) +  elev_maximum + elev_p50 + (1|SS) , data=dd, family = binomial, offset = SWTH_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_6_bootAUC<-bootMer(md_6, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_6_bootAUC$t[,1])
R2m_stdErr<-sd(md_6_bootAUC$t[,1])/sqrt(length(md_6_bootAUC$t[,1]))
R2c_mean<-mean(md_6_bootAUC$t[,2])
R2c_stdErr<-sd(md_6_bootAUC$t[,2])/sqrt(length(md_6_bootAUC$t[,2]))
AUC_mean<-mean(md_6_bootAUC$t[,3])
AUC_stdErr<-sd(md_6_bootAUC$t[,3])/sqrt(length(md_6_bootAUC$t[,3]))

R2m_sd<-sd(md_6_bootAUC$t[,1])
R2c_sd<-sd(md_6_bootAUC$t[,2])
AUC_sd<-sd(md_6_bootAUC$t[,3])

md_6_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)


SWTH_predictMap_m_06<-raster::predict(scaled_stack, md_6, re.form=NA, type='response')
writeRaster(SWTH_predictMap_m_06,file="3_output/maps/predictedDistributions/SWTH/SWTH_predictMap_m_06", format="GTiff", overwrite=T)

##########################
dd<-d3%>%filter(SS_lidar_timelag==-7)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$SWTH_OCC, pred))),3)
}

md_7<-lme4::glmer(SWTH_OCC ~ poly(ndvi_lag_7, 2) +  elev_maximum + elev_p50 + (1|SS) , data=dd, family = binomial, offset = SWTH_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_7_bootAUC<-bootMer(md_7, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_7_bootAUC$t[,1])
R2m_stdErr<-sd(md_7_bootAUC$t[,1])/sqrt(length(md_7_bootAUC$t[,1]))
R2c_mean<-mean(md_7_bootAUC$t[,2])
R2c_stdErr<-sd(md_7_bootAUC$t[,2])/sqrt(length(md_7_bootAUC$t[,2]))
AUC_mean<-mean(md_7_bootAUC$t[,3])
AUC_stdErr<-sd(md_7_bootAUC$t[,3])/sqrt(length(md_7_bootAUC$t[,3]))

R2m_sd<-sd(md_7_bootAUC$t[,1])
R2c_sd<-sd(md_7_bootAUC$t[,2])
AUC_sd<-sd(md_7_bootAUC$t[,3])

md_7_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

SWTH_predictMap_m_07<-raster::predict(scaled_stack, md_7, re.form=NA, type='response')
writeRaster(SWTH_predictMap_m_07,file="3_output/maps/predictedDistributions/SWTH/SWTH_predictMap_m_07", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-8)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$SWTH_OCC, pred))),3)
}

md_8<-lme4::glmer(SWTH_OCC ~ poly(ndvi_lag_8, 2) +  elev_maximum + elev_p50 + (1|SS) , data=dd, family = binomial, offset = SWTH_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_8_bootAUC<-bootMer(md_8, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_8_bootAUC$t[,1])
R2m_stdErr<-sd(md_8_bootAUC$t[,1])/sqrt(length(md_8_bootAUC$t[,1]))
R2c_mean<-mean(md_8_bootAUC$t[,2])
R2c_stdErr<-sd(md_8_bootAUC$t[,2])/sqrt(length(md_8_bootAUC$t[,2]))
AUC_mean<-mean(md_8_bootAUC$t[,3])
AUC_stdErr<-sd(md_8_bootAUC$t[,3])/sqrt(length(md_8_bootAUC$t[,3]))

R2m_sd<-sd(md_8_bootAUC$t[,1])
R2c_sd<-sd(md_8_bootAUC$t[,2])
AUC_sd<-sd(md_8_bootAUC$t[,3])

md_8_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

SWTH_predictMap_m_08<-raster::predict(scaled_stack, md_8, re.form=NA, type='response')
writeRaster(SWTH_predictMap_m_08,file="3_output/maps/predictedDistributions/SWTH/SWTH_predictMap_m_08", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-9)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$SWTH_OCC, pred))),3)
}

md_9<-lme4::glmer(SWTH_OCC ~ poly(ndvi_lag_9, 2) +  elev_maximum + elev_p50 + (1|SS) , data=dd, family = binomial, offset = SWTH_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_9_bootAUC<-bootMer(md_9, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_9_bootAUC$t[,1])
R2m_stdErr<-sd(md_9_bootAUC$t[,1])/sqrt(length(md_9_bootAUC$t[,1]))
R2c_mean<-mean(md_9_bootAUC$t[,2])
R2c_stdErr<-sd(md_9_bootAUC$t[,2])/sqrt(length(md_9_bootAUC$t[,2]))
AUC_mean<-mean(md_9_bootAUC$t[,3])
AUC_stdErr<-sd(md_9_bootAUC$t[,3])/sqrt(length(md_9_bootAUC$t[,3]))

R2m_sd<-sd(md_9_bootAUC$t[,1])
R2c_sd<-sd(md_9_bootAUC$t[,2])
AUC_sd<-sd(md_9_bootAUC$t[,3])

md_9_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)


SWTH_predictMap_m_09<-raster::predict(scaled_stack, md_9, re.form=NA, type='response')
writeRaster(SWTH_predictMap_m_09,file="3_output/maps/predictedDistributions/SWTH/SWTH_predictMap_m_09", format="GTiff", overwrite=T)

##########################
dd<-d3%>%filter(SS_lidar_timelag==-10)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$SWTH_OCC, pred))),3)
}

md_10<-lme4::glmer(SWTH_OCC ~ poly(ndvi_lag_10, 2) +  elev_maximum + elev_p50 + (1|SS) , data=dd, family = binomial, offset = SWTH_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_10_bootAUC<-bootMer(md_10, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_10_bootAUC$t[,1])
R2m_stdErr<-sd(md_10_bootAUC$t[,1])/sqrt(length(md_10_bootAUC$t[,1]))
R2c_mean<-mean(md_10_bootAUC$t[,2])
R2c_stdErr<-sd(md_10_bootAUC$t[,2])/sqrt(length(md_10_bootAUC$t[,2]))
AUC_mean<-mean(md_10_bootAUC$t[,3])
AUC_stdErr<-sd(md_10_bootAUC$t[,3])/sqrt(length(md_10_bootAUC$t[,3]))

R2m_sd<-sd(md_10_bootAUC$t[,1])
R2c_sd<-sd(md_10_bootAUC$t[,2])
AUC_sd<-sd(md_10_bootAUC$t[,3])

md_10_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

SWTH_predictMap_m_10<-raster::predict(scaled_stack, md_10, re.form=NA, type='response')
writeRaster(SWTH_predictMap_m_10,file="3_output/maps/predictedDistributions/SWTH/SWTH_predictMap_m_10", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-11)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$SWTH_OCC, pred))),3)
}

md_11<-lme4::glmer(SWTH_OCC ~ poly(ndvi_lag_11, 2) +  elev_maximum + elev_p50 + (1|SS) , data=dd, family = binomial, offset = SWTH_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_11_bootAUC<-bootMer(md_11, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_11_bootAUC$t[,1])
R2m_stdErr<-sd(md_11_bootAUC$t[,1])/sqrt(length(md_11_bootAUC$t[,1]))
R2c_mean<-mean(md_11_bootAUC$t[,2])
R2c_stdErr<-sd(md_11_bootAUC$t[,2])/sqrt(length(md_11_bootAUC$t[,2]))
AUC_mean<-mean(md_11_bootAUC$t[,3])
AUC_stdErr<-sd(md_11_bootAUC$t[,3])/sqrt(length(md_11_bootAUC$t[,3]))

R2m_sd<-sd(md_11_bootAUC$t[,1])
R2c_sd<-sd(md_11_bootAUC$t[,2])
AUC_sd<-sd(md_11_bootAUC$t[,3])

md_11_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

SWTH_predictMap_m_11<-raster::predict(scaled_stack, md_11, re.form=NA, type='response')
writeRaster(SWTH_predictMap_m_11,file="3_output/maps/predictedDistributions/SWTH/SWTH_predictMap_m_11", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-12)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$SWTH_OCC, pred))),3)
}

md_12<-lme4::glmer(SWTH_OCC ~ poly(ndvi_lag_12, 2) +  elev_maximum + elev_p50 + (1|SS) , data=dd, family = binomial, offset = SWTH_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_12_bootAUC<-bootMer(md_12, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_12_bootAUC$t[,1])
R2m_stdErr<-sd(md_12_bootAUC$t[,1])/sqrt(length(md_12_bootAUC$t[,1]))
R2c_mean<-mean(md_12_bootAUC$t[,2])
R2c_stdErr<-sd(md_12_bootAUC$t[,2])/sqrt(length(md_12_bootAUC$t[,2]))
AUC_mean<-mean(md_12_bootAUC$t[,3])
AUC_stdErr<-sd(md_12_bootAUC$t[,3])/sqrt(length(md_12_bootAUC$t[,3]))

R2m_sd<-sd(md_12_bootAUC$t[,1])
R2c_sd<-sd(md_12_bootAUC$t[,2])
AUC_sd<-sd(md_12_bootAUC$t[,3])

md_12_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

SWTH_predictMap_m_12<-raster::predict(scaled_stack, md_12, re.form=NA, type='response')
writeRaster(SWTH_predictMap_m_12,file="3_output/maps/predictedDistributions/SWTH/SWTH_predictMap_m_12", format="GTiff", overwrite=T)

##########################
dd<-d3%>%filter(SS_lidar_timelag==-13)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$SWTH_OCC, pred))),3)
}

md_13<-lme4::glmer(SWTH_OCC ~ poly(ndvi_lag_13, 2) +  elev_maximum + elev_p50 + (1|SS) , data=dd, family = binomial, offset = SWTH_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_13_bootAUC<-bootMer(md_13, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_13_bootAUC$t[,1])
R2m_stdErr<-sd(md_13_bootAUC$t[,1])/sqrt(length(md_13_bootAUC$t[,1]))
R2c_mean<-mean(md_13_bootAUC$t[,2])
R2c_stdErr<-sd(md_13_bootAUC$t[,2])/sqrt(length(md_13_bootAUC$t[,2]))
AUC_mean<-mean(md_13_bootAUC$t[,3])
AUC_stdErr<-sd(md_13_bootAUC$t[,3])/sqrt(length(md_13_bootAUC$t[,3]))

R2m_sd<-sd(md_13_bootAUC$t[,1])
R2c_sd<-sd(md_13_bootAUC$t[,2])
AUC_sd<-sd(md_13_bootAUC$t[,3])

md_13_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

SWTH_predictMap_m_13<-raster::predict(scaled_stack, md_13, re.form=NA, type='response')
writeRaster(SWTH_predictMap_m_13,file="3_output/maps/predictedDistributions/SWTH/SWTH_predictMap_m_13", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-14)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$SWTH_OCC, pred))),3)
}

md_14<-lme4::glmer(SWTH_OCC ~ poly(ndvi_lag_14, 2) +  elev_maximum + elev_p50 + (1|SS) , data=dd, family = binomial, offset = SWTH_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_14_bootAUC<-bootMer(md_14, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_14_bootAUC$t[,1])
R2m_stdErr<-sd(md_14_bootAUC$t[,1])/sqrt(length(md_14_bootAUC$t[,1]))
R2c_mean<-mean(md_14_bootAUC$t[,2])
R2c_stdErr<-sd(md_14_bootAUC$t[,2])/sqrt(length(md_14_bootAUC$t[,2]))
AUC_mean<-mean(md_14_bootAUC$t[,3])
AUC_stdErr<-sd(md_14_bootAUC$t[,3])/sqrt(length(md_14_bootAUC$t[,3]))

R2m_sd<-sd(md_14_bootAUC$t[,1])
R2c_sd<-sd(md_14_bootAUC$t[,2])
AUC_sd<-sd(md_14_bootAUC$t[,3])

md_14_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

SWTH_predictMap_m_14<-raster::predict(scaled_stack, md_14, re.form=NA, type='response')
writeRaster(SWTH_predictMap_m_14,file="3_output/maps/predictedDistributions/SWTH/SWTH_predictMap_m_14", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-15)
set.seed(1)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$SWTH_OCC, pred))),3)
}

md_15<-lme4::glmer(SWTH_OCC ~ poly(ndvi_lag_15, 2) +  elev_maximum + elev_p50 + (1|SS) , data=dd, family = binomial, offset = SWTH_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_15_bootAUC<-bootMer(md_15, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_15_bootAUC$t[,1])
R2m_stdErr<-sd(md_15_bootAUC$t[,1])/sqrt(length(md_15_bootAUC$t[,1]))
R2c_mean<-mean(md_15_bootAUC$t[,2])
R2c_stdErr<-sd(md_15_bootAUC$t[,2])/sqrt(length(md_15_bootAUC$t[,2]))
AUC_mean<-mean(md_15_bootAUC$t[,3])
AUC_stdErr<-sd(md_15_bootAUC$t[,3])/sqrt(length(md_15_bootAUC$t[,3]))
R2m_range<-range(md_15_bootAUC$t[,1])


R2m_sd<-sd(md_15_bootAUC$t[,1])
R2c_sd<-sd(md_15_bootAUC$t[,2])
AUC_sd<-sd(md_15_bootAUC$t[,3])

md_15_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)


SWTH_predictMap_m_15<-raster::predict(scaled_stack, md_15, re.form=NA, type='response')
writeRaster(SWTH_predictMap_m_15,file="3_output/maps/predictedDistributions/SWTH/SWTH_predictMap_m_15", format="GTiff", overwrite=T)


####################################################################
# save models
SWTH_models_4<-list(md_0, md_1, md_2, md_3, md_4, md_5, md_6, md_7, md_8, md_9, md_10, md_11, md_12, md_13, md_14, md_15)
names(SWTH_models_4) <- c("md_0", "md_1", "md_2", "md_3", "md_4", "md_5", "md_6", "md_7", "md_8", "md_9", "md_10", "md_11", "md_12", "md_13", "md_14", "md_15")
save(SWTH_models_4, file="2_pipeline/store/models/SWTH_models_4.rData")

####################################################################
# create an AUC dataframe
lag<-c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
model<-c("md_0", "md_1", "md_2", "md_3", "md_4", "md_5", "md_6", "md_7", "md_8", "md_9", "md_10", "md_11", "md_12", "md_13", "md_14", "md_15")

SWTH_models_AUC_4<-as.data.frame(rbind(md_0_meanAUC, md_1_meanAUC, md_2_meanAUC, md_3_meanAUC, md_4_meanAUC, md_5_meanAUC, md_6_meanAUC, md_7_meanAUC, md_8_meanAUC, md_9_meanAUC, md_10_meanAUC, md_11_meanAUC, md_12_meanAUC, md_13_meanAUC, md_14_meanAUC, md_15_meanAUC))%>%
  cbind(lag)

# SWTH_AUC_lm_test<-lm(AUC_mean~ lag, data=SWTH_models_AUC_4)
# summary(SWTH_AUC_lm_test)
# 
# SWTH_r2c_lm_test<-lm(R2c_mean~ lag, data=SWTH_models_AUC_4)
# summary(SWTH_r2c_lm_test)
# 
# SWTH_r2m_lm_test<-lm(R2m_mean~ lag, data=SWTH_models_AUC_4)
# summary(SWTH_r2m_lm_test)
# 
# ggplot(SWTH_models_AUC_4, aes(x=lag, y=AUC_mean)) + 
#     geom_errorbar(aes(ymin=AUC_mean-1.96*AUC_stdErr, ymax=AUC_mean+ 1.96*AUC_stdErr, width=.2))+
#     geom_point(size=1) + 
#     geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
#     geom_smooth(method = lm)+
#     theme(panel.background = element_blank(),
#           text = element_text(size=10),
#           axis.ticks=element_blank(),
#           #strip.background =element_rect(fill="white"),
#           panel.border = element_rect(color = "black", fill = NA, size = .2),
#           axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
#           axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
#           #panel.background = element_rect(fill="white"),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           legend.position="none")



save(SWTH_models_AUC_4, file="2_pipeline/store/models/SWTH_models_AUC_4.rData")

##############################################################
## save bootmer results
##############################################################
SWTH_models_bootAUC_4<-list(md_0_bootAUC, md_1_bootAUC, md_2_bootAUC, md_3_bootAUC, md_4_bootAUC, md_5_bootAUC, md_6_bootAUC, md_7_bootAUC, md_8_bootAUC, md_9_bootAUC, md_10_bootAUC, md_11_bootAUC, md_12_bootAUC, md_13_bootAUC, md_14_bootAUC, md_15_bootAUC)
names(SWTH_models_bootAUC_4) <- c("md_0_bootAUC", "md_1_bootAUC", "md_2_bootAUC", "md_3_bootAUC", "md_4_bootAUC", "md_5_bootAUC", "md_6_bootAUC", "md_7_bootAUC", "md_8_bootAUC", "md_9_bootAUC", "md_10_bootAUC", "md_11_bootAUC", "md_12_bootAUC", "md_13_bootAUC", "md_14_bootAUC", "md_15_bootAUC")
save(SWTH_models_bootAUC_4, file="2_pipeline/store/models/SWTH_models_bootAUC_4.rData")

##############################################################
## stack rasters
##############################################################

map_list<-list.files(path='3_output/maps/predictedDistributions/SWTH/predictMap', pattern = "*.tif$", full.names = TRUE)
SWTH_stack<-raster::stack(map_list)

#save stack as r object
stackSave(SWTH_stack,file="3_output/maps/predictedDistributions/SWTH/SWTH_stack.stk")
####################################################################################################################################
##WIWR##############################################################################################################################
####################################################################################################################################

#stats function for AUC

#dd<-d3%>%filter(SS_lidar_timelag==0)

dd<-d3%>%filter(SS_lidar_timelag==0)


# AUCFun <- function(x) {
#   library(pROC)
#   pred<-predict(x, type="response", allow.new.levels=TRUE)
#   AUC<-as.numeric(auc(d_0$WIWR_OCC, pred))
# }


#both AUC and R2
# AUCFun <- function(fit) {
#   library(pROC)
#   pred<-predict(fit, type="response", allow.new.levels=TRUE)
#   R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WIWR_OCC, pred))),3)
#   }
#   
AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WIWR_OCC, pred))),3)
}


#models
md_0<-lme4::glmer(WIWR_OCC ~ ndvi_lag_0 +  canopy_relief_ratio+ elev_4pnt00_to_6pnt00_return_proportion+ elev_maximum +   percentage_first_returns_above_mean+(1|SS) , data=dd, family = binomial, offset = WIWR_OFF,  na.action = na.fail, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)))


md_0_bootAUC<-bootMer(md_0, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)


# meanAUC<-mean(md_0_bootAUC$t)
# std.err<-sd(md_0_bootAUC$t)/sqrt(length(md_0_bootAUC$t))
# CI.lower <- meanAUC - std.err*1.96
# CI.upper <- meanAUC + std.err*1.96
# 
# md_0_meanAUC<-data.frame(meanAUC, std.err, CI.lower, CI.upper)

R2m_mean<-mean(md_0_bootAUC$t[,1])
R2m_stdErr<-sd(md_0_bootAUC$t[,1])/sqrt(length(md_0_bootAUC$t[,1]))
R2c_mean<-mean(md_0_bootAUC$t[,2])
R2c_stdErr<-sd(md_0_bootAUC$t[,2])/sqrt(length(md_0_bootAUC$t[,2]))
AUC_mean<-mean(md_0_bootAUC$t[,3])
AUC_stdErr<-sd(md_0_bootAUC$t[,3])/sqrt(length(md_0_bootAUC$t[,3]))

R2m_sd<-sd(md_0_bootAUC$t[,1])
R2c_sd<-sd(md_0_bootAUC$t[,2])
AUC_sd<-sd(md_0_bootAUC$t[,3])

md_0_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)


WIWR_predictMap_m_0<-raster::predict(scaled_stack, md_0, re.form=NA, type='response')
writeRaster(WIWR_predictMap_m_0,file="3_output/maps/predictedDistributions/WIWR/WIWR_predictMap_m_00", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-1)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WIWR_OCC, pred))),3)
}

md_1<-lme4::glmer(WIWR_OCC ~ ndvi_lag_1 +  canopy_relief_ratio+ elev_4pnt00_to_6pnt00_return_proportion+ elev_maximum +   percentage_first_returns_above_mean+(1|SS) , data=dd, family = binomial, offset = WIWR_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_1_bootAUC<-bootMer(md_1, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_1_bootAUC$t[,1])
R2m_stdErr<-sd(md_1_bootAUC$t[,1])/sqrt(length(md_1_bootAUC$t[,1]))
R2m_sd<-sd(md_1_bootAUC$t[,1])
R2c_mean<-mean(md_1_bootAUC$t[,2])
R2c_stdErr<-sd(md_1_bootAUC$t[,2])/sqrt(length(md_1_bootAUC$t[,2]))
R2c_sd<-sd(md_1_bootAUC$t[,2])
AUC_mean<-mean(md_1_bootAUC$t[,3])
AUC_stdErr<-sd(md_1_bootAUC$t[,3])/sqrt(length(md_1_bootAUC$t[,3]))
AUC_sd<-sd(md_1_bootAUC$t[,3])

R2m_sd<-sd(md_1_bootAUC$t[,1])
R2c_sd<-sd(md_1_bootAUC$t[,2])
AUC_sd<-sd(md_1_bootAUC$t[,3])

md_1_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

WIWR_predictMap_m_01<-raster::predict(scaled_stack, md_1, re.form=NA, type='response')
writeRaster(WIWR_predictMap_m_01,file="3_output/maps/predictedDistributions/WIWR/WIWR_predictMap_m_01", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-2)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WIWR_OCC, pred))),3)
}

md_2<-lme4::glmer(WIWR_OCC ~ ndvi_lag_2 +  canopy_relief_ratio+ elev_4pnt00_to_6pnt00_return_proportion+ elev_maximum +   percentage_first_returns_above_mean+(1|SS) , data=dd, family = binomial, offset = WIWR_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_2_bootAUC<-bootMer(md_2, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_2_bootAUC$t[,1])
R2m_stdErr<-sd(md_2_bootAUC$t[,1])/sqrt(length(md_2_bootAUC$t[,1]))
R2c_mean<-mean(md_2_bootAUC$t[,2])
R2c_stdErr<-sd(md_2_bootAUC$t[,2])/sqrt(length(md_2_bootAUC$t[,2]))
AUC_mean<-mean(md_2_bootAUC$t[,3])
AUC_stdErr<-sd(md_2_bootAUC$t[,3])/sqrt(length(md_2_bootAUC$t[,3]))

R2m_sd<-sd(md_2_bootAUC$t[,1])
R2c_sd<-sd(md_2_bootAUC$t[,2])
AUC_sd<-sd(md_2_bootAUC$t[,3])

md_2_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

WIWR_predictMap_m_02<-raster::predict(scaled_stack, md_2, re.form=NA, type='response')
writeRaster(WIWR_predictMap_m_02,file="3_output/maps/predictedDistributions/WIWR/WIWR_predictMap_m_02", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-3)


AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WIWR_OCC, pred))),3)
}

md_3<-lme4::glmer(WIWR_OCC ~ ndvi_lag_3 +  canopy_relief_ratio+ elev_4pnt00_to_6pnt00_return_proportion+ elev_maximum +   percentage_first_returns_above_mean+(1|SS) , data=dd, family = binomial, offset = WIWR_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_3_bootAUC<-bootMer(md_3, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_3_bootAUC$t[,1])
R2m_stdErr<-sd(md_3_bootAUC$t[,1])/sqrt(length(md_3_bootAUC$t[,1]))
R2c_mean<-mean(md_3_bootAUC$t[,2])
R2c_stdErr<-sd(md_3_bootAUC$t[,2])/sqrt(length(md_3_bootAUC$t[,2]))
AUC_mean<-mean(md_3_bootAUC$t[,3])
AUC_stdErr<-sd(md_3_bootAUC$t[,3])/sqrt(length(md_3_bootAUC$t[,3]))

R2m_sd<-sd(md_3_bootAUC$t[,1])
R2c_sd<-sd(md_3_bootAUC$t[,2])
AUC_sd<-sd(md_3_bootAUC$t[,3])

md_3_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

WIWR_predictMap_m_03<-raster::predict(scaled_stack, md_3, re.form=NA, type='response')
writeRaster(WIWR_predictMap_m_03,file="3_output/maps/predictedDistributions/WIWR/WIWR_predictMap_m_03", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-4)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WIWR_OCC, pred))),3)
}

md_4<-lme4::glmer(WIWR_OCC ~ ndvi_lag_4 +  canopy_relief_ratio+ elev_4pnt00_to_6pnt00_return_proportion+ elev_maximum +   percentage_first_returns_above_mean+(1|SS) , data=dd, family = binomial, offset = WIWR_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_4_bootAUC<-bootMer(md_4, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_4_bootAUC$t[,1])
R2m_stdErr<-sd(md_4_bootAUC$t[,1])/sqrt(length(md_4_bootAUC$t[,1]))
R2c_mean<-mean(md_4_bootAUC$t[,2])
R2c_stdErr<-sd(md_4_bootAUC$t[,2])/sqrt(length(md_4_bootAUC$t[,2]))
AUC_mean<-mean(md_4_bootAUC$t[,3])
AUC_stdErr<-sd(md_4_bootAUC$t[,3])/sqrt(length(md_4_bootAUC$t[,3]))

R2m_sd<-sd(md_4_bootAUC$t[,1])
R2c_sd<-sd(md_4_bootAUC$t[,2])
AUC_sd<-sd(md_4_bootAUC$t[,3])

md_4_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

WIWR_predictMap_m_04<-raster::predict(scaled_stack, md_4, re.form=NA, type='response')
writeRaster(WIWR_predictMap_m_04,file="3_output/maps/predictedDistributions/WIWR/WIWR_predictMap_m_04", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-5)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WIWR_OCC, pred))),3)
}

md_5<-lme4::glmer(WIWR_OCC ~ ndvi_lag_5 +  canopy_relief_ratio+ elev_4pnt00_to_6pnt00_return_proportion+ elev_maximum +   percentage_first_returns_above_mean+(1|SS) , data=dd, family = binomial, offset = WIWR_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_5_bootAUC<-bootMer(md_5, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_5_bootAUC$t[,1])
R2m_stdErr<-sd(md_5_bootAUC$t[,1])/sqrt(length(md_5_bootAUC$t[,1]))
R2c_mean<-mean(md_5_bootAUC$t[,2])
R2c_stdErr<-sd(md_5_bootAUC$t[,2])/sqrt(length(md_5_bootAUC$t[,2]))
AUC_mean<-mean(md_5_bootAUC$t[,3])
AUC_stdErr<-sd(md_5_bootAUC$t[,3])/sqrt(length(md_5_bootAUC$t[,3]))

R2m_sd<-sd(md_5_bootAUC$t[,1])
R2c_sd<-sd(md_5_bootAUC$t[,2])
AUC_sd<-sd(md_5_bootAUC$t[,3])

md_5_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

WIWR_predictMap_m_05<-raster::predict(scaled_stack, md_5, re.form=NA, type='response')
writeRaster(WIWR_predictMap_m_05,file="3_output/maps/predictedDistributions/WIWR/WIWR_predictMap_m_05", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-6)


AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WIWR_OCC, pred))),3)
}

md_6<-lme4::glmer(WIWR_OCC ~ ndvi_lag_6 +  canopy_relief_ratio+ elev_4pnt00_to_6pnt00_return_proportion+ elev_maximum +   percentage_first_returns_above_mean+(1|SS) , data=dd, family = binomial, offset = WIWR_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_6_bootAUC<-bootMer(md_6, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_6_bootAUC$t[,1])
R2m_stdErr<-sd(md_6_bootAUC$t[,1])/sqrt(length(md_6_bootAUC$t[,1]))
R2c_mean<-mean(md_6_bootAUC$t[,2])
R2c_stdErr<-sd(md_6_bootAUC$t[,2])/sqrt(length(md_6_bootAUC$t[,2]))
AUC_mean<-mean(md_6_bootAUC$t[,3])
AUC_stdErr<-sd(md_6_bootAUC$t[,3])/sqrt(length(md_6_bootAUC$t[,3]))

R2m_sd<-sd(md_6_bootAUC$t[,1])
R2c_sd<-sd(md_6_bootAUC$t[,2])
AUC_sd<-sd(md_6_bootAUC$t[,3])

md_6_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)


WIWR_predictMap_m_06<-raster::predict(scaled_stack, md_6, re.form=NA, type='response')
writeRaster(WIWR_predictMap_m_06,file="3_output/maps/predictedDistributions/WIWR/WIWR_predictMap_m_06", format="GTiff", overwrite=T)

##########################
dd<-d3%>%filter(SS_lidar_timelag==-7)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WIWR_OCC, pred))),3)
}

md_7<-lme4::glmer(WIWR_OCC ~ ndvi_lag_7 +  canopy_relief_ratio+ elev_4pnt00_to_6pnt00_return_proportion+ elev_maximum +   percentage_first_returns_above_mean+(1|SS) , data=dd, family = binomial, offset = WIWR_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_7_bootAUC<-bootMer(md_7, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_7_bootAUC$t[,1])
R2m_stdErr<-sd(md_7_bootAUC$t[,1])/sqrt(length(md_7_bootAUC$t[,1]))
R2c_mean<-mean(md_7_bootAUC$t[,2])
R2c_stdErr<-sd(md_7_bootAUC$t[,2])/sqrt(length(md_7_bootAUC$t[,2]))
AUC_mean<-mean(md_7_bootAUC$t[,3])
AUC_stdErr<-sd(md_7_bootAUC$t[,3])/sqrt(length(md_7_bootAUC$t[,3]))

R2m_sd<-sd(md_7_bootAUC$t[,1])
R2c_sd<-sd(md_7_bootAUC$t[,2])
AUC_sd<-sd(md_7_bootAUC$t[,3])

md_7_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

WIWR_predictMap_m_07<-raster::predict(scaled_stack, md_7, re.form=NA, type='response')
writeRaster(WIWR_predictMap_m_07,file="3_output/maps/predictedDistributions/WIWR/WIWR_predictMap_m_07", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-8)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WIWR_OCC, pred))),3)
}

md_8<-lme4::glmer(WIWR_OCC ~ ndvi_lag_8 +  canopy_relief_ratio+ elev_4pnt00_to_6pnt00_return_proportion+ elev_maximum +   percentage_first_returns_above_mean+(1|SS) , data=dd, family = binomial, offset = WIWR_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_8_bootAUC<-bootMer(md_8, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_8_bootAUC$t[,1])
R2m_stdErr<-sd(md_8_bootAUC$t[,1])/sqrt(length(md_8_bootAUC$t[,1]))
R2c_mean<-mean(md_8_bootAUC$t[,2])
R2c_stdErr<-sd(md_8_bootAUC$t[,2])/sqrt(length(md_8_bootAUC$t[,2]))
AUC_mean<-mean(md_8_bootAUC$t[,3])
AUC_stdErr<-sd(md_8_bootAUC$t[,3])/sqrt(length(md_8_bootAUC$t[,3]))

R2m_sd<-sd(md_8_bootAUC$t[,1])
R2c_sd<-sd(md_8_bootAUC$t[,2])
AUC_sd<-sd(md_8_bootAUC$t[,3])

md_8_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

WIWR_predictMap_m_08<-raster::predict(scaled_stack, md_8, re.form=NA, type='response')
writeRaster(WIWR_predictMap_m_08,file="3_output/maps/predictedDistributions/WIWR/WIWR_predictMap_m_08", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-9)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WIWR_OCC, pred))),3)
}

md_9<-lme4::glmer(WIWR_OCC ~ ndvi_lag_9 +  canopy_relief_ratio+ elev_4pnt00_to_6pnt00_return_proportion+ elev_maximum +   percentage_first_returns_above_mean+(1|SS) , data=dd, family = binomial, offset = WIWR_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_9_bootAUC<-bootMer(md_9, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_9_bootAUC$t[,1])
R2m_stdErr<-sd(md_9_bootAUC$t[,1])/sqrt(length(md_9_bootAUC$t[,1]))
R2c_mean<-mean(md_9_bootAUC$t[,2])
R2c_stdErr<-sd(md_9_bootAUC$t[,2])/sqrt(length(md_9_bootAUC$t[,2]))
AUC_mean<-mean(md_9_bootAUC$t[,3])
AUC_stdErr<-sd(md_9_bootAUC$t[,3])/sqrt(length(md_9_bootAUC$t[,3]))

R2m_sd<-sd(md_9_bootAUC$t[,1])
R2c_sd<-sd(md_9_bootAUC$t[,2])
AUC_sd<-sd(md_9_bootAUC$t[,3])

md_9_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)


WIWR_predictMap_m_09<-raster::predict(scaled_stack, md_9, re.form=NA, type='response')
writeRaster(WIWR_predictMap_m_09,file="3_output/maps/predictedDistributions/WIWR/WIWR_predictMap_m_09", format="GTiff", overwrite=T)

##########################
dd<-d3%>%filter(SS_lidar_timelag==-10)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WIWR_OCC, pred))),3)
}

md_10<-lme4::glmer(WIWR_OCC ~ ndvi_lag_10 +  canopy_relief_ratio+ elev_4pnt00_to_6pnt00_return_proportion+ elev_maximum +   percentage_first_returns_above_mean+(1|SS) , data=dd, family = binomial, offset = WIWR_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_10_bootAUC<-bootMer(md_10, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_10_bootAUC$t[,1])
R2m_stdErr<-sd(md_10_bootAUC$t[,1])/sqrt(length(md_10_bootAUC$t[,1]))
R2c_mean<-mean(md_10_bootAUC$t[,2])
R2c_stdErr<-sd(md_10_bootAUC$t[,2])/sqrt(length(md_10_bootAUC$t[,2]))
AUC_mean<-mean(md_10_bootAUC$t[,3])
AUC_stdErr<-sd(md_10_bootAUC$t[,3])/sqrt(length(md_10_bootAUC$t[,3]))

R2m_sd<-sd(md_10_bootAUC$t[,1])
R2c_sd<-sd(md_10_bootAUC$t[,2])
AUC_sd<-sd(md_10_bootAUC$t[,3])

md_10_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

WIWR_predictMap_m_10<-raster::predict(scaled_stack, md_10, re.form=NA, type='response')
writeRaster(WIWR_predictMap_m_10,file="3_output/maps/predictedDistributions/WIWR/WIWR_predictMap_m_10", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-11)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WIWR_OCC, pred))),3)
}

md_11<-lme4::glmer(WIWR_OCC ~ ndvi_lag_11 +  canopy_relief_ratio+ elev_4pnt00_to_6pnt00_return_proportion+ elev_maximum +   percentage_first_returns_above_mean+(1|SS) , data=dd, family = binomial, offset = WIWR_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_11_bootAUC<-bootMer(md_11, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_11_bootAUC$t[,1])
R2m_stdErr<-sd(md_11_bootAUC$t[,1])/sqrt(length(md_11_bootAUC$t[,1]))
R2c_mean<-mean(md_11_bootAUC$t[,2])
R2c_stdErr<-sd(md_11_bootAUC$t[,2])/sqrt(length(md_11_bootAUC$t[,2]))
AUC_mean<-mean(md_11_bootAUC$t[,3])
AUC_stdErr<-sd(md_11_bootAUC$t[,3])/sqrt(length(md_11_bootAUC$t[,3]))

R2m_sd<-sd(md_11_bootAUC$t[,1])
R2c_sd<-sd(md_11_bootAUC$t[,2])
AUC_sd<-sd(md_11_bootAUC$t[,3])

md_11_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

WIWR_predictMap_m_11<-raster::predict(scaled_stack, md_11, re.form=NA, type='response')
writeRaster(WIWR_predictMap_m_11,file="3_output/maps/predictedDistributions/WIWR/WIWR_predictMap_m_11", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-12)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WIWR_OCC, pred))),3)
}

md_12<-lme4::glmer(WIWR_OCC ~ ndvi_lag_12 +  canopy_relief_ratio+ elev_4pnt00_to_6pnt00_return_proportion+ elev_maximum +   percentage_first_returns_above_mean+(1|SS) , data=dd, family = binomial, offset = WIWR_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_12_bootAUC<-bootMer(md_12, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_12_bootAUC$t[,1])
R2m_stdErr<-sd(md_12_bootAUC$t[,1])/sqrt(length(md_12_bootAUC$t[,1]))
R2c_mean<-mean(md_12_bootAUC$t[,2])
R2c_stdErr<-sd(md_12_bootAUC$t[,2])/sqrt(length(md_12_bootAUC$t[,2]))
AUC_mean<-mean(md_12_bootAUC$t[,3])
AUC_stdErr<-sd(md_12_bootAUC$t[,3])/sqrt(length(md_12_bootAUC$t[,3]))

R2m_sd<-sd(md_12_bootAUC$t[,1])
R2c_sd<-sd(md_12_bootAUC$t[,2])
AUC_sd<-sd(md_12_bootAUC$t[,3])

md_12_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

WIWR_predictMap_m_12<-raster::predict(scaled_stack, md_12, re.form=NA, type='response')
writeRaster(WIWR_predictMap_m_12,file="3_output/maps/predictedDistributions/WIWR/WIWR_predictMap_m_12", format="GTiff", overwrite=T)

##########################
dd<-d3%>%filter(SS_lidar_timelag==-13)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WIWR_OCC, pred))),3)
}

md_13<-lme4::glmer(WIWR_OCC ~ ndvi_lag_13 +  canopy_relief_ratio+ elev_4pnt00_to_6pnt00_return_proportion+ elev_maximum +   percentage_first_returns_above_mean+(1|SS) , data=dd, family = binomial, offset = WIWR_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_13_bootAUC<-bootMer(md_13, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_13_bootAUC$t[,1])
R2m_stdErr<-sd(md_13_bootAUC$t[,1])/sqrt(length(md_13_bootAUC$t[,1]))
R2c_mean<-mean(md_13_bootAUC$t[,2])
R2c_stdErr<-sd(md_13_bootAUC$t[,2])/sqrt(length(md_13_bootAUC$t[,2]))
AUC_mean<-mean(md_13_bootAUC$t[,3])
AUC_stdErr<-sd(md_13_bootAUC$t[,3])/sqrt(length(md_13_bootAUC$t[,3]))

R2m_sd<-sd(md_13_bootAUC$t[,1])
R2c_sd<-sd(md_13_bootAUC$t[,2])
AUC_sd<-sd(md_13_bootAUC$t[,3])

md_13_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

WIWR_predictMap_m_13<-raster::predict(scaled_stack, md_13, re.form=NA, type='response')
writeRaster(WIWR_predictMap_m_13,file="3_output/maps/predictedDistributions/WIWR/WIWR_predictMap_m_13", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-14)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WIWR_OCC, pred))),3)
}

md_14<-lme4::glmer(WIWR_OCC ~ ndvi_lag_14 +  canopy_relief_ratio+ elev_4pnt00_to_6pnt00_return_proportion+ elev_maximum +   percentage_first_returns_above_mean+(1|SS) , data=dd, family = binomial, offset = WIWR_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_14_bootAUC<-bootMer(md_14, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_14_bootAUC$t[,1])
R2m_stdErr<-sd(md_14_bootAUC$t[,1])/sqrt(length(md_14_bootAUC$t[,1]))
R2c_mean<-mean(md_14_bootAUC$t[,2])
R2c_stdErr<-sd(md_14_bootAUC$t[,2])/sqrt(length(md_14_bootAUC$t[,2]))
AUC_mean<-mean(md_14_bootAUC$t[,3])
AUC_stdErr<-sd(md_14_bootAUC$t[,3])/sqrt(length(md_14_bootAUC$t[,3]))

R2m_sd<-sd(md_14_bootAUC$t[,1])
R2c_sd<-sd(md_14_bootAUC$t[,2])
AUC_sd<-sd(md_14_bootAUC$t[,3])

md_14_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

WIWR_predictMap_m_14<-raster::predict(scaled_stack, md_14, re.form=NA, type='response')
writeRaster(WIWR_predictMap_m_14,file="3_output/maps/predictedDistributions/WIWR/WIWR_predictMap_m_14", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-15)
set.seed(1)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WIWR_OCC, pred))),3)
}

md_15<-lme4::glmer(WIWR_OCC ~ ndvi_lag_15 +  canopy_relief_ratio+ elev_4pnt00_to_6pnt00_return_proportion+ elev_maximum +   percentage_first_returns_above_mean+(1|SS) , data=dd, family = binomial, offset = WIWR_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_15_bootAUC<-bootMer(md_15, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_15_bootAUC$t[,1])
R2m_stdErr<-sd(md_15_bootAUC$t[,1])/sqrt(length(md_15_bootAUC$t[,1]))
R2c_mean<-mean(md_15_bootAUC$t[,2])
R2c_stdErr<-sd(md_15_bootAUC$t[,2])/sqrt(length(md_15_bootAUC$t[,2]))
AUC_mean<-mean(md_15_bootAUC$t[,3])
AUC_stdErr<-sd(md_15_bootAUC$t[,3])/sqrt(length(md_15_bootAUC$t[,3]))
R2m_range<-range(md_15_bootAUC$t[,1])


R2m_sd<-sd(md_15_bootAUC$t[,1])
R2c_sd<-sd(md_15_bootAUC$t[,2])
AUC_sd<-sd(md_15_bootAUC$t[,3])

md_15_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

WIWR_predictMap_m_15<-raster::predict(scaled_stack, md_15, re.form=NA, type='response')
writeRaster(WIWR_predictMap_m_15,file="3_output/maps/predictedDistributions/WIWR/WIWR_predictMap_m_15", format="GTiff", overwrite=T)

####################################################################
# save models
WIWR_models_4<-list(md_0, md_1, md_2, md_3, md_4, md_5, md_6, md_7, md_8, md_9, md_10, md_11, md_12, md_13, md_14, md_15)
names(WIWR_models_4) <- c("md_0", "md_1", "md_2", "md_3", "md_4", "md_5", "md_6", "md_7", "md_8", "md_9", "md_10", "md_11", "md_12", "md_13", "md_14", "md_15")
save(WIWR_models_4, file="2_pipeline/store/models/WIWR_models_4.rData")

####################################################################
# create an AUC dataframe
lag<-c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
model<-c("md_0", "md_1", "md_2", "md_3", "md_4", "md_5", "md_6", "md_7", "md_8", "md_9", "md_10", "md_11", "md_12", "md_13", "md_14", "md_15")

WIWR_models_AUC_4<-as.data.frame(rbind(md_0_meanAUC, md_1_meanAUC, md_2_meanAUC, md_3_meanAUC, md_4_meanAUC, md_5_meanAUC, md_6_meanAUC, md_7_meanAUC, md_8_meanAUC, md_9_meanAUC, md_10_meanAUC, md_11_meanAUC, md_12_meanAUC, md_13_meanAUC, md_14_meanAUC, md_15_meanAUC))%>%
  cbind(lag)

# WIWR_AUC_lm_test<-lm(AUC_mean~ lag, data=WIWR_models_AUC_4)
# summary(WIWR_AUC_lm_test)
# 
# WIWR_r2c_lm_test<-lm(R2c_mean~ lag, data=WIWR_models_AUC_4)
# summary(WIWR_r2c_lm_test)
# 
# WIWR_r2m_lm_test<-lm(R2m_mean~ lag, data=WIWR_models_AUC_4)
# summary(WIWR_r2m_lm_test)
# 
# ggplot(WIWR_models_AUC_4, aes(x=lag, y=AUC_mean)) + 
#     geom_errorbar(aes(ymin=AUC_mean-1.96*AUC_stdErr, ymax=AUC_mean+ 1.96*AUC_stdErr, width=.2))+
#     geom_point(size=1) + 
#     geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
#     geom_smooth(method = lm)+
#     theme(panel.background = element_blank(),
#           text = element_text(size=10),
#           axis.ticks=element_blank(),
#           #strip.background =element_rect(fill="white"),
#           panel.border = element_rect(color = "black", fill = NA, size = .2),
#           axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
#           axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
#           #panel.background = element_rect(fill="white"),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           legend.position="none")


save(WIWR_models_AUC_4, file="2_pipeline/store/models/WIWR_models_AUC_4.rData")

##############################################################
## save bootmer results
##############################################################
WIWR_models_bootAUC_4<-list(md_0_bootAUC, md_1_bootAUC, md_2_bootAUC, md_3_bootAUC, md_4_bootAUC, md_5_bootAUC, md_6_bootAUC, md_7_bootAUC, md_8_bootAUC, md_9_bootAUC, md_10_bootAUC, md_11_bootAUC, md_12_bootAUC, md_13_bootAUC, md_14_bootAUC, md_15_bootAUC)
names(WIWR_models_bootAUC_4) <- c("md_0_bootAUC", "md_1_bootAUC", "md_2_bootAUC", "md_3_bootAUC", "md_4_bootAUC", "md_5_bootAUC", "md_6_bootAUC", "md_7_bootAUC", "md_8_bootAUC", "md_9_bootAUC", "md_10_bootAUC", "md_11_bootAUC", "md_12_bootAUC", "md_13_bootAUC", "md_14_bootAUC", "md_15_bootAUC")
save(WIWR_models_bootAUC_4, file="2_pipeline/store/models/WIWR_models_bootAUC_4.rData")

##############################################################
## stack rasters
##############################################################

map_list<-list.files(path='3_output/maps/predictedDistributions/WIWR/predictMap', pattern = "*.tif$", full.names = TRUE)
WIWR_stack<-raster::stack(map_list)

#save stack as r object
stackSave(WIWR_stack,file="3_output/maps/predictedDistributions/WIWR/WIWR_stack.stk")

####################################################################################################################################
##WTSP##############################################################################################################################
####################################################################################################################################
#stats function for AUC

#dd<-d3%>%filter(SS_lidar_timelag==0)

dd<-d3%>%filter(SS_lidar_timelag==0)


# AUCFun <- function(x) {
#   library(pROC)
#   pred<-predict(x, type="response", allow.new.levels=TRUE)
#   AUC<-as.numeric(auc(d_0$WTSP_OCC, pred))
# }


#both AUC and R2
# AUCFun <- function(fit) {
#   library(pROC)
#   pred<-predict(fit, type="response", allow.new.levels=TRUE)
#   R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WTSP_OCC, pred))),3)
#   }
#   
AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WTSP_OCC, pred))),3)
}


#models
md_0<-lme4::glmer(WTSP_OCC ~ ndvi_lag_0  + elev_cv +  elev_maximum + percentage_first_returns_above_2pnt00 + (1|SS) , data=dd, family = binomial, offset = WTSP_OFF,  na.action = na.fail, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)))


md_0_bootAUC<-bootMer(md_0, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

# meanAUC<-mean(md_0_bootAUC$t)
# std.err<-sd(md_0_bootAUC$t)/sqrt(length(md_0_bootAUC$t))
# CI.lower <- meanAUC - std.err*1.96
# CI.upper <- meanAUC + std.err*1.96
# 
# md_0_meanAUC<-data.frame(meanAUC, std.err, CI.lower, CI.upper)

R2m_mean<-mean(md_0_bootAUC$t[,1])
R2m_stdErr<-sd(md_0_bootAUC$t[,1])/sqrt(length(md_0_bootAUC$t[,1]))
R2c_mean<-mean(md_0_bootAUC$t[,2])
R2c_stdErr<-sd(md_0_bootAUC$t[,2])/sqrt(length(md_0_bootAUC$t[,2]))
AUC_mean<-mean(md_0_bootAUC$t[,3])
AUC_stdErr<-sd(md_0_bootAUC$t[,3])/sqrt(length(md_0_bootAUC$t[,3]))

R2m_sd<-sd(md_0_bootAUC$t[,1])
R2c_sd<-sd(md_0_bootAUC$t[,2])
AUC_sd<-sd(md_0_bootAUC$t[,3])

md_0_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)


WTSP_predictMap_m_0<-raster::predict(scaled_stack, md_0, re.form=NA, type='response')
writeRaster(WTSP_predictMap_m_0,file="3_output/maps/predictedDistributions/WTSP/WTSP_predictMap_m_00", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-1)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WTSP_OCC, pred))),3)
}

md_1<-lme4::glmer(WTSP_OCC ~ ndvi_lag_1  + elev_cv +  elev_maximum + percentage_first_returns_above_2pnt00 + (1|SS) , data=dd, family = binomial, offset = WTSP_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_1_bootAUC<-bootMer(md_1, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_1_bootAUC$t[,1])
R2m_stdErr<-sd(md_1_bootAUC$t[,1])/sqrt(length(md_1_bootAUC$t[,1]))
R2m_sd<-sd(md_1_bootAUC$t[,1])
R2c_mean<-mean(md_1_bootAUC$t[,2])
R2c_stdErr<-sd(md_1_bootAUC$t[,2])/sqrt(length(md_1_bootAUC$t[,2]))
R2c_sd<-sd(md_1_bootAUC$t[,2])
AUC_mean<-mean(md_1_bootAUC$t[,3])
AUC_stdErr<-sd(md_1_bootAUC$t[,3])/sqrt(length(md_1_bootAUC$t[,3]))
AUC_sd<-sd(md_1_bootAUC$t[,3])

R2m_sd<-sd(md_1_bootAUC$t[,1])
R2c_sd<-sd(md_1_bootAUC$t[,2])
AUC_sd<-sd(md_1_bootAUC$t[,3])

md_1_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

WTSP_predictMap_m_01<-raster::predict(scaled_stack, md_1, re.form=NA, type='response')
writeRaster(WTSP_predictMap_m_01,file="3_output/maps/predictedDistributions/WTSP/WTSP_predictMap_m_01", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-2)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WTSP_OCC, pred))),3)
}

md_2<-lme4::glmer(WTSP_OCC ~ ndvi_lag_2  + elev_cv +  elev_maximum + percentage_first_returns_above_2pnt00 + (1|SS) , data=dd, family = binomial, offset = WTSP_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_2_bootAUC<-bootMer(md_2, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_2_bootAUC$t[,1])
R2m_stdErr<-sd(md_2_bootAUC$t[,1])/sqrt(length(md_2_bootAUC$t[,1]))
R2c_mean<-mean(md_2_bootAUC$t[,2])
R2c_stdErr<-sd(md_2_bootAUC$t[,2])/sqrt(length(md_2_bootAUC$t[,2]))
AUC_mean<-mean(md_2_bootAUC$t[,3])
AUC_stdErr<-sd(md_2_bootAUC$t[,3])/sqrt(length(md_2_bootAUC$t[,3]))

R2m_sd<-sd(md_2_bootAUC$t[,1])
R2c_sd<-sd(md_2_bootAUC$t[,2])
AUC_sd<-sd(md_2_bootAUC$t[,3])

md_2_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

WTSP_predictMap_m_02<-raster::predict(scaled_stack, md_2, re.form=NA, type='response')
writeRaster(WTSP_predictMap_m_02,file="3_output/maps/predictedDistributions/WTSP/WTSP_predictMap_m_02", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-3)


AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WTSP_OCC, pred))),3)
}

md_3<-lme4::glmer(WTSP_OCC ~ ndvi_lag_3  + elev_cv +  elev_maximum + percentage_first_returns_above_2pnt00 + (1|SS) , data=dd, family = binomial, offset = WTSP_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_3_bootAUC<-bootMer(md_3, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_3_bootAUC$t[,1])
R2m_stdErr<-sd(md_3_bootAUC$t[,1])/sqrt(length(md_3_bootAUC$t[,1]))
R2c_mean<-mean(md_3_bootAUC$t[,2])
R2c_stdErr<-sd(md_3_bootAUC$t[,2])/sqrt(length(md_3_bootAUC$t[,2]))
AUC_mean<-mean(md_3_bootAUC$t[,3])
AUC_stdErr<-sd(md_3_bootAUC$t[,3])/sqrt(length(md_3_bootAUC$t[,3]))

R2m_sd<-sd(md_3_bootAUC$t[,1])
R2c_sd<-sd(md_3_bootAUC$t[,2])
AUC_sd<-sd(md_3_bootAUC$t[,3])

md_3_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

WTSP_predictMap_m_03<-raster::predict(scaled_stack, md_3, re.form=NA, type='response')
writeRaster(WTSP_predictMap_m_03,file="3_output/maps/predictedDistributions/WTSP/WTSP_predictMap_m_03", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-4)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WTSP_OCC, pred))),3)
}

md_4<-lme4::glmer(WTSP_OCC ~ ndvi_lag_4  + elev_cv +  elev_maximum + percentage_first_returns_above_2pnt00 + (1|SS) , data=dd, family = binomial, offset = WTSP_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_4_bootAUC<-bootMer(md_4, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_4_bootAUC$t[,1])
R2m_stdErr<-sd(md_4_bootAUC$t[,1])/sqrt(length(md_4_bootAUC$t[,1]))
R2c_mean<-mean(md_4_bootAUC$t[,2])
R2c_stdErr<-sd(md_4_bootAUC$t[,2])/sqrt(length(md_4_bootAUC$t[,2]))
AUC_mean<-mean(md_4_bootAUC$t[,3])
AUC_stdErr<-sd(md_4_bootAUC$t[,3])/sqrt(length(md_4_bootAUC$t[,3]))

R2m_sd<-sd(md_4_bootAUC$t[,1])
R2c_sd<-sd(md_4_bootAUC$t[,2])
AUC_sd<-sd(md_4_bootAUC$t[,3])

md_4_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

WTSP_predictMap_m_04<-raster::predict(scaled_stack, md_4, re.form=NA, type='response')
writeRaster(WTSP_predictMap_m_04,file="3_output/maps/predictedDistributions/WTSP/WTSP_predictMap_m_04", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-5)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WTSP_OCC, pred))),3)
}

md_5<-lme4::glmer(WTSP_OCC ~ ndvi_lag_5  + elev_cv +  elev_maximum + percentage_first_returns_above_2pnt00 + (1|SS) , data=dd, family = binomial, offset = WTSP_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_5_bootAUC<-bootMer(md_5, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_5_bootAUC$t[,1])
R2m_stdErr<-sd(md_5_bootAUC$t[,1])/sqrt(length(md_5_bootAUC$t[,1]))
R2c_mean<-mean(md_5_bootAUC$t[,2])
R2c_stdErr<-sd(md_5_bootAUC$t[,2])/sqrt(length(md_5_bootAUC$t[,2]))
AUC_mean<-mean(md_5_bootAUC$t[,3])
AUC_stdErr<-sd(md_5_bootAUC$t[,3])/sqrt(length(md_5_bootAUC$t[,3]))

R2m_sd<-sd(md_5_bootAUC$t[,1])
R2c_sd<-sd(md_5_bootAUC$t[,2])
AUC_sd<-sd(md_5_bootAUC$t[,3])

md_5_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

WTSP_predictMap_m_05<-raster::predict(scaled_stack, md_5, re.form=NA, type='response')
writeRaster(WTSP_predictMap_m_05,file="3_output/maps/predictedDistributions/WTSP/WTSP_predictMap_m_05", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-6)


AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WTSP_OCC, pred))),3)
}

md_6<-lme4::glmer(WTSP_OCC ~ ndvi_lag_6  + elev_cv +  elev_maximum + percentage_first_returns_above_2pnt00 + (1|SS) , data=dd, family = binomial, offset = WTSP_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_6_bootAUC<-bootMer(md_6, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_6_bootAUC$t[,1])
R2m_stdErr<-sd(md_6_bootAUC$t[,1])/sqrt(length(md_6_bootAUC$t[,1]))
R2c_mean<-mean(md_6_bootAUC$t[,2])
R2c_stdErr<-sd(md_6_bootAUC$t[,2])/sqrt(length(md_6_bootAUC$t[,2]))
AUC_mean<-mean(md_6_bootAUC$t[,3])
AUC_stdErr<-sd(md_6_bootAUC$t[,3])/sqrt(length(md_6_bootAUC$t[,3]))

R2m_sd<-sd(md_6_bootAUC$t[,1])
R2c_sd<-sd(md_6_bootAUC$t[,2])
AUC_sd<-sd(md_6_bootAUC$t[,3])

md_6_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)


WTSP_predictMap_m_06<-raster::predict(scaled_stack, md_6, re.form=NA, type='response')
writeRaster(WTSP_predictMap_m_06,file="3_output/maps/predictedDistributions/WTSP/WTSP_predictMap_m_06", format="GTiff", overwrite=T)

##########################
dd<-d3%>%filter(SS_lidar_timelag==-7)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WTSP_OCC, pred))),3)
}

md_7<-lme4::glmer(WTSP_OCC ~ ndvi_lag_7  + elev_cv +  elev_maximum + percentage_first_returns_above_2pnt00 + (1|SS) , data=dd, family = binomial, offset = WTSP_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_7_bootAUC<-bootMer(md_7, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_7_bootAUC$t[,1])
R2m_stdErr<-sd(md_7_bootAUC$t[,1])/sqrt(length(md_7_bootAUC$t[,1]))
R2c_mean<-mean(md_7_bootAUC$t[,2])
R2c_stdErr<-sd(md_7_bootAUC$t[,2])/sqrt(length(md_7_bootAUC$t[,2]))
AUC_mean<-mean(md_7_bootAUC$t[,3])
AUC_stdErr<-sd(md_7_bootAUC$t[,3])/sqrt(length(md_7_bootAUC$t[,3]))

R2m_sd<-sd(md_7_bootAUC$t[,1])
R2c_sd<-sd(md_7_bootAUC$t[,2])
AUC_sd<-sd(md_7_bootAUC$t[,3])

md_7_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

WTSP_predictMap_m_07<-raster::predict(scaled_stack, md_7, re.form=NA, type='response')
writeRaster(WTSP_predictMap_m_07,file="3_output/maps/predictedDistributions/WTSP/WTSP_predictMap_m_07", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-8)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WTSP_OCC, pred))),3)
}

md_8<-lme4::glmer(WTSP_OCC ~ ndvi_lag_8  + elev_cv +  elev_maximum + percentage_first_returns_above_2pnt00 + (1|SS) , data=dd, family = binomial, offset = WTSP_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_8_bootAUC<-bootMer(md_8, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_8_bootAUC$t[,1])
R2m_stdErr<-sd(md_8_bootAUC$t[,1])/sqrt(length(md_8_bootAUC$t[,1]))
R2c_mean<-mean(md_8_bootAUC$t[,2])
R2c_stdErr<-sd(md_8_bootAUC$t[,2])/sqrt(length(md_8_bootAUC$t[,2]))
AUC_mean<-mean(md_8_bootAUC$t[,3])
AUC_stdErr<-sd(md_8_bootAUC$t[,3])/sqrt(length(md_8_bootAUC$t[,3]))

R2m_sd<-sd(md_8_bootAUC$t[,1])
R2c_sd<-sd(md_8_bootAUC$t[,2])
AUC_sd<-sd(md_8_bootAUC$t[,3])

md_8_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

WTSP_predictMap_m_08<-raster::predict(scaled_stack, md_8, re.form=NA, type='response')
writeRaster(WTSP_predictMap_m_08,file="3_output/maps/predictedDistributions/WTSP/WTSP_predictMap_m_08", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-9)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WTSP_OCC, pred))),3)
}

md_9<-lme4::glmer(WTSP_OCC ~ ndvi_lag_9 +  + elev_cv +  elev_maximum + percentage_first_returns_above_2pnt00 + (1|SS) , data=dd, family = binomial, offset = WTSP_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_9_bootAUC<-bootMer(md_9, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_9_bootAUC$t[,1])
R2m_stdErr<-sd(md_9_bootAUC$t[,1])/sqrt(length(md_9_bootAUC$t[,1]))
R2c_mean<-mean(md_9_bootAUC$t[,2])
R2c_stdErr<-sd(md_9_bootAUC$t[,2])/sqrt(length(md_9_bootAUC$t[,2]))
AUC_mean<-mean(md_9_bootAUC$t[,3])
AUC_stdErr<-sd(md_9_bootAUC$t[,3])/sqrt(length(md_9_bootAUC$t[,3]))

R2m_sd<-sd(md_9_bootAUC$t[,1])
R2c_sd<-sd(md_9_bootAUC$t[,2])
AUC_sd<-sd(md_9_bootAUC$t[,3])

md_9_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)


WTSP_predictMap_m_09<-raster::predict(scaled_stack, md_9, re.form=NA, type='response')
writeRaster(WTSP_predictMap_m_09,file="3_output/maps/predictedDistributions/WTSP/WTSP_predictMap_m_09", format="GTiff", overwrite=T)

##########################
dd<-d3%>%filter(SS_lidar_timelag==-10)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WTSP_OCC, pred))),3)
}

md_10<-lme4::glmer(WTSP_OCC ~ ndvi_lag_10  + elev_cv +  elev_maximum + percentage_first_returns_above_2pnt00 + (1|SS) , data=dd, family = binomial, offset = WTSP_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_10_bootAUC<-bootMer(md_10, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_10_bootAUC$t[,1])
R2m_stdErr<-sd(md_10_bootAUC$t[,1])/sqrt(length(md_10_bootAUC$t[,1]))
R2c_mean<-mean(md_10_bootAUC$t[,2])
R2c_stdErr<-sd(md_10_bootAUC$t[,2])/sqrt(length(md_10_bootAUC$t[,2]))
AUC_mean<-mean(md_10_bootAUC$t[,3])
AUC_stdErr<-sd(md_10_bootAUC$t[,3])/sqrt(length(md_10_bootAUC$t[,3]))

R2m_sd<-sd(md_10_bootAUC$t[,1])
R2c_sd<-sd(md_10_bootAUC$t[,2])
AUC_sd<-sd(md_10_bootAUC$t[,3])

md_10_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

WTSP_predictMap_m_10<-raster::predict(scaled_stack, md_10, re.form=NA, type='response')
writeRaster(WTSP_predictMap_m_10,file="3_output/maps/predictedDistributions/WTSP/WTSP_predictMap_m_10", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-11)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WTSP_OCC, pred))),3)
}

md_11<-lme4::glmer(WTSP_OCC ~ ndvi_lag_11  + elev_cv +  elev_maximum + percentage_first_returns_above_2pnt00 + (1|SS) , data=dd, family = binomial, offset = WTSP_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_11_bootAUC<-bootMer(md_11, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_11_bootAUC$t[,1])
R2m_stdErr<-sd(md_11_bootAUC$t[,1])/sqrt(length(md_11_bootAUC$t[,1]))
R2c_mean<-mean(md_11_bootAUC$t[,2])
R2c_stdErr<-sd(md_11_bootAUC$t[,2])/sqrt(length(md_11_bootAUC$t[,2]))
AUC_mean<-mean(md_11_bootAUC$t[,3])
AUC_stdErr<-sd(md_11_bootAUC$t[,3])/sqrt(length(md_11_bootAUC$t[,3]))

R2m_sd<-sd(md_11_bootAUC$t[,1])
R2c_sd<-sd(md_11_bootAUC$t[,2])
AUC_sd<-sd(md_11_bootAUC$t[,3])

md_11_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

WTSP_predictMap_m_11<-raster::predict(scaled_stack, md_11, re.form=NA, type='response')
writeRaster(WTSP_predictMap_m_11,file="3_output/maps/predictedDistributions/WTSP/WTSP_predictMap_m_11", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-12)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WTSP_OCC, pred))),3)
}

md_12<-lme4::glmer(WTSP_OCC ~ ndvi_lag_12  + elev_cv +  elev_maximum + percentage_first_returns_above_2pnt00 + (1|SS) , data=dd, family = binomial, offset = WTSP_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_12_bootAUC<-bootMer(md_12, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_12_bootAUC$t[,1])
R2m_stdErr<-sd(md_12_bootAUC$t[,1])/sqrt(length(md_12_bootAUC$t[,1]))
R2c_mean<-mean(md_12_bootAUC$t[,2])
R2c_stdErr<-sd(md_12_bootAUC$t[,2])/sqrt(length(md_12_bootAUC$t[,2]))
AUC_mean<-mean(md_12_bootAUC$t[,3])
AUC_stdErr<-sd(md_12_bootAUC$t[,3])/sqrt(length(md_12_bootAUC$t[,3]))

R2m_sd<-sd(md_12_bootAUC$t[,1])
R2c_sd<-sd(md_12_bootAUC$t[,2])
AUC_sd<-sd(md_12_bootAUC$t[,3])

md_12_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

WTSP_predictMap_m_12<-raster::predict(scaled_stack, md_12, re.form=NA, type='response')
writeRaster(WTSP_predictMap_m_12,file="3_output/maps/predictedDistributions/WTSP/WTSP_predictMap_m_12", format="GTiff", overwrite=T)

##########################
dd<-d3%>%filter(SS_lidar_timelag==-13)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WTSP_OCC, pred))),3)
}

md_13<-lme4::glmer(WTSP_OCC ~ ndvi_lag_13  + elev_cv +  elev_maximum + percentage_first_returns_above_2pnt00 + (1|SS) , data=dd, family = binomial, offset = WTSP_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_13_bootAUC<-bootMer(md_13, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_13_bootAUC$t[,1])
R2m_stdErr<-sd(md_13_bootAUC$t[,1])/sqrt(length(md_13_bootAUC$t[,1]))
R2c_mean<-mean(md_13_bootAUC$t[,2])
R2c_stdErr<-sd(md_13_bootAUC$t[,2])/sqrt(length(md_13_bootAUC$t[,2]))
AUC_mean<-mean(md_13_bootAUC$t[,3])
AUC_stdErr<-sd(md_13_bootAUC$t[,3])/sqrt(length(md_13_bootAUC$t[,3]))

R2m_sd<-sd(md_13_bootAUC$t[,1])
R2c_sd<-sd(md_13_bootAUC$t[,2])
AUC_sd<-sd(md_13_bootAUC$t[,3])

md_13_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

WTSP_predictMap_m_13<-raster::predict(scaled_stack, md_13, re.form=NA, type='response')
writeRaster(WTSP_predictMap_m_13,file="3_output/maps/predictedDistributions/WTSP/WTSP_predictMap_m_13", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-14)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WTSP_OCC, pred))),3)
}

md_14<-lme4::glmer(WTSP_OCC ~ ndvi_lag_14  + elev_cv +  elev_maximum + percentage_first_returns_above_2pnt00 + (1|SS) , data=dd, family = binomial, offset = WTSP_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_14_bootAUC<-bootMer(md_14, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_14_bootAUC$t[,1])
R2m_stdErr<-sd(md_14_bootAUC$t[,1])/sqrt(length(md_14_bootAUC$t[,1]))
R2c_mean<-mean(md_14_bootAUC$t[,2])
R2c_stdErr<-sd(md_14_bootAUC$t[,2])/sqrt(length(md_14_bootAUC$t[,2]))
AUC_mean<-mean(md_14_bootAUC$t[,3])
AUC_stdErr<-sd(md_14_bootAUC$t[,3])/sqrt(length(md_14_bootAUC$t[,3]))

R2m_sd<-sd(md_14_bootAUC$t[,1])
R2c_sd<-sd(md_14_bootAUC$t[,2])
AUC_sd<-sd(md_14_bootAUC$t[,3])

md_14_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

WTSP_predictMap_m_14<-raster::predict(scaled_stack, md_14, re.form=NA, type='response')
writeRaster(WTSP_predictMap_m_14,file="3_output/maps/predictedDistributions/WTSP/WTSP_predictMap_m_14", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-15)
set.seed(1)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$WTSP_OCC, pred))),3)
}

md_15<-lme4::glmer(WTSP_OCC ~ ndvi_lag_15  + elev_cv +  elev_maximum + percentage_first_returns_above_2pnt00 + (1|SS) , data=dd, family = binomial, offset = WTSP_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_15_bootAUC<-bootMer(md_15, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_15_bootAUC$t[,1])
R2m_stdErr<-sd(md_15_bootAUC$t[,1])/sqrt(length(md_15_bootAUC$t[,1]))
R2c_mean<-mean(md_15_bootAUC$t[,2])
R2c_stdErr<-sd(md_15_bootAUC$t[,2])/sqrt(length(md_15_bootAUC$t[,2]))
AUC_mean<-mean(md_15_bootAUC$t[,3])
AUC_stdErr<-sd(md_15_bootAUC$t[,3])/sqrt(length(md_15_bootAUC$t[,3]))
R2m_range<-range(md_15_bootAUC$t[,1])


R2m_sd<-sd(md_15_bootAUC$t[,1])
R2c_sd<-sd(md_15_bootAUC$t[,2])
AUC_sd<-sd(md_15_bootAUC$t[,3])

md_15_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)


WTSP_predictMap_m_15<-raster::predict(scaled_stack, md_15, re.form=NA, type='response')
writeRaster(WTSP_predictMap_m_15,file="3_output/maps/predictedDistributions/WTSP/WTSP_predictMap_m_15", format="GTiff", overwrite=T)


####################################################################
# save models
WTSP_models_4<-list(md_0, md_1, md_2, md_3, md_4, md_5, md_6, md_7, md_8, md_9, md_10, md_11, md_12, md_13, md_14, md_15)
names(WTSP_models_4) <- c("md_0", "md_1", "md_2", "md_3", "md_4", "md_5", "md_6", "md_7", "md_8", "md_9", "md_10", "md_11", "md_12", "md_13", "md_14", "md_15")
save(WTSP_models_4, file="2_pipeline/store/models/WTSP_models_4.rData")

####################################################################
# create an AUC dataframe
lag<-c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
model<-c("md_0", "md_1", "md_2", "md_3", "md_4", "md_5", "md_6", "md_7", "md_8", "md_9", "md_10", "md_11", "md_12", "md_13", "md_14", "md_15")

WTSP_models_AUC_4<-as.data.frame(rbind(md_0_meanAUC, md_1_meanAUC, md_2_meanAUC, md_3_meanAUC, md_4_meanAUC, md_5_meanAUC, md_6_meanAUC, md_7_meanAUC, md_8_meanAUC, md_9_meanAUC, md_10_meanAUC, md_11_meanAUC, md_12_meanAUC, md_13_meanAUC, md_14_meanAUC, md_15_meanAUC))%>%
  cbind(lag)

# WTSP_AUC_lm_test<-lm(AUC_mean~ lag, data=WTSP_models_AUC_4)
# summary(WTSP_AUC_lm_test)
# 
# WTSP_r2c_lm_test<-lm(R2c_mean~ lag, data=WTSP_models_AUC_4)
# summary(WTSP_r2c_lm_test)
# 
# WTSP_r2m_lm_test<-lm(R2m_mean~ lag, data=WTSP_models_AUC_4)
# summary(WTSP_r2m_lm_test)
# 
# ggplot(WTSP_models_AUC_4, aes(x=lag, y=AUC_mean)) + 
#     geom_errorbar(aes(ymin=AUC_mean-1.96*AUC_stdErr, ymax=AUC_mean+ 1.96*AUC_stdErr, width=.2))+
#     geom_point(size=1) + 
#     geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
#     geom_smooth(method = lm)+
#     theme(panel.background = element_blank(),
#           text = element_text(size=10),
#           axis.ticks=element_blank(),
#           #strip.background =element_rect(fill="white"),
#           panel.border = element_rect(color = "black", fill = NA, size = .2),
#           axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
#           axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
#           #panel.background = element_rect(fill="white"),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#legend.position="none")



save(WTSP_models_AUC_4, file="2_pipeline/store/models/WTSP_models_AUC_4.rData")

##############################################################
## save bootmer results
##############################################################
WTSP_models_bootAUC_4<-list(md_0_bootAUC, md_1_bootAUC, md_2_bootAUC, md_3_bootAUC, md_4_bootAUC, md_5_bootAUC, md_6_bootAUC, md_7_bootAUC, md_8_bootAUC, md_9_bootAUC, md_10_bootAUC, md_11_bootAUC, md_12_bootAUC, md_13_bootAUC, md_14_bootAUC, md_15_bootAUC)
names(WTSP_models_bootAUC_4) <- c("md_0_bootAUC", "md_1_bootAUC", "md_2_bootAUC", "md_3_bootAUC", "md_4_bootAUC", "md_5_bootAUC", "md_6_bootAUC", "md_7_bootAUC", "md_8_bootAUC", "md_9_bootAUC", "md_10_bootAUC", "md_11_bootAUC", "md_12_bootAUC", "md_13_bootAUC", "md_14_bootAUC", "md_15_bootAUC")
save(WTSP_models_bootAUC_4, file="2_pipeline/store/models/WTSP_models_bootAUC_4.rData")

##############################################################
## stack rasters
##############################################################

map_list<-list.files(path='3_output/maps/predictedDistributions/WTSP/predictMap', pattern = "*.tif$", full.names = TRUE)
WTSP_stack<-raster::stack(map_list)

#save stack as r object
stackSave(WTSP_stack,file="3_output/maps/predictedDistributions/WTSP/WTSP_stack.stk")

####################################################################################################################################
##YBSA##############################################################################################################################
####################################################################################################################################

#stats function for AUC

#dd<-d3%>%filter(SS_lidar_timelag==0)

dd<-d3%>%filter(SS_lidar_timelag==0)


# AUCFun <- function(x) {
#   library(pROC)
#   pred<-predict(x, type="response", allow.new.levels=TRUE)
#   AUC<-as.numeric(auc(d_0$YBSA_OCC, pred))
# }


#both AUC and R2
# AUCFun <- function(fit) {
#   library(pROC)
#   pred<-predict(fit, type="response", allow.new.levels=TRUE)
#   R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$YBSA_OCC, pred))),3)
#   }
#   
AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$YBSA_OCC, pred))),3)
}


#models
md_0<-lme4::glmer(YBSA_OCC ~ poly(ndvi_lag_0,2) +  elev_4pnt00_to_6pnt00_return_proportion+  elev_maximum*elev_stddev+(1|SS) , data=dd, family = binomial, offset = YBSA_OFF,  na.action = na.fail, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)))


md_0_bootAUC<-bootMer(md_0, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)


# meanAUC<-mean(md_0_bootAUC$t)
# std.err<-sd(md_0_bootAUC$t)/sqrt(length(md_0_bootAUC$t))
# CI.lower <- meanAUC - std.err*1.96
# CI.upper <- meanAUC + std.err*1.96
# 
# md_0_meanAUC<-data.frame(meanAUC, std.err, CI.lower, CI.upper)

R2m_mean<-mean(md_0_bootAUC$t[,1])
R2m_stdErr<-sd(md_0_bootAUC$t[,1])/sqrt(length(md_0_bootAUC$t[,1]))
R2c_mean<-mean(md_0_bootAUC$t[,2])
R2c_stdErr<-sd(md_0_bootAUC$t[,2])/sqrt(length(md_0_bootAUC$t[,2]))
AUC_mean<-mean(md_0_bootAUC$t[,3])
AUC_stdErr<-sd(md_0_bootAUC$t[,3])/sqrt(length(md_0_bootAUC$t[,3]))

R2m_sd<-sd(md_0_bootAUC$t[,1])
R2c_sd<-sd(md_0_bootAUC$t[,2])
AUC_sd<-sd(md_0_bootAUC$t[,3])

md_0_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)


YBSA_predictMap_m_0<-raster::predict(scaled_stack, md_0, re.form=NA, type='response')
writeRaster(YBSA_predictMap_m_0,file="3_output/maps/predictedDistributions/YBSA/YBSA_predictMap_m_00", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-1)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$YBSA_OCC, pred))),3)
}

md_1<-lme4::glmer(YBSA_OCC ~ poly(ndvi_lag_1,2) +  elev_4pnt00_to_6pnt00_return_proportion+  elev_maximum*elev_stddev+(1|SS) , data=dd, family = binomial, offset = YBSA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_1_bootAUC<-bootMer(md_1, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_1_bootAUC$t[,1])
R2m_stdErr<-sd(md_1_bootAUC$t[,1])/sqrt(length(md_1_bootAUC$t[,1]))
R2m_sd<-sd(md_1_bootAUC$t[,1])
R2c_mean<-mean(md_1_bootAUC$t[,2])
R2c_stdErr<-sd(md_1_bootAUC$t[,2])/sqrt(length(md_1_bootAUC$t[,2]))
R2c_sd<-sd(md_1_bootAUC$t[,2])
AUC_mean<-mean(md_1_bootAUC$t[,3])
AUC_stdErr<-sd(md_1_bootAUC$t[,3])/sqrt(length(md_1_bootAUC$t[,3]))
AUC_sd<-sd(md_1_bootAUC$t[,3])

R2m_sd<-sd(md_1_bootAUC$t[,1])
R2c_sd<-sd(md_1_bootAUC$t[,2])
AUC_sd<-sd(md_1_bootAUC$t[,3])

md_1_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

YBSA_predictMap_m_01<-raster::predict(scaled_stack, md_1, re.form=NA, type='response')
writeRaster(YBSA_predictMap_m_01,file="3_output/maps/predictedDistributions/YBSA/YBSA_predictMap_m_01", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-2)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$YBSA_OCC, pred))),3)
}

md_2<-lme4::glmer(YBSA_OCC ~ poly(ndvi_lag_2,2) +  elev_4pnt00_to_6pnt00_return_proportion+  elev_maximum*elev_stddev+(1|SS) , data=dd, family = binomial, offset = YBSA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_2_bootAUC<-bootMer(md_2, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_2_bootAUC$t[,1])
R2m_stdErr<-sd(md_2_bootAUC$t[,1])/sqrt(length(md_2_bootAUC$t[,1]))
R2c_mean<-mean(md_2_bootAUC$t[,2])
R2c_stdErr<-sd(md_2_bootAUC$t[,2])/sqrt(length(md_2_bootAUC$t[,2]))
AUC_mean<-mean(md_2_bootAUC$t[,3])
AUC_stdErr<-sd(md_2_bootAUC$t[,3])/sqrt(length(md_2_bootAUC$t[,3]))

R2m_sd<-sd(md_2_bootAUC$t[,1])
R2c_sd<-sd(md_2_bootAUC$t[,2])
AUC_sd<-sd(md_2_bootAUC$t[,3])

md_2_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

YBSA_predictMap_m_02<-raster::predict(scaled_stack, md_2, re.form=NA, type='response')
writeRaster(YBSA_predictMap_m_02,file="3_output/maps/predictedDistributions/YBSA/YBSA_predictMap_m_02", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-3)


AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$YBSA_OCC, pred))),3)
}

md_3<-lme4::glmer(YBSA_OCC ~ poly(ndvi_lag_3,2) +  elev_4pnt00_to_6pnt00_return_proportion+  elev_maximum*elev_stddev+(1|SS) , data=dd, family = binomial, offset = YBSA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_3_bootAUC<-bootMer(md_3, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_3_bootAUC$t[,1])
R2m_stdErr<-sd(md_3_bootAUC$t[,1])/sqrt(length(md_3_bootAUC$t[,1]))
R2c_mean<-mean(md_3_bootAUC$t[,2])
R2c_stdErr<-sd(md_3_bootAUC$t[,2])/sqrt(length(md_3_bootAUC$t[,2]))
AUC_mean<-mean(md_3_bootAUC$t[,3])
AUC_stdErr<-sd(md_3_bootAUC$t[,3])/sqrt(length(md_3_bootAUC$t[,3]))

R2m_sd<-sd(md_3_bootAUC$t[,1])
R2c_sd<-sd(md_3_bootAUC$t[,2])
AUC_sd<-sd(md_3_bootAUC$t[,3])

md_3_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

YBSA_predictMap_m_03<-raster::predict(scaled_stack, md_3, re.form=NA, type='response')
writeRaster(YBSA_predictMap_m_03,file="3_output/maps/predictedDistributions/YBSA/YBSA_predictMap_m_03", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-4)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$YBSA_OCC, pred))),3)
}

md_4<-lme4::glmer(YBSA_OCC ~ poly(ndvi_lag_4,2) +  elev_4pnt00_to_6pnt00_return_proportion+  elev_maximum*elev_stddev+(1|SS) , data=dd, family = binomial, offset = YBSA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_4_bootAUC<-bootMer(md_4, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_4_bootAUC$t[,1])
R2m_stdErr<-sd(md_4_bootAUC$t[,1])/sqrt(length(md_4_bootAUC$t[,1]))
R2c_mean<-mean(md_4_bootAUC$t[,2])
R2c_stdErr<-sd(md_4_bootAUC$t[,2])/sqrt(length(md_4_bootAUC$t[,2]))
AUC_mean<-mean(md_4_bootAUC$t[,3])
AUC_stdErr<-sd(md_4_bootAUC$t[,3])/sqrt(length(md_4_bootAUC$t[,3]))

R2m_sd<-sd(md_4_bootAUC$t[,1])
R2c_sd<-sd(md_4_bootAUC$t[,2])
AUC_sd<-sd(md_4_bootAUC$t[,3])

md_4_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

YBSA_predictMap_m_04<-raster::predict(scaled_stack, md_4, re.form=NA, type='response')
writeRaster(YBSA_predictMap_m_04,file="3_output/maps/predictedDistributions/YBSA/YBSA_predictMap_m_04", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-5)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$YBSA_OCC, pred))),3)
}

md_5<-lme4::glmer(YBSA_OCC ~ poly(ndvi_lag_5,2) +  elev_4pnt00_to_6pnt00_return_proportion+  elev_maximum*elev_stddev+(1|SS) , data=dd, family = binomial, offset = YBSA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_5_bootAUC<-bootMer(md_5, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_5_bootAUC$t[,1])
R2m_stdErr<-sd(md_5_bootAUC$t[,1])/sqrt(length(md_5_bootAUC$t[,1]))
R2c_mean<-mean(md_5_bootAUC$t[,2])
R2c_stdErr<-sd(md_5_bootAUC$t[,2])/sqrt(length(md_5_bootAUC$t[,2]))
AUC_mean<-mean(md_5_bootAUC$t[,3])
AUC_stdErr<-sd(md_5_bootAUC$t[,3])/sqrt(length(md_5_bootAUC$t[,3]))

R2m_sd<-sd(md_5_bootAUC$t[,1])
R2c_sd<-sd(md_5_bootAUC$t[,2])
AUC_sd<-sd(md_5_bootAUC$t[,3])

md_5_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

YBSA_predictMap_m_05<-raster::predict(scaled_stack, md_5, re.form=NA, type='response')
writeRaster(YBSA_predictMap_m_05,file="3_output/maps/predictedDistributions/YBSA/YBSA_predictMap_m_05", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-6)


AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$YBSA_OCC, pred))),3)
}

md_6<-lme4::glmer(YBSA_OCC ~ poly(ndvi_lag_6,2) +  elev_4pnt00_to_6pnt00_return_proportion+  elev_maximum*elev_stddev+(1|SS) , data=dd, family = binomial, offset = YBSA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_6_bootAUC<-bootMer(md_6, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_6_bootAUC$t[,1])
R2m_stdErr<-sd(md_6_bootAUC$t[,1])/sqrt(length(md_6_bootAUC$t[,1]))
R2c_mean<-mean(md_6_bootAUC$t[,2])
R2c_stdErr<-sd(md_6_bootAUC$t[,2])/sqrt(length(md_6_bootAUC$t[,2]))
AUC_mean<-mean(md_6_bootAUC$t[,3])
AUC_stdErr<-sd(md_6_bootAUC$t[,3])/sqrt(length(md_6_bootAUC$t[,3]))

R2m_sd<-sd(md_6_bootAUC$t[,1])
R2c_sd<-sd(md_6_bootAUC$t[,2])
AUC_sd<-sd(md_6_bootAUC$t[,3])

md_6_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)


YBSA_predictMap_m_06<-raster::predict(scaled_stack, md_6, re.form=NA, type='response')
writeRaster(YBSA_predictMap_m_06,file="3_output/maps/predictedDistributions/YBSA/YBSA_predictMap_m_06", format="GTiff", overwrite=T)

##########################
dd<-d3%>%filter(SS_lidar_timelag==-7)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$YBSA_OCC, pred))),3)
}

md_7<-lme4::glmer(YBSA_OCC ~ poly(ndvi_lag_7,2) +  elev_4pnt00_to_6pnt00_return_proportion+  elev_maximum*elev_stddev+(1|SS) , data=dd, family = binomial, offset = YBSA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_7_bootAUC<-bootMer(md_7, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_7_bootAUC$t[,1])
R2m_stdErr<-sd(md_7_bootAUC$t[,1])/sqrt(length(md_7_bootAUC$t[,1]))
R2c_mean<-mean(md_7_bootAUC$t[,2])
R2c_stdErr<-sd(md_7_bootAUC$t[,2])/sqrt(length(md_7_bootAUC$t[,2]))
AUC_mean<-mean(md_7_bootAUC$t[,3])
AUC_stdErr<-sd(md_7_bootAUC$t[,3])/sqrt(length(md_7_bootAUC$t[,3]))

R2m_sd<-sd(md_7_bootAUC$t[,1])
R2c_sd<-sd(md_7_bootAUC$t[,2])
AUC_sd<-sd(md_7_bootAUC$t[,3])

md_7_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

YBSA_predictMap_m_07<-raster::predict(scaled_stack, md_7, re.form=NA, type='response')
writeRaster(YBSA_predictMap_m_07,file="3_output/maps/predictedDistributions/YBSA/YBSA_predictMap_m_07", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-8)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$YBSA_OCC, pred))),3)
}

md_8<-lme4::glmer(YBSA_OCC ~ poly(ndvi_lag_8,2) +  elev_4pnt00_to_6pnt00_return_proportion+  elev_maximum*elev_stddev+(1|SS) , data=dd, family = binomial, offset = YBSA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_8_bootAUC<-bootMer(md_8, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_8_bootAUC$t[,1])
R2m_stdErr<-sd(md_8_bootAUC$t[,1])/sqrt(length(md_8_bootAUC$t[,1]))
R2c_mean<-mean(md_8_bootAUC$t[,2])
R2c_stdErr<-sd(md_8_bootAUC$t[,2])/sqrt(length(md_8_bootAUC$t[,2]))
AUC_mean<-mean(md_8_bootAUC$t[,3])
AUC_stdErr<-sd(md_8_bootAUC$t[,3])/sqrt(length(md_8_bootAUC$t[,3]))

R2m_sd<-sd(md_8_bootAUC$t[,1])
R2c_sd<-sd(md_8_bootAUC$t[,2])
AUC_sd<-sd(md_8_bootAUC$t[,3])

md_8_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

YBSA_predictMap_m_08<-raster::predict(scaled_stack, md_8, re.form=NA, type='response')
writeRaster(YBSA_predictMap_m_08,file="3_output/maps/predictedDistributions/YBSA/YBSA_predictMap_m_08", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-9)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$YBSA_OCC, pred))),3)
}

md_9<-lme4::glmer(YBSA_OCC ~ poly(ndvi_lag_9,2) +  elev_4pnt00_to_6pnt00_return_proportion+  elev_maximum*elev_stddev+(1|SS) , data=dd, family = binomial, offset = YBSA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_9_bootAUC<-bootMer(md_9, nsim = 500, seed = 101, FUN=AUCFun,
                      type = "parametric",
                      verbose = TRUE, .progress = "none",
                      use.u = FALSE)

R2m_mean<-mean(md_9_bootAUC$t[,1])
R2m_stdErr<-sd(md_9_bootAUC$t[,1])/sqrt(length(md_9_bootAUC$t[,1]))
R2c_mean<-mean(md_9_bootAUC$t[,2])
R2c_stdErr<-sd(md_9_bootAUC$t[,2])/sqrt(length(md_9_bootAUC$t[,2]))
AUC_mean<-mean(md_9_bootAUC$t[,3])
AUC_stdErr<-sd(md_9_bootAUC$t[,3])/sqrt(length(md_9_bootAUC$t[,3]))

R2m_sd<-sd(md_9_bootAUC$t[,1])
R2c_sd<-sd(md_9_bootAUC$t[,2])
AUC_sd<-sd(md_9_bootAUC$t[,3])

md_9_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)


YBSA_predictMap_m_09<-raster::predict(scaled_stack, md_9, re.form=NA, type='response')
writeRaster(YBSA_predictMap_m_09,file="3_output/maps/predictedDistributions/YBSA/YBSA_predictMap_m_09", format="GTiff", overwrite=T)

##########################
dd<-d3%>%filter(SS_lidar_timelag==-10)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$YBSA_OCC, pred))),3)
}

md_10<-lme4::glmer(YBSA_OCC ~ poly(ndvi_lag_10,2) +  elev_4pnt00_to_6pnt00_return_proportion+  elev_maximum*elev_stddev+(1|SS) , data=dd, family = binomial, offset = YBSA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_10_bootAUC<-bootMer(md_10, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_10_bootAUC$t[,1])
R2m_stdErr<-sd(md_10_bootAUC$t[,1])/sqrt(length(md_10_bootAUC$t[,1]))
R2c_mean<-mean(md_10_bootAUC$t[,2])
R2c_stdErr<-sd(md_10_bootAUC$t[,2])/sqrt(length(md_10_bootAUC$t[,2]))
AUC_mean<-mean(md_10_bootAUC$t[,3])
AUC_stdErr<-sd(md_10_bootAUC$t[,3])/sqrt(length(md_10_bootAUC$t[,3]))

R2m_sd<-sd(md_10_bootAUC$t[,1])
R2c_sd<-sd(md_10_bootAUC$t[,2])
AUC_sd<-sd(md_10_bootAUC$t[,3])

md_10_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

YBSA_predictMap_m_10<-raster::predict(scaled_stack, md_10, re.form=NA, type='response')
writeRaster(YBSA_predictMap_m_10,file="3_output/maps/predictedDistributions/YBSA/YBSA_predictMap_m_10", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-11)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$YBSA_OCC, pred))),3)
}

md_11<-lme4::glmer(YBSA_OCC ~ poly(ndvi_lag_11,2) +  elev_4pnt00_to_6pnt00_return_proportion+  elev_maximum*elev_stddev+(1|SS) , data=dd, family = binomial, offset = YBSA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_11_bootAUC<-bootMer(md_11, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_11_bootAUC$t[,1])
R2m_stdErr<-sd(md_11_bootAUC$t[,1])/sqrt(length(md_11_bootAUC$t[,1]))
R2c_mean<-mean(md_11_bootAUC$t[,2])
R2c_stdErr<-sd(md_11_bootAUC$t[,2])/sqrt(length(md_11_bootAUC$t[,2]))
AUC_mean<-mean(md_11_bootAUC$t[,3])
AUC_stdErr<-sd(md_11_bootAUC$t[,3])/sqrt(length(md_11_bootAUC$t[,3]))

R2m_sd<-sd(md_11_bootAUC$t[,1])
R2c_sd<-sd(md_11_bootAUC$t[,2])
AUC_sd<-sd(md_11_bootAUC$t[,3])

md_11_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

YBSA_predictMap_m_11<-raster::predict(scaled_stack, md_11, re.form=NA, type='response')
writeRaster(YBSA_predictMap_m_11,file="3_output/maps/predictedDistributions/YBSA/YBSA_predictMap_m_11", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-12)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$YBSA_OCC, pred))),3)
}

md_12<-lme4::glmer(YBSA_OCC ~ poly(ndvi_lag_12,2) +  elev_4pnt00_to_6pnt00_return_proportion+  elev_maximum*elev_stddev+(1|SS) , data=dd, family = binomial, offset = YBSA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_12_bootAUC<-bootMer(md_12, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_12_bootAUC$t[,1])
R2m_stdErr<-sd(md_12_bootAUC$t[,1])/sqrt(length(md_12_bootAUC$t[,1]))
R2c_mean<-mean(md_12_bootAUC$t[,2])
R2c_stdErr<-sd(md_12_bootAUC$t[,2])/sqrt(length(md_12_bootAUC$t[,2]))
AUC_mean<-mean(md_12_bootAUC$t[,3])
AUC_stdErr<-sd(md_12_bootAUC$t[,3])/sqrt(length(md_12_bootAUC$t[,3]))

R2m_sd<-sd(md_12_bootAUC$t[,1])
R2c_sd<-sd(md_12_bootAUC$t[,2])
AUC_sd<-sd(md_12_bootAUC$t[,3])

md_12_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

YBSA_predictMap_m_12<-raster::predict(scaled_stack, md_12, re.form=NA, type='response')
writeRaster(YBSA_predictMap_m_12,file="3_output/maps/predictedDistributions/YBSA/YBSA_predictMap_m_12", format="GTiff", overwrite=T)

##########################
dd<-d3%>%filter(SS_lidar_timelag==-13)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$YBSA_OCC, pred))),3)
}

md_13<-lme4::glmer(YBSA_OCC ~ poly(ndvi_lag_13,2) +  elev_4pnt00_to_6pnt00_return_proportion+  elev_maximum*elev_stddev+(1|SS) , data=dd, family = binomial, offset = YBSA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_13_bootAUC<-bootMer(md_13, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_13_bootAUC$t[,1])
R2m_stdErr<-sd(md_13_bootAUC$t[,1])/sqrt(length(md_13_bootAUC$t[,1]))
R2c_mean<-mean(md_13_bootAUC$t[,2])
R2c_stdErr<-sd(md_13_bootAUC$t[,2])/sqrt(length(md_13_bootAUC$t[,2]))
AUC_mean<-mean(md_13_bootAUC$t[,3])
AUC_stdErr<-sd(md_13_bootAUC$t[,3])/sqrt(length(md_13_bootAUC$t[,3]))

R2m_sd<-sd(md_13_bootAUC$t[,1])
R2c_sd<-sd(md_13_bootAUC$t[,2])
AUC_sd<-sd(md_13_bootAUC$t[,3])

md_13_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

YBSA_predictMap_m_13<-raster::predict(scaled_stack, md_13, re.form=NA, type='response')
writeRaster(YBSA_predictMap_m_13,file="3_output/maps/predictedDistributions/YBSA/YBSA_predictMap_m_13", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-14)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$YBSA_OCC, pred))),3)
}

md_14<-lme4::glmer(YBSA_OCC ~ poly(ndvi_lag_14,2) +  elev_4pnt00_to_6pnt00_return_proportion+  elev_maximum*elev_stddev+(1|SS) , data=dd, family = binomial, offset = YBSA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_14_bootAUC<-bootMer(md_14, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_14_bootAUC$t[,1])
R2m_stdErr<-sd(md_14_bootAUC$t[,1])/sqrt(length(md_14_bootAUC$t[,1]))
R2c_mean<-mean(md_14_bootAUC$t[,2])
R2c_stdErr<-sd(md_14_bootAUC$t[,2])/sqrt(length(md_14_bootAUC$t[,2]))
AUC_mean<-mean(md_14_bootAUC$t[,3])
AUC_stdErr<-sd(md_14_bootAUC$t[,3])/sqrt(length(md_14_bootAUC$t[,3]))

R2m_sd<-sd(md_14_bootAUC$t[,1])
R2c_sd<-sd(md_14_bootAUC$t[,2])
AUC_sd<-sd(md_14_bootAUC$t[,3])

md_14_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)

YBSA_predictMap_m_14<-raster::predict(scaled_stack, md_14, re.form=NA, type='response')
writeRaster(YBSA_predictMap_m_14,file="3_output/maps/predictedDistributions/YBSA/YBSA_predictMap_m_14", format="GTiff", overwrite=T)
##########################
dd<-d3%>%filter(SS_lidar_timelag==-15)
set.seed(1)

AUCFun <- function(fit) {
  library(pROC)
  pred<-predict(fit, type="response", allow.new.levels=TRUE)
  R2<-round(c(r.squaredGLMM(fit)[1,c(1,2)], AUC=as.numeric(auc(dd$YBSA_OCC, pred))),3)
}

md_15<-lme4::glmer(YBSA_OCC ~ poly(ndvi_lag_15,2) +  elev_4pnt00_to_6pnt00_return_proportion+  elev_maximum*elev_stddev+(1|SS) , data=dd, family = binomial, offset = YBSA_OFF,  na.action = na.exclude, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e9)) )

md_15_bootAUC<-bootMer(md_15, nsim = 500, seed = 101, FUN=AUCFun,
                       type = "parametric",
                       verbose = TRUE, .progress = "none",
                       use.u = FALSE)

R2m_mean<-mean(md_15_bootAUC$t[,1])
R2m_stdErr<-sd(md_15_bootAUC$t[,1])/sqrt(length(md_15_bootAUC$t[,1]))
R2c_mean<-mean(md_15_bootAUC$t[,2])
R2c_stdErr<-sd(md_15_bootAUC$t[,2])/sqrt(length(md_15_bootAUC$t[,2]))
AUC_mean<-mean(md_15_bootAUC$t[,3])
AUC_stdErr<-sd(md_15_bootAUC$t[,3])/sqrt(length(md_15_bootAUC$t[,3]))
R2m_range<-range(md_15_bootAUC$t[,1])


R2m_sd<-sd(md_15_bootAUC$t[,1])
R2c_sd<-sd(md_15_bootAUC$t[,2])
AUC_sd<-sd(md_15_bootAUC$t[,3])

md_15_meanAUC<-data.frame(R2m_mean, R2m_stdErr, R2m_sd, R2c_mean, R2c_stdErr,R2c_sd, AUC_mean, AUC_stdErr, AUC_sd)



YBSA_predictMap_m_15<-raster::predict(scaled_stack, md_15, re.form=NA, type='response')
writeRaster(YBSA_predictMap_m_15,file="3_output/maps/predictedDistributions/YBSA/YBSA_predictMap_m_15", format="GTiff", overwrite=T)


####################################################################
# save models
YBSA_models_4<-list(md_0, md_1, md_2, md_3, md_4, md_5, md_6, md_7, md_8, md_9, md_10, md_11, md_12, md_13, md_14, md_15)
names(YBSA_models_4) <- c("md_0", "md_1", "md_2", "md_3", "md_4", "md_5", "md_6", "md_7", "md_8", "md_9", "md_10", "md_11", "md_12", "md_13", "md_14", "md_15")
save(YBSA_models_4, file="2_pipeline/store/models/YBSA_models_4.rData")

####################################################################
# create an AUC dataframe
lag<-c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)
model<-c("md_0", "md_1", "md_2", "md_3", "md_4", "md_5", "md_6", "md_7", "md_8", "md_9", "md_10", "md_11", "md_12", "md_13", "md_14", "md_15")

YBSA_models_AUC_4<-as.data.frame(rbind(md_0_meanAUC, md_1_meanAUC, md_2_meanAUC, md_3_meanAUC, md_4_meanAUC, md_5_meanAUC, md_6_meanAUC, md_7_meanAUC, md_8_meanAUC, md_9_meanAUC, md_10_meanAUC, md_11_meanAUC, md_12_meanAUC, md_13_meanAUC, md_14_meanAUC, md_15_meanAUC))%>%
  cbind(lag)

# YBSA_AUC_lm_test<-lm(AUC_mean~ lag, data=YBSA_models_AUC_4)
# summary(YBSA_AUC_lm_test)
# 
# YBSA_r2c_lm_test<-lm(R2c_mean~ lag, data=YBSA_models_AUC_4)
# summary(YBSA_r2c_lm_test)
# 
# YBSA_r2m_lm_test<-lm(R2m_mean~ lag, data=YBSA_models_AUC_4)
# summary(YBSA_r2m_lm_test)
# 
# ggplot(YBSA_models_AUC_4, aes(x=lag, y=AUC_mean)) + 
#     geom_errorbar(aes(ymin=AUC_mean-1.96*AUC_stdErr, ymax=AUC_mean+ 1.96*AUC_stdErr, width=.2))+
#     geom_point(size=1) + 
#     geom_segment(x=0, xend=15, y=.7, yend=.7, col="red", linetype="longdash")+
#     geom_smooth(method = lm)+
#     theme(panel.background = element_blank(),
#           text = element_text(size=10),
#           axis.ticks=element_blank(),
#           #strip.background =element_rect(fill="white"),
#           panel.border = element_rect(color = "black", fill = NA, size = .2),
#           axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
#           axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)),
#           #panel.background = element_rect(fill="white"),
#           panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),
#           legend.position="none")



save(YBSA_models_AUC_4, file="2_pipeline/store/models/YBSA_models_AUC_4.rData")

##############################################################
## save bootmer results
##############################################################
YBSA_models_bootAUC_4<-list(md_0_bootAUC, md_1_bootAUC, md_2_bootAUC, md_3_bootAUC, md_4_bootAUC, md_5_bootAUC, md_6_bootAUC, md_7_bootAUC, md_8_bootAUC, md_9_bootAUC, md_10_bootAUC, md_11_bootAUC, md_12_bootAUC, md_13_bootAUC, md_14_bootAUC, md_15_bootAUC)
names(YBSA_models_bootAUC_4) <- c("md_0_bootAUC", "md_1_bootAUC", "md_2_bootAUC", "md_3_bootAUC", "md_4_bootAUC", "md_5_bootAUC", "md_6_bootAUC", "md_7_bootAUC", "md_8_bootAUC", "md_9_bootAUC", "md_10_bootAUC", "md_11_bootAUC", "md_12_bootAUC", "md_13_bootAUC", "md_14_bootAUC", "md_15_bootAUC")
save(YBSA_models_bootAUC_4, file="2_pipeline/store/models/YBSA_models_bootAUC_4.rData")

##############################################################
## stack rasters
##############################################################

map_list<-list.files(path='3_output/maps/predictedDistributions/YBSA/predictMap', pattern = "*.tif$", full.names = TRUE)
YBSA_stack<-raster::stack(map_list)

#save stack as r object
stackSave(YBSA_stack,file="3_output/maps/predictedDistributions/YBSA/YBSA_stack.stk")
