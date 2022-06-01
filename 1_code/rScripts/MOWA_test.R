d<-d%>%
  filter(ss_year==2009)

# add ONI data
cas_dst<-read.csv("../../data/spatial/external/CASFRI/Alberta/alberta_dst.csv")


d_0<-sdm_d%>%filter(SS_lidar_timelag==0)
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(d_0), replace=TRUE, prob=c(0.7,0.3))
train <- d_0[sample, ]
test <- d_0[!sample, ] 

md_0<-lme4::glmer(MOWA ~  canopy_relief_ratio +  elev_maximum  + total_all_returns +  (1|SS), data=train, family = binomial, offset = MOWA_OFF,  na.action=na.fail) 
predicted <- predict(md_0, test, type="response")
auc_d_0<-auc(test$MOWA, predicted)

############################################################
d_1<-sdm_d%>%filter(SS_lidar_timelag==-1)
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(d_1), replace=TRUE, prob=c(0.7,0.3))
train <- d_1[sample, ]
test <- d_1[!sample, ] 

md_1<-glmer(MOWA ~   canopy_relief_ratio +  elev_maximum + elev_stddev + total_all_returns, +  (1|SS), data=train, family = binomial, offset = MOWA_OFF,  na.action=na.fail)
predicted <- predict(md_1, test, type="response")
auc_d_1<-auc(test$MOWA, predicted)

############################################################
d_2<-sdm_d%>%filter(SS_lidar_timelag==-2)
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(d_2), replace=TRUE, prob=c(0.7,0.3))
train <- d_2[sample, ]
test <- d_2[!sample, ] 

md_2<-glmer(MOWA ~   canopy_relief_ratio +  elev_maximum + elev_stddev + total_all_returns, +  (1|SS), data=train, family = binomial, offset = MOWA_OFF,  na.action=na.fail)
predicted <- predict(md_2, test, type="response")
auc_d_2<-auc(test$MOWA, predicted)

############################################################
d_3<-sdm_d%>%filter(SS_lidar_timelag==-3)
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(d_3), replace=TRUE, prob=c(0.7,0.3))
train <- d_3[sample, ]
test <- d_3[!sample, ] 

md_3<-glmer(MOWA ~   canopy_relief_ratio +  elev_maximum + elev_stddev + total_all_returns, +  (1|SS), data=train, family = binomial, offset = MOWA_OFF,  na.action=na.fail)
predicted <- predict(md_3, test, type="response")
auc_d_3<-auc(test$MOWA, predicted)


############################################################
d_4<-sdm_d%>%filter(SS_lidar_timelag==-4)
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(d_4), replace=TRUE, prob=c(0.7,0.3))
train <- d_4[sample, ]
test <- d_4[!sample, ] 

md_4<-glmer(MOWA ~   canopy_relief_ratio +  elev_maximum + elev_stddev + total_all_returns, +  (1|SS), data=train, family = binomial, offset = MOWA_OFF,  na.action=na.fail)
predicted <- predict(md_4, test, type="response")
auc_d_4<-auc(test$MOWA, predicted)


############################################################
d_5<-sdm_d%>%filter(SS_lidar_timelag==-5)
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(d_5), replace=TRUE, prob=c(0.7,0.3))
train <- d_5[sample, ]
test <- d_5[!sample, ] 

md_5<-glmer(MOWA ~   canopy_relief_ratio +  elev_maximum + elev_stddev + total_all_returns, +  (1|SS), data=train, family = binomial, offset = MOWA_OFF,  na.action=na.fail)
predicted <- predict(md_5, test, type="response")
auc_d_5<-auc(test$MOWA, predicted)

############################################################
d_6<-sdm_d%>%filter(SS_lidar_timelag==-6)
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(d_6), replace=TRUE, prob=c(0.7,0.3))
train <- d_6[sample, ]
test <- d_6[!sample, ] 

md_6<-glmer(MOWA ~   canopy_relief_ratio +  elev_maximum + elev_stddev + total_all_returns, +  (1|SS), data=train, family = binomial, offset = MOWA_OFF,  na.action=na.fail)
predicted <- predict(md_6, test, type="response")
auc_d_6<-auc(test$MOWA, predicted)


############################################################
d_7<-sdm_d%>%filter(SS_lidar_timelag==-7)
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(d_7), replace=TRUE, prob=c(0.7,0.3))
train <- d_7[sample, ]
test <- d_7[!sample, ] 

md_7<-glmer(MOWA ~   canopy_relief_ratio +  elev_maximum + elev_stddev + total_all_returns, +  (1|SS), data=train, family = binomial, offset = MOWA_OFF,  na.action=na.fail)
predicted <- predict(md_7, test, type="response")
auc_d_7<-auc(test$MOWA, predicted)

############################################################
d_8<-sdm_d%>%filter(SS_lidar_timelag==-8)
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(d_8), replace=TRUE, prob=c(0.7,0.3))
train <- d_8[sample, ]
test <- d_8[!sample, ] 

md_8<-glmer(MOWA ~   canopy_relief_ratio +  elev_maximum + elev_stddev + total_all_returns, +  (1|SS), data=train, family = binomial, offset = MOWA_OFF,  na.action=na.fail)
predicted <- predict(md_8, test, type="response")
auc_d_8<-auc(test$MOWA, predicted)

############################################################
d_9<-sdm_d%>%filter(SS_lidar_timelag==-9)
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(d_9), replace=TRUE, prob=c(0.7,0.3))
train <- d_9[sample, ]
test <- d_9[!sample, ] 

md_9<-glmer(MOWA ~   canopy_relief_ratio +  elev_maximum + elev_stddev + total_all_returns, +  (1|SS), data=train, family = binomial, offset = MOWA_OFF,  na.action=na.fail)
predicted <- predict(md_9, test, type="response")
auc_d_9<-auc(test$MOWA, predicted)

############################################################
d_10<-sdm_d%>%filter(SS_lidar_timelag==-10)
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(d_10), replace=TRUE, prob=c(0.7,0.3))
train <- d_10[sample, ]
test <- d_10[!sample, ] 

md_10<-glmer(MOWA ~   canopy_relief_ratio +  elev_maximum + elev_stddev + total_all_returns, +  (1|SS), data=train, family = binomial, offset = MOWA_OFF,  na.action=na.fail)
predicted <- predict(md_10, test, type="response")
auc_d_10<-auc(test$MOWA, predicted)

############################################################
d_11<-sdm_d%>%filter(SS_lidar_timelag==-11)
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(d_11), replace=TRUE, prob=c(0.7,0.3))
train <- d_11[sample, ]
test <- d_11[!sample, ] 

md_11<-glmer(MOWA ~   canopy_relief_ratio +  elev_maximum + elev_stddev + total_all_returns, +  (1|SS), data=train, family = binomial, offset = MOWA_OFF,  na.action=na.fail)
predicted <- predict(md_11, test, type="response")
auc_d_11<-auc(test$MOWA, predicted)

############################################################
d_12<-sdm_d%>%filter(SS_lidar_timelag==-12)
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(d_12), replace=TRUE, prob=c(0.7,0.3))
train <- d_12[sample, ]
test <- d_12[!sample, ] 

md_12<-glmer(MOWA ~   canopy_relief_ratio +  elev_maximum + elev_stddev + total_all_returns, +  (1|SS), data=train, family = binomial, offset = MOWA_OFF,  na.action=na.fail)
predicted <- predict(md_12, test, type="response")
auc_d_12<-auc(test$MOWA, predicted)

############################################################
d_13<-sdm_d%>%filter(SS_lidar_timelag==-13)
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(d_13), replace=TRUE, prob=c(0.7,0.3))
train <- d_13[sample, ]
test <- d_13[!sample, ] 

md_13<-glmer(MOWA ~   canopy_relief_ratio +  elev_maximum + elev_stddev + total_all_returns, +  (1|SS), data=train, family = binomial, offset = MOWA_OFF,  na.action=na.fail)
predicted <- predict(md_13, test, type="response")
auc_d_13<-auc(test$MOWA, predicted)

############################################################
d_14<-sdm_d%>%filter(SS_lidar_timelag==-14)
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(d_14), replace=TRUE, prob=c(0.7,0.3))
train <- d_14[sample, ]
test <- d_14[!sample, ] 

md_14<-glmer(MOWA ~   canopy_relief_ratio +  elev_maximum + elev_stddev + total_all_returns, +  (1|SS), data=train, family = binomial, offset = MOWA_OFF,  na.action=na.fail)
predicted <- predict(md_14, test, type="response")
auc_d_14<-auc(test$MOWA, predicted)

############################################################
d_15<-sdm_d%>%filter(SS_lidar_timelag==-15)
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(d_15), replace=TRUE, prob=c(0.7,0.3))
train <- d_15[sample, ]
test <- d_15[!sample, ] 

md_15<-glmer(MOWA ~   canopy_relief_ratio +  elev_maximum + elev_stddev + total_all_returns, +  (1|SS), data=train, family = binomial, offset = MOWA_OFF,  na.action=na.fail)
predicted <- predict(md_15, test, type="response")
auc_d_15<-auc(test$MOWA, predicted)



# create an AUC dataframe

#lag<-as.data.frame(c(1,3, 8,9,10,12)) #removed outlier years
lag<-as.data.frame(c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
colnames(lag)<-"SS_lidar_timelag"
MOWA_models_AUC<-as.data.frame(rbind(auc_d_0, auc_d_1, auc_d_2, auc_d_3, auc_d_4, auc_d_5, auc_d_6, auc_d_7, auc_d_8, auc_d_9, auc_d_10, auc_d_11, auc_d_12, auc_d_13, auc_d_14, auc_d_15))%>%
#MOWA_models_AUC<-as.data.frame(rbind(auc_d_1, auc_d_3,  auc_d_8, auc_d_9, auc_d_10,  auc_d_12))%>%
  cbind(lag)%>%
  dplyr::rename(AUC=V1)
#Create a model name column
MOWA_models_AUC <- cbind(rownames(MOWA_models_AUC), data.frame(MOWA_models_AUC, row.names=NULL))
MOWA_models_AUC$model<-str_sub(MOWA_models_AUC$`rownames(MOWA_models_AUC)`,1,nchar(MOWA_models_AUC$`rownames(MOWA_models_AUC)`) -4)
MOWA_models_AUC<-MOWA_models_AUC[c(4,3,2)]
save(MOWA_models_AUC, file="2_pipeline/store/models/MOWA_models_AUC.rData")



MOWA_models<-list(md_0, md_1, md_2, md_3, md_4, md_5, md_6, md_7, md_8, md_9, md_10, md_11, md_12, md_13, md_14, md_15)
names(MOWA_models) <- c("md_0", "md_1", "md_2", "md_3", "md_4", "md_5", "md_6", "md_7", "md_8", "md_9", "md_10", "md_11", "md_12", "md_13", "md_14", "md_15")
save(MOWA_models, file="2_pipeline/store/models/MOWA_models.rData")

MOWA_model_stats <- model.sel(MOWA_models, rank="AIC", extra = c(r2=function(x) round(r.squaredglmerM(x)[1,c(1,2)],3)))%>%
  add_rownames(var = "model")%>%
  left_join(MOWA_models_AUC)

save(MOWA_model_stat, file="2_pipeline/store/models/MOWA_model_stats.rData")


ggplot(MOWA_models_AUC, aes(x=SS_lidar_timelag, y=AUC)) + geom_point() + geom_smooth(method = lm)
AUC_test<-lm(AUC~ SS_lidar_timelag, data=MOWA_models_AUC)

ggplot(MOWA_model_stats, aes(x=SS_lidar_timelag, y=r2.R2m)) + geom_point() + geom_smooth(method = lm)
AUC_test<-lm(r2.R2m~ SS_lidar_timelag, data=MOWA_model_stats)

# extract multipe r2 from glmer

hoslem.test(md_0$y, md_0$fitted)
