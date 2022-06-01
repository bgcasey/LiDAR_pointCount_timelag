library(boot)

d<-sdm_d


d_0<-sdm_d%>%filter(SS_lidar_timelag==0)

md_0<-glm(MOWA ~  canopy_relief_ratio +  elev_maximum + elev_stddev + total_all_returns, data=d_0, family = binomial, offset = MOWA_OFF,  na.action=na.fail)


test<-cv.glm(d_0, md_0)
test$delta
