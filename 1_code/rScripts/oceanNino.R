load("2_pipeline/store/models/BTNW_models_AUC_4.rData")
ONI<-read.csv("0_data/external/OceanicNinoIndex.csv")

load("2_pipeline/tmp/sdm_d.rData")
d<-sdm_d%>%
  dplyr::select(ss_year, SS_lidar_timelag)%>%
distinct()

Year<-c(2009,2008,2007,2006,2005,2004,2003,2002,2001, 2000, 1999, 1998, 1997, 1996, 1995, 1994)

t<-cbind(b4a,Year)

test<-left_join(t,ONI)

lm1<-lm(AUC_mean~ lag, data=test)
lm2<-lm(AUC_mean~ lag + JFM, data=test)
lm3<-lm(AUC_mean~ poly(lag,2), data=test)

nsi


b4a<-BTNW_models_AUC_4
BTNW_models_AUC_4<-b4a

ggplot(BTNW_models_AUC_4, aes(x=lag, y=AUC_mean)) + 
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