library(ggplot2)
library(dplyr)
library(plyr)
library(base)
library(tidyr)


RETN<-read.csv("0_data/external/RETN_sampling.csv")

countRETN<-plyr::count(RETN, c('YearsAgo_Range', 'dist_ext_upper_1'))

countRETN<-countRETN[-c(1:5, 14), ]


countRETN$dist_ext_upper_1 <- as.factor(countRETN$dist_ext_upper_1)
#create a column that calculates the percentage of sites
countRETN <- countRETN %>% 
  mutate(percentSites = freq / nrow(RETN)*100)

countRETN_sub<-countRETN[countRETN$dist_ext_upper_1!='95-100',]

cbp2<-c("#a2d9ce","#73c6b6","#45b39d","#16a085","#138d75","#117a65","#0e6655","#0b5345")

RETN_plot_1<-ggplot(na.omit(countRETN_sub), aes(fill=YearsAgo_Range, y=freq, x=dist_ext_upper_1)) +
geom_bar(position="dodge", stat="identity")+
  xlab("harvest severity (%)")+
  ylab("number of sites")+ 
  labs(fill = "years postharvest")+
  scale_fill_manual(values = cbp2)+
  geom_bar(position="stack", stat="identity")
RETN_plot_1
ggsave("3_output/figures/RETN_samplePlot_1.png")



countRETN_sub_2<-countRETN[countRETN$dist_ext_upper_1=='95-100',]

RETN_plot_2<-ggplot(na.omit(countRETN_sub_2), aes(fill=YearsAgo_Range, y=freq, x=dist_ext_upper_1)) +
  geom_bar(position="dodge", stat="identity", width = 0.2)+
  xlab("harvest severity (%)")+
  ylab("number of sites")+ 
  labs(fill = "years postharvest")+
  scale_fill_manual(values = cbp2)+
  geom_bar(position="stack", stat="identity", width = 0.2)
RETN_plot_2
ggsave("3_output/figures/RETN_samplePlot_3.png")


#scale_fill_brewer(palette = "YlGnBu")+

