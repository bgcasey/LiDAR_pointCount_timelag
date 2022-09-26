library(knitr)
library(kableExtra)
library(xlsx)
library(tinytex)
library(rrtable)
library(htmltools)
library(remotes)
remotes::install_github("gorkang/html2latex")
library("html2latex")
library(lme4)
library(huxtable)
library(ggplot2)
library(dplyr)
library(ggstance)

## ---- covDes
cov_description<-xlsx::read.xlsx("../tables/covariate_list.xlsx", "subset")
kable(cov_description, col.names = c("Metric",
                                     "Source",
                                     "Description"),
  align = c("l","l","r"), escape=F, caption = 'Spatial covariates included in the analysis.',"latex", booktabs=TRUE, linesep="")%>% 
  column_spec(column = 3, width = "30em") %>% 
  kable_styling(latex_options="scale_down", font_size = 9)
  # kable_styling(font_size = 6, position = "center", full_width = T)


# summ_top_2<-summ_top_2[c(1:2,4:6,8)]
# #summ_top_2<-summ_top_2[c(1:3)]
# knitr::kable(summ_top_2, 
#              col.names = c("response","fixed effects", "df", "R2m", "R2c", "source"),
#              align=c("l","l","l","l","r","r","r","r", "r","r"), escape=F,
#              caption = "Top models for each response variable",
#              "latex", linesep="", longtable=T, booktabs=T)%>%
#   kable_styling(latex_options=c("HOLD_position", "repeat_header"), full_width = F, font_size = 6, position = "center")%>%
#   #kable_styling(font_size = 7)%>%
#   #column_spec(column = 1, width = "10em") %>% 
#   column_spec(column = 2, width = "20em") 




## ---- workflow
# file.copy(from="../images/chapter3_workflow.pdf", to="figures/chapter3_workflow.pdf",
#           overwrite = TRUE, recursive = FALSE,
#           copy.mode = TRUE)
knitr::include_graphics("figures/chapter3_workflow.pdf", dpi = 300, auto_pdf = TRUE)

## ---- studyArea
# file.copy(from="../empirical/3_output/maps/studyArea_inset.png", to="figures/studyArea_inset.png", 
#           overwrite = TRUE, recursive = FALSE, 
#           copy.mode = TRUE)
knitr::include_graphics("figures/studyArea_inset.png", dpi = 300, auto_pdf = TRUE)




## ---- summAll
load("../empirical/3_output/tables/summary_all/summ_top_4a_ltx.rData")
summ_top_4a<-summ_top_4a[c(3,1:2,4, 6:9)]
#summ_top_4a<-summ_top_4a[c(1:3)]
knitr::kable(summ_top_4a, 
             col.names = c("response", "rank", "source", "fixed effects", "df", "R2m", "R2c", "AIC"),
             align=c("l","l","l","l","r","r","r","r", "r","r"), escape=F,
             caption = 'Top models for each covariate type for all response variables',
             "latex", longtable = TRUE, linesep="", booktabs=T)%>%
  kable_styling(latex_options=c("HOLD_position", "repeat_header"), full_width = F, font_size = 7, position = "center")%>%
  #kable_styling(font_size = 7)%>%
  #column_spec(column = 1, width = "10em") %>% 
  column_spec(column = 3, width = "5em") %>% 
  column_spec(column = 4, width = "20em") %>% 
  #pack_rows(index = table(summ_top_4a$response), escape =F)%>%
  #group_rows(index=summ_top_4a$response, group_label = summ_top_4a$response) %>%
  collapse_rows(columns= 1, latex_hline = "major", valign = "middle")%>%
  kableExtra::landscape()


## ---- summTop
load("../empirical/3_output/tables/summary_all/summ_top_2_ltx.rData")
summ_top_2<-summ_top_2[c(1:2,4:6,8)]
#summ_top_2<-summ_top_2[c(1:3)]
knitr::kable(summ_top_2, 
             col.names = c("Response","Fixed effects", "df", "R2m", "R2c", "Source"),
             align=c("l","l","r","r","r","r", "r","r"), escape=F,
             caption = "Top models for each response variable and corresponding $R^2_{GLMM}$ (marginal $R^2$ (R2m) and conditional $R^2$ (R2c).",
             "latex", linesep="", longtable=T, booktabs=T)%>%
  kable_styling(latex_options=c("HOLD_position", "repeat_header"), repeat_header_text = "\\textit{(continued)}", repeat_header_method="replace",
                full_width = F, font_size = 9, position = "center")%>%
  # kable_styling(latex_options=c("HOLD_position", "repeat_header"), full_width = F, font_size = 6, position = "center")%>%
  
  #kable_styling(font_size = 7)%>%
  #column_spec(column = 1, width = "10em") %>% 
  column_spec(column = 2, width = "16em") 

# 
# ## ---- summTopCom
# load("../empirical/3_output/tables/summary_all/summ_top_2_ltx.rData")
# summ_top_com<-summ_top_2[c(1:2,4:6,8)]
# summ_top_com<- summ_top_com[c(1:5),]
# #summ_top_2<-summ_top_2[c(1:3)]
# knitr::kable(summ_top_com, 
#              col.names = c("response","fixed effects", "df", "R2m", "R2c", "source"),
#              align=c("l","l","l","l","r","r","r","r", "r","r"), escape=F,
#              caption = "Top models for each response variable",
#              "latex", linesep="", longtable=T, booktabs=T)%>%
#   kable_styling(latex_options=c("HOLD_position", "repeat_header"), full_width = F, font_size = 6, position = "center")%>%
#   #kable_styling(font_size = 7)%>%
#   #column_spec(column = 1, width = "10em") %>% 
#   column_spec(column = 2, width = "20em") 
# 
# 
# 
# ## ---- summTopSpp
# load("../empirical/3_output/tables/summary_all/summ_top_2_ltx.rData")
# summ_top_spp<-summ_top_2[c(1:2,4:6,8)]
# summ_top_spp<- summ_top_spp[c(6:25),]
# #summ_top_2<-summ_top_2[c(1:3)]
# knitr::kable(summ_top_spp, 
#              col.names = c("response","fixed effects", "df", "R2m", "R2c", "source"),
#              align=c("l","l","l","l","r","r","r","r", "r","r"), escape=F,
#              caption = "Top models for each response variable",
#              "latex", linesep="", longtable=T, booktabs=T)%>%
#   kable_styling(latex_options=c("HOLD_position", "repeat_header"), full_width = F, font_size = 6, position = "center")%>%
#   #kable_styling(font_size = 7)%>%
#   #column_spec(column = 1, width = "10em") %>% 
#   column_spec(column = 2, width = "20em") 



###### Frequency bar plots

## ---- CovFreqAll
# file.copy(from="../latex_figures/summ_freq_all_c.png", to="figures/summ_freq_all_c.png", 
#           overwrite = TRUE, recursive = FALSE, 
#           copy.mode = TRUE)
knitr::include_graphics("figures/summ_freq_all_c.png", dpi = 300)








###### semi-partial r2 change plots

## ---- r2Change
# file.copy(from="../empirical/3_output/figures/r2_plot_c.png", to="figures/r2_plot_c.png", 
#           overwrite = TRUE, recursive = FALSE, 
#           copy.mode = TRUE)
knitr::include_graphics("figures/r2_plot_c.png", dpi=5000)



##### % contribution of fixed effects

## ---- contrFixed
# file.copy(from="../latex_figures/contrFixed_c.png", to="figures/contrFixed_c.png", 
#           overwrite = TRUE, recursive = FALSE, 
#           copy.mode = TRUE)
#knitr::include_graphics("figures/contrFixed_c.png", dpi = 300)
knitr::include_graphics("figures/fixedR2_cov_all.png", dpi = 300)



## ---- summrich
load("../empirical/2_pipeline/tmp/d5.rData")
load("../empirical/2_pipeline/store/models/rich_fmods_2.rData")
load("../empirical/2_pipeline/store/models/rich_cmods_2.rData")
load("../empirical/2_pipeline/store/models/rich_lMods_2.rData")
load("../empirical/2_pipeline/store/models/rich_nMods.rData")
jtools::export_summs(
        rich_cmods_2$cm29, rich_nMods$nm16, rich_lMods_2$lm23, rich_fmods_2$fm15,
        digits=2, error_pos = "same",
        model.names=c("CAS-FRI", "NBR", "LiDAR", "FUSION"))%>%
  # set_width(1.25)%>%
  # set_height(.50)%>%
  set_font_size(7) %>%
  set_latex_float("t") %>%
  set_top_padding(0)%>%
  set_bottom_padding(0)%>%
  set_left_padding(0)%>%
  set_left_padding(0)%>%
  set_row_height(0.0)%>%
  set_caption("Richness model summaries.")


# ## ---- summshann
# load("../empirical/2_pipeline/tmp/d5.rData")
# load("../empirical/2_pipeline/store/models/shann_fmods_2.rData")
# load("../empirical/2_pipeline/store/models/shann_cmods_2.rData")
# load("../empirical/2_pipeline/store/models/shann_lMods_2.rData")
# load("../empirical/2_pipeline/store/models/shann_nMods.rData")
# jtools::export_summs(
#   shan_cmods_2$cm29, shan_nMods$nm16, shan_lMods_2$lm23, shan_fmods_2$fm15,
#   digits=2, error_pos = "same",
#   model.names=c("CAS-FRI", "NBR", "LiDAR", "FUSION"))%>%
#   # set_width(1.25)%>%
#   # set_height(.50)%>%
#   set_font_size(7) %>%
#   set_latex_float("t") %>%
#   set_top_padding(0)%>%
#   set_bottom_padding(0)%>%
#   set_left_padding(0)%>%
#   set_left_padding(0)%>%
#   set_row_height(0.0)%>%
#   set_caption("shan model summaries.")
# 
# ## ---- summFRic
# load("../empirical/2_pipeline/tmp/d5.rData")
# load("../empirical/2_pipeline/store/models/FRic_fmods_2.rData")
# load("../empirical/2_pipeline/store/models/FRic_cmods_2.rData")
# load("../empirical/2_pipeline/store/models/FRic_lMods_2.rData")
# load("../empirical/2_pipeline/store/models/FRic_nMods.rData")
# jtools::export_summs(
#   FRic_cmods_2$cm29, FRic_nMods$nm16, FRic_lMods_2$lm23, FRic_fmods_2$fm15,
#   digits=2, error_pos = "same",
#   model.names=c("CAS-FRI", "NBR", "LiDAR", "FUSION"))%>%
#   # set_width(1.25)%>%
#   # set_height(.50)%>%
#   set_font_size(7) %>%
#   set_latex_float("t") %>%
#   set_top_padding(0)%>%
#   set_bottom_padding(0)%>%
#   set_left_padding(0)%>%
#   set_left_padding(0)%>%
#   set_row_height(0.0)%>%
#   set_caption("FRic model summaries.")
# 
# ## ---- summFDiv
# load("../empirical/2_pipeline/tmp/d5.rData")
# load("../empirical/2_pipeline/store/models/FDiv_fmods_2.rData")
# load("../empirical/2_pipeline/store/models/FDiv_cmods_2.rData")
# load("../empirical/2_pipeline/store/models/FDiv_lMods_2.rData")
# load("../empirical/2_pipeline/store/models/FDiv_nMods.rData")
# jtools::export_summs(
#   FDiv_cmods_2$cm29, FDiv_nMods$nm16, FDiv_lMods_2$lm23, FDiv_fmods_2$fm15,
#   digits=2, error_pos = "same",
#   model.names=c("CAS-FRI", "NBR", "LiDAR", "FUSION"))%>%
#   # set_width(1.25)%>%
#   # set_height(.50)%>%
#   set_font_size(7) %>%
#   set_latex_float("t") %>%
#   set_top_padding(0)%>%
#   set_bottom_padding(0)%>%
#   set_left_padding(0)%>%
#   set_left_padding(0)%>%
#   set_row_height(0.0)%>%
#   set_caption("FDiv model summaries.")

 

