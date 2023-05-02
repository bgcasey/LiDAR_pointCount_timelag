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
library(jtools)

## ---- covDes
cov_description<-xlsx::read.xlsx("../3_output/tables/covariate_list.xlsx", "variables")
kable(cov_description, position = "h!", col.names = c("Metric",
                                     "Source",
                                     "Description"),
  align = c("l","l","r"), escape=F, caption = 'Spatial covariates included in the analysis.',"latex", booktabs=TRUE, linesep=c("", "", "", "", "\\addlinespace"))%>% 
  # column_spec(column = 3, width = "50em") %>% 
  kable_styling(latex_options="scale_down", font_size = 9)
  # kable_styling(font_size = 6, position = "center", full_width = T)


## ---- workflow
knitr::include_graphics("../3_output/figures/chapter1_workflow.pdf", dpi = 300, auto_pdf = TRUE)

## ---- studyArea
knitr::include_graphics("../3_output/maps/studyArea_inset.png", dpi = 300, auto_pdf = TRUE)

## ---- topModels
topModels<-xlsx::read.xlsx("../3_output/tables/top_models.xlsx", "Sheet3")
kable(topModels, position = "h!",col.names = c("Species",
                                               "Fixed effects",
                                               "$r^{2}m$","$r^{2}c$", "AUC"),
      align = c("l","l","r", "r", "r"), escape=F, caption = 'The fixed effects and summary statistics for top models for each species.',"latex", booktabs=TRUE,  digits=2, linesep=c("\\addlinespace", "\\addlinespace"))%>%
  column_spec(2, width = "25em")%>%
  column_spec(1,  latex_valign = "top")%>%
  kable_styling(latex_options="scale_down", font_size = 8)%>%
  collapse_rows(columns = 2:5, latex_hline="linespace", valign = "top", target = 2)

# kable(topModels, position = "h!",col.names = c("Species",
#                                                "Fixed effects",
#                                                "$r^{2}m$","$r^{2}c$", "AUC"),
#       booktabs=FALSE, digits=2)%>% 
#   # column_spec(2, width = "30em")%>% 
#   # kable_styling(latex_options="scale_down", font_size = 9)%>%
#   collapse_rows(columns = 2:5)




# kable_styling(font_size = 6, position = "center", full_width = T)


## ---- AUCLag
knitr::include_graphics("../3_output/figures/timelag_stats/sppAll_AUC_lm.png", dpi = 300, auto_pdf = TRUE)





## ---- sppAll_FACO
knitr::include_graphics("../3_output/figures/timelag_stats/sppAll_FACO_lm.png", dpi = 300, auto_pdf = TRUE)


## ---- fage
knitr::include_graphics("../3_output/figures/timelag_stats/sppAll_FADIF_lm.png", dpi = 300, auto_pdf = TRUE)



## ---- scatter
knitr::include_graphics("../3_output/figures/timelag_stats/spp_scatter_00to15.png", dpi = 300, auto_pdf = TRUE)



## ---- model_stats
knitr::include_graphics("../3_output/tables/spp_models_stats_00_b.png", dpi = 300, auto_pdf = TRUE)

## ---- varImp
load("../3_output/tables/spp_r2beta_long_rank.rData")
kable(spp_r2beta_long_rank,  col.names = c("Predictor",
                                           "AMRE",
                                           "BTNW",
                                           "MOWA",
                                           "SWTH",
                                           "WIWR",
                                           "WTSP"),
      align = c("l","r","r","r","r","r","r"), 
      caption = "Predictor variables ranked according to their semi-partial $R^2$. AMRE=American Redstart (\\emph{Setophaga ruticilla}); BTNW=Black-throated Green Warbler (\\emph{Setophaga virens}); MOWA=Mourning Warbler (\\emph{Geothlypis philadelphia}); SWTH=Swainson's Thrush (\\emph{Catharus ustulatus}); WIWR=Winter Wren (\\emph{Troglodytes hiemalis}); WTSP=White-throated Sparrow (\\emph{Zonotrichia albicollis}).", 
      booktabs=TRUE,  digits=2, escape = FALSE)%>%
  kable_styling(latex_options="scale_down", font_size = 8)%>%
  column_spec(1, width = "20em")
# %>%
#   column_spec(c(2:7), width = "4em")

# 
# %>%
#   row_spec(1, latex_valign = "b")




