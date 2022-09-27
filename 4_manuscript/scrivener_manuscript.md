# Introduction

LiDAR has the potential to improve bird models by providing high resolution structural covariates which, when paired with bird monitoring data, can provide insight into bird-habitat relationships [REF]. However, LiDAR acquisitions do not always coincide temporally with bird surveys, and it is unclear how much these time lags influence models. Disturbance-succession cycles change vegetation structure. Eventually, LiDAR metrics will no longer reflect ground conditions, and their usefulness as explanatory variables will degrade. Here, we evaluated how the time lag between LiDAR acquisitions and bird surveys influenced model robustness for early successional, mature forest, and generalist birds.

The composition and structure of forests are changing in response to climate change, shifts to natural disturbance regimes, and increasing industrial development [@Brandt2013]. Predictive models linking distribution, abundance, and community structure to select environmental variables have been used to understand how forest birds respond to these changes  [@Carrillo-Rubio2014; @Engler2017a; @guisanPredictiveHabitatDistribution2000; @He2015]. Broadly known as species distribution models(SDMs), this family of statistical methods relate field observations (e.g. occupancy or abundance) with spatial covariates [@Guisan2005]. Find correlations between detection data and eviromnetal covariates to predict... While methods vary, SDMs predict species occurrence by comparing the habitat where individuals have been observed (via traditional human point counts or autonomous bioacoustic monitoring) against habitat where species are absent [Guisan2005]. With innovations in modelling methods, computational power, and environmental monitoring, SDMs are an important tool in ecology, conservation biology, and wildlife management [@Elith2009]. SDMs and resulting predictive distribution maps are often applied to bird research @englerAvianSDMsCurrent2017, and have informed conservation management planning, environmental impact assessments[REF], and bird diversity modelling [REF][@englerAvianSDMsCurrent2017; @franklinMappingSpeciesDistributions2010]. 



------------------------------------------------------------------------- 


See reviews by Engler et al. -@englerAvianSDMsCurrent2017 and Elith et al. -@Elith2009. Engler et al. -@englerAvianSDMsCurrent2017 presented a detailed overview of the many applications of SDMs in the study of birds. 


They provide insite into: 
	1. habitat characteristics necessary for birds, 
	2. Predictions of distributions and forecast distributions under changing 			scenarios. 
	3. Used to estimate species abundance and density.
Explore the patterns and processes driving distributions of species in space. 




Species distribution models can be used to link occurrence data with environmental covariates to predict species distributions. 

While SDMs encompass a variety of statistical approaches including Generalized linear models, generalized additive models, and Bayesian approaches.  MOST often encorpprate presence-absence data from species detection data from traditional human point counts or autonomous bioacoustic monitoring. Most follow the same 




Many factors influence the predictive capacity of SDMs, but the inclusion of ecologically relevant spatial covariates are key drivers of model accuracy [@Franklin1995; @Vaughn2003; @fourcadePaintingsPredictDistribution2018]. Bird SDMs often rely on categorical predictors derived from digital maps delineating land cover, vegetation composition, and human footprint. While these models are often supplemented with continuous bioclimatic variables, they often miss important features driving bird response [REF]. 
Continuous predictors from remote sensing can better describe the mechanisms driving habitat selection [@heWillRemoteSensing2015?]. For example, spectral metrics linked to vegetation are often representative of the shelter and food resources used by birds. The normalized vegetation index (NDVI) derived from Sentinel and Landsat satellites has been used to measure vegetation productivity, habitat variability, and plant phenology [@pettorelliNormalizedDifferenceVegetation2011]. And the normalized burn ratio (NBR) can quantify disturbance severity and successional recovery rates [@hislopUsingLandsatSpectral2018]. MODIS land surface temperature (LST) has been used to predict bird response to heatwaves [@albrightHeatWavesMeasured2011].  

------------------
maps and digital forest resource inventories (FRIs) supplement bioclimatic variables; les. While these products may contain useful metrics related to plant species composition and human footprint, and canopy height, they often lack data on three-dimensional often lack more detailed measures of important habitat features driving bird habitat selection. They don't fully describe the mechanisms driving habitat selection. Lack data 
Predictors from space-based optical remote sensors are used most often and can provide more detailed levels of eco improve on the class based habitat predictors of FRI [@heWillRemoteSensing2015]. 
and extreme weather events [Space-based  land surface temperature and precipitation measurements from MODIS have been used over terrestrial weather station data for _models [REF].
has been used to extract predictors related to harvest and burn severity.while spectral indices derived from satellite and aerial imagery can map land cover change and measure rates of post-disturbance habitat recovery [@Northrup2019; @Rittenhouse2010]




While spectral metrics are used in many broad scale SDMs, they don't fully capture bird habitat, namely the three dimensional structure of vegetation. Vegetation structure influences the abundance, distribution, and behavior of birds [@ MacArthurMacArthur1961, @Davies2014a]. Vegetation at different heights and densities influence where birds perch, feed, and reproduce [@Bradbury2005] by mediating microclimates, providing shelter from weather [@CarrascalDiaz2006], concealment from predators [@GotmarkBlomqvist1995], and creating habitat for insect prey [@halajImportanceHabitatStructure2000]. Light Detection and Ranging
(LiDAR) can directly characterize these structures by measuring vegetation height, cover, structural complexity, and density of forest strata [@Davies2014a; @Lefsky2002; @Bae2018; @Kortmann2018; @Renner2018]. Such covariates tend to improve the predictive power of bird SDMS [REF].


Despite the established predictive power or LIDAR-based SDMs [@Farrell2013], LiDAR derived products have not been fully adopted in avian ecology. Most studies continue to rely on classified land cover data and spectral measures of vegetation cover (e.g. NDVI). Studies that use LIDAR tend to be spatially and temporally limited.





LiDAR has the potential to improve research by providing high resolution structural covariates which, When paired with bird monitoring data, can provide insight into bird-habitat relationships.  Given the time and cost constraints to acquiring LiDAR it is often limited to a single season and to smaller study areas, and with long multiyear gaps between subsequent aquistions. However, LiDAR acquisitions do not always coincide temporally with wildlife surveys, and it is unclear how much these time lags influence the performance of SDMs. 
Publicly funded LIDAR acquisitions and open source software have made terabytes of structural data accessible to researchers [REF]. However, LiDAR continues to be underutilized in bird ecology. The temporal resolution of LiDAR may be a factor. Available LiDAR is often limited to a single season, with long multiyear gaps between repeat acquisitions. Consequently, LiDAR acquisitions do not always coincide with wildlife surveys. It is unclear how much time lags between LiDAR acquisitions and bird surveys influence the performance of SDMs. Disturbance-succession cycles change vegetation structure over time, and, eventually, LiDAR metrics no longer reflect ground conditions. Their usefulness as explanatory variables should degrade over time. This effect should be greater in rapidly changing landscapes. 
Thus, the impact of time-lag on model performance is important to consider, especially in rapidly changing landscapes like Canada's boreal forests. The boreal forest is a dynamic successional mosaic driven by forestry, fire, and energy exploration [@Brandt2013; gauthierBorealForestHealth2015]. The forests  are changing as the historical drivers of succession (fire, insects, and disease) are giving way to forestry and oil and gas exploration. This has broad landscape scale consequences.  For example, conventional even-aged forest management and salvage logging has shifted the age structure and composition of boreal forests towards younger broadleaf dominated stands [@kuuluvainenYoungOldForest2018a]. Disturbance-driven succession characteristic of the boreal influences the composition of bird communities [@lestonLongtermChangesBoreal2018] as species occupancy corresponds with available structural resources [@sittersAssociationsOccupancyHabitat2014]. Forest recovery from timber harvesting provides an illustration. In the years immediately following a timber harvest, birds associated with open-vegetation and dense shrubs become more common. As the forest regenerates and new vegetation resources become available for nesting and foraging, species associated with older forest classes return and open-habitat birds become less common [@Schieck2006].  Thus, succession occurring between the time the LiDAR was acquired and the bird survey will likely effect SDM performance. How long LiDAR is reliable may vary by habitat (e.g. forest age, disturbance history, dominant vegetation, etc.) and the life history characteristics of particular bird species. 








------------------------------------------------------------------
In the boreal, fire, forestry harvests, and linear features like roads and seismic lines effect forest composition, structure, and age. 
 froim one driven by fire to one dominated by forestry and energy exploration.  composition and structure of boreal forests are changing in response to climate change, shifts to Natural disturbance regimes are being replaced by forestry and industrial development







Timelag between the  Uderstanding the temporal limitations of LiDAR is important when selecting sources of environmental covariates. 



Here, we evaluated how the time lag between LiDAR acquisitions and bird surveys influence the performance of SDMs across a gradient of 0 to 15 years. We evaluated if the influence of time-lag varies between early successional, mature forest, and forest generalist birds. Finally, we assessed how differences in the resultant predictive distribution maps correspond to forest age. 



We hypothesized that the performance of models will decrease with increased LiDAR acquisition time-lag and that the magnitude of change will vary according the habitat preferences of the study species [figure]. 



---------------------------------------------------------------
We predicted that (1) SDMs for early successional species will see the greatest declines in performance due to more rapid amounts of change in their preferred habitat during the time lag period and models will no longer be reliable after 5 years lag. (2) Forest generalist species will see moderate declines in performance as time lag increases (3) The effect of time lag on SDMs for mature forest species will be smaller due the the relative stability of their forest habitats, but will still observe mild decreases in performance as windfall and disease opens new gaps in the mature forest canopy. For all species, differences in predictive maps will be negatively correlated with the age of the forest when LiDAR was acquired. 

# Methods

Our methodological workflow is illustrated in Figure \@ref(fig:workflow). Analyses were done using R statistical software [@R-base]. We developed SDMs following the methodology outlined by Guisan and Thuiller -@Guisan2005 with bird data collected by the Calling Lake Fragmentation project. 

```{r workflow, fig.cap= "Conceptual diagram of our methodology. SDM methods were repeated at every time lag for each species and compared using AUC and correlation between predictive maps. "}
```

The study area is $\approx$ 11,327 ha of boreal mixedwood forests near Calling Lake, in northern Alberta, Canada (55º14'51'' N, 113º28'59'' W) Figure \@ref(fig:studyArea)). Calling Lake is located in the Boreal Central Mixedwood Natural Subregion of Northern Alberta [@Downing2006]. When the point counts were conducted the area contained black spruce (*Picea mariana*) fens and mixedwood forests composed of trembling aspen (*Populus tremuloides*, balsam poplar (*Populus balsamifera*),  and white spruce (*Picea glauca*), with alder (*Alnus spp.*) and willow (*Salix spp.*) dominated understories [@Schmiegelow1997]. Experimental forest harvesting done through the Calling Lake Fragmentation Project has left a patchwork of successional stages amidst tracts of unharvested forest (@Schmiegelow1997). left a patchwork of forests of different stages of recovery alongside unharvested tracks (@Schmiegelow1997).
```{r studyArea, fig.cap= "Locations of point counts from the Calling Lake Fragmentation Study near Calling Lake, Alberta (@Schmiegelow1997). Consecutive annual counts were conducted between ... and ... ", out.width="70%"}
```


Data Sources 
Point count data from the Calling Lake Fragmentation Study was accessed through the Boreal Avian Modelling Project's (BAM) avian database [REF]. Point count locations have had sixteen years of  consecutive years of point count surveys, providing an opportunity to test the effects of LiDAR acquisition time lag on SDMs. See Schmiegelow et al. -@Schmiegelow1997 for details on the Calling Lake Fragmentation Project's study design. Summer bird surveys were conducted annually between 1995 and 2014. Point count durations were five minutes. Within a radius of one hundred meters. Morning point counts (sunrise-10:00) 3-5 between 16 May and 7 July. We used detection data from 150 stations that all had  consecutive annual surveys between 1993 and 2014. Stations were selected that were within 0 to 16 years of LiDAR acquisition date (296 stations and >14000 individual point counts). Point count locations were spaced $\approx$ 200 m apart. 

To reduce the likelihood of double-counting birds or incorrect assignment of observations to site type, we restricted analyses to detections within the 50 m sampling radius." 







We performed our time lag analysis on seven bird species associated with different nesting and foraging guilds, forest strata, habitat structures, and forest age classes (TABLE). [@EltonTraits2021].  



Species were selected that exhibited low variability in the total number of detections each year across the 16 years modelled (CV  < 0.5). To ensure that the selected species was abundant enough to model, we chose species that were detected in > 10% of all point count events. Variability of detections between years

The seven selected species included early successional specialists:  mature forests species, habitat generalist ...   American Redstart (*Setophaga ruticilla*), Black-throated Green Warbler (*Setophaga virens*), and Swainson's Thrush (*Catharus ustulatus*), Mourning Warbler (*Geothlypis philadelphia*), White-throated Sparrow (*Zonotrichia albicollis*), , Winter Wren(*Troglodytes hiemalis*).


Based on this, we selected Black-throated Green Warbler (BTNW),  a mature forest species; Swainson's Thrush (SWTH) a forest generalist; and Mourning Warbler (MOWA) an early-seral specialist. YBSA Yellow-bellied Sapsucker AMRE American redstart (mid seral) WTSP White throated sparrow (early)

To minimize the influence of forest edges and adjacent differently aged forest, we excluded point count stations with high variation of forest age classes within a hundred meter radius (SD >5 yrs) 

We matched point count data from the BAM database with LiDAR data collected by the provincial government of Alberta (GOA) between 2008 and 2009, classified landcover data from CAS-FRI [REF], and climate data from _.

LiDAR data covering the study area collected between 2008-2009 was supplied by Alberta Agriculture and Forestry, Government of Alberta. The data was part of a wider effort to gather wall-to-wall LiDAR coverage across the province. For an overview of the LiDAR specifications and collection protocols see Alberta Environment and Sustainable Resource Development -@AESRD2013.


We selected model covariates from 34 lidar candidate predictors associated with canopy closure, height, and vegetation density. Metrics were summarized within an 100 meter radius of each point count station location using the *raster* package in R [@R-raster].


"CAS-FRI is a compilation of standardized forest resource inventory data from across Canada. It includes common vegetation and disturbance attributes used in bird habitat models, including forest composition and disturbance history [@Cumming2011a]. We used the following CAS-FRI attributes as model covariates in our analysis: post-harvest forest type (coniferous, deciduous, or mixed-wood stand), vegetation composition, crown closure, canopy height, and harvest intensity."[self] Estimated forest age at the time of the point count,  and disturbance history. As there wasn't much variation in dominant vegetation species of survey locations, we excluded vegetation composition as a covariate in our models. 

Forest age, forest age class, disturbance history. 

Forest age classes adapted from @chen2002dynamics.

To minimize the influence of forest edges and adjacent differently aged forest, remove stations that are surrounded by a variety of forest ages (sd>5).

We developed SDMs following the methodology outlined by Guisan and Thuiller -@Guisan2005. Using point counts conducted the same year as the LiDAR acquisition date, we used mixed effect logistic regression to assess the influence of candidate lidar predictors on specifics occupancy using the *lme4* package in R[@batesFittingLinearMixedeffects2015] using station location as a random effect.  We built global models using all available LiDAR predictors as fixed effects. We built candidate mixed effect logistic regression models useing all available lidar predictors n=34. To avoid multicollinearity between predictors we used Pearson correlation and VIF scores to iteratively removed highly correlated predictors from our models. We kept metrics with low correlation (*r* < 0.5 and VIF < 3) and are associated with different measures of vegetation structure: height, cover, and structural complexity [@valbuenaStandardizingEcosystemMorphological2020]. For correlated metrics associated with similar, we selected the most statistically significant predictor.
Once we had a global model wth a reduced set of candidate predictors variables, deselected the best model using the *MuMIn* package in R @bartonMuMInMultimodelInference2020. 

I will combine LiDAR metrics with the CAS-FRI data to model species habitat relationships for American Redstart, Black-throated Green Warbler, and Swainson's Thrush. I will evaluate bird abundance as a function of LiDAR metrics and CAS-FRI data [@MacKenzie2006] for each year (time lag) for 10 years after the LiDAR acquisition date using GLMs. To accommodate the influence of survey methods and nuisance parameters on detection probabilities, I will generate statistical offsets via QPAD [@SolymosMatsuoka2013]. 




# Results

# Discussion

While AUCs dididnt significantly decline for all species, the mapped spatial distribution did, suggesting that timelag plays a role. 

# Conclusion