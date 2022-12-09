# Introduction

LiDAR has the potential to improve bird models by providing high resolution structural covariates which, when paired with bird monitoring data, can give insight into bird-habitat relationships [@Bradbury2005]. However, LiDAR acquisitions do not always coincide in time with point count surveys. It is unclear how much this temporal misalignment can influence bird distribution models that use LiDAR derived predictor variables. As disturbance-succession cycles change vegetation structure, eventually LiDAR metrics will no longer reflect ground conditions. Their usefulness as explanatory variables will degrade [@VierlingSwift2014]. Here, we evaluated how time lag between LiDAR acquisitions and bird surveys influenced model robustness for early successional, mature forest, and forest generalist birds.

The composition and structure of forests are changing in response to climate change, shifts to natural disturbance regimes, and increasing industrial development [@Brandt2013]. Predictive models linking field observations to environmental variables can reveal how birds respond to these changes  [@Carrillo-Rubio2014; @englerAvianSDMsCurrent2017; @guisanPredictiveHabitatDistribution2000; @He2015]. Broadly known as species distribution models (SDMs), this family of statistical methods predict bird distributions by comparing habitat where individuals were observed against habitat where they were absent [@Guisan2005]. SDMs and resulting predictive distribution maps are used to understand bird habitat preferences and the drivers of broad scale population declines and have applications in conservation management planning and environmental impact assessments [@englerAvianSDMsCurrent2017; @franklinMappingSpeciesDistributions2010].  



Many factors influence the predictive capacity of SDMs, but the inclusion of ecologically relevant spatial covariates are key drivers of model accuracy [@Franklin1995; @Vaughn2003; @fourcadePaintingsPredictDistribution2018]. Bird SDMs often rely on categorical predictors derived from digital maps delineating land cover, vegetation composition, and human footprint. While useful, they often miss key forest features driving habitat selection, namely those related to vegetation structure. 


Vegetation structure influences the abundance, distribution, and behavior of birds [@MacArthurMacArthur1961, @Davies2014a]. The height and density of vegetation influence where birds perch, feed, and reproduce [@Bradbury2005] by mediating microclimates, providing shelter from weather [@CarrascalDiaz2006], concealment from predators [@GotmarkBlomqvist1995], and creating habitat for insect prey [@halajImportanceHabitatStructure2000]. Light Detection and Ranging (LiDAR) can characterize these three-dimensional forest structures [@limLiDARRemoteSensing2003]. Common LiDAR derived metrics correspond with vegetation height, cover, structural complexity, and density of forest strata [@Davies2014a; @Lefsky2002; @Bae2018; @Kortmann2018; @Renner2018]. Used as predictor variables, LiDAR metrics can improve the predictive power of bird SDMS [@farrellUsingLiDARderivedVegetation2013b; @ficetolaHowManyPredictors2014 @Bae2014; @clawgesUseAirborneLidar2008]. 


Publicly funded regional LIDAR data and space-based sensors like NASA's Ice, Cloud and Land Elevation Satellite-2 (ICESat-2) and Global Ecosystem Dynamics Investigation (GEDI), have made large amounts of wall-to-wall structural data available to researchers [@coopsForestStructureHabitat2016; @dubayahGlobalEcosystemDynamics2020a; @abdalatiICESat2LaserAltimetry2010]. However, LiDAR continues to be under-used in bird ecology. The limited temporal resolution of most LiDAR products may be a factor. LiDAR is often limited to a single season, with long multiyear gaps between repeat surveys. Temporal misalignment between wildlife surveys and LiDAR is common. 


Temporal misalignment occurs when wildlife surveys and LiDAR acquisitions are done at different times [@babcockModelingForestBiomass2016, @VierlingSwift2014]. It's unclear how much temporal misalignment influences the performance of LiDAR based SDMs. Disturbance-succession cycles drive changes in vegetation structure, and eventually, LiDAR gathered over a season will no longer reflect ground conditions. This can occur when the surveyed forest transitions between stages of stand development, e.g. from stand initiation to stem exclusion [@babcockModelingForestBiomass2016; @brassardStandStructureComposition2010]. Temporal misalignment can impact the power of bird SDMs as successional changes in forest structure influence habitat selection by birds [@sittersAssociationsOccupancyHabitat2014]. 

Consider Canada's boreal forests. It is a dynamic successional mosaic driven by forestry, fire, and energy exploration [@Brandt2013; @gauthierBorealForestHealth2015]. The landscape is a patchwork of early to late successional stands with distinct structural characteristics [@brassardStandStructureComposition2010, @Bergeron2012] and bird communities [@Schieck2006]. In early successional forests, bird communities are dominated by species that nest and forage in open vegetation, wetlands, and shrubs, along with some habitat generalists. As trees regenerate and the stand's structural properties change, open habitat species give way to species associated with corresponding forest age classes and strata [@Schieck2006, @lestonLongtermChangesBoreal2018].

Thus, succession occurring between LiDAR and wildlife surveys may influence SDM performance. Consequently, LiDAR's usefulness as a source of explanatory variables can degrade as temporal misalignment increases. For researchers pairing LiDAR covariates with long-term wildlife survey data, this can lead to a trade-off: (1) minimize temporal misalignment by reducing the sample size to survey data gathered near the time of the LiDAR acquisition, or (2) maximize sample size and risk sacrificing model power. 





To inform this trade-off, we addressed the question of how much temporal misalignment is acceptable in LiDAR based SDMs. Our objectives were to (1) evaluate how the time lag between LiDAR acquisitions and bird surveys influence the performance of SDMs across a gradient of 0 to 15 years, (2) compare the influence of temporal misalignment on models for early successional, mid-successional, mature forest, and forest generalist birds, and (3) assess how differences in resultant predictive distribution maps correlate with forest age. 



The effects of temporal misalignment on SDMs will likely vary by habitat type (e.g. forest age, disturbance history, and dominant vegetation) and the life history characteristics of the study species. We predicted that the performance of SDMs will decrease with increased temporal misalignment and that the magnitude of change will vary according to the habitat associations of the focal species. We predicted that (1) SDMs for early successional specialists, Mourning Warbler (*Geothlypis philadelphia*) and White-throated Sparrow (*Zonotrichia albicollis*), would be most affected by temporal misalignment because of faster vertical growth rates of establishment trees and loss of dense shrub layers [@mccarthy2001gap; @fallsWhitethroatedSparrowZonotrichia2020; @pitocchelliMourningWarblerGeothlypis2020]. (2) SDMs for mid-seral species like American Redstart (*Setophaga ruticilla*) that are associated with dense midstory vegetation, would see moderate declines in performance as temporal misalignment increases due to self-thinning during the stem exclusion stage of succession [@sherryAmericanRedstartSetophaga2020a; @brassardStandStructureComposition2010]. And (3) mature forest associates, Black-throated Green Warbler (*Setophaga virens*), will be least effected by temporal misalignment as the processes effecting mature forest canopy structure (insect defoliation and windthrow) happen at too small a scale to effect overall model performance [@morseBlackthroatedGreenWarbler2020; @VierlingSwift2014]. For all species, we predicted that differences in distribution maps will be negatively correlated with forest age. 


# Methods

Our methodological workflow is illustrated in Figure \@ref(fig:workflow). Analyses were done using R statistical software [@R-base]. We built SDMs using bird data from the Calling Lake Fragmentation project [@Schmiegelow1997]. 

```{r workflow, fig.cap= "Conceptual diagram of our methodology. SDM methods were repeated at every time lag for each species. SDMs were compared using AUC and correlation between predictive maps."}
```

## Study area
We used bird survey data from the Calling Lake Fragmentation Experiment [@Schmiegelow1997]. Surveys were conducted across $\approx$ 14,000 ha of boreal mixedwood forests near Calling Lake, in northern Alberta, Canada (55º14'51'' N, 113º28'59'' W) (Figure \@ref(fig:studyArea)). The experiment was designed to study the long-term impacts of forest harvesting on birds [@Schmiegelow1997;@hannonCorridorsMayNot2002;@lestonLongtermChangesBoreal2018]. The study's experimental harvest treatments have led to a landscape patchwork of early- to mid- successional stands surrounded by tracts of unharvested mature forests. When the experiment began in 1994, the landscape was dominated by older mixedwood forests composed of trembling aspen (*Populus tremuloides*, balsam poplar (*Populus balsamifera*) and white spruce (*Picea glauca*) and treed bogs containing black spruce (*Picea mariana*) and larch (*Larix laricina*). Understory vegetation in the mixedwood forests was composed  mostly of alder (*Alnus spp.*) and willow species(*Salix spp.*). 

```{r studyArea, fig.cap= "Locations of point count survey sites from the Calling Lake Fragmentation Study near Calling Lake, Alberta (@Schmiegelow1997). Repeat point counts were conducted during the breeding seasons from 1993 and 2015.", out.width="70%"}
```


## Bird data 
The Calling Lake Fragmentation Experiment included long term bird monitoring via annual repeated point counts. Point counts were done for 20 consecutive breeding seasons (from 1995-2015). As the experiment's study area overlaps spatially with government wall-to-wall LiDAR coverage, there is an opportunity to study the impacts of temporal misalignment between point counts and LiDAR on bird SDMs.

We used detection data from 187 stations where consecutive annual point counts were conducted within sixteen years of the LiDAR acquisition date. Stations were spaced $\approx$ 200 m apart. At each station, three to five morning point count surveys were conducted over each breeding season (May 16 to July 7) between sunrise and 10:00 h. Observers recorded the species detected during each five minute point count interval within sampling radii of 50 and 100 meters using. Please see Schmiegelow et al. -@Schmiegelow1997 for further information on the Calling Lake Fragmentation Experiment's study design and point count protocols. To minimize the influence of forest edges on model predictions, we limited point count stations to those conducted within a single forest stand age (*SD <5 yrs* within a hundred meter buffer of the station). We accessed point count data using the Boreal Avian Modelling project's avian database [@BAM2018]. 




We tested the effects of LiDAR temporal misalignment on seven bird species common to the study area (detected in $\ge$ 10% of all point count events) that were associated with different forest age classes (TABLE). The focal species showed low variability in the total number of detections each year across the 16 years modelled (*CV < 0.5*). 

## Predictor variables
Habitat covariates included LiDAR vegetation metrics provided the provincial government of Alberta (GOA), forest stand attributes from the Common Attribute Schema for Forest Resource Inventories (CAS-FRI) [@Cumming2011a], and mean summer NDVI calculated from a time series of Landsat images [@geologicalsurveyLandsat47Surface2018] (Table \@ref(tab:covDes)). 

LiDAR data covering the study area collected between 2008-2009 was supplied by Alberta Agriculture and Forestry, Government of Alberta. 
Airborne LiDAR was gathered between 2008-2009 by Alberta Agriculture and Forestry, Government of Alberta. The data was part of a larger provincial wall-to-wall LiDAR mapping effort. For an overview of the LiDAR specifications and collection protocols see Alberta Environment and Sustainable Resource Development -@AESRD2013.
The Government of Alberta provided us with 30m LiDAR raster layers representing vegetation height, cover, and density metrics. The rasters were calculated from point cloud data using FUSION software [@mcgaugheyFUSIONLDVSoftware2018]. For each raster, we calculated the mean pixel value within a 100 meter radius of point count stations using the *raster* package in R [@R-raster]. A total of _ LiDAR predictor variables were evaluated in our analyses [TABLE]. 



Forest stand attributes were extracted from the Common Attribute Schema for Forest Resource Inventories (CAS-FRI). CAS-FRI is a standardized collection of 2 ha forest inventory geospatial data [@Cumming2011a]. CAS-FRI stand attributes were interpreted using 1:10,000 to 1:40,000 aerial photography flown between 1987 and 2010. For each point-count station location we determined the disturbance history and mean forest age. As there wasn't much variation in the dominant vegetation species at survey locations, we excluded vegetation composition as a covariate in our models. 






We used the Normalize Difference Vegetation Index (NDVI) as an indicator of vegetation cover [@pettorelliNormalizedDifferenceVegetation2011]. We generated 30 m composite NDVI rasters from 1995 to 2015 using surface reflectance imagery from the Landsat 5 Thematic Mapper (bands 3 and 4), the Landsat 7 Enhanced Thematic Mapper (bands 3 and 4), and the Landsat 8 Operational Land Imager (bands 4 and 5)[@geologicalsurveyLandsat47Surface2018]. Satellite images were accessed and processed using the Google Earth Engine (GEE) Code Editor [@gorelickGoogleEarthEngine2017]. As all of the point counts occurred during the summer breeding season, we limited Landsat images to those taken between June and September. Images were masked to exclude snow, cloud, and cloud shadow pixels using the CFMask algorithm [@fogaCloudDetectionAlgorithm2017]. We generated annual median composites of masked Landsat images and calculated NDVI rasters from the composites ($NDVI=\frac{NIR-R}{NIR+R}$ [@USGS_NDVI]. For each survey year we calculated the mean values of NDVI pixels within a 100 m buffer of point count locations. 


## Analyses

We evaluated the effects of LiDAR temporal misalignment on model performance by comparing mixed effects logistic regression models. We built models using the `glmer` function in the R package *lme4* [@batesFittingLinearMixedeffects2015]. To accommodate the influence of survey methods and nuisance parameters on detection probabilities, we included statistical offsets in the models generated using QPAD [@SolymosMatsuoka2013]. 

We used the following multi-step process for each focal species. In Step 1, we grouped detection data according to the amount of temporal misalignment with LiDAR. There were 16 groups, one group for each year of time-lag between LiDAR and point count surveys (zero through fifteen years). 

In Step 2, we built and evaluated models for the zero time-lag group of point counts. We first computed a global model with all candidate predictor variables as fixed effects and station location as a random effect. We checked for nonlinear relationships between response and predictors by separately evaluating, linear, quadratic, and cubic functions of each variable. To avoid multicollinearity between predictors, we used Pearson correlation coefficients and VIF scores to iteratively remove highly correlated predictors from the global model. We kept metrics with low correlation (*r* < 0.5 and VIF < 3) that were associated with different vegetation structure categories: height, cover, and complexity [@valbuenaStandardizingEcosystemMorphological2020]. For correlated metrics associated with the same category, we selected the variable with the lowest *P* value. We evaluated models consisting of the remaining predictors using the 'dredge' function in the R package *MuMIn* [@bartonMuMInMultimodelInference2020]. We defined the top model as that with the lowest Akaike's Information Criterion (AIC)[@burnhamModelSelectionMultimodel2002]. We calculated pseudo-$R^2$ as a measure of explanatory power @nakagawaGeneralSimpleMethod2013]. For models with similar AIC values (a difference than two) we selected the model with the largest pseudo-$R^2$.

In Step 3, we applied the Step 2 model to the remaining groups of time-lag point counts. For each group, we used the fitted model and a raster stack of covariates to map species occurrence probability using the 'predict' function in the R package *raster* [@hijmansRasterGeographicData2021].

In Step 4, we compared the performance of different time-lag models. We compared their predictive accuracy using the area under the receiver operating curve (ROC) (AUC) calculated using the 'auc' function in the pROC package in R [@robinPROCOpensourcePackage2011]. We compared the predictive maps for different time-lag groups by calculating the per pixel differences between them. I.e., we subtracted the zero time lag map from the map of each subsequent time lag group, resulting in 15 "difference" rasters. We used Pearson's correlations to examine the relationship between differences in species occurrence probability and forest age.

(Table \@ref(tab:topModels))








# References {-}

<div id="refs"></div>

\pagebreak


# Tables {-}


```{r covDes}
```

```{r topModels}
```
