# Introduction

The composition and structure of forests are changing in response to climate change, shifts to natural disturbance regimes, and increasing industrial development [@Brandt2013]. Predictive models linking field observations to environmental variables can reveal how birds respond to these changes  [@Carrillo-Rubio2014; @englerAvianSDMsCurrent2017; @guisanPredictiveHabitatDistribution2000; @He2015]. Broadly known as species distribution models (SDMs), this family of statistical methods predict bird distributions by comparing habitat where individuals were observed against habitat where they were absent [@Guisan2005]. SDMs and resulting predictive distribution maps are used to understand bird habitat preferences and the drivers of broad scale population declines and have applications in conservation management planning and environmental impact assessments [@englerAvianSDMsCurrent2017; @franklinMappingSpeciesDistributions2010].  



Many factors influence the predictive capacity of SDMs, but the inclusion of ecologically relevant spatial covariates are key drivers of model accuracy [@Franklin1995; @Vaughn2003; @fourcadePaintingsPredictDistribution2018]. Bird SDMs often rely on categorical predictors derived from digital maps delineating land cover, vegetation composition, and human footprint. While useful, they often miss key forest features driving habitat selection, namely those related to vegetation structure. 


Vegetation structure influences the abundance, distribution, and behavior of birds [@MacArthurMacArthur1961, @Davies2014a]. The height and density of vegetation influence where birds perch, feed, and reproduce [@Bradbury2005] by mediating microclimates, providing shelter from weather [@CarrascalDiaz2006], concealment from predators [@GotmarkBlomqvist1995], and creating habitat for insect prey [@halajImportanceHabitatStructure2000]. Light Detection and Ranging (LiDAR) can characterize these three-dimensional forest structures [@limLiDARRemoteSensing2003]. Common LiDAR derived metrics correspond with vegetation height, cover, structural complexity, and density of forest strata [@Davies2014a; @Lefsky2002; @Bae2018; @Kortmann2018; @Renner2018]. Used as predictor variables, LiDAR metrics can improve the predictive power of bird SDMs [@farrellUsingLiDARderivedVegetation2013b; @ficetolaHowManyPredictors2014; @Bae2014; @clawgesUseAirborneLidar2008]. 


Publicly funded regional LIDAR data and space-based sensors like NASA's Ice, Cloud and Land Elevation Satellite-2 (ICESat-2) and Global Ecosystem Dynamics Investigation (GEDI), have made large amounts of wall-to-wall structural data available to researchers [@coopsForestStructureHabitat2016; @dubayahGlobalEcosystemDynamics2020a; @abdalatiICESat2LaserAltimetry2010]. However, LiDAR continues to be under-used in bird ecology. The limited temporal resolution of most LiDAR products may be a factor. LiDAR is often limited to a single season, with long multiyear gaps between repeat surveys. Temporal misalignment between wildlife surveys and LiDAR is common. 


Temporal misalignment occurs when wildlife surveys and LiDAR acquisitions are done at different times [@babcockModelingForestBiomass2016; @VierlingSwift2014]. It's unclear how much temporal misalignment influences the performance of LiDAR based SDMs. Disturbance-succession cycles drive changes in vegetation structure, and eventually, LiDAR gathered over a season will no longer reflect ground conditions. This can occur when the surveyed forest transitions between stages of stand development, e.g. from stand initiation to stem exclusion [@babcockModelingForestBiomass2016; @brassardStandStructureComposition2010]. Temporal misalignment can impact the power of bird SDMs as successional changes in forest structure influence habitat selection by birds [@sittersAssociationsOccupancyHabitat2014]. 

Consider Canada's boreal forests. It is a dynamic successional mosaic driven by forestry, fire, and energy exploration [@Brandt2013; @gauthierBorealForestHealth2015]. The landscape is a patchwork of early to late-successional stands with distinct structural characteristics [@brassardStandStructureComposition2010; @Bergeron2012] and bird communities [@Schieck2006]. In early-successional forests, bird communities are dominated by species that nest and forage in open vegetation, wetlands, and shrubs, along with some habitat generalists. As trees regenerate and the stand's structural properties change, open habitat species give way to species associated with corresponding forest age classes and strata [@Schieck2006; @lestonLongtermChangesBoreal2018].

Thus, succession occurring between LiDAR and wildlife surveys may influence SDM performance. Consequently, LiDAR's usefulness as a source of explanatory variables can degrade as temporal misalignment increases. For researchers pairing LiDAR covariates with long-term wildlife survey data, this can lead to a trade-off: (1) minimize temporal misalignment by reducing the sample size to survey data gathered near the time of the LiDAR acquisition, or (2) maximize sample size and risk sacrificing model power. 





To inform this trade-off, we addressed the question of how much temporal misalignment is acceptable in LiDAR based SDMs. Our objectives were to (1) evaluate how the time lag between LiDAR acquisitions and bird surveys influence the performance of SDMs across a gradient of 0 to 15 years, (2) compare the influence of temporal misalignment on models for early-successional, mid-successional, mature-forest, and forest generalist birds, and (3) assess how differences in resultant predictive distribution maps correlate with forest age. 



The effects of temporal misalignment on SDMs will likely vary by habitat type (e.g. forest age, disturbance history, and dominant vegetation) and the life history characteristics of the study species. We predicted that the performance of SDMs will decrease with increased temporal misalignment and that the magnitude of change will vary according to the habitat associations of the focal species. We predicted that (1) SDMs for early-successional associates, Mourning Warbler (*Geothlypis philadelphia*) and White-throated Sparrow (*Zonotrichia albicollis*), would be most affected by temporal misalignment because of faster vertical growth rates of establishment trees and loss of dense shrub layers [@mccarthy2001gap; @fallsWhitethroatedSparrowZonotrichia2020; @pitocchelliMourningWarblerGeothlypis2020]. (2) SDMs for mid-seral species like American Redstart (*Setophaga ruticilla*) that are associated with dense midstory vegetation, would see moderate declines in performance as temporal misalignment increases due to self-thinning during the stem exclusion stage of succession [@sherryAmericanRedstartSetophaga2020a; @brassardStandStructureComposition2010]. And (3) mature forest associates, Black-throated Green Warbler (*Setophaga virens*), will be least effected by temporal misalignment as the processes effecting mature forest canopy structure (insect defoliation and windthrow) happen at too small a scale to effect overall model performance [@morseBlackthroatedGreenWarbler2020a; @VierlingSwift2014]. For all species, we predicted that differences in distribution maps will be negatively correlated with forest age. 


# Methods

Our methodological workflow is illustrated in Figure \@ref(fig:workflow). Analyses were done using R statistical software [@R-base]. We built SDMs using bird data from the Calling Lake Fragmentation project [@Schmiegelow1997]. 

```{r workflow, fig.cap= "Conceptual diagram of our methodology. SDM methods were repeated at every time lag for each species. SDMs were compared using AUC and correlation between predictive maps."}
```

## Study area
We used bird survey data from the Calling Lake Fragmentation Experiment [@Schmiegelow1997]. Surveys were conducted across $\approx$ 14,000 ha of boreal mixedwood forests near Calling Lake, in northern Alberta, Canada (55º14'51'' N, 113º28'59'' W) (Figure \@ref(fig:studyArea)). The experiment was designed to study the long-term impacts of forest harvesting on birds [@Schmiegelow1997;@hannonCorridorsMayNot2002;@lestonLongtermChangesBoreal2018]. The study's experimental harvest treatments have led to a landscape patchwork of early- to mid- successional stands surrounded by tracts of unharvested mature forests. When the experiment began in 1994, the landscape was dominated by older mixedwood forests composed of trembling aspen (*Populus tremuloides*, balsam poplar (*Populus balsamifera*) and white spruce (*Picea glauca*) and treed bogs containing black spruce (*Picea mariana*) and larch (*Larix laricina*). Understory vegetation in the mixedwood forests was composed  mostly of alder (*Alnus spp.*) and willow species(*Salix spp.*). 
(ref:caption) [@Schmiegelow1997]
```{r studyArea, fig.cap= "Locations of point count survey sites from the Calling Lake Fragmentation Study near Calling Lake, Alberta (ref:caption). Repeat point counts were conducted during the breeding seasons from 1993 and 2015.", out.width="100%"}
```


## Bird data 
The Calling Lake Fragmentation Experiment included long term bird monitoring via annual repeated point counts. Point counts were done for 20 consecutive breeding seasons (from 1995-2015). As the experiment's study area overlaps spatially with government wall-to-wall LiDAR coverage, there is an opportunity to study the impacts of temporal misalignment between point counts and LiDAR on bird SDMs.

We used detection data from 187 stations where consecutive annual point counts were conducted within sixteen years of the LiDAR acquisition date. Stations were spaced $\approx$ 200 m apart. At each station, three to five morning point count surveys were conducted over each breeding season (May 16 to July 7) between sunrise and 10:00 h. Observers recorded the species detected during each five minute point count interval within sampling radii of 50 and 100 meters using. See Schmiegelow et al. [-@Schmiegelow1997] for further information on the Calling Lake Fragmentation Experiment's study design and point count protocols. To minimize the influence of forest edges on model predictions, we limited point count stations to those conducted within a single forest stand age (*SD <5 yrs* within a hundred meter buffer of the station). We accessed point count data using the Boreal Avian Modelling project's avian database [@BAM2018]. 




We tested the effects of LiDAR temporal misalignment on six bird species common to the study area (detected in $\ge$ 10% of all point count events) that were associated with different forest age classes: American Redstart (*Setophaga ruticilla*), Black-throated Green Warbler (*Setophaga virens*), Mourning Warbler (*Geothlypis philadelphia*), Swainson's Thrush (*Catharus ustulatus*), White-throated Sparrow (*Zonotrichia albicollis*), and Winter Wren (*Troglodytes hiemalis*). The focal species showed low variability in the total number of detections each year across the 16 years modelled (*CV < 0.5*). 

## Predictor variables
Habitat covariates included LiDAR vegetation metrics provided the provincial government of Alberta (GOA), forest stand attributes from the Common Attribute Schema for Forest Resource Inventories (CAS-FRI) [@Cumming2011a], and mean summer NDVI calculated from a time series of Landsat images [@geologicalsurveyLandsat47Surface2018] (Table \@ref(tab:covDes)). 

```{r covDes}
```


LiDAR data covering the study area collected between 2008-2009 was supplied by Alberta Agriculture and Forestry, Government of Alberta. 
Airborne LiDAR was gathered between 2008-2009 by Alberta Agriculture and Forestry, Government of Alberta. The data was part of a larger provincial wall-to-wall LiDAR mapping effort. For an overview of the LiDAR specifications and collection protocols see Alberta Environment and Sustainable Resource Development -@AESRD2013.
The Government of Alberta provided us with 30m LiDAR raster layers representing vegetation height, cover, and density metrics. The rasters were calculated from point cloud data using FUSION software [@mcgaugheyFUSIONLDVSoftware2018]. For each raster, we calculated the mean pixel value within a 100 meter radius of point count stations using the *raster* package in R [@R-raster].


Forest stand attributes were extracted from the Common Attribute Schema for Forest Resource Inventories (CAS-FRI). CAS-FRI is a standardized collection of 2 ha forest inventory geospatial data [@Cumming2011a]. CAS-FRI stand attributes were interpreted using 1:10,000 to 1:40,000 aerial photography flown between 1987 and 2010. For each point-count station location we determined the disturbance history and mean forest age. As there wasn't much variation in the dominant vegetation species at survey locations, we excluded vegetation composition as a covariate in our models. 






We used the Normalize Difference Vegetation Index (NDVI) as an indicator of vegetation cover [@pettorelliNormalizedDifferenceVegetation2011]. We generated 30 m composite NDVI rasters from 1995 to 2015 using surface reflectance imagery from the Landsat 5 Thematic Mapper (bands 3 and 4), the Landsat 7 Enhanced Thematic Mapper (bands 3 and 4), and the Landsat 8 Operational Land Imager (bands 4 and 5) [@geologicalsurveyLandsat47Surface2018]. Satellite images were accessed and processed using the Google Earth Engine (GEE) Code Editor [@gorelickGoogleEarthEngine2017]. As all of the point counts occurred during the summer breeding season, we limited Landsat images to those taken between June and September. Images were masked to exclude snow, cloud, and cloud shadow pixels using the CFMask algorithm [@fogaCloudDetectionAlgorithm2017]. We generated annual median composites of masked Landsat images and calculated NDVI rasters from the composites ($NDVI=\frac{NIR-R}{NIR+R}$) [@USGS_NDVI]. For each survey year we calculated the mean values of NDVI pixels within a 100 m buffer of point count locations. 


## Analyses

We evaluated the effects of LiDAR temporal misalignment on model performance by comparing mixed effects logistic regression models. We built models using the `glmer` function in the R package *lme4* [@batesFittingLinearMixedeffects2015]. To accommodate the influence of survey methods and nuisance parameters on detection probabilities, we included statistical offsets in the models generated using QPAD [@SolymosMatsuoka2013]. 

We used the following multi-step process for each focal species. In Step 1, we grouped detection data according to the amount of temporal misalignment with LiDAR. There were 16 groups, one group for each year of time-lag between LiDAR and point count surveys (zero through fifteen years). 

In Step 2, we built and evaluated models for the zero time-lag group of point counts. We first computed a global model with all candidate predictor variables as fixed effects and station location as a random effect. We checked for nonlinear relationships between response and predictors by separately evaluating, linear, quadratic, and cubic functions of each variable. To avoid multicollinearity between predictors, we used Pearson correlation coefficients and VIF scores to iteratively remove highly correlated predictors from the global model. We kept metrics with low correlation (*r* < 0.5 and VIF < 3) that were associated with different vegetation structure categories: height, cover, and complexity [@valbuenaStandardizingEcosystemMorphological2020]. For correlated metrics associated with the same category, we selected the variable with the lowest *P* value. We evaluated models consisting of the remaining predictors using the 'dredge' function in the R package *MuMIn* [@bartonMuMInMultimodelInference2020]. We defined the top model as that with the lowest Akaike's Information Criterion (AIC)[@burnhamModelSelectionMultimodel2002]. We calculated pseudo-$R^2$ as a measure of explanatory power @nakagawaGeneralSimpleMethod2013]. For models with similar AIC values (a difference than two) we selected the model with the largest pseudo-$R^2$.

In Step 3, we applied the Step 2 model to the remaining groups of time-lag point counts. For each group, we used the fitted model and a raster stack of covariates to map species occurrence probability using the 'predict' function in the R package *raster* [@hijmansRasterGeographicData2021].

In Step 4, we compared the performance of different time-lag models. We compared their predictive accuracy using the area under the receiver operating curve (ROC) (AUC) calculated using the 'auc' function in the pROC package in R [@robinPROCOpensourcePackage2011]. Models with an AUC >0.7 were considered moderately predictive of species occurrence [@vanagasReceiverOperatingCharacteristic2004] We tracked the amount of LiDAR time-lag necessary for models to have an AUC <0.70. The contribution of individual fixed effects were estimated by calculating semi-partial $R^2$ values using the *r2beta* function from the *r2glmm* package using standardized general variances [@jaegerR2glmmComputesSquared2017]. We compared the predictive maps for different time-lag groups by calculating the per pixel differences between them. I.e., we subtracted the zero time lag map from the map of each subsequent time lag group, resulting in 15 "difference" rasters. We used Pearson's correlations to examine the relationship between differences in species occurrence probability and forest age.










# Results
The LiDAR variables used in top performing models varied by species (Table \@ref(tab:topModels)). The most common predictor variables across species were maximum vegetation height (used in all top models), and mean summer NDVI (used in top models for five out of six species). The rates and magnitude by which AUC was effected by LiDAR-bird survey temporal misalignment varied by species (Figure \@ref(fig:AUCLag)).

\renewcommand{\arraystretch}{2}
```{r topModels}
```


```{r AUCLag, fig.cap= "The relationship between AUC and LiDAR temporal misalignment with bird surveys for each species."}
```






## American Redstart 

Occupancy probability for American Redstart increased with NDVI, the coefficient of variation of vegetation height, and the 50th percentile vegetation height, and decreased with maximum vegetation height, and the proportion of lidar returns between 2 and 4 meters high. The model built using temporally aligned covariates explained 46% of the variance in American redstart occupancy and had an AUC of 0.784. The coefficient of variation of vegetation height contributed most to predicting occupancy  (*b* = 3.149, SE = 0.691, *p* < 0.001, semi-partial $R^2$ = 0.065), followed by NDVI (*b* = 0.92, SE = 0.267, *p* < 0.001, semi-partial $R^2$ = 0.051), and maximum elevation (*b* = -3.633, SE = 0.861, *p* < 0.001, semi-partial $R^2$ = 0.046) (Table \@ref(tab:varImp)). The percentage of explained variance did not decline with temporally misaligned LiDAR. For American Redstart, temporal misalignment did not lead to a decrease in model performance as measured by AUC. 
\renewcommand{\arraystretch}{1}
```{r varImp}
```

## Black-Throated Green Warbler 
Occupancy probability for Black-Throated Green Warbler increased with maximum vegetation height and decreased with the 50th percentile of canopy height and the total of all LiDAR height returns. The model built using temporally aligned covariates explained 48% of the variance in Black-Throated Green Warbler occupancy and had an AUC of 0.799. Maximum vegetation height contributed the most to model performance (*b* = 2.743, SE = 0.559, *p* < 0.001, semi-partial $R^2$ = 0.138) followed by the 50th percentile of canopy height (*b* = -1.439, SE = 0.351, *p* < 0.001, semi-partial $R^2$ = 0.035). There was no discernible trend in the explained variance with increasing LiDAR temporal misalignment. However, we observed a significant decrease in model performance ($r$2=.28, *p*<0.05). The AUC statistic for the zero-time lag model was less than 0.7 with 14 years of LiDAR-bird survey time lag.






## Mourning Warbler 
Mourning Warbler occupancy responded positively to the percentage of first returns above the mean vegetation height, NDVI, the density of vegetation below two meters, and maximum vegetation height at low standard deviation of vegetation height. Mourning Warbler occupancy decreased with increased vegetation density between two and four meters. The model built using temporally aligned covariates explained 49% of the variance in Mourning Warbler occupancy and had an AUC of 0.782. NDVI contributed the most to model predictions (*b* = 25.81, SE = 0.354, *p* < 0.001, semi-partial $R^2$ = 0.074)followed by the proportion of vegetation returns below two meters (*b* = 0.797, SE = 0.215, *p* < 0.001, semi-partial $R^2$ = 0.028). For Mourning Warbler, we found that increased LiDAR-point count temporal misalignment led to reductions in the amount of explained variance ($r^2$=.29, *p*<.05) and model performance ($r^2$=.29, *p*<.05). AUC statistics were less than 0.7 with > 13 years of LiDAR temporal misalignment.


## Swainson's Thrush 
Swainson's Thrush occupancy probability responded non-linearly to NDVI (the probability of occupancy increased with increasing low NDVI values and decreased with higher values). Occupancy probability responded negatively to the 50th percentile of vegetation returns and positively to the maximum vegetation height. The model built using temporally aligned covariates explained 13% of the variance in Swainson's Thrush occupancy and had an AUC of 0.668. The maximum vegetation height contributed the most to model performance (*b* = 0.955, SE = 0.163, *p* < 0.001, semi-partial $R^2$ = 0.067) followed by the 50th percentile of vegetation height (*b* = -0.944, SE = 0.166, *p* < 0.001, semi-partial $R^2$ = 0.063). The percentage of explained variance did not decline with temporally misaligned LIDAR, nor was there a decrease in model performance as measured by AUC. AUC values were < 0.70 for all Swainson's Thrush models.


 

## Winter Wren 
Winter Wren occupancy was positively influenced by NDVI, the maximum vegetation height, and the percentage of first vegetation returns above the mean vegetation height. Winter Wren occupancy responded negatively to the canopy relief ratio and the density of vegetation returns between four and six meters in height. The model built using temporally aligned covariates explained 42% of the variance in Winter Wren occupancy and had an AUC of 0.696. Maximum vegetation height contributed the most to model performance (*b* = 1.024, SE = 0.27, *p* < 0.001, semi-partial $R^2$ = 0.047) followed by the canopy relief ratio (*b* = -1.666, SE = 0.518, *p* < 0.01, semi-partial $R^2$ = 0.024). The percentage of explained variance did not decline with temporally misaligned LIDAR, nor was there a significant decrease in model performance as measured by AUC. 


## White-Throated Sparrow 
The top White-Throated Sparrow model predicted that occupancy probability responds positively to NDVI, the coefficient of variation of vegetation height, and the maximum vegetation height, and negatively to the percentage of LiDAR vegetation returns above two meters. The model built using temporally aligned covariates explained 20% of the variance in White-Throated Sparrow occupancy, and had an AUC of 0.705. Maximum vegetation height contributed the most to model performance (*b* = 0.813, SE = 0.155, *p* < 0.001, semi-partial $R^2$ = 0.064) followed by NDVI (*b* =  0.477, SE = 0.151, *p* < 0.01, semi-partial $R^2$ = 0.022). There was no discernible trend in the explained variance with increasing LiDAR temporal misalignment. However, we observed a significant decrease in model performance ($r$2=0.28, *p*<0.05). AUC statistics were < 0.7 with over five years of temporal misalignment.


## Forest age 

For American Redstart and Black-throated Green Warbler, we found a significant relationship between forest age and the pixel-wise differences between predictive maps (*p*<001). Comparing the zero and fifteen year time lag models, we found that models using 15-year-old LiDAR data overestimated the occupancy probability of American Redstart in stands <25 years old. The 15-year-old LiDAR data overestimated the probability of Black-throated Green Warbler occupancy in forests of all ages (Figure \@ref(fig:scatter)). Forest age explained 23% and 30% of the variance between predictive maps for American Redstart and Black-throated Green Warbler, respectively.

```{r scatter, fig.cap= "Scatter plots of per-pixel occupancy probability for predictive distribution maps representing zero and 15 years of time-lag between LIDAR and bird data. Scatter plots are coloured according to the forest age of each mapped pixel", fig.pos="!ht", out.extra=''}
```


For Mourning Warbler, Swainson's Thrush, Winter Wren, and White-Throated Sparrow, we found significant but weak relationships between forest age and the differences between predictive maps built with increasing amounts of LiDAR-point count temporal misalignment (*p*<001). For these species, forest age explained < 5% of the variance between predictive maps. 




# Discussion

## Model performance

We found LiDAR based models were moderately predictive of occupancy probability (0.7<AUC<-0.9) for four of the six focal species: American Redstart, Black-Throated Green Warbler, Mourning Warbler, and White-throated Sparrow. The influence of LiDAR time-lag with bird observations on SDM performance varied. 
















As predicted, SDMs for Mourning Warbler, an early-successional associate, saw significant declines in model performance with increased LiDAR temporal misalignment. Mourning Warbler nest and feed near the ground in dense shrub vegetation and colonize clearings opened by forestry and oil and gas exploration [@pitocchelliMourningWarblerGeothlypis2020; @atwellSongbirdResponseExperimental2008]. As forests regenerate, and canopy closing trees replace early-successional vegetation, Mourning Warbler abundance declines (as early as 10 years post-disturbance) [@brawnRoleDisturbanceEcology2001]. The proportion of LiDAR vegetation returns below two meters, an indicator of dense shrub understory vegetation, was the LiDAR variable that contributed the most to the explained variation in Mourning Warbler occupancy. This may help explain the declines in model performance with increased LiDAR temporal misalignment. As shrub density decreases through succession, LiDAR metrics indicating shrub becomes less useful. For Mourning Warbler, we found models became less predictive (AUC < -.70) with 13 years between LIDAR and bird surveys.



Temporal misalignment also strongly influenced the performance of SDMs for the White-throated Sparrow. The White-throated Sparrow is one of the most abundant species in Alberta's boreal mixed-wood forests [@Schmiegelow1997]. They occur along forested edges, in early-successional stands or mature forest canopy gaps [@fallsWhitethroatedSparrowZonotrichia2020]. White-throated Sparrows nest and feed near the ground with low dense vegetation cover [@fallsWhitethroatedSparrowZonotrichia2020]. Similar to Mourning Warbler, it's feeding and nesting preferences likely impact the amount of acceptable time lag between LiDAR and bird survey data in predictive models because of changes in shrub layer vegetation occurring between the LiDAR acquisition and point-counts. The White-throated sparrow SDMs became less predictive (AUC < 0.70) after five years of time lag between LiDAR and point-count data.


We predicted that increasing the temporal lag between LiDAR and point-count data would lead to moderate declines in the performance of American Redstart SDMs. Our findings did not bear this out. Temporal misalignment had no discernible impact on model performance. American Redstart SDMs remained moderately predictive of occupancy with a 15 year gap between LIDAR and bird detections. American Redstart occurs in a range of successional stands and mixed-age plots [@sherryAmericanRedstartSetophaga2020a]. In Alberta, they are associated with structurally complex deciduous forests [@lestonLongtermChangesBoreal2018; @Mahon2016]. Our results support this. LiDAR measures of structural complexity were more predictive of American Redstart occupancy than other LiDAR variables. Two things may account for the American Restart models' resilience to LIDAR temporal misalignment. (1) The structurally complex, uneven-aged forests that American Redstart are associated with change over decades [@brassardStandStructureComposition2010]. The rate of change may not be captured with 15 years of time lag between LiDAR and point-counts. (2) Near Calling Lake, AB., the American Redstart decreases in abundance after harvesting [@nortonSongbirdResponsePartialcut1997]. Given that we controlled for natural and human disturbances occurring between LiDAR acquisition and bird surveys, American Redstart declines caused by harvesting were not captured by our models. 




Contrary to our predictions, we observed a significant negative influence of LiDAR temporal misalignment on the performance of Black-throated Green Warbler SDMs. However, the SDMs were still moderately predictive of occupancy with fifteen years of time-lag between LIDAR and bird surveys. Canopy height was the biggest predictor of Black-throated Green Warbler occupancy. Black-throated Green Warbler is a forest interior species associated with older deciduous and mixed-wood forests [@morseBlackthroatedGreenWarbler2020a; Mahon2016, @Schieck2006]. Declines in SDM performance were likely the result of changes in canopy height caused by natural gap opening events like individual tree fall or insect defoliation [@brassardStandStructureComposition2010]. However, canopy changes that occurred during our 15 year study period weren't large enough to reduce AUC to <0.70.




## Recommendations

We identified two studies examining the influence of temporal misalignment between LiDAR and wildlife data on the performance of species-habitat models. Vierling et al. [-@VierlingSwift2014] studied the effect of six years of LiDAR time-lag with wildlife surveys on Brown Creeper (*Certhia americana*) SDMs. They found that the six-year time-lag had only a small influence on model performance (a 5% decrease in mapped occupancy probability). Similarly, Hill and Hinsley [-@hillAirborneLidarWoodland2015] examined how LiDAR data with a time-lag of up to 11 years with field data influenced breeding habitat models for the Great Tit (*Parus major*). When comparing time-lags of one, four, and 11 years, they found only a small impact (less than 1%) on model predictions. Both studies cautiously suggested that for mature and stable forests, temporal misalignment does not play a major role in the performance of predictive bird models. Brown Creeper occupies late-successional mature forests [@poulinBrownCreeperCerthia2020] and the Great Tit is a habitat generalist [@vanbalenComparativeSudyBreeding1973]. We found similar results for Black-throated Green Warbler and American Redstart. However, our results for early-successional species suggest that, for birds strongly associated with dense shrubs and open canopies, over five years of time-lag between LiDAR and wildlife surveys may erode model performance. 



With its ability to capture vegetation structural attributes often missing from classified land-cover data, LiDAR is increasingly being used in bird studies [@Lefsky2002; @Davies2014a]. Despite the increasing availability of LiDAR, multitemporal LiDAR data remains limited  [@Lesak2011a]. Consequently, most studies modelling bird-habitat relationships include some temporal misalignment between LiDAR and bird data [@moudryRoleVegetationStructure2021]: e.g., 3 years [@hinsleyApplicationLidarWoodland2006], 4 years [@vogelerTerrainVegetationStructural2014a; @Goetz2010], 5 years [@Weisberg2014], and 10 years [@huberUsingRemotesensingData2016]. Our results suggest that temporal misalignment should be considered when applying LiDAR to bird-habitat models. For species associated primarily with mid- to late-successional boreal forests, coincident bird and LiDAR data may not be necessary. But caution should be taken with early-successional species occupying burned and harvested areas, and those that nest and feed near the ground with dense shrub vegetation. For these, we recommend limiting temporal misalignment to <5 years. If multi-temporal LiDAR is unavailable, other remote sensing may be better for characterizing post-disturbance vegetation, like time-series of spectral indices from optical satellites [@Kennedy2018] 




# Conclusion

We evaluated how time lag between LiDAR acquisitions and bird surveys influenced model robustness for early-successional, mature forest, and forest generalist birds. We found that LIDAR-based SDMs are moderately predictive of occupancy for American Redstart, Black-throated Green Warbler, Mourning Warbler, and White-throated Sparrow. The influence of temporal misalignment on SDMs varied across species with the greatest impact on models for early-successional associates. For species occupying older, more stable forests, temporal misalignment between LiDAR and bird surveys had only a small impact on the predictive power of SDMs. For early-successional birds, our findings suggest that a time difference of 5-13 years between LIDAR and bird data may reduce model performance.

\pagebreak

# References {-}

<div id="refs"></div>

\pagebreak
