# GAM applications for water quality status and trends {.tabset .tabset-pills}

## Yang and Moyer 2020

Estimation of nonlinear water-quality trends in high-frequency monitoring data [link](https://doi.org/10.1016/j.scitotenv.2020.136686)

* Close examination of serial correlation issue with high-frequency monitoring data using GAMs, also evaluated trend estimation (maybe same as Murphy?) and slope change detection at arithmetic scales, dealing with back-transformation bias
* Evaluated water temp, turbidity, and specific conductance, focusing mostly on identifying drivers of change
* Followed similar setup as Murphy et al. developing GAMs with increasing complexity, time and other wq variables as predictors
* Also evaluated effects of changing k on trend estimates and fit, concluding that the "exact k-value is not that essential for GAM trend estimation" (p. 10)

## Murphy et al. 2019

A Generalized Additive Model approach to evaluating water quality: Chesapeake Bay case study [link](https://doi.org/10.1016/j.envsoft.2019.03.027)
    
* Suggest looking at Box/Cox methods (study area and data section)
* page 7, in refernece to larger k values "In other applications, however, a higher amount of variability may be necessary, perhaps where climatic patterns that vary dramatically year-to-year are the driver of the change being evaluated."

## Testa et al. 2018

Nutrient- and Climate-Induced Shifts in the Phenology of Linked Biogeochemical Cycles in a Temperate Estuary. [link](https://doi.org/10.3389/fmars.2018.00114)

* Evaluated phenological/seasonal shifts in biogeochemical cycles in Chesapeake Bay using 32 year record
* Used GAMs to model daily observations of response variables with smoother functions for decimal date, day of year, salinity, and different ti terms.  A lagged fixed year effect was also added. 
* smooths with p values > 0.1 were dropped
* Usec box-cox methods for transformation

## Beck and Murphy 2017

Numerical and Qualitative Contrasts of Two Statistical Models for Water Quality Change in Tidal Waters [link](https://doi.org/10.1111/1752-1688.12489)

* Compared ability of GAMs and WRTDS to describe long-term trends in chlorophyll time series, including ability to estimate flow-normalized components
* Results were similar between the two, but computional requirements for GAMs are much less
* Followed similar format for GAM as that described in Murphy et al. 2019

## Qiao et al. 2017

Long-term changes in nutrients, chlorophyll a and their relationships in a semi-enclosed eutrophic ecosystem, Bohai Bay, China [link](https://linkinghub.elsevier.com/retrieve/pii/S0025326X17301170)

* Evaluated long-term changes in nutrient concentrations, nutrient ratios, chlorophyll and responses of chlorophyll to changes in nutrients in the spring and summer
* Need to evaluate how seasonal differences were analyzed
    
## Lefcheck et al. 2017

Multiple stressors threaten the imperiled coastal foundation species eelgrass (Zostera marina) in Chesapeake Bay, USA [link](https://onlinelibrary.wiley.com/doi/abs/10.1111/gcb.13623)

* Documented decline in seagrass coverage in Chesapeake Bay using 31 years of data to elucidate patterns and drivers of abundance
* significant predictors of eelgrass cover were identified with GAMMs, random region/year effects
* Used lagged smoother to account for temporal correlation
* GAMMs fit by depth strata had differnet signicant predictors

## Riemann et al. 2016

Recovery of Danish Coastal Ecosystems After Reductions in Nutrient Loading: A Holistic Ecosystem Approach [link](https://link.springer.com/article/10.1007%2Fs12237-015-9980-0)

* Evaluated long-term recovery of Danish waters by evaluating trends in 25 years of data following management actions for large-scale nutrient reductions
* Used GAMs to evaluate "changes over time in the different ecosystem components", using annual means for the same years in order to compare different responses to nutrient reductions
* This paper is minimal in its use of GAMs, see Fig 9 
    
## Harding et al. 2016

Long-Term Trends of Nutrients and Phytoplankton in Chesapeake Bay [link](https://link.springer.com/article/10.1007%2Fs12237-015-0023-7)

* Used GAMs and GAMMs to generate flow-adjusted time series and compute long-term trends accounting for climate effects on hydrology
* Documented a eutrophication period followed by partial reverseal of nutrient over-enrichment, could make parallel with south SF Bay
* Tested lag effects with GAMMs by including an autoregressoin term (as noted as future needs in Murphy et al.2019)
* Fitting GAMs used default upper limit on knots, flow-normalized was based on holding SRF constant (also differing from MUrphy et al. 2019)
* GAMs also fit to annual means

## Haraguchi et al. 2015

Long-term changes of the phytoplankton community and biomass in the subtropical shallow Patos Lagoon Estuary, Brazil [link](https://doi.org/10.1016/j.ecss.2015.03.007)

* Evaluated long-term changes in phytoplankton (20 years) in a shallow lagoon
* Used GAMs to remove salinity from time series, which was a substantial component explaining the trends, both lagoon outflow and oceanic inflows
* Year and month used as categorical variables, saliniyt as continuous in GAM to compare with other water quality parameters
* Isolated long-term climatic signal from ENSO after removing salinity effect from time series

## Richards et al 2013

Using generalized additive models for water quality assessments: A case study example from Australia [link](https://doi.org/10.2112/SI65-020.1)

* Suggests extensive application of GAMs to air quality studies
* Used GAMs to identify significant predictors of turbidity along the coastal region of the Gold Coast, Australia

## Richards et al. 2010

Using Generalized Additive Models to assess, explore, and unify environmental monitoring datasets [link](https://scholarsarchive.byu.edu/cgi/viewcontent.cgi?article=2629&context=iemssconference)

* Proceedings paper demonstrating three case studies using GAMs to explore coastal datasets
* Case one focuses on "unifying" findings of a long-term program with those of a short-term, case two is evaluating spatial patterns in a biomonitoring dataset, and case three evaluates 12 months of continuous monitoring data of oceanographic conditions (third case study is focus of Richards et al. 2013)
* Applications use GAMs and GAMMs, where the latter used an autoregressive component

## Morton and Henderson 2008 

Estimation of nonlinear trends in water quality: an improved approach using generalized additive models [link](https://doi.org/10.1029/2007WR006191)

* Advocates for use of GAMs to evaluate non-linear trends in the presence of serially-correlated errors
* Compares GAMs with Seasonal Kendall and robust regression
* Also includes methods for adjustment to seasonal or flow effects
* Provides citations for use of GAMs in air quality studies  

## Wan et al. 2017

Decadal and seasonal trends of nutrient concentration and export from highly managed coastal catchments [link](https://doi.org/10.1016/j.watres.2017.02.068)

* Uses seasonal-trend decomposition and LOESS to evaluate trends of nutrient concentrations and export over a 35 year period in South Florida
* Emphasis on chemostatic and chemodynamic trends where the former indicates that export is independent of climate/precipitation patterns and latter is the opposite, often linked to anthropogenic sources
