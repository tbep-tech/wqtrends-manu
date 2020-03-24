* Murphy et al. 2019. A Generalized Additive Model approach to evaluating water quality: Chesapeake Bay case study [link](https://doi.org/10.1016/j.envsoft.2019.03.027)
    * Suggest looking at Box/Cox methods (study area and data section)
    * page 7, in refernece to larger k values "In other applications, however, a higher amount of variability may be necessary, perhaps where climatic patterns that vary dramatically year-to-year are the driver of the change being evaluated."

Haraguchi et al. 2015. Long-term changes of the phytoplankton community and biomass in the subtropical shallow Patos Lagoon Estuary, Brazil [link](https://doi.org/10.1016/j.ecss.2015.03.007)
    * Evaluated long-term changes in phytoplankton (20 years) in a shallow lagoon
    * Used GAMs to remove salinity from time series, which was a substantial component explaining the trends, both lagoon outflow and oceanic inflows
    * Isolated long-term climatic signal from ENSO after removing salinity effect from time series

Riemann et al. 2016. Recovery of Danish Coastal Ecosystems After Reductions in Nutrient Loading: A Holistic Ecosystem Approach [link](https://link.springer.com/article/10.1007%2Fs12237-015-9980-0)
    * Evaluated long-term recovery of Danish waters by evaluating trends in 25 years of data following management actions for large-scale nutrient reductions
    * Used GAMs to evaluate "changes over time in the different ecosystem components", using annual means for the same years in order to compare different responses to nutrient reductions
    * This paper is minimal in its use of GAMs, see Fig 9 
    
Harding et al. 2016. Long-Term Trends of Nutrients and Phytoplankton in Chesapeake Bay [link](https://link.springer.com/article/10.1007%2Fs12237-015-0023-7)
    * Used GAMs and GAMMs to generate flow-adjusted time series and compute long-term trends accounting for climate effects on hydrology
Testa et al. 2018
    * Documented a eutrophication period followed by partial reverseal of nutrient over-enrichment, could make parallel with south SF Bay
    * Tested lag effects with GAMMs by including an autoregressoin term (as noted as future needs in Murphy et al.2019)
    * Fitting GAMs used default upper limit on knots, flow-normalized was based on holding SRF constant (also differing from MUrphy et al. 2019)
    * GAMs also fit to annual means

Lefcheck et al. 2017. Multiple stressors threaten the imperiled coastal foundation species eelgrass (Zostera marina) in Chesapeake Bay, USA [link](https://onlinelibrary.wiley.com/doi/abs/10.1111/gcb.13623)
    * Documented decline in seagrass coverage in Chesapeake Bay using 31 years of data to elucidate patterns and drivers of abundance
    * significant predictors of eelgrass cover were identified with GAMMs, random region/year effects
    * Used lagged smoother to account for temporal correlation
    * GAMMs fit by depth strata had differnet signicant predictors
    
Richards et al. 2013 Using generalized additive models for water quality assessments: A case study example from Australia [link](https://doi.org/10.2112/SI65-020.1)
    * Suggests extensive application of GAMs to air quality studies
    * Used GAMs to identify significant predictors of turbidity along the coastal region of the Gold Coast, Australia

Richards et al. 2010. Using Generalized Additive Models to assess, explore, and unify environmental monitoring datasets [link](https://scholarsarchive.byu.edu/cgi/viewcontent.cgi?article=2629&context=iemssconference)
    * Proceedings paper demonstrating three case studies using GAMs to explore coastal datasets
    * Case one focuses on "unifying" findings of a long-term program with those of a short-term, case two is evaluating spatial patterns in a biomonitoring dataset, and case three evaluates 12 months of continuous monitoring data of oceanographic conditions (third case study is focus of Richards et al. 2013)
    * Applications use GAMs and GAMMs, where the latter used an autoregressive component

Qiao et al. 2017. Long-term changes in nutrients, chlorophyll a and their relationships in a semi-enclosed eutrophic ecosystem, Bohai Bay, China [link](https://linkinghub.elsevier.com/retrieve/pii/S0025326X17301170)
    * Evaluated long-term changes in nutrient concentrations, nutrient ratios, chlorophyll and responses of chlorophyll to changes in nutrients in the spring and summer
    * Need to evaluate how seasonal differences were analyzed