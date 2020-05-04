

* Introduction
     * What are needs/objectives of long-term trend analysis
          * What can influence water quality change and how can it appear in time series?
          * Identify needs of environmental managers vs describing system dynamics
     * Importance of describing seasonal changes
          * Describe bloom phenology in major systems
          * Why do shifts occur, e.g., species invasion, climate change, etc. and what are implications
          * Need to understand if the change is non-random and communicate certainty of results (e.g., WWTP upgrades can involve billion-dollar decisions)
     * Existing methods for trend analysis - scope and limitations
          * Seasonal kendall or other non-parametric (STL decomposition) - only direction and magnitude, assumes monotonic
          * non-linear, loess - does not describe a functional form and is just a local fit
          * WRTDS - includes functional form but is local, requires window width selection, computationally intense
          * balance tradeoffs with objectives of the analysis or who needs the information
     * GAMs as method for trend analysis
          * Application to environmenetal data (lit review) - air quality and water quality applications
          * Use in management context
          * Overview of Murphy et al. 2019, Yang and Moyer 2020
          * Limitations of existing studies
     * San Francisco Bay NMS story - increase in South Bay, need to see if increase is 'significant'
     * Study objectives: 
          * Present methods and modification of GAMs as applied to SF South Bay
          * Quantify relative model performance for explaning long-term trends
          * Present secondary methods for trend analysis with GAM results, including estimates of seasonal change

* Methods
     * Study area - SF south Bay dataset, USGS monitoring data, show observed data summer/fall IQR plots (is the trend significant? when did it start or stop being significant?)
     * GAM general format and structures (gam0, gam1, gam2, gam6)
     * Model application to SF time series and model evaluation, including transformation for response variables (why Box-Cox?)
     * Model predictions (complete time series, annual trend) and back-transformation estimates
     * Secondary methods for trend analysis, estimate of seasonal values, hypothesis tests including mixed meta-analysis
     * Sensitivity analysis - do trend estimates or hypothesis test results change by model type? 
     
* Results - key tables/figures
     * Tables
         * GAM structures (table 3.1 in tech memo)
         * Water quality characteristics by stations (annual/seasonal averages for chl, do, nutrients, etc.)
         * Model performance summary statistics for each GAM by station
         * Summary table of trends
     * Figures
         * Study area map with stations
         * summer/fall observed IQR chlorophyll data in South Bay
         * GAM predicted time series for each station or representative stations
         * Example plots of trend assessments from GAM results - annual/by season
         * Summary maps of trends - annual/by season
         
* Discussion
     * Emphasis on adaptability/flexibility of the approach that builds off Chesapeake
     * Methods developed to address stakeholder questions but has broader importance beyond SF
     * Which gam structure is best, highlight gam2 v gam6 at station 18 vs station 32 and longitudinal change along axis
     * Ecological interpretation, maybe link to GAM applications to other variables (e.g., DO, GPP)
     * Trend assessment - what second stage analysis adds to the GAM approach, possibly brief discussion about how the management need would influence how trends are assessed
     * Limitations and follow-up analysis
         * Did not address serial correlation (e.g., Yang and Moyer 2020), but not a huge issue for monthly LTM data
         * Did not incorporate flow-normalization
         * Addition of explanatory predictors
     * Highlight web tools - Shiny dashboard and R package
     * Conclusions
