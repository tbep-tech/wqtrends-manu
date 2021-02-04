# libraries
library(tidyverse)
library(wqtrends)
library(mgcv)
library(flextable)
library(officer)
library(lubridate)
library(mgcv)

source('R/funcs.R')

# chlorophyll models, model S only ----------------------------------------

modssta <- rawdat %>% 
  filter(param == 'chl') %>% 
  group_by(station) %>% 
  nest() %>% 
  mutate(
    model = purrr::pmap(list(station, data), function(station, data){

      cat(station, '\n')
      out <- anlz_gam(data, trans = 'log10')
      
      return(out)
      
    })
  )

# separate models into diff files by stations
tosv <- modssta %>% 
  pull(station) %>% 
  unique

for(i in seq_along(tosv)){
  
  cat(i, 'of', length(tosv), '\n')
  
  sta <- tosv[i]
  
  fl <- modssta %>% 
    filter(station %in% !!sta) 
  
  flnm <- paste0('modslog_chl', sta)
  
  assign(flnm, fl)
  
  save(list = flnm, file = paste0('data/', flnm, '.RData'), compress = 'xz')
  
}

# model performance summaries ---------------------------------------------

modprf <- list.files('data', pattern = '^modslog\\_chl', full.names = T) %>% 
  enframe() %>% 
  group_by(value) %>% 
  nest %>% 
  mutate(
    prf = purrr::map(value, function(x){
      
      load(file = x)

      nm <- basename(x)
      nm <- gsub('\\.RData', '', nm)

      mod <- get(nm) %>% 
        pull(model) %>% 
        deframe()
      
      out <- anlz_fit(mod)
      
      return(out)
      
    })
  ) %>% 
  ungroup %>% 
  select(-data) %>% 
  unnest('prf') %>% 
  select(-AIC) %>% 
  mutate(
    value = gsub('^data/modslog\\_chl|\\.RData$', '', value)
  ) %>% 
  rename(
    station = value
  ) 

save(modprf, file = 'data/modprf.RData', compress = 'xz')

# seasonal trends by decade, jan-jun and jul-dec --------------------------

seastrnd <- list.files('data', pattern = '^modslog\\_chl', full.names = T) %>% 
  crossing(
    fl = ., 
    tibble(
      doystr = c(41, 213), 
      doyend = c(213, 338)
    ), 
    tibble(
      yrstr = c(1991, 2000, 2010),
      yrend = c(2000, 2010, 2019)
    )
  ) %>% 
  group_by(fl) %>% 
  nest() %>% 
  mutate(
    mod = purrr::map(fl, function(x){
      
      load(file = x)
      
      nm <- basename(x)
      nm <- gsub('\\.RData', '', nm)
      
      out <- get(nm) %>% 
        pull(model) %>% 
        deframe()
      
      return(out)
      
    })
  ) %>% 
  unnest(c('data')) %>% 
  group_by(doystr, doyend, mod, fl) %>% 
  nest() %>% 
  mutate(
    avgseas = purrr::pmap(list(mod = mod, doystr = doystr, doyend = doyend), anlz_avgseason)
  ) %>% 
  unnest('data') %>% 
  mutate(
    yrstr = purrr::pmap(list(yrstr, avgseas), function(yrstr, avgseas){
      max(yrstr, min(avgseas$yr))
    }),
    yrend = purrr::pmap(list(yrend, avgseas), function(yrend, avgseas){
      min(yrend, max(avgseas$yr))
    }),
    metatrnd = purrr::pmap(list(avgseason = avgseas, yrstr = yrstr, yrend = yrend), anlz_mixmeta)
  ) %>% 
  mutate(
    yrcoef = purrr::pmap(list(mod, metatrnd), function(mod, metatrnd){

      dispersion <- summary(mod)$dispersion
      bt_prd <- 10 ^ (predict(metatrnd) + log(10) * dispersion / 2)
      df <- data.frame(chl = bt_prd, yr = metatrnd$model$yr)
      slope <- lm(chl ~ yr, df) %>% summary %>% coefficients %>% .[2, 1]
        
      return(slope)
      
      }
    ), 
    pval = purrr::map(metatrnd, function(x){
      coefficients(summary(x)) %>% data.frame %>% .[2, 4]
      }
    )
  ) %>% 
  ungroup %>% 
  select(station = fl, seas = doystr, yrs = yrstr, yrcoef, pval) %>% 
  mutate(
    station = gsub('^data/modslog\\_chl|\\.RData$', '', station),
    seas = factor(seas, levels = c('41', '213'), labels = c('Jan-Jun', 'Jul-Dec')), 
    yrs = case_when(
      yrs < 1995 ~ '1991-2000', 
      yrs >=1995 & yrs < 2005 ~ '2000-2010', 
      yrs >= 2005 ~ '2010-2019'
    ), 
    yrs = factor(yrs)
  ) %>% 
  unnest(c('yrcoef', 'pval'))

save(seastrnd, file = 'data/seastrnd.RData', compress = 'xz')

# moving window seasonal changes ------------------------------------------

seastrnd2 <- list.files('data', pattern = '^modslog\\_chl', full.names = T) %>% 
  crossing(
    fl = ., 
    tibble(
      doystr = c(41, 213), 
      doyend = c(213, 338)
    )
  ) %>% 
  group_by(fl) %>% 
  nest() %>% 
  mutate(
    mod = purrr::map(fl, function(x){
      
      load(file = x)
      
      nm <- basename(x)
      nm <- gsub('\\.RData', '', nm)
      
      out <- get(nm) %>% 
        pull(model) %>% 
        deframe()
      
      return(out)
      
    })
  ) %>% 
  unnest(c('data')) %>% 
  group_by(doystr, doyend, mod, fl) %>% 
  nest() %>% 
  mutate(
    res = purrr::pmap(list(fl, mod = mod, doystr, doyend), function(fl, mod, doystr, doyend){
      
      # get slope trends
      out <- anlz_trndseason(mod = mod, doystr = doystr, doyend = doyend, justify = 'center', win = 10)
      
      return(out)
      
    })
  ) %>% 
  unnest(res) %>%
  ungroup() %>% 
  select(-mod, -data) %>% 
  mutate(
    fl = gsub('^data/modslog_chl|\\.RData', '', fl) 
  ) %>%
  rename(
    station = fl
  )

save(seastrnd2, file = 'data/seastrnd2.RData', compress = 'xz')

# trend model comparisons -------------------------------------------------

cmptrnd <- list.files('data', pattern = '^modslog\\_chl', full.names = T) %>% 
  crossing(
    fl = ., 
    tibble(
      doystr = c(1, 91, 182, 274),
      doyend = c(90, 181, 273, 364)
      # doystr = c(41, 213), 
      # doyend = c(213, 338)
    ), 
    tibble(
      yrstr = c(1991, 2000, 2010),
      yrend = c(2000, 2010, 2019)
    )
  ) %>% 
  group_by(fl) %>% 
  nest() %>% 
  mutate(
    mod = purrr::map(fl, function(x){
      
      load(file = x)
      
      nm <- basename(x)
      nm <- gsub('\\.RData', '', nm)
      
      out <- get(nm) %>% 
        pull(model) %>% 
        deframe()
      
      return(out)
      
    })
  ) %>% 
  unnest(c('data')) %>% 
  group_by(doystr, doyend, mod, fl) %>% 
  nest() %>% 
  mutate(
    avgseas = purrr::pmap(list(mod = mod, doystr = doystr, doyend = doyend), anlz_avgseason)
  ) %>% 
  unnest('data') %>% 
  mutate(
    yrstr = purrr::pmap(list(yrstr, avgseas), function(yrstr, avgseas){
      max(yrstr, min(avgseas$yr))
    }), 
    yrend = purrr::pmap(list(yrend, avgseas), function(yrend, avgseas){
      min(yrend, max(avgseas$yr))
    }),
    metatrnd = purrr::pmap(list(avgseason = avgseas, yrstr = yrstr, yrend = yrend), anlz_mixmeta),
    lmtrnd = purrr::pmap(list(avgseas, yrstr, yrend), function(avgseas, yrstr, yrend){
      
      out <- avgseas %>% 
        filter(yr >= yrstr & yr <= yrend) %>% 
        lm(avg ~ yr, .)
      
      return(out)
      
    }), 
    obstrnd = purrr::pmap(list(doystr, doyend, mod, yrstr, yrend), function(doystr, doyend, mod, yrstr, yrend){
      
      moddat <- mod$model %>% 
        mutate(
          yr = floor(cont_year), 
          doy = yday(date_decimal(cont_year)),
        ) %>% 
        filter(doy >= doystr & doy <= doyend) %>% 
        filter(yr >= yrstr & yr <= yrend) %>% 
        group_by(yr) %>%
        summarise(value = mean(value, na.rm = T), .groups = 'drop') 
      out <- lm(value ~ yr, moddat)
      
      return(out)
      
    })
  ) %>% 
  ungroup %>% 
  select(-avgseas) %>% 
  gather('modtyp', 'est', metatrnd, lmtrnd, obstrnd) %>% 
  mutate(
    yrcoef = purrr::pmap(list(mod, modtyp, est), function(mod, modtyp, est){
      
      if(modtyp %in% c('metatrnd', 'lmtrnd')){
        
        dispersion <- summary(mod)$dispersion
        bt_prd <- 10 ^ (predict(est) + log(10) * dispersion / 2)
        df <- data.frame(chl = bt_prd, yr = est$model$yr)
        slope <- lm(chl ~ yr, df) %>% summary %>% coefficients %>% .[2, 1]
        
      }
      
      if(modtyp %in% 'obstrnd'){
        
        s2 <- (summary(est)$sigma)^2
        bt_prd <- 10 ^ (predict(est) + log(10) * s2 / 2)
        df <- data.frame(chl = bt_prd, yr = est$model$yr)
        slope <- lm(chl ~ yr, df) %>% summary %>% coefficients %>% .[2, 1]
        
      }
      
      return(slope)
      
    }),
    pval = purrr::map(est, function(x){
      
      if(inherits(x, 'list'))
        x <- x[[1]]
      
      coefficients(summary(x)) %>% data.frame %>% .[2, 4]
      
    })
  ) %>% 
  select(station = fl, seas = doystr, yrs = yrstr, modtyp, yrcoef, pval) %>% 
  mutate(
    station = gsub('^data/modslog\\_chl|\\.RData$', '', station),
    seas = factor(seas, levels = c('1', '91', '182', '274'), labels = c('Jan-Mar', 'Apr-Jun', 'Jul-Sep', 'Oct-Dec')),
    yrs = case_when(
      yrs < 1995 ~ '1990-2000', 
      yrs >=1995 & yrs < 2005 ~ '2000-2010', 
      yrs >= 2005 ~ '2010-2019'
    ), 
    yrs = factor(yrs), 
    modtyp = factor(modtyp, levels = c('obstrnd', 'lmtrnd', 'metatrnd'), labels = c('Observed', 'Average', 'Mixed-meta')), 
    pval = ifelse(pval <= 0.05, 'p < 0.05', 'ns')
  ) %>% 
  unnest('yrcoef') %>% 
  mutate(station = factor(station, levels = rev(unique(station))))

save(cmptrnd, file = 'data/cmptrnd.RData', compress = 'xz')

# model structure comparisons ---------------------------------------------

tomod <- rawdat %>%
  filter(station %in% 32) %>%
  filter(param %in% 'chl')

# get transformation
moddat <- anlz_trans(tomod, trans = 'log10')

# frms <- c(
#   'S' = "value ~ s(cont_year, k = 360)",  
#   'SY' = "value ~ cont_year + s(cont_year, k = 360)",
#   'SYD' = "value ~ cont_year + s(cont_year, k = 360) + s(doy, bs = 'cc', k = 360)",
#   'SYDI' = "value ~ cont_year + s(cont_year, k = ) + s(doy, bs = 'cc', k = 90) + ti(cont_year, doy, bs = c('tp', 'cc'), k = 90)"
# ) 

S <- gam(value ~ s(cont_year, k = 360),
         knots = list(doy = c(1, 366)),
         data = moddat,
         na.action = na.exclude,
         select = F)
S$trans <- 'log10'
SY <- gam(value ~ cont_year + s(cont_year, k = 360),
          knots = list(doy = c(1, 366)),
          data = moddat,
          na.action = na.exclude,
          select = F)
SY$trans <- 'log10'
SYD <- gam(value ~ cont_year + s(cont_year, k = 360) + s(doy, bs = 'cc', k = 230),
           knots = list(doy = c(1, 366)),
           data = moddat,
           na.action = na.exclude,
           select = F)
SYD$trans <- 'log10'
SYDI <- gam(value ~ cont_year + s(cont_year, k = 300) + s(doy, bs = 'cc', k = 105) + ti(cont_year, doy, bs = c('tp', 'cc'), k = 15),
            knots = list(doy = c(1, 366)),
            data = moddat,
            na.action = na.exclude,
            select = F)
SYDI$trans <- 'log10'

modstr <- list(
  S = S, 
  SY = SY,
  SYD = SYD, 
  SYDI = SYDI
)

# lapply(modstr, anlz_fit)

save(modstr, file = 'data/modstr.RData', compress = 'xz')
