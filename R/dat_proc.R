# libraries
library(tidyverse)
library(wqtrends)
library(mgcv)
library(flextable)
library(officer)
library(lubridate)

source('R/funcs.R')

# chlorophyll models, model S only ----------------------------------------

modssta <- rawdat %>% 
  filter(param == 'chl') %>% 
  group_by(station) %>% 
  nest() %>% 
  mutate(
    model = purrr::pmap(list(station, data), function(station, data){
      
      cat(station, '\n')
      moddat <- anlz_trans(data, trans = 'log10')
      out <- gam(value ~ s(cont_year, k = 360),
        knots = list(doy = c(1, 366)),
        data = moddat,
        na.action = na.exclude,
        select = F)
      
      out$trans <- 'log10'
      
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
      
      dat <- get(nm) %>% 
        select(model) %>% 
        deframe()
      
      out <- anlz_fit(mods = dat)
      
      return(out)
      
    })
  ) %>% 
  ungroup %>% 
  select(-data) %>% 
  unnest('prf') %>% 
  select(-AIC, -k, -F, -p.value, -value) %>% 
  rename(
    station = model
  ) 

save(modprf, file = 'data/modprf.RData', compress = 'xz')

# seasonal trends by decade -----------------------------------------------

seastrnd <- list.files('data', pattern = '^modslog\\_chl', full.names = T) %>% 
  crossing(
    fl = ., 
    tibble(
      doystr = c(1, 91, 182, 274), 
      doyend = c(90, 181, 273, 364)
    ), 
    tibble(
      yrstr = c(1990, 2000, 2010),
      yrend = c(1999, 2009, 2017)
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
        pull(model)
      
      return(out)
      
    })
  ) %>% 
  unnest(c('data')) %>% 
  group_by(doystr, doyend, fl, mod) %>%
  nest() %>%
  mutate(
    avgseas = purrr::pmap(list(mods = mod, doystr, doyend), anlz_avgseason)
  ) %>% 
  unnest('data') %>% 
  mutate(
    yrstr = purrr::pmap(list(yrstr, avgseas), function(yrstr, avgseas){
      max(yrstr, min(avgseas$yr))
    }), 
    yrend = purrr::pmap(list(yrend, avgseas), function(yrend, avgseas){
      min(yrend, max(avgseas$yr))
    }),
    metatrnd = purrr::pmap(list(avgseas, yrstr, yrend), anlz_mixmeta)
  ) %>% 
  mutate(
    yrcoef = purrr::map(metatrnd, function(x) x[[1]]$coefficients['yr']), 
    pval = purrr::map(metatrnd, function(x){
      coefficients(summary(x[[1]])) %>% data.frame %>% .[2, 4]
    }
    )
  ) %>% 
  ungroup %>% 
  select(station = fl, seas = doystr, yrs = yrstr, yrcoef, pval) %>% 
  mutate(
    station = gsub('^data/modslog\\_chl|\\.RData$', '', station),
    seas = factor(seas, levels = c('1', '91', '182', '274'), labels = c('JFM', 'AMJ', 'JAS', 'OND')), 
    yrs = case_when(
      yrs < 1995 ~ '1990-2000', 
      yrs >=1995 & yrs < 2005 ~ '2000-2010', 
      yrs >= 2005 ~ '2010-2016'
    ), 
    yrs = factor(yrs)
  ) %>% 
  unnest(c('yrcoef', 'pval'))

save(seastrnd, file = 'data/seastrnd.RData', compress = 'xz')

# seasonal trends by decade, jan-jul and aug-dec --------------------------

seastrnd2 <- list.files('data', pattern = '^modslog\\_chl', full.names = T) %>% 
  crossing(
    fl = ., 
    tibble(
      doystr = c(41, 214), 
      doyend = c(213, 338)
    ), 
    tibble(
      yrstr = c(1991, 2000, 2010),
      yrend = c(1999, 2009, 2016)
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
        pull(model)
      
      return(out)
      
    })
  ) %>% 
  unnest(c('data')) %>% 
  group_by(doystr, doyend, mod, fl) %>% 
  nest() %>% 
  mutate(
    avgseas = purrr::pmap(list(mods = mod, doystr, doyend), anlz_avgseason)
  ) %>% 
  unnest('data') %>% 
  mutate(
    yrstr = purrr::pmap(list(yrstr, avgseas), function(yrstr, avgseas){
      max(yrstr, min(avgseas$yr))
    }), 
    yrend = purrr::pmap(list(yrend, avgseas), function(yrend, avgseas){
      min(yrend, max(avgseas$yr))
    }),
    metatrnd = purrr::pmap(list(avgseas, yrstr, yrend), anlz_mixmeta)
  ) %>% 
  mutate(
    yrcoef = purrr::map(metatrnd, function(x) x[[1]]$coefficients['yr']), 
    pval = purrr::map(metatrnd, function(x){
      coefficients(summary(x[[1]])) %>% data.frame %>% .[2, 4]
    }
    )
  ) %>% 
  ungroup %>% 
  select(station = fl, seas = doystr, yrs = yrstr, yrcoef, pval) %>% 
  mutate(
    station = gsub('^data/modslog\\_chl|\\.RData$', '', station),
    seas = factor(seas, levels = c('41', '214'), labels = c('Jan-Jul', 'Aug-Dec')), 
    yrs = case_when(
      yrs < 1995 ~ '1990-2000', 
      yrs >=1995 & yrs < 2005 ~ '2000-2010', 
      yrs >= 2005 ~ '2010-2016'
    ), 
    yrs = factor(yrs)
  ) %>% 
  unnest(c('yrcoef', 'pval'))

save(seastrnd2, file = 'data/seastrnd2.RData', compress = 'xz')

# decadal percent changes -------------------------------------------------

chgtrnd <- list.files('data', pattern = '^modslog\\_chl', full.names = T) %>% 
  crossing(
    fl = ., 
    tibble(
      yrstr = c(1990, 2000, 2010),
      yrend = c(1999, 2009, 2017)
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
        pull(model)
      
      return(out)
      
    })
  ) %>% 
  unnest(c('data')) %>% 
  mutate(
    perchg = purrr::pmap(list(mods = mod, baseyr = yrstr, testyr = yrend), anlz_perchg)
  ) %>% 
  select(-mod) %>% 
  unnest('perchg') %>% 
  ungroup %>% 
  select(station = fl, yrs = yrstr, perchg, pval) %>% 
  mutate(
    station = gsub('^data/modslog\\_chl|\\.RData$', '', station),
    yrs = case_when(
      yrs < 1995 ~ '1990, 2000', 
      yrs >=1995 & yrs < 2005 ~ '2000, 2010', 
      yrs >= 2005 ~ '2010, 2016'
    ), 
    yrs = factor(yrs), 
    persgn = sign(perchg), 
    persgn = factor(persgn, levels = c('1', '-1'), labels = c('inc', 'dec')), 
    pval = ifelse(pval < 0.05, 'sig', 'ns')
  ) 

save(chgtrnd, file = 'data/chgtrnd.RData', compress = 'xz')

# trend model comparisons -------------------------------------------------

cmptrnd <- list.files('data', pattern = '^modslog\\_chl', full.names = T) %>% 
  crossing(
    fl = ., 
    tibble(
      # doystr = c(1, 91, 182, 274), 
      # doyend = c(90, 181, 273, 364)
      doystr = c(41, 214), 
      doyend = c(213, 338)
    ), 
    tibble(
      yrstr = c(1990, 2000, 2010),
      yrend = c(1999, 2009, 2017)
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
        pull(model)
      
      return(out)
      
    })
  ) %>% 
  unnest(c('data')) %>% 
  group_by(doystr, doyend, mod, fl) %>% 
  nest() %>% 
  mutate(
    avgseas = purrr::pmap(list(mods = mod, doystr, doyend), anlz_avgseason)
  ) %>% 
  unnest('data') %>% 
  mutate(
    yrstr = purrr::pmap(list(yrstr, avgseas), function(yrstr, avgseas){
      max(yrstr, min(avgseas$yr))
    }), 
    yrend = purrr::pmap(list(yrend, avgseas), function(yrend, avgseas){
      min(yrend, max(avgseas$yr))
    }),
    metatrnd = purrr::pmap(list(avgseas, yrstr, yrend), anlz_mixmeta),
    lmtrnd = purrr::pmap(list(avgseas, yrstr, yrend), function(avgseas, yrstr, yrend){
      out <- avgseas %>% 
        filter(yr >= yrstr & yr <= yrend) %>% 
        lm(predicted ~ yr, .)
      return(out)
    }), 
    obstrnd = purrr::pmap(list(doystr, doyend, mod, yrstr, yrend), function(doystr, doyend, mod, yrstr, yrend){

      moddat <- mod[[1]]$model %>% 
        mutate(
          yr = floor(cont_year), 
          doy = yday(date_decimal(cont_year)),
          value = 10 ^ value
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
  select(-mod, -avgseas) %>% 
  gather('mod', 'est', metatrnd, lmtrnd, obstrnd) %>% 
  mutate(
    yrcoef = purrr::pmap(list(mod, est), function(mod, est){
      
      if(mod == 'metatrnd')
        est <- est[[1]]
      
      est$coefficients[[2]]
      
    }),
    pval = purrr::map(est, function(x){
      
      if(inherits(x, 'list'))
        x <- x[[1]]
      
      coefficients(summary(x)) %>% data.frame %>% .[2, 4]
      
    })
  ) %>% 
  select(station = fl, seas = doystr, yrs = yrstr, mod, yrcoef, pval) %>% 
  mutate(
    station = gsub('^data/modslog\\_chl|\\.RData$', '', station),
    seas = factor(seas, levels = c('41', '214'), labels = c('Jan-Jul', 'Aug-Dec')),
    yrs = case_when(
      yrs < 1995 ~ '1990-2000', 
      yrs >=1995 & yrs < 2005 ~ '2000-2010', 
      yrs >= 2005 ~ '2010-2016'
    ), 
    yrs = factor(yrs), 
    mod = factor(mod, levels = c('obstrnd', 'lmtrnd', 'metatrnd'), labels = c('Observed', 'Average', 'Mixed-meta')), 
    pval = ifelse(pval <= 0.05, 'p < 0.05', 'ns')
  ) %>% 
  unnest('yrcoef') %>% 
  mutate(station = factor(station, levels = rev(unique(station))))

save(cmptrnd, file = 'data/cmptrnd.RData', compress = 'xz')
