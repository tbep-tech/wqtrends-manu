# libraries
library(tidyverse)
library(wqtrends)
library(flextable)
library(officer)

source('R/funcs.R')

# model performance summaries ---------------------------------------------

modprf <- list.files('data', pattern = '^mods\\_chl', full.names = T) %>% 
  enframe() %>% 
  group_by(value) %>% 
  nest %>% 
  mutate(
    prf = purrr::map(value, function(x){
      
      load(file = x)
      
      nm <- basename(x)
      nm <- gsub('\\.RData', '', nm)
      
      dat <- get(nm) %>% 
        select(model, modi) %>% 
        deframe()
      
      prf <- anlz_fit(mods = dat)
      
      return(prf)
      
    })
  ) %>% 
  ungroup %>%
  select(-data) %>% 
  unnest('prf') %>% 
  select(-k, -F) %>% 
  mutate(
    value = gsub('^data/mods\\_chl|\\.RData$', '', value), 
    p.value = p_ast(p.value), 
    p.value = ifelse(is.na(p.value), '-', p.value)
  ) %>% 
  rename(
    station = value, 
    `P-value` = p.value
    )

save(modprf, file = 'data/modprf.RData', compress = 'xz')

# seasonal trends by decade -----------------------------------------------

seastrnd <- list.files('data', pattern = '^mods\\_chl', full.names = T) %>% 
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
        filter(model == 'gam6') %>% 
        select(model, modi) %>% 
        deframe()
      
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
    station = gsub('^data/mods\\_chl|\\.RData$', '', station),
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

