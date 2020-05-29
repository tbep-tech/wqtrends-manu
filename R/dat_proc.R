# libraries
library(tidyverse)
library(wqtrends)
library(flextable)
library(officer)

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
  select(-k) %>% 
  mutate(
    value = gsub('^data/mods\\_chl|\\.RData$', '', value)
  ) %>% 
  rename(station = value)

save(modprf, file = 'data/modprf.RData', compress = 'xz')
