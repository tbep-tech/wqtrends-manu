library(wqtrends)
library(tidyverse)

data(rawdat)

tomod <- rawdat %>% 
  filter(station %in% 30) %>% 
  filter(param %in% 'chl')

mod <- anlz_gam(tomod, trans = 'log10')

yrstr1 <- 1990
yrend1 <- 2007
yrstr2 <- 2007
yrend2 <- 2019
doystr <- 213
doyend <- 304
  
ylab <- 'Chl-a (ug/L)'
alpha <- 1

# get predictions
prds <- anlz_prd(mod)

# get transformation
trans <- unique(prds$trans)

# raw data
tobacktrans <- mod$model %>% 
  dplyr::mutate(
    trans = mod$trans
  )

moddat <- anlz_backtrans(tobacktrans) %>% 
  dplyr::mutate(
    date = lubridate::date_decimal(cont_year), 
    date = as.Date(date)
  )

# transformation used
trans <- mod$trans

# get seasonal averages
avgseason <- anlz_avgseason(mod, doystr = doystr, doyend = doyend) 

# get mixmeta models
mixmet1 <- anlz_mixmeta(avgseason, yrstr = yrstr1, yrend = yrend1)
mixmet2 <- anlz_mixmeta(avgseason, yrstr = yrstr2, yrend = yrend2)

# title
dts <- as.Date(c(doystr, doyend), origin = as.Date("2000-12-31"))
strt <- paste(lubridate::month(dts[1], label = T, abbr = T), lubridate::day(dts[1]))
ends <- paste(lubridate::month(dts[2], label = T, abbr = T), lubridate::day(dts[2]))
ttl <- paste0('Fitted averages with 95% confidence intervals: ', strt, '-',  ends)

# plot objects
toplo1 <- avgseason

toplo2 <- data.frame(
  yr = seq(yrstr1, yrend1, length = 50)
  ) %>% 
  dplyr::mutate( 
    avg = predict(mixmet1, newdata = data.frame(yr = yr)), 
    se = predict(mixmet1, newdata = data.frame(yr = yr), se = T)[, 2], 
    bt_lwr = avg - 1.96 * se,
    bt_upr = avg + 1.96 * se,
    bt_avg = avg
  )

# subtitle info
pval1 <- coefficients(summary(mixmet1)) %>% data.frame %>% .[2, 4] %>% anlz_pvalformat()
pval2 <- coefficients(summary(mixmet2)) %>% data.frame %>% .[2, 4] %>% anlz_pvalformat()

dispersion <- summary(mod)$dispersion

# backtransform mixmeta predictions
toplo2a <- data.frame(
  yr = seq(yrstr1, yrend1, length = 50)
  ) %>% 
  dplyr::mutate( 
    avg = predict(mixmet1, newdata = data.frame(yr = yr)), 
    se = predict(mixmet1, newdata = data.frame(yr = yr), se = T)[, 2], 
    bt_lwr = 10^((avg - 1.96 * se) + log(10) * dispersion / 2),
    bt_upr = 10^((avg + 1.96 * se) + log(10) * dispersion / 2),
    bt_avg = 10^(avg + log(10) * dispersion / 2)
  )

# backtransform mixmeta predictions
toplo2b <- data.frame(
    yr = seq(yrstr2, yrend2, length = 50)
  ) %>% 
  dplyr::mutate( 
    avg = predict(mixmet2, newdata = data.frame(yr = yr)), 
    se = predict(mixmet2, newdata = data.frame(yr = yr), se = T)[, 2], 
    bt_lwr = 10^((avg - 1.96 * se) + log(10) * dispersion / 2),
    bt_upr = 10^((avg + 1.96 * se) + log(10) * dispersion / 2),
    bt_avg = 10^(avg + log(10) * dispersion / 2)
  )

# for subtitle
slope1 <- lm(bt_avg ~ yr, toplo2a) %>% summary %>% coefficients %>% .[2, 1]
slope1 <- round(slope1, 2)
logslope1 <- summary(mixmet1)$coefficients[2, c(1, 5, 6)]
logslope1 <- round(logslope1, 2)
logslope1 <- paste0(logslope1[1], ' (', logslope1[2], ', ', logslope1[3], ')')
subttl1 <- paste0('Trend from ', yrstr1, ' to ', yrend1, ': approximate slope ', slope1, ', log-slope ', logslope1, ', ', pval1)
slope2 <- lm(bt_avg ~ yr, toplo2b) %>% summary %>% coefficients %>% .[2, 1]
slope2 <- round(slope2, 2)
logslope2 <- summary(mixmet2)$coefficients[2, c(1, 5, 6)]
logslope2 <- round(logslope2, 2)
logslope2 <- paste0(logslope2[1], ' (', logslope2[2], ', ', logslope2[3], ')')
subttl2 <- paste0('Trend from ', yrstr2, ' to ', yrend2, ': approximate slope ', slope2, ', log-slope ', logslope2, ', ', pval2)


bassz <- 24
ptsz <- 3

p1 <- ggplot2::ggplot(prds, ggplot2::aes(x = date)) + 
  ggplot2::geom_point(data = moddat, ggplot2::aes(y = value), size = 2) +
  # ggplot2::geom_line(ggplot2::aes(y = value), size = 0.75, alpha = alpha) + 
  # ggplot2::geom_line(ggplot2::aes(y = annvalue), alpha = alpha, colour = 'tomato1') +
  coord_cartesian(ylim = c(0,45)) +
  ggplot2::theme_minimal(base_family = 'serif', base_size = bassz) + 
  ggplot2::theme(
    legend.position = 'top', 
    legend.title = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(), 
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  ) + 
  ggplot2::labs(
    y = ylab
  )

p2 <- ggplot2::ggplot(prds, ggplot2::aes(x = date)) + 
  # ggplot2::geom_point(data = moddat, ggplot2::aes(y = value), size = -1) +
  # ggplot2::geom_line(ggplot2::aes(y = value), size = 0.75, alpha = alpha) + 
  coord_cartesian(ylim = c(0,45)) +
  ggplot2::geom_line(ggplot2::aes(y = annvalue), alpha = alpha, colour = 'deepskyblue3', size = 1) +
  ggplot2::theme_minimal(base_family = 'serif', base_size = bassz) + 
  ggplot2::theme(
    legend.position = 'top', 
    legend.title = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(), 
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  ) + 
  ggplot2::labs(
    y = ylab
  )

# plot output
p3 <- ggplot2::ggplot(data = toplo1, ggplot2::aes(x = yr, y = bt_avg)) + 
  ggplot2::geom_point(colour = 'deepskyblue3', size = ptsz) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = bt_lwr, ymax = bt_upr), colour = 'deepskyblue3') +
  # ggplot2::geom_ribbon(data = toplo2, ggplot2::aes(ymin = bt_lwr, ymax = bt_upr), fill = 'pink', alpha = 0.4) +
  # ggplot2::geom_line(data = toplo2, color = 'pink') +
  ggplot2::theme_minimal(base_family = 'serif', base_size = bassz) + 
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(), 
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  ) +
  ggplot2::labs(
    y = ylab
  )

# plot output
p4 <- ggplot2::ggplot(data = toplo1, ggplot2::aes(x = yr, y = bt_avg)) + 
  ggplot2::geom_point(colour = 'deepskyblue3', size = ptsz) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = bt_lwr, ymax = bt_upr), colour = 'deepskyblue3') +
  ggplot2::geom_ribbon(data = toplo2a, ggplot2::aes(ymin = bt_lwr, ymax = bt_upr), fill = 'pink', alpha = 0.4) +
  ggplot2::geom_line(data = toplo2a, color = 'pink') +
  ggplot2::theme_minimal(base_family = 'serif', base_size =bassz) + 
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(), 
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  ) +
  ggplot2::labs(
    y = ylab
  )

# plot output
p5 <- ggplot2::ggplot(data = toplo1, ggplot2::aes(x = yr, y = bt_avg)) + 
  ggplot2::geom_point(colour = 'deepskyblue3', size = ptsz) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = bt_lwr, ymax = bt_upr), colour = 'deepskyblue3') +
  ggplot2::geom_ribbon(data = toplo2b, ggplot2::aes(ymin = bt_lwr, ymax = bt_upr), fill = 'pink', alpha = 0.4) +
  ggplot2::geom_line(data = toplo2b, color = 'pink') +
  ggplot2::theme_minimal(base_family = 'serif', base_size =bassz) + 
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank(), 
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank()
  ) +
  ggplot2::labs(
    y = ylab
  )

wd <- 6
hi <- 4.5

png('figs/graphabp1.png', height = hi, width = wd, family = 'serif', units = 'in', res = 300)
p1
dev.off()

png('figs/graphabp2.png', height = hi, width = wd, family = 'serif', units = 'in', res = 300)
p2
dev.off()

png('figs/graphabp3.png', height = hi, width = wd, family = 'serif', units = 'in', res = 300)
p3
dev.off()

png('figs/graphabp4.png', height = hi, width = wd, family = 'serif', units = 'in', res = 300)
p4
dev.off()

png('figs/graphabp5.png', height = hi, width = wd, family = 'serif', units = 'in', res = 300)
p5
dev.off()