  
# Evaluation of randomized data {.tabset}


```r
knitr::opts_chunk$set(message = F, warning = F)

library(tidyverse)
library(ggmap)
library(lubridate)
library(geosphere)
library(stringi)
library(tibble)
library(raster)
library(sp)
library(rgdal)
library(foreach)
library(doParallel)

data(restdat)
data(reststat)
data(wqdat)
data(wqstat)
data(tbpoly)
data(allchg)

# source R files
source('R/get_chg.R')
source('R/get_clo.R')
source('R/get_cdt.R')
source('R/get_brk.R')
source('R/get_fin.R')
source('R/get_all.R')
source('R/rnd_dat.R')

# Set parameters, yr half-window for matching, mtch is number of closest matches
yrdf <- 5
mtch <- 10

# number of random iterations
n <- 100

# total random sites to create
tot <- nrow(restdat)

# base map
ext <- make_bbox(reststat$lon, reststat$lat, f = 0.1)
map <- get_stamenmap(ext, zoom = 10, maptype = "toner-lite")
pbase <- ggmap(map) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  )
```

## Actual data


```r
# summarize observed
toplo <- allchg %>% 
  group_by(hab, wtr, salev) %>% 
  summarise(
    chvalmd = median(chval, na.rm = T),
    chvallo = chvalmd, 
    chvalhi = chvalmd
  ) %>% 
  unite('rest', hab, wtr, sep = ', ') %>% 
  mutate(
    salev = factor(salev, levels = c('lo', 'md', 'hi')), 
    dat = 'Observed'
  )

# plot
ggplot(toplo, aes(x = rest, y = chvalmd)) + 
  theme_bw() + 
  theme(
    axis.title.y = element_blank()
  ) +
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin = chvallo, ymax = chvalhi)) + 
  facet_wrap(~ salev, ncol = 1) + 
  coord_flip() +
  scale_y_continuous('chlorophyll', limits = c(0, 15))
```

![](rnd_eval_files/figure-html/unnamed-chunk-1-1.png)<!-- -->


```r
# combine restoration locations, date, type
resgrp <- 'top'
restall <- left_join(restdat, reststat, by = 'id')
names(restall)[names(restall) %in% resgrp] <- 'Restoration\ngroup'

# map by restoration type
pbase +
  geom_point(data = restall, aes(x = lon, y = lat, fill = `Restoration\ngroup`), size = 4, pch = 21)
```

![](rnd_eval_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
# map by date
pbase +
  geom_point(data = restall, aes(x = lon, y = lat, fill = factor(date)), size = 4, pch = 21)
```

![](rnd_eval_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

```r
# barplot of date counts
toplo <- restall %>% 
  group_by(date)
ggplot(restall, aes(x = factor(date))) + 
  geom_bar() + 
  coord_flip() + 
  theme_bw() + 
  theme(
    axis.title.y = element_blank()
  ) +
  scale_y_discrete(expand = c(0, 0))
```

![](rnd_eval_files/figure-html/unnamed-chunk-2-3.png)<!-- -->

## Same locations, same dates, random project types


```r
# setup parallel
ncores <- detectCores() - 1
registerDoParallel(cores = ncores)
strt <- Sys.time()

# run
allchg_fft <- foreach(i = 1:n, .packages = c('stringi', 'tidyr', 'dplyr', 'raster', 'sp', 'rgeos', 'tibble', 'rgdal', 'purrr', 'geosphere')) %dopar% {

  sink('log.txt')
  cat('Log entry time', as.character(Sys.time()), '\n')
  cat(i, ' of ', n, '\n')
  print(Sys.time() - strt)
  sink()
  
  # get random data
  rnd <- rnd_dat(restdat, reststat, tbpoly, loc = F, dts = F, prj = T)
  restdat_rnd <- rnd$restdat_rnd
  reststat_rnd <- rnd$reststat_rnd
  
  # run all conditional prob functions
  allchg <- get_all(restdat_rnd, reststat_rnd, wqdat, wqstat, mtch = mtch, yrdf = yrdf, resgrp = 'top', qts = c(0.33, 0.66), 
                    lbs = c('lo', 'md', 'hi'), 'hab', 'wtr')
  
  out <- allchg %>% 
    group_by(hab, wtr, salev) %>% 
    summarize(cval = mean(cval))
  
  out
  
}

save(allchg_fft, file = 'data/allchg_fft.RData', compress = 'xz')
```


```r
data(allchg_fft)

# summarize random
toplo <- allchg_fft %>% 
  enframe('iter') %>% 
  unnest %>% 
  group_by(hab, wtr, salev) %>% 
  summarise(
    chvalmd = median(cval, na.rm = T), #t.test(cval, na.rm = T)$estimate,
    chvallo = quantile(cval, probs = 0.05, na.rm = T), # t.test(cval, na.rm = T),$conf.int[1],
    chvalhi = quantile(cval, probs = 0.95, na.rm = T) #t.test(cval, na.rm = T),$conf.int[2]
  ) %>% 
  ungroup %>% 
  unite('rest', hab, wtr, sep = ', ') %>% 
  mutate(
    salev = factor(salev, levels = c('lo', 'md', 'hi'))
  ) 

# plot
ggplot(toplo, aes(x = rest, y = chvalmd)) + 
  theme_bw() + 
  theme(
    axis.title.y = element_blank()
  ) +
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin = chvallo, ymax = chvalhi)) + 
  facet_wrap( ~ salev, ncol = 1) + 
  coord_flip() +
  scale_y_continuous('chlorophyll', limits = c(0, 15))
```

![](rnd_eval_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


```r
# get random data
rnd <- rnd_dat(restdat, reststat, tbpoly, loc = F, dts = F, prj = T)
restdat_rnd <- rnd$restdat_rnd
reststat_rnd <- rnd$reststat_rnd

# combine restoration locations, date, type
resgrp <- 'top'
restall_rnd <- left_join(restdat_rnd, reststat_rnd, by = 'id')
names(restall_rnd)[names(restall_rnd) %in% resgrp] <- 'Restoration\ngroup'

# map by restoration type
pbase +
  geom_point(data = restall_rnd, aes(x = lon, y = lat, fill = `Restoration\ngroup`), size = 4, pch = 21)
```

![](rnd_eval_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
# map by date
pbase +
  geom_point(data = restall_rnd, aes(x = lon, y = lat, fill = factor(date)), size = 4, pch = 21)
```

![](rnd_eval_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

```r
# barplot of date counts
toplo <- restall_rnd %>% 
  group_by(date)
ggplot(restall_rnd, aes(x = factor(date))) + 
  geom_bar() + 
  coord_flip() + 
  theme_bw() + 
  theme(
    axis.title.y = element_blank()
  ) +
  scale_y_discrete(expand = c(0, 0))
```

![](rnd_eval_files/figure-html/unnamed-chunk-5-3.png)<!-- -->

## Same locations, random dates, same project types


```r
# setup parallel
ncores <- detectCores() - 1
registerDoParallel(cores = ncores)
strt <- Sys.time()

# run
allchg_ftf <- foreach(i = 1:n, .packages = c('stringi', 'tidyr', 'dplyr', 'raster', 'sp', 'rgeos', 'tibble', 'rgdal', 'purrr', 'geosphere')) %dopar% {

  sink('log.txt')
  cat('Log entry time', as.character(Sys.time()), '\n')
  cat(i, ' of ', n, '\n')
  print(Sys.time() - strt)
  sink()
  
  # get random data
  rnd <- rnd_dat(restdat, reststat, tbpoly, loc = F, dts = T, prj = F)
  restdat_rnd <- rnd$restdat_rnd
  reststat_rnd <- rnd$reststat_rnd
  
  # run all conditional prob functions
  allchg <- get_all(restdat_rnd, reststat_rnd, wqdat, wqstat, mtch = mtch, yrdf = yrdf, resgrp = 'top', qts = c(0.33, 0.66), 
                    lbs = c('lo', 'md', 'hi'), 'hab', 'wtr')
  
  out <- allchg %>% 
    group_by(hab, wtr, salev) %>% 
    summarize(cval = mean(cval))
  
  out
  
}

save(allchg_ftf, file = 'data/allchg_ftf.RData', compress = 'xz')
```


```r
data(allchg_ftf)

# summarize random
toplo <- allchg_ftf %>% 
  enframe('iter') %>% 
  unnest %>% 
  group_by(hab, wtr, salev) %>% 
  summarise(
    chvalmd = median(cval, na.rm = T), #t.test(cval, na.rm = T)$estimate,
    chvallo = quantile(cval, probs = 0.05, na.rm = T), # t.test(cval, na.rm = T),$conf.int[1],
    chvalhi = quantile(cval, probs = 0.95, na.rm = T) #t.test(cval, na.rm = T),$conf.int[2]
  ) %>% 
  ungroup %>% 
  unite('rest', hab, wtr, sep = ', ') %>% 
  mutate(
    salev = factor(salev, levels = c('lo', 'md', 'hi'))
  ) 

# plot
ggplot(toplo, aes(x = rest, y = chvalmd)) + 
  theme_bw() + 
  theme(
    axis.title.y = element_blank()
  ) +
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin = chvallo, ymax = chvalhi)) + 
  facet_wrap( ~ salev, ncol = 1) + 
  coord_flip() +
  scale_y_continuous('chlorophyll', limits = c(0, 15))
```

![](rnd_eval_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


```r
# get random data
rnd <- rnd_dat(restdat, reststat, tbpoly, loc = F, dts = T, prj = F)
restdat_rnd <- rnd$restdat_rnd
reststat_rnd <- rnd$reststat_rnd

# combine restoration locations, date, type
resgrp <- 'top'
restall_rnd <- left_join(restdat_rnd, reststat_rnd, by = 'id')
names(restall_rnd)[names(restall_rnd) %in% resgrp] <- 'Restoration\ngroup'

# map by restoration type
pbase +
  geom_point(data = restall_rnd, aes(x = lon, y = lat, fill = `Restoration\ngroup`), size = 4, pch = 21)
```

![](rnd_eval_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

```r
# map by date
pbase +
  geom_point(data = restall_rnd, aes(x = lon, y = lat, fill = factor(date)), size = 4, pch = 21)
```

![](rnd_eval_files/figure-html/unnamed-chunk-8-2.png)<!-- -->

```r
# barplot of date counts
toplo <- restall_rnd %>% 
  group_by(date)
ggplot(restall_rnd, aes(x = factor(date))) + 
  geom_bar() + 
  coord_flip() + 
  theme_bw() + 
  theme(
    axis.title.y = element_blank()
  ) +
  scale_y_discrete(expand = c(0, 0))
```

![](rnd_eval_files/figure-html/unnamed-chunk-8-3.png)<!-- -->

## Random locations, same dates, same project types


```r
# setup parallel
ncores <- detectCores() - 1
registerDoParallel(cores = ncores)
strt <- Sys.time()

# run
allchg_tff <- foreach(i = 1:n, .packages = c('stringi', 'tidyr', 'dplyr', 'raster', 'sp', 'rgeos', 'tibble', 'rgdal', 'purrr', 'geosphere')) %dopar% {

  sink('log.txt')
  cat('Log entry time', as.character(Sys.time()), '\n')
  cat(i, ' of ', n, '\n')
  print(Sys.time() - strt)
  sink()
  
  # get random data
  rnd <- rnd_dat(restdat, reststat, tbpoly, loc = T, dts = F, prj = F)
  restdat_rnd <- rnd$restdat_rnd
  reststat_rnd <- rnd$reststat_rnd
  
  # run all conditional prob functions
  allchg <- get_all(restdat_rnd, reststat_rnd, wqdat, wqstat, mtch = mtch, yrdf = yrdf, resgrp = 'top', qts = c(0.33, 0.66), 
                    lbs = c('lo', 'md', 'hi'), 'hab', 'wtr')
  
  out <- allchg %>% 
    group_by(hab, wtr, salev) %>% 
    summarize(cval = mean(cval))
  
  out
  
}

save(allchg_tff, file = 'data/allchg_tff.RData', compress = 'xz')
```


```r
data(allchg_tff)

# summarize random
toplo <- allchg_tff %>% 
  enframe('iter') %>% 
  unnest %>% 
  group_by(hab, wtr, salev) %>% 
  summarise(
    chvalmd = median(cval, na.rm = T), #t.test(cval, na.rm = T)$estimate,
    chvallo = quantile(cval, probs = 0.05, na.rm = T), # t.test(cval, na.rm = T),$conf.int[1],
    chvalhi = quantile(cval, probs = 0.95, na.rm = T) #t.test(cval, na.rm = T),$conf.int[2]
  ) %>% 
  ungroup %>% 
  unite('rest', hab, wtr, sep = ', ') %>% 
  mutate(
    salev = factor(salev, levels = c('lo', 'md', 'hi'))
  ) 

# plot
ggplot(toplo, aes(x = rest, y = chvalmd)) + 
  theme_bw() + 
  theme(
    axis.title.y = element_blank()
  ) +
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin = chvallo, ymax = chvalhi)) + 
  facet_wrap( ~ salev, ncol = 1) + 
  coord_flip() +
  scale_y_continuous('chlorophyll', limits = c(0, 15))
```

![](rnd_eval_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


```r
# get random data
rnd <- rnd_dat(restdat, reststat, tbpoly, loc = T, dts = F, prj = F)
restdat_rnd <- rnd$restdat_rnd
reststat_rnd <- rnd$reststat_rnd

# combine restoration locations, date, type
resgrp <- 'top'
restall_rnd <- left_join(restdat_rnd, reststat_rnd, by = 'id')
names(restall_rnd)[names(restall_rnd) %in% resgrp] <- 'Restoration\ngroup'

# map by restoration type
pbase +
  geom_point(data = restall_rnd, aes(x = lon, y = lat, fill = `Restoration\ngroup`), size = 4, pch = 21)
```

![](rnd_eval_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
# map by date
pbase +
  geom_point(data = restall_rnd, aes(x = lon, y = lat, fill = factor(date)), size = 4, pch = 21)
```

![](rnd_eval_files/figure-html/unnamed-chunk-11-2.png)<!-- -->

```r
# barplot of date counts
toplo <- restall_rnd %>% 
  group_by(date)
ggplot(restall_rnd, aes(x = factor(date))) + 
  geom_bar() + 
  coord_flip() + 
  theme_bw() + 
  theme(
    axis.title.y = element_blank()
  ) +
  scale_y_discrete(expand = c(0, 0))
```

![](rnd_eval_files/figure-html/unnamed-chunk-11-3.png)<!-- -->

## Random locations, random dates, same project types


```r
# setup parallel
ncores <- detectCores() - 1
registerDoParallel(cores = ncores)
strt <- Sys.time()

# run
allchg_ttf <- foreach(i = 1:n, .packages = c('stringi', 'tidyr', 'dplyr', 'raster', 'sp', 'rgeos', 'tibble', 'rgdal', 'purrr', 'geosphere')) %dopar% {

  sink('log.txt')
  cat('Log entry time', as.character(Sys.time()), '\n')
  cat(i, ' of ', n, '\n')
  print(Sys.time() - strt)
  sink()
  
  # get random data
  rnd <- rnd_dat(restdat, reststat, tbpoly, loc = T, dts = T, prj = F)
  restdat_rnd <- rnd$restdat_rnd
  reststat_rnd <- rnd$reststat_rnd
  
  # run all conditional prob functions
  allchg <- get_all(restdat_rnd, reststat_rnd, wqdat, wqstat, mtch = mtch, yrdf = yrdf, resgrp = 'top', qts = c(0.33, 0.66), 
                    lbs = c('lo', 'md', 'hi'), 'hab', 'wtr')
  
  out <- allchg %>% 
    group_by(hab, wtr, salev) %>% 
    summarize(cval = mean(cval))
  
  out
  
}

save(allchg_ttf, file = 'data/allchg_ttf.RData', compress = 'xz')
```


```r
data(allchg_ttf)

# summarize random
toplo <- allchg_ttf %>% 
  enframe('iter') %>% 
  unnest %>% 
  group_by(hab, wtr, salev) %>% 
  summarise(
    chvalmd = median(cval, na.rm = T), #t.test(cval, na.rm = T)$estimate,
    chvallo = quantile(cval, probs = 0.05, na.rm = T), # t.test(cval, na.rm = T),$conf.int[1],
    chvalhi = quantile(cval, probs = 0.95, na.rm = T) #t.test(cval, na.rm = T),$conf.int[2]
  ) %>% 
  ungroup %>% 
  unite('rest', hab, wtr, sep = ', ') %>% 
  mutate(
    salev = factor(salev, levels = c('lo', 'md', 'hi'))
  ) 

# plot
ggplot(toplo, aes(x = rest, y = chvalmd)) + 
  theme_bw() + 
  theme(
    axis.title.y = element_blank()
  ) +
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin = chvallo, ymax = chvalhi)) + 
  facet_wrap( ~ salev, ncol = 1) + 
  coord_flip() +
  scale_y_continuous('chlorophyll', limits = c(0, 15))
```

![](rnd_eval_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


```r
# get random data
rnd <- rnd_dat(restdat, reststat, tbpoly, loc = T, dts = T, prj = F)
restdat_rnd <- rnd$restdat_rnd
reststat_rnd <- rnd$reststat_rnd

# combine restoration locations, date, type
resgrp <- 'top'
restall_rnd <- left_join(restdat_rnd, reststat_rnd, by = 'id')
names(restall_rnd)[names(restall_rnd) %in% resgrp] <- 'Restoration\ngroup'

# map by restoration type
pbase +
  geom_point(data = restall_rnd, aes(x = lon, y = lat, fill = `Restoration\ngroup`), size = 4, pch = 21)
```

![](rnd_eval_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

```r
# map by date
pbase +
  geom_point(data = restall_rnd, aes(x = lon, y = lat, fill = factor(date)), size = 4, pch = 21)
```

![](rnd_eval_files/figure-html/unnamed-chunk-14-2.png)<!-- -->

```r
# barplot of date counts
toplo <- restall_rnd %>% 
  group_by(date)
ggplot(restall_rnd, aes(x = factor(date))) + 
  geom_bar() + 
  coord_flip() + 
  theme_bw() + 
  theme(
    axis.title.y = element_blank()
  ) +
  scale_y_discrete(expand = c(0, 0))
```

![](rnd_eval_files/figure-html/unnamed-chunk-14-3.png)<!-- -->

## Random locations, same dates, random project types


```r
# setup parallel
ncores <- detectCores() - 1
registerDoParallel(cores = ncores)
strt <- Sys.time()

# run
allchg_tft <- foreach(i = 1:n, .packages = c('stringi', 'tidyr', 'dplyr', 'raster', 'sp', 'rgeos', 'tibble', 'rgdal', 'purrr', 'geosphere')) %dopar% {

  sink('log.txt')
  cat('Log entry time', as.character(Sys.time()), '\n')
  cat(i, ' of ', n, '\n')
  print(Sys.time() - strt)
  sink()
  
  # get random data
  rnd <- rnd_dat(restdat, reststat, tbpoly, loc = T, dts = F, prj = T)
  restdat_rnd <- rnd$restdat_rnd
  reststat_rnd <- rnd$reststat_rnd
  
  # run all conditional prob functions
  allchg <- get_all(restdat_rnd, reststat_rnd, wqdat, wqstat, mtch = mtch, yrdf = yrdf, resgrp = 'top', qts = c(0.33, 0.66), 
                    lbs = c('lo', 'md', 'hi'), 'hab', 'wtr')
  
  out <- allchg %>% 
    group_by(hab, wtr, salev) %>% 
    summarize(cval = mean(cval))
  
  out
  
}

save(allchg_tft, file = 'data/allchg_tft.RData', compress = 'xz')
```


```r
data(allchg_tft)

# summarize random
toplo <- allchg_tft %>% 
  enframe('iter') %>% 
  unnest %>% 
  group_by(hab, wtr, salev) %>% 
  summarise(
    chvalmd = median(cval, na.rm = T), #t.test(cval, na.rm = T)$estimate,
    chvallo = quantile(cval, probs = 0.05, na.rm = T), # t.test(cval, na.rm = T),$conf.int[1],
    chvalhi = quantile(cval, probs = 0.95, na.rm = T) #t.test(cval, na.rm = T),$conf.int[2]
  ) %>% 
  ungroup %>% 
  unite('rest', hab, wtr, sep = ', ') %>% 
  mutate(
    salev = factor(salev, levels = c('lo', 'md', 'hi'))
  ) 

# plot
ggplot(toplo, aes(x = rest, y = chvalmd)) + 
  theme_bw() + 
  theme(
    axis.title.y = element_blank()
  ) +
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin = chvallo, ymax = chvalhi)) + 
  facet_wrap( ~ salev, ncol = 1) + 
  coord_flip() +
  scale_y_continuous('chlorophyll', limits = c(0, 15))
```

![](rnd_eval_files/figure-html/unnamed-chunk-16-1.png)<!-- -->


```r
# get random data
rnd <- rnd_dat(restdat, reststat, tbpoly, loc = T, dts = F, prj = T)
restdat_rnd <- rnd$restdat_rnd
reststat_rnd <- rnd$reststat_rnd

# combine restoration locations, date, type
resgrp <- 'top'
restall_rnd <- left_join(restdat_rnd, reststat_rnd, by = 'id')
names(restall_rnd)[names(restall_rnd) %in% resgrp] <- 'Restoration\ngroup'

# map by restoration type
pbase +
  geom_point(data = restall_rnd, aes(x = lon, y = lat, fill = `Restoration\ngroup`), size = 4, pch = 21)
```

![](rnd_eval_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

```r
# map by date
pbase +
  geom_point(data = restall_rnd, aes(x = lon, y = lat, fill = factor(date)), size = 4, pch = 21)
```

![](rnd_eval_files/figure-html/unnamed-chunk-17-2.png)<!-- -->

```r
# barplot of date counts
toplo <- restall_rnd %>% 
  group_by(date)
ggplot(restall_rnd, aes(x = factor(date))) + 
  geom_bar() + 
  coord_flip() + 
  theme_bw() + 
  theme(
    axis.title.y = element_blank()
  ) +
  scale_y_discrete(expand = c(0, 0))
```

![](rnd_eval_files/figure-html/unnamed-chunk-17-3.png)<!-- -->

## Same locations, random dates, random project types


```r
# setup parallel
ncores <- detectCores() - 1
registerDoParallel(cores = ncores)
strt <- Sys.time()

# run
allchg_ftt <- foreach(i = 1:n, .packages = c('stringi', 'tidyr', 'dplyr', 'raster', 'sp', 'rgeos', 'tibble', 'rgdal', 'purrr', 'geosphere')) %dopar% {

  sink('log.txt')
  cat('Log entry time', as.character(Sys.time()), '\n')
  cat(i, ' of ', n, '\n')
  print(Sys.time() - strt)
  sink()
  
  # get random data
  rnd <- rnd_dat(restdat, reststat, tbpoly, loc = F, dts = T, prj = T)
  restdat_rnd <- rnd$restdat_rnd
  reststat_rnd <- rnd$reststat_rnd
  
  # run all conditional prob functions
  allchg <- get_all(restdat_rnd, reststat_rnd, wqdat, wqstat, mtch = mtch, yrdf = yrdf, resgrp = 'top', qts = c(0.33, 0.66), 
                    lbs = c('lo', 'md', 'hi'), 'hab', 'wtr')
  
  out <- allchg %>% 
    group_by(hab, wtr, salev) %>% 
    summarize(cval = mean(cval))
  
  out
  
}

save(allchg_ftt, file = 'data/allchg_ftt.RData', compress = 'xz')
```


```r
data(allchg_ftt)

# summarize random
toplo <- allchg_ftt %>% 
  enframe('iter') %>% 
  unnest %>% 
  group_by(hab, wtr, salev) %>% 
  summarise(
    chvalmd = median(cval, na.rm = T), #t.test(cval, na.rm = T)$estimate,
    chvallo = quantile(cval, probs = 0.05, na.rm = T), # t.test(cval, na.rm = T),$conf.int[1],
    chvalhi = quantile(cval, probs = 0.95, na.rm = T) #t.test(cval, na.rm = T),$conf.int[2]
  ) %>% 
  ungroup %>% 
  unite('rest', hab, wtr, sep = ', ') %>% 
  mutate(
    salev = factor(salev, levels = c('lo', 'md', 'hi'))
  ) 

# plot
ggplot(toplo, aes(x = rest, y = chvalmd)) + 
  theme_bw() + 
  theme(
    axis.title.y = element_blank()
  ) +
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin = chvallo, ymax = chvalhi)) + 
  facet_wrap( ~ salev, ncol = 1) + 
  coord_flip() +
  scale_y_continuous('chlorophyll', limits = c(0, 15))
```

![](rnd_eval_files/figure-html/unnamed-chunk-19-1.png)<!-- -->


```r
# get random data
rnd <- rnd_dat(restdat, reststat, tbpoly, loc = F, dts = T, prj = T)
restdat_rnd <- rnd$restdat_rnd
reststat_rnd <- rnd$reststat_rnd

# combine restoration locations, date, type
resgrp <- 'top'
restall_rnd <- left_join(restdat_rnd, reststat_rnd, by = 'id')
names(restall_rnd)[names(restall_rnd) %in% resgrp] <- 'Restoration\ngroup'

# map by restoration type
pbase +
  geom_point(data = restall_rnd, aes(x = lon, y = lat, fill = `Restoration\ngroup`), size = 4, pch = 21)
```

![](rnd_eval_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

```r
# map by date
pbase +
  geom_point(data = restall_rnd, aes(x = lon, y = lat, fill = factor(date)), size = 4, pch = 21)
```

![](rnd_eval_files/figure-html/unnamed-chunk-20-2.png)<!-- -->

```r
# barplot of date counts
toplo <- restall_rnd %>% 
  group_by(date)
ggplot(restall_rnd, aes(x = factor(date))) + 
  geom_bar() + 
  coord_flip() + 
  theme_bw() + 
  theme(
    axis.title.y = element_blank()
  ) +
  scale_y_discrete(expand = c(0, 0))
```

![](rnd_eval_files/figure-html/unnamed-chunk-20-3.png)<!-- -->

## Random locations, random dates, random project types


```r
# setup parallel
ncores <- detectCores() - 1
registerDoParallel(cores = ncores)
strt <- Sys.time()

# run
allchg_ttt <- foreach(i = 1:n, .packages = c('stringi', 'tidyr', 'dplyr', 'raster', 'sp', 'rgeos', 'tibble', 'rgdal', 'purrr', 'geosphere')) %dopar% {

  sink('log.txt')
  cat('Log entry time', as.character(Sys.time()), '\n')
  cat(i, ' of ', n, '\n')
  print(Sys.time() - strt)
  sink()
  
  # get random data
  rnd <- rnd_dat(restdat, reststat, tbpoly, loc = T, dts = T, prj = T)
  restdat_rnd <- rnd$restdat_rnd
  reststat_rnd <- rnd$reststat_rnd
  
  # run all conditional prob functions
  allchg <- get_all(restdat_rnd, reststat_rnd, wqdat, wqstat, mtch = mtch, yrdf = yrdf, resgrp = 'top', qts = c(0.33, 0.66), 
                    lbs = c('lo', 'md', 'hi'), 'hab', 'wtr')
  
  out <- allchg %>% 
    group_by(hab, wtr, salev) %>% 
    summarize(cval = mean(cval))
  
  out
  
}

save(allchg_ttt, file = 'data/allchg_ttt.RData', compress = 'xz')
```


```r
data(allchg_ttt)

# summarize random
toplo <- allchg_ttt %>% 
  enframe('iter') %>% 
  unnest %>% 
  group_by(hab, wtr, salev) %>% 
  summarise(
    chvalmd = median(cval, na.rm = T), #t.test(cval, na.rm = T)$estimate,
    chvallo = quantile(cval, probs = 0.05, na.rm = T), # t.test(cval, na.rm = T),$conf.int[1],
    chvalhi = quantile(cval, probs = 0.95, na.rm = T) #t.test(cval, na.rm = T),$conf.int[2]
  ) %>% 
  ungroup %>% 
  unite('rest', hab, wtr, sep = ', ') %>% 
  mutate(
    salev = factor(salev, levels = c('lo', 'md', 'hi'))
  ) 

# plot
ggplot(toplo, aes(x = rest, y = chvalmd)) + 
  theme_bw() + 
  theme(
    axis.title.y = element_blank()
  ) +
  geom_bar(stat = 'identity') + 
  geom_errorbar(aes(ymin = chvallo, ymax = chvalhi)) + 
  facet_wrap( ~ salev, ncol = 1) + 
  coord_flip() +
  scale_y_continuous('chlorophyll', limits = c(0, 15))
```

![](rnd_eval_files/figure-html/unnamed-chunk-22-1.png)<!-- -->


```r
# get random data
rnd <- rnd_dat(restdat, reststat, tbpoly, loc = T, dts = T, prj = T)
restdat_rnd <- rnd$restdat_rnd
reststat_rnd <- rnd$reststat_rnd

# combine restoration locations, date, type
resgrp <- 'top'
restall_rnd <- left_join(restdat_rnd, reststat_rnd, by = 'id')
names(restall_rnd)[names(restall_rnd) %in% resgrp] <- 'Restoration\ngroup'

# map by restoration type
pbase +
  geom_point(data = restall_rnd, aes(x = lon, y = lat, fill = `Restoration\ngroup`), size = 4, pch = 21)
```

![](rnd_eval_files/figure-html/unnamed-chunk-23-1.png)<!-- -->

```r
# map by date
pbase +
  geom_point(data = restall_rnd, aes(x = lon, y = lat, fill = factor(date)), size = 4, pch = 21)
```

![](rnd_eval_files/figure-html/unnamed-chunk-23-2.png)<!-- -->

```r
# barplot of date counts
toplo <- restall_rnd %>% 
  group_by(date)
ggplot(restall_rnd, aes(x = factor(date))) + 
  geom_bar() + 
  coord_flip() + 
  theme_bw() + 
  theme(
    axis.title.y = element_blank()
  ) +
  scale_y_discrete(expand = c(0, 0))
```

![](rnd_eval_files/figure-html/unnamed-chunk-23-3.png)<!-- -->

