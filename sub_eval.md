  
# Evaluation of subset data{.tabset}

Matched to nearest two sites in each category, +/- five years.

```r
knitr::opts_chunk$set(message = F, warning = F)

library(tidyverse)
library(ggmap)
library(lubridate)
library(geosphere)
library(stringi)
library(tibble)
library(bnlearn)
library(sp)
library(sf)

data(restdat)
data(reststat)
data(wqdat)
data(wqstat)

# source R files
source('R/get_chg.R')
source('R/get_clo.R')
source('R/get_cdt.R')
source('R/get_brk.R')
source('R/get_fin.R')
source('R/get_all.R')
source('R/rnd_dat.R')

# globals
mtch <- 2
yrdf <- 5
resgrp <- 'top' 

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

## Pre-1994 data


```r
# get sub data, restoration sites
restdat_sub <- restdat %>% 
  filter(date < 1994)
reststat_sub <- reststat %>% 
  filter(id %in% restdat_sub$id)

# get conditional probability tables
allchg_pre <- get_all(restdat_sub, reststat_sub, wqdat, wqstat,
                  mtch = mtch, yrdf = yrdf, resgrp = resgrp, qts = c(0.5), lbs = c('lo', 'hi'))

toplo <- allchg_pre[[1]] %>% 
  group_by(hab, wtr, salev) %>% 
  summarize(
    chvalmd = mean(cval, na.rm = T)
    ) %>% 
  na.omit %>% 
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
  facet_wrap(~ salev, ncol = 1) + 
  coord_flip() +
  scale_y_continuous('chlorophyll', limits = c(0,15))
```

![](sub_eval_files/figure-html/unnamed-chunk-1-1.png)<!-- -->


```r
# combine restoration locations, date, type
resgrp <- 'top'
restall_sub <- left_join(restdat_sub, reststat_sub, by = 'id')
names(restall_sub)[names(restall_sub) %in% resgrp] <- 'Restoration\ngroup'

# map by restoration type
pbase +
  geom_point(data = restall_sub, aes(x = lon, y = lat, fill = `Restoration\ngroup`), size = 4, pch = 21)
```

![](sub_eval_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
# map by date
pbase +
  geom_point(data = restall_sub, aes(x = lon, y = lat, fill = factor(date)), size = 4, pch = 21)
```

![](sub_eval_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

```r
# barplot of date counts
toplo <- restall_sub %>% 
  group_by(date)
ggplot(restall_sub, aes(x = factor(date))) + 
  geom_bar() + 
  coord_flip() + 
  theme_bw() + 
  theme(
    axis.title.y = element_blank()
  ) +
  scale_y_discrete(expand = c(0, 0))
```

![](sub_eval_files/figure-html/unnamed-chunk-2-3.png)<!-- -->


```r
cdat <- allchg_pre[[2]] %>% 
  select_if(is.character) %>% 
  na.omit %>% 
  mutate_if(is.character, factor) %>% 
  data.frame

# create Network
net <- model2network("[hab][wtr][salev|hab:wtr][chlev|hab:wtr:salev]")
plot(net)
```

![](sub_eval_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#Creating CPTs from data
fittedBN <- bn.fit(net, data = cdat)
fittedBN$chlev
```

```
## 
##   Parameters of node chlev (multinomial distribution)
## 
## Conditional probability table:
##  
## , , salev = hi, wtr = wtr_aft
## 
##      hab
## chlev   hab_aft   hab_bef
##    hi 0.2500000 0.2500000
##    lo 0.7500000 0.7500000
## 
## , , salev = lo, wtr = wtr_aft
## 
##      hab
## chlev   hab_aft   hab_bef
##    hi 0.0000000 0.5000000
##    lo 1.0000000 0.5000000
## 
## , , salev = hi, wtr = wtr_bef
## 
##      hab
## chlev   hab_aft   hab_bef
##    hi 0.3636364 0.2916667
##    lo 0.6363636 0.7083333
## 
## , , salev = lo, wtr = wtr_bef
## 
##      hab
## chlev   hab_aft   hab_bef
##    hi 0.7647059 0.8750000
##    lo 0.2352941 0.1250000
```

```r
#Get inferences
cpquery(fittedBN,
        event = (chlev == "hi"),
        evidence= (hab == "hab_aft" & wtr == 'wtr_aft')
        )
```

```
## [1] 0.1608187
```


```r
ests <- unique(cdat) %>% 
  mutate(est = NA)

for(i in 1:nrow(ests)){
  
  toest <- ests[i, ]
  est <- cpquery(fittedBN,
                 event = (chlev == toest$chlev),
                 evidence= (wtr == toest$wtr & hab == toest$hab & salev == toest$salev)
  )
  
  ests[i, 'est'] <- est
  
}

ggplot(ests, aes(x = salev, y = est, fill = chlev)) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  facet_wrap(hab ~ wtr) + 
  theme_bw()
```

![](sub_eval_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## Post-1994 data


```r
# get sub data, restoration sites
restdat_sub <- restdat %>% 
  filter(date >= 1994)
reststat_sub <- reststat %>% 
  filter(id %in% restdat_sub$id)

# get conditional probability tables
allchg_pst <- get_all(restdat_sub, reststat_sub, wqdat, wqstat,
                  mtch = mtch, yrdf = yrdf, resgrp = resgrp, qts = c(0.5), lbs = c('lo', 'hi'))

# summarize
toplo <- allchg_pst[[1]] %>% 
  group_by(hab, wtr, salev) %>% 
  summarize(
    chvalmd = mean(cval, na.rm = T)
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
  facet_wrap(~ salev, ncol = 1) + 
  coord_flip() +
  scale_y_continuous('chlorophyll', limits = c(0, 15))
```

![](sub_eval_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


```r
# combine restoration locations, date, type
resgrp <- 'top'
restall_sub <- left_join(restdat_sub, reststat_sub, by = 'id')
names(restall_sub)[names(restall_sub) %in% resgrp] <- 'Restoration\ngroup'

# map by restoration type
pbase +
  geom_point(data = restall_sub, aes(x = lon, y = lat, fill = `Restoration\ngroup`), size = 4, pch = 21)
```

![](sub_eval_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
# map by date
pbase +
  geom_point(data = restall_sub, aes(x = lon, y = lat, fill = factor(date)), size = 4, pch = 21)
```

![](sub_eval_files/figure-html/unnamed-chunk-6-2.png)<!-- -->

```r
# barplot of date counts
toplo <- restall_sub %>% 
  group_by(date)
ggplot(restall_sub, aes(x = factor(date))) + 
  geom_bar() + 
  coord_flip() + 
  theme_bw() + 
  theme(
    axis.title.y = element_blank()
  ) +
  scale_y_discrete(expand = c(0, 0))
```

![](sub_eval_files/figure-html/unnamed-chunk-6-3.png)<!-- -->


```r
cdat <- allchg_pst[[2]] %>% 
  select_if(is.character) %>% 
  na.omit %>% 
  mutate_if(is.character, factor) %>% 
  data.frame

# create Network
net <- model2network("[hab][wtr][salev|hab:wtr][chlev|hab:wtr:salev]")
plot(net)
```

![](sub_eval_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
#Creating CPTs from data
fittedBN <- bn.fit(net, data = cdat)
fittedBN$chlev
```

```
## 
##   Parameters of node chlev (multinomial distribution)
## 
## Conditional probability table:
##  
## , , salev = hi, wtr = wtr_aft
## 
##      hab
## chlev   hab_aft   hab_bef
##    hi 0.2307692 0.1875000
##    lo 0.7692308 0.8125000
## 
## , , salev = lo, wtr = wtr_aft
## 
##      hab
## chlev   hab_aft   hab_bef
##    hi 0.8666667 0.8750000
##    lo 0.1333333 0.1250000
## 
## , , salev = hi, wtr = wtr_bef
## 
##      hab
## chlev   hab_aft   hab_bef
##    hi 0.2142857 0.0000000
##    lo 0.7857143 1.0000000
## 
## , , salev = lo, wtr = wtr_bef
## 
##      hab
## chlev   hab_aft   hab_bef
##    hi 0.6190476 0.8000000
##    lo 0.3809524 0.2000000
```

```r
#Get inferences
cpquery(fittedBN,
        event = (chlev == "hi"),
        evidence= (hab == "hab_aft" & wtr == 'wtr_aft')
        )
```

```
## [1] 0.4692702
```


```r
ests <- unique(cdat) %>% 
  mutate(est = NA)

for(i in 1:nrow(ests)){
  
  toest <- ests[i, ]
  est <- cpquery(fittedBN,
                 event = (chlev == toest$chlev),
                 evidence= (wtr == toest$wtr & hab == toest$hab & salev == toest$salev)
  )
  
  ests[i, 'est'] <- est
  
}

ggplot(ests, aes(x = salev, y = est, fill = chlev)) + 
  geom_bar(stat = 'identity', position = 'dodge') + 
  facet_wrap(hab ~ wtr) + 
  theme_bw()
```

![](sub_eval_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
