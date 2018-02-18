---
output:
  html_document:
    keep_md: yes
    code_folding: hide
toc: no
self_contained: no
---
  
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
                  mtch = mtch, yrdf = yrdf, resgrp = resgrp)

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
## chlev hab_aft hab_bef
##    hi                
##    lo                
##    md                
## 
## , , salev = lo, wtr = wtr_aft
## 
##      hab
## chlev    hab_aft hab_bef
##    hi 0.50000000        
##    lo 0.25000000        
##    md 0.25000000        
## 
## , , salev = md, wtr = wtr_aft
## 
##      hab
## chlev hab_aft    hab_bef
##    hi         0.50000000
##    lo         0.25000000
##    md         0.25000000
## 
## , , salev = hi, wtr = wtr_bef
## 
##      hab
## chlev    hab_aft    hab_bef
##    hi 0.12500000 0.25000000
##    lo 0.75000000 0.50000000
##    md 0.12500000 0.25000000
## 
## , , salev = lo, wtr = wtr_bef
## 
##      hab
## chlev    hab_aft    hab_bef
##    hi 0.13043478 0.20000000
##    lo 0.47826087 0.40000000
##    md 0.39130435 0.40000000
## 
## , , salev = md, wtr = wtr_bef
## 
##      hab
## chlev    hab_aft    hab_bef
##    hi 0.25000000 0.08333333
##    lo 0.50000000 0.66666667
##    md 0.25000000 0.25000000
```

```r
#Get inferences
cpquery(fittedBN,
        event = (chlev == "hi"),
        evidence= (hab == "hab_aft" & wtr == 'wtr_aft')
        )
```

```
## [1] 0.4871795
```


```r
# 
# toeval <- allchg_pre[[2]] %>% 
#   dplyr::select(hab, wtr, chlev) %>% 
#   unique %>% 
#   na.omit %>% 
#   mutate(
#     ave = NA, 
#     std = NA
#   )
# 
# for(rw in 1:nrow(toeval)){
#   
#   
#   cpdist(fittedBN,
#         nodes = 'chlev',
#         evidence= (hab == toeval[rw, 'hab'][[1]] & wtr == toeval[rw, 'wtr'][[1]])
#         ) %>% 
#     table
#   
# }
#Get inferences
```


## Post-1994 data


```r
# get sub data, restoration sites
restdat_sub <- restdat %>% 
  filter(date >= 1994)
reststat_sub <- reststat %>% 
  filter(id %in% restdat_sub$id)

# get conditional probability tables
allchg_pst <- get_all(restdat_sub, reststat_sub, wqdat, wqstat,
                  mtch = mtch, yrdf = yrdf, resgrp = resgrp)

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
## chlev    hab_aft    hab_bef
##    hi 0.28571429 0.20000000
##    lo 0.57142857 0.60000000
##    md 0.14285714 0.20000000
## 
## , , salev = lo, wtr = wtr_aft
## 
##      hab
## chlev    hab_aft    hab_bef
##    hi 0.28571429 0.23076923
##    lo 0.47619048 0.53846154
##    md 0.23809524 0.23076923
## 
## , , salev = md, wtr = wtr_aft
## 
##      hab
## chlev    hab_aft    hab_bef
##    hi 0.23076923 0.28571429
##    lo 0.53846154 0.42857143
##    md 0.23076923 0.28571429
## 
## , , salev = hi, wtr = wtr_bef
## 
##      hab
## chlev hab_aft hab_bef
##    hi                
##    lo                
##    md                
## 
## , , salev = lo, wtr = wtr_bef
## 
##      hab
## chlev    hab_aft    hab_bef
##    hi 0.05555556 0.33333333
##    lo 0.72222222 0.44444444
##    md 0.22222222 0.22222222
## 
## , , salev = md, wtr = wtr_bef
## 
##      hab
## chlev    hab_aft    hab_bef
##    hi 0.25000000 0.27777778
##    lo 0.31250000 0.44444444
##    md 0.43750000 0.27777778
```

```r
# #Get inferences
# cpquery(fittedBN,
#         event = (chlev == "hi"),
#         evidence= (hab == "hab_aft" & wtr == 'wtr_aft' & salev == 'lo')
#         )
```
