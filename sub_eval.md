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
library(scales)

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
qts <- c(0.5)
lbs <- c('lo', 'hi')

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

## Distance to restoration sites
wqmtch <- get_clo(restdat_sub, reststat_sub, wqstat, resgrp = resgrp, mtch = mtch)

## Summarizing effects of restoration projects on salinity
sachg <- get_chg(wqdat, wqmtch, statdat, restdat, wqvar = 'sal', yrdf = yrdf) %>% 
  rename(saval = cval)

## Summarizing effects of restoration projects on chl
chchg <- get_chg(wqdat, wqmtch, statdat, restdat, wqvar = 'chla', yrdf = yrdf) %>% 
  rename(chval = cval)

# combine all using a super simple approach
cdat_pre <- sachg %>% 
  left_join(chchg, by = c('stat', 'hab', 'wtr')) %>% 
  mutate(
    salev = cut(saval, breaks = c(-Inf, quantile(saval, qts, na.rm = T), Inf), labels = lbs),
    chlev = cut(chval, breaks = c(-Inf, quantile(chval, qts, na.rm = T), Inf), labels = lbs)
  ) %>% 
  # dplyr::select(-saval, -nival, -chval, -stat) %>% 
  mutate_if(is.character, factor) %>% 
  data.frame

toplo <- cdat_pre %>% 
  group_by(hab, wtr, salev) %>% 
  summarize(
    chvalmd = mean(chval, na.rm = T)
    ) %>% 
  na.omit %>% 
  unite('rest', hab, wtr, sep = ', ')

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
cdat_pre_mod <- cdat_pre %>% 
  select_if(is.factor) %>% 
  na.omit %>% 
  data.frame

# create Network
net <- model2network("[hab][wtr][salev|hab:wtr][chlev|hab:wtr:salev]")
plot(net)
```

![](sub_eval_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
#Creating CPTs from data
fittedBN <- bn.fit(net, data = cdat_pre_mod)
```


```r
# get conditional probabilities of lo/md/hi chl for bef/aft of each project type
ests <- cdat_pre_mod %>% 
  dplyr::select(-salev) %>% 
  mutate_if(is.factor, as.character) %>% 
  gather('project', 'event', -chlev) %>% 
  unique %>% 
  mutate(est = NA)

for(i in 1:nrow(ests)){
  
  toest <- ests[i, ]
  est <- paste0('cpquery(fittedBN, event = (chlev == toest$chlev), evidence = (', toest$project, ' == "', toest$event, '"))')
  est <- eval(parse(text = est))
  ests[i, 'est'] <- est
  
}

# format for plot
ests <- ests %>% 
  mutate(event = gsub('^wtr\\_|^hab\\_', '', event)) %>% 
  mutate(chlev = factor(chlev, levels = lbs)) %>% 
  spread(event, est) %>% 
  mutate(
    chg = 100 * (aft - bef),
    chg = round(chg, 1)
    )

ggplot(ests, aes(x = chlev, y = project, fill = chg)) +
  geom_tile(colour = 'black') +
  geom_text(aes(label = chg, size = abs(chg))) +
  scale_size(guide = F) +
  theme_bw(base_family = 'serif') +
  scale_x_discrete('Chlorophyll', expand = c(0, 0)) + 
  scale_y_discrete('Restoration project', expand = c(0, 0)) +
  scale_fill_gradient2('% change', low = muted("red"), mid = "white", high = muted("blue"), midpoint = 0)
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

## Distance to restoration sites
wqmtch <- get_clo(restdat_sub, reststat_sub, wqstat, resgrp = resgrp, mtch = mtch)

## Summarizing effects of restoration projects on salinity
sachg <- get_chg(wqdat, wqmtch, statdat, restdat, wqvar = 'sal', yrdf = yrdf) %>% 
  rename(saval = cval)

## Summarizing effects of restoration projects on chl
chchg <- get_chg(wqdat, wqmtch, statdat, restdat, wqvar = 'chla', yrdf = yrdf) %>% 
  rename(chval = cval)

# combine all using a super simple approach
cdat_pst <- sachg %>% 
  left_join(chchg, by = c('stat', 'hab', 'wtr')) %>% 
  mutate(
    salev = cut(saval, breaks = c(-Inf, quantile(saval, qts, na.rm = T), Inf), labels = lbs),
    chlev = cut(chval, breaks = c(-Inf, quantile(chval, qts, na.rm = T), Inf), labels = lbs)
  ) %>% 
  # dplyr::select(-saval, -nival, -chval, -stat) %>% 
  mutate_if(is.character, factor) %>% 
  data.frame

toplo <- cdat_pst %>% 
  group_by(hab, wtr, salev) %>% 
  summarize(
    chvalmd = mean(chval, na.rm = T)
    ) %>% 
  na.omit %>% 
  unite('rest', hab, wtr, sep = ', ')

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
cdat_pst_mod <- cdat_pst %>% 
  select_if(is.factor) %>% 
  na.omit %>% 
  data.frame

# create Network
net <- model2network("[hab][wtr][salev|hab:wtr][chlev|hab:wtr:salev]")
plot(net)
```

![](sub_eval_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
#Creating CPTs from data
fittedBN <- bn.fit(net, data = cdat_pst_mod)
```


```r
# get conditional probabilities of lo/md/hi chl for bef/aft of each project type
ests <- cdat_pst_mod %>% 
  dplyr::select(-salev) %>% 
  mutate_if(is.factor, as.character) %>% 
  gather('project', 'event', -chlev) %>% 
  unique %>% 
  mutate(est = NA)

for(i in 1:nrow(ests)){
  
  toest <- ests[i, ]
  est <- paste0('cpquery(fittedBN, event = (chlev == toest$chlev), evidence = (', toest$project, ' == "', toest$event, '"))')
  est <- eval(parse(text = est))
  ests[i, 'est'] <- est
  
}

# format for plot
ests <- ests %>% 
  mutate(event = gsub('^wtr\\_|^hab\\_', '', event)) %>% 
  mutate(chlev = factor(chlev, levels = lbs)) %>% 
  spread(event, est) %>% 
  mutate(
    chg = 100 * (aft - bef),
    chg = round(chg, 1)
    )

ggplot(ests, aes(x = chlev, y = project, fill = chg)) +
  geom_tile(colour = 'black') +
  geom_text(aes(label = chg, size = abs(chg))) +
  scale_size(guide = F) +
  theme_bw(base_family = 'serif') +
  scale_x_discrete('Chlorophyll', expand = c(0, 0)) + 
  scale_y_discrete('Restoration project', expand = c(0, 0)) +
  scale_fill_gradient2('% change', low = muted("red"), mid = "white", high = muted("blue"), midpoint = 0)
```

![](sub_eval_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
