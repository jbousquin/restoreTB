## ----message = F, warning = F, results = 'hide'--------------------------
library(tidyverse)
library(readxl)
library(ggmap)
library(lubridate)
library(geosphere)
library(stringi)
library(tibble)
library(leaflet)
knitr::knit('tbrest.Rmd', tangle = TRUE)
file.copy('tbrest.R', 'R/tbrest.R', overwrite = TRUE)
file.remove('tbrest.R')

# source R files
source('R/get_chg.R')
source('R/get_clo.R')
source('R/get_cdt.R')
source('R/get_brk.R')

## ----warning = F, message = F--------------------------------------------
# Load data
data(restdat)
data(reststat)
data(wqdat)
data(wqstat)

# Set parameters, yr half-window for matching, mtch is number of closest matches
yrdf <- 5
mtch <- 10

## ------------------------------------------------------------------------
head(restdat)

## ------------------------------------------------------------------------
head(reststat)

## ------------------------------------------------------------------------
head(wqdat)

## ------------------------------------------------------------------------
head(wqstat)

## ------------------------------------------------------------------------
wqmtch <- get_clo(restdat, reststat, wqstat, resgrp = 'top', mtch = mtch)
save(wqmtch, file = 'data/wqmtch.RData', compress = 'xz')

head(wqmtch)

## ----message = F, warning = F, fig.width = 7, fig.height = 8, eval = T----
## 
# plots

# combine lat/lon for the plot
toplo <- wqmtch %>% 
  left_join(wqstat, by = 'stat') %>% 
  left_join(reststat, by = 'id') %>% 
  rename(
    `Restoration\ngroup` = resgrp,
    `Distance (dd)` = dist
  )
    
# restoration project grouping column
resgrp <- 'top'
restall <- left_join(restdat, reststat, by = 'id')
names(restall)[names(restall) %in% resgrp] <- 'Restoration\ngroup'

# extent
ext <- make_bbox(wqstat$lon, wqstat$lat, f = 0.1)
map <- get_stamenmap(ext, zoom = 12, maptype = "toner-lite")

# base map
pbase <- ggmap(map) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank()
  ) +
  geom_point(data = restall, aes(x = lon, y = lat, fill = `Restoration\ngroup`), size = 4, pch = 21) +
  geom_point(data = wqstat, aes(x = lon, y = lat), size = 2)

# closest
toplo1 <- filter(toplo, rnk %in% 1)

pbase + 
  geom_segment(data = toplo1, aes(x = lon.x, y = lat.x, xend = lon.y, yend = lat.y, alpha = -`Distance (dd)`, linetype = `Restoration\ngroup`), size = 1)

## ----message = F, warning = F, fig.width = 7, fig.height = 8, eval = T----
# closest five percent
fvper <- max(toplo$rnk) %>% 
  `*`(0.2) %>% 
  ceiling
toplo2 <- filter(toplo, rnk %in% c(1:fvper))

pbase + 
  geom_segment(data = toplo2, aes(x = lon.x, y = lat.x, xend = lon.y, yend = lat.y, alpha = -`Distance (dd)`, linetype = `Restoration\ngroup`), size = 1)

## ----message = F, warning = F, fig.width = 7, fig.height = 8, eval = T----
# closest all combo
toplo3 <- toplo

pbase + 
  geom_segment(data = toplo3, aes(x = lon.x, y = lat.x, xend = lon.y, yend = lat.y, alpha = -`Distance (dd)`, linetype = `Restoration\ngroup`), size = 1)

## ------------------------------------------------------------------------
# dates for hab projects
restn <- restall %>% 
  select(id, date)
restall <- restall %>% 
  mutate(
    top = `Restoration\ngroup`
  )

#rest
lplo <- toplo1 %>% 
  select(stat, `Restoration\ngroup`, id, lon.x, lat.x, lat.y, lon.y) %>% 
  mutate(top = `Restoration\ngroup`) %>% 
  left_join(restn, by = 'id')
restln <- lplo %>% 
  gather('latgrp', 'lat', lat.x:lat.y) %>% 
  gather('longrp', 'lon', lon.x:lon.y)

pal <- colorFactor(c("navy", "red"), domain = c("hab", "wtr"))

leaflet(lplo) %>% 
  addTiles() %>%
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addCircleMarkers(~lon.x, ~lat.x,
    radius = 4,
    color = 'green',
    stroke = FALSE, opacity = 0.8,
    popup = ~as.character(paste('WQ', stat)), 
    group = 'Water quality'
  ) %>% 
  addCircleMarkers(data = restall, ~lon, ~lat,
    radius = 6,
    color = ~pal(top),
    stroke = FALSE, opacity = 0.8, 
    popup = ~as.character(paste(top, date)), 
    group = 'Restoration sites'
  ) %>% 
  addLayersControl(
    overlayGroups = c('Water quality', 'Restoration sites'),
    options = layersControlOptions(collapsed = FALSE)
  )

## ------------------------------------------------------------------------
salchgout <- get_chg(wqdat, wqmtch, statdat, restdat, wqvar = 'sal', yrdf = yrdf, chgout = TRUE)
salchg <- get_chg(wqdat, wqmtch, statdat, restdat, wqvar = 'sal', yrdf = yrdf)
save(salchgout, file = 'data/salchgout.RData')
save(salchg, file = 'data/salchg.RData')
head(salchgout)
head(salchg)

## ------------------------------------------------------------------------
wqcdt <- get_cdt(salchg, 'hab', 'wtr')
head(wqcdt)

## ----fig.height = 5, fig.width = 7, message = F, warning = F-------------
salbrk <- get_brk(wqcdt, qts = c(0.33, 0.66), 'hab', 'wtr')
salbrk

## ----fig.height = 5, fig.width = 7, message = F, warning = F-------------
toplo <- select(wqcdt, -data, -crv) %>% 
  unnest
ggplot(toplo, aes(x = cval, y = cumest)) + 
  geom_line() + 
  geom_segment(data = salbrk, aes(x = qts, y = 0, xend = qts, yend = brk)) +
  geom_segment(data = salbrk, aes(x = min(toplo$cval), y = brk, xend = qts, yend = brk)) +
  facet_grid(hab ~ wtr) +
  theme_bw()

## ----eval = T, fig.height = 4, fig.width = 8, message = F, warning = F----
# get chlorophyll changes
chlchgout <- get_chg(wqdat, wqmtch, statdat, restdat, wqvar = 'chla', yrdf = yrdf, chgout = TRUE)
chlchg <- get_chg(wqdat, wqmtch, statdat, restdat, wqvar = 'chla', yrdf = yrdf)
save(chlchgout, file = 'data/chlchgout.RData')
save(chlchg, file = 'data/chlchg.RData')

# merge with salinity, bet salinity levels
salbrk <- salbrk %>% 
  group_by(hab, wtr) %>% 
  nest(.key = 'levs')
allchg <- full_join(chlchg, salchg, by = c('hab', 'wtr', 'stat')) %>% 
  rename(
    salev = cval.y, 
    cval = cval.x
  ) %>% 
  group_by(hab, wtr) %>% 
  nest %>% 
  left_join(salbrk, by = c('hab', 'wtr')) %>% 
  mutate(
    sallev = pmap(list(data, levs), function(data, levs){
      # browser()
      out <- data %>% 
        mutate(
          saval = salev,
          salev = cut(salev, breaks = c(-Inf, levs$qts, Inf), labels = c('lo', 'md', 'hi')),
          salev = as.character(salev)
        )
      
      return(out)
      
    })
  ) %>% 
  select(-data, -levs) %>% 
  unnest
salchg <- select(allchg, stat, hab, wtr, salev, saval)
save(salchg, file = 'data/salchg.RData', compress = 'xz')

chlcdt <- get_cdt(allchg, 'hab', 'wtr', 'salev')
save(chlcdt, file = 'data/chlcdt.RData', compress = 'xz')

chlbrk <- get_brk(chlcdt, c(0.33, 0.66), 'hab', 'wtr', 'salev')
chlbrk %>% 
  print(n = nrow(.))

## ------------------------------------------------------------------------
chlbar <- chlbrk %>% 
  group_by(hab, wtr, salev) %>% 
  nest %>% 
  mutate(
    data = map(data, function(x){
      
      brk <- x$brk
      out <- data.frame(
        lo = brk[1], md = brk[2] - brk[1], hi = 1 - brk[2]
      )
      
      return(out)
      
    })
  ) %>% 
  unnest %>% 
  gather('chllev', 'chlval', lo:hi) %>% 
  mutate(
    salev = factor(salev, levels = c('lo', 'md', 'hi')),
    chllev = factor(chllev, levels = c('lo', 'md', 'hi'))
  )
save(chlbar, file = 'data/chlbar.RData', compress = 'xz')

chlbar %>% 
  print(n = nrow(.))

## ------------------------------------------------------------------------
# discretize all chl data by breaks 
chlbrk <- chlbrk %>% 
  group_by(hab, wtr, salev) %>% 
  nest(.key = 'levs')
allchg <- allchg %>% 
  group_by(hab, wtr, salev) %>% 
  nest %>% 
  full_join(chlbrk, by = c('hab', 'wtr', 'salev')) %>% 
  mutate(
    lev = pmap(list(data, levs), function(data, levs){
      
      out <- data %>% 
        mutate(
          lev = cut(cval, breaks = c(-Inf, levs$qts, Inf), labels = c('lo', 'md', 'hi')),
          lev = as.character(lev)
        )
      browser()
      return(out)
      
    })
  ) %>% 
  select(-data, -levs) %>% 
  unnest %>% 
  rename(
    chlev = lev, 
    chval = cval
    )

save(allchg, file = 'data/allchg.RData', compress = 'xz')


## ----fig.width = 8, fig.height = 7---------------------------------------
ggplot(chlbar, aes(x = chllev, y = chlval, group = salev, fill = salev)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  facet_grid(hab ~ wtr) +
  theme_bw()

## ----fig.height = 7, fig.width = 8, message = F, warning = F-------------
toplo <- select(chlcdt, -data, -crv) %>% 
  unnest %>%
  mutate(
    salev = factor(salev, levels = c('lo', 'md', 'hi'))
  )
chlbrk <- unnest(chlbrk)
ggplot(toplo, aes(x = cval, y = cumest, group = salev, colour = salev)) + 
  geom_line() + 
  geom_segment(data = chlbrk, aes(x = qts, y = 0, xend = qts, yend = brk)) +
  geom_segment(data = chlbrk, aes(x = min(toplo$cval), y = brk, xend = qts, yend = brk)) +
  facet_grid(hab ~ wtr, scales = 'free_x') +
  theme_bw()

