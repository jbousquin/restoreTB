######
# grid search for average differences, all stations, restoration type, year window, match combos, for ind_eval

library(tidyverse)
library(lubridate)
library(geosphere)
library(stringi)
library(tibble)
library(scales)
library(sf)
library(sp)
library(foreach)
library(doParallel)

data(restdat)
data(reststat)
data(wqdat)
data(wqstat)

# source R files
source('R/get_chgdf.R')
source('R/get_clo.R')

# inputs
yrdf <- 10
mtch <- 10
resgrp <- 'type'


# setup parallel backend
ncores <- detectCores() - 1  
cl<-makeCluster(ncores)
registerDoParallel(cl)
strt<-Sys.time()

grds <- crossing(
  yrdf = c(5, 10), 
  mtch = c(5, 10), 
  rnd = 1:100,
  rndtyp = c('dt', 'loc', 'both'),
  resgrp = c('type')
) 

res <- foreach(i = 1:nrow(grds), .packages = c('tidyverse', 'bnlearn', 'sf', 'sp', 'geosphere')) %dopar% {
  
  # source R files
  source('R/get_chgdf.R')
  source('R/get_clo.R')
  
  # log
  sink('log.txt')
  cat(i, 'of', nrow(grds), '\n')
  print(Sys.time()-strt)
  sink()
  
  # restoration project type levels, labels and colors
  typelev <- c('hab_enh', 'hab_est', 'hab_pro', 'non_src', 'pnt_src')
  typelab <- c('Habitat\nenhance', 'Habitat\nestablish', 'Habitat\nprotect', 'Nonpoint\ncontrol', 'Point\ncontrol')
  typecol <- c('#31a354', '#74c476' , '#c7e9c0', '#3182bd', '#9ecae1')
  
  # grid row values
  vls <- grds[i, ]
  
  # inputs
  yrdf <- vls$yrdf
  mtch <- vls$mtch
  rndtyp <- vls$rndtyp
  
  restdatrnd <- restdat
  reststatrnd <- reststat
  
  if(rndtyp %in% c('dt', 'both')){
    
    restdatrnd <- restdat %>% 
      mutate(date = sample(seq(1971, 2018), size = nrow(.), replace = T))
  
  if(rndtyp %in% c('loc', 'both')){
    
    reststatrnd <- reststat %>% 
      mutate(
        lat = runif(nrow(.), min(lat), max(lat)),
        lon = runif(nrow(.), min(lon), max(lon))
      )
    
  }
    
  # get wqmtch
  wqmtch <- get_clo(restdatrnd, reststatrnd, wqstat, resgrp = 'type', mtch = mtch)
  
  # get differences
  wqdf <- get_chgdf(wqdat, wqmtch, wqstat, restdatrnd, wqvar = 'chla', yrdf = yrdf)

  # anova model
  mod <- aov(avedf ~ resgrp, data = wqdf)
  
  # get significance letters for multiple comparisons
  modhsd <- TukeyHSD(mod) %>% 
    .$resgrp %>% 
    data.frame
  pvals <- modhsd$p.adj
  names(pvals) <- rownames(modhsd)
  lets <- multcompLetters(pvals)
  lets <- lets$Letters
  lets <- lets %>% 
    enframe('resgrp', 'lets')
  
  # get average and max chl chng for color mapping
  datdf <- wqdf %>% 
    group_by(resgrp) %>% 
    mutate(
      maxdf = 10 #max(avedf, na.rm = T)
    ) %>% 
    ungroup
  
  # join lets with max dif values for labels
  lets <- datdf %>% 
    select(resgrp, maxdf) %>% 
    unique %>% 
    mutate(resgrp = as.character(resgrp)) %>% 
    left_join(lets, by = 'resgrp')
  
  # test if df values diff from zero, by group
  difzr <- datdf %>% 
    group_by(resgrp) %>% 
    nest %>% 
    mutate(
      tst = purrr::map(data, ~ t.test(x = .$avedf)),
      pvl = purrr::map(tst, function(x){
        
        ifelse(x$p.value > 0.05, 'mean = 0', 
               ifelse(x$statistic > 0, 'mean > 0', 'mean < 0'))
        
      }), 
      mnest = purrr::map(tst, function(x){
        x$estimate
      })
    ) %>% 
    select(resgrp, pvl, mnest) %>% 
    unnest %>% 
    mutate(resgrp = as.character(resgrp)) 
  
  # multiplecomparison letters and diff values from difzr
  lets <- lets %>% 
    left_join(difzr, by = 'resgrp') %>% 
    unite(lets, c('lets', 'pvl'), sep = ', ')
  
  out <- list(mod = mod, lets = lets)
  
  return(out)
  
}