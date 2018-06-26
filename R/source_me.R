library(tidyverse)
library(lubridate)
library(geosphere)
library(stringi)
library(tibble)
library(bnlearn)
library(scales)
library(sf)
library(sp)
library(foreach)
library(doParallel)

data(restdat)
data(reststat)
data(wqdat)
data(wqstat)

# setup parallel backend
ncores <- detectCores() - 1  
cl<-makeCluster(ncores)
registerDoParallel(cl)
strt<-Sys.time()

# eval grid
grds <- crossing(
  yrdf = 1:10, 
  mtch = 1:10, 
  resgrp = c('type'), 
  yrstr = c(2004), 
  yrend = c(2017)
  ) 

res <- foreach(i = 1:nrow(grds), .packages = c('tidyverse', 'bnlearn', 'sf', 'sp', 'geosphere')) %dopar% {

  # source R files
  source('R/get_chg.R')
  source('R/get_clo.R')
  source('R/get_dat.R')
  source('R/get_lik.R')

  # globals
  chlspl <- 11
  nitspl <- 0.5
  salspl <- 26   
  
  # log
  sink('log.txt')
  cat(i, 'of', nrow(grds), '\n')
  print(Sys.time()-strt)
  sink()
  
  # grid row values
  vls <- grds[i, ]

  # inputs
  yrdf <- vls$yrdf
  mtch <- vls$mtch
  yrs <- c(vls$yrstr, vls$yrend)
  resgrp <- vls$resgrp
  
  # get model structure based on resgrp
  if(resgrp == 'top')
    modstr <- "[hab][wtr][salev|hab:wtr][chlev|hab:wtr:salev]"
  else 
    modstr <- "[hab_enh][hab_est][hab_pro][non_src][pnt_src][salev|hab_enh:hab_est:hab_pro:non_src:pnt_src][nilev|salev:hab_enh:hab_est:hab_pro:non_src:pnt_src][chlev|nilev]"

  # create bn network object
  net <- model2network(modstr)

  # subset restoration data by years
  restdat_sub <- restdat %>% 
    filter(date >= yrs[1] & date <= yrs[2])
  reststat_sub <- reststat %>% 
    filter(id %in% restdat_sub$id)

  # wq stats matched to rest stats
  allcdat <- try({
    get_dat(resgrp, restdat_sub, reststat_sub, wqstat, wqdat, mtch, yrdf, chlspl, nitspl, salspl)
    })
  
  # exit if error
  if(inherits(allcdat, 'try-error')) return(NA)
    
  # get conditional data for bn input
  cdat <- allcdat$cdat
  
  # fit simple bn model
  cdat_mod <- cdat %>%
    select_if(is.factor) %>%
    na.omit %>%
    data.frame %>% 
    bn.fit(net, data = .)
    
  # get likelihood estimates from mod, bef/aft only (no salinity)
  ests <- get_lik(cdat, cdat_mod) %>%
    mutate(event = gsub('^wtr\\_|^hab\\_|^hab\\_enh\\_|^hab\\_est\\_|^hab\\_pro\\_|^non\\_src\\_|^pnt\\_src\\_', '', event)) %>%
    mutate(chlev = factor(chlev, levels = c('lo', 'hi'))) %>%
    spread(event, est) %>%
    mutate(
      chg = 100 * (aft - bef),
      chg = round(chg, 1)
    )
  
  return(ests)
  
}

# combine results with grds, remove scenarios that returned NA
grdsres <- grds %>%
  mutate(
    res = res,
    resgrp = factor(resgrp, levels = c('top', 'type'), labels = c('simple', 'complex'))
    ) %>%
  unite('yrs', yrstr, yrend, sep = '-') %>% 
  filter(map_lgl(res, ~ !is.logical(.x))) %>% 
  unnest

save(grdsres, file = 'data/grdsres.RData', compress = 'xz')
