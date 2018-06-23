

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

# globals
chlspl <- 8
nitspl <- 0.5
salspl <- 27   

# eval grid
grds <- crossing(
  yrdf = 1:10, 
  mtch = 1:10, 
  resgrp = c('type'), 
  yrstr = c(1974), 
  yrend = c(2017)
  ) %>% 
  filter(yrend > yrstr)

res <- foreach(i = 1:nrow(grds), .packages = c('tidyverse', 'bnlearn', 'sf', 'sp', 'geosphere')) %dopar% {

  # source R files
  source('R/get_chg.R')
  source('R/get_clo.R')
  source('R/get_dat.R')
  source('R/get_lik.R')

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

# combin with grds for plot
grdsres <- grds %>% 
  mutate(res = res) %>% 
  unnest %>% 
  unite('yrs', yrstr, yrend, sep = '-') %>% 
  filter(chlev == 'lo')

ggplot(grdsres, aes(x = yrdf, y = chg, colour = mtch, group = mtch)) + 
  # geom_line() +
  geom_point() + 
  stat_smooth(method = 'lm', se = F) +
  facet_grid(salev ~ project) +
  theme_bw() + 
  geom_hline(yintercept = 0)
  



