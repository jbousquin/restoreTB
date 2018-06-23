# dat proc

library(tidyverse)
library(forcats)
library(stringi)
library(sf)
library(sp)
library(geosphere)

# functions
source('R/funcs.R')
source('R/get_chg.R')
source('R/get_clo.R')
source('R/get_dat.R')
source('R/get_lik.R')

##
# process raw water quality data

# raw
wqdat_raw <- read_csv('other/epchc_clean_data_07162017.csv')

# rename, select relevant columns, integrate variables across depths
# annual averages by site, variable
wqdat <- wqdat_raw %>% 
  rename(
    yr = YEAR,
    mo = month,
    dttm = SampleTime,
    stat = epchc_station, 
    lat = Latitude, 
    lon = Longitude,
    sallo = Sal_Bottom_ppth, 
    salmd = Sal_Mid_ppth,
    salhi = Sal_Top_ppth, 
    dolo = DO_Bottom_mg_L,
    domd = DO_Mid_mg_L, 
    dohi = DO_Top_mg_L,
    chla = chl_a, 
    tn = TN_TOTAL
  ) %>% 
  select(stat, yr, mo, dttm, lat, lon, sallo, salmd, salhi, dolo, domd, dohi, chla, tn) %>% 
  gather('var', 'val', sallo:tn) %>% 
  mutate(val = as.numeric(val)) %>% 
  spread('var', 'val') %>% 
  rowwise() %>%
  mutate(
    sal = mean(c(sallo, salmd, salhi), na.rm = TRUE),
    do = mean(c(dolo, domd, dohi), na.rm = TRUE)
  ) %>%
  select(-sallo, -salmd, -salhi, -dolo, -domd, -dohi, -dttm) %>% 
  mutate(
    dy = 1
  ) %>% 
  unite('datetime', yr, mo, dy, sep = '-') %>% 
  mutate(
    datetime = as.Date(datetime, format = '%Y-%m-%d')
  )

# get station locations
wqstat <- wqdat %>% 
  select(stat, lon, lat) %>% 
  unique

# remove denormalized rows
wqdat <- wqdat %>% 
  select(-lon, -lat)

save(wqstat, file= 'data/wqstat.RData', compress = 'xz')
save(wqdat, file = 'data/wqdat.RData', compress = 'xz')

##
# create restdat, reststat from new file from Ed

# format raw data
raw <- read.csv('other/DRAFT_TBEP_Combined_Projects_1971-2017_06222018.csv', stringsAsFactors = F) %>% 
  rename(
    date = Completion_Date, 
    type = Project_Activity, 
    tech = Project_Technology, 
    lat = ProjectLatitude,
    lon = ProjectLongitude
  ) %>% 
  select(date, type, tech, lat, lon) %>% 
  filter(
    lon > -1e6 & lat > 20 & !is.na(date)
  ) %>% 
  mutate(
    id = stri_rand_strings(nrow(.), length = 4),
    top = fct_recode(type,
                     hab = 'Habitat_Enhancement',
                     hab = 'Habitat_Establishment', 
                     hab = 'Habitat_Protection', 
                     wtr = 'Nonpoint_Source', 
                     wtr = 'Point_Source'
    ),
    type = fct_recode(type,
                      hab_enh = 'Habitat_Enhancement',
                      hab_est = 'Habitat_Establishment',
                      hab_pro = 'Habitat_Protection',
                      non_src = 'Nonpoint_Source',
                      pnt_src = 'Point_Source'
    ),
    tech = toupper(tech),
    date = gsub('\\?$', '', date),
    date = as.numeric(date)
  ) 

# restdat
restdat <- raw %>% 
  select(date, tech, type, top, id)

# reststat
reststat <- raw %>% 
  select(id, lat, lon)

save(restdat, file = 'data/restdat.RData')
save(reststat, file = 'data/reststat.RData')

##
# get wqmtch of wq stations with restoration stations based on data created above

data(restdat)
data(reststat)
data(wqstat)

wqmtch <- get_clo(restdat, reststat, wqstat, resgrp = 'top', mtch = 10)
save(wqmtch, file = 'data/wqmtch.RData', compress = 'xz')
