# dat proc

library(tidyverse)
library(readxl)
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

# URL of EPCHC's long-term dataset in Excel Spreadsheet format
epchc_url <- "ftp://ftp.epchc.org/EPC_ERM_FTP/WQM_Reports/RWMDataSpreadsheet_ThroughCurrentReportMonth.xlsx"
download.file(url = epchc_url, destfile = './other/epchc.xlsx', method = "libcurl", mode = "wb")

# EPC data column name file
epcnames <- readLines("./other/epchc_column_names.csv")

# EPC sites within Tampa Bay used for the Annual TBEP WQ Assessment
epcsites <- c(6, 7, 8, 44, 52, 55, 70, 71, 73, 80, 36, 38, 40, 41, 46, 47, 50, 51, 60, 63, 64, 65, 66, 67, 68, 9,               11, 81, 84, 13, 14, 32, 33, 16, 19, 28, 82, 23, 24, 25, 90, 91, 92, 93, 95)
# Station Lists by Bay Segment
otb_stations <- c(36, 38, 40, 41, 46, 47, 50, 51, 60, 63, 64, 65, 66, 67, 68)
hb_stations <- c(6, 7, 8, 44, 52, 55, 70, 71, 73, 80)
mtb_stations <- c(9, 11, 81, 84, 13, 14, 32, 33, 16, 19, 28, 82)
ltb_stations <- c(23, 24, 25, 90, 91, 92, 93, 95)
# Short Bay Segment Names
bay_segments = c("OTB", "HB", "MTB", "LTB")

# Import the raw dataset into R
epcdata <- read_xlsx("./other/epchc.xlsx",
                     sheet="RWMDataSpreadsheet",
                     col_types = c("numeric", "numeric", "text", "text", "text", "text",
                                   "numeric", "numeric", "text", "numeric", "numeric",
                                   "text", "date", "text", "numeric", "text", "text",
                                   "numeric", "numeric", "numeric", "numeric", "text",
                                   "text", "text", "numeric", "text", "numeric", "text",
                                   "numeric", "text", "numeric", "text", "numeric",
                                   "text", "numeric", "text", "numeric", "text",
                                   "numeric", "text", "numeric", "text", "numeric",
                                   "text", "numeric", "text", "numeric", "text",
                                   "numeric", "text", "numeric", "text", "numeric",
                                   "text", "numeric", "text", "numeric", "text",
                                   "numeric", "text", "numeric", "text", "numeric",
                                   "text", "numeric", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text", "text",
                                   "text", "text", "text", "text", "text", "text",
                                   "text", "text", "text"),
                     col_names = epcnames,
                     skip=1, na="")

# process raw data
wqdat <- epcdata %>%
  rename(
    datetime = SampleTime,
    stat = StationNumber, 
    lat = Latitude, 
    lon = Longitude,
    sallo = Sal_Bottom_ppth, 
    salmd = Sal_Mid_ppth,
    salhi = Sal_Top_ppth, 
    dolo = DO_Bottom_mgL,
    domd = DO_Mid_mgL, 
    dohi = DO_Top_mgL,
    chla = Chlorophyll_a_uncorr_ugL, 
    tn = Total_Nitrogen_mgL
  ) %>% 
  filter(stat %in% epcsites) %>% 
  select(stat, datetime, lat, lon, sallo, salmd, salhi, dolo, domd, dohi, chla, tn) %>% 
  gather('var', 'val', sallo:tn) %>% 
  mutate(val = as.numeric(val)) %>% 
  spread('var', 'val') %>% 
  rowwise() %>%
  mutate(
    sal = mean(c(sallo, salmd, salhi), na.rm = TRUE),
    do = mean(c(dolo, domd, dohi), na.rm = TRUE)
  ) %>%
  select(-sallo, -salmd, -salhi, -dolo, -domd, -dohi) %>% 
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

# setup parallel backend
ncores <- detectCores() - 1  
cl<-makeCluster(ncores)
registerDoParallel(cl)
strt<-Sys.time()

grds <- crossing(
  yrdf = 1:10, 
  mtch = 1:10, 
  resgrp = c('top', 'type')
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
  
  # grid row values
  vls <- grds[i, ]
  
  # inputs
  yrdf <- vls$yrdf
  mtch <- vls$mtch
  resgrp <- vls$resgrp
  
  # get wqmtch
  wqmtch <- get_clo(restdat, reststat, wqstat, resgrp = resgrp, mtch = mtch)
  
  # get differences
  wqdf <- get_chgdf(wqdat, wqmtch, wqstat, restdat, wqvar = 'chla', yrdf = yrdf)
  
  return(wqdf)
  
}
stopCluster(cl)

# combine results with grid
grdave <- res %>% 
  enframe %>% 
  bind_cols(grds,. ) %>% 
  dplyr::select(-name)

save(grdave, file = 'data/grdave.RData', compress = 'xz')
