## ----message = F, warning = F, results = 'hide'--------------------------
library(tidyverse)
library(readxl)
library(ggmap)
library(lubridate)
library(geosphere)
library(stringi)
library(tibble)
knitr::knit('project_merge.Rmd', tangle = TRUE)
file.copy('project_merge.R', 'R/project_merge.R', overwrite = T)
file.remove('project_merge.R')

## ----warning = F, message = F, fig.width = 8, fig.height = 6-------------
fl <- 'data-raw/TBEP_Restoration Database_11_21_07_JRH.csv'

# clean up habitat restoration data
habdat <- fl %>% 
  read_csv %>% 
  select(Latitude, Longitude, Project_Completion_Date, `Project_Technology`, `Project_Activity`, `Acres-1`) %>% 
  rename(
    lat = Latitude, 
    lon = Longitude, 
    date = Project_Completion_Date, 
    tech = `Project_Technology`, 
    type = `Project_Activity`,
    acre = `Acres-1`
  ) %>% 
  mutate(
    id = stri_rand_strings(nrow(.), length = 4),
    lat = as.numeric(lat),
    lon = as.numeric(lon),
    date = as.numeric(date),
    tech = toupper(tech),
    type = toupper(type)
  ) %>% 
  filter(lat > 27.3 & lat < 28.2) %>% 
  filter(!is.na(date))

# habitat restoration station locs
habstat <- habdat %>% 
  select(id, lat, lon) %>% 
  unique

# normalized habitat data
habdat <- habdat %>% 
  select(-lat, -lon)

save(habdat, file = 'data/habdat.RData', compress = 'xz')
save(habstat, file = 'data/habstat.RData', compress = 'xz')

## ------------------------------------------------------------------------
head(habdat)

## ----warning = F, message = F, fig.width = 8, fig.height = 6-------------
bmp_tr <- 'data-raw/apdb_completed_categorized.csv'

# clean up BMP-Treatment data
bmpdat <- bmp_tr %>% 
  read_csv %>% 
  select(Unique_Project_ID, ProjectLatitude, ProjectLongitude, Completion_Date, Project_Technology, Project_Activity, Acres) %>% 

  rename(
    id = Unique_Project_ID,
    lat = ProjectLatitude, 
    lon = ProjectLongitude, 
    date = Completion_Date, 
    tech = Project_Technology, 
    type = Project_Activity, 
    acre = Acres
  ) %>% 
  mutate(
    id = stri_rand_strings(nrow(.), length = 4),
    lat = as.numeric(lat),
    lon = as.numeric(lon),
    date = as.numeric(date),
    tech = toupper(tech),
    type = toupper(type)
  ) %>% 
  filter(lat > 27.3 & lat < 28.2) %>% 
  filter(!is.na(date))

# BMP-Treatment station locs
bmpstat <- bmpdat %>% 
  select(id, lat, lon) %>% 
  unique

# normalized habitat data
bmpdat <- bmpdat %>% 
  select(-lat, -lon)

save(bmpdat, file = 'data/bmpdat.RData', compress = 'xz')
save(bmpstat, file = 'data/bmpstat.RData', compress = 'xz')

## ------------------------------------------------------------------------
head(bmpdat)

## ------------------------------------------------------------------------
head(bmpstat)

## ------------------------------------------------------------------------
restdat <- rbind(habdat, bmpdat)
reststat <- rbind(habstat, bmpstat)
save(restdat, file = 'data/restdat.RData', compress = 'xz')
save(reststat, file = 'data/reststat.RData', compress = 'xz')

checkdata <- restdat %>%
             select(type) %>%
             unique()
checkdata2 <- restdat %>%
             select(tech) %>%
             unique()

