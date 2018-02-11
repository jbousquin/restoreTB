#' Spatial join of water quality stations and restoration sites
#'
#' @param resgrp chr string of gouping variable in restdat
#' @param mtch numeric for number of restoration sites to match with each wq station
#' 
get_clo <- function(restdat, reststat, wqstat, resgrp = 'top', mtch = 10){
  
  # join restoration site data and locs, make top level grouping column
  restall <- left_join(restdat, reststat, by = 'id')
  
  # restoration project grouping column
  names(restall)[names(restall) %in% resgrp] <- 'resgrp'
  
  # stop if mtch greater than minimum size of resgrp
  minval <- table(restall$resgrp) %>% 
    min
  if(mtch > minval)
    stop(paste('maximum value for mtch is', minval))

  # restall to utm
  restall <- restall %>% 
    st_as_sf(coords = c('lon', 'lat')) %>% 
    st_set_crs('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') %>% 
    as('Spatial') %>% 
    spTransform(CRS("+proj=utm +zone=17. +datum=WGS84")) %>% 
    data.frame %>% 
    select(-optional) %>% 
    rename(
      lon = coords.x1, 
      lat = coords.x2
    )
    
  # wqstat to utm
  wqstat <- wqstat %>% 
    st_as_sf(coords = c('lon', 'lat')) %>% 
    st_set_crs('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs') %>% 
    as('Spatial') %>% 
    spTransform(CRS("+proj=utm +zone=17. +datum=WGS84")) %>% 
    data.frame %>% 
    select(-optional) %>% 
    rename(
      lon = coords.x1,
      lat = coords.x2
    )

  # match habitat restoration locations with wq stations by closest mtch locations
  wqmtch <- wqstat %>% 
    group_by(stat) %>% 
    nest %>% 
    mutate(
      clo = map(data, function(sta){
        
        # get top mtch closest restoration projects to each station
        # grouped by resgrp column
        dists <- rbind(sta, restall[, c('lon', 'lat')]) %>%
          mutate(
            lon = lon,
            lat = lat
          ) %>% 
          dist(method = 'euclidean') %>% 
          as.matrix %>% 
          .[-1, 1] %>%
          data.frame(
            restall[, c('id', 'resgrp')],
            dist = ., stringsAsFactors = F
          ) %>%
          group_by(resgrp) %>%
          arrange(dist) %>% 
          nest %>%
          mutate(
            data = map(data, function(x) x[1:mtch, ]),
            rnk = map(data, function(x) seq(1:nrow(x)))
          ) %>% 
          unnest %>% 
          mutate(dist = dist / 1000) # to km
        
        return(dists)
        
      })
    ) %>% 
    dplyr::select(-data) %>% 
    unnest
  
  return(wqmtch)
  
  }
