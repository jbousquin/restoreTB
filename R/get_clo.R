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

  # match habitat restoration locations with wq stations by closest mtch locations
  wqmtch <- wqstat %>% 
    group_by(stat) %>% 
    nest %>% 
    mutate(
      clo = map(data, function(sta){
        
        # get top mtch closest restoration projects to each station
        # grouped by resgrp column
        dists <- distm(rbind(sta, restall[, c('lat', 'lon')])) %>%
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
          unnest
        
        return(dists)
        
      })
    ) %>% 
    select(-data) %>% 
    unnest
  
  return(wqmtch)
  
  }
