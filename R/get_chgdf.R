#' Get weighted means of treatment, before/after difference
#'
#' @param yrdf year diff to summarize wq data, in years before/after restoration projects
#'
get_chgdf <- function(wqdat, wqmtch, statdat, restdat, wqvar = 'sal', yrdf = 5, chgout = FALSE){
  
  # get only date, station, relevant wq variable
  names(wqdat)[names(wqdat) %in% wqvar] <- 'wqvar'
  wqdat <- wqdat %>%
    dplyr::select(datetime, stat, wqvar)
  
  # get weighted means of water quality value for restoration treatments, types
  chg <- wqmtch %>%
    left_join(restdat, by = 'id') %>%
    .[, !names(.) %in% c('tech', 'type', 'acre')] %>% 
    mutate(
      date = paste0(date, '-07-01'),
      date = as.Date(date, format = '%Y-%m-%d'), 
      wts = dist / min(dist),
      wts = 1 / wts, 
      slcs = pmap(list(stat, date), function(stat, date){
        
        # summarize before/after wq data based on restoration date
        date <- as.Date(date, origin = '1970-01-01')
        
        # filter wq data by stat, get date bounds
        statdat <- wqdat[wqdat$stat %in% stat, ]
        orrng <- range(statdat$datetime)
        
        # get date range +/- restoration proj defined by yrdf
        dtrng <- c(date - yrdf * 365, date + yrdf * 365)
        # dtrng <- with(dt, c(date - 365, date + yrdf * 365))
        
        ## get values within window in dtrng, only if dates available
        ## values are summarized as mean before/after
        bef <- NA; aft <- NA
        
        # before
        if(dtrng[1] >= orrng[1]){
          
          # summarizes values before
          bef <- filter(statdat, datetime >= dtrng[1] & datetime <= date) %>%
            .$wqvar %>% 
            mean(na.rm = TRUE)
          
        }
        
        # after
        if(dtrng[2] <= orrng[2]){
          
          # summarize values after
          aft <- filter(statdat, datetime <= dtrng[2] & datetime >= date) %>%
            .$wqvar %>% 
            mean(na.rm = TRUE)
          
        }
        
        # combine/return the wq station/restoration station summary
        out <- data.frame(bef = bef, aft = aft, dtend = dtrng[1], dtaft = dtrng[2])
        
        return(out)
        
      })
    ) %>%
    unnest %>% 
    dplyr::select(stat, rnk, resgrp, wts, bef, aft, dtend, date, dtaft) 
  
  # return temporary chg object if T
  if(chgout) return(chg)
  
  # get difference before after, separate for each station, rep by project type
  # take average of the differences
  dfout <- chg %>% 
    mutate(
      difv = aft - bef
    ) %>% 
    group_by(stat, resgrp) %>% 
    summarise(
      avedf = mean(difv, na.rm = T)
    )
 
  return(dfout)
  
}