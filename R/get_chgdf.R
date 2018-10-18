#' Get weighted means of treatment, before/after difference
#'
#' @param yrdf year diff to summarize wq data, in years before/after restoration projects
#' @param chgout logical indicating if only the changes before/after are returned without differences and not summarizedby number of matches
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
      slcs = purrr::pmap(list(stat, date), function(stat, date){
        
        # summarize before/after wq data based on restoration date
        date <- as.Date(date, origin = '1970-01-01')
        
        # filter wq data by stat, get date bounds
        statdat <- wqdat[wqdat$stat %in% stat, ]
        orrng <- range(statdat$datetime)
        browser()
        # get date range +/- restoration proj defined by yrdf
        # dtrng <- c(date - yrdf * 365, date + yrdf * 365)
        dtrng <- c(date - 365, date + yrdf * 365 - 365, date + yrdf * 365)
        
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
        if(dtrng[3] <= orrng[2]){
          
          # summarize values after
          aft <- filter(statdat, datetime <= dtrng[3] & datetime >= dtrng[2]) %>%
            .$wqvar %>% 
            mean(na.rm = TRUE)
          
        }
        
        # combine/return the wq station/restoration station summary
        out <- data.frame(bef = bef, aft = aft, dtend = dtrng[1], dtaft = dtrng[3])
        
        return(out)
        
      })
    ) %>%
    unnest %>% 
    dplyr::select(stat, rnk, resgrp, dist, wts, bef, aft, dtend, date, dtaft) 
  
  # return temporary chg object if T
  if(chgout) return(chg)
  
  # get difference before after, separate for each station, rep by project type
  # take average of the differences, check if conf int includes zero (ns if yes, sig if not)
  dfout <- chg %>% 
    mutate(
      difv = aft - bef
    ) %>% 
    group_by(stat, resgrp) %>% 
    summarise(
      avedf = mean(difv, na.rm = T),
      inczr = ifelse(inherits(try({t.test(difv)}, silent = T), 'try-error'), NA, findInterval(0, t.test(difv)$conf.int)),
      avedists = mean(dist, na.rm = T),
      avecompl = mean(lubridate::decimal_date(date))
    ) %>% 
    mutate(
      inczr = case_when(
        inczr %in% c(0, 2) ~ 'sig',
        inczr == 1 ~ 'ns'
      )
    )
  
  return(dfout)
  
}