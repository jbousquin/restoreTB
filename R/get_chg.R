#' Get weighted means of treatment, grouping combinations of water quality data relative to matching restoration sites
#'
#' @param yrdf year diff to summarize wq data, in years before/after restoration projects
#'
get_chg <- function(wqdat, wqmtch, statdat, restdat, wqvar = 'sal', yrdf = 5, chgout = FALSE){
  
  # get only date, station, relevant wq variable
  names(wqdat)[names(wqdat) %in% wqvar] <- 'wqvar'
  wqdat <- wqdat %>%
    dplyr::select(datetime, stat, wqvar)

  # get weighted means of water quality value for restoration treatments, types
  str <- Sys.time()
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
  
  # get weighted mean by resgrps
  chg <- chg %>%
    group_by(stat, resgrp) %>%
    summarise(
      bef = weighted.mean(bef, w = wts, na.rm = TRUE),
      aft = weighted.mean(aft, w = wts, na.rm = TRUE)
    )

  # get combinations of restoration types
  bef <- unique(chg$resgrp) %>% 
    paste0(., '_bef')
  aft <- gsub('bef', 'aft', bef)
  tosel <- cbind(bef, aft) %>% 
    t %>%
    data.frame %>% 
    as.list %>% 
    expand.grid  
  
  # average wq values by each restoration combination
  chgcmb <- chg %>% 
    gather('tmp', 'cval', bef, aft) %>% 
    unite('resgrp', resgrp, tmp, sep = '_') %>% 
    group_by(stat) %>% 
    nest %>% 
    mutate(
      cmb = purrr::map(data, function(x){
 
        # average each combo for the station
        out <- apply(tosel, 1, function(sel){
          
          sel <- as.character(sel)
          sub <- x[x$resgrp %in% sel, ] %>% 
            .$cval %>% 
            mean(na.rm = T)
          
          c(sel, sub)
          
        })
        
        return(out)
        
      }), 
      cmb = purrr::map(cmb, ~ t(.x) %>% data.frame(., stringsAsFactors = FALSE))
    ) %>%
    dplyr::select(-data) %>%
    unnest

  # add column names
  names(chgcmb) <- c('stat', as.character(unique(chg$resgrp)), 'cval')
  chgcmb$cval <- as.numeric(chgcmb$cval)

  return(chgcmb)

}