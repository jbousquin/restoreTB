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
  chg <- wqmtch %>%
    left_join(restdat, by = 'id') %>%
    .[, !names(.) %in% c('tech', 'type', 'acre')] %>% 
    mutate(
      date = paste0(date, '-07-01'),
      date = as.Date(date, format = '%Y-%m-%d'), 
      wts = dist / min(dist),
      wts = 1 / wts
    ) %>%
    split(.$stat) %>%
    map(., function(x){

      # cat(unique(x$stat), '\t')
      # iterate through the restoration sites closest to each wq station
      bysta <- x %>%
        group_by(rnk, resgrp) %>%
        nest %>%
        mutate(
          wqchg = map(data, function(dt){
            
            # summarize before/after wq data based on restoration date
            
            # filter wq data by stat, get date bounds
            statdat <- filter(wqdat, stat %in% dt$stat)
            orrng <- range(statdat$datetime)
            
            # get date range +/- restoration proj defined by yrdf
            dtrng <- with(dt, c(date - yrdf * 365, date + yrdf * 365))
            
            ## get values within window in dtrng, only if dates available
            ## values are summarized as mean before/after
            bef <- NA; aft <- NA
            
            # before
            if(dtrng[1] >= orrng[1]){
              
              # summarizes values before
              bef <- filter(statdat, datetime >= dtrng[1] & datetime <= dt$date) %>%
                .$wqvar %>% 
                mean(na.rm = TRUE)
              
            }
            
            # after
            if(dtrng[2] <= orrng[2]){
              
              # summarize values after
              aft <- filter(statdat, datetime <= dtrng[2] & datetime >= dt$date) %>%
                .$wqvar %>% 
                mean(na.rm = TRUE)
             
            }
            
            # combine/return the wq station/restoration station summary
            out <- data.frame(bef = bef, aft = aft)
            
            return(out)
            
          })
          
        )
      
      # return the complete restoration summary
      bysta <- unnest(bysta)
      
      return(bysta)
      
    }) %>%
    do.call('rbind', .) %>%
    remove_rownames() %>% 
    dplyr::select(stat, rnk, resgrp, wts, bef, aft) %>%
    group_by(stat, resgrp) %>%
    summarise(
      bef = weighted.mean(bef, w = wts, na.rm = TRUE),
      aft = weighted.mean(aft, w = wts, na.rm = TRUE)
    )
  
  # return temporary chg object if T
  if(chgout) return(chg)

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
      cmb = map(data, function(x){
        
        # average each combo for the station
        out <- apply(tosel, 1, function(sel){
          
          sel <- as.character(sel)
          sub <- x[x$resgrp %in% sel, ] %>% 
            .$cval %>% 
            mean
          
          c(sel, sub)
          
        })
        
        return(out)
        
      }), 
      cmb = map(cmb, ~ t(.x) %>% data.frame(., stringsAsFactors = FALSE))
    ) %>%
    dplyr::select(-data) %>%
    unnest

  # add column names
  names(chgcmb) <- c('stat', as.character(unique(chg$resgrp)), 'cval')
  chgcmb$cval <- as.numeric(chgcmb$cval)

  return(chgcmb)

}