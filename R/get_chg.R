#' Get weighted means of treatment, grouping combinations of water quality data relative to matching restoration sites
#'
#' @param yrdf year diff to summarize wq data, in years before/after restoration projects
#'
get_chg <- function(wqdat, wqmtch, statdat, restdat, wqvar = 'sal', yrdf = 5, chgout = FALSE){
  
  # get only date, station, relevant wq variable
  names(wqdat)[names(wqdat) %in% wqvar] <- 'wqvar'
  wqdat <- wqdat %>%
    select(datetime, stat, wqvar)

  # get weighted means of water quality value for restoration treatments, types
  chg <- wqmtch %>%
    left_join(restdat, by = 'id') %>%
    select(-tech, -type, -acre) %>% 
    mutate(
      date = paste0(date, '-07-01'),
      date = as.Date(date, format = '%Y-%m-%d'), 
      wts = dist / min(dist),
      wts = 1 / wts
    ) %>%
    split(.$stat) %>%
    map(., function(x){

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
    select(stat, rnk, resgrp, wts, bef, aft) %>%
    gather('trt', 'val', bef:aft) %>%
    group_by(stat, resgrp, trt) %>%
    summarise(
      cval = weighted.mean(val, w = wts, na.rm = TRUE)
    ) %>%
    unite('cmb', resgrp, trt)
  
    # return temporary chg object if T
    if(chgout) return(chg)
  
    # get combinations
    
    # combine temporal categories by restoration typ
    chgcmb <- chg %>% 
      group_by(stat) %>% 
      nest %>% 
      mutate(
        cmb = map(data, function(x){
          
          nms <- x$cmb
          x <- as.list(x$cval)
          names(x) <- nms
          
          x <- combn(x, 2, simplify = FALSE) %>% 
            lapply(function(x){
              
              nms <- names(x)
              cval <- x %>% 
                unlist %>% 
                mean
              
              out <- data.frame(nms[1], nms[2], cval)
              
              return(out)
              
            }) %>% 
            do.call('rbind', .)
        
          return(x)
           
        })
      ) %>% 
      select(-data) %>% 
      unnest
    
    # remove combined categories with the same restoration type
    torm <- with(chgcmb,  
      which(
        gsub('_.*$', '', nms.1.) == gsub('_.*$', '', nms.2.)
      )
    )
    chgcmb <- chgcmb[-torm, ] %>% 
      rename(
        hab = nms.1.,
        wtr = nms.2.
      )

  return(chgcmb)

}