#' Get conditional data as input for bn model, using simplified approach witout estimating curves
#' 
#' @param resgrp chr string of restoration project grouping variable, top is simple model, type is complex
#' @param restdat restoration data
#' @param reststat restoration station location
#' @param wqstat water quality station location
#' @param wqdat water quality data
#' @param mtch numeric for number of restoration projects by type to match to water quality stations
#' @param yrdf numeric for year difference window before/after a restoration project
#' @param qts numeric value for quantile defining categorical groupings of water quality response
#' @param lbs chr string for labels defining categorical groupings of water quality response
#'
#' @return a two element list of conditional data and matched water quality and restoration stations (from get_clo)
get_dat <- function(resgrp = c('top', 'type'), restdat, reststat, wqstat, wqdat, mtch, yrdf, qts, lbs){

  # get model type, top is simple, type is complex
  resgrp <- match.arg(resgrp)
 
  ## Distance to restoration sites
  wqmtch <- get_clo(restdat, reststat, wqstat, resgrp = resgrp, mtch = mtch)
  
  ## Summarizing effects of restoration projects on salinity
  sachg <- get_chg(wqdat, wqmtch, statdat, restdat, wqvar = 'sal', yrdf = yrdf) %>% 
    rename(saval = cval)
  
  ## Summarizing effects of restoration projects on chl
  chchg <- get_chg(wqdat, wqmtch, statdat, restdat, wqvar = 'chla', yrdf = yrdf) %>% 
    rename(chval = cval)
  
  # simple model
  if(resgrp == 'top'){
    
    # combine all using a super simple approach
    out <- sachg %>% 
      left_join(chchg, by = c('stat', 'hab', 'wtr')) %>% 
      mutate(
        salev = cut(saval, breaks = c(-Inf, quantile(saval, qts, na.rm = T), Inf), labels = lbs),
        chlev = cut(chval, breaks = c(-Inf, quantile(chval, qts, na.rm = T), Inf), labels = lbs)
      ) %>% 
      # dplyr::select(-saval, -nival, -chval, -stat) %>% 
      mutate_if(is.character, factor) %>% 
      data.frame
    
  # complex model    
  } else {
    
    ## Summarizing effects of restoration projects on tn
    tnchg <- get_chg(wqdat, wqmtch, statdat, restdat, wqvar = 'tn', yrdf = yrdf) %>% 
      rename(nival = cval)
    
    # combine all using a super simple approach
    out <- sachg %>% 
      left_join(tnchg, by = c('stat', 'hab_enh', 'hab_est', 'hab_pro', 'non_src', 'pnt_src')) %>% 
      left_join(chchg, by = c('stat', 'hab_enh', 'hab_est', 'hab_pro', 'non_src', 'pnt_src')) %>% 
      mutate(
        salev = cut(saval, breaks = c(-Inf, quantile(saval, qts, na.rm = T), Inf), labels = lbs),
        nilev = cut(nival, breaks = c(-Inf, quantile(nival, qts, na.rm = T), Inf), labels = lbs),
        chlev = cut(chval, breaks = c(-Inf, quantile(chval, qts, na.rm = T), Inf), labels = lbs)
      ) %>% 
      # dplyr::select(-saval, -nival, -chval, -stat) %>% 
      mutate_if(is.character, factor) %>% 
      data.frame
    
  }

  # combine cdat with wqmtch for outpout    
  out <- list(cdat = out, wqmtch = wqmtch)
  return(out)
  
}