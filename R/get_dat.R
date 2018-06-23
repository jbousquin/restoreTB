#' Get conditional data as input for bn model, using simplified approach witout estimating curves
#' 
#' @param resgrp chr string of restoration project grouping variable, top is simple model, type is complex
#' @param restdat restoration data
#' @param reststat restoration station location
#' @param wqstat water quality station location
#' @param wqdat water quality data
#' @param mtch numeric for number of restoration projects by type to match to water quality stations
#' @param yrdf numeric for year difference window before/after a restoration project
#' @param chlspl numeric value defining categorical groupings of chl response, defaults to median of estimated groups
#' @param nitspl numeric value defining categorical groupings of nit response, defaults to median of estimated groups
#' @param salspl numeric value defining categorical groupings of sal response, defaults to median of estimated groups
#'
#' @return a two element list of conditional data and matched water quality and restoration stations (from get_clo)
get_dat <- function(resgrp = c('top', 'type'), restdat, reststat, wqstat, wqdat, mtch, yrdf, chlspl = NULL, nitspl = NULL, salspl = NULL){
  
  # categorical labels for splits
  lbs <- c('lo', 'hi')
  
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
  
  # get salinity, chlorophyll nominal split if not provided
  if(is.null(salspl)) salspl <- quantile(sachg$saval, 0.5, na.rm = T)
  if(is.null(chlspl)) chlspl <- quantile(chchg$chval, 0.5, na.rm = T)
  
  # simple model
  if(resgrp == 'top'){
    
    # combine all using a super simple approach
    out <- sachg %>% 
      left_join(chchg, by = c('stat', 'hab', 'wtr')) %>% 
      mutate(
        salev = cut(saval, breaks = c(-Inf, salspl, Inf), labels = lbs),
        chlev = cut(chval, breaks = c(-Inf, chlspl, Inf), labels = lbs)
      ) %>% 
      # dplyr::select(-saval, -nival, -chval, -stat) %>% 
      mutate_if(is.character, factor) %>% 
      data.frame
    
  # complex model    
  } else {
    
    ## Summarizing effects of restoration projects on tn
    tnchg <- get_chg(wqdat, wqmtch, statdat, restdat, wqvar = 'tn', yrdf = yrdf) %>% 
      rename(nival = cval)
    
    # get nitrogen nominal split if not provided
    if(is.null(nitspl)) nitspl <- quantile(tnchg$nival, 0.5, na.rm = T)
    
    # combine all using a super simple approach
    out <- sachg %>% 
      left_join(tnchg, by = c('stat', 'hab_enh', 'hab_est', 'hab_pro', 'non_src', 'pnt_src')) %>% 
      left_join(chchg, by = c('stat', 'hab_enh', 'hab_est', 'hab_pro', 'non_src', 'pnt_src')) %>% 
      mutate(
        salev = cut(saval, breaks = c(-Inf, salspl, Inf), labels = lbs),
        nilev = cut(nival, breaks = c(-Inf, nitspl, Inf), labels = lbs),
        chlev = cut(chval, breaks = c(-Inf, chlspl, Inf), labels = lbs)
      ) %>% 
      # dplyr::select(-saval, -nival, -chval, -stat) %>% 
      mutate_if(is.character, factor) %>% 
      data.frame
    
  }

  # combine cdat with wqmtch for outpout    
  out <- list(cdat = out, wqmtch = wqmtch)
  return(out)
  
}