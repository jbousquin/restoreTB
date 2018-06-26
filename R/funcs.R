######
# get legend from an existing ggplot object
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# Data to plot for effects
#
# modin lm model
# cvar chr string of variable to hold constant
# pos is where the labels are, left or right of effects line
# fct is scaling factor for labels from end of lines
get_pldat <- function(modin, cvar, pos = c('left', 'right'), fct = NULL){
  
  pos <- match.arg(pos)
  
  # crossing of model data by range
  x <- modin$model %>% 
    .[, -1] %>% 
    data.frame %>% 
    as.list %>% 
    map(range) %>%
    map(function(x) seq(x[1], x[2], length = 100))
  
  # quantiles for cvar
  x[[cvar]] <- modin$model[[cvar]]%>% quantile(., c(0, 1))
  
  # make data frame
  nms <- names(x) 
  x <- crossing(x[[1]], x[[2]])
  names(x) <- nms
  x <- x[, c(names(x)[!names(x) %in% cvar], cvar)]
  
  # get predictions, combine with exp vars
  prd_vl <- predict(modin, newdata = x, se = T) %>% 
    data.frame(., x) %>% 
    dplyr::select(-df, -residual.scale) %>% 
    mutate(
      hi = fit + se.fit, 
      lo = fit - se.fit
    )
  names(prd_vl)[1] <- all.vars(formula(modin))[1]
  
  # min x axis values for quantile labels
  yvar <- names(prd_vl)[1]
  xvar <- all.vars(formula(modin))
  xvar <- xvar[!xvar %in% c(yvar, cvar)]
  
  locs <- prd_vl %>% 
    group_by(.dots = list(cvar))   
  if(pos == 'right'){
    if(is.null(fct)) fct <- 1.05
    locs <- filter(locs, row_number() == n())
  } else {
    if(is.null(fct)) fct <- 0.95
    locs <- filter(locs, row_number() == 1)
  }
  
  yval <- locs[[yvar]]
  xval <- locs[[xvar]] %>% unique %>% `*`(fct)
  xlab <- data.frame(
    lab = c('Max', 'Min'), 
    x = xval, y = yval,
    stringsAsFactors = F)
  dr <- locs[[cvar]] %>% range %>% diff %>% sign
  if(dr == 1) xlab$lab <- rev(xlab$lab)
  
  # output
  out <- list(prd_vl = prd_vl, xlab = xlab)
  return(out)
  
}