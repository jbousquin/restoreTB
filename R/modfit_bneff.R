library(tidyverse)
source('R/funcs.R')

# fit simple models explaining chl response to mtch and yrdf combos, as fixed linear effects, no interactions
modout <- grdsres %>% 
  filter(chlev %in% 'lo') %>% 
  group_by(resgrp, yrs, salev, project) %>% 
  nest %>% 
  mutate(
    mod = purrr::map(data, function(x){
      # browser()
      # model only
      datin <<- na.omit(x)
      
      modsel <- lm(chg ~ yrdf + mtch, data = datin, na.action = na.pass) %>% 
          dredge %>% 
          get.models(subset = 1) %>% 
          .[[1]]
      
      return(modsel)
      
    }),
    prd = purrr::map(mod, function(x){
      
      datin <- crossing(
        mtch = 1:10, 
        yrdf = 1:10
      )
      
      prdout <- predict(x, datin, se = T) %>% 
        cbind(datin) %>% 
        dplyr::select(-df, -residual.scale) %>% 
        mutate(
          hi = fit + se.fit, 
          lo = fit - se.fit
        )
      
      return(prdout)
      
    }), 
    coef = purrr::map(mod, function(x){
      
      out <- summary(x)$coefficients %>% 
        data.frame %>% 
        rownames_to_column('est') %>% 
        .[, c(1, 2, 5)] %>% 
        mutate(
          `Pr...t..` = p_ast(`Pr...t..`), 
          Estimate = round(Estimate, 2)
          ) %>% 
        unite('Estimate', Estimate, `Pr...t..`, sep = '') %>% 
        gather('var', 'val', Estimate)
      
      return(out)
        
      
    })
    
  )

save(modout, file = 'data/modout.RData')

coeftab <- modout %>% 
  select(-mod, -data, -prd) %>% 
  unnest %>% 
  select(-resgrp, -yrs, -var) %>% 
  spread(est, val, fill = '-')

# inputs
toploprd <- modout %>% 
  select(-mod, -data, -coef) %>% 
  unnest %>% 
  filter(mtch %in% c(1, 10))

toplopts <- modout %>% 
  select(-mod, -prd, -coef) %>% 
  unnest %>% 
  filter(mtch %in% c(1, 10))

p1 <- ggplot() +
  geom_ribbon(data = toploprd, aes(x = yrdf, ymin = lo, ymax = hi, group = mtch), alpha = 0.5, fill = 'grey') +
  geom_line(data = toploprd, aes(x = yrdf, y = fit, group = mtch, colour = mtch), size = 1) + 
  geom_point(data = toplopts, aes(x = yrdf, y = chg, colour = mtch)) +
  theme_bw() +
  scale_x_continuous('Year difference windows') +
  scale_colour_gradientn(colours = c('tomato1', 'blue')) + 
  scale_y_continuous('Likelihood of low chlorophyll') +
  facet_grid(salev ~ project) +
  geom_hline(yintercept = 0)

p1