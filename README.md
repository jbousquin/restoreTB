
# README

Materials for the evaluation of restoration projects in Tampa Bay.

# Data

Files in `/data/` all created using `R/dat_proc.R`, unless otherwise noted

* `grdave.RData` grid analyses of year/site match combos for average differences of all stations in TB, for evaluation of all differences section in `ind_eval.Rmd` 

* `grdsres.RData` grid analyses of year/site match combos for different bn models, created in `R/source_me.R`

* `map.RData` base map for plots in `all_eval.Rmd` created in same file

* `restdat.RData` restoration projects combined

* `reststat.RData` location of restoration projects combined

* `wqdat.RData` TB water quality data by site, date, var, val

* `wqmtch.RData` Match between water quality stations and restoration site locations, for manuscript

* `wqstat.RData` TB water quality stations as lat/lon

# Content

Evaluation of BN models: [link](http://162.243.131.102:3838/restorebayes/all_eval.Rmd)

Evaluation of data aggregation: [link](http://162.243.131.102:3838/restorebayes/ind_eval.Rmd)
