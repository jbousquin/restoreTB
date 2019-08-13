
# README

Materials for the evaluation of restoration projects in Tampa Bay.

Repository DOI: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.2539623.svg)](https://doi.org/10.5281/zenodo.2539623)

Manuscript: <https://doi.org/10.1007/s12237-019-00619-w>

# Data

Files in `/data/` all created using `R/dat_proc.R`, unless otherwise noted

* `grdave.RData` grid analyses of year/site match combos for average differences of all stations in TB, for evaluation of all differences section in `ind_eval.Rmd` 

* `grdavemanu.RData` grid analyses of year/site match combos for average differences of all stations in TB, similar to `grdave.RData` but  addition of subsets by bay segment, early/late/full time series, and only 5/10 combos for slice and mtches 

* `grdsres.RData` grid analyses of year/site match combos for different bn models, created in `R/source_me.R`

* `map.RData` base map for plots in `all_eval.Rmd` created in same file

* `restdat.RData` restoration projects combined

* `reststat.RData` location of restoration projects combined

* `rndsims.RData` nested tibble of results for random simulations 

* `tb_seg.RData` sf object of Tampa Bay segments

* `wqdat.RData` TB water quality data by site, date, var, val

* `wqmtch.RData` Match between water quality stations and restoration site locations, for manuscript

* `wqstat.RData` TB water quality stations as lat/lon

# Shiny application

View results: [link](http://www.fawda123.com/shiny/restorebayes/ind_eval.Rmd)
