# HEFTcom24-Analysis

Analysis of the Hybrid Renewable Energy Forecasting and Tracing Competition 2024 to be included in a forthcomming paper.

To run `analysis.R`, the following files should be downloaded from [Zenodo](https://doi.org/10.5281/zenodo.13950764) (not published yet, [contact Jethro](mailto:jethro.browell@glasgow.ac.uk) for access) and added to the `data/` directory:
- trades.csv
- pinball.csv
- forecasts.csv
- Energy_Data_20200920_20240118.csv
- Energy_Data_20240119_20240519.csv
- overall_leaderboard.csv
- HEFTcom Reports.csv


## R Environment Info

`analysis.R` was last run using R Studio and package versions listed below.


RStudio 2023.09.1+494 "Desert Sunflower" Release (cd7011dce393115d3a7c3db799dda4b1c7e88711, 2023-10-16) for windows
Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) RStudio/2023.09.1+494 Chrome/116.0.5845.190 Electron/26.2.4 Safari/537.36


```r
> sessionInfo()
R version 4.2.1 (2022-06-23 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 26100)

Matrix products: default

locale:
[1] LC_COLLATE=English_United Kingdom.utf8  LC_CTYPE=English_United Kingdom.utf8    LC_MONETARY=English_United Kingdom.utf8
[4] LC_NUMERIC=C                            LC_TIME=English_United Kingdom.utf8    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] patchwork_1.2.0   latex2exp_0.9.6   xtable_1.8-4      ggridges_0.5.4    ggplot2_3.5.1     rstudioapi_0.14  
[7] data.table_1.15.4 dplyr_1.1.4      

loaded via a namespace (and not attached):
 [1] pillar_1.9.0       compiler_4.2.1     RColorBrewer_1.1-3 tools_4.2.1        lattice_0.20-45    nlme_3.1-157      
 [7] viridisLite_0.4.2  lifecycle_1.0.4    tibble_3.2.1       gtable_0.3.5       mgcv_1.8-40        pkgconfig_2.0.3   
[13] rlang_1.1.1        Matrix_1.5-1       cli_3.6.2          withr_3.0.0        stringr_1.5.1      generics_0.1.3    
[19] vctrs_0.6.5        systemfonts_1.0.4  grid_4.2.1         tidyselect_1.2.1   glue_1.6.2         R6_2.5.1          
[25] textshaping_0.3.6  fansi_1.0.3        tidyr_1.3.1        purrr_1.0.2        farver_2.1.2       magrittr_2.0.3    
[31] splines_4.2.1      scales_1.3.0       colorspace_2.1-0   labeling_0.4.3     ragg_1.2.4         utf8_1.2.2        
[37] stringi_1.7.8      munsell_0.5.1      crayon_1.5.3 
```