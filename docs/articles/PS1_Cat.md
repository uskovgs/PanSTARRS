# How to work with catalogs

``` r
library(panstarrs)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(data.table)
#> 
#> Attaching package: 'data.table'
#> The following objects are masked from 'package:dplyr':
#> 
#>     between, first, last
```

## Example 1

### Get MeanPSFMag in the grizy filters from PS1 catalog for the KQ Uma source.

#### 1. Get coords for the source

``` r
coords <- ps1_mast_resolve('KQ Uma')
coords
#> $ra
#> [1] 139.3345
#> 
#> $decl
#> [1] 68.63509
```

#### 2. Search sources in the circle around the KQ position

``` r
df_cone <- ps1_cone(
  coords$ra, 
  coords$decl,
  r_arcmin = 0.01, 
  table = 'mean',
  release = 'dr2'
)

# tidyverse approach
df_cone |>
  dplyr::select(dplyr::matches('[grizy]MeanPSFMag$'))
#>    gMeanPSFMag rMeanPSFMag iMeanPSFMag zMeanPSFMag yMeanPSFMag
#>          <num>       <num>       <num>       <num>       <num>
#> 1:          NA          NA          NA          NA          NA

# or if you prefer data.table approach
df_cone[, .SD, .SDcols = grepl('[grizy]MeanPSFMag$', names(df_cone))]
#>    gMeanPSFMag rMeanPSFMag iMeanPSFMag zMeanPSFMag yMeanPSFMag
#>          <num>       <num>       <num>       <num>       <num>
#> 1:          NA          NA          NA          NA          NA
```
