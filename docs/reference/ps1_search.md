# Do a general search of the PS1 catalog (possibly without ra/dec/radius)

Do a general search of the PS1 catalog (possibly without ra/dec/radius)

## Usage

``` r
ps1_search(
  table = c("mean", "stack", "detection", "forced_mean"),
  release = c("dr2", "dr1"),
  columns = NULL,
  verbose = FALSE,
  ...
)
```

## Arguments

- table:

  "mean", "stack", "detection" or "forced_mean"

- release:

  "dr1" or "dr2"(default)

- columns:

  list of column names to include (NULL means use defaults)

- verbose:

  print info about request

- ...:

  other parameters (e.g., nDetections.min = 2).

## Value

data.frame

## Examples

``` r
if (FALSE) { # \dontrun{
ps1_search(
table='detection',
release='dr2',
objid = '190361393344112894')

ps1_search(
table='mean',
release='dr2',
objid = '190361393344112894',
columns = c('objName', 'raMean', 'decMean', 'rMeanPSFMag'))
} # }
```
