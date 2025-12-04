# Do a cone search of the PS1 catalog

Do a cone search of the PS1 catalog

## Usage

``` r
ps1_cone(
  ra,
  dec,
  r_arcmin = 0.05,
  table = c("mean", "stack", "detection", "forced_mean"),
  release = c("dr2", "dr1"),
  columns = NULL,
  verbose = FALSE,
  ...
)
```

## Arguments

- ra:

  (degrees) J2000 Right Ascension

- dec:

  (degrees) J2000 Declination

- r_arcmin:

  (arcmins) Search radius (\<= 30 arcmins)

- table:

  "mean"(default), "stack", "detection" or "forced_mean"

- release:

  "dr1" or "dr2"(default)

- columns:

  list of column names to include (NULL means use defaults)

- verbose:

  print info about request

- ...:

  other parameters (e.g., nDetections.min = 2)

## Value

data.frame

## Examples

``` r
if (FALSE) { # \dontrun{
ps1_cone(ra = 139.334,dec = 68.635,r_arcmin = 0.05, nDetections.gt = 1)
} # }
```
