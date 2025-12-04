# Do a cross-match with PS1 catalog

Do a cross-match with PS1 catalog

## Usage

``` r
ps1_crossmatch(
  ra,
  dec,
  r_arcmin = 0.05,
  table = c("mean", "stack", "detection", "forced_mean"),
  release = c("dr2", "dr1"),
  verbose = FALSE
)
```

## Arguments

- ra:

  (degrees) numeric vector of J2000 Right Ascension

- dec:

  (degrees) numeric vector of J2000 Declination

- r_arcmin:

  (arcmins) Search radius (\<= 30 arcmins)

- table:

  "mean"(default), "stack", "detection", "forced_mean"

- release:

  "dr1" or "dr2"(default)

- verbose:

  print info about request

## Value

data.frame

## Examples

``` r
if (FALSE) { # \dontrun{
ps1_crossmatch(ra = c(268.70342, 168.87258), dec = c(71.54292, 60.75153))
} # }
```
