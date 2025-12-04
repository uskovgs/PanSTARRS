# Metadata from PS1

Return metadata for the specified catalog and table

## Usage

``` r
ps1_metadata(table = "mean", release = "dr2")
```

## Arguments

- table:

  "mean", "stack", "forced_mean" or "detection"

- release:

  "dr1" or "dr2"(default)

## Value

Returns data.frame with columns: name, type, description

## Examples

``` r
if (FALSE) { # \dontrun{
ps1_metadata()
} # }
```
