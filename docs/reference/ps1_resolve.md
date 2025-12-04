# Get the RA and Dec for objects from PanSTARRS catalog.

Only works for "north" objects with decl \> -30. For all objects see
function \`ps1_mast_resolve\`.

## Usage

``` r
ps1_resolve(target_names, verbose = FALSE)
```

## Arguments

- target_names:

  character vector of target names (see example)

- verbose:

  print info about request

## Value

data.frame

## Examples

``` r
if (FALSE) { # \dontrun{
ps1_resolve(c('Andromeda', "SN 2005D", 'Antennae', 'ANTENNAE'))
} # }
```
