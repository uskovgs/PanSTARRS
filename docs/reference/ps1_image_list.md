# Get list of images

Query ps1filenames.py service to get a list of images.

## Usage

``` r
ps1_image_list(ra, dec, size = 240, filters = "grizy")
```

## Arguments

- ra:

  ra position in degrees

- dec:

  dec position in degrees

- size:

  image size in pixels (0.25 arcsec/pixel)

- filters:

  string with filters to include

## Value

table with the results

## Details

src: https://ps1images.stsci.edu/ps1image.html

## Examples

``` r
if (FALSE) { # \dontrun{
# Crab nebulae image
ps1_image_list(ra = 83.633210, dec = 22.014460, size = 1280, filters = "grz")
} # }
```
