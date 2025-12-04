# Get URL of images

Get URL of images

## Usage

``` r
ps1_image_url(
  ra,
  dec,
  size = 240,
  output_size = NULL,
  filters = "grizy",
  format = "jpg",
  color = FALSE
)
```

## Arguments

- ra:

  ra position in degrees

- dec:

  dec position in degrees

- size:

  extracted image size in pixels (0.25 arcsec/pixel)

- output_size:

  output (display) image size in pixels (default = size). output_size
  has no effect for fits format images.

- filters:

  string with filters to include

- format:

  data format (options are "jpg", "png" or "fits")

- color:

  if TRUE, creates a color image (only for jpg or png format). Default
  is return a list of URLs for single-filter grayscale images.

## Value

string with the URL

## Examples

``` r
if (FALSE) { # \dontrun{
ps1_image_url(
ra = 83.633210,
dec = 22.014460,
size = 1280,
format = "jpg",
filters = "grz",
color = T)
} # }
```
