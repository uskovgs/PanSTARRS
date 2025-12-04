# Get color image at a sky position

Get color image at a sky position

## Usage

``` r
ps1_image_color(
  ra,
  dec,
  size = 240,
  output_size = NULL,
  filters = "grizy",
  format = "jpg"
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

  data format (options are "jpg", "png")

## Value

the image url

## Examples

``` r
if (FALSE) { # \dontrun{
ps1_image_color(ra = 83.633210, dec = 22.014460, size = 1280, filters="grz")
} # }
```
