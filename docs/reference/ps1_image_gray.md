# Get grayscale image at a sky position

Get grayscale image at a sky position

## Usage

``` r
ps1_image_gray(
  ra,
  dec,
  size = 240,
  output_size = NULL,
  filter = "g",
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

- filter:

  string with filter to extract (one of grizy)

- format:

  data format (options are "jpg", "png")

## Value

the image

## Examples

``` r
if (FALSE) { # \dontrun{
ps1_image_gray(ra = 83.633210, dec = 22.014460, size = 1280, filter = "i")
} # }
```
