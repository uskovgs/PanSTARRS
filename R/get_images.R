#' Get list of images
#'
#' Query ps1filenames.py service to get a list of images.
#'
#' src: https://ps1images.stsci.edu/ps1image.html
#'
#' @param ra ra position in degrees
#' @param dec dec position in degrees
#' @param size image size in pixels (0.25 arcsec/pixel)
#' @param filters string with filters to include
#'
#' @return table with the results
#' @export
#'
#' @importFrom RCurl getURL
#' @importFrom readr read_delim
#' @importFrom magrittr %>%
#' @importFrom glue glue
#'
#' @examples
#' \dontrun{
#' # Crab nebulae image
#' ps1_image_list(ra = 83.633210, dec = 22.014460, size = 1280, filters = "grz")
#' }
#'
ps1_image_list <- function(ra, dec, size = 240, filters = "grizy") {
  service <- "https://ps1images.stsci.edu/cgi-bin/ps1filenames.py"

  url <- glue("{service}?ra={ra}&dec={dec}&size={size}&format=fits&filters={filters}")
  df <- url %>%
    getURL() %>%
    read_delim(show_col_types = FALSE)

  return(df)
}


#' Get URL of images
#'
#' @param ra ra position in degrees
#' @param dec dec position in degrees
#' @param size extracted image size in pixels (0.25 arcsec/pixel)
#' @param output_size output (display) image size in pixels (default = size).
#' output_size has no effect for fits format images.
#' @param filters string with filters to include
#' @param format data format (options are "jpg", "png" or "fits")
#' @param color if TRUE, creates a color image (only for jpg or png format). Default is return a list of URLs for single-filter grayscale images.
#'
#' @return string with the URL
#' @export
#'
#' @importFrom attempt stop_if
#' @importFrom attempt stop_if_not
#' @importFrom glue glue
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#' ps1_image_url(
#' ra = 83.633210,
#' dec = 22.014460,
#' size = 1280,
#' format = "jpg",
#' filters = "grz",
#' color = T)
#' }
#'
ps1_image_url <- function(ra, dec, size = 240, output_size = NULL, filters = "grizy", format = "jpg", color = FALSE) {
  stop_if(
    .x = color & format == "fits",
    msg = "color images are available only for jpg or png formats"
  )

  stop_if_not(
    .x = format %in% c("jpg", "png", "fits"),
    msg = "format must be one of jpg, png, fits"
  )

  table <- ps1_image_list(ra, dec, size = size, filters = filters)
  url <- glue(
    "https://ps1images.stsci.edu/cgi-bin/fitscut.cgi?",
    "ra={ra}&dec={dec}&size={size}&format={format}"
  )

  if (!is.null(output_size)) url <- glue(url, "&output_size={output_size}")

  table <- table %>%
    mutate(filter = factor(.data$filter, levels = c("y", "z", "i", "r", "g"))) %>%
    arrange(.data$filter)

  if (color) {
    if (nrow(table) > 3) table <- table[c(1, 2, 3), ]

    color_labels <- c("red", "green", "blue")
    url_color_part <- glue("&{color_labels}={table$filename}") %>%
      paste0(collapse = "")

    url <- paste0(url, url_color_part)
  } else {
    url <- paste0(url, "&red=", table$filename)
  }
  return(url)
}


#' Get grayscale image at a sky position
#'
#'
#' @param ra ra position in degrees
#' @param dec dec position in degrees
#' @param size extracted image size in pixels (0.25 arcsec/pixel)
#' @param output_size output (display) image size in pixels (default = size).
#' output_size has no effect for fits format images.
#' @param filter string with filter to extract (one of grizy)
#' @param format data format (options are "jpg", "png")
#'
#' @return the image
#' @export
#'
#' @importFrom attempt stop_if_not
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' ps1_image_gray(ra = 83.633210, dec = 22.014460, size = 1280, filter = "i")
#' }
ps1_image_gray <- function(ra, dec, size = 240, output_size = NULL, filter = "g", format = "jpg") {
  stop_if_not(format %in% c("jpg", "png"),
    msg = "format must be jpg or png"
  )

  stop_if_not(filter %in% c("g", "r", "i", "z", "y"),
    msg = "filter must be one of grizy"
  )

  url <- ps1_image_url(ra, dec, size = size, filters = filter, output_size = output_size, format = format)

  return(url[1])
}

#' Get color image at a sky position
#'
#' @param ra ra position in degrees
#' @param dec dec position in degrees
#' @param size extracted image size in pixels (0.25 arcsec/pixel)
#' @param output_size output (display) image size in pixels (default = size).
#' output_size has no effect for fits format images.
#' @param filters string with filters to include
#' @param format data format (options are "jpg", "png")
#'
#' @return the image url
#' @export
#'
#' @importFrom attempt stop_if_not
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom magrittr %>%
#'
#' @examples
#' \dontrun{
#' ps1_image_color(ra = 83.633210, dec = 22.014460, size = 1280, filters="grz")
#' }
ps1_image_color <- function(ra, dec, size = 240, output_size = NULL, filters = "grizy", format = "jpg") {
  stop_if_not(format %in% c("jpg", "png"),
    msg = "format must be jpg or png"
  )

  url <- ps1_image_url(ra, dec, size = size, filters = filters, output_size = output_size, format = format, color = TRUE)
  return(url)
}
