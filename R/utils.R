

panstarrs_user_agent <- function() {
  httr::user_agent(paste0(
    "panstarrs/", utils::packageVersion("panstarrs"), " ",
    "(https://cran.r-project.org/web/packages/panstarrs/)", " ",
    "httr/", utils::packageVersion("httr")
  ))
}

#' Check ra, dec params
#'
#' @param ra (degrees) Right Ascension
#' @param dec (degrees) Declination
#' @param .length length of coordinates. If `NULL` (default) then coordinates
#' can be any size.
#'
#' @return If the check is successful, the function returns nothing.
#'
validate_radec <- function(ra, dec, .length = NULL) {

  attempt::stop_if(
    length(ra) != length(dec),
    msg = paste0(
      "Length of ra [", length(ra), "] ",
      "is not equal to length of dec [", length(dec), "]"
    )
  )


  if (! is.null(.length)) {
    attempt::stop_if(
      length(ra) != .length,
      msg = paste0("The length of the coordinates array must be", .length)
    )
  }

  attempt::stop_if_any(
    ra < 0 | ra > 360,
    msg = "The `ra` must be in the range [0, 360]"
  )
  attempt::stop_if_any(
    dec < -90 | dec > 90,
    msg = "The `dec` must be in the range [-90, 90]"
  )
}

