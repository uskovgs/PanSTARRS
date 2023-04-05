

panstarrs_user_agent <- function() {
  httr::user_agent(paste0(
    "panstarrs/", utils::packageVersion("panstarrs"), " ",
    "(https://cran.r-project.org/web/packages/panstarrs/)", " ",
    "httr/", utils::packageVersion("httr")
  ))
}


