## New version v0.2.0

This is a new release. In this version:

-   Now the package follows the CRAN policy 'Packages which use Internet resources should fail gracefully with an informative message. if the resource is not available or has changed (and not give a check warning nor error).'

-   If there is no internet connection or the server API does not work, then `invisible(NULL)` is returned and an error message appears.

-   Some dependencies have been removed: attempt, dplyr, glue, magrittr, purrr, Rcurl, readr, rlang, stringr.

-   HTTP User Agent "panstarrs/{version} (<https://CRAN.R-project.org/package=panstarrs>)" has been added to GET/POST requests.

-   Vignettes have been updated.

-   Some internal functions has been removed.
