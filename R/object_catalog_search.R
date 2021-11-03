#' Check legal
#'
#' Checks if this combination of table and release is acceptable.
#'
#' @param table "mean", "stack", "detection"
#' @param release "dr2", "dr1"
#'
#'
checklegal <- function(table, release) {
  releaselist <- c("dr1", "dr2")


  attempt::stop_if_not(release %in% releaselist,
                       msg = glue::glue(
                         "Bad value for release (must be one of",
                         paste(releaselist, collapse = ", "),
                         ")"
                       )
  )
  if (release == "dr1") {
    tablelist <- c("mean", "stack")
  } else {
    tablelist <- c("mean", "stack", "detection")
  }

  attempt::stop_if_not(table %in% tablelist,
                       msg = glue::glue("Bad value for table (for {release} must be one of {
                                        paste(tablelist,  collapse = ', ')
                     })")
  )
}

#' Metadata from PS1
#'
#' Return metadata for the specified catalog and table
#'
#' @param table "mean", "stack", or "detection"
#' @param release "dr1" or "dr2"(default)
#'
#' @return Returns data.frame with columns: name, type, description
#' @export
#'
#' @importFrom glue glue
#' @importFrom httr GET
#' @importFrom httr content
#' @importFrom httr stop_for_status
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr select
#'
#' @examples
#' \dontrun{
#' ps1_metadata()
#' }
ps1_metadata <- function(table = c("mean", "stack", "detection"), release = c("dr2", "dr1")) {

  table <- table[1]
  release <- release[1]

  baseurl <- "https://catalogs.mast.stsci.edu/api/v0.1/panstarrs"

  checklegal(table, release)

  resp <- GET(url =  glue("{baseurl}/{release[1]}/{table[1]}/metadata"))

  stop_for_status(resp)


  meta_info <- resp %>%
    content(as = "text") %>%
    fromJSON() %>%
    select(name, type, description)

  return(meta_info)
}


#' Do a general search of the PS1 catalog (possibly without ra/dec/radius)
#'
#' @param table "mean", "stack", or "detection"
#' @param release "dr1" or "dr2"(default)
#' @param columns list of column names to include (NULL means use defaults)
#' @param verbose print info about request
#' @param ... other parameters (e.g., nDetections.min = 2).
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' ps1_search(table='detection',release='dr2',objid = '190361393344112894')
#'
#' ps1_search(table='mean',release='dr2',objid = '190361393344112894', columns = c('objName', 'raMean', 'decMean', 'rMeanPSFMag'))
#' }
ps1_search <- function(table = c("mean", "stack", "detection"),
                       release = c("dr2", "dr1"),
                       columns = NULL,
                       verbose = FALSE,
                       ...) {

  release <- release[1]
  tabel <- table[1]



  baseurl = "https://catalogs.mast.stsci.edu/api/v0.1/panstarrs"

  data <-  list(...)

  attempt::stop_if(length(data) == 0,
                   msg = "You must specify some parameters for search")

  checklegal(table, release)

  # attempt::stop_if_not(format %in% c("csv","votable","json"),
  #                      msg = "Bad value for format")

  url <- glue::glue("{baseurl}/{release}/{table}.json")


  if(!is.null(columns)){
    # check that column values are legal
    # create a dictionary to speed this up


    cols_meta <- ps1_metadata(table,release)$name %>% tolower()

    columns2 <- columns %>% tolower() %>% stringr::str_squish()

    badcols <- columns[which(!(columns2 %in% cols_meta))]


    attempt::message_if(length(badcols) != 0,
                        msg = glue::glue("Some columns not found in table: {
                                          paste(badcols, collapse = ', ')
                         }"))

    data['columns'] <- columns %>% jsonlite::toJSON()


  }

  resp <-  httr::GET(url, query = data)

  if(verbose)
    print(resp)

  httr::stop_for_status(resp)

  cont <- resp %>%
    httr::content(as='text') %>%
    jsonlite::fromJSON(simplifyVector = F)

  json_colnames <- cont$info %>% purrr::map_chr(~.x$name)
  json_df <- cont$data %>%
    purrr::map_dfr(~purrr::set_names(.x, nm = json_colnames))

  return(json_df)
}

#' Do a cone search of the PS1 catalog
#'
#' @param ra (degrees) J2000 Right Ascension
#' @param dec (degrees) J2000 Declination
#' @param r_arcmin (arcmins) Search radius (<= 30 arcmins)
#' @param table "mean"(default), "stack", or "detection"
#' @param release "dr1" or "dr2"(default)
#' @param columns list of column names to include (NULL means use defaults)
#' @param verbose print info about request
#' @param ... other parameters (e.g., nDetections.min = 2)
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' ps1_cone(ra = 139.334,dec = 68.635,r_arcmin = 0.05, nDetections.gt = 1)
#' }
ps1_cone <- function(ra,
                    dec,
                    r_arcmin = 0.05,
                    table = c("mean", "stack", "detection"),
                    release = c("dr2", "dr1"),
                    columns=NULL,
                    verbose=FALSE,
                    ...){
  ps1_search(table=table[1],
             release=release[1],
             columns=columns,
             verbose=verbose,
             ra = ra,
             dec = dec,
             radius = r_arcmin/60.0)

}



#' Do a cross-match with PS1 catalog
#'
#' @param ra (degrees) numeric vector of J2000 Right Ascension
#' @param dec (degrees) numeric vector of J2000 Declination
#' @param r_arcmin (arcmins) Search radius (<= 30 arcmins)
#' @param table "mean"(default), "stack", or "detection"
#' @param release "dr1" or "dr2"(default)
#' @param verbose print info about request
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' ps1_crossmatch(ra = c(268.70342, 168.87258), dec = c(71.54292, 60.75153))
#' }
ps1_crossmatch <- function(ra,
                           dec,
                           r_arcmin = 0.05,
                           table = c("mean", "stack", "detection"),
                           release = c("dr2", "dr1"),
                           verbose = FALSE){

  base_url <- 'https://catalogs.mast.stsci.edu/api/v0.1/panstarrs'

  table <- table[1]
  release <- release[1]

  checklegal(table, release)

  attempt::stop_if(length(ra) != length(dec),
                   msg = glue::glue("Length of ra [{length(ra)}] is not equal to length of dec [{length(dec)}]"))

  attempt::stop_if(length(ra) > 5000,
                   msg = glue::glue("The length of the sources [{length(ra)}] is more tha 5000 items."))

  sources_json <- data.frame(ra = ra, dec = dec) %>%
    jsonlite::toJSON()

  resp <- httr::POST(
    url = glue::glue('{base_url}/{release}/{table}/crossmatch/'),
    query = list(
      resolve = FALSE,
      radius = r_arcmin/60,
      ra_name = "ra",
      dec_name = "dec",
      # target_name = 'target',
      targets = sources_json
    ))

  httr::stop_for_status(resp)

  if(verbose)
    print(resp)

  resp %>%
    httr::content(as = 'text') %>%
    jsonlite::fromJSON() %>%
    .$data %>%
    dplyr::as_tibble()
}

#' Get the RA and Dec for objects from PanSTARRS catalog.
#'
#' Only works for "north" objects with decl > -30. For all objects see function `ps1_mast_resolve`.
#'
#' @param target_names character vector of target names (see example)
#' @param full_table show full cross-matched table or only main columns.
#' @param verbose print info about request
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' ps1_resolve(c('Andromeda', "SN 2005D", 'Antennae', 'ANTENNAE'))
#' }
ps1_resolve <- function(target_names,
                        full_table = FALSE,
                        verbose = FALSE){

  table <- "mean"
  release <- "dr2"
  base_url <- 'https://catalogs.mast.stsci.edu/api/v0.1/panstarrs'

  checklegal(table, release)
  # Create json list

  attempt::stop_if(length(target_names) > 500,
                   msg = glue::glue("The length of the `target_names`
                                    [{length(target_names)}] is more than 5000 items."))


  sources_df <- data.frame(target = target_names,
                           search_id = 0:(length(target_names)-1))

  sources_json <- sources_df %>% select(target) %>% jsonlite::toJSON()

  response <- httr::POST(
    url = glue::glue('{base_url}/{release}/{table}/crossmatch/'),
    query = list(
      resolve = TRUE,
      target_name = 'target',
      targets = sources_json
    ))

  httr::stop_for_status(response)

  if(verbose)
    print(response)

  df <- response %>%
    httr::content(as = 'text') %>%
    jsonlite::fromJSON() %>%
    .$data %>%
    dplyr::rename(ra=`_ra_`, dec=`_dec_`) %>%
    dplyr::left_join(sources_df, by=c('_searchID_'='search_id')) %>%
    dplyr::select(target, everything()) %>%
    dplyr::select(target, ra, dec) %>%
    dplyr::group_by(target) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  df
}


#' Perform a MAST query.
#'
#' @param request (list): The MAST request json object
#'
#' @return Returns response
#'
ps1_mast_query <- function(request){
  # """Perform a MAST query.
  #
  # Parameters
  # ----------
  # request (dictionary): The MAST request json object
  #
  # Returns head,content where head is the response HTTP headers, and content is the returned data
  # """

  # Encoding the request as a json string
  requestString <- request %>% jsonlite::toJSON(auto_unbox = TRUE)

  resp <- httr::GET(url = "https://mast.stsci.edu/api/v0/invoke",
            query = list(request=requestString),
            encode='form')

  return(resp)
}

#' Get the RA and Dec for an object using the MAST name resolver
#'
#' @param name Name of object
#'
#' @return list of ra, decl
#' @export
#'
#' @examples
#' \dontrun{
#' ps1_mast_resolve('Acrux')
#' }
ps1_mast_resolve <- function(name) {
  resolverRequest <- list(
    "service" = "Mast.Name.Lookup",
    "params" = list(
      "input" = name[1],
      "format" = "json"
    )
  )

  mastquery <- ps1_mast_query(resolverRequest)
  httr::stop_for_status(mastquery)

  # The resolver returns a variety of information about the resolved object,
  # however for our purposes all we need are the RA and Dec


  resolvedObject <- httr::content(mastquery)

  coords <- attempt::try_catch(
    resolvedObject$resolvedCoordinate[[1]][c("ra", "decl")],
    .e = ~message(glue::glue("Unknown object '{name}'"))
  )

  return(coords)
}


