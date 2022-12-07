most_recent_season <- function() {
  dplyr::if_else(
    as.double(substr(Sys.Date(), 6, 7)) >= 9,
    as.double(substr(Sys.Date(), 1, 4)),
    as.double(substr(Sys.Date(), 1, 4)) - 1
  )
}

#' @import dbplyr
NULL

#' @import utils
utils::globalVariables(
  c(
    "where"
  )
)
