#' Building options
#'
#' Create an input table of BPIP building options.
#' @param BUILDING Building names. Separate multiple sources with commas.
#' @keywords building bpip input coords
#' @export
#' @examples
#' building_tbl(BUILDING = "Bld_1")
# 
# 

building_tbl <- function(BUILDING = "Bld_1",
                         HEIGHT = "",
                         ELEV = "",
                         XCOORDS = "",
                         YCOORDS = ""
) {

df <- data.frame(BUILDING = BUILDING,
                 HEIGHT   = HEIGHT,
                 ELEV     = ELEV,
                 XCOORDS  = XCOORDS,
                 YCOORDS  = YCOORDS,
                 stringsAsFactors = F)
return(df)
}

##