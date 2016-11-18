#' Building parameters
#'
#' Create an input table of building parameters for BPIP.
#' @param building Building names. Separate multiple sources with commas.
#' @param dist_from_source Distance from center of source to the center of the building. 
#' @keywords building bpip input coords
#' @export
#' @examples
#' bld_tbl(building = "Bld_1")
# 
# 

bld_tbl <- function(prj_title           = "BPIP input",
                    building            = "Bld_1",
                    height              = 10,
                    length              = 5,
                    width               = 10,
                    bld_rotation        = 0,
                    angle_units         = "degrees",
                    elev                = 0,
                    n_tiers             = 1,
                    bld_xcoords         = c(),
                    bld_ycoords         = c(),
                    dist_from_source    = 20,
                    angle_from_source   = 0,
                    source_name         = "Stack_1",
                    source_coords       = c(0, 0),
                    source_elev         = 0,
                    source_height       = 10
                    ) {

df <- tibble::tibble(PRJ_TITLE         = prj_title,
             BUILDING          = building,
             HEIGHT            = height,
             LENGTH            = length,
             WIDTH             = width,
             BLD_ROTATION      = bld_rotation,
             ANGLE_UNITS       = angle_units,
             ELEV              = elev,
             N_TIERS           = n_tiers,
             BLD_XCOORDS       = list(bld_xcoords),
             BLD_YCOORDS       = list(bld_ycoords),
             DIST_FROM_SOURCE  = dist_from_source,
             ANGLE_FROM_SOURCE = angle_from_source,
             SOURCE_NAME       = source_name,
             SOURCE_COORDS     = list(source_coords),
             SOURCE_ELEV       = source_elev,
             SOURCE_HEIGHT     = source_height)

return(df)
}

##