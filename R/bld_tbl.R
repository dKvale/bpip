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
                    width_x             = 5,
                    length_y             = 10,
                    bld_rotation        = 0,
                    angle_units         = "degrees",
                    elev                = 0,
                    n_tiers             = 1,
                    bld_xcoords         = c(),
                    bld_ycoords         = c(),
                    dist_from_source    = 20,
                    angle_from_source   = 0,
                    source_name         = "Stack_1",
                    source_xy           = c(0, 0),
                    source_elev         = 0,
                    source_height       = 10
                    ) {

df <- tibble::tibble(prj_title         = prj_title,
                     building          = building,
                     height            = height,
                     width_x           = width_x,
                     length_y          = length_y,
                     bld_rotation      = bld_rotation,
                     angle_units       = angle_units,
                     elev              = elev,
                     n_tiers           = n_tiers,
                     bld_xcoords       = list(bld_xcoords),
                     bld_ycoords       = list(bld_ycoords),
                     dist_from_source  = dist_from_source,
                     angle_from_source = angle_from_source,
                     source_name       = source_name,
                     source_xy         = list(source_xy),
                     source_elev       = source_elev,
                     source_height     = source_height)

return(df)
}

##