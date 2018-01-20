#' A BPIP data frame of building parameters
#'
#' Create an input table of building parameters for BPIP.
#' @param bld_id Building ID or name.
#' @param bld_height Height of building in meters. 
#' @param width_x Width of building from East to West in meters. 
#' @param length_y Length of building from North to South in meters. 
#' @param bld_rotation Building rotation from North in degrees. 
#' @param dist_from_source Distance from center of source to center of building in meters. 
#' @param angle_from_source Angle between center of building and center of source in degrees. 
#'                          Straight North is "0".
#'                          Straight East is "90". 
#' @keywords building bpip input 
#' @export
#' @examples
#' new_bpip(bld_id = "Boiler house")
# 
new_bpip <- function(prj_title           = "BPIP input",
                     bld_id              = "Bld_1",
                     bld_height          = 10,
                     width_x             = 5,
                     length_y            = 10,
                     bld_rotation        = 0,
                     angle_units         = "degrees",
                     bld_elev            = 0,
                     n_tiers             = 1,
                     bld_xcoords         = as.numeric(NA),
                     bld_ycoords         = as.numeric(NA),
                     dist_from_source    = 20,
                     angle_from_source   = 0,
                     source_name         = "Stack_1",
                     source_xy           = c(0, 0),
                     source_elev         = 0,
                     source_height       = 10
                     ) {

df <- tibble::tibble(prj_title         = prj_title,
                     bld_id            = bld_id,
                     bld_height        = bld_height,
                     width_x           = width_x,
                     length_y          = length_y,
                     bld_rotation      = bld_rotation,
                     angle_units       = angle_units,
                     bld_elev          = bld_elev,
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