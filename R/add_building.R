#' Add building  (deprecated, suggest using rbind)
#'
#' Create building parameters for BPIP.
#' @param building Building names. Separate multiple sources with commas.
#' @keywords building bpip input coords
#' @export
#' @examples
#' add_building(building = "Bld_1")
# 
# 

add_building <- function(data_table          = NULL,
                         building            = "Bld_1",
                         source_coords       = c(0, 0),
                         dist_from_source    = 20,
                         angle_from_source   = 0,
                         elev                = 0,
                         height              = 10,
                         length              = 5,
                         width               = 10,
                         bld_rotation        = 0,
                         angle_units         = "degrees"
) {
  
  if(angle_units == "degrees") {
    angle_from_source <- angle_from_source * pi/180
    bld_rotation <- bld_rotation * pi/180
  }
  
  bld_center_x <- source_coords[1] - round(distance * cos(angle_from_source), 2)
  
  bld_center_y <- source_coords[2] + round(distance * sin(angle_from_source), 2)
  
  bld_corners <- bld_coords(source_coords       = source_coords,
                            distance            = distance,
                            angle_from_source   = angle_from_source,
                            length              = length,
                            width               = width,
                            bld_rotation        = bld_rotation,
                            angle_units         = "radians")
  
  return(bld_corners)
  
  
}

