#' Add building
#'
#' Create building parameters for BPIP.
#' @param BUILDING Building names. Separate multiple sources with commas.
#' @keywords building bpip input coords
#' @export
#' @examples
#' add_building(BUILDING = "Bld_1")
# 
# 

add_building <- function(BUILDING       = "Bld_1",
                         SOURCE_COORDS  = c(0, 0),
                         DISTANCE       = 20,
                         ANGLE_DEGREES  = 10,
                         ELEV           = 1,
                         HEIGHT         = 10,
                         LENGTH         = 5,
                         WIDTH          = 10
) {
  
  ANGLE_RADS <- ANGLE_DEGREES * pi/180
  
  bld_center_x <- SOURCE_COORDS[1] - round(DISTANCE * cos(ANGLE_RADS), 2)
  
  bld_center_y <- SOURCE_COORDS[2] + round(DISTANCE * sin(ANGLE_RADS), 2)
  
  bld_coords <- rectangle(center_x = bld_center_x,
                          center_y = bld_center_y,
                          length = LENGTH,
                          width  = WIDTH)
  
  plot(c(SOURCE_COORDS[1], bld_coords$XCOORDS),
       c(SOURCE_COORDS[2], bld_coords$YCOORDS),
       col  = c("red", rep("blue", 4)),
       pch  = 16,
       xlab = paste("length =", LENGTH),
       ylab = paste("width =", WIDTH),
       xlim = c(SOURCE_COORDS[1] - DISTANCE - WIDTH, 
                SOURCE_COORDS[1] + DISTANCE + WIDTH),
       ylim = c(SOURCE_COORDS[1] - DISTANCE - LENGTH,
                SOURCE_COORDS[1] + DISTANCE + LENGTH))
  
  
  return(bld_coords)
  
}

