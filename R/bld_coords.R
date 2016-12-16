#' Generate building coordinates
#'
#' Calculate building coordinates for BPIP.
#' @param source_xy c(x,y) coordinates of emissions source.
#' @keywords building bpip input coords
#' @export
#' @examples
#' bld_coords(source_xy = c(0, 0))
# 
# 

bld_coords <- function(source_xy           = c(0, 0),
                       dist_from_source    = 10,
                       angle_from_source   = 0,
                       width_x             = 5,
                       length_y            = 10,
                       bld_rotation        = 0,
                       angle_units         = "degrees",
                       show_plot           = TRUE
) {
  
  if(angle_units == "degrees") {
    angle_from_source <- -(angle_from_source - 450) * pi/180
    bld_rotation      <- -(bld_rotation - 450) * pi/180
  }
  
  bld_center_x <- source_xy[1] + round(dist_from_source * cos(angle_from_source), 2)
  
  bld_center_y <- source_xy[2] + round(dist_from_source * sin(angle_from_source), 2)
  
  bld_corners <- rotate_bld(center_x    = bld_center_x,
                            center_y    = bld_center_y,
                            width_x     = width_x,
                            length_y    = length_y,
                            rotation    = bld_rotation,
                            angle_units = "radians",
                            show_plot   = FALSE)
  
  if(show_plot) {
    graphics::plot(bld_corners$x_coords,
                   bld_corners$y_coords,
                   col  = "steelblue",
                   xlab = paste("length =", signif(max(bld_corners$x_coords) - min(bld_corners$x_coords), 2)),
                   ylab = paste("width =", signif(max(bld_corners$y_coords) - min(bld_corners$y_coords), 2)),
                   xlim = c(source_xy[1] - dist_from_source - length_y, 
                            source_xy[1] + dist_from_source + length_y),
                   ylim = c(source_xy[1] - dist_from_source - width_x,
                            source_xy[1] + dist_from_source + width_x))
  
    graphics::polygon(bld_corners$x_coords, bld_corners$y_coords, col = "steelblue")
  
    graphics::points(source_xy[1], source_xy[2], pch =16, col = "orange")
  }
  
  return(bld_corners)
  
}
