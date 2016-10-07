#' Generate building coordinates
#'
#' Calculate building coordinates for BPIP.
#' @param building Building names. Separate multiple sources with commas.
#' @keywords building bpip input coords
#' @export
#' @examples
#' bld_coords(building = "Bld_1")
# 
# 

bld_coords <- function(source_coords       = c(0, 0),
                       dist_from_source    = 20,
                       angle_from_source   = 0,
                       length              = 5,
                       width               = 10,
                       bld_rotation        = 0,
                       angle_units         = "degrees",
                       show_plot           = TRUE
) {
  
  if(angle_units == "degrees") {
    angle_from_source <- angle_from_source * pi/180
    bld_rotation <- bld_rotation * pi/180
  }
  
  bld_center_x <- source_coords[1] - round(dist_from_source * cos(angle_from_source), 2)
  
  bld_center_y <- source_coords[2] + round(dist_from_source * sin(angle_from_source), 2)
  
  bld_corners <- rectangle(center_x    = bld_center_x,
                           center_y    = bld_center_y,
                           length      = length,
                           width       = width,
                           rotation    = bld_rotation,
                           angle_units = "radians",
                           show_plot   = FALSE)
  
  if(show_plot) {
  plot(bld_corners$XCOORDS,
       bld_corners$YCOORDS,
       col  = "steelblue",
       xlab = paste("length =", signif(max(bld_corners$XCOORDS) - min(bld_corners$XCOORDS), 2)),
       ylab = paste("width =", signif(max(bld_corners$YCOORDS) - min(bld_corners$YCOORDS), 2)),
       xlim = c(source_coords[1] - dist_from_source - width, 
                source_coords[1] + dist_from_source + width),
       ylim = c(source_coords[1] - dist_from_source - length,
                source_coords[1] + dist_from_source + length))
  
  polygon(bld_corners$XCOORDS, bld_corners$YCOORDS, col = "steelblue")
  
  points(source_coords[1], source_coords[2], pch =16, col = "orange")
  }
  
  return(bld_corners)
  
}
