#' Plot buildings from a BPIP dataframe
#'
#' Plot a BPIP building dataframe.
#' @param data Dataframe of bpip parameters.
#' @keywords plot bpip buildings downwash
#' @export
#' @examples
#' plot_bpip(data = bpip_blds)
# 
#

plot_bpip <- function(data = bpip) {
  
  source_coords <- data[1, ]$source_coords[[1]]

  coord_msg <- TRUE
  
  for(i in 1:nrow(data)) {
    
    if(is.null(data[i, "bld_xcoords"]) | 
       is.null(data[i, "bld_ycoords"]) | 
       length(unlist(data[i, "bld_xcoords"], ",")) < 3 |
       length(unlist(data[i, "bld_ycoords"], ",")) < 3 ) {
      
      if(coord_msg) {
        print("Building vertices were calculated based on a rectanglular footprint. To create a custom building shape provide 3 or more x,y coordinates.")
        
        coord_msg <- FALSE
      }
      
      coords <- bld_coords(source_coords       = data[i, ]$source_coords,
                           dist_from_source    = data[i, ]$dist_from_source,
                           angle_from_source   = data[i, ]$angle_from_source,
                           length              = data[i, ]$length,
                           width               = data[i, ]$width,
                           bld_rotation        = data[i, ]$bld_rotation,
                           angle_units         = data[i, ]$angle_units,
                           show_plot           = FALSE)
      
    
      data[i, ]$bld_xcoords  <- list(coords$x_coords)
      data[i, ]$bld_ycoords  <- list(coords$y_coords)
    }
  }
  
  
  xcoords <- unlist(data$bld_xcoords)
  ycoords <- unlist(data$bld_ycoords)
  #coords  <- c(xcoords, ycoords, source_coords) 
  
  bld_range <- max(abs(source_coords[2] - ycoords), abs(source_coords[1] - xcoords))
  
  graphics::plot(xcoords,
                 ycoords,
                 col  = "steelblue",
                 xlab = paste("South"),
                 ylab = paste("West"),
                 xlim = c(source_coords[1] - bld_range, source_coords[1] + bld_range),
                 ylim = c(source_coords[2] - bld_range, source_coords[2] + bld_range))
  
  for(i in 1:nrow(data)) {
    graphics::polygon(unlist(data[i, ]$BLD_XCOORDS), unlist(data[i,]$BLD_YCOORDS), col = "steelblue") 
    }
  
  graphics::points(source_coords[1], source_coords[2], pch =13, col = "orange", cex=1.8)
}