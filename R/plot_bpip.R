#' Plot buildings from a BPIP dataframe.
#'
#' Plot a BPIP building dataframe.
#' @param path File location. Default is "bpip.inp" in the working directory.
#' @keywords plot bpip buildings downwash
#' @export
#' @examples
#' plot_bpip(data = bpip_blds)
# 
#

plot_bpip <- function(data = bpip) {
  
  source_coords <- data[1, ]$SOURCE_COORDS[[1]]

  coord_msg <- TRUE
  
  for(i in 1:nrow(data)) {
    
    if(is.null(data[i, "BLD_XCOORDS"]) | 
       is.null(data[i, "BLD_YCOORDS"]) | 
       length(unlist(data[i, "BLD_XCOORDS"], ",")) < 3 |
       length(unlist(data[i, "BLD_YCOORDS"], ",")) < 3 ) {
      
      if(coord_msg) {
        print("Building vertices were calculated based on a rectanglular footprint. To create a custom building shape, provide 3 or more x,y coordinates.")
        
        coord_msg <- FALSE
      }
      
      coords <- bld_coords(source_coords       = data[i, ]$SOURCE_COORDS[[1]],
                           dist_from_source    = data[i, ]$DIST_FROM_SOURCE,
                           angle_from_source   = data[i, ]$ANGLE_FROM_SOURCE,
                           length              = data[i, ]$LENGTH,
                           width               = data[i, ]$WIDTH,
                           bld_rotation        = data[i, ]$BLD_ROTATION,
                           angle_units         = data[i, ]$ANGLE_UNITS,
                           show_plot           = FALSE)
      
    
      data[i, ]$BLD_XCOORDS  <- list(coords$XCOORDS)
      data[i, ]$BLD_YCOORDS  <- list(coords$YCOORDS)
    }
  }
  
  
  xcoords <- unlist(data$BLD_XCOORDS)
  ycoords <- unlist(data$BLD_YCOORDS)
  #coords  <- c(xcoords, ycoords, source_coords) 
  
  bld_range <- max(abs(source_coords[2] - ycoords), abs(source_coords[1] - xcoords))
  
  plot(xcoords,
       ycoords,
       col  = "steelblue",
       xlab = paste("X"),
       ylab = paste("Y"),
       xlim = c(source_coords[1] - bld_range, source_coords[1] + bld_range),
       ylim = c(source_coords[2] - bld_range, source_coords[2] + bld_range)
       
  )
  
  for(i in 1:nrow(data)) {
     polygon(unlist(data[i, ]$BLD_XCOORDS), unlist(data[i,]$BLD_YCOORDS), col = "steelblue") 
    }
  
  points(source_coords[1], source_coords[2], pch =13, col = "orange", cex=1.8)
}