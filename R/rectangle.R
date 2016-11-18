#' Rectangle coordinates
#'
#' Generate rectangle coordinates for BPIP buildings.
#' @param center_x Building's center X coordinate.
#' @param center_y Building's center Y coordinate.
#' @keywords building bpip coords rectangle
#' @export
#' @examples
#' rectangle(center_x = 0, center_y = 0, length = 1, width = 2)
# 
# 

rectangle <- function(center_x = 0,
                      center_y = 0,
                      length   = 3,
                      width    = 4,
                      rotation = 0,
                      angle_units = "degrees",
                      show_plot = TRUE
){
  
  if(angle_units == "degrees") {
    rotation <- -(rotation-450) * pi/180
  }
  
  corner_distance <- sqrt((width/2)**2 + (length/2)**2)
  
  # Trigonometry is real
  corner_angle <- asin((length/2)/corner_distance)
  
  rotated_angle1 <- rotation + corner_angle + pi/2
  rotated_angle2 <- rotation + pi/2 - corner_angle
  
  xvar1 <- corner_distance * cos(rotated_angle1)
  xvar2 <- corner_distance * cos(rotated_angle2)
  
  #xvar3 <- corner_distance * cos(rotation - (pi/2 - corner_angle))
  #xvar4 <- corner_distance * cos(rotation - pi/2 - corner_angle))
  
  yvar1 <- corner_distance * sin(rotated_angle1)
  yvar2 <- corner_distance * sin(rotated_angle2)
  
  df <- tibble::tibble(XCOORDS = c(center_x + xvar1, 
                           center_x + xvar2,
                           center_x - xvar1, 
                           center_x - xvar2),
                   
                       YCOORDS = c(center_y + yvar1, 
                           center_y + yvar2,
                           center_y - yvar1, 
                           center_y - yvar2))
  
  df$XCOORDS <- round(df$XCOORDS, 4)
  
  df$YCOORDS <- round(df$YCOORDS, 4)
  
  if(show_plot) {
  plot(df$XCOORDS,
       df$YCOORDS,
       col = "steelblue",
       xlab = paste("length =", signif(max(df$XCOORDS) - min(df$XCOORDS), 2)),
       ylab = paste("width =", signif(max(df$YCOORDS) - min(df$YCOORDS), 2)),
       xlim = c(min(df), max(df)),
       ylim = c(min(df), max(df)))
  
  #rect(df[1,2], df[3,3], df[2,2], df[1,3])
  
  polygon(df$XCOORDS, df$YCOORDS, col = "steelblue")
  
  points(center_x, center_y)
  }
  
  return(df)
}