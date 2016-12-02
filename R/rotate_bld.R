#' Rotated rectangle coordinates
#'
#' Generate coordinates for a rotated rectangle.
#' @param center_x Building's center X coordinate.
#' @param center_y Building's center Y coordinate.
#' @keywords building bpip coords rectangle
#' @export
#' @examples
#' rotate_bld(center_x = 0, center_y = 0, width_x = 2, length_y = 1)
#
#

rotate_bld <- function(center_x    = 0,
                       center_y    = 0,
                       width_x     = 4,
                       length_y    = 3,
                       rotation    = 0,
                       angle_units = "degrees",
                       show_plot   = TRUE
){

  if(angle_units == "degrees") {
    rotation <- -(rotation-450) * pi/180
  }

  corner_distance <- sqrt((width_x/2)**2 + (length_y/2)**2)

  # Calculate corner angles
  corner_angle <- asin((length_y/2)/corner_distance)

  rotated_angle1 <- rotation + corner_angle + pi/2
  rotated_angle2 <- rotation + pi/2 - corner_angle

  xvar1 <- corner_distance * cos(rotated_angle1)
  xvar2 <- corner_distance * cos(rotated_angle2)

  #xvar3 <- corner_distance * cos(rotation - (pi/2 - corner_angle))
  #xvar4 <- corner_distance * cos(rotation - pi/2 - corner_angle))

  yvar1 <- corner_distance * sin(rotated_angle1)
  yvar2 <- corner_distance * sin(rotated_angle2)

  df <- tibble::tibble(x_coords = c(center_x + xvar1,
                                    center_x + xvar2,
                                    center_x - xvar1,
                                    center_x - xvar2),

                       y_coords = c(center_y + yvar1,
                                    center_y + yvar2,
                                    center_y - yvar1,
                                    center_y - yvar2))

  df$x_coords <- round(df$x_coords, 4)

  df$y_coords <- round(df$y_coords, 4)

  if(show_plot) {
   graphics::plot(df$x_coords,
                  df$y_coords,
                  col = "steelblue",
                  xlab = paste("width =", signif(max(df$x_coords) - min(df$x_coords), 2)),
                  ylab = paste("length =", signif(max(df$y_coords) - min(df$y_coords), 2)),
                  xlim = c(min(df), max(df)),
                  ylim = c(min(df), max(df)))

    graphics::polygon(df$x_coords, df$y_coords, col = "steelblue")

    graphics::points(center_x, center_y)
  }

  return(df)
}
