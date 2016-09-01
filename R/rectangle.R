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
                      length   = 1,
                      width    = 2
){
  df <- data.frame(Point = 1:4,
                   XCOORDS = c(center_x - length/2, 
                               center_x + length/2,
                               center_x - length/2, 
                               center_x + length/2),
                   YCOORDS = c(center_y + width/2, 
                               center_y + width/2,
                               center_y - width/2, 
                               center_y - width/2)
                   )
  
  #plot(center_x, 
  #     center_y, 
  #     xlim = range(df$XCOORDS), 
  #     ylim = range(df$YCOORDS),
  #     xlab = paste("length =", length),
  #     ylab = paste("width =", width))

  #rect(df[1,2], df[3,3], df[2,2], df[1,3])
  
  return(df)
}