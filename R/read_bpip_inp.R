#' Read BPIP input file
#'
#' Read a bpip.inp file into a dataframe.
#' @param file A path to a file or a text string containing line breaks. Default is "bpip.inp".
#' @keywords read bpip input
#' @export
#' @examples
#' bpip_inp <- "'Example1'
#' 'p'
#' 'METERS' 1.00
#' 'UTMN' 0.00
#' 1
#' 'Bld_1' 1 0
#' 4 10
#' -22.5 5
#' -17.5 5
#' -17.5 -5
#' -22.5 -5
#' 1
#' 'Stack_1' 0 10 0 0
#' 'Stack_2' 0 10 0 0
#'"
#'
#' read_bpip_inp(file = bpip_inp)
# 
#

read_bpip_inp <- function(file) {
  
  if(grepl("\n", file)) { 
    inp <- readLines(textConnection(file))
  } else { 
    inp <- readLines(file)
    }
     
  buildings   <- c()
  n_tiers     <- c()
  heights     <- c()
  lengths     <- c()
  widths      <- c()
  elevs       <- c()
  x_coords    <- c()
  y_coords    <- c()
  bld_xcoords <- list()[0]
  bld_ycoords <- list()[0]
  skip        <- FALSE
  
  # Search for the last line not containing a single or double quote 
  sources_start <- max((1:(length(inp)-1))[!grepl("[']", inp) & !grepl('["]', inp)], na.rm=T)
  
  
  # Collect building names and coordinates
  for(i in 6:(sources_start - 1)) {
    
    if(!skip) {
    
      line  <- strsplit(paste0(" ", inp[i]), "\\s+")[[1]]
  
      # Check if line contains building name
      if(grepl("[']", line[2]) | grepl('["]', line[2])) {
          
          # Record last buildings info
          if(length(x_coords) > 1) {
            widths   <- c(widths, signif(max(x_coords) - min(x_coords), 2))
            lengths  <- c(lengths, signif(max(y_coords) - min(y_coords), 2))
            bld_xcoords[length(bld_xcoords)+1] <- list(x_coords)
            bld_ycoords[length(bld_ycoords)+1] <- list(y_coords)
            x_coords <- c()
            y_coords <- c()
          }
            
          # Gather new buildings info
          buildings <- c(buildings, line[2])
          n_tiers   <- c(n_tiers, line[3])
          elevs     <- c(elevs, line[4])
          
          heights <- c(heights, strsplit(paste0(" ", inp[i+1]), "\\s+")[[1]][3]) 
      
          skip <- TRUE
          
      } else {
         x_coords <- c(x_coords, as.numeric(line[2]))
         y_coords <- c(y_coords, as.numeric(strsplit(paste0(" ", inp[i]), "\\s+")[[1]][3]))
      }
    
    } else skip <- FALSE
  }
  
  # Record the very last buildings info
  if(length(x_coords) > 1) {
    widths   <- c(widths, signif(max(x_coords) - min(x_coords), 2))
    lengths  <- c(lengths, signif(max(y_coords) - min(y_coords), 2))
    bld_xcoords[length(bld_xcoords)+1] <- list(x_coords)
    bld_ycoords[length(bld_ycoords)+1] <- list(y_coords)
    x_coords <- c()
    y_coords <- c()
  }
  
  # Collect source info
  sources      <- c()
  src_elevs    <- c()
  src_heights  <- c()
  src_coords   <- list()[0] 
  
  for(i in (sources_start + 1):length(inp)) {
    
    line       <- strsplit(paste0(" ", inp[i]), "\\s+")[[1]]
    
    if(length(line) > 1) {
    
      sources     <-  c(sources, line[[2]])
      src_elevs   <-  c(src_elevs, line[[3]])
      src_heights <-  c(src_heights, line[[4]])
      src_coords[length(src_coords) + 1]  <-  list(as.numeric(c(line[[5]], line[[6]])))
    
      } 
  } 
  
  distances  <- c()
  angles     <- c()
  
  for(i in 1:length(buildings)) {
    
    bld_center <- c(mean(bld_xcoords[[i]]), mean(bld_ycoords[[i]]))
    
    distances <- c(distances, round(stats::dist(rbind(src_coords[[1]], bld_center))[[1]], 5))
    
    # Normalize coordinates
    a <- c(0, 1)
    
    b <- c(bld_center[1] - as.numeric(src_coords[[1]][1]), bld_center[2] - as.numeric(src_coords[[1]][2]))
    
    # Calculate angle between building and North
    theta <- acos( sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)) ) )
    
    ang_degrees <- (theta * 180/pi ) %% 360
    
    if(b[1] < 1) ang_degrees <- -ang_degrees
    
    angles    <- c(angles, ang_degrees)
  }
  
  # Create data frame
  inp  <- tibble::tibble(prj_title         = inp[1],
                         building          = buildings,
                         bld_height        = heights,
                         width_x           = widths,
                         length_y          = lengths,
                         bld_rotation      = NA,
                         angle_units       = "degrees",
                         bld_elev          = elevs,
                         n_tiers           = n_tiers,
                         bld_xcoords       = bld_xcoords,
                         bld_ycoords       = bld_ycoords,
                         dist_from_source  = distances,
                         angle_from_source = angles,
                         source_name       = sources,
                         source_xy         = src_coords,
                         source_elev       = src_elevs,
                         source_height     = src_heights)

  return(inp)
  
}
