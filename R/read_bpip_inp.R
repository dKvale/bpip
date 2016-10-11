#' Read BPIP input file
#'
#' Read an aermod.inp file into an AERMOD input table.
#' @param path File location. Default is "bpip.inp" in the working directory.
#' @keywords read bpip input
#' @export
#' @examples
#' read_bpip_inp(file = "bpip.inp")
# 
#

read_bpip_inp <- function(file = "bpip.inp") {
  
  inp <- readLines(file)
  
  # Search for the last line not containing a single or double quote 
  sources_start <- max((1:length(inp))[!grepl("[']", inp) & !grepl('["]', inp)], na.rm=T)
  
  #output_type   <- inp[2] 
  
  buildings   <- c()
  n_tiers     <- c()
  heights     <- c()
  widths      <- c()
  lengths     <- c()
  elevs       <- c()
  xcoords     <- c()
  ycoords     <- c()
  bld_xcoords <- list()[0]
  bld_ycoords <- list()[0]
  skip        <- FALSE
  
  for(i in 6:(sources_start - 1)) {
    
    if(!skip) {
    
      line  <- strsplit(paste0(" ", inp[i]), "\\s+")[[1]]
  
      # Check if line contains building name
      if(grepl("[']", line[2]) | grepl('["]', line[2])) {
          
          if(length(xcoords) > 1) {
            lengths <- c(lengths, signif(max(xcoords) - min(xcoords), 2))
            widths  <- c(widths, signif(max(ycoords) - min(ycoords), 2))
            bld_xcoords[length(bld_xcoords)+1] <- list(xcoords)
            bld_ycoords[length(bld_ycoords)+1] <- list(ycoords)
            xcoords <- c()
            ycoords <- c()
          }
            
          buildings <- c(buildings, first)
          n_tiers   <- c(n_tiers, line[3])
          elevs     <- c(elevs, line[4])
          
          heights <- c(heights, strsplit(paste0(" ", inp[i+1]), "\\s+")[[1]][3]) 
      
          skip <- TRUE
      } else {
         xcoords <- c(xcoords, as.numeric(line[2]))
         ycoords <- c(ycoords, as.numeric(strsplit(paste0(" ", inp[i]), "\\s+")[[1]][3]) )
      }
    
    } else skip <- FALSE
  }
  
  if(length(xcoords) > 1) {
    bld_xcoords[length(bld_xcoords)+1] <- list(xcoords)
    bld_ycoords[length(bld_ycoords)+1] <- list(ycoords)
    xcoords <- c()
    ycoords <- c()
  }
  
  sources      <- c()
  src_elevs    <- c()
  src_heights  <- c()
  src_coords   <- list()[0] 
  
  for(i in (sources_start + 1):length(inp)) {
    
    line          <- strsplit(paste0(" ", inp[i]), "\\s+")[[1]]
    
    if(length(line) > 1) {
    
      sources     <-  c(sources, line[[2]])
      src_elevs   <-  c(src_elevs, line[[3]])
      src_heights <-  c(src_heights, line[[4]])
      src_coords[length(src_coords) + 1]  <-  list(as.numeric(c(line[[5]]), line[[6]]))
    } 
  } 
  
  distances  <- c()
  angles     <- c()
  
  for(i in 1:length(buildings)) {
    
    bld_center <- c(mean(bld_xcoords[[i]]), mean(bld_ycoords[[i]]))
    
    distances <- c(distances, dist(cbind(src_coords[[1]], bld_center))[[1]])
    
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
  inp <- tibble(PRJ_TITLE         = inp[1],
                BUILDING          = buildings,
                HEIGHT            = heights,
                LENGTH            = lengths,
                WIDTH             = widths,
                BLD_ROTATION      = NA,
                ANGLE_UNITS       = "degrees",
                ELEV              = elevs,
                N_TIERS           = n_tiers,
                BLD_XCOORDS       = bld_xcoords,
                BLD_YCOORDS       = bld_ycoords,
                DIST_FROM_SOURCE  = distances,
                ANGLE_FROM_SOURCE = angles,
                SOURCE_NAME       = sources,
                SOURCE_COORDS     = src_coords,
                SOURCE_ELEV       = src_elevs,
                SOURCE_HEIGHT     = src_heights
                )
      
  return(inp)
  
}