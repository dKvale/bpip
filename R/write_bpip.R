#' Write building table to a BPIP input text file.
#'
#' Output a BPIP input file from a data frame of building parameters.
#' @param x A data frame of building parameters.
#' @param path Path to write to. Default is "bpip.inp".
#' @param prj_title Title of project added to BPIP file. Defaults to path name.
#' @param output_type A character flag controls the BPIP model's output type. For input to either an ISCST2 or an ISCLT2 input file. 
#'                    The three flags are:
#'                                        'p'  - Output will be for PRIME or AERMOD input. 
#'                                        'ST' - Output will be for ISCST2 input.
#'                                        'LT' - Output will be for ISCLT2 input.
#' @keywords building bpip write save input
#' @export
#' @examples
#' write_bpip(x = facility_blds)
# 
# 


write_bpip <- function(x, 
                       path        = "bpip.inp",
                       prj_title   = gsub("[.]inp", "", path),
                       output_type = "p"
                       ) {
  
  if(nrow(x) < 1) return("Data frame is empty. BPIP requires at least 1 building.")
  
  inp    <- paste0("'", prj_title, "'")     # Title
  
  inp[2] <- paste0("'", output_type, "'")   # Output type
  
  inp[3] <- paste0("'METERS' 1.00")         # Distance units
  
  inp[4] <- paste0("'UTMN' 0.00")           # Coordinate orientation
  
  inp[5] <- unique(x$BUILDING)              # Number of buildings
  
  coord_msg <- TRUE                         # Prevent repeat messages
  
  for(i in 1:nrow(x)) {                     # Building names and tier coordinates
    
    inp[length(inp) +1] <- paste0(" '",  substring(x[i, "BUILDING"], 1, 8), "' ", x[i, "N_TIERS"], " ",x[i, "ELEV"])
    
    if(is.null(x[i, "BLD_XCOORDS"]) | 
       is.null(x[i, "BLD_YCOORDS"]) | 
       length(unlist(x[i, "BLD_XCOORDS"], ",")) < 3 |
       length(unlist(x[i, "BLD_YCOORDS"], ",")) < 3 ) {
      
      if(coord_msg) {
        print("Building vertices were calculated for a rectangle. To create 
               a custom building shape, provide 3 or more x,y coordinates.")
        
        coord_msg <- FALSE
      }
      
      coords <- bld_coords(source_coords       = x[i, ]$SOURCE_COORDS[[1]],
                           dist_from_source    = x[i, ]$DIST_FROM_SOURCE,
                           angle_from_source   = x[i, ]$ANGLE_FROM_SOURCE,
                           length              = x[i, ]$LENGTH,
                           width               = x[i, ]$WIDTH,
                           bld_rotation        = x[i, ]$BLD_ROTATION,
                           angle_units         = x[i, ]$ANGLE_UNITS,
                           show_plot           = FALSE)
      
    } else {
      
      coords <- data.frame(XCOORDS = unlist(x[i, "BLD_XCOORDS"]),
                           YCOORDS = unlist(x[i, "BLD_YCOORDS"]))
    }
     
    
    inp[length(inp) +1] <- paste("   ", length(coords$XCOORDS) , x[i, "HEIGHT"])
    
    for(n in 1:nrow(coords)) {
      inp[length(inp) +1] <- paste("     ", coords[n, "XCOORDS"], coords[n, "YCOORDS"])
    }
    
  }
  
  inp[length(inp) +1] <- 1       # Number of stacks
  
  inp[length(inp) +1] <- paste0("  '", substring(x[1, "SOURCE_NAME"], 1, 8), "' ", 
                                x[1, "SOURCE_ELEV"], " ", 
                                x[1, "SOURCE_HEIGHT"], " ",
                                paste(unlist(x[1, ]$SOURCE_COORDS), collapse=" "))
  
  #cat(inp)
  
  if(is.null(path) | nchar(path) < 1) {
    return(inp)
  } else  writeLines(inp, path)
  
    
}

