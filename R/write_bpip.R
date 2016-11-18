#' Write building table to a BPIP input text file.
#'
#' Output a BPIP input file from a data frame of building parameters.
#' @param data Dataframe of bpip parameters.
#' @param path Path to write to. Default is "bpip.inp".
#' @param prj_title Title of project added to BPIP file. Defaults to path name.
#' @param output_type A character flag controls the BPIP model's output type. For input to either an ISCST2 or an ISCLT2 input file. 
#'                    The three flags are:
#'                        'p'  - Output for PRIME or AERMOD input. 
#'                        'ST' - Output for ISCST2 input.
#'                        'LT' - Output for ISCLT2 input.
#' @keywords building bpip write save input
#' @export
#' @examples
#' write_bpip(data = facility_blds)
# 
# 

write_bpip <- function(data, 
                       path        = "bpip.inp",
                       prj_title   = gsub("[.]inp", "", path),
                       output_type = "p"
                       ) {
  
  if(nrow(data) < 1) return("Data frame is empty. BPIP requires at least 1 building.")
  
  inp    <- paste0("'", prj_title, "'")     # Title
  
  inp[2] <- paste0("'", output_type, "'")   # Output type
  
  inp[3] <- paste0("'METERS' 1.00")         # Distance units
  
  inp[4] <- paste0("'UTMN' 0.00")           # Coordinate orientation
  
  inp[5] <- length(unique(data$BUILDING))           # Number of buildings
  
  coord_msg <- TRUE                         # Prevent repeat messages
  
  for(i in 1:nrow(data)) {                     # Building names and tier coordinates
    
    inp[length(inp) +1] <- paste0(" '",  substring(data[i, "BUILDING"], 1, 8), "' ", data[i, "N_TIERS"], " ",data[i, "ELEV"])
    
    if(is.null(data[i, "BLD_XCOORDS"]) | 
       is.null(data[i, "BLD_YCOORDS"]) | 
       length(unlist(data[i, "BLD_XCOORDS"], ",")) < 3 |
       length(unlist(data[i, "BLD_YCOORDS"], ",")) < 3 ) {
      
      if(coord_msg) {
        print("Building vertices were calculated for a rectangle. To create a custom building shape, provide 3 or more x,y coordinates.")
        
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
      
    } else {
      
      coords <- data.frame(XCOORDS = unlist(data[i, "BLD_XCOORDS"]),
                           YCOORDS = unlist(data[i, "BLD_YCOORDS"]))
    }
     
    
    inp[length(inp) +1] <- paste("   ", length(coords$XCOORDS) , data[i, "HEIGHT"])
    
    for(n in 1:nrow(coords)) {
      inp[length(inp) +1] <- paste("     ", coords[n, "XCOORDS"], coords[n, "YCOORDS"])
    }
    
  }
  
  inp[length(inp) +1] <- 1       # Number of stacks
  
  inp[length(inp) +1] <- paste0("  '", substring(data[1, "SOURCE_NAME"], 1, 8), "' ", 
                                data[1, "SOURCE_ELEV"], " ", 
                                data[1, "SOURCE_HEIGHT"], " ",
                                paste(unlist(data[1, ]$SOURCE_COORDS), collapse=" "))
  
  cat("\nGenerated input file: \n\n")
  invisible(writeLines(inp))
  
  if(is.null(path) | nchar(path) < 1) {
    return(inp)
  } else  writeLines(inp, path)
    
}

