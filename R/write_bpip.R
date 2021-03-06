#' Write a data frame to a BPIP input file.
#'
#' Output a BPIP input file from a data frame of building parameters.
#' @param data Data frame of BPIP parameters.
#' @param path Path to write to. Default is "bpip.inp".
#' @param prj_title Title of project added to BPIP file. Defaults to path name.
#' @param output_type A character flag controls the BPIP model's output type.
#'                    The three flags are:
#'                        (1) 'p' for PRIME or AERMOD input;  
#'                        (2) 'ST' for ISCST2 input; 
#'                        (3) 'LT' for ISCLT2 input.
#' @keywords building bpip write save input
#' @export
#' @examples
#' buildings <- new_bpip()
#' 
#' write_bpip(data = buildings, path = "bpip.inp")
# 
# 
write_bpip <- function(data, 
                       path        = NULL,
                       prj_title   = gsub("[.]inp", "", path),
                       output_type = "p"
                       ) {
  
  if(nrow(data) < 1) return("Data frame is empty. BPIP requires at least 1 building.")
  
  inp    <- paste0("'", prj_title, "'")     # Title
  
  inp[2] <- paste0("'", output_type, "'")   # Output type
  
  inp[3] <- paste0("'METERS' 1.00")         # Distance units
  
  inp[4] <- paste0("'UTMN' 0.00")           # Coordinate orientation
  
  inp[5] <- length(unique(data$bld_id))     # Number of buildings
  
  coord_msg <- TRUE                         # Prevent repeat messages
  
  for(i in 1:nrow(data)) {                  # Building names and tier coordinates
    
    inp[length(inp) + 1] <- paste0("'",  substring(data[i, "bld_id"], 1, 8), "' ", data[i, "n_tiers"], " ",data[i, "bld_elev"])
    
    if(is.null(data[i, "bld_xcoords"]) | 
       is.null(data[i, "bld_ycoords"]) | 
       length(unlist(data[i, "bld_xcoords"], ",")) < 3 |
       length(unlist(data[i, "bld_ycoords"], ",")) < 3 ) {
      
      if(coord_msg) {
        print("Building vertices were calculated for a rectangle. To create a custom building shape, provide 3 or more x,y coordinates.")
        
        coord_msg <- FALSE
      }
      
      coords <- bld_coords(source_xy           = data[i, ]$source_xy[[1]],
                           dist_from_source    = data[i, ]$dist_from_source,
                           angle_from_source   = data[i, ]$angle_from_source,
                           width_x             = data[i, ]$width_x,
                           length_y            = data[i, ]$length_y,
                           bld_rotation        = data[i, ]$bld_rotation,
                           angle_units         = data[i, ]$angle_units,
                           show_plot           = FALSE)
      
    } else {
      
      coords <- data.frame(x_coords = unlist(data[i, "bld_xcoords"]),
                           y_coords = unlist(data[i, "bld_ycoords"]))
    }
     
    
    inp[length(inp) +1] <- paste("   ", length(coords$x_coords) , data[i, "bld_height"])
    
    for(n in 1:nrow(coords)) {
      inp[length(inp) +1] <- paste("     ", coords[n, "x_coords"], coords[n, "y_coords"])
    }
    
  }
  
  inp[length(inp) + 1] <- 1       # Number of stacks
  
  inp[length(inp) + 1] <- paste0("'", substring(data[1, "source_name"], 1, 8), "' ", 
                                data[1, "source_elev"], " ", 
                                data[1, "source_height"], " ",
                                paste(unlist(data[1, ]$source_xy), collapse=" "))
  
  cat("\nGenerated input file: \n\n")
  
  invisible(writeLines(inp))
  
  if(is.null(path) || nchar(path) < 1) {
    
    return(inp)
    
  } else  {
    
    con <- file(path)
    
    writeLines(inp, con)
    
    close(con)
  }
    
}

