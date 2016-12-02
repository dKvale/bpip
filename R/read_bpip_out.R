#' Read BPIP output file
#'
#' Read a bpip.out file into a dataframe.
#' @param file A path to a file or a text string containing line breaks. Default is "bpip.out".
#' @param as_text Output results as a character string. If FALSE ouput as a dataframe.
#' @keywords read bpip output results
#' @export
#' @examples
#' 
#' bpip_out <- "bpip                                                                          
#'
#'BPIP (Dated: 04274)
#'DATE : 10/ 7/2016
#'TIME : 13:16:32
#'                                                                        
#'BPIP output is in meters
#'
#'
#'SO BUILDHGT Stack_1     0.00    0.00    0.00    0.00    0.00   10.00
#'SO BUILDHGT Stack_1    10.00   10.00   10.00   10.00   10.00   10.00
#'SO BUILDHGT Stack_1     0.00    0.00    0.00    0.00    0.00    0.00
#'SO BUILDHGT Stack_1     0.00    0.00    0.00    0.00    0.00   10.00
#'SO BUILDHGT Stack_1    10.00   10.00   10.00   10.00   10.00   10.00
#'SO BUILDHGT Stack_1     0.00    0.00    0.00    0.00    0.00    0.00
#'SO BUILDWID Stack_1     0.00    0.00    0.00    0.00    0.00   11.16
#'SO BUILDWID Stack_1    11.11   10.72   10.00   10.72   11.11   11.16
#'SO BUILDWID Stack_1     0.00    0.00    0.00    0.00    0.00    0.00
#'SO BUILDWID Stack_1     0.00    0.00    0.00    0.00    0.00   11.16
#'SO BUILDWID Stack_1    11.11   10.72   10.00   10.72   11.11   11.16
#'SO BUILDWID Stack_1     0.00    0.00    0.00    0.00    0.00    0.00
#'SO BUILDLEN Stack_1     0.00    0.00    0.00    0.00    0.00    9.33
#'SO BUILDLEN Stack_1     8.12    6.66    5.00    6.66    8.12    9.33
#'SO BUILDLEN Stack_1     0.00    0.00    0.00    0.00    0.00    0.00
#'SO BUILDLEN Stack_1     0.00    0.00    0.00    0.00    0.00    9.33
#'SO BUILDLEN Stack_1     8.12    6.66    5.00    6.66    8.12    9.33
#'SO BUILDLEN Stack_1     0.00    0.00    0.00    0.00    0.00    0.00
#'SO XBADJ    Stack_1     0.00    0.00    0.00    0.00    0.00  -21.99
#'SO XBADJ    Stack_1   -22.85  -23.03  -22.50  -23.03  -22.85  -21.99
#'SO XBADJ    Stack_1     0.00    0.00    0.00    0.00    0.00    0.00
#'SO XBADJ    Stack_1     0.00    0.00    0.00    0.00    0.00   12.66
#'SO XBADJ    Stack_1    14.73   16.37   17.50   16.37   14.73   12.66
#'SO XBADJ    Stack_1     0.00    0.00    0.00    0.00    0.00    0.00
#'SO YBADJ    Stack_1     0.00    0.00    0.00    0.00    0.00   10.00
#'SO YBADJ    Stack_1     6.84    3.47    0.00   -3.47   -6.84  -10.00
#'SO YBADJ    Stack_1     0.00    0.00    0.00    0.00    0.00    0.00
#'SO YBADJ    Stack_1     0.00    0.00    0.00    0.00    0.00  -10.00
#'SO YBADJ    Stack_1    -6.84   -3.47    0.00    3.47    6.84   10.00
#'SO YBADJ    Stack_1     0.00    0.00    0.00    0.00    0.00    0.00"
#' 
#' read_bpip_out(file = bpip_out)
# 
#

read_bpip_out <- function(file    = "bpip.out",
                          as_text = TRUE) {
  
  if(grepl("\n", file)) { 
    out <- readLines(textConnection(file))
  } else { 
    out <- readLines(file)
  }
  
  # Search for the start of BPIP output
  so_start <- min(grep("BUILDHGT", out), na.rm=T)
  
  # Cut output to results table
  out <- out[so_start:length(out)]
    
  
  if(!as_text) {
    
  # Remove leading white space 
  out <- trimws(out)
  
  # Replace spaces with commas
  out <- gsub("\\s+", ",", out)
  
  # Add column names
  out <- c("SO,input,STACK,ANG1,ANG2,ANG3,ANG4,ANG5,ANG6\n", out)
    
  # Collapse to a single string
  out <- paste0(out, collapse = "\n")
  
  # Read into data table
  out <- utils::read.csv(textConnection(out), stringsAsFactors = F)

  # Flip angles long format
  out <- tidyr::gather(out[ , -1], key = "ANGLE", value = Value, ANG1, ANG2, ANG3, ANG4, ANG5, ANG6)
  
  out <- dplyr::arrange(out, STACK)
  
  # Label wind direction in degrees
  angles <- c()
  
  for(i in 1:6) angles <- c(angles, rep(i * 10 + 60 * (0:5), 5)) 
  
  out$ANGLE <- angles
  
  # Flip inputs to separate columns
  out <- tidyr::spread(out, input, Value)
  
  }
  
  return(out)
  
}