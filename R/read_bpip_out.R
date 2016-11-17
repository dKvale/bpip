#' Read BPIP output file
#'
#' Read a bpip.out file into a dataframe.
#' @param file File location. Default is "bpip.out" in the working directory.
#' @param as_text Output results as a character string. If FALSE ouput as a dataframe.
#' @keywords read bpip output results
#' @export
#' @examples
#' read_bpip_out(file = "bpip.out")
# 
#

read_bpip_out <- function(file    = "bpip.out",
                          as_text = TRUE) {
  
  out <- readLines(file)
  
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
  out <- c("SO,INPUT,STACK,ANG1,ANG2,ANG3,ANG4,ANG5,ANG6", out)
    
  # Collapse to a single string
  out <- paste0(out, collapse = "\n")
  
  # Read into data table
  out <- read.csv(textConnection(out), stringsAsFactors = F)
  #df2 <- read_csv(out)
  
  # Flip angles long format
  out <- tidyr::gather(out[ , -1], key = "ANGLE", value = Value, ANG1, ANG2, ANG3, ANG4, ANG5, ANG6)
  
  out2 <- out
  
  out2$STACK = "Stack2"
  
  out <- rbind(out, out2)
  
  angles <- c()
  
  for(i in 1:6) angles <- c(angles, rep(i * 10 + 60 * (0:5), 5)) 
  
  out$ANGLE <- angles
  
  # Flip inputs to separate columns
  out <- tidyr::spread(out, INPUT, Value)
  
  }
  
  return(out)
  
}