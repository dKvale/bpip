#' Read BPIP output file
#'
#' Read an bpip.out file into a dataframe.
#' @param path File location. Default is "bpip.out" in the working directory.
#' @keywords read bpip output results
#' @export
#' @examples
#' read_bpip_out(file = "bpip.out")
# 
#


read_bpip_out <- function(file = "data-raw/bpip.out") {
  
  out <- readLines(file)
  
  sources       <- c()
  bld_heights   <- c()
  bld_widths    <- c()
  bld_lengths   <- c()
  xbadjs        <- c()
  ybadjs        <- c()

  
  # Search for the start of BPIP output
  so_start <- min(grep("BUILDHGT", out), na.rm=T)
  
  for(i in so_start:length(out)) {
    line          <- out[i]
    sources       <- c(sources, strsplit(out[i], "\\s+")[[1]][4])
    bld_heights   <- c(bld_heights, gsub("\\s+|SO|BUILDHGT|", "", out[[i]))
    bld_widths    <- c(bld_widths, )
    bld_lengths   <- c(bld_lengths, )
    xbadjs        <- c(xbadjs, )
    ybadjs        <- c(ybadjs, )
    
  }
  
  # Create dataframe
  out <- tibble(PRJ_TITLE         = strsplit(out[1], "\\s+")[[1]][2],
                BUILDHGT          = bld_heights,
                BUILDWID          = bld_widths,
                BUILDLEN          = bld_lengths,       
                XBADJ             = xbadjs,
                YBADJ             = ybadjs,
                DEGREES           = degrees
  )
  
  return(out)
  
}