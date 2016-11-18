#' Install BPIP
#'
#' Download bpip-prime from EPA and install to entered folder.  
#' @param dir Folder for installation
#' @keywords downwash building bpip
#' @export
#' @examples
#' install_bpip(dir = "bpip")
# 
# 

install_bpip <- function(dir = "bpip_exe") {
 
  tf <- tempfile(dir, fileext = ".zip")
  
  download.file("https://www3.epa.gov/ttn/scram/models/relat/bpipprime.zip", tf)
  
  unzip(tf, exdir = dir)
  
}