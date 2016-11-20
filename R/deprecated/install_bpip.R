#' Install BPIP
#'
#' Download bpip-prime from EPA and install to entered folder.  
#' @param dir Folder for installation. Defaults to "bpip.exe".
#' @keywords downwash building bpip
#' @export
#' @examples
#' install_bpip(dir = "bpip")
# 
# 

install_bpip <- function(dir = "bpip_exe") {
 
  tf <- tempfile("bpip", fileext = ".zip")
  
  download.file("https://www3.epa.gov/ttn/scram/models/relat/bpipprime.zip", tf)
  
  unzip(tf, exdir = dir)
  
}