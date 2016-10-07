install_bpip <- function() {
 
  tf <- tempfile("bpip", fileext = ".zip")
  
  download.file("https://www3.epa.gov/ttn/scram/models/relat/bpipprime.zip", tf)
  
  unzip(tf, exdir="bpip")
  
}