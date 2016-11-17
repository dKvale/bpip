#' Run BPIP
#'
#' Call BPIP.exe and run input file
#' @param data Dataframe of bpip parameters.
#' @param output Filename for bpip results.
#' @param exe_folder Folder containing bpip.exe.
#' @keywords building bpip coords rectangle
#' @export
#' @examples
#' run_bpip(data = bpip_inp, output = "bpip_results")
# 
# 

run_bpip <- function(data       = bpip_inp, 
                     output     = "bpip_results",
                     exe_folder = getwd()) {
  
  # Check if .exe exists
  check4bpip <- "Bpipprm.exe" %in% tolower(list.files(exe_folder))
  
  if(!check4bpip) {
    warning("Bpipprm.exe was not found in ", exe_folder)
    stop()
  }
  
  # Copy input file to folder
  writeLines(print(data), paste0(exe_folder, "/bpip.inp"))
  
  # Shell command
  relocate <- paste0(substring(exe_folder, 1, 1), ": & CD ", exe_folder)
  
  shell(paste(relocate, "& Bpipprim.exe", data, paste0(output, ".out")))
  
  # Copy output file
  #shell(paste0(relocate, ' & COPY bpip.out "', output, '.out"'))
  
  # Clean house      
  invisible(readLines(paste0(output, ".out")))
}