# Call BPIP .exe and run input file
#exe_folder <- "M:/KME Files/RASSUpdate2015/BPIP"

run_bpip <- function(input  = bpip_inp, 
                     output = "bpip_results",
                     exe_folder = getwd()) {
  
  # Check if .exe exists
  check4bpip <- "Bpipprm.exe" %in% tolower(list.files(exe_folder))
  
  if(!check4bpip) {
    warning("Bpipprm.exe was not found in ", exe_folder)
    stop()
  }
  
  # Copy input file to folder
  writeLines(print(input), paste0(exe_folder, "/bpip.inp"))
  
  # Shell command
  relocate <- paste0(substring(exe_folder, 1, 1), ": & CD ", exe_folder)
  
  shell(paste(relocate, "& Bpipprim.exe", input, paste0(output, ".out")))
  
  # Copy output file
  #shell(paste0(relocate, ' & COPY bpip.out "', output, '.out"'))
  
  # Clean house      
  invisible(readLines(paste0(output, ".out")))
}