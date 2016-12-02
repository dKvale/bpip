#' Run the BPIP downwash model
#'
#' Call bpip.exe and run input file.
#' @param input Path to input file.
#' @param output Filename for BPIP results.
#' @param exe_folder Folder containing bpip.exe.
#' @keywords building bpip downwash
#' @export
#' @examples
#' \dontrun{
#' run_bpip(input      = "bpip.inp", 
#'          output     = "bpip_results", 
#'          exe_folder = "EPA/bpip")
#' }
# 

run_bpip <- function(input      = "bpip.inp", 
                     output     = "bpip_results",
                     exe_folder = "bpip") {
  
  # Check if .exe exists
  check_bpip <- "bpipprm.exe" %in% tolower(list.files(exe_folder))
  
  if(!check_bpip) {
    warning("Bpipprm.exe was not found in ", exe_folder)
    stop()
  }
  
  # Copy input file to .exe folder
  writeLines(readLines(input), paste0(exe_folder, "/bpip.inp"))
  
  # Shell command
  relocate <- paste0("CD /d ", getwd(), " & CD /d ", exe_folder)
  
  shell(paste(relocate, "& bpipprm bpip.inp", paste0(gsub("[.]out", "", output), ".out"), paste0(gsub("[.]out", "", output), ".sum")))
  
  # Copy output file
  shell(paste0(relocate, ' & COPY "', paste0(gsub("[.]out", "", output), ".out"), '"', ' "', paste0(getwd(), "/", gsub("[.]out", "", output), ".out"), '"'))
  
  # Clean house      
  invisible(writeLines(readLines(paste0(gsub("[.]out", "", output), ".out"))))
}