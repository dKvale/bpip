#' Fixed width strings
#'
#' Adjust the length of a character string by cutting if too long, or by adding spaces to the end if too short.
#' @param string Text to . Can be passed a list.
#' @param nchars Number of characters to make each string.
#' @keywords fixed width fw cut string
#' @export
#' @examples
#' fw("cutThisString", width = 10)
# 
# 

# Extend or cut a string to a set length
fw <- function(string = list("2short", "cutThisString"), nchars = 10) {

    new_string <- sprintf(paste0("%-", nchars, "s"), string)
    
    return(substr(new_string, 1, nchars))
}