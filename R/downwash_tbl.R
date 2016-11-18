#' Downwash options
#'
#' Create an input table of downwash options for AERMOD.
#' @param stack Stack names.
#' @keywords downwash building aermod input
#' @export
#' @examples
#' downwash_tbl(stack = "STK_1")
# 
# 

downwash_tbl <- function(stack     = "STK_1",
                         angle     = seq(10,360,10),
                         buildhgts = 10,
                         buildwids = 10,
                         buildlens = 10,
                         xbadj     = 1,
                         ybadj     = 1
) {

df <- tibble::tibble(STACK     = stack,
             ANGLE     = angle,
             BUILDHGTS = buildhgts,
             BUILDWIDS = buildwids,
             BUILDLENS = buildlens,
             XBADJ     = xbadj,
             YBADJ     = ybadj,
             stringsAsFactors = F)

return(df)
}

##