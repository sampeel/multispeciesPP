##' Print summary table for multispeciesPP model
##'
##' @title Print summary table for multispeciesPP model
##' @param x a summary of an multispeciesPP object.
##' @param ... ignored.
##' @author William Fithian
##' @importFrom stats printCoefmat
print.summary.multispeciesPP <-
function(x,...) {
    printCoefmat(x$coef.table)
}
