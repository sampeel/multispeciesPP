##' Allows us to encode a GLM with an extra linear term as a GLM.
##'
##' @title Linear "family" for rows.
##' @param tol Numerical tolerance.  Should be small.
##' @author William Fithian
##' @export
linear <-
function(tol=1e-10) {
    structure(list(family = "linear", linkfun = function(mu) mu / tol,
                   linkinv = function(eta) eta * tol,
                   variance = function(mu) rep(tol, length(mu)),
                   dev.resids = function(y, mu, wt) - 2 * wt * y * mu / tol,
                   mu.eta = function(eta) rep(tol, length(eta)),
                   validmu = function(mu) TRUE, valideta = function(eta) TRUE,
                   class = "family"))
}
