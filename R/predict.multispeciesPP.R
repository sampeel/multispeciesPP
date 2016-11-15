##' Predict species intensities or biased species intensities based on
##' a multispeciesPP model.
##'
##' @title Predict species intensities or biased species intensities
##'   based on a multispeciesPP model.
##' @param object a fitted object of class \code{multispeciesPP}
##' @param newdata optionally, a data frame in which to look for
##'   variables with which to predict. If omitted, the fitted linear
##'   predictors are used.
##' @param sdm ??
##' @param bias ??
##' @param species vector of species names.
##' @param dispersion ignored.
##' @param terms ignored.
##' @param na.action ignored.
##' @param ... ignored.
##' @author William Fithian
##' @importFrom stats model.matrix na.pass
predict.multispeciesPP <-
function (object, newdata,
              sdm=TRUE, bias=FALSE,
              species=colnames(object$fit.PA),
              dispersion = NULL, terms = NULL,
              na.action = na.pass, ...)
{
    na.act <- object$na.action
    object$na.action <- NULL
    pred <- matrix(0,nrow(newdata),length(species),dimnames=list(rownames(newdata),species))
    if(sdm) {
        sdm.mat <-  model.matrix(object$sdm.formula,newdata)
        good.rows <- row.names(sdm.mat)
        pred[!(row.names(pred) %in% good.rows),] <- NA
        pred[good.rows,] <- sdm.mat %*% object$species.coef[-nrow(object$species.coef),species]
    }
    if(bias) {
        bias.mat <- model.matrix(object$bias.formula,newdata)
        good.rows <- rownames(bias.mat)
        pred[!(row.names(pred) %in% good.rows),] <- NA
        pred[good.rows,] <- pred[good.rows,] + bias.mat[good.rows,] %*% object$bias.coef
        pred <- pred + rep(object$species.coef["isPO",species],each=nrow(pred))
    }
    pred
}
