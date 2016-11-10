# LASSO.BETAS (06.08.2014)
#' Title
#'
#' @param x
#' @param scale
#' @param nonzero
#' @param lambdamin
#' @param names
#'
#' @return
#' @export
#'
#' @examples

lasso.betas <- function(x, scale = FALSE, nonzero = TRUE, lambdamin = FALSE, names = FALSE, order = TRUE){
  if(lambdamin == TRUE){
    b.parse <- coef(x, s = 'lambda.min')
    betas <- as.vector(b.parse)[-1]
    names(betas) <- rownames(b.parse)[-1]
  } else{
    b.parse <- coef(x, s = 'lambda.1se')
    betas <- as.vector(b.parse)[-1]
    names(betas) <- rownames(b.parse)[-1]
  }
  if(order) betas <- betas[order(abs(betas), decreasing = TRUE)]
  if(nonzero == TRUE) betas <- betas[betas != 0]
  if(scale == TRUE) betas <- abs(betas)/sum(abs(betas))
  if(names == TRUE) return(names(betas))
  else return(betas)
}
