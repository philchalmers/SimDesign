# L' matrix
simple <- complex <- matrix(c(.7, .7, .75, .8, .8, numeric(15),
                              .7, .7, .75, .8, .8, numeric(15),
                              .7, .7, .75, .8, .8), nrow=3, byrow=TRUE)
complex[c(3,11,27)] <- c(.7, .7, .7)

# residual variances
simple.resids <- complex.resids <- 1 - colSums(simple^2)
complex.resids[c(1,4,9)] <- c(.51, .36, .36)

# latent correlations
fcor <- matrix(1,3,3)
fcor[lower.tri(fcor)] <- fcor[upper.tri(fcor)] <- c(.5, .4, .3)

# variable distribution properties
kurtosis.common_2.3 <- c(-1, 2, 5)
kurtosis.unique_2.4 <- c(-1,.5,2.5,4.5,6.5,-1,1,3,5,7,
                         -.5,1.5,3.5,5.5,7.5)
Ztransform <- function(n=1) sqrt(rchisq(n, df=5)) / sqrt(3)


# organize all into fixed_object list
fo <- list(simple=simple, simple.resids=simple.resids,
           complex=complex, complex.resids=complex.resids,
           fcor=fcor, kurtosis.unique_2.4=kurtosis.unique_2.4,
           kurtosis.common_2.3=kurtosis.common_2.3,
           Ztransform=Ztransform)

rm(list=setdiff(ls(), c("Design", "fo")))


#' Generate lavaan syntax for Hu and Bentler (1999) simulation
#'
#' @param model simple/complex
#' @param fit_model true/mis1/mis2
#' @examples
#' cat(genLavaanSyntax('simple', 'true'))
#' cat(genLavaanSyntax('complex', 'mis2'))
#'
genLavaanSyntax <- function(model, fit_model){
    ret <- if(model == 'simple'){
        add <- switch(fit_model,
                      true = "",
                      mis1 = "\nf1 ~~ 0*f2",
                      mis2 = "\nf1 ~~ 0*f2\nf1 ~~ 0*f3")
        sprintf("
        f1 =~ NA*V1 + V2 + V3 + V4 + 0.8*V5
        f2 =~ NA*V6 + V7 + V8 + V9 + 0.8*V10
        f3 =~ NA*V11 + V12 + V13 + V14 + 0.8*V15%s", add)
    } else if(model == 'complex'){
        # complex
        add <- switch(fit_model,
                      true = c(' + V1', '+ V9'),
                      mis1 = c('', '+ V9'),
                      mis2 = c('', ''))
        sprintf("
        f1 =~ NA*V1 + V2 + V3 + V4 + 0.8*V5
        f2 =~ NA*V6 + V7 + V8 + V9 + 0.8*V10%s
        f3 =~ NA*V11 + V12 + V13 + V14 + 0.8*V15 + V4%s", add[1], add[2])
    } else stop('model type not included')
    ret
}
