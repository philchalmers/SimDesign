#' @param J number of variables
#' @param analyse logical; is syntax being used to data generation or analysis?
#' @examples
#' cat(genLavaanSyntax(factors=1, indicators=10))
#' cat(genLavaanSyntax(factors=2, indicators=10))
#' cat(genLavaanSyntax(factors=1, indicators=10, analyse = TRUE))
#' cat(genLavaanSyntax(factors=2, indicators=10, analyse = TRUE))
#'
genLavaanSyntax <- function(factors, indicators, analyse = FALSE){
    ret <- if(factors == 1){
        if(analyse){
            paste0(paste0('f1 =~ NA*x1 + ', paste0(paste0('x', 2:indicators),
                                                   collapse=' + ')),
                   '\nf1 ~~ 1*f1')
        } else {
            paste0(paste0('f1 =~ ', paste0(rep(.7, indicators), 
                                           paste0('*x', 1:indicators),
                                           collapse=' + '), ' \n'),
                   paste0(sprintf('x%s ~~ 0.51*x%s', 1:indicators, 1:indicators), 
                          collapse=' \n'))
        }
    } else if(factors == 2){
        if(analyse){
            paste0(paste0('f1 =~ NA*x1 + ',
                          paste0(paste0('x', 2:indicators), collapse=' + ')),
                   paste0(sprintf('\nf2 =~ NA*x%s + ', indicators+1),
                          paste0(paste0('x', 2:indicators + indicators), collapse=' + ')),
                   '\nf1 ~~ 1*f1 \nf2 ~~ 1*f2 \nf1 ~~ f2')
        } else {
            paste0(paste0('f1 =~ ', paste0(rep(.7, indicators), 
                                           paste0('*x', 1:indicators),
                                           collapse=' + '), ' \n'),
                   paste0('f2 =~ ', paste0(rep(.7, indicators), 
                                           paste0('*x', 1:indicators + indicators),
                                           collapse=' + '), ' \n'),
                   'f1 ~~ .3*f2 \n',
                   paste0(sprintf('x%s ~~ 0.51*x%s', 1:indicators, 1:indicators), 
                          collapse=' \n'))
        }
    } else stop('factors input is incorrect')
    ret
}