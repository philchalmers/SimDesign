#' Function for decomposing the simulation into ANOVA-based effect sizes
#'
#' Given the results from a simulation with \code{\link{runSimulation}} form an ANOVA table (without
#' p-values) indicating the effect sizes based on the eta-squared statistic. These provide approximate
#' indications of observable simulation effects, therefore use these results are generally useful
#' as exploratory rather than inferential tools.
#'
#' @param formula an R formula generally of the form suitable for \code{\link{lm}} or
#'   \code{\link{aov}}. However, if the dependent variable (left size of the equation) is ommited
#'   then all the dependent variables in the simulation will be used and the result will return
#'   a list of analyses
#'
#' @param dat an object returned from \code{\link{runSimulation}} of class \code{'SimDesign'}
#'
#' @param rates logical; does the dependent variable consist of rates (e.g., returned from
#'   \code{\link{ECR}} or \code{\link{EDR}})? Default is TRUE, which will use the logit of the DV
#'   to help stabalize the proportions when computing the parameters and effect sizes
#'
#' @return returns a single object containing the data to be analyzed (usually a
#'   \code{vector}, \code{matrix}, or \code{data.frame}),
#'   or a list with a the elements \code{'dat'} and \code{'parameters'}. If a list is returned
#'   the \code{'dat'} element should be the observed data object while the
#'   \code{'parameters'} element should be a named list containing the simulated parameters
#'   (if there are any. Otherwise, this could just be an empty list)
#'
#' @aliases SimAnova
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Given 'Final' object from runSimulation() example, inspect 2 way interactions
#' SimAnova(lessthan.05.welch ~ (sample_size + group_size_ratio + standard_deviation_ratio)^2,
#'     Final)
#'
#' # run all DVs at once using the same formula
#' SimAnova(~  group_size_ratio*standard_deviation_ratio, Final)
#'
#' }
#'
SimAnova <- function(formula, dat, rates = TRUE){

    # function borrowed and edited from lrs::etaSquared. Feb 29, 2016
    etaSquared <- function (x)
    {
        ss.tot <- sum((x$model[, 1] - mean(x$model[, 1]))^2)
        ss.res <- sum((x$residuals)^2)
        terms <- attr(x$terms, "factors")[-1, , drop = FALSE]
        l <- attr(x$terms, "term.labels")
        ss <- matrix(NA, length(l), 1)
        rownames(ss) <- l
        for (i in seq_along(ss)) {
            vars.this.term <- which(terms[, i] != 0)
            dependent.terms <- which(apply(terms[vars.this.term,
                                                 , drop = FALSE], 2, prod) > 0)
            m0 <- lm(x$terms[-dependent.terms], x$model)
            if (length(dependent.terms) > 1) {
                m1 <- lm(x$terms[-setdiff(dependent.terms,
                                          i)], x$model)
                ss[i] <- anova(m0, m1)$`Sum of Sq`[2]
            }
            else {
                ss[i] <- anova(m0, x)$`Sum of Sq`[2]
            }
        }
        ss <- rbind(ss, ss.res)
        eta2 <- ss/ss.tot
        eta2p <- ss/(ss + ss.res)
        k <- length(ss)
        eta2p[k] <- NA
        df <- anova(x)[, "Df"]
        ms <- ss/df
        Fval <- ms/ms[k]
        p <- 1 - pf(Fval, df, rep.int(df[k], k))
        E <- cbind(eta2, eta2p, ss, df, ms, Fval, p)
        E[k, 6:7] <- NA
        colnames(E) <- c("eta.sq", "eta.sq.part", "SS", "df",
                         "MS", "F", "p")
        rownames(E) <- rownames(ss)
        rownames(E)[k] <- "Residuals"
        siglevel <- cbind(E[,'p'] < .01, E[,'p'] < .001, E[,'p'] < .0001)
        siglevel <- apply(siglevel, 1, function(x) c('.', '*', '**', '***')[sum(x) + 1L])
        siglevel[length(siglevel)] <- ' '
        E <- data.frame(round(E[,-c(1:2)], 3), sig=siglevel,
                        round(E[,1:2], 3), check.names = FALSE)
        return(E)
    }

    # lst <- lapply(as.list(formula), as.character)
    # if(any(sapply(lst, function(x) all(x == '.')))){
    #     new <- as.formula(paste0('~', paste0(attr(dat, 'design_names')$design, collapse = '*')))
    #     formula <- update.formula(formula, new)
    # }
    if(length(as.list(formula)) == 2L){
        ys <- attributes(dat)$design_names$sim
        ret <- vector('list', length(ys))
        names(ret) <- ys
        for(i in 1L:length(ys)){
            f2 <- update.formula(formula, as.formula(paste0(ys[[i]], ' ~ .')))
            ret[[i]] <- SimAnova(formula=f2, dat=dat, rates=rates)
        }
        return(ret)
    }

    dat2 <- model.frame(formula, dat)
    if(rates){
        dat2[,1] <- suppressWarnings(qlogis(dat2[,1]))
        dat2[dat2[,1] == Inf, 1] <- max(dat2[,1])
        dat2[dat2[,1] == -Inf, 1] <- min(dat2[,1])
    }
    mod <- lm(formula, dat2)
    return(etaSquared(mod))
}
