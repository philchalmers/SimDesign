# Function to present bootstrap standard errors estimates for Monte Carlo simulation meta-statistics
#
# This function generates bootstrap confidence intervals for the meta-statistics called within the
# \code{summarise} function with \code{\link{runSimulation}}
# that included the argument \code{bootSE = TRUE}.
#
# @param results object returned from \code{\link{runSimulation}} where \code{bootSE = TRUE}
#   was used
#
# @param CI desired confidence interval level for each meta-statistic using the bootstrap
#  SE estimate. Default is .99, which constructs a 99\% confidence interval
#
# @references
# Sigal, M. J., & Chalmers, R. P. (2016). Play it again: Teaching statistics with Monte
# Carlo simulation. \code{Journal of Statistics Education, 24}(3), 136-156.
# \doi{10.1080/10691898.2016.1246953}
#
# @author Phil Chalmers \email{rphilip.chalmers@@gmail.com}
#
# @export
#
SimBoot <- function(results, summarise, condition, fixed_objects, boot_method,
                    boot_draws, CI){
    stopifnot(boot_method %in% c('basic', 'percentile', 'norm'))
    replications <- if(is.data.frame(results)) nrow(
        results) else length(results)
    t0 <- summarise(condition=condition, results=results,
                    fixed_objects=fixed_objects)
    t <- do.call(rbind, lapply(1L:boot_draws, function(r){
        pick <- rint(n = replications, min = 1L, max = replications)
        tmp <- if(!is.data.frame(results)) results[pick]
        else results[pick, , drop=FALSE]
        summarise(results=tmp, condition=condition, fixed_objects=fixed_objects)
    }))
    if(boot_method == 'basic'){
        ql <- apply(t, 2L, quantile, prob = (1 - CI)/2)
        qu <- apply(t, 2L, quantile, prob = 1 - (1 - CI)/2)
        lower <- 2*t0 - qu
        upper <- 2*t0 - ql
    } else if(boot_method == 'percentile'){
        lower <- apply(t, 2L, quantile, prob = (1 - CI)/2)
        upper <- apply(t, 2L, quantile, prob = 1 - (1 - CI)/2)
    } else if(boot_method == 'norm'){
        SEs <- apply(t, 2L, sd)
        lower <- t0 + qnorm((1 - CI)/2) * SEs
        upper <- t0 + qnorm(1 - (1 - CI)/2) * SEs
    }
    CIs <- as.vector(rbind(lower, upper))
    names(CIs) <- paste0(as.vector(sapply(paste0("BOOT_", names(t0), "_"), function(x)
        paste0(x, c( (1 - CI)/2 * 100, (1 - (1 - CI)/2)*100)))), "%")
    CIs
}
