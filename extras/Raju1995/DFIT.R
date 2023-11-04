#' Raju's Differential Item/Test Framework
#'
#' Raju's detection framework for non-compensatory DIF and compensatory DTF/DBF. As a statistical detection
#' framework DFIT should should not be used because it is highly dependent upon the sampling charactersitics,
#' but the relative effect sizes could be useful within a given sample. Therefore, the interpretation
#' of the NCDIF values relative to other NCDIF values in a
#' given sample is fine, but generalizing NCDIF accross different sample and populations is highly
#' problematic and should be avoided.
#'
#' @param mod a multiple Group model with only two groups. The focal group is always
#'      understood to be group 2
#' @param focal_items an integer vector indicating which items to inspect with DFIT. Including only
#'   one value will perform a DIF test, while including more than one will perform a simultaneous
#'   test (DBF). If missing, a simultaneous test using all the items will be used (i.e., DTF)
#' @param method factor score estimation method
#' @param ... additional arguments to pass to \code{fscores()}
#'
#' @references
#'
#' Oshima, T. C.; Raju, N. S.; Flowers, C. P. & Slinde, J. A. (1998). Differential bundle
#'   functioning using the DFIT framework: Procedures for identifying possible sources of
#'   differential functioning. Applied Measurement in Education, 11, 353-369
#'
#' Raju, N. S.; van der Linden, W. J. & Fleer, P. F. (1995). IRT-based internal measures of
#'   differential functioning of items and tests. Applied Psychological Measurement, 19, 353-368.
#'
#' @example
#' \dontrun{
#'
#' library(mirt)
#'
#' set.seed(1234)
#' n <- 30
#' N <- 500
#'
#' # only first 5 items as anchors
#' model <- 'F = 1-30
#'   CONSTRAINB = (1-5, a1), (1-5, d)'
#'
#' a <- matrix(1, n)
#' d <- matrix(rnorm(n), n)
#' group <- c(rep('G1', N), rep('G2', N*2))
#'
#' ## -------------
#' # groups completely equal
#' dat1 <- simdata(a, d, N, itemtype = 'dich')
#' dat2 <- simdata(a, d, N*2, itemtype = 'dich')
#' dat <- rbind(dat1, dat2)
#' mod <- multipleGroup(dat, model, group=group,
#'   invariance=c('free_means', 'free_var'))
#'
#' DFIT(mod)
#' tail(round(DFIT(mod, DIF=TRUE), 3))
#' DFIT(mod, method = 'MAP')
#' tail(round(DFIT(mod, method = 'MAP', DIF=TRUE), 3))
#'
#' DRF(mod)
#' tail(DRF(mod, DIF=TRUE))
#'
#' # change reference group
#' group2 <- c(rep('G1', N*2), rep('G2', N))
#' dat2 <- rbind(dat2, dat1)
#' mod2 <- multipleGroup(dat2, model, group=group2,
#'   invariance=c('free_means', 'free_var'))
#' anova(mod, mod2) #equivalent models
#'
#' DFIT(mod2) # very different p-values
#' tail(round(DFIT(mod2, DIF=TRUE), 3))
#'
#' DRF(mod2) #same, just flipped
#' tail(DRF(mod2, DIF=TRUE))
#'
#'
#'
#' ## -------------
#' ## systematic differing slopes and intercepts (clear DTF)
#' dat1 <- simdata(a, d, N, itemtype = 'dich', mu=.50, sigma=matrix(1.5))
#' dat2 <- simdata(a + c(numeric(15), rnorm(n-15, 1, .25)), d + c(numeric(15), rnorm(n-15, 1, .5)),
#'   N*2, itemtype = 'dich')
#' dat <- rbind(dat1, dat2)
#' mod3 <- multipleGroup(dat, model, group=group,
#'   invariance=c('free_means', 'free_var'))
#' plot(mod3) #visible DTF happening
#' plot(mod3, type ='itemscore')
#'
#' # DIF(mod3, c('a1', 'd'), items2test=16:30)
#' DFIT(mod3)
#' tail(round(DFIT(mod3, DIF=TRUE), 3))
#'
#' DRF(mod3)
#' tail(DRF(mod3, DIF=TRUE))
#'
#'
#' #############
#' # complete cancellation
#' N <- 10000
#' a <- a2 <- matrix(rlnorm(n, .2, .2))
#' d <- d2 <- matrix(rnorm(n))
#' group <- c(rep('G1', N), rep('G2', N))
#'
#' a2[1:2] <- 1
#' a[1:2] <- 2
#' d2[1:2] <- -1
#' d[1:2] <- 1
#'
#' a[3:4] <- 1
#' a2[3:4] <- 2
#' d[3:4] <- -1
#' d2[3:4] <- 1
#'
#' dat1 <- simdata(a, d, N, itemtype = 'dich')
#' dat2 <- simdata(a2, d2, N, itemtype = 'dich')
#' dat <- rbind(dat1, dat2)
#' mod <- multipleGroup(dat, 1, group=group,
#'   invariance=c('free_means', 'free_var', colnames(dat)[-c(1:4)]))
#'
#' plot(mod, type = 'itemscore')
#' plot(mod)
#'
#' DRF(mod)
#' DRF(mod, DIF=TRUE)
#' DRF(mod, focal_items = c(1:4))
#'
#' DFIT(mod)
#' DFIT(mod, DIF=TRUE)
#'
#' }
DFIT <- function(mod, focal_items, method = 'EAP', DIF = FALSE, ...){
    require(mirt)
    stopifnot(extract.mirt(mod, 'ngroups') == 2L)
    J <- length(extract.mirt(mod, 'K'))
    index <- 1L:J
    if(missing(focal_items)) focal_items <- index
    focal <- extract.group(mod, 2L)
    gp <- mirt:::ExtractGroupPars(focal@ParObjects$pars[[J+1L]])
    fs1 <- fscores(focal, method = method, full.scores=TRUE, verbose=FALSE,
                   mean = gp$gmeans, cov = gp$gcov, ...)
    ref <- extract.group(mod, 1L)
    ref@Data <- focal@Data
    fs2 <- fscores(ref, method = method, full.scores=TRUE, verbose=FALSE,
                   mean = gp$gmeans, cov = gp$gcov, ...)
    T1 <- expected.test(mod, fs1, group=1L, individual=TRUE, which.items = focal_items)
    T2 <- expected.test(mod, fs2, group=2L, individual=TRUE, which.items = focal_items)
    d <- T2 - T1
    D <- rowSums(d)
    NCDIF <- colMeans(d^2)
    CDIF <- apply(d, 2, function(dj)
        cov(dj, D) + mean(dj) * mean(D))
    vars <- colVars(d)
    df <- nrow(d)
    if(DIF){
        X2 <- df * NCDIF / vars
        p.X2 <- 1 - pchisq(X2, df=df)
        ret <- data.frame(CDIF, NCDIF, X2, df, p.X2)
        rownames(ret) <- colnames(mod@Data$data)[focal_items]
    } else {
        DTF <- sum(CDIF)
        X2 <- sum(D^2) / var(D)
        p.X2 <- 1 - pchisq(X2, df=df)
        ret <- data.frame(DTF=DTF, X2=X2, df=df, p.X2=p.X2)
    }
    ret
}
