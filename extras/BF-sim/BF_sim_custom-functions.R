#-------------------------------------------------------------------
# Custom functions required (not defined in other R packages)

#' Jacknife variance test
#' 
#' @param DV a numeric vector
#' @param group a character/factor vector containing the group membership
Jacknife_vartest <- function(DV, group){
    stopifnot(length(DV) == length(group))
    nms <- unique(group)
    g <- length(nms)
    Ns <- s2 <- Ui. <- numeric(g)
    Uij <- vector('list', g)
    for(i in 1:g){
        pick <- group == nms[i]
        sub <- subset(DV, pick)
        Ns[i] <- length(sub)
        s2[i] <- var(sub)
        sij2 <- numeric(Ns[i])
        for(j in 1:length(sub))
            sij2[j] <- var(sub[-j])
        Uij[[i]] <- Ns[i] * log(s2[i]) - (Ns[i]-1) * log(sij2)
        Ui.[i] <- mean(Uij[[i]])
    }
    U.. <- mean(do.call(c, Uij))
    num <- sum(Ns * (Ui. - U..)^2) / (g-1)
    den <- 0
    for(i in 1:g)
        den <- den + sum((Uij[[i]] - Ui.[i])^2) / sum(Ns-1)
    J <- num/den
    df1 <- g-1; df2 <- sum(Ns-1)
    p.value <- pf(J, df1, df2, lower.tail = FALSE)
    data.frame(J=J, df1=df1, df2=df2, p.value=p.value, row.names="")
}

#' Layard's variance test
#' 
#' @param DV a numeric vector
#' @param group a character/factor vector containing the group membership
Layard_vartest <- function(DV, group){
    stopifnot(length(DV) == length(group))
    nms <- unique(group)
    Ns <- table(group)
    N <- sum(Ns)
    g <- length(nms)
    log_s2 <- numeric(g)
    deviation <- numeric(N)
    for(i in 1:g){
        pick <- group == nms[i]
        sub <- subset(DV, pick)
        xbar <- mean(sub)
        deviation[pick] <- sub - xbar
        log_s2[i] <- log(var(sub))
    }
    gamma <- N * sum(deviation^4) / (sum(deviation^2)^2) - 3
    tau2 <- 2 + (1 - g/N) * gamma
    S <- sum( (Ns-1) * (log_s2 - sum((Ns-1) * log_s2)/sum(Ns-1))^2) / tau2
    df <- g - 1
    p.value <- pchisq(S, df, lower.tail=FALSE)
    data.frame(S=S, df=df, p.value=p.value, row.names = "")
}
