# replicate generation of Table 4?
nitems <- 40
if(FALSE){
    a <- if(model == '1PL') rep(1, nitems) else rlnorm(nitems, meanlog = 0, sdlog=1/2)
    b <- rnorm(nitems)
    c <- if(model == '3PL') rbeta(nitems, 5, 17) else rep(0, nitems)
} else {
    mat <- matrix(c(
        1.1005,  0.4078, 0.2228,   
        2.2093,  0.5696, 0.2332,   
        1.4493, -1.0610, 0.2337,  
        0.7514, -0.2437, 0.1445,  
        1.5789,  0.3206, 0.2581,   
        0.6425, -1.3762, 0.2712,  
        1.6254, -0.9800, 0.1232,  
        1.3415, -0.6881, 0.1954,  
        0.9180, -0.3526, 0.2709,  
        1.8027,  0.2400, 0.2984,   
        0.8159,  0.5917, 0.0587,   
        0.9375,  1.8891, 0.1405,   
        0.9126, -0.2690, 0.2339,  
        1.9395,  0.3673, 0.2387,   
        0.3746, -0.9681, 0.3527,  
        0.6730, -1.2601, 0.1206,  
        0.4166,  0.5225, 0.1244,   
        1.2093, -1.3356, 0.1167,  
        0.9486,  0.9515, 0.2787,   
        1.4916,  0.9811, 0.1923,
        0.5659, -0.1257, 0.3426,
        0.6128, -0.7826, 0.1925,
        1.1037,  0.0615, 0.2324,
        1.9886,  0.4244, 0.1396,
        0.5691, -0.7350, 0.2059,
        1.0346,  0.9836, 0.3124,
        1.1384, -1.2651, 0.1832,
        3.3488, -0.2252, 0.1811,
        2.6306, -0.6576, 0.2537,
        0.6652,  1.7007, 0.2184,
        1.0342,  1.0805, 0.2261,
        1.0163, -2.0452, 0.3464,
        1.2945,  0.1627, 0.1455,
        1.6521,  0.0573, 0.3861,
        0.9696,  1.2171, 0.1046,
        1.2369,  2.1226, 0.1656,
        0.7812,  0.4228, 0.2696,
        0.7728, -0.1656, 0.1780,
        0.5441, -0.2055, 0.1961,
        1.4025,  1.2841, 0.2917), ncol=3, byrow=TRUE)
    a <- mat[,1]; b <- mat[,2]; c <- mat[,3]
}
fo <- list(a=a, b=b, c=c) 



#################################################################
# Helper functions

# transform difficulty parameters to intercepts (for mirt)
b2d <- function(a,b,c){
    nitems <- length(a)
    d <- numeric(nitems)
    for(i in 1:nitems)
        d[i] <- mirt::traditional2mirt(c('a'=a[i], 'b'=b[i], 
                                         'g'=c[i], 'u'=1), cls='3PL', ncat = 2)[2]
    d
}

# helper function for organizing parameter sets
get_parameters <- function(condition, fixed_objects){
    nitems <- condition$nitems
    model <- condition$model
    pick <- 1:nitems
    b <- fixed_objects$b[pick]
    d <- fixed_objects$d[pick]
    a <- if(model == "1PL") rep(1, nitems) else 
        fixed_objects$a[pick]
    c <- if(model  == '3PL') rbeta(nitems, 5, 7) else rep(0, nitems)
    d <- b2d(a=a, b=b, c=c)
    ret <- list(a=a, b=b, c=c, d=d)
    ret
}

# find lowest information value and counts corresponding to lowest model (Table 9)
best_info <- function(infomat)
    colMeans(infomat == apply(infomat, 1, min))
