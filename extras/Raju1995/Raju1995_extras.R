###################

# Table 1
reference <- data.frame(a = c(rep(.55,2), rep(.73,8), rep(1.00, 20), rep(1.36,8), rep(1.80,2)),
                        b = c(rep(0,2), rep(-1.04,2), rep(0, 4), rep(1.04,2),
                              rep(-1.96, 2), rep(-1.04, 4), rep(0, 8), rep(1.04,4),
                              rep(1.96,2), rep(-1.04, 2), rep(0,4), rep(1.04,2), rep(0,2)))
focal1 <- focal2 <- focal3 <- reference

# DIF effects
focal1[c(5,10), ] <- matrix(c(.73, .73, 1.00, 2.04), ncol=2)
focal2[c(5,10,15,20), ] <- matrix(c(.73, .73, 1.00, 1.00,
                                   1.00, 1.54, -.04, .5), ncol=2)
focal3[c(5,10,15,20,25,30, 35,40), ] <- matrix(
    c(.73, .73, .5, .5, 1.00, 1.00, .86, 1.30,
      1.00, 1.54, -.54, 0, 2.04, 2.46, .5, 0), ncol=2)

itempars_uni <- list(reference=reference, focal1=focal1, focal2=focal2, focal3=focal3)

###################

#Table 2
focal1 <- focal2 <- focal3 <- reference
focal1[c(5,6), ] <- matrix(c(.73, .73, 1, -1), ncol=2)
focal2[c(5,6,15,16), ] <- matrix(c(.73, .73, 1, 1,
                                   1,-1,-.54,-1.54), ncol=2)
focal3[c(5,6,15,16,25,26,29,30), ] <- matrix(
    c(1.23,.73,.5,1,1,1,1,1,
      0,0,-1.04,-1.04,2.04,.04,2.46,1.46), ncol=2)

reference_focal3 <- reference
reference_focal3[c(6, 16), ] <- matrix(c(1.23, .5, 0, -1.04), ncol=2)

itempars_bi <- list(reference=reference, focal1=focal1, focal2=focal2,
                    reference_focal3=reference_focal3, focal3=focal3)

###################

# transform to slope-intercept form for mirt
itempars_uni2 <- lapply(itempars_uni, function(pars){
    t(apply(data.frame(pars, g=0, u=1), 1L, mirt::traditional2mirt, cls='2PL', ncat=2L))
})

itempars_bi2 <- lapply(itempars_bi, function(pars){
    t(apply(data.frame(pars, g=0, u=1), 1L, mirt::traditional2mirt, cls='2PL', ncat=2L))
})

###################

# fixed_object input
fo <- list(uni=itempars_uni2, bi=itempars_bi2)

# clean-up work-space
rm(reference, focal1, focal2, focal3, itempars_uni, itempars_bi,
   itempars_uni2, itempars_bi2, reference_focal3)
