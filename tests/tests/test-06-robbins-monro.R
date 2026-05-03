context('RobbinsMonro')

test_that('RobbinsMonro (Polyak-Juditsky)', {

  .f <- \(x) x - mean(rnorm(10000, mean = pi, sd = 0.5))

  rm <- RobbinsMonro(f = .f,
                     p = 2.1, # initial guess
                     Polyak_Juditsky = TRUE,
                     tol = 0.001)

  expect_equal(c(rm$root), mean(rm$history))
})
