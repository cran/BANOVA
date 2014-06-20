BANOVA.Bern <-
function(l1_formula = 'NA', l2_formula = 'NA', data, id, burnin = 500, sample = 1000, thin = 1, jags = findjags()){
  if (jags == 'JAGS not found') stop('Please install the JAGS software (Version 3.4 or above). Check http://mcmc-jags.sourceforge.net/')
  sol <- BANOVA.BernNormal(l1_formula, l2_formula, data, id, burnin = burnin, sample = sample, thin = thin, jags = jags)
  sol$call <- match.call()
  class(sol) <- 'BANOVA.Bern'
  sol
}
