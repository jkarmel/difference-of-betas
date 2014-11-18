rel.diff.of.betas = function(n.A, n.B, successes.A, successes.B, num.simulations=10000, prior.params.A = c(1,1), prior.params.B = c(1,1)) {
  failures.A = n.A - successes.A
  failures.B = n.B - successes.B
  prior.params.A = c(1,1)
  prior.params.B = c(1,1)
  beta.A = rbeta(num.simulations, successes.A + prior.params.A[1], failures.A + prior.params.A[2])
  beta.B = rbeta(num.simulations, successes.B + prior.params.B[1], failures.B + prior.params.B[2])
  sort((beta.A - beta.B)/beta.A)
}

percentiles = c(.01, .025,.05, .1, .25, .5, .75, .90, .95, .975,.99)  

views.A = 9496
views.B = 9315
bids.A = 1535
bids.B = 1420
intros.A = 660
intros.B = 571

bid.rate.rel.diffs = rel.diff.of.betas(views.B, views.A, bids.B, bids.A, num.simulations = 10000)

print(quantile(bid.rate.rel.diffs, percentiles))

