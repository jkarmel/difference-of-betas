rel.diff.of.betas = function(n.A, n.B, successes.A, successes.B, num.simulations=10000, prior.params.A = c(1,1), prior.params.B = c(1,1)) {
  failures.A = n.A - successes.A
  failures.B = n.B - successes.B
  prior.params.A = c(1,1)
  prior.params.B = c(1,1)
  beta.A = rbeta(num.simulations, successes.A + prior.params.A[1], failures.A + prior.params.A[2])
  beta.B = rbeta(num.simulations, successes.B + prior.params.B[1], failures.B + prior.params.B[2])
  sort((beta.A - beta.B)/beta.A)
}

multiply.distribution.diffs = function(sample.diffs.from.A, sample.diffs.from.B){
  sort(unlist(lapply(sample.diffs.from.A, function(diff.from.A){ (1 + diff.from.A) * (1 + sample.diffs.from.B)}))) - 1
}
percentiles = c(.01, .025,.05, .1, .25, .5, .75, .90, .95, .975,.99)  

views.A = 6015
views.B = 5914
bids.A = 966
bids.B = 908
intros.A = 417
intros.B = 360

bid.rate.rel.diffs = rel.diff.of.betas(views.B, views.A, bids.B, bids.A, num.simulations = 10000)
intro.rate.rel.diffs = rel.diff.of.betas(bids.B, bids.A, intros.B, intros.A, num.simulations = 10000)
intros.per.view.diffs = rel.diff.of.betas(views.B, views.A, intros.B, intros.A, num.simulations = 10000)

view.to.intro.rate.as.product = multiply.distribution.diffs(bid.rate.rel.diffs, intro.rate.rel.diffs)

percentile = function (list){
  quantile(list, percentiles)
}

tiles = rbind(percentile(bid.rate.rel.diffs), percentile(intro.rate.rel.diffs), percentile(view.to.intro.rate.as.product))

tiles.T = t(tiles[,2:ncol(tiles)])
colnames(tiles.T) = c('bids per views change', 'intros per bid change', 'intros per view change' )
write.table(tiles.T, sep = '\t')