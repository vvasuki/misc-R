# Simulation of gamblers ruin. Base code got from internet, and tweaked.
# Converges pretty slowly. For large values of simulation.rounds, rund quite slowly.


# Basic parameters.
simulation.rounds = 100000
x.capital = 25
y.capital = 100000
x.p = .495 # Probability of winning 

# A function to simulate a game until ruin.
gen.ruin = function(n, x.cnt, y.cnt, x.p) {
  x.rnd = rbinom(n, 1, p = x.p)
  x.rnd[x.rnd == 0] = -1
  ruin.data = cumsum(x.rnd) + x.cnt

  if (any(which(ruin.data >= x.cnt + y.cnt)) | any(which(ruin.data <= 0))) { cut.data = 1 + min(which(ruin.data >= x.cnt + y.cnt), which(ruin.data <= 0))

    ruin.data[cut.data:length(ruin.data)] = 0

  }

  return(ruin.data)

}

simulation.results = replicate(simulation.rounds, gen.ruin(n = 10000, x.cnt = x.capital, y.cnt = y.capital, x.p = x.p))
simulation.results[ruin.sim == 0] = NA

# Publish a histogram of the turns needed for ruin. Mark the median and the mean.
hist(apply(simulation.results == x.capital + y.capital | is.na(simulation.results), 2, which.max), nclass = 100, col = '8', main = "Distribution of Number of Turns",
     xlab = "Turn Number")
abline(v = mean(apply(simulation.results == x.capital + y.capital | is.na(simulation.results), 2, which.max)), lwd = 3, col = 'red')
abline(v = median(apply(simulation.results == x.capital + y.capital | is.na(simulation.results), 2, which.max)), lwd = 3, col = 'green')


x.annihilation = apply(simulation.results == x.capital + y.capital, 2, which.max)
(x.probability_annihilation = length(x.annihilation[x.annihilation != 1]) / simulation.rounds)


state.cnt = simulation.results
state.cnt[state.cnt != x.cnt + y.cnt] = 0
state.cnt[state.cnt == x.cnt + y.cnt] = 1
mean.state = apply(simulation.results, 1, mean, na.rm = T)
plot(mean.state, xlim = c(0, which.max(mean.state)), ylim = c(0, 20), ylab = "Points", xlab = "Number of Plays", pch = 16, cex = .5, col = 'green')
lines(mean.state, col = 'green')
points(x.capital + y.capital - mean.state, pch = 16, cex = .5, col = 'blue')
lines(x.capital + y.capital - mean.state, col = 'blue')