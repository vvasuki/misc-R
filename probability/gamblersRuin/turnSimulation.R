# Simulation of gamblers ruin. Base code got from internet, and tweaked.
# Converges pretty slowly. For large values of simulation.rounds, runs quite slowly.


# Basic parameters.
simulation.rounds = 100
x.capital = 25
y.capital = 1000
x.p = .495 # Probability of winning 

# A function to simulate a game until ruin.
gen.ruin = function(n, x.cnt, y.cnt, x.p) {
  # Draw n samples from a binomial distribution with turn success probability x.p.
  x.rnd = rbinom(n, 1, p = x.p)
  # Mark 0-outcomes as -1 (loosing money to y).
  x.rnd[x.rnd == 0] = -1
  # Generate a vector showing x's money at each turn. 
  ruin.data = cumsum(x.rnd) + x.cnt

  # Find ruin conditions, if any. Then set x's wealth to 0 beyond the ruin state.
  if (any(which(ruin.data >= x.cnt + y.cnt)) | any(which(ruin.data <= 0))) { 
    cut.data = 1 + min(which(ruin.data >= x.cnt + y.cnt), which(ruin.data <= 0))

    ruin.data[cut.data:length(ruin.data)] = 0

  }

  return(ruin.data)

}

# Create a matrix where each column represents a run. 
simulation.results = replicate(simulation.rounds, gen.ruin(n = 1000, x.cnt = x.capital, y.cnt = y.capital, x.p = x.p))
simulation.results[simulation.results == 0] = NA

# For each trial column, determine the ruin turn. 
ruin.turn = apply(simulation.results == x.capital + y.capital | is.na(simulation.results), 2, which.max)
# Publish a histogram of the turns needed for ruin. Mark the median and the mean.
hist(ruin.turn, nclass = 100, col = '8', main = "Distribution of Number of Turns",
     xlab = "Turn Number")
# Mark the mean and median
abline(v = mean(ruin.turn), lwd = 3, col = 'red')
abline(v = median(ruin.turn), lwd = 3, col = 'green')

mean.state = apply(simulation.results, 1, mean, na.rm = T)
plot(mean.state, xlim = c(0, which.max(mean.state)), ylim = c(0, 20), ylab = "Points", xlab = "Number of Plays", pch = 16, cex = .5, col = 'green')
lines(mean.state, col = 'green')
points(x.capital + y.capital - mean.state, pch = 16, cex = .5, col = 'blue')
lines(x.capital + y.capital - mean.state, col = 'blue')