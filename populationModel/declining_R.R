library(gsubfn)  # need 0.7-0 or later

generate_population_vector = function(init_population, R_0, k) {
  population = c(init_population)
  R = c(R_0)
  
  for( t in 2:k) {
    R[length(R) + 1] = R[length(R)]* (1- population[length(population)]/k)
    population[length(population) + 1] = population[length(population)] * R[length(R)]
  }
  return(list(population, R))
}

R_0 = 1
par(mfrow = c(1,2))
par(mfg=c(1,1)) 
plot(NULL, xlim=c(0,1000), ylim=c(0,1000), xlab="Generations", ylab = "N_t")
par(mfg=c(1,2)) 
plot(NULL, xlim=c(0,50), ylim=c(0,1), xlab="Generations", ylab = "R_t")

add_plot = function(k, color) {
  list[population, R] = generate_population_vector(init_population = 500, R_0 = R_0, k=k)
  par(mfg=c(1,1)) 
  lines(x=seq(k), y=population, col = color)
  par(mfg=c(1,2)) 
  lines(x=seq(k), y=R, col = color)
}
add_plot(k=2000, color = "red")
add_plot(k=1500, color = "blue")
add_plot(k=1000, color = "green")
par(mfg=c(1,2)) 
legend( x="topright", 
        legend=c("k=2k","k=1.5k", "k=1k"),
        col=c("red","blue", "green"))
par(mfg=c(1,1)) 
legend( x="topright", 
        legend=c("k=2k","k=1.5k", "k=1k"),
        col=c("red","blue", "green"))
