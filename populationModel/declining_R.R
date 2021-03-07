
generate_population_vector = function(init_population, R_0, k) {
  population = c()
  population[1] = init_population

  next_gen =  function(N_t) {
    return (N_t * R_0 * (1- N_t/k))
  }
  
  for( t in 2:k) {
    population[t] = next_gen(population[t-1])
  }
  return(population)
}

plot(x=seq(2000), y=generate_population_vector(init_population = 500, R_0 = 1, k=2000), col = 'blue', xlab="Generations", ylab = "N_t")
line(x=seq(1000), y=generate_population_vector(init_population = 500, R_0 = 1, k=1000), col = 'green', xlab="Generations", ylab = "N_t")
