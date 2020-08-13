x.p = 0.495
x.capital = 25
y.capital = 10000
maxCapital = x.capital + y.capital

getCapitalProbabilities = function(prevCapitalProbabilities) {
  capitalProbabilities = rep(0, maxCapital + 1)
  for (i in seq(maxCapital + 1)) {
    if(i < maxCapital) {
      capitalProbabilities[i] = prevCapitalProbabilities[i+1]*(1-x.p)
    }
    if(i > 1) {
      capitalProbabilities[i] = capitalProbabilities[i] + prevCapitalProbabilities[i-1]*x.p
    }
  }
  return(capitalProbabilities)
}

