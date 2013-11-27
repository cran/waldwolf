waldwolf.test <-
function(x){
  
  DNAME <- deparse(substitute(x))
  m = median(x)
  N = length(x)
  S <- array(dim = N)
  n1 = 0; n2= 0
  for (i in 1:N)
  {
    if(x[i] >= m)
    {S[i] = "A"; n1= n1+1}
    else
    {S[i] = "B"; n2 = n2+1}
  }
  t = 1
  for (i in 2:N)
  {
    if(S[i] != S[i-1])
      t = t + 1
  }
  
  mu <- 2*n1*n2/N + 1
  s <- sqrt(2*n1*n2*(2*n1*n2-N)/(N^2*(N-1)))
  
  pvalue <- pnorm(t, mean = mu, sd = s)
  
  WAL <- list(statistic = c(T1 = t), p.value = pvalue, method = "Wald-Wolfowitz trend test", 
              data.name = DNAME)
  class(WAL) <- "htest"
  return(WAL)
}
