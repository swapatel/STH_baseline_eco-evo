# functions for running numerical solutions of model
library(deSolve)
# need to source parameters_for_species file

odefn <- function(t, n, parms) {
  with(as.list(c(parms, n)), {
    dW = beta_eff*N*L - (mu_h + mu_a)*W
    dL = 1/2 *W *lambda_eff - (mu_l + beta_eff*N)*L
    dx = beta_eff*N*L*(y-x)/W
    dy = 1/2*lambda_eff*W *(x-y)/L    
    return(list(c(dW,dL,dx,dy))) 
  })
}

# treatment function
treatfun <- function(t, y, parms){
  with(as.list(parms, y), {
    x <- y[3]
    mubar_MDA <- (mu_AA*x^2 + mu_AB*2*x*(1-x) + mu_BB*(1-x)^2)
    mubar_A <- (mu_AA*x + mu_AB*(1-x)) 
    y[1] <- y[1]*(1-cov*mubar_MDA)
    y[3] <- y[3]*(1-cov*mubar_A)/(1-cov*mubar_MDA)
    return(y)
  })
}

solve_ode <- function(initial_cond, times, treat.times, parameters){
  ans <- ode(y=initial_cond, times=times, func=odefn, parms=parameters, events=list(func = treatfun, time = treat.times))
  print(ans[seq(1,300,by=30), ])
  print(head(ans))
  return(ans)
}


