# Figure 9 - optimization in frequency and coverage
library(deSolve)
# load parameter_for_species file
source("parameters_for_species.R")
# load model.R file 
source("model.R")

parms_vary <- c(0.1, 0.5, 0.9)
freq <- 0.5/0.9
parms<-parmsHookworm

n0<-c(W=100,L=1000, x=0, y=0) #set initial conditions
tf<-20*365
times<-0:tf
if(365/freq < tf) treat.times <- round(seq(365/freq, tf, by=365/freq))
treat.times <- round(seq(366/freq, tf, by=366/freq))
ans <- solve_ode(n0, times, treat.times, parms)

worm_mat <- ans[,2]
x_mat <- ans[,4]

pdf(file="Fig9A.pdf", width=4, height=3)

plot(times, ans[,2], type= "l", ylim=c(0,200), xlab="Time in days", ylab="Adult worms")

for(i in 1:length(parms_vary)){
  print(i)
  freq <- 0.5/parms_vary[i]
  parmsHookworm<-list(
    beta_eff = 0.35/365, # host contact with environment
    mu_h =  0.0167/365, # 1/60/365, # host death rate; 
    lambda_eff = 1.095e6/365, # number of eggs produced per fertilized adult female
    mu_l = 30/365, # larvae death rate
    N = 0.000123, # total host population size
    mu_a = 0.5/365, #natural death rate of adult worms in hosts
    mu_AA = 0.1, # drug induced death probability for homozygote resistant
    mu_AB = 0.5, # heterozygote
    mu_BB = 0.9, # susceptible homozygote  
    cov = parms_vary[i] #coverage
  )
  parms <- parmsHookworm
  n0<-c(W=100,L=1000, x=0, y=0) #set initial conditions
  tf<-20*365
  times<-0:tf
  if(365/freq < tf) treat.times <- round(seq(365/freq, tf, by=365/freq))
  treat.times <- round(seq(366/freq, tf, by=366/freq))
  ans <- solve_ode(n0, times, treat.times, parms)
  
  worm_mat <- rbind(worm_mat, ans[,2])
  x_mat <- rbind(x_mat, ans[,4])
  
  points(times, worm_mat[i,], type= "l", col=i+1)
  
}
dev.off()

plot(times, x_mat[1,], type= "l", ylim=c(0,1), xlab="Time in days", ylab="Allele Frequency (x)")
for(i in 1:length(parms_vary)){
  points(times, x_mat[i,], type= "l", col=i+1)
}

