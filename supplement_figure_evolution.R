# Supplementary Figures- evolution
library(deSolve)
# load parameter_for_species file
source("~/parameters_for_species.R")
# load model.R file 
source("~/model.R")

solve_ode.beta <- function(initial_cond, times, treat.times, parameters){
  R0 <- c(1,5,10)
  ans.list <- list()
  for(i in 1:length(R0))
  {
    parameters$R0 <- R0[i]
    beta <- parameters$R0 * (parameters$mu_a + parameters$mu_h) * parameters$mu_l / (parameters$N * (parameters$lambda_eff - parameters$R0 * (parameters$mu_a + parameters$mu_h)))
    parameters$beta_eff <- beta 
    
    ans <- ode(y=initial_cond, times=times, func=odefn, parms=parameters, events=list(func = treatfun, time = treat.times))
    
    ansR0 <- cbind(ans, rep(beta, nrow(ans))) #***
    colnames(ansR0)[6] <- "variable" 
    ans.list[[i]] <- ansR0
  }
  
  df.ans <- as.data.frame(do.call(rbind, ans.list))
  df.ans$variable <- round(df.ans$variable, 5) 
  #	df.ans$variable <- factor(df.ans$variable, levels=unique(df.ans$variable)) 
  print(head(ans.list[[3]]))
  
  return(df.ans)
}

solve_ode.lambda <- function(initial_cond, times, treat.times, parameters){
  R0 <- c(1,5,10)
  ans.list <- list()
  for(i in 1:length(R0))
  {
    parameters$R0 <- R0[i]
    lambda <-  parameters$R0 * (parameters$mu_a + parameters$mu_h) * (parameters$beta_eff * parameters$N + parameters$mu_l) / (parameters$beta_eff * parameters$N) #lambda_eff
    parameters$lambda_eff <- lambda 
    
    ans <- ode(y=initial_cond, times=times, func=odefn, parms=parameters, events=list(func = treatfun, time = treat.times))
    
    ansR0 <- cbind(ans, rep(lambda, nrow(ans))) 
    colnames(ansR0)[6] <- "variable" 
    ans.list[[i]] <- ansR0
  }
  
  df.ans <- as.data.frame(do.call(rbind, ans.list))
  df.ans$variable <- round(df.ans$variable, 5) 
  #	df.ans$variable <- factor(df.ans$variable, levels=unique(df.ans$variable)) 
  print(head(ans.list[[3]]))
  
  return(df.ans)
}

solve_ode.N <- function(initial_cond, times, treat.times, parameters){
  R0 <- c(1,5,10)
  ans.list <- list()
  for(i in 1:length(R0))
  {
    parameters$R0 <- R0[i]
    N <-  parameters$R0 * (parameters$mu_a + parameters$mu_h) * parameters$mu_l / (parameters$beta_eff * (parameters$lambda_eff - parameters$R0 * (parameters$mu_a + parameters$mu_h))) #N
    parameters$N <- N 
    
    ans <- ode(y=initial_cond, times=times, func=odefn, parms=parameters, events=list(func = treatfun, time = treat.times))
    
    ansR0 <- cbind(ans, rep(N, nrow(ans))) #***
    colnames(ansR0)[6] <- "variable" 
    ans.list[[i]] <- ansR0
  }
  
  df.ans <- as.data.frame(do.call(rbind, ans.list))
  df.ans$variable <- round(df.ans$variable, 5) 
  #	df.ans$variable <- factor(df.ans$variable, levels=unique(df.ans$variable)) 
  print(head(ans.list[[3]]))
  
  return(df.ans)
}

solve_ode.mu_a <- function(initial_cond, times, treat.times, parameters){
  R0 <- c(1,5,10)
  ans.list <- list()
  for(i in 1:length(R0))
  {
    parameters$R0 <- R0[i]
    mu_a <-  ((parameters$beta_eff * parameters$N * parameters$lambda_eff)/(parameters$R0 * (parameters$beta_eff * parameters$N + parameters$mu_l))) - parameters$mu_h #mu_a
    parameters$mu_a <- mu_a 
    
    ans <- ode(y=initial_cond, times=times, func=odefn, parms=parameters, events=list(func = treatfun, time = treat.times))
    
    ansR0 <- cbind(ans, rep(mu_a, nrow(ans))) #***
    colnames(ansR0)[6] <- "variable" 
    ans.list[[i]] <- ansR0
  }
  
  df.ans <- as.data.frame(do.call(rbind, ans.list))
  df.ans$variable <- round(df.ans$variable, 5) 
  #	df.ans$variable <- factor(df.ans$variable, levels=unique(df.ans$variable)) 
  print(head(ans.list[[3]]))
  
  return(df.ans)
}

solve_ode.mu_l <- function(initial_cond, times, treat.times, parameters){
  R0 <- c(1,5,10)
  ans.list <- list()
  for(i in 1:length(R0))
  {
    parameters$R0 <- R0[i]
    mu_l <-  (parameters$beta_eff * parameters$N * (parameters$lambda_eff - parameters$R0 * (parameters$mu_a + parameters$mu_h))) / (parameters$R0 * (parameters$mu_a + parameters$mu_h)) #mu_l
    parameters$mu_l <- mu_l
    
    ans <- ode(y=initial_cond, times=times, func=odefn, parms=parameters, events=list(func = treatfun, time = treat.times))
    
    ansR0 <- cbind(ans, rep(mu_l, nrow(ans))) 
    colnames(ansR0)[6] <- "variable" 
    ans.list[[i]] <- ansR0
  }
  
  df.ans <- as.data.frame(do.call(rbind, ans.list))
  df.ans$variable <- round(df.ans$variable, 5) 
  #	df.ans$variable <- factor(df.ans$variable, levels=unique(df.ans$variable)) 
  print(head(ans.list[[3]]))
  
  return(df.ans)
}

p.beta <- list()
for(i in 1:length(parms.list)){
  parms <- parms.list[[i]]
  species_stub <- names(parms.list)[i]
  
  # Eco-Evo Case - w/ MDA
  n0<-c(W=100, L=30, x=0.01, y=0) #set initial conditions
  tf<-5*365
  freq <- 1.5
  times<-0:tf
  treat.times <- round(seq(365/freq, tf, by=365/freq))
  
  ans <- solve_ode.beta(n0, times, treat.times, parms)
  
  p.beta[[i]] <- ggplot(data=ans, aes(x=time, y=x, colour=as.factor(variable),linetype=as.factor(variable))) + theme_classic() + geom_line() + ylab("frequency") + xlab("time in days") + ylim(0, 1) +scale_color_discrete(name=expression(paste(italic(beta))),labels = function(x) signif(as.numeric(x), 1)) +  guides(linetype=FALSE)
}

p.lambda <- list()
for(i in 1:length(parms.list)){
  parms <- parms.list[[i]]
  species_stub <- names(parms.list)[i]
  
  # Eco-Evo Case - w/ MDA
  n0<-c(W=100, L=30, x=0.01, y=0) #set initial conditions
  tf<-5*365
  freq <- 1.5
  times<-0:tf
  treat.times <- round(seq(365/freq, tf, by=365/freq))
  
  ans <- solve_ode.lambda(n0, times, treat.times, parms)
  
  p.lambda[[i]] <- ggplot(data=ans, aes(x=time, y=x, colour=as.factor(variable),linetype=as.factor(variable))) + theme_classic() + geom_line() + ylab("frequency") + xlab("time in days") + ylim(0, 1) +scale_color_discrete(name=expression(paste(italic(lambda))),labels = function(x) signif(as.numeric(x), 1))+  guides(linetype=FALSE)
}

p.N <- list()
for(i in 1:length(parms.list)){
  parms <- parms.list[[i]]
  species_stub <- names(parms.list)[i]
  
  # Eco-Evo Case - w/ MDA
  n0<-c(W=100, L=30, x=0.01, y=0) #set initial conditions
  tf<-5*365
  freq <- 1.5
  times<-0:tf
  treat.times <- round(seq(365/freq, tf, by=365/freq))
  
  ans <- solve_ode.N(n0, times, treat.times, parms)
  
  p.N[[i]] <- ggplot(data=ans, aes(x=time, y=x, colour=as.factor(variable),linetype=as.factor(variable))) + theme_classic() + geom_line() + ylab("frequency") + xlab("time in days") + ylim(0, 1)  +scale_color_discrete(name= expression(italic(N)),labels = function(x) signif(as.numeric(x), 1))+  guides(linetype=FALSE)
}

p.mu_a <- list()
for(i in 1:length(parms.list)){
  parms <- parms.list[[i]]
  species_stub <- names(parms.list)[i]
  
  # Eco-Evo Case - w/ MDA
  n0<-c(W=100, L=30, x=0.01, y=0) #set initial conditions
  tf<-5*365
  freq <- 1.5
  times<-0:tf
  treat.times <- round(seq(365/freq, tf, by=365/freq))
  
  ans <- solve_ode.mu_a(n0, times, treat.times, parms)
  
  p.mu_a[[i]] <- ggplot(data=ans, aes(x=time, y=x, colour=as.factor(variable),linetype=as.factor(variable))) + theme_classic() + geom_line() + ylab("frequency") + xlab("time in days") + ylim(0, 1)  +scale_color_discrete(name=expression(paste(italic(mu[a]))),labels = function(x) signif(as.numeric(x), 1))+ guides(linetype=FALSE)
}

p.mu_l <- list()
for(i in 1:length(parms.list)){
  parms <- parms.list[[i]]
  species_stub <- names(parms.list)[i]
  
  # Eco-Evo Case - w/ MDA
  n0<-c(W=100, L=30, x=0.01, y=0) #set initial conditions
  tf<-5*365
  freq <- 1.5
  times<-0:tf
  treat.times <- round(seq(365/freq, tf, by=365/freq))
  
  ans <- solve_ode.mu_l(n0, times, treat.times, parms)
  
  p.mu_l[[i]] <- ggplot(data=ans, aes(x=time, y=x, colour=as.factor(variable),linetype=as.factor(variable))) + theme_classic() + geom_line() + ylab("frequency") + xlab("time in days") + ylim(0, 1)  +scale_color_discrete(name=expression(paste(italic(mu[l]))),labels = function(x) signif(as.numeric(x), 1))+  guides(linetype=FALSE)
}

parms.lists <- parms.list
parms.lists[[4]] <- "Trichostrongylus colubriformis" #remove country names
parms.lists[[5]] <- "Ostertagia circumcincta"
parms.lists[[6]] <- "Haemonchus contortus"

#subset to only plotting parasite traits: lambda, mu_a, mu_l and Beta
p1 = do.call(grid.arrange,c(p.lambda[5], p.mu_a[5],p.mu_l[5],p.beta[5]))
p1

create_column <- function(index) {
  
  pPanel <- ggarrange(p.lambda[[index]], p.mu_a[[index]], p.mu_l[[index]], p.beta[[index]], ncol = 1)
  
  pAnnotated <- annotate_figure(pPanel, top = text_grob(parms.lists[[index]], size = 10))
  return(pAnnotated)
}

annotated_columns <- lapply(c(1,3,4,6), create_column)
final_plot2 <- do.call(ggarrange, c(annotated_columns, ncol=4))
final_plot2
save_plot("SI_fig2v2.pdf", final_plot2, base_height=8, base_width=15)

annotated_columns <- lapply(c(2,5), create_column)
final_plot2 <- do.call(ggarrange, c(annotated_columns, ncol=2))
final_plot2
save_plot("Fig8_v2.pdf", final_plot2, base_height=8, base_width=8)
