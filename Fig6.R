# Figure 8 - Evolutionary trajectories dependent on parameters
library(ggplot2)
library(ggpubr)
library(viridis)
library(deSolve)
library(gridExtra)
library(cowplot)
library(grid)

# load parameter_for_species file
source("parameters_for_species.R")
# load model.R file 
source("model.R")

solve_ode.beta <- function(initial_cond, times, treat.times, parameters){
  R0 <- c(1,5,10)
  ans.list <- list()
  for(i in 1:length(R0))
  {
    parameters$R0 <- R0[i]
    beta <- parameters$R0 * (parameters$mu_a + parameters$mu_h) * parameters$mu_l / (parameters$N * (parameters$lambda_eff - parameters$R0 * (parameters$mu_a + parameters$mu_h)))
    parameters$beta_eff <- beta 
    
    ans <- ode(y=initial_cond, times=times, func=odefn, parms=parameters, events=list(func = treatfun, time = treat.times))
    
    ansR0 <- cbind(ans, rep(beta, nrow(ans))) 
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
  
  p.beta[[i]] <- ggplot(data=ans, aes(x=time, y=x, colour=as.factor(variable),linetype=as.factor(variable))) + theme_classic() + geom_line() + ylab("Frequency") + xlab("Time in days") + ylim(0, 1) +scale_color_discrete(name=expression(paste(italic(beta))),labels = function(x) signif(as.numeric(x), 1)) +  guides(linetype=FALSE)
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
  
  p.lambda[[i]] <- ggplot(data=ans, aes(x=time, y=x, colour=as.factor(variable),linetype=as.factor(variable))) + theme_classic() + geom_line() + ylab("Frequency") + xlab("Time in days") + ylim(0, 1) +scale_color_discrete(name=expression(paste(italic(lambda))),labels = function(x) signif(as.numeric(x), 1))+  guides(linetype=FALSE)
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
  
  p.N[[i]] <- ggplot(data=ans, aes(x=time, y=x, colour=as.factor(variable),linetype=as.factor(variable))) + theme_classic() + geom_line() + ylab("Frequency") + xlab("Time in days") + ylim(0, 1)  +scale_color_discrete(name= expression(italic(N)),labels = function(x) signif(as.numeric(x), 1))+  guides(linetype=FALSE)
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
  
  p.mu_a[[i]] <- ggplot(data=ans, aes(x=time, y=x, colour=as.factor(variable),linetype=as.factor(variable))) + theme_classic() + geom_line() + ylab("Frequency") + xlab("Time in days") + ylim(0, 1)  +scale_color_discrete(name=expression(paste(italic(mu[w]))),labels = function(x) signif(as.numeric(x), 1))+ guides(linetype=FALSE)
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
  
  p.mu_l[[i]] <- ggplot(data=ans, aes(x=time, y=x, colour=as.factor(variable),linetype=as.factor(variable))) + theme_classic() + geom_line() + ylab("Frequency") + xlab("Time in days") + ylim(0, 1)  +scale_color_discrete(name=expression(paste(italic(mu[l]))),labels = function(x) signif(as.numeric(x), 1))+  guides(linetype=FALSE)
}

#subset to only plotting parasite traits: lambda, mu_a, mu_l and Beta
#p1 = do.call(grid.arrange,c(p.lambda[2], p.mu_a[2],p.mu_l[2],p.beta[2]))

# Remove x-axis labels from plots except beta
p.lambda <- lapply(p.lambda, function(p) p + theme(axis.title.x = element_blank()))
p.mu_a <- lapply(p.mu_a, function(p) p + theme(axis.title.x = element_blank()))
p.mu_l <- lapply(p.mu_l, function(p) p + theme(axis.title.x = element_blank()))

# Remove y-axis labels from the second column
p.lambda[[5]] <- p.lambda[[5]] + theme(axis.title.y = element_blank()) 
p.mu_a[[5]] <- p.mu_a[[5]] + theme(axis.title.y = element_blank())
p.mu_l[[5]] <- p.mu_l[[5]] + theme(axis.title.y = element_blank())
p.beta[[5]] <- p.beta[[5]] + theme(axis.title.y = element_blank())


# Combine the subplots into a grid with two columns
p1 <- grid.arrange(
  grobs = list(
    # Row 1: lambda
    p.lambda[[2]],p.lambda[[5]],
    # Row 2: mu_a
    p.mu_a[[2]],p.mu_a[[5]],
    # Row 3: mu_l
    p.mu_l[[2]],p.mu_l[[5]],
    # Row 4: beta
    p.beta[[2]],p.beta[[5]]
  ),
  ncol = 2, # Two columns for species
  nrow = 4, # Four rows for traits
  layout_matrix = rbind(
    c(1, 2), # lambda row
    c(3, 4), # mu_a row
    c(5, 6), # mu_l row
    c(7, 8)  # beta row
  ),
  widths = c(1, 0.9),
  top = textGrob("A. duodenale                                             O. circumcincta       ",
                 gp = gpar(fontsize = 12, fontface = "italic"),
                 just = "center")
)

# Display the plot
p1

save_plot("Fig8_R1.pdf", plot=p1, base_height=6, base_width=6)
#save_plot("Fig8_R1.png", plot=p1, base_height=6, base_width=6, dpi=300)
