# Figure 3 of main text

library(ggplot2)
library(ggpubr)
library(viridis)
library(deSolve)
library(gridExtra)
library(cowplot)

# load parameter_for_species file
source("parameters_for_species.R")
# load model.R file 
source("model.R")


# example for Hookworm
parms <- list(
  species = "Hookworm",
  beta_eff = 0.35/365, 		# host contact with environment
  mu_h =  0.0167/365, 		# 1/60/365, # host death rate; 
  lambda_eff = 1.095e6/365, 	# number of eggs produced per fertilized adult female
  mu_l = 30/365, 			# larvae death rate
  N = 0.000123, 			# total host population size
  mu_a = 0.5/365, 		# natural death rate of adult worms in hosts
  mu_AA = 0.1, 			# drug induced death probability for homozygote resistant
  mu_AB = 0.5, 			# heterozygote
  mu_BB = 0.9, 			# susceptible homozygote
  cov = 0.3, #coverage
  WLratio = 5.5*10^-5 
)

freqs <- c(0.5, 1, 12)
ll <- list()
for(i in 1:length(freqs)){
  freq <- freqs[i]
  species_stub <- parms$species
  print(species_stub)
  
  # Eco-Evo Case - w/ MDA
  n0<-c(W=100, L=1000, x=0.01, y=0) #set initial conditions
  tf<-32*365
  times<-0:tf
  treat.times <- round(seq(365/freq, tf, by=365/freq))
  filename <- paste0(species_stub, freq, "x_0.3", "_EcoEvoMDA_TS.pdf")
  ans <- solve_ode(n0, times, treat.times, parms)
  df <- as.data.frame(ans)
  ll[[i]] <- df
}

ps <- list()
colours <- c("#EEEE33","#66CC99","#9999CC")
xlabs <- c("", "", "Time in days")
for(i in 1:length(ll)){
  p <- ggplot(data=ll[[i]], aes(x=time, y=W)) + theme_classic() + geom_line(colour=colours[i], linewidth=1) +
    xlab(xlabs[i]) + ylab(expression(paste("Adult worms"))) +
    theme(axis.title=element_text(size=16), axis.text=element_text(size=14), plot.margin = margin(0,0,0,1, "cm"))
  print(p)
  ps[[i]] <- p
}

pic <- plot_grid(ps[[1]], ps[[2]], ps[[3]], labels=c("A", "B", "C"), ncol=1)
#save_plot("HW_TS_example.pdf", plot=pic, base_height=9, base_width=12)
pic 

# Make panel plot of timelines and critical time between treatments depending on mu_BB
# critT line for Hookworm

#restrict to muBB=0.5
# the following vector is from Mathematica notebook, numerically solving for the critical times
y <- c(41.3275, 62.4616, 83.9212, 105.717, 127.858, 150.358, 173.227, 196.478, 220.124, 244.179, 268.657, 293.572, 318.942, 344.782, 371.111, 397.948, 425.312, 453.224)
x <- seq(0.1,0.95,0.05)

df <- data.frame(x=x, y=y)

q <- ggplot(data=df, aes(x=x, y=y)) + theme_classic() + geom_line(size=1) +
  xlab("Drug efficacy") + ylab("Days between treatments") +
  geom_segment(aes(x=0.9, y=30.41667, xend=0.1, yend=30.41667), colour="#9999CC",size=1.5, arrow = arrow(length = unit(0.75, "cm"))) +
  geom_segment(aes(x=0.9, y=365, xend=0.1, yend=365), colour="#66CC99",size=1.5, arrow = arrow(length = unit(0.75, "cm"))) +
  geom_segment(aes(x=0.9, y=730, xend=0.1, yend=730), colour="#EEEE33",size=1.5, arrow = arrow(length = unit(0.75, "cm"))) +
  theme(axis.title=element_text(size=16), axis.text=element_text(size=14))

pic = pic + theme(plot.margin = margin(5.5, 5.5, 5.5, 5.5, "mm")) 
pic2 <- plot_grid(pic, q, labels=c("", "D"), ncol=2)
pic2
save_plot("Fig3.pdf", plot=pic2, base_height=6, base_width=10)
