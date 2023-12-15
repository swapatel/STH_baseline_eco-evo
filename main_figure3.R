# Figure 3 of main text

library(ggplot2)
library(ggpubr)
library(viridis)
library(deSolve)
library(gridExtra)
library(cowplot)

# load parameter_for_species file
source("~/parameters_for_species.R")
# load model.R file 
source("~/model.R")


# example for Hookworm
parms <- parmsHookworm

freqs <- c(0.5, 1, 12)
ll <- list()
for(i in 1:length(freqs)){
  freq <- freqs[i]
  species_stub <- parms$species
  print(species_stub)
  
  # Eco-Evo Case - w/ MDA
  n0<-c(W=1, L=100, x=0.01, y=0) #set initial conditions
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
xlabs <- c("", "", "time in days")
for(i in 1:length(ll)){
  p <- ggplot(data=ll[[i]], aes(x=time, y=W)) + theme_classic() + geom_line(colour=colours[i], linewidth=1) +
    xlab(xlabs[i]) + ylab(expression(paste("helminth density"))) +
    theme(axis.title=element_text(size=14), axis.text=element_text(size=12), plot.margin = margin(0,0,0,1, "cm"))
  print(p)
  ps[[i]] <- p
}

pic <- plot_grid(ps[[1]], ps[[2]], ps[[3]], labels=c("a", "b", "c"), ncol=1)
#save_plot("HW_TS_example.pdf", plot=pic, base_height=9, base_width=12)
pic 

# Make panel plot of timelines and critical time between treatments depending on mu_BB
# critT line for Hookworm

#restrict to muBB=0.5
# the following vector is from Mathematica notebook, numerically solving for the critical times
y <- c(41.3275, 62.4616, 83.9212, 105.717, 127.858, 150.358, 173.227, 196.478, 220.124, 244.179, 268.657, 293.572, 318.942, 344.782, 371.111, 397.948, 425.312, 453.224)
x <- seq(0.1,0.95,0.05)

df <- data.frame(x=x, y=y)

q <- ggplot(data=df, aes(x=x, y=y)) + theme_classic() + geom_line(linewidth=1) +
  xlab("efficacy") + ylab("time between treatments (days)") +
  geom_segment(aes(x=0.9, y=30.41667, xend=0.1, yend=30.41667), colour="#9999CC",linewidth=1, arrow = arrow(length = unit(0.75, "cm"))) +
  geom_segment(aes(x=0.9, y=365, xend=0.1, yend=365), colour="#66CC99",linewidth=1, arrow = arrow(length = unit(0.75, "cm"))) +
  geom_segment(aes(x=0.9, y=730, xend=0.1, yend=730), colour="#EEEE33",linewidth=1, arrow = arrow(length = unit(0.75, "cm"))) +
  theme(axis.title=element_text(size=16), axis.text=element_text(size=14))

pic = pic + theme(plot.margin = margin(5.5, 5.5, 5.5, 5.5, "mm")) 
pic2 <- plot_grid(pic, q, labels=c("", "d"), ncol=2)
pic2
save_plot("Fig3_panel_timeseries_tcrit.pdf", plot=pic2, base_height=6, base_width=10)
