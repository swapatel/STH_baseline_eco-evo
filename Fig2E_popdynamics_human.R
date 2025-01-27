# Figure 2 Population Dynamics of main text

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

spp.names = c("Ascaris lumbricoides", "Hookworm", "Trichuris trichiura", "Trichostrongylus colubriformis", "Ostertagia circumcincta","Haemonchus contortus")

p <- list()
pW <- list()
ans.all <- c()

# run for each species
for(i in 1:length(parms.listH)){
  parms <- parms.list[[i]]
  species_stub <- spp.names[i]
  
  
n0<-c(W=100,L=3000, x=1, y=1) #set initial conditions
tf<-10*365
times<-0:tf
treat.times <- round(seq(500, tf, by=500))
#filename <- paste0(species_stub, freq, "x_0.3", "_EcoEvoMDA_TS.pdf")
ans <- solve_ode(n0, times, treat.times, parms)
ans = as.data.frame(ans)
ans$species = spp.names[i]
ans.all = rbind(ans.all,ans)
}

p1 <- ggplot(data=ans.all, aes(x=time, y=W, col=species)) + 
  theme_classic() + geom_line(aes(linetype=species), size=1) +
  ylab("Adult worms") + xlab("Time in days") + 
  scale_color_manual(values=c("#F8766D","#00BA38","#619CFF"), labels = c(
    expression(italic("A. lumbricoides")),
    expression(italic("A. duodenale")),
    expression(italic("T. trichiura"))
  )) +
  theme(
    axis.title = element_text(size = 20), # Size of axis titles
    axis.text = element_text(size = 18),  # Size of axis text
    legend.position="none"
    #legend.title = element_text(size = 14), # Size of legend title
    #legend.text = element_text(size = 14),  # Size of legend text
  )


p1
save_plot("Fig2E.pdf", p1, base_height=3, base_width=6)

#df <- as.data.frame(ans)

#p <- ggplot(data=df, aes(x=time, y=W)) + theme_classic() + geom_line(colour=colours[1], linewidth=1) +
 # xlab(xlabs[i]) + ylab(expression(paste("Helminth density"))) +
 # theme(axis.title=element_text(size=16), axis.text=element_text(size=14), plot.margin = margin(0,0,0,1, "cm"))
#print(p)

#parametric plot
#par(mfrow=c(1,1))
#plot(ans[,2], ans[,3], type="l")#, ylim=c(0,100000), xlim=c(0,366*9))
#points(ans[1,2], ans[1,3], col="red")

