# Figure 7 Evolutionary trajectories depending on species

# load parameter_for_species file
source("parameters_for_species.R")
# load model.R file 
source("model.R")

p <- list()
pW <- list()
ans.all <- c()

spp.names = c("Ascaris lumbricoides", "Hookworm", "Trichuris trichiura", "Trichostrongylus colubriformis", "Ostertagia circumcincta","Haemonchus contortus")

# run for each species
for(i in 1:length(parms.list)){
  x0 <- 0.1
  y0 <- 0
  parms <- parms.list[[i]]
  species_stub <- spp.names[i]
  
  # run full model
  n0<-c(W=10000*parms$WLratio, L = 10000, x=x0, y=y0) #set initial conditions
  tf<-5*365
  freq <- 1.5
  times<-0:tf
  treat.times <- round(seq(365/freq, tf, by=365/freq))
  ans <- solve_ode(n0, times, treat.times, parms)
  ans = as.data.frame(ans)
  ans$species = spp.names[i]
  ans.all = rbind(ans.all,ans)
  
}

# graph

p1 <- ggplot(data=ans.all, aes(x=time, y=x, col=species)) + 
  theme_classic() + geom_line(position = position_dodge(width = 10)) +
  ylab("frequency") + ylab("time in days") + ylim(0, 1) +
  scale_color_manual(values=c("#F8766D", "#B79F00", "#00BA38", "#00BFC4", "#F564E3","#619CFF"))

p1
save_plot("Fig7_6spp_timeseries.pdf", p1, base_height=5, base_width=7)
