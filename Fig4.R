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

setwd("../Grids")
#input results from matlab
treat1 = read.csv("50grid_treatment.csv")
treat = read.csv("50grid_treatment_res3.csv",header=F)
colnames(treat) = "outcome" #missing header
treat = cbind(treat1,treat)

host1 = read.csv("50grid_host_v2.csv")
host = read.csv("50grid_host_v2_1_res.csv",header=F)
colnames(host) = "outcome" #missing header
host = cbind(host1,host)

hostsub1 = read.csv("50grid_hostsubset.csv")
hostsub = read.csv("50grid_hostsubset_res3.csv",header=F)
colnames(hostsub) = "outcome" #missing header
hostsub = cbind(hostsub1,hostsub)

parasite1 = read.csv("50grid_parasite.csv")
parasite = read.csv("50grid_parasite_res.csv",header=F)
colnames(parasite) = "outcome" #missing header
parasite = cbind(parasite1,parasite)

parasitemu1 = read.csv("50grid_parasite_mul.csv")
parasitemu = read.csv("50grid_parasite_mul_res.csv",header=F)
colnames(parasitemu) = "outcome" #missing header
parasitemu = cbind(parasitemu1,parasitemu)

spp.list <- c("Ascaris lumbricoides", "Hookworm", "Trichuris trichiura","Trichostrongylus colubriformis AUS", "Ostertagia circumcincta UK","Haemonchuns contortus AUS") 

#for each species
p <- list()
pH <- list()
pHs <- list()
pP <- list()
pPm <- list()

for(i in 1:length(spp.list)) { #length(spp.list)
  species_stub <- spp.list[i]
  treat.spp = treat[which(treat$species==species_stub),]
  host.spp = host[which(host$species==species_stub),]
  hostsub.spp = hostsub[which(hostsub$species==species_stub),]
  parasite.spp = parasite[which(parasite$species==species_stub),]
  parasitemu.spp = parasitemu[which(parasitemu$species==species_stub),]
  
  p[[i]] <- ggplot(data=treat.spp[,c(2,3,13,14)], aes(x=freq, y=p, color=as.factor(outcome))) +
    theme_bw() +
    geom_point() +
    labs(x="Treatment frequency", y= "Coverage") +
    theme(legend.position="right") +
#    ggtitle("Treatment grid") +
    scale_color_manual(values = c("#FFFF99","#66CC99","#9999CC","#9999CC"),labels = c("grow","rescue","decline","decline"),drop=FALSE) +theme(legend.position="none")
#  print(p)
  
  pH[[i]] <- ggplot(data=host.spp[,c(6,7,13,14)], aes(x=beta_eff, y=N, color=as.factor(outcome))) +
    theme_bw() +
    geom_point() +
    labs(x=expression(paste(italic(beta))), y= "Host density") +
    geom_point(aes_string(x = parms.list[[i]]$beta_eff, y = parms.list[[i]]$N), colour="black")+
    theme(legend.position="right") +
#    ggtitle("Host grid") +
    scale_color_manual(values = c("#FFFF99","#66CC99","#9999CC","#9999CC"),labels = c("grow","rescue","decline","decline"),drop=FALSE) +theme(legend.position="none")
#  print(pH)

  pHs[[i]] <- ggplot(data=hostsub.spp[,c(6,7,13,14)], aes(x=beta_eff, y=N, color=as.factor(outcome))) +
    theme_bw() +
    geom_point() +
    labs(x=expression(paste(italic(beta))), y= "Host density") +
    geom_point(aes_string(x = parms.list[[i]]$beta_eff, y = parms.list[[i]]$N), colour="black")+
    theme(legend.position="right") +
#    ggtitle("Host subset grid") +
    scale_color_manual(values = c("#FFFF99","#66CC99","#9999CC","#9999CC"),labels = c("grow","rescue","decline","decline"),drop=FALSE) +theme(legend.position="none")

  pP[[i]] <- ggplot(data=parasite.spp[,c(5,8,13,14)], aes(x=mu_l, y=log(lambda_eff), color=as.factor(outcome))) +
    theme_bw() +
    geom_point() +
    labs(x=expression(paste(italic(mu[l]))), y= expression(paste(ln(lambda)))) +
    geom_point(aes_string(x = parms.list[[i]]$mu_l, y = log(parms.list[[i]]$lambda_eff)), colour="black")+
    theme(legend.position="right") +
#    ggtitle("Parasite grid") +
    scale_color_manual(values = c("#FFFF99","#66CC99","#9999CC","#9999CC"),labels = c("grow","rescue","decline","decline"),drop=FALSE)+theme(legend.position="none")
#  print(pP)

  pPm[[i]] <- ggplot(data=parasitemu.spp[,c(4,8,13,14)], aes(x=mu_l, y=mu_a, color=as.factor(outcome))) +
    theme_bw() +
    geom_point() +
    labs(x=expression(paste(italic(mu[l]))), y= expression(paste(italic(mu[a])))) +
    geom_point(aes_string(x = parms.list[[i]]$mu_l, y = parms.list[[i]]$mu_a), colour="black")+
    theme(legend.position="right") +
#    ggtitle("Parasite grid") +
    scale_color_manual(values = c("#FFFF99","#66CC99","#9999CC","#9999CC"),labels = c("grow","rescue","decline","decline"),drop=FALSE)+theme(legend.position="none")
#  print(pPm)
  
}

setwd("../Code_Jan22_2025")
p4A = ggarrange(p[[2]], pH[[2]],pP[[2]],ncol = 1, nrow = 3) #pPm[[2]],
#save_plot("fig4A_grids.pdf", p4A, base_height=5, base_width=2.5)
p4A
p4B = ggarrange(p[[6]], pH[[6]],pP[[6]],ncol = 1, nrow = 3) #pPm[[6]], 
#save_plot("fig4B_grids.pdf", p4B, base_height=5, base_width=2.5)
p_combined <- grid.arrange(
  p4A, p4B,
  ncol = 2,
  nrow = 1,
  widths = c(1, 1), # Equal widths for left and right columns
  top = textGrob("Human-infecting: A. duodenale                        Livestock-infecting: H. contortus       ", 
                 gp = gpar(fontsize = 12))
)

p_combined
ggsave("Fig4.svg", plot = p_combined, width = 7, height = 7, units = "in")

#subset plot
sub = pHs[[2]]+ theme(axis.title.x = element_blank(), axis.title.y = element_blank())
ggsave("Fig4_sub.png", plot = sub, width = 3, height = 2, units = "in")
