# Supplementary figures- qualtitative behavior grid plots

# read in csv file produced from mathematica to code distinct qualitative behaviors


parms.sub <- list(Ascaris=parmsAscaris,
                  Trichuris=parmsTrichuris,
                  TcolubAUS=parmsTcolubAUS,
                  OcircUK=parmsOcircUK)

#input results from matlab
treat1 = read.csv("50grid_treatment.csv")
treat = read.csv("50grid_treatment_res3.csv",header=F)
colnames(treat) = "outcome" #missing header
treat = cbind(treat1,treat)

host1 = read.csv("50grid_host_v2.csv")
host = read.csv("50grid_host_v2 1_res.csv",header=F)
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

spp.list <- c("Ascaris lumbricoides", "Trichuris trichiura","Trichostrongylus colubriformis AUS", "Ostertagia circumcincta UK") 

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
    labs(x="frequency", y= "coverage") +
    theme(legend.position="right") +
    labs(x=expression(paste(italic(mu[l]))), y= expression(paste(italic(mu[l])))) +
    #    ggtitle("Treatment grid") +
    scale_color_manual(values = c("#FFFF99","#66CC99","#9999CC","#9999CC"),labels = c("grow","rescue","decline","decline"),drop=FALSE) +theme(legend.position="none")
  #  print(p)
  
  pH[[i]] <- ggplot(data=host.spp[,c(6,7,13,14)], aes(x=beta_eff, y=N, color=as.factor(outcome))) +
    theme_bw() +
    geom_point() +
    labs(x=expression(paste(italic(beta))), y= "host density") +
    geom_point(aes_string(x = parms.sub[[i]]$beta_eff, y = parms.sub[[i]]$N), colour="black")+
    theme(legend.position="right") +
    #    ggtitle("Host grid") +
    scale_color_manual(values = c("#FFFF99","#66CC99","#9999CC","#9999CC"),labels = c("grow","rescue","decline","decline"),drop=FALSE) +theme(legend.position="none")
  #  print(pH)
  
  pHs[[i]] <- ggplot(data=hostsub.spp[,c(6,7,13,14)], aes(x=beta_eff, y=N, color=as.factor(outcome))) +
    theme_bw() +
    geom_point() +
    labs(x=expression(paste(italic(beta))), y= "host density") +
    geom_point(aes_string(x = parms.sub[[i]]$beta_eff, y = parms.sub[[i]]$N), colour="black")+
    theme(legend.position="right") +
    #    ggtitle("Host subset grid") +
    scale_color_manual(values = c("#FFFF99","#66CC99","#9999CC","#9999CC"),labels = c("grow","rescue","decline","decline"),drop=FALSE) +theme(legend.position="none")
  
  pP[[i]] <- ggplot(data=parasite.spp[,c(5,8,13,14)], aes(x=mu_l, y=log(lambda_eff), color=as.factor(outcome))) +
    theme_bw() +
    geom_point() +
    labs(x=expression(paste(italic(mu[l]))), y= expression(paste(ln(lambda)))) +
    geom_point(aes_string(x = parms.sub[[i]]$mu_l, y = log(parms.sub[[i]]$lambda_eff)), colour="black")+
    theme(legend.position="right") +
    #    ggtitle("Parasite grid") +
    scale_color_manual(values = c("#FFFF99","#66CC99","#9999CC","#9999CC"),labels = c("grow","rescue","decline","decline"),drop=FALSE)+theme(legend.position="none")
  #  print(pP)
  
  pPm[[i]] <- ggplot(data=parasitemu.spp[,c(4,8,13,14)], aes(x=mu_l, y=mu_a, color=as.factor(outcome))) +
    theme_bw() +
    geom_point() +
    labs(x=expression(paste(italic(mu[l]))), y= expression(paste(italic(mu[a])))) +
    geom_point(aes_string(x = parms.sub[[i]]$mu_l, y = parms.sub[[i]]$mu_a), colour="black")+
    theme(legend.position="right") +
    #    ggtitle("Parasite grid") +
    scale_color_manual(values = c("#FFFF99","#66CC99","#9999CC","#9999CC"),labels = c("grow","rescue","decline","decline"),drop=FALSE)+theme(legend.position="none")
  #  print(pPm)
  
}

pPanel = ggarrange(p[[1]], pH[[1]],pP[[1]],pPm[[1]], ncol = 1, nrow = 4)
#save_plot("SI_fig1.pdf", p1, base_height=5, base_width=10)

spp.list <- c("Ascaris lumbricoides", "Trichuris trichiura","Trichostrongylus colubriformis", "Ostertagia circumcincta") 
create_column <- function(index) {
  # Assuming each entry in p, pH, pP, pPm corresponds to a species in the same order
  pPanel <- ggarrange(p[[index]], pH[[index]], pP[[index]], pPm[[index]], ncol = 1)
  
  pAnnotated <- annotate_figure(pPanel, top = text_grob(spp.list[index], size = 10))
  return(pAnnotated)
}

annotated_columns <- lapply(1:4, create_column)

# Combine all the annotated columns into a single plot
final_plot <- do.call(ggarrange, c(annotated_columns, ncol=4))
final_plot

#save_plot("SI_fig1.pdf", final_plot, base_height=6, base_width=10)

