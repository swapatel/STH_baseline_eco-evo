# Parameters for different species of helminths
#for human helminths
parmsAscaris<-list(
  species = "Ascaris lumbricoides",
  beta_eff = 0.04/365, 		# host contact with environment # use 0.163/365 for R0=12
  mu_h =  0.0167/365, 		# 1/60/365, # host death rate; 
  lambda_eff = 3.65e6/365, 	# number of eggs produced per fertilized adult female
  mu_l = 6/365, 			# larvae death rate
  N = 0.000123, 			# total host population size 0.000123
  mu_a = 1/365, 			# natural death rate of adult worms in hosts
  mu_AA = 0.1, 			# drug induced death probability for homozygote resistant
  mu_AB = 0.5, 			# heterozygote
  mu_BB = 0.9, 			# susceptible homozygote
  cov = 1.0, #coverage
  WLratio = 3.5*10^-6 
)

parmsHookworm<-list(
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
  cov = 1.0, #coverage
  WLratio = 5.5*10^-5 
)

parmsTrichuris<-list(
  species = "Trichuris trichiura",
  beta_eff = 0.62/365, 		# host contact with environment
  mu_h =  0.0167/365, 		# 1/60/365, # host death rate; 
  lambda_eff = 7.3e5/365, 	# number of eggs produced per fertilized adult female
  mu_l = 18.25/365, 		# larvae death rate
  N = 0.000123, 			# total host population size
  mu_a = 1/365, 			# natural death rate of adult worms in hosts
  mu_AA = 0.1, 			# drug induced death probability for homozygote resistant
  mu_AB = 0.5, 			# heterozygote
  mu_BB = 0.9, 			# susceptible homozygote
  cov = 1.0, #coverage
  WLratio = 5.1*10^-5 
)

#for sheep
parmsTcolubAUS<-list(
  species = "Trichostrongylus colubriformis",
  beta_eff = 1124.2/365, 	# host contact with environment
  mu_h =  0.1/365, 		# 1/60/365, # host death rate; 
  lambda_eff = 2463.75/365, 	# number of eggs produced per fertilized adult female
  mu_l = 28.4/365, 		# larvae death rate
  N = 0.001, 			# total host population size
  mu_a = 17.89/365, 		# natural death rate of adult worms in hosts
  mu_AA = 0.1, 			# drug induced death probability for homozygote resistant
  mu_AB = 0.5, 			# heterozygote
  mu_BB = 0.9, 			# susceptible homozygote
  cov = 1.0, #coverage
  WLratio = 0.035 
)

parmsOcircUK<-list(
  species = "Ostertagia circumcincta",
  beta_eff = 956.3/365, # host contact with environment
  mu_h =  0.1/365, # 1/60/365, # host death rate; 
  lambda_eff = 1752/365, # number of eggs produced per fertilized adult female
  mu_l = 11.12/365, # larvae death rate
  N = 0.002, # total host population size
  mu_a = 14.9/365, #natural death rate of adult worms in hosts
  mu_AA = 0.1, # drug induced death probability for homozygote resistant
  mu_AB = 0.5, # heterozygote
  mu_BB = 0.9, # susceptible homozygote
  cov = 1.0, #coverage
  WLratio = 0.045 
)

parmsHcontAUS<-list(
  species = "Haemonchuns contortus",
  beta_eff = 890.6/365, # host contact with environment
  mu_h =  0.1/365, # 1/60/365, # host death rate; 
  lambda_eff = 5501.8/365, # number of eggs produced per fertilized adult female
  mu_l = 230.36/365, # larvae death rate
  N = 0.001, # total host population size
  mu_a = 3.65/365, #natural death rate of adult worms in hosts
  mu_AA = 0.1, # drug induced death probability for homozygote resistant
  mu_AB = 0.5, # heterozygote
  mu_BB = 0.9, # susceptible homozygote
  cov = 1.0, #coverage
  WLratio = 0.086
)

parms.list <- list(Ascaris=parmsAscaris,
                   Hookworm=parmsHookworm,
                   Trichuris=parmsTrichuris,
                   TcolubAUS=parmsTcolubAUS,
                   OcircUK=parmsOcircUK,
                   HcontAUS=parmsHcontAUS)

parms.listH <- list(Ascaris=parmsAscaris,
                   Hookworm=parmsHookworm,
                   Trichuris=parmsTrichuris)

parms.listL <- list(TcolubAUS=parmsTcolubAUS,
                   OcircUK=parmsOcircUK,
                   HcontAUS=parmsHcontAUS)