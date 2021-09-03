setwd("C:\\Users\\Nihaar\\Desktop\\RobustBayesian\\Code")
library(Rtools)
library(rstan)
library(bayesplot)
library(mvtnorm)
library(quantmod)
library(matlab)

#We loop through N or keep it fixed
N=2

#true market parameters
sigma = -0.8*(ones(N)-diag(N))+diag(N)
mean = c(2.5*sigma%*%rep(1,N)/N)

#returns time series y: simulations (chapter 4) or use uploaded finanical dataset (chapter 5)
y=rmvnorm(n=1000, mean = mean, sigma=sigma) #ch4
y=read.csv("all_stocks_5yr.csv")

#prior parameters (mu)
sigma0 = diag(diag(sigma))
mu0 = c(0.5*sigma0%*%rep(1,N)/N)

#RStan model inputs
dat <- list(S=dim(y)[1], N = N, mu0=mu0, sigma=sigma, sigma0=sigma0, y=y)

#compile model
stan_model <- rstan::stan_model("MFvsFR.stan")


#Variational Bayes using mean-field algorithm
stan_vb_mf <- rstan::vb(object = stan_model, data = dat, seed = 92,
                        output_samples = 10000, elbo_samples=80, eval_elbo=100, algorithm="meanfield")

mf_mu1_1 <- stan_vb_mf@sim$samples[[1]]$mu.1
mf_mu1_2 <-stan_vb_mf@sim$samples[[1]]$mu.2

#Variational Bayes using full-rank algorithm
stan_vb_fr <- rstan::vb(object = stan_model, data = dat, seed = 1102,
                        output_samples = 10000, elbo_samples=10, eval_elbo=1000, algorithm="fullrank")

fr_mu1_1 <- stan_vb_fr@sim$samples[[1]]$mu.1
fr_mu1_2 <-stan_vb_fr@sim$samples[[1]]$mu.2


#MCMC
stan_MCMC <- rstan::stan("MFvsFR.stan", data = dat, chains=1, iter=10000)

#TracePlot
pdf("TP_mu1.pdf",width = 6, height = 4) 
mcmc_trace(stan_MCMC, pars = c("mu[1]"),  facet_args = list(strip.position = "left"))
dev.off()

#ACF plot
pdf("ACF_mu1.pdf",width = 6, height = 4) 
mcmc_acf(stan_MCMC, pars = c("mu[1]"), facet_args = list(strip.position = "left") )
dev.off()


#Samples from analytical posterior distribution (conjugacy)
mu1 <- solve((solve(sigma0)+1000*solve(sigma)))%*%(solve(sigma0)%*%mu0+1000*solve(sigma)%*%c(mean(y[,1]), mean(y[,2])))
sigma1 <- solve((solve(sigma0)+1000*solve(sigma)))
pos_samples <- rmvnorm(n=10000, mean = mu1, sigma=sigma1)
con_mu1_1 <- pos_samples[,1]
con_mu1_2 <- pos_samples[,2]


#Collate results from the three approaches
results <- data.frame(mf_mu1_1, mf_mu1_2, fr_mu1_1, fr_mu1_2, con_mu1_1, con_mu1_2)


#Plotting
pdf("contours.pdf",width = 15, height = 6 ) 
par(mfrow=c(3,1),mar=c(5,6,4,1)+.1)
a <- ggplot(data=results, aes(x=mf_mu1_1, y=mf_mu1_2)) + 
  geom_density_2d(aes(x=mf_mu1_1, y=mf_mu1_2),colour="blue", size=0.9) +
  xlim(0.15, 0.35) +
  ylim(0.15,0.35) +
  xlab("mu_1") + 
  ylab("mu_2") + 
  theme_light() 

b <- ggplot(data=results, aes(x=fr_mu1_1, y=fr_mu1_2)) +
  geom_density_2d(aes(x=fr_mu1_1, y=fr_mu1_2),colour="green", size=0.9) +
  xlim(0.15, 0.35) +
  ylim(0.15,0.35) +
  xlab("mu_1") + 
  ylab("mu_2") +
  theme_light() 
  
c <- ggplot(data=results, aes(x=con_mu1_1, y=con_mu1_2)) +
  geom_density_2d(aes(x=con_mu1_1, y=con_mu1_2),colour="red", size=0.9) +
  xlim(0.15, 0.35) +
  ylim(0.15,0.35) +
  xlab("mu_1") + 
  ylab("mu_2") + 
  theme_light()  

grid.arrange(a, b, c, ncol=3)
# Close the pdf file
dev.off() 



pdf("scatter.pdf",width = 15, height = 6 ) 
par(mfrow=c(1,2),mar=c(5,6,4,1)+.1)
ggplot(data=results, aes(x=mf_mu1_1, y=mf_mu1_2, colour="mean-field ADVI")) + 
  geom_point(size =0.8) + 
  geom_point(aes(x=fr_mu1_1, y=fr_mu1_2, colour="full-rank ADVI"), size =0.8) + 
  geom_point(aes(x=con_mu1_1, y=con_mu1_2, colour="analytical"), size =0.8)+
  xlab("mu_1") + 
  ylab("mu_2") + 
  labs(colour = "Approach") + 
  theme_light() 
# Close the pdf file
dev.off() 



