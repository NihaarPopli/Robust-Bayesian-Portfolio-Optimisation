require(ggplot2)
require(fGarch)
require(flexmix)


#http://stats.stackexchange.com/questions/7440/kl-divergence-between-two-univariate-gaussians
## KL example

KLnormalnormal <- function(m1,m2,s1,s2){
  if(length(m2)!=1 | length(s2)!=1){
    stop("Length m2 or s2 are not 1")
  }
  return(.5 * (log(s2^2/s1^2) - 1 + ((s1^2 + (m1-m2)^2)/s2^2 ) ) ) ### KL divergence manual
}

## build KLnormalnormal fucntion based on mathematical expression for KL divergence between two normal distributions

## 1 distributions that are used, mathematical KL divergence calculated to see if matches with function KLdiv
KLnormalnormal(m1=0.25,s1=1,m2=1,s2=1.5) #example one KL=0.5


## function that provides denstity for distribution 1, distribution 2 and the KL div between 1 and 2
KLskewednormal <- function(mu1,mu2,sigma1,sigma2,gamma1,gamma2,x){
  y1 <- dsnorm(x, mean=mu1, sd=sigma1,xi=gamma1, log=F) #density pi(1)
  y2 <- dsnorm(x, mean=mu2, sd=sigma2,xi=gamma2, log=F) # density pi(2)
  y3 <- (dsnorm(x, mean=mu1, sd=sigma1,xi=gamma1, log=TRUE) -
           dsnorm(x, mean=mu2, sd=sigma2,xi=gamma2, log=TRUE)) *
    dsnorm(x, mean=mu1, sd=sigma1,xi=gamma1) # kl divergence KL(pi(1)||pi(2))
  out <- cbind(y1,y2,y3)
  colnames(out) <- c("density pi-1","density pi-2","density kl(pi-1||pi2)")
  return(out)
}

png(file="figure2.png",width=800,height=400) #to store in png
par(mfrow=c(1,1))
par(mar = c(5, 4, 3, 0.2)) ## setting for legend function


## figure 1
x.axis <- seq(-4,4,by=.001) #get range for x-axis
out <- KLskewednormal(mu1=0.25,mu2=1,sigma1=1,sigma2=1.5,gamma1 = 1,gamma2 = 1,x=x.axis) 
KLdiv(out[,1:2],eps=1e-8)[1,2] # #.125 exact equal to what should be: 0.125 

plot(x.axis,out[,1],col="red",type="l",xlab=expression(theta),
     ylab="density",xlim=c(-4,4),ylim=c(-.15,.6),lwd=3, cex.lab = 1.5)
#polygon(x.axis,out[,1],col="red",density=10)
lines(x.axis,out[,2],col="blue",type="l",lwd=3)
#polygon(x.axis,out[,2],col="blue",density=10)
lines(x.axis,out[,3],col="darkgreen",lwd=3)
polygon(x.axis,out[,3],col="darkgreen",density=10)

## legend function from https://stackoverflow.com/questions/3932038/plot-a-legend-outside-of-the-plotting-area-in-base-graphics
add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}
add_legend("top", legend=c("P","Q",expression(paste("KL(P||Q)"))), lty=1, 
           col=c("red", "blue","darkgreen"),
           horiz=TRUE, bty='n', cex=1.7,lwd=3)

dev.off() # get png filled


