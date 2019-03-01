
library(mvtnorm)
xvals<-seq(-6,6,by=0.2)        # generate a sequence of values that will be used for values of  x1 and x2
nvals<-length(xvals)           # the length of the sequence xvals
x1<-rep(xvals, each=nvals)     # repeat each element of xvals for nvals times
x2<-rep(xvals, nvals)            # repeat  xvals for nvals times
x12 <- cbind(x1, x2)          # column combination to get pairs of x1 and x2
mu1 <- c(-1.5,-1.5)
mu2 <- c(1.5,1.5)
Sigma <- rbind(c(1, 0), c(0, 1))
p1 <- 0.2
p2 <- 1-p1
prob.z <- matrix(p1*dmvnorm(x12, mean = mu1,sigma = Sigma) + 
                   p2*dmvnorm(x12, mean = mu2, sigma = Sigma), ncol = nvals, byrow = T)
persp(x=xvals, y=xvals, xlab="x1", ylab="x2", prob.z, zlab="", theta = 30, 
      phi = 30, r = 20, expand = 1.0, col="lightblue",ticktype = "detailed", lwd=2, cex.axis=1.2, cex.lab=1.2, cex.main=1.5, cex=1.5,main="Perspective Plot")

a = solve(Sigma)%*%(mu1-mu2)
a

b = t(a)%*%(mu1+mu2)/2
b

# draw the probability density contour plots
contour(x=xvals, y=xvals, prob.z, xlab="x1", ylab="x2",  lwd=2, cex.axis=1.2, 
        cex.lab=1.2, cex.main=1.5, cex=1.5, main="Probability Density Contour", nlevels = 6)
text(-5.5,3.75,expression(Sigma))
text(-5,3.75,"=")
text(-4.5,3.75, "[",cex=2.5)
text(-4,4, Sigma[1,1])
text(-3,4, Sigma[1,2])
text(-4,3.5, Sigma[2,1])
text(-3,3.5, Sigma[2,2])
text(-2.5,3.75, "]",cex=2.5)
abline(b/a[1],-a[2]/a[1])

b.prior = t(a)%*%(mu1+mu2)/2+log(p2)-log(p1)
b.prior
abline(b.prior/a[1],-a[2]/a[1], col=2, lwd=2)

c12 <- 10
c21 <- 1

b.prior.cost = t(a)%*%(mu1+mu2)/2+log(p2)-log(p1) + log(c12)-log(c21)
b.prior.cost
abline(b.prior.cost/a[1],-a[2]/a[1], col=3, lwd=2)


mu1 <- c(-1.5, -1.5)
mu2 <- c(1, 1)
Sigma1 <- rbind(c(.5, 0), c(0, .5))
Sigma2 <- rbind(c(2, 0), c(0, 2))
p1 <- 0.5
p2 <- 1-p1
prob.z <- matrix(p1*dmvnorm(x12, mean = mu1,sigma = Sigma1) + 
                   p2*dmvnorm(x12, mean = mu2, sigma = Sigma2), ncol = nvals, byrow = T)
persp(x=xvals, y=xvals, xlab="x1", ylab="x2", prob.z, zlab="", theta = 30, 
      phi = 30, r = 20, expand = 1.0, col="lightblue",ticktype = "detailed", lwd=2, cex.axis=1.2, cex.lab=1.2, cex.main=1.5, cex=1.5,main="Perspective Plot")

f <- function(x){
  log(det(Sigma1)/det(Sigma2))+t(x-mu1)%*%solve(Sigma1)%*%(x-mu1) - t(x-mu2)%*%solve(Sigma2)%*%(x-mu2)
}

par(mfrow=c(1,1))
contour(x=xvals, y=xvals, prob.z, xlab="x1", ylab="x2",  lwd=2, cex.axis=1.2, 
        cex.lab=1.2, cex.main=1.5, cex=1.5, main="Probability Density Contour", nlevels = 30)
rr <- matrix(apply(x12,1,f),ncol=nvals,byrow=T)
contour(x=xvals, y=xvals, rr,  lwd=2, add=T,col="red", levels = 0)



