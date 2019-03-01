#=================================================#
#                   STAT 445/645                  #
#             Bivariate Normal Distribution       #
#        Probability Density Perspective Plots    #
#           and Contour Plots and Eigenanalysis   #
#=================================================#

#=================================================
# Compute eigenvalues and eigenvectors of
# numeric  matrices
#=================================================
A<-cbind(c(1, .75), c(.75, 1)) # combine two columns to form a matrix A
re<-eigen(A)   # compute eigenvalues and eigenvectors of A
re$values      # get the eigenvalues
re$vectors     # get the eigenvectors (column vectors)
t(re$vectors[,1])%*%re$vectors[,1] # this is equal to 1 (normalization)
t(re$vectors[,2])%*%re$vectors[,2] # this is equal to 1 (normalization)
t(re$vectors[,1])%*%re$vectors[,2] # this is equal to 0 (orthogonality)

#=================================================
# Probability Density Perspective Plots 
# and Contour Plots of
# Bivariate Normal Distribution       
#=================================================


# Parameter setting for our illustrative 
# bivariate normal distribution 
# Note: we will use the mean vector  (0,0)'
#----------------------------------------------
rho<-0.6    # correlation coefficient between x1 and x2
sigma1<-1   # standard deviation for x1
sigma2<-1   # standard deviation for x2
sigma11<-sigma1^2 # variance of x1
sigma22<-sigma2^2 # variance of x2
Sigma<-rbind(c(sigma11, rho*sigma1*sigma2), c(rho*sigma1*sigma2, sigma22)) # variance-covariance for (x1,x2)'

# Compute the probability density values for
# many pairs of x1 and x2, which will be used 
# in the probability density perspective plots
# and contour plots
#----------------------------------------------
xvals<-seq(-6,6,by=0.2)        # generate a sequence of values that will be used for values of  x1 and x2
nvals<-length(xvals)           # the length of the sequence xvals
x1<-rep(xvals, each=nvals)     # repeat each element of xvals for nvals times
x2<-rep(xvals, nvals)            # repeat  xvals for nvals times
x12 <- cbind(x1, x2)          # column combination to get pairs of x1 and x2
prob.z <- matrix(dmvnorm(x12, mean = c(0, 0), 
                         sigma = Sigma), ncol = nvals, byrow = T) # probability density for pairs of x1 and x2
# draw the probability density perspective plot
# you can modify the values of theta, phi, and r to change the way how you look at this
# 3-D plot

persp(x=xvals, y=xvals, xlab="x1", ylab="x2", prob.z, zlab="", theta = 30, 
     phi = 30, r = 20, expand = 1.0, col="lightblue",ticktype = "detailed", lwd=2, cex.axis=1.2, cex.lab=1.2, cex.main=1.5, cex=1.5,main="Perspective Plot")

# draw the probability density contour plots
contour(x=xvals, y=xvals, prob.z, xlab="x1", ylab="x2",  lwd=2, cex.axis=1.2, cex.lab=1.2, cex.main=1.5, cex=1.5, main="Probability Density Contour", nlevels = 6)
text(-5.5,3.75,expression(Sigma))
text(-5,3.75,"=")
text(-4.5,3.75, "[",cex=2.5)
text(-4,4, Sigma[1,1])
text(-3,4, Sigma[1,2])
text(-4,3.5, Sigma[2,1])
text(-3,3.5, Sigma[2,2])
text(-2.5,3.75, "]",cex=2.5)

# Add the directions  of the ellipses axes on the contour plots. 
reEigen<-eigen(Sigma)
c<-2.447747 # sqrt(qchisq(0.95,2))
arrows(0,0,reEigen$vectors[1,1]*c*sqrt(reEigen$values[1]),reEigen$vectors[2,1]*c*sqrt(reEigen$values[1]), lwd=2)
arrows(0,0,reEigen$vectors[1,2]*c*sqrt(reEigen$values[2]),reEigen$vectors[2,2]*c*sqrt(reEigen$values[2]), lwd=2, lty=2, col=2)






