library(mvtnorm)
# R may be used to nd out the eigenvalues and (orthonormal) eigenvectors:
eigen(rbind(c(1, .75), c(.75, 1)))

# The density surface of a bivariate normal distribution with rho = 0 and equal variance:
zxy <- cbind(x=rep((-30:30)/5, rep(61, 61)), y = rep((-30:30)/5, 61))
zdata <- matrix(dmvnorm(zxy, mean = c(0, 0), sigma = rbind(c(1, 0), c(0, 1))), ncol = 61, byrow = T)
persp((-30:30)/5, (-30:30)/5, zdata, theta = 30, phi = 30, r = 20, expand = .5)
contour((-30:30)/5, (-30:30)/5, zdata)

#The density surface of a bivariate normal distribution with rho = 0:75 and equal variance:
zxy <- cbind(x=rep((-30:30)/10, rep(61, 61)), y = rep((-30:30)/10, 61))
zdata <- matrix(dmvnorm(zxy, mean = c(0, 0), sigma = rbind(c(1, 0.75), c(0.75, 1))), ncol = 61, byrow = T)
persp((-30:30)/10, (-30:30)/10, zdata, theta = 30, phi = 30, r = 20, expand = .5)


#Example 2.14
B <- eigen(rbind(c(4, 1, 2), c(1, 9, -3), c(2, -3, 25)))
SRSigma <- B$vectors%*%diag(sqrt(B$values))%*%t(B$vectors)

#Exercise 4.2
A <- eigen(rbind(c(2, 1/sqrt(2)), c(1/sqrt(2), 1)))
# major axis
sqrt(A$values[1]*1.39)
A$vectors[,1]
# minor axis
sqrt(A$values[2]*1.39)
A$vectors[,2]
# The density surface 
zxy <- cbind(x=rep((-30:30)/10, rep(61, 61)), y = rep((-10:50)/10, 61))
zdata <- matrix(dmvnorm(zxy, mean = c(0, 2), sigma = rbind(c(2, 1/sqrt(2)), c(1/sqrt(2), 1))), ncol = 61, byrow = T)
persp((-30:30)/10, (-10:50)/10, zdata, theta = 30, phi = 30, r = 20, expand = .5)



