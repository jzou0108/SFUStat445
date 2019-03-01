t11.2 <- read.table("Data/T11-2.DAT", header = F,
                    col.names = c("group", "gender", "freshwater", "marine"))
t11.2$group <- factor(t11.2$group, labels = c("Alaskan", "Canadian"))
t11.2$gender <- factor(t11.2$gender, labels = c("female", "male"))
dim(t11.2)
t11.2[1:10,]

zmu1 <- colMeans(t11.2[t11.2$group=="Alaskan", 3:4])
zmu1
zmu2 <- colMeans(t11.2[t11.2$group=="Canadian", 3:4])
zmu2
zs1 <- var(t11.2[t11.2$group=="Alaskan", 3:4])
zs1
zs2 <- var(t11.2[t11.2$group=="Canadian", 3:4])
zs2
n1 <- nrow(t11.2[t11.2$group=="Alaskan",])
n1
n2 <- nrow(t11.2[t11.2$group=="Canadian",])
n2
zs <- ((n1-1)*zs1 + (n2-1)*zs2)/(n1-1+n2-1)
zs
za <- solve(zs) %*% (zmu1 - zmu2)
za
t(za) %*% (zmu1 + zmu2)/2
plot(as.matrix(t11.2[,3:4]),xlab="",ylab="",type="n")
text(t11.2[,3:4],as.character(factor(t11.2$group,labels=c("A","C"))))
lines(t11.2[,3],(5.541+0.128*t11.2[,3])/.052,lty=2)
zres <- (as.matrix(t11.2[, 3:4]) %*% za) >= (t(za) %*% (zmu1 + zmu2)/2)[1, 1]
table(zres, t11.2$group)

#To class a new Salmon with the first-year freshwater growth of 100in 
#and the first-year marine growth of 400in
c(100, 400) %*% za > 5.541



library(MASS)
z <- lda(group ~ freshwater + marine, data = t11.2)
z
#(The coefficents from lda are scaled by the square root of $a'S_{pooled}a$.)
t(za)%*%zs%*%za 
as.vector(za)/sqrt(t(za)%*%zs%*%za)
z$prior
z$counts
z$means
z$scaling
#To class a new Salmon with the first-year freshwater growth of 100in 
#and the first-year marine growth of 400in
predict(z, newdata = data.frame(freshwater = 100, marine = 400))

#If we do not make the equal covariance assumption,
#we can obtain the quadratic discrimination function
z <- qda(group ~ freshwater + marine, data = t11.2)
z
z$scaling










