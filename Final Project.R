library(MASS)
library(glmnet)

#loading and cleaning data
credit = read.csv("Zou_Youneng_301126580_Data.csv", stringsAsFactors = FALSE)


credit = credit[, -c(1)]
#credit = credit[, -c(1,10)]
credit = na.omit(credit)

credit$Job[credit$Job == "0"] = "unskilled and non-resident"
credit$Job[credit$Job == "1"] = "unskilled and resident"
credit$Job[credit$Job == "2"] = "skilled"
credit$Job[credit$Job == "3"] = "highly skilled"

credit$Sex = factor(credit$Sex)
credit$Job = factor(credit$Job)
credit$Housing = factor(credit$Housing)
credit$Saving.accounts = factor(credit$Saving.accounts)
credit$Checking.account = factor(credit$Checking.account)
credit$Purpose = factor(credit$Purpose)
credit$Risk = factor(credit$Risk)




#variable selection
set.seed(123)
set = ifelse(runif(nrow(credit))>0.8, 0, 1)
trainset = credit[which(set == 1),]
testset = credit[which(set == 0),]

#build model
full.model = glm((Risk=="bad")~., data = trainset, family = binomial())
#predict probability
full.model.prob = predict.glm(full.model, testset[,-10], type = "response")
contrasts(credit$Risk)
full.model.prob = ifelse(full.model.prob > 0.5, "bad", "good")
table(pred = full.model.prob, true=testset$Risk)
#accuracy
mean(full.model.prob==testset$Risk)

#LASSO
x = model.matrix(Risk~., trainset)
cv.out = cv.glmnet(x, trainset$Risk, family = "binomial", type.measure = "mse")
plot(cv.out)
#min value of lambda
cv.out$lambda.min
#best value of lambda
cv.out$lambda.1se
#regression coefficients
coef(cv.out,s=cv.out$lambda.1se)
#get test data
x.test = model.matrix(Risk~., testset)
#predict probability
lasso.prob = predict(cv.out, newx = x.test, s=cv.out$lambda.1se, type = "response")
lasso.prob = ifelse(lasso.prob > 0.5, "good", "bad")
table(pred=lasso.prob,true=testset$Risk)
#accuracy
mean(lasso.prob==testset$Risk)



#cross-vaildation for model selection
logistic.predict = c()
lda.predict = c()

logistic.mod = list()
lda.mod = list()

for (i in 1:10) {
  train = credit[-c((52*i-51):(52*i)),]
  test = credit[(52*i-51):(52*i),]
  #log
  logistic.mod = glm((Risk=="bad")~., family=binomial(), data = train)
  logistic.prob = predict(logistic.mod, newdata = test, type = "response")
  logistic.prob = ifelse(logistic.prob > 0.5, "bad", "good")
  logistic.predict = c(logistic.predict, logistic.prob)
  #lda
  lda.mod = lda((Risk=="bad")~., family=binomial(), data = train)
  lda.prob = predict(lda.mod, test[, -10])$class
  lda.predict = c(lda.predict, lda.prob)
}

#accuracy for logistic
table(logistic.predict, credit$Risk[1:520])
mean(logistic.predict==credit$Risk[1:520])

#accuracy for lda
lda.predict = ifelse(lda.predict == 1, "good", "bad")
table(lda.predict, credit$Risk[1:520])
mean(lda.predict==credit$Risk[1:520])


final.model = glm((Risk=="bad")~., family=binomial(), data = credit)
summary(final.model)


#further study
table(credit$Sex, credit$Job)