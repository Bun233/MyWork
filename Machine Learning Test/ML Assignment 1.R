install.packages("palmerpenguins")
library(splines)
library("palmerpenguins")
?penguins

polar_bear <- which( rowSums( is.na( penguins ) ) > 0 )
palmer <- penguins[-polar_bear,]
dim(palmer)

y = palmer$body_mass_g
x = palmer$flipper_length_mm

table(cut(x,4)) #not correct as not evenly distributed
summary(x)

spline1 = lm( y ~ bs( x, degree = 1, knots = summary(x)[c(2,3,5)] ) )

Q7.1 <- lm(y~poly(x,1))
Q7.2 <- lm(y~poly(x,2))
Q7.3 <- lm(y~poly(x,3))
Q7.4 <- lm(y~poly(x,4))
Q7.5 <- lm(y~poly(x,5))
anova(Q7.1,Q7.2,Q7.3,Q7.4,Q7.5)

#Q9
step = lm( y ~ cut(x, 8) )
pred.Q9 <- predict(step, newdata = list(x = sort(x)), se = TRUE)
plot(x, y, cex.lab = 1.1, col="darkgrey", bty = 'l')
se.bands8 <- cbind(pred.Q9$fit + 2*pred.Q9$se.fit, pred.Q9$fit-2*pred.Q9$se.fit)
lines(x = sort(x), y = pred.Q9$fit, lwd = 2, col = "red")
matlines(sort(x), se.bands8, lwd = 1.4, col = "red", lty = 3)

#Fast food analysis
install.packages("openintro")
library("openintro")
?fastfood

fries <- fastfood[,-c(1,2,4,7,17)]
burger <- which( rowSums( is.na( fries ) ) > 0 )
ffood <- fries[-burger,]
sum( is.na( ffood ) )

fastfood[,c(1,2,4,7,17)]
head(fastfood)
head(ffood)
a <- ffood[,-c(5,6,9,10,11,12)]
pairs(a)

#Q14
set.seed(1)
library(glmnet)
y.14 = ffood$calories
x.14 = model.matrix(calories~.,ffood)[,-1]
lasso = glmnet(x.14,y.14)
lasso.cv <- cv.glmnet(x.14,y.14,nfolds = 20)
lasso.cv$lambda.min
coef(lasso.cv, s = 'lambda.min') #fit under lasso (Q14)

library(leaps)
bkwd <- regsubsets(calories~.,ffood,nvmax = 12,method = 'backward')
result <- summary(bkwd)
result #Q14
names(result)
cp <- result$cp
which.min(cp)
coef(bkwd,4) #Q15

#Q16
library(pls)
set.seed(3)
rep200 <- 200
predict.regsubsets = function(object, newdata, id, ...){
  form = as.formula(object$call[[2]])
  mat = model.matrix(form, newdata)
  coefi = coef(object, id = id)
  xvars = names(coefi)
  mat[, xvars]%*%coefi
}

result.best <- c()
result.ridge <- c()
result.pcr <- c()

for (i in 1:rep200){
  training <- sample(1:301,230)
  x.train <- model.matrix(calories~.,data = ffood[training,])[,-1]
  y.train <- ffood$calories[training]
  x.test <- model.matrix(calories~.,data = ffood[-training,])[,-1]
  y.test <- ffood$calories[-training]
  
  best.train <- regsubsets(calories~.,data = ffood[training,],nvmax = 11)
  bic.min.index <- which.min(summary(best.train)$bic)
  ridge.train <- cv.glmnet(x.train,y.train,alpha = 0,nfolds = 5)
  pcr.train <- pcr(calories~., data = ffood[training,], scale = TRUE, validation = 'CV')
  pcr.min.index <- which.min(MSEP(pcr.train)$val[1,,])-1
  
  predict.best <- predict.regsubsets(best.train,ffood[-training,],bic.min.index)
  predict.ridge <- predict(ridge.train, x.test, s = 'lambda.min')
  predict.pcr <- predict(pcr.train,ffood[-training,], ncomp = pcr.min.index)
  
  result.best[i] <- cor(y.test,predict.best)
  result.ridge[i] <- cor(y.test,predict.ridge)
  result.pcr[i] <- cor(y.test,predict.pcr)
} 

boxplot(result.best,result.ridge,result.pcr)

#Q17
set.seed(3)
min.valid=c()
for(n in 1:rep200){ 
  set.seed(n)
  training.obs = sample(1:301, 230) 
  train = ffood[training.obs, ] 
  test = ffood[-training.obs, ] 
  best = regsubsets(calories~., data = train, nvmax = 11)
  val.error<-c() 
  for(i in 1:11){ 
    pred = predict.regsubsets(best, test, i) 
    val.error[i] = mean((test$calories - pred)^2)
  }
  val.error 
  min.valid[n] = which.min(val.error)
}
hist(min.valid, col = 3, breaks = seq( from = 0, to = 11, length = 11 ),
     xlab = 'Number of Predictors', main = 'Best Subset Selection with validation')

#Q18
set.seed(3)
store <- c()
for(n in 1:rep200){ 
  set.seed(n)
  training.obs = sample(1:301, 230) 
  train = ffood[training.obs, ] 
  test = ffood[-training.obs, ] 
  pcr.fit = pcr(calories~., data=train,scale = TRUE, validation="CV")
  min.pcr = which.min(MSEP(pcr.fit)$val[1,, ] ) - 1
  store[n] = min.pcr
}
print(round(mean(store)),0)

#Q19
library(gam)
par(mfrow = c(1,3)) 
gam.fit = gam(calories ~ s(total_fat, df =6 ) + ns(cholesterol, df = 3) + vit_a, data = ffood)
plot(gam.fit, se = TRUE, col = "red")