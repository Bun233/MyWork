library(MASS)
library(ggplot2)
installed.packages("dplyr")
install.packages("tidyverse")
install.packages("caret")
install.packages("caTools")
library(caret)
library(dplyr)
library(caTools)
library(tidyverse)
library(MASS)

#model selection

patient <- read.csv('C:/University/Master/Durham/Statistics/Assignment/Assignment 2/insurance.csv')
patient1 <- patient %>% 
  mutate(smoking = if_else(smoker == "no",0,1)) %>%
  mutate(sex = if_else(sex == "male",1,0))
cleaned=patient1[,c('charges','bmi','age', 'children','smoking','sex')]
pairs(cleaned,col=c('gold','deepskyblue','firebrick','chocolate','beige'))

full <- lm(charges~ .,data = cleaned)
null <- lm(charges~ 1,data = cleaned)
step(null, scope=list(lower=null, upper=full), direction="forward")
step(full, direction="backward")
step(null, scope = list(upper=full), direction="both")
mlm.fitted <- lm(formula = charges ~ bmi + age + children + smoking, data = cleaned)
summary(mlm.fitted)
plot(mlm.fitted,col = c('blue','chocolate','darkorchid','green'))

lm.bmi <- lm(charges~ bmi, data = cleaned)
lm.age <- lm(charges~ age, data = cleaned)
lm.smoking <- lm(charges~ smoking, data = cleaned)
lm.children <- lm(charges~ children, data = cleaned)
lm.sex <- lm(charges~ sex, data = cleaned)

summary(lm.age)
summary(lm.children)
summary(lm.smoking)
summary(lm.sex)

#final
install.packages("car")
library(car)
train <- patient[1:1070,]
test <- patient[1071:1338,]

full <- lm(charges~ .,data = patient)
null <- lm(charges~ 1,data = patient)
new.fitted <- lm(formula = charges ~ smoker + age + bmi + children + region, 
                 data = patient)
new.fitted2 <- lm(formula = charges ~ smoker + age + bmi + children, 
                  data = patient)
summary(new.fitted2)
plot(new.fitted2)
summary(new.fitted)
plot(new.fitted)

plot(new.fitted)
full.log0 <- lm(log(charges)~ smoker + log(age) + log(bmi) + children + region + sex,data = patient)
null.log0 <- lm(log(charges)~ 1,data = patient)
step(full.log0, direction="backward")
step(null.log0, scope=list(lower=null.log0, upper=full.log0), direction="forward")
step(null.log0, scope = list(upper=full.log0), direction="both")
new.fitted.log0 <- lm(formula = log(charges) ~ smoker + log(age) + log(bmi) + children + 
                        region + sex, data = patient)
summary(new.fitted.log0)
plot(new.fitted.log0)

#cross-validation
library(boot)
glm.fit.log <- glm(log(charges) ~ smoker + log(age) + log(bmi) + children + 
                     region + sex, data = patient)
summary(glm.fit.log)
patient.log <- patient
patient.log$charges <- log(patient.log$charges)
view(patient.log)

cv.err <- cv.glm(patient.log, glm.fit.log)
cv.err$delta[2]
cv.err5 <- cv.glm(patient.log, glm.fit.log, K = 5)
cv.err5$delta[2]
cv.err10 <- cv.glm(patient.log,glm.fit.log,K=10)
cv.err10$delta[2]
  
boot.fn <- function(d, index) {
  coef(lm(formula = log(charges) ~ smoker + log(age) + log(bmi) + children + 
            region + sex, data = d, subset = index))
}
boot.fn(patient,1:9366)
boot(patient,boot.fn,1000)

#Breusch-Pagan Test
library(lmtest)
bptest(new.fitted.log0)


  
full2 <- lm(charges~ bmi + age,data = patient)
summary(full2)
plot(full2,col=c('gold','deepskyblue'))

full.log1 <- lm(log(charges)~ smoking + log(age) + log(bmi) + sex,data = cleaned)
full.log2 <- lm(log(charges)~ log(age) + log(bmi),data = cleaned)
lm.age.log <- lm(log(charges)~ log(age), data = cleaned)
lm.bmi.log <- lm(log(charges)~ log(bmi), data = cleaned)
plot(lm.age.log)
plot(lm.bmi.log)
plot(full.log1,col=c('gold','deepskyblue'))
summary(full.log1)
summary(full.log2)
summary(lm.bmi.log)
cleaned.scale$cgscaled <- scale(cleaned$charges)
cleaned.scale$smkscaled <- scale(cleaned$smoking)
cleaned.scale$agescaled <- scale(cleaned$age)
cleaned.scale$bmiscaled <- scale(cleaned$bmi)
cleaned.scale$chdscaled <- scale(cleaned$children)
cleaned.scale$sexscaled <- scale(cleaned$sex)
full.scale <- lm(cgscaled~ .,data = cleaned.scale)
null.scale <- lm(cgscaled~ 1,data = cleaned.scale)
step(full.scale, direction="backward")
fitted.scale <- lm(formula = cgscaled ~ smkscaled + agescaled + bmiscaled + chdscaled, 
                   data = cleaned.scale)
plot(fitted.scale,col=c('gold','deepskyblue'))
lm.age.scale <- lm(cgscaled~ agescaled,data = cleaned.scale)
plot(lm.age.scale)

lm.bmi.qua <- lm(log(charges)~ poly(bmi,2), data = patient)
summary(lm.bmi.qua)
plot(lm.bmi.qua)

fu <- lm(charges~ .,data = patient)
nu <- lm(charges~ 1,data = patient)
summary(fu)
contrasts(patient$sex)
contrasts(patient$smoker)


#data description

summary(patient)
sd(patient$age)
sd(patient$bmi)
sd(patient$charges)
sd(patient$children)

patient.box <- patient
patient.box$age.box <- patient$age
patient.box$bmi.box <- patient$bmi
patient.box$children.box <- patient$children
patient.box$charges.box <- patient$charges
patient.box <- patient.box[,8:11]
patient.box2 <- patient.box[,1:3]
boxplot(patient.box2)
boxplot(patient$children,xlab = 'children')
boxplot(patient.box)

par(mfrow=c(2,2))
hist(patient$age, main="Histogram of age")
hist(patient$bmi, main="Histogram of bmi")
hist(patient$children, main="Histogram of children")
hist(patient$charges, main="Histogram of charges")

table(patient$sex)
table(patient$smoker)
table(patient$region)
par(mfrow=c(3,1))
pie(table(patient$sex),main = 'Pie Chart of sex')
barplot(table(patient$sex),main = 'Barplot of sex')
pie(table(patient$smoker),main = 'Pie chart of smoker')
barplot(table(patient$smoker),main = 'Barplot of smoker')
pie(table(patient$region),main = 'Pie chart of region')
barplot(table(patient$region),main = 'Barplot of region')


#EDA

par(mfrow=c(2,3))
ggplot(patient,aes(x=age, y=charges))+geom_point()
ggplot(patient,aes(x=bmi, y=charges))+geom_point()
ggplot(patient,aes(x=children, y=charges))+geom_point()
ggplot(patient,aes(x=sex, y=charges))+geom_point()
ggplot(patient,aes(x=smoker, y=charges))+geom_point()
ggplot(patient,aes(x=region, y=charges))+geom_point()

pairs(patient,col=c('gold','deepskyblue','firebrick','chocalate','beige','aquamarine','blue'))
fpairs <- cleaned[,1:4]
pairs(fpairs,col=c('gold','deepskyblue','firebrick','chocolate'))
fpairs2 <- cleaned[,1:3]
pairs(fpairs2,col=c('gold','deepskyblue','firebrick'))