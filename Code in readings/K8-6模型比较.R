library(car)
library(tidyverse)
states <- as.data.frame(state.x77[, c("Murder", "Population", "Illiteracy", 
                                      "Income", "Frost")])
fit1 <- lm(Murder ~ Population + Illiteracy + Income + Frost, 
           data = states)
fit2 <- lm(Murder ~ Population + Illiteracy, data = states)
anova(fit2, fit1)

## AIC赤池信息准则，越小的模型要优先选择
AIC(fit1, fit2)

## 变量选择
### 逐步回归 模型会添加或者删除一个变量，直到达到某个判定值为止 stepwise method
### 向前回归是每次都增加一个变量进去 foward stepwise
### 向后回归是每次都删除一个变量进去 backward stepwise
library(MASS)
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, 
          data = states)
stepAIC(fit, direction = "backward")

### 全子集回归 所有的模型都会被检验
library(leaps)
leaps <- regsubsets(Murder ~ Population + Illiteracy + Income + Frost, 
                    data = states, nbest = 4)
summary(leaps)
plot(leaps, scale = "adjr2")
library(car)
subsets(leaps, statistic = "cp", 
        main = "Cp Plot for All Subsets Regression")
abline(1, 1, lty = 2, col = "red")

