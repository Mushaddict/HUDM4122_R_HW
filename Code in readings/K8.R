women <- women
fit <- lm(weight ~ height, data = women)
summary(fit)

women$weight
fitted(fit)
residuals(fit)
plot(women$height, women$weight, 
     xlab = "Height", 
     ylab = "Weight")
abline(fit)
## weight = -87.52 + 3.45 * Height

## 多项式回归
fit2 <- lm(weight ~ height + I(height^2), data = women)
summary(fit2)
plot(women$height, women$weight, 
     xlab = "Height", 
     ylab = "Weight")
lines(women$height, fitted(fit2))
## weight = 261.88 - 7.35*height + 0.083 * height^2

library(car)
scatterplot(weight ~ height, 
            data = women, 
            spread = FALSE, lty.smooth = 2, 
            pch = 19, 
            main = "Women Age 30-39", 
            xlab = "Height", 
            ylab = "Weight")

## 多元线性回归
states <- as.data.frame(state.x77[, c("Murder", "Population", "Illiteracy", 
                                     "Income", "Frost")])
## 检查相关性
cor(states)
scatterplotMatrix(states, spread = FALSE, lty.smooth = 2, 
                  main = "Scatter Plot Matrix")
## 多元线性回归
fit <- lm(Murder ~ Population + Illiteracy + Income + Frost, 
          data = states)
summary(fit)
d
## 有交互项的多元线性回归
mtcars <- mtcars
fit <- lm(mpg ~ hp + wt + hp:wt, data = mtcars)
summary(fit)
library(effects)
plot(effect("hp:wt", fit, 
            list(wt = c(2.2, 3.2, 4.2))), 
     multiline = TRUE)
