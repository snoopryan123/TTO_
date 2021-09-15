library(tidyverse)
library(rstanarm)
library(ggplot2)
library(bayesplot)
theme_set(bayesplot::theme_default())

################################
########### THE CODE ###########
################################

# data
data(kidiq)

# bayesian linear regression, 4 MCMC's
post1 <- stan_glm(kid_score ~ mom_hs, data = kidiq,
                  family = gaussian(link = "identity"),
                  seed = 12345)
post2 <- update(post1, formula = . ~ mom_iq)
post3 <- update(post1, formula = . ~ mom_hs + mom_iq)
post4 <- update(post1, formula = . ~ mom_hs * mom_iq)

# plot: overlay the estimated regression line
base <- ggplot(kidiq, aes(x = mom_hs, y = kid_score)) +
        geom_point(size = 1, position = position_jitter(height = 0.05, width = 0.1)) +
        scale_x_continuous(breaks = c(0,1), labels = c("No HS", "HS"))
base + geom_abline(intercept = coef(post1)[1], slope = coef(post1)[2],
                   color = "skyblue4", size = 1)

# plot: add our uncertainty estimates, by plotting the estimated regression line at 
# each draw from the posterior ditribution
draws <- as.data.frame(post1)
colnames(draws)[1:2] <- c("a", "b")
base +
  geom_abline(data = draws, aes(intercept = a, slope = b),
              color = "skyblue", size = 0.2, alpha = 0.25) +
  geom_abline(intercept = coef(post1)[1], slope = coef(post1)[2],
              color = "skyblue4", size = 1)

# plot for second model
draws <- as.data.frame(as.matrix(post2))
colnames(draws)[1:2] <- c("a", "b")
ggplot(kidiq, aes(x = mom_iq, y = kid_score)) +
  geom_point(size = 1) +
  geom_abline(data = draws, aes(intercept = a, slope = b),
              color = "skyblue", size = 0.2, alpha = 0.25) +
  geom_abline(intercept = coef(post2)[1], slope = coef(post2)[2],
              color = "skyblue4", size = 1)

# plot 3rd and 4th model
reg0 <- function(x, ests) cbind(1, 0, x) %*% ests
reg1 <- function(x, ests) cbind(1, 1, x) %*% ests

args <- list(ests = coef(post3))
kidiq$clr <- factor(kidiq$mom_hs, labels = c("No HS", "HS"))
lgnd <- guide_legend(title = NULL)
base2 <- ggplot(kidiq, aes(x = mom_iq, fill = relevel(clr, ref = "HS"))) +
        geom_point(aes(y = kid_score), shape = 21, stroke = .2, size = 1) +
        guides(color = lgnd, fill = lgnd) +
        theme(legend.position = "right")
base2 +
        stat_function(fun = reg0, args = args, aes(color = "No HS"), size = 1.5) +
        stat_function(fun = reg1, args = args, aes(color = "HS"), size = 1.5)
#
reg0 <- function(x, ests) cbind(1, 0, x, 0 * x) %*% ests
reg1 <- function(x, ests) cbind(1, 1, x, 1 * x) %*% ests
args <- list(ests = coef(post4))
base2 +
  stat_function(fun = reg0, args = args, aes(color = "No HS"), size = 1.5) +
  stat_function(fun = reg1, args = args, aes(color = "HS"), size = 1.5)

# Posterior Predictions
IQ_SEQ <- seq(from = 75, to = 135, by = 5)
y_nohs <- posterior_predict(post4, newdata = data.frame(mom_hs = 0, mom_iq = IQ_SEQ))
y_hs <- posterior_predict(post4, newdata = data.frame(mom_hs = 1, mom_iq = IQ_SEQ))
dim(y_hs)


par(mfrow = c(1:2), mar = c(5,4,2,1))
boxplot(y_hs, axes = FALSE, outline = FALSE, ylim = c(10,170),
        xlab = "Mom IQ", ylab = "Predicted Kid IQ", main = "Mom HS")
axis(1, at = 1:ncol(y_hs), labels = IQ_SEQ, las = 3)
axis(2, las = 1)
boxplot(y_nohs, outline = FALSE, col = "red", axes = FALSE, ylim = c(10,170),
        xlab = "Mom IQ", ylab = NULL, main = "Mom No HS")
axis(1, at = 1:ncol(y_hs), labels = IQ_SEQ, las = 3)


