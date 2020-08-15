library(tidyverse)
library(leaps)
set.seed(100)

df <- read_csv("CASP.csv")
cor(df)
train <- sample(nrow(df), nrow(df) / 10)
df.train <- df[train,]
df.test <- df[-train,]

#outliers
for (i in seq_len(ncol(df.train))) {
  df.train <- df.train[df.train[i] < boxplot.stats(as.matrix(df.train[i]))$stats[5],]
  df.train <- df.train[df.train[i] > boxplot.stats(as.matrix(df.train[i]))$stats[1],]
}

single.lm <- list()
for (i in 1:9) {
  frm <- paste0("RMSD", "~F", i)
  single.lm[[i]] <- lm(as.formula(frm), df.train)
}
#visualizing realtion between RMSD and features
ggplot(data = df.train, aes(x = F1, y = RMSD)) +
  geom_point() +
  geom_abline(slope = single.lm[[1]]$coefficients[2], intercept = single.lm[[1]]$coefficients[1], col = "red")
ggplot(data = df.train, aes(x = F2, y = RMSD)) +
  geom_point() +
  geom_abline(slope = single.lm[[2]]$coefficients[2], intercept = single.lm[[2]]$coefficients[1], col = "red")
ggplot(data = df.train, aes(x = F3, y = RMSD)) +
  geom_point() +
  geom_abline(slope = single.lm[[3]]$coefficients[2], intercept = single.lm[[3]]$coefficients[1], col = "red")
ggplot(data = df.train, aes(x = F4, y = RMSD)) +
  geom_point() +
  geom_abline(slope = single.lm[[4]]$coefficients[2], intercept = single.lm[[4]]$coefficients[1], col = "red")
ggplot(data = df.train, aes(x = F5, y = RMSD)) +
  geom_point() +
  geom_abline(slope = single.lm[[5]]$coefficients[2], intercept = single.lm[[5]]$coefficients[1], col = "red")
ggplot(data = df.train, aes(x = F6, y = RMSD)) +
  geom_point() +
  geom_abline(slope = single.lm[[6]]$coefficients[2], intercept = single.lm[[6]]$coefficients[1], col = "red")
ggplot(data = df.train, aes(x = F7, y = RMSD)) +
  geom_point() +
  geom_abline(slope = single.lm[[7]]$coefficients[2], intercept = single.lm[[7]]$coefficients[1], col = "red")
ggplot(data = df.train, aes(x = F8, y = RMSD)) +
  geom_point() +
  geom_abline(slope = single.lm[[8]]$coefficients[2], intercept = single.lm[[8]]$coefficients[1], col = "red")
ggplot(data = df.train, aes(x = F9, y = RMSD)) +
  geom_point() +
  geom_abline(slope = single.lm[[9]]$coefficients[2], intercept = single.lm[[9]]$coefficients[1], col = "red")


multiple.lm <- lm(RMSD ~ ., df.train)
summary(multiple.lm)
multiple.lm <- update(multiple.lm, ~. - F3)
summary(multiple.lm)
summary(multiple.lm)$r.squared
mean((multiple.lm$fitted.values - df.train$RMSD)^2)

ggplot(data = multiple.lm, aes(x = multiple.lm$fitted.values, y = multiple.lm$residuals)) +
  geom_point() +
  geom_smooth()
ggplot(data = multiple.lm, aes(x = multiple.lm$fitted.values, y = multiple.lm$residuals^2)) +
  geom_point() +
  geom_smooth()


ggplot(data = multiple.lm, aes(x = df.train$F1, y = multiple.lm$residuals)) +
  geom_point() +
  geom_smooth(color = "red")
ggplot(data = multiple.lm, aes(x = df.train$F2, y = multiple.lm$residuals)) +
  geom_point() +
  geom_smooth(color = "red")
ggplot(data = multiple.lm, aes(x = df.train$F3, y = multiple.lm$residuals)) +
  geom_point() +
  geom_smooth(color = "red")
ggplot(data = multiple.lm, aes(x = df.train$F4, y = multiple.lm$residuals)) +
  geom_point() +
  geom_smooth(color = "red")
ggplot(data = multiple.lm, aes(x = df.train$F5, y = multiple.lm$residuals)) +
  geom_point() +
  geom_smooth(color = "red")
ggplot(data = multiple.lm, aes(x = df.train$F6, y = multiple.lm$residuals)) +
  geom_point() +
  geom_smooth(color = "red")
ggplot(data = multiple.lm, aes(x = df.train$F7, y = multiple.lm$residuals)) +
  geom_point() +
  geom_smooth(color = "red")
ggplot(data = multiple.lm, aes(x = df.train$F8, y = multiple.lm$residuals)) +
  geom_point() +
  geom_smooth(color = "red")
ggplot(data = multiple.lm, aes(x = df.train$F9, y = multiple.lm$residuals)) +
  geom_point() +
  geom_smooth(color = "red")


for (i in 1:9) {
  df.train[, i + 10] <- df.train[, i + 1]^2
}
for (i in 1:9) {
  df.train[, i + 19] <- df.train[, i + 1]^3
}
for (i in 1:9) {
  df.train[, i + 28] <- log(df.train[, i + 1]^2 + 0.00000000001)
}
for (i in 1:9) {
  df.train[, i + 37] <- 1 / (0.0000000000001 + df.train[, i + 1])
}

quad.lm <- lm(RMSD ~ ., df.train[, 1:19])
summary(quad.lm)
ggplot(data = quad.lm, aes(x = quad.lm$fitted.values, y = quad.lm$residuals)) +
  geom_point() +
  geom_smooth()

cub.lm <- lm(RMSD ~ ., df.train[, 1:28])
summary(cub.lm)
ggplot(data = cub.lm, aes(x = cub.lm$fitted.values, y = cub.lm$residuals)) +
  geom_point() +
  geom_smooth()

log.lm <- lm(RMSD ~ ., df.train[, 1:37])
summary(log.lm)
ggplot(data = log.lm, aes(x = log.lm$fitted.values, y = log.lm$residuals)) +
  geom_point() +
  geom_smooth()

full.lm <- lm(RMSD ~ ., df.train)
summary(full.lm)
ggplot(data = full.lm, aes(x = full.lm$fitted.values, y = full.lm$residuals)) +
  geom_point() +
  geom_smooth()

#best subset
df.train <- df.train[, 1:10]
subset.lm <- regsubsets(RMSD ~ ., df.train, nvmax = 9)
summary(subset.lm)
which.max(summary(subset.lm)$adjr2)
which.min(summary(subset.lm)$cp)
which.min(summary(subset.lm)$bic)
ggplot(data = data.frame(summary(subset.lm)$adjr2), aes(x = 1:9, y = summary.subset.lm..adjr2)) +
  geom_point() +
  geom_line(col = "red")
ggplot(data = data.frame(summary(subset.lm)$cp), aes(x = 1:9, y = summary.subset.lm..cp)) +
  geom_point() +
  geom_line(col = "red")
ggplot(data = data.frame(summary(subset.lm)$bic), aes(x = 1:9, y = summary.subset.lm..bic)) +
  geom_point() +
  geom_line(col = "red")
summary(subset.lm)$bic


for (i in 1:9) {
  df.train[, i + 10] <- df.train[, i + 1]^2
}
for (i in 1:9) {
  df.train[, i + 19] <- df.train[, i + 1]^3
}
for (i in 1:9) {
  df.train[, i + 28] <- log(df.train[, i + 1]^2 + 0.00000000001)
}

################## don't run this it takes a lot of time:)
all.subset.lm <- regsubsets(RMSD ~ ., df.train, nvmax = 36)
##################

backward.sub.lm <- regsubsets(RMSD ~ ., df.train, nvmax = 36, method = "backward")
summary(backward.sub.lm)$adjr2
which.max(summary(backward.sub.lm)$adjr2)
summary(backward.sub.lm)$cp
which.min(summary(backward.sub.lm)$cp)
summary(backward.sub.lm)$bic
which.min(summary(backward.sub.lm)$bic)
ggplot(data = data.frame(summary(backward.sub.lm)$bic), aes(x = 1:36, y = summary.backward.sub.lm..bic)) +
  geom_point() +
  geom_line(col = "red")
ggplot(data = data.frame(summary(backward.sub.lm)$cp), aes(x = 1:36, y = summary.backward.sub.lm..cp)) +
  geom_point() +
  geom_line(col = "red")
ggplot(data = data.frame(summary(backward.sub.lm)$adjr2), aes(x = 1:36, y = summary.backward.sub.lm..adjr2)) +
  geom_point() +
  geom_line(col = "red")

for (i in 1:9) {
  df.test[, i + 10] <- df.test[, i + 1]^2
}
for (i in 1:9) {
  df.test[, i + 19] <- df.test[, i + 1]^3
}
for (i in 1:9) {
  df.test[, i + 28] <- log(df.test[, i + 1]^2 + 0.00000000001)
}

test.matrix <- model.matrix(RMSD ~ ., df.test)
test.err <- rep(NA, 36)
for (i in 1:36) {
  coefi <- coef(backward.sub.lm, id = i)
  pred <- test.matrix[, names(coefi)] %*% coefi
  test.err[i] <- mean((df.test$RMSD - pred)^2)
}
test.err
which.min(test.err)
min(test.err)
ggplot(data = NULL,aes(x=1:36,y=test.err)) + geom_point() +geom_line(color = "red")
df.train.7 <- df.train[names(coef(backward.sub.lm, 7))[-1]]
df.train.7$RMSD <- df.train$RMSD
best.backward.sub.lm <- lm(RMSD ~ ., df.train.7)
summary(best.backward.sub.lm)

ggplot(data = best.backward.sub.lm, aes(x = best.backward.sub.lm$fitted.values, y = best.backward.sub.lm$residuals)) +
  geom_point() +
  geom_smooth()

df.train <- df.train[, 1:10]
df.test <- df.test[, 1:10]


###
boxplot.stats(df.train$RMSD)
df.1 <- df.train[df.train$RMSD < 7.5,]
df.2 <- df.train[!df.train$RMSD < 7.5,]
F2.first.half.lm <- lm(RMSD~F2,df.1)
F2.second.half.lm <- lm(RMSD~F2,df.2)
ggplot(data = df.train,aes(x=F2,y=RMSD)) +
  geom_point() +
  geom_hline(yintercept = 7.5, color ="blue" , linetype = "dashed") +
  geom_abline(slope = F2.first.half.lm$coefficients[2],intercept = F2.first.half.lm$coefficients[1], color = "red") +
  geom_abline(slope = F2.second.half.lm$coefficients[2],intercept = F2.second.half.lm$coefficients[1], color = "green")

df.train$class <- df.train$RMSD > 7.5
class.fit <- glm(class ~ . - RMSD, df.train, family = binomial)
pre.class <- predict(class.fit, type = "response")
predict.class <- pre.class > 0.5
df.train$pre.class <- predict.class
mean(predict.class == df.train$class)

new.fit <- lm(RMSD ~ .:class, df.train)
summary(new.fit)
pre.fit <- lm(RMSD ~ .:pre.class, df.train[, -11])
summary(pre.fit)
ggplot(data = NULL, aes(x = pre.fit$fitted.values, y = pre.fit$residuals)) +
  geom_smooth() +
  geom_point()
ggplot(data = NULL, aes(x = new.fit$fitted.values, y = new.fit$residuals)) +
  geom_smooth() +
  geom_point() +
  geom_vline(xintercept = 7.5, color = "red")