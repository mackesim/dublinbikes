" 
  Code to perform regression models on estimated trips.
"

library(MASS)
library(tidyverse)
library(broom)
library(lmtest)
theme_set(theme_classic())

# Read previously processed data
result_table <- read_rds("C:/Users/user/Documents/DublinBikes/result_table.rds")
each_interval <- read_rds("C:/Users/user/Documents/DublinBikes/each_interval.rds")
sam_data <- read_rds("C:/Users/user/Documents/DublinBikes/sam_data")

######################################### individual DAM model ######################################################
attach(result_table)
iDAM <- lm(formula = T ~ 0 + Tx + Tx2)
summary(iDAM)

# Check hypothesis validity with graph
par(mfrow = c(2, 2))
plot(iDAM)
model.diag.metrics <- augment(iDAM)
model.diag.metrics <- model.diag.metrics %>%
  mutate(index = 1:nrow(model.diag.metrics)) %>%
  select(index, everything(), -.se.fit, -.sigma)

# Check hypothesis validity with test
raintest(iDAM)
shapiro.test(resid(iDAM))
bptest(iDAM)
dwtest(iDAM)

# Test by delete outliers
test <- result_table[-c(22, 23,72), ]
iDAM2 <- lm(formula = test$T ~ 0 + log1p(test$Tx) + test$Tx2)
summary(iDAM2)
par(mfrow = c(2, 2))
plot(iDAM2)
raintest(iDAM2)
shapiro.test(resid(iDAM2))
bptest(iDAM2)
dwtest(iDAM2)

# Calculate RMSE
RMSE <- sqrt(c(crossprod(iDAM2$residuals)) / length(iDAM2$residuals))

######################################### combined DAM model ######################################################
cDAM <- lm(formula = T ~ 0 + Tx + (Tx2/A))
summary(cDAM)

# Check hypothesis validity with graph
par(mfrow = c(2, 2))
plot(cDAM)
model.diag.metrics <- augment(cDAM)
model.diag.metrics <- model.diag.metrics %>%
  mutate(index = 1:nrow(model.diag.metrics)) %>%
  select(index, everything(), -.se.fit, -.sigma)

# Check hypothesis validity with test
raintest(cDAM)
shapiro.test(resid(cDAM))
bptest(cDAM)
dwtest(cDAM)

######################################### Interval aggregation model #################################################
attach(each_interval)
IAM <- lm(formula = I ~ 0 + absdiff + A + A2)
summary(IAM)

# Check hypothesis validity with graph
par(mfrow = c(2, 2))
plot(IAM)
model.diag.metrics <- augment(IAM)
model.diag.metrics <- model.diag.metrics %>%
  mutate(index = 1:nrow(model.diag.metrics)) %>%
  select(index, everything(), -.se.fit, -.sigma)

# Check hypothesis validity with test
raintest(IAM)
shapiro.test(resid(IAM))
bptest(IAM)
dwtest(IAM)

# Test by delete outliers
test <- each_interval
IAM2 <- lm(formula = test$I ~ 0 + log1p(test$absdiff) + test$A + test$A2)
summary(IAM2)
par(mfrow = c(2, 2))
plot(IAM2)
raintest(IAM2)
shapiro.test(resid(IAM2))
bptest(IAM2)
dwtest(IAM2)

######################################### Station aggregation model #################################################
attach(sam_data)
SAM <- lm(formula = I ~ 0 + absdiff + A + A2)
summary(SAM)

# Check hypothesis validity with graph
par(mfrow = c(2, 2))
plot(SAM)
model.diag.metrics <- augment(SAM)
model.diag.metrics <- model.diag.metrics %>%
  mutate(index = 1:nrow(model.diag.metrics)) %>%
  select(index, everything(), -.se.fit, -.sigma)

# Check hypothesis validity with test
raintest(SAM)
shapiro.test(resid(SAM))
bptest(SAM)
dwtest(SAM)

# Test by delete outliers
test <- sam_data
SAM2 <- lm(formula = test$I ~ 0 + sqrt(test$absdiff) + sqrt(test$A) + sqrt(test$A2))
summary(SAM2)
par(mfrow = c(2, 2))
plot(SAM2)
raintest(SAM2)
shapiro.test(resid(SAM2))
bptest(SAM2)
dwtest(SAM2)