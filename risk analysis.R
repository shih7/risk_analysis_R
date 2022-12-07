#CRAWFORD DEVELOPMENT CO. AND SOUTHEAST BANK OF TEXAS Case Study
#Group 4 Team B

library(ggplot2)
library(magrittr)
library(tidyverse)
library(gridExtra)
library(fitdistrplus)
library(FinCal)

#Loading data 
expected <- c(66382.35, 60852.39, 65872.30, 64541.97, 73871.30, 52689.86, 63470.57, 55828.80, 63972.39, 61999.35, 78151.43, 75928.84, 73195.79, 70009.95, 58471.00, 66242.29, 
              79315.70, 60000.95, 60839.91, 65844.29, 70511.40, 49527.53, 67793.12, 72333.48, 55992.65, 75956.10, 61969.04, 55875.70, 74283.09, 85382.88, 69717.37, 56725.72)
indicators <- c(-1, 0, 0, 1, 1, -1, -1, 1, 0, 1, 1, 1, -1, -1, 0, 1, 0, -1, 1, -1, -1, -1, 0, -1, 1, -1, 0, 0, 1, 0, 1, -1)
actual <- c(51302.37, 63452.49, 67932.41, 79689.09, 70408.31, 46006.15, 61575.80, 65413.68, 57756.02, 72319.87, 84962.93, 82105.66,64361.14, 64554.01, 56729.86, 76268.45,
            84896.93, 43384.19, 68533.78, 51601.49, 62379.33, 36775.17, 70128.14, 61806.21, 68218.22, 63401.88, 60720.96, 65842.55, 78509.94, 83909.33, 87853.35, 38732.77)

officesale <- data.frame(expected, indicators, actual)

## EDA
officesale %>% group_by(indicators) %>% summarise(count = sum(actual > 0)) 
officesale_1 <- filter(officesale, indicators == 1)
officesale_2 <- filter(officesale, indicators == 0)
officesale_3 <- filter(officesale, indicators == -1)
# little diff in each group

#view trend of expected sales
hist(expected)  #seems like normal distribution

#try to fit with different distributions
expec_model1<- fitdist(officesale$expected, "norm")
expec_model2<- fitdist(officesale$expected,"weibull")
expec_model3<- fitdist(officesale$expected,"lnorm")
gofstat(list(expec_model1,expec_model2,expec_model3))
#Lognormal distribution has the lowest KS value and lowest AD value. Therefore, the distribution of expected value is close to Lognormal distribution.

plot(expec_model3)

expec_model3$estimate #meanlog: 11.0902055, sdlog: 0.1263053

#view trend of actual sales
hist(actual)

#check the relationship between actual sales and expected sales
cor(officesale$expected, officesale$actual) # corrleation = 0.67
cor(officesale_1$expected, officesale_1$actual) # correlation = 0.72
cor(officesale_2$expected, officesale_2$actual) # correlation = 0.89
cor(officesale_3$expected, officesale_3$actual) # correlation = 0.89

ggplot(data = officesale) + 
  geom_point(mapping = aes(x = expected, y = actual)) + 
  geom_smooth(mapping = aes(x = expected, y = actual)) + 
  xlab("Expected sales") + ylab("Actual sales") #close to linear

# So we should build a linear reg model with 2 predictors: actual sales ~ indicators + expected sales

office.lm <- lm(actual ~ expected + factor(indicators))
summary(office.lm) # Adjusted R-squared is 0.8485, which seems like fair. p-value is far smaller than 0.05, which means like significant

#try to identify leverage points through hat statistics
hat.plot <- function(x){
  p <- length(coefficients(x))
  n <- length(fitted(x))
  plot(hatvalues(x), main = "Index Plot of Hat Values")
  abline(h = c(2, 3) * p/n, col = "red", lty = 2)
  identify(1:n, hatvalues(x), names(hatvalues(x)))
}

hat.plot(office.lm) #only one point is above 0.25

hatvalues(office.lm)[which(hatvalues(office.lm) > 0.25)] #point of 30 is leverage point

#try to identify the influence points
cutoff <- 4/(nrow(actual) - length(office.lm$coefficients) - 2)
plot(office.lm, which = 4, cook.levels = cutoff)
abline(h = cutoff, lty = 2, col = "red") #points of 5, 28, and 31 are influence points

#refit the model after removing the leverage points and influence points
office.lm2 <- lm(actual ~ expected + factor(indicators), data = officesale[-c(5, 28, 30, 31), ])
summary(office.lm2) # Adjusted R-squared is increased to 0.892. p-value is still far smaller than 0.05.
#The refitted model performs better.


#Information from case:  P(indicators=-1) = 12/32 , P(indicators=0) = 9/32 , P(indicators=1) = 11/32
#simulate actual sales

n <- 1000
rlnorm2 <- function(n, mean, sd){
  rlnorm(n, log(mean*(1+sd^2/mean^2)^-0.5), log(1+sd^2/mean^2)^0.5)
}

expected_sim <- rlnorm2(n, mean(expected), sd(expected))

prob <- runif(n, 0, 1)
indicators_sim <- ifelse(prob < 12/32, -1, ifelse(prob > 21/32, 1, 0))

df_sim <- data.frame(expected = expected_sim, indicators = indicators_sim)

df_sim$Predcted_Actual <- predict(office.lm2, df_sim)


ggplot(data = df_sim) + 
  geom_histogram(aes(x = Predcted_Actual, y = ..count../n), bins = 20) + 
  xlab("Predicted actual sales") + 
  ylab("Probability") + 
  labs(title = "Histogram of the predicted actual sales")

mean(df_sim$Predcted_Actual)
mean(df_sim$Predcted_Actual)-qnorm(0.95)*sd(df_sim$Predcted_Actual)/(n^0.5)
mean(df_sim$Predcted_Actual)+qnorm(0.95)*sd(df_sim$Predcted_Actual)/(n^0.5)

# calculate cumulative FCF
df_sim$cFCF <- 37875 - 24375 - 9000 - 12500 + (df_sim$Predcted_Actual - 8636.03 - 38375.00)

ggplot(data = df_sim) + 
  geom_histogram(aes(x = cFCF, y = ..count../n)) + 
  xlab("Cumulated FCF") + 
  ylab("Probability") + 
  labs(title = "Histogram of the cumulated FCF")

mean(df_sim$cFCF)
mean(df_sim$cFCF)-qnorm(0.95)*sd(df_sim$cFCF)/sqrt(n)
mean(df_sim$cFCF)+qnorm(0.95)*sd(df_sim$cFCF)/sqrt(n)


# calculate discounted FCF
DiscFactor <- (1.07)^(1/12) - 1
df_sim$discounted <- 37875 - 24375/((1+DiscFactor) ^ 3) - 9000/1.07 - 12500/(1.07^2) + (df_sim$Predcted_Actual - 8636.03 - 38375.00)/(1.07^3)
ggplot(data = df_sim) + 
  geom_histogram(aes(x = discounted, y = ..count../n)) + 
  xlab("Discounted FCF") + 
  ylab("Probability") + 
  labs(title = "Histogram of the discounted FCF")

#95% interval
mean(df_sim$discounted)
mean(df_sim$discounted)-qnorm(0.95)*sd(df_sim$discounted)/sqrt(n)
mean(df_sim$discounted)+qnorm(0.95)*sd(df_sim$discounted)/sqrt(n)

#max project for Bank and CDC
max(df_sim$discounted)
max(df_sim$cFCF)

#Probability CDC lose money
sum(df_sim$discounted<0)/n
sum(df_sim$cFCF<0)/n

#probability revenue of office project smaller than residential project
sum(df_sim$cFCF - 9088.97 <0)/n

#calculate interest through 7% to 11% 
for(IR in c(0.07, 0.08, 0.09, 0.10, 0.11)){
  DiscFactor1 <- (IR+1)^(1/12) - 1
  IT <- fv(IR,3,-38375,0) - 38375 
  
  OP <- 37875 - 24375 - 9000 - 12500 + (df_sim$Predcted_Actual - IT - 38375.00)
  RP <- 9088.97 + 8636.03 - IT
  cat("When interest rate is", IR,", interest will be $", IT, "thousand dollars, cause FCF of $", mean(OP), "thousand dollars 
  in the office project, and FCF of $", RP, " thousand dollars in the residential project", "\n")
}

