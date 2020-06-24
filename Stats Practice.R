

data("ToothGrowth")

tooth = ToothGrowth

colnames(tooth) = c('Length', 'Supplement', 'Dose')



set.seed(123)

sample1 = tooth[1:30,]

sample2 = tooth[31:60,]




# ***** ON TOOTH
# ______________

# Basics Statistics

# Mean, Mediun, Mode,

mean(tooth$Length)

# [1] 18.81333

median(tooth$Length)

# [1] 19.25


mode(tooth$Dose)

sd(tooth$Length)

# [1] 7.649315

###################################################################################

# Histogram - Skewness, Kutoses

hist(tooth$Length, col = 'seagreen')

plot(density(tooth$Length))


library(e1071)

skewness(tooth$Length)

# Skew = 3 * (Mean - Median) / SD

# [1] -0.1425376 {Negative skewness, distribution slightly skewed left}



kurtosis(tooth$Length)

# [1] -1.042514 

###################################################################################


# Check Outliers (Box Plot) - ALso use percentile at 90%, 95%, 99%

boxplot(tooth$Length, col = 'cornflowerblue')

quantile(tooth$Length, 0.90)

# 27.3


quantile(tooth$Length, 0.95)

# 29.57


quantile(tooth$Length, 0.99)

# 33.07

###################################################################################


# Check Central Limit Theorum with 5 samples of 10 observations each.. 

ind = sample(5, nrow(tooth), replace = TRUE, prob = c(0.2, 0.2, 0.2, 0.2, 0.2))

s1 = tooth[ind == 1,]
s2 = tooth[ind == 2,]
s3 = tooth[ind == 3,]
s4 = tooth[ind == 4,]
s5 = tooth[ind == 5,]


# Actual Mean

mean(tooth$Length)
# [1] 18.81333



# Samples Mean from only 3 sets

mean(s1$Length)
# [1] 20.64286

mean(s4$Length)
# [1] 21.48182

mean(s5$Length)
# [1] 17.9

## Mean of 3 Samples mean

# (20.6428 + 21.4181 + 17.9) / 3
# [1] 19.98697

###################################################################################



# Distribution - Normal.

mean(tooth$Length)

sd(tooth$Length)

# Find the probability of Leangth of less than 10

qnorm(10, 18.81333, 7.649, lower.tail = FALSE)   # 2.6 % 
# If more than use qnorm()


# Find the probability of Leanght between 12 and 15

dnorm(15, 18.8133, 7.649) - dnorm(12, 18.8133, 7.649)


# 90% tooth leanth is less than. 





###################################################################################



# Variance, Correlation, Co-variance, Standerd Deviation

var(tooth$Length)
# [1] 58.51202

var(tooth$Dose)
# [1] 0.3954802

cor(tooth$Length, tooth$Dose)
# [1] 0.8026913    {Strong positive correlation}

cov(tooth$Length, tooth$Dose)
# [1] 3.861299     {Positive covariance}





# Normalization & Standerdization

# Fourier Transformation




# ***** SAMPLE 1 and 2
# ____________________



# Create Hypothes 1

# H0: Smaple 1 and Sample 2 tooth length are equal
# Ha: Sample 1 and Sample 2 tooth length are not equal 

# Use two tailed t-test 

t.test(sample1$Length, sample2$Length)

# Based on p-value we failed to reject Null Hypothesis. 

# 95 percent confidence interval:
# -7.5710156  0.1710156
# On lower end and upper end change.. 

# Right Sample Size:

delta = mean(sample1$Length) - mean(sample2$Length)

sd(sample1$Length)
sd(sample2$Length)

avgSD = (((30-1)*(8.266^2)+(30-1)*(6.605^2))/(30+30-2))^0.5

power.t.test(n = 30, delta = delta, sd = avgSD, sig.level = 0.05, type = 'two.sample')

# power is 0.4695

power.t.test(power = 0.4695, delta = delta, sd = avgSD, sig.level = 0.05, type = 'two.sample')

# Right sie of sample is 30. As mean is very close. 





# Creat Hypothesis 2

# H0: Tooth Leangth No change  with power of dose
# Ha: Toth Length Changes with to power of dose  

# Use Analysis of CoVariance - ANCOVA






