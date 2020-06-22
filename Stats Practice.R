

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

ind = sample(5, nrow(tooth), replace = FALSE, prob = c(0.2, 0.2, 0.2, 0.2, 0.2))

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

(20.6428 + 21.4181 + 17.9) / 3
# [1] 19.98697

###################################################################################



# Distribution - Normal, Binomial?






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

# Creat Hypothesis

# T-test, Anaova, check p value (Check Standerd Error of mean in table)



