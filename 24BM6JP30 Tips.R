##### Univariate Analysis ######
### Overview of dataset ###
tips = read.csv("D:/SSD Assignment/tips.csv")
tips <- na.omit(tips)
tips

#     total_bill  tip     sex smoker  day time    size
# 1       16.99   1.01 Female     No  Sun Dinner    2
# 2       10.34   1.66   Male     No  Sun Dinner    3
# 3       21.01   3.50   Male     No  Sun Dinner    3
# 4       23.68   3.31   Male     No  Sun Dinner    2
# 5       24.59   3.61 Female     No  Sun Dinner    4

### Summary of numerical columns ###
summary(tips$total_bill)
summary(tips$tip)
summary(tips$size)

# total_bill         tip            size  
# Min.   : 3.07   Min.   : 1.000   Min.   : 1.00
# 1st Qu.:13.35   1st Qu.: 2.000   1st Qu.: 2.00
# Median :17.80   Median : 2.900   Median : 2.00
# Mean   :19.79   Mean   : 2.998   Mean   : 2.57
# 3rd Qu.:24.13   3rd Qu.: 3.562   3rd Qu.: 3.00
# Max.   :50.81   Max.   :10.000   Max.   : 6.00

### Distribution visualization ###
### Histogram of numerical columns ###
hist(tips$total_bill)
hist(tips$tip)
hist(tips$size)


### Boxplot for numerical columns ###
boxplot(tips$total_bill)
boxplot(tips$tip)
boxplot(tips$size)

### Categorical variable analysis ###
barplot(table(tips$sex),col = c("skyblue", "salmon"))
barplot(table(tips$smoker),col = c("skyblue", "salmon"))
barplot(table(tips$day),col = c("skyblue", "salmon"))
barplot(table(tips$time),col = c("skyblue", "salmon"))
barplot(table(tips$size),col = c("skyblue", "salmon"))

##### Multivariate Analysis #####
# total_bill, tip and size
cor(tips$total_bill, tips$tip, method = "pearson", use = "complete.obs")
cor(tips$total_bill, tips$size, method = "pearson", use = "complete.obs")
cor(tips$tip, tips$size, method = "pearson", use = "complete.obs")

### Scatter Plot Visualization ###
plot(tips$total_bill, tips$tip,col=3)
abline(lm(tip~total_bill,data=tips),col='red') 

plot(tips$total_bill, tips$size,col=3)
abline(lm(size~total_bill,data=tips),col='red')

plot(tips$tip, tips$size,col=3)
abline(lm(size~tip,data=tips),col='red')

### Categorical - Numerical Analysis ###
# Average total_bill for each sex
avg_total_bill_by_sex <- aggregate(total_bill ~ sex, data = tips, FUN = mean)
sex <- avg_total_bill_by_sex$sex
avg_total_bill <- avg_total_bill_by_sex$total_bill
barplot(avg_total_bill, names.arg = sex, col = c("skyblue", "lightgreen", "orange"), main = "Average Total Bill by sex", xlab = "sex", ylab = "Average Total Bill")

# Average total_bill for each smoker v/s non-smoker
avg_total_bill_by_smoker <- aggregate(total_bill ~ smoker, data = tips, FUN = mean)
smoker <- avg_total_bill_by_smoker$smoker
avg_total_bill <- avg_total_bill_by_smoker$total_bill
barplot(avg_total_bill, names.arg = smoker, col = c("skyblue", "lightgreen", "orange"), main = "Average Total Bill by smoker", xlab = "smoker", ylab = "Average Total Bill")

# Average total_bill for each day
avg_total_bill_by_day <- aggregate(total_bill ~ day, data = tips, FUN = mean)
day <- avg_total_bill_by_day$day
avg_total_bill <- avg_total_bill_by_day$total_bill
barplot(avg_total_bill, names.arg = day, col = c("skyblue", "lightgreen", "orange"), main = "Average Total Bill by day", xlab = "day", ylab = "Average Total Bill")

# Average tip for each day
avg_tip_by_day <- aggregate(tip ~ day, data = tips, FUN = mean)
day <- avg_tip_by_day$day
avg_tip <- avg_tip_by_day$tip
barplot(avg_tip, names.arg = day, col = c("skyblue", "lightgreen", "orange"), main = "Average Tip by day", xlab = "day", ylab = "Average Tip")

# Average tip for each sex
avg_tip_by_sex <- aggregate(tip ~ sex, data = tips, FUN = mean)
sex <- avg_tip_by_sex$sex
avg_tip <- avg_tip_by_sex$tip
barplot(avg_tip, names.arg = sex, col = c("skyblue", "lightgreen", "orange"), main = "Average Tip by sex", xlab = "sex", ylab = "Average Tip")

# Average tip for each smoker v/s non-smoker
avg_tip_by_smoker <- aggregate(tip ~ smoker, data = tips, FUN = mean)
smoker <- avg_tip_by_smoker$smoker
avg_tip <- avg_tip_by_smoker$tip
barplot(avg_tip, names.arg = smoker, col = c("skyblue", "lightgreen", "orange"), main = "Average Tip by smoker", xlab = "smoker", ylab = "Average Tip")

# Average size for each smoker v/s non-smoker
avg_size_by_smoker <- aggregate(size ~ smoker, data = tips, FUN = mean)
smoker <- avg_size_by_smoker$smoker
avg_size <- avg_size_by_smoker$size
barplot(avg_size, names.arg = smoker, col = c("skyblue", "lightgreen", "orange"), main = "Average Size by smoker", xlab = "smoker", ylab = "Average Size")

# Average size for each sex
avg_size_by_sex <- aggregate(size ~ sex, data = tips, FUN = mean)
sex <- avg_size_by_sex$sex
avg_size <- avg_size_by_sex$size
barplot(avg_size, names.arg = sex, col = c("skyblue", "lightgreen", "orange"), main = "Average Size by sex", xlab = "sex", ylab = "Average Size")

# Average size for each day
avg_size_by_day <- aggregate(size ~ day, data = tips, FUN = mean)
day <- avg_size_by_day$day
avg_size <- avg_size_by_day$size
barplot(avg_size, names.arg = day, col = c("skyblue", "lightgreen", "orange"), main = "Average Size by day", xlab = "day", ylab = "Average Size")

### Categorical - Categorical Analysis ###
table(tips$sex, tips$smoker)
table(tips$sex, tips$day)
table(tips$day, tips$smoker)

### Multiple Regression ###
# One-hot encoding of variable
tips$Female <- ifelse(tips$sex == "Female", 1, 0)
tips$smoker <- ifelse(tips$smoker == "Yes", 1, 0)
tips$dinner <- ifelse(tips$time == "Dinner", 1, 0)
tips$sunday <- ifelse(tips$day == "Sun", 1, 0)
tips$saturday <- ifelse(tips$day == "Sat", 1, 0)
tips$thursday <- ifelse(tips$day == "Thur", 1, 0)

# Creating model for tip
model <- lm(tip ~ total_bill + size + Female + smoker + dinner + sunday + saturday + thursday, data = tips)

summary(model)

### Model Diagnostics ###
plot(model)

##### Advanced Analysis #####
### Principal Component Analysis ###
numerical_vars <- c("total_bill", "tip", "size")
tips_num <- tips[, numerical_vars]

tips.pc <- princomp(tips_num, cor=T)
summary(tips.pc,loadings=T)

plot(1:(length(tips.pc$sdev)), (tips.pc$sdev)^2, type='b', main="Scree Plot", xlab="Number of Components", ylab="Eigenvalue Size")

### Biplot ###
biplot(airquality.pc, xlim=c(-0.1,0.1), ylim=c(-0.1,0.1))