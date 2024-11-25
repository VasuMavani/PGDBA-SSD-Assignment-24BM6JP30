##### Univariate Analysis #####
### Overview of dataset ###
titanic = read.csv("D:/SSD Assignment/Titanic-Dataset.csv")
titanic <- na.omit(titanic)
titanic

#       Sex   Age SibSp Parch           Ticket     Fare       Cabin Embarked
# 1    male 22.00     1     0        A/5 21171   7.2500                    S
# 2  female 38.00     1     0         PC 17599  71.2833         C85        C
# 3  female 26.00     0     0 STON/O2. 3101282   7.9250                    S
# 4  female 35.00     1     0           113803  53.1000        C123        S
# 5    male 35.00     0     0           373450   8.0500                    S

### Summary of numerical columns ###
summary(titanic$Age)
summary(titanic$Fare)
summary(titanic$Parch)
summary(titanic$SibSp)

# Age             Fare              Parch               SibSp
# Min.   : 0.42   Min.   :  0.00    Min.    : 0.0000    Min.    : 0.0000
# 1st Qu.:20.12   1st Qu.:  8.05    1st Qu. : 0.0000    1st Qu. : 0.0000
# Median :28.00   Median : 15.74    Median  : 0.0000    Median  : 0.0000
# Mean   :29.70   Mean   : 34.69    Mean    : 0.4314    Mean    : 0.5126
# 3rd Qu.:38.00   3rd Qu.: 33.38    3rd Qu. : 1.0000    3rd Qu. : 1.0000
# Max.   :80.00   max.   : 512.33   Max.    : 6.0000    Max.    : 5.0000

### Distribution visualization ###
## Histogram of numerical columns ##
hist(titanic$Age)
hist(titanic$Fare)
hist(titanic$SibSp)
hist(titanic$Parch)

### Boxplot for numerical columns ###
boxplot(titanic$Age)
boxplot(titanic$Fare)
boxplot(titanic$Parch)
boxplot(titanic$SibSp)

### Categorical variable analysis ###
barplot(table(titanic$Survived),col = c("skyblue", "salmon"))
barplot(table(titanic$Pclass),col = c("skyblue", "salmon"))
barplot(table(titanic$Sex),col = c("skyblue", "salmon"))
barplot(table(titanic$SibSp),col = c("skyblue", "salmon"))
barplot(table(titanic$Parch),col = c("skyblue", "salmon"))

##### Multivariate Analysis #####
# Age, Fare, SibSp and Parch
cor(titanic$Age, titanic$Fare, method = "pearson", use = "complete.obs")
cor(titanic$Age, titanic$SibSp, method = "pearson", use = "complete.obs")
cor(titanic$Age, titanic$Parch, method = "pearson", use = "complete.obs")
cor(titanic$Fare, titanic$SibSp, method = "pearson", use = "complete.obs")
cor(titanic$Fare, titanic$Parch, method = "pearson", use = "complete.obs")
cor(titanic$SibSp, titanic$Parch, method = "pearson", use = "complete.obs")

### Scatter Plot Visualization ###
plot(titanic$Age, titanic$Fare,col=3)
abline(lm(Fare~Age,data=titanic),col='red')

### Numeric - Categorical variables ###
# Average Fare for each sex
avg_fare_by_sex <- aggregate(Fare ~ Sex, data = titanic, FUN = mean)
sex <- avg_fare_by_sex$Sex
avg_fare <- avg_fare_by_sex$Fare
barplot(avg_fare, names.arg = sex, col = c("skyblue", "lightgreen", "orange"), main = "Average Fare by Sex", xlab = "Sex", ylab = "Average Fare")

# Average Fare for each Embarked
avg_fare_by_embarked <- aggregate(Fare ~ Embarked, data = titanic, FUN = mean)
Embarked <- avg_fare_by_embarked$Embarked
avg_fare <- avg_fare_by_embarked$Fare
barplot(avg_fare, names.arg = Embarked, col = c("skyblue", "lightgreen", "orange"), main = "Average Fare by Embarked", xlab = "Embarked", ylab = "Average Fare")

# Average SibSp for each Embarked
avg_sibsp_by_embarked <- aggregate(SibSp ~ Embarked, data = titanic, FUN = mean)
Embarked <- avg_sibsp_by_embarked$Embarked
avg_sibsp <- avg_sibsp_by_embarked$SibSp
barplot(avg_sibsp, names.arg = Embarked, col = c("skyblue", "lightgreen", "orange"), main = "Average SibSp by Embarked", xlab = "Embarked", ylab = "Average SibSp")

# Average SibSp for each Sex
avg_sibsp_by_sex <- aggregate(SibSp ~ Sex, data = titanic, FUN = mean)
sex <- avg_sibsp_by_sex$Sex
avg_sibsp <- avg_sibsp_by_sex$SibSp
barplot(avg_sibsp, names.arg = sex, col = c("skyblue", "lightgreen", "orange"), main = "Average SibSp by Sex", xlab = "Sex", ylab = "Average SibSp")

# Average Parch for each sex
avg_parch_by_sex <- aggregate(Parch ~ Sex, data = titanic, FUN = mean)
sex <- avg_parch_by_sex$Sex
avg_parch <- avg_parch_by_sex$Parch
barplot(avg_parch, names.arg = sex, col = c("skyblue", "lightgreen", "orange"), main = "Average parch by sex", xlab = "sex", ylab = "Average Parch")

# Average Parch for each Embarked
avg_parch_by_embarked <- aggregate(Parch ~ Embarked, data = titanic, FUN = mean)
embarked <- avg_parch_by_embarked$Embarked
avg_parch <- avg_parch_by_embarked$Parch
barplot(avg_parch, names.arg = embarked, col = c("skyblue", "lightgreen", "orange"), main = "Average parch by embarked", xlab = "embarked", ylab = "Average Parch")

### Categorical - Categorical Analysis ###
table(titanic$Sex, titanic$Embarked)
table(titanic$Sex, titanic$SibSp)
table(titanic$Sex, titanic$Parch)
table(titanic$Embarked, titanic$SibSp)
table(titanic$Embarked, titanic$Parch)
table(titanic$SibSp, titanic$Parch)

### Multiple Regression ###
# One-hot encoding of variable
titanic$female <- ifelse(titanic$Sex == "female", 1, 0)
titanic$embarked_C <- ifelse(titanic$Embarked == "C", 1, 0)
titanic$embarked_q <- ifelse(titanic$Embarked == "Q", 1, 0)

# Creating model for Fare
model <- lm(Fare ~ Age + SibSp + Parch + female + embarked_C + embarked_q, data = titanic)

summary(model)

### Model Diagnostics ###
plot(model)

##### Advanced Analysis #####
### Principal Component Analysis ###
numerical_vars <- c("Age", "SibSp", "Parch", "Fare")
titanic_num <- titanic[, numerical_vars]

titanic.pc <- princomp(titanic_num, cor=T)
summary(titanic.pc,loadings=T)

plot(1:(length(titanic.pc$sdev)), (titanic.pc$sdev)^2, type='b', main="Scree Plot", xlab="Number of Components", ylab="Eigenvalue Size")

### Biplot ###
biplot(titanic.pc, xlim=c(-0.1,0.1), ylim=c(-0.1,0.1))
