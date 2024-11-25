##### Univariate Analysis #####
### Overview of dataset ###
airquality <- na.omit(airquality)
airquality

#     Ozone Solar.R Wind Temp Month Day
# 1      41     190  7.4   67     5   1
# 2      36     118  8.0   72     5   2
# 3      12     149 12.6   74     5   3
# 4      18     313 11.5   62     5   4
# 5      NA      NA 14.3   56     5   5

### Summary of numerical columns ###
summary(airquality$Ozone)
summary(airquality$Solar.R)
summary(airquality$Wind)
summary(airquality$Temp)

# Ozone           Solar.R           Wind             Temp       
# Min.   :  1.00   Min.   :  7.0  Min.   : 2.30   Min.   :57.00
# 1st Qu.: 18.00   1st Qu.:113.5  1st Qu.: 7.40   1st Qu.:71.00
# Median : 31.0   Median :207.0   Median : 9.70   Median :79.00
# Mean   : 42.1   Mean   :18489   Mean   : 9.94   Mean   :77.79
# 3rd Qu.: 62.0   3rd Qu.:255.5   3rd Qu.:11.50   3rd Qu.:84.50
# Max.   :168.0   Max.   :334.0   Max.   :20.70   Max.   :97.00

### Distribution visualization ###
### Histogram of numerical columns ###
hist(airquality$Ozone)
hist(airquality$Solar.R)
hist(airquality$Wind)
hist(airquality$Temp)

### Boxplot for numerical columns ###
boxplot(airquality$Ozone)
boxplot(airquality$Solar.R)
boxplot(airquality$Wind)
boxplot(airquality$Temp)

### Categorical variable analysis ###
barplot(table(airquality$Month),col = c("skyblue", "salmon"))
barplot(table(airquality$Day),col = c("skyblue", "salmon"))

##### Multivariate Analysis #####
### Correlation Analysis ###
cor(airquality$Ozone, airquality$Solar.R, method = "pearson", use = "complete.obs")
cor(airquality$Ozone, airquality$Wind, method = "pearson", use = "complete.obs")
cor(airquality$Ozone, airquality$Temp, method = "pearson", use = "complete.obs")
cor(airquality$Solar.R, airquality$Wind, method = "pearson", use = "complete.obs")
cor(airquality$Solar.R, airquality$Temp, method = "pearson", use = "complete.obs")
cor(airquality$Wind, airquality$Temp, method = "pearson", use = "complete.obs")

### Scatter plot visualization ###
plot(airquality$Ozone, airquality$Solar.R,col=3)
abline(lm(Solar.R~Ozone,data=airquality),col='red')

plot(airquality$Ozone, airquality$Wind,col=3)
abline(lm(Wind~Ozone,data=airquality),col='red')

plot(airquality$Ozone, airquality$Temp,col=3)
abline(lm(Temp~Ozone,data=airquality),col='red')

plot(airquality$Solar.R, airquality$Wind,col=3)
abline(lm(Wind~Solar.R,data=airquality),col='red')

plot(airquality$Solar.R, airquality$Temp,col=3)
abline(lm(Temp~Solar.R,data=airquality),col='red')

plot(airquality$Wind, airquality$Temp,col=3)
abline(lm(Temp~Wind,data=airquality),col='red')

### Categorical - Numerical Analysis ###
# Average Ozone for each month
avg_ozone_by_month <- aggregate(Ozone ~ Month, data = airquality, FUN = mean)
plot(avg_ozone_by_month$Month, avg_ozone_by_month$Ozone, type = "b", xlab = "Month", ylab = "Average Ozone", main = "Average Ozone by Month")

# Average Solar.R for each month
avg_solar_by_month <- aggregate(Solar.R ~ Month, data = airquality, FUN = mean)
plot(avg_solar_by_month$Month, avg_solar_by_month$Solar.R, type = "b", xlab = "Month", ylab = "Average Solar Radiation", main = "Average Solar Radiation by Month")

# Average Wind for each month
avg_wind_by_month <- aggregate(Wind ~ Month, data = airquality, FUN = mean)
plot(avg_wind_by_month$Month, avg_wind_by_month$Wind, type = "b", xlab = "Month", ylab = "Average Wind", main = "Average Wind by Month")

# Average temperature for each month
avg_temp_by_month <- aggregate(Temp ~ Month, data = airquality, FUN = mean)
plot(avg_temp_by_month$Month, avg_temp_by_month$Temp, type = "b", xlab = "Month", ylab = "Average Temperature", main = "Average Temperature by Month")

### Multiple Regression ###
# One-hot encoding for Month column
airquality$May <- ifelse(airquality$Month == 5, 1, 0)
airquality$June <- ifelse(airquality$Month == 6, 1, 0)
airquality$July <- ifelse(airquality$Month == 7, 1, 0)
airquality$August <- ifelse(airquality$Month == 8, 1, 0)

# Creating model for Temp
model <- lm(Temp ~ Ozone + Solar.R + Wind + May + June + July + August, data = airquality)

summary(model)

### Model Diagnostics ###
plot(model)

##### Advanced Analysis #####
### Principal Component Analysis ###
numerical_vars <- c("Ozone", "Solar.R", "Wind", "Temp")
airquality_num <- airquality[, numerical_vars]

airquality.pc <- princomp(airquality_num, cor=T)
summary(airquality.pc,loadings=T)

plot(1:(length(airquality.pc$sdev)), (airquality.pc$sdev)^2, type='b', main="Scree Plot", xlab="Number of Components", ylab="Eigenvalue Size")

### Biplot ###
biplot(airquality.pc, xlim=c(-0.1,0.1), ylim=c(-0.1,0.1))