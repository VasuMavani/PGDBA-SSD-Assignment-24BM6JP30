##### Univariate Analysis #####
### Overview of dataset ###
penguin = read.csv("D:/SSD Assignment/penguins.csv")
penguin <- na.omit(penguin)
penguin

#     species    island bill_length_mm bill_depth_mm flipper_length_mm body_mass_g    sex
# 1    Adelie Torgersen           39.1          18.7               181        3750   MALE
# 2    Adelie Torgersen           39.5          17.4               186        3800 FEMALE
# 3    Adelie Torgersen           40.3          18.0               195        3250 FEMALE
# 4    Adelie Torgersen           36.7          19.3               193        3450 FEMALE
# 5    Adelie Torgersen           39.3          20.6               190        3650   MALE

### Summary of numerical columns ###
summary(penguin$bill_length_mm)
summary(penguin$bill_depth_mm)
summary(penguin$flipper_length_mm)
summary(penguin$body_mass_g)

# bill_length_mm  bill_depth_mm   flipper_length_mm  body_mass_g           
# Min.   :32.10   Min.   :13.10   Min.   : 172.0     Min.   :2700       
# 1st Qu.:39.23   1st Qu.:15.60   1st Qu.: 190.0     1st Qu.:3550  
# Median :44.45   Median :17.30   Median : 197.0     Median :4050  
# Mean   :43.92   Mean   :17.15   Mean   : 200.9     Mean   :4202                     
# 3rd Qu.:48.50   3rd Qu.:18.70   3rd Qu.: 213.0     3rd Qu.:4750                     
# Max.   :59.60   Max.   :21.50   Max.   : 231.0     Max.   :6300                     

### Distribution visualization ###
### Histogram of numerical columns ###
hist(penguin$bill_length_mm)
hist(penguin$bill_depth_mm)
hist(penguin$flipper_length_mm)
hist(penguin$body_mass_g)

### Boxplot for numerical columns ###
boxplot(penguin$bill_length_mm)
boxplot(penguin$bill_depth_mm)
boxplot(penguin$flipper_length_mm)
boxplot(penguin$body_mass_g)

### Categorical variable analysis ###
barplot(table(penguin$species),col = c("skyblue", "salmon"))
barplot(table(penguin$island),col = c("skyblue", "salmon"))
barplot(table(penguin$sex),col = c("skyblue", "salmon"))

##### Multivariate Analysis #####
cor(penguin$bill_length_mm, penguin$bill_depth_mm, method = "pearson", use = "complete.obs")
cor(penguin$bill_length_mm, penguin$flipper_length_mm, method = "pearson", use = "complete.obs")
cor(penguin$bill_length_mm, penguin$body_mass_g, method = "pearson", use = "complete.obs")
cor(penguin$bill_depth_mm, penguin$flipper_length_mm, method = "pearson", use = "complete.obs")
cor(penguin$bill_depth_mm, penguin$body_mass_g, method = "pearson", use = "complete.obs")
cor(penguin$flipper_length_mm, penguin$body_mass_g, method = "pearson", use = "complete.obs")

### Scatter plot visualization ###
plot(penguin$bill_length_mm, penguin$bill_depth_mm,col=3)
abline(lm(bill_depth_mm~bill_length_mm,data=penguin),col='red')

plot(penguin$bill_length_mm, penguin$flipper_length_mm,col=3)
abline(lm(flipper_length_mm~bill_length_mm,data=penguin),col='red')

plot(penguin$bill_length_mm, penguin$body_mass_g,col=3)
abline(lm(body_mass_g~bill_length_mm,data=penguin),col='red')

plot(penguin$bill_depth_mm, penguin$flipper_length_mm,col=3)
abline(lm(flipper_length_mm~bill_depth_mm,data=penguin),col='red')

plot(penguin$bill_depth_mm, penguin$body_mass_g,col=3)
abline(lm(body_mass_g~bill_depth_mm,data=penguin),col='red')

plot(penguin$flipper_length_mm, penguin$body_mass_g,col=3)
abline(lm(body_mass_g~flipper_length_mm,data=penguin),col='red')

### Categorical - Numerical Analysis ###
# Average bill depth for each species
avg_bill_depth_by_species <- aggregate(bill_depth_mm ~ species, data = penguin, FUN = mean)
species <- avg_bill_depth_by_species$species
avg_bill_depth <- avg_bill_depth_by_species$bill_depth_mm
barplot(avg_bill_depth, names.arg = species, col = c("skyblue", "lightgreen", "orange"), main = "Average Bill Depth by Species", xlab = "Species", ylab = "Average Bill Depth (mm)")

# Average bill depth for each island
avg_bill_depth_by_island <- aggregate(bill_depth_mm ~ island, data = penguin, FUN = mean)
island <- avg_bill_depth_by_island$island
avg_bill_depth <- avg_bill_depth_by_island$bill_depth_mm
barplot(avg_bill_depth, names.arg = island, col = c("skyblue", "lightgreen", "orange"), main = "Average Bill Depth by Island", xlab = "Island", ylab = "Average Bill Depth (mm)")

# Average bill depth for each sex
avg_bill_depth_by_sex <- aggregate(bill_depth_mm ~ sex, data = penguin, FUN = mean)
sex <- avg_bill_depth_by_sex$sex
avg_bill_depth <- avg_bill_depth_by_sex$bill_depth_mm
barplot(avg_bill_depth, names.arg = sex, col = c("skyblue", "lightgreen"), main = "Average Bill Depth by Sex", xlab = "Sex", ylab = "Average Bill Depth (mm)")

# Average bill length for each sex
avg_bill_length_by_sex <- aggregate(bill_length_mm ~ sex, data = penguin, FUN = mean)
sex <- avg_bill_length_by_sex$sex
avg_bill_length <- avg_bill_length_by_sex$bill_length_mm
barplot(avg_bill_length, names.arg = sex, col = c("skyblue", "lightgreen"), main = "Average Bill Length by Species", xlab = "Species", ylab = "Average Bill Depth (mm)")

# Average bill length for each island
avg_bill_length_by_island <- aggregate(bill_length_mm ~ island, data = penguin, FUN = mean)
island <- avg_bill_length_by_island$island
avg_bill_length <- avg_bill_length_by_island$bill_length_mm
barplot(avg_bill_length, names.arg = island, col = c("skyblue", "lightgreen", "orange"), main = "Average Bill Length by Island", xlab = "Island", ylab = "Average Bill Length (mm)")

# Average bill length for each species
avg_bill_length_by_species <- aggregate(bill_length_mm ~ species, data = penguin, FUN = mean)
species <- avg_bill_length_by_species$species
avg_bill_length <- avg_bill_length_by_species$bill_length_mm
barplot(avg_bill_length, names.arg = species, col = c("skyblue", "lightgreen", "orange"), main = "Average Bill Length by Species", xlab = "Species", ylab = "Average Bill Length (mm)")

# Average flipper length for each species
avg_flipper_length_by_species <- aggregate(flipper_length_mm ~ species, data = penguin, FUN = mean)
species <- avg_flipper_length_by_species$species
avg_flipper_length <- avg_flipper_length_by_species$flipper_length_mm
barplot(avg_flipper_length, names.arg = species, col = c("skyblue", "lightgreen", "orange"), main = "Average Flipper Length by Species", xlab = "Species", ylab = "Average Flipper Length (mm)")

# Average flipper length for each island
avg_flipper_length_by_island <- aggregate(flipper_length_mm ~ island, data = penguin, FUN = mean)
island <- avg_flipper_length_by_island$island
avg_flipper_length <- avg_flipper_length_by_island$flipper_length_mm
barplot(avg_flipper_length, names.arg = island, col = c("skyblue", "lightgreen", "orange"), main = "Average Flipper Length by island", xlab = "island", ylab = "Average Flipper Length (mm)")

# Average flipper length for each sex
avg_flipper_length_by_sex <- aggregate(flipper_length_mm ~ sex, data = penguin, FUN = mean)
sex <- avg_flipper_length_by_sex$sex
avg_flipper_length <- avg_flipper_length_by_sex$flipper_length_mm
barplot(avg_flipper_length, names.arg = sex, col = c("skyblue", "lightgreen"), main = "Average Flipper Length by sex", xlab = "sex", ylab = "Average Flipper Length (mm)")

# Average body_mass_g for each sex
avg_flipper_body_mass_by_sex <- aggregate(body_mass_g ~ sex, data = penguin, FUN = mean)
sex <- avg_flipper_body_mass_by_sex$sex
avg_flipper_body_mass <- avg_flipper_body_mass_by_sex$body_mass_g
barplot(avg_flipper_body_mass, names.arg = sex, col = c("skyblue", "lightgreen"), main = "Average Body Mass by sex", xlab = "sex", ylab = "Average Body Mass (g)")

# Average body_mass_g for each island
avg_flipper_body_mass_by_island <- aggregate(body_mass_g ~ island, data = penguin, FUN = mean)
island <- avg_flipper_body_mass_by_island$island
avg_flipper_body_mass <- avg_flipper_body_mass_by_island$body_mass_g
barplot(avg_flipper_body_mass, names.arg = island, col = c("skyblue", "lightgreen", "orange"), main = "Average Body Mass by island", xlab = "island", ylab = "Average Body Mass (g)")

# Average body_mass_g for each species
avg_flipper_body_mass_by_species <- aggregate(body_mass_g ~ species, data = penguin, FUN = mean)
species <- avg_flipper_body_mass_by_species$species
avg_flipper_body_mass <- avg_flipper_body_mass_by_species$body_mass_g
barplot(avg_flipper_body_mass, names.arg = species, col = c("skyblue", "lightgreen", "orange"), main = "Average Body Mass by species", xlab = "species", ylab = "Average Body Mass (g)")

### Categorical - Categorical Analysis ###
table(penguin$species, penguin$island)
table(penguin$species, penguin$sex)
table(penguin$island, penguin$sex)

### Multiple Regression ###
# One-hot encoding for species, island and sex variable
penguin$Adelie <- ifelse(penguin$species == "Adelie", 1, 0)
penguin$Chinstrap <- ifelse(penguin$species == "Chinstrap", 1, 0)
penguin$Torgersen <- ifelse(penguin$island == "Torgersen", 1, 0)
penguin$Dream <- ifelse(penguin$island == "Dream", 1, 0)
penguin$FEMALE <- ifelse(penguin$sex == "FEMALE", 1, 0)

# Creating model for body_mass)g
model <- lm(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm + Adelie + Chinstrap + Torgersen + Dream + FEMALE, data = penguin)

summary(model)

### Model Diagnostics ###
plot(model)

##### Advanced Analysis #####
### Principal Component Analysis ###
numerical_vars <- c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")
penguin_num <- penguin[, numerical_vars]

penguin.pc <- princomp(penguin_num, cor=T)
summary(penguin.pc,loadings=T)

plot(1:(length(penguin.pc$sdev)), (penguin.pc$sdev)^2, type='b', main="Scree Plot", xlab="Number of Components", ylab="Eigenvalue Size")

### Biplot ###
biplot(penguin.pc, xlim=c(-0.1,0.1), ylim=c(-0.1,0.1))