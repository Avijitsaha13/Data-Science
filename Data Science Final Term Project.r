dataset <- read.csv("E:/Data Science Final Project/wine-quality-white-and-red.csv",na.strings=c(("")),header= TRUE, sep = ",")
dataset




install.packages("dplyr")
library(dplyr)


hist(dataset$fixed_acidity, main = "fixed_acidity Distribution", xlab = "fixed_acidity", ylab="Frequency" ,col = "lightblue")
hist(dataset$volatile_acidity, main = "volatile_acidity Distribution", xlab = "volatile_acidity", ylab="Frequency" ,col = "lightgreen")
hist(dataset$citric_acid, main = "citric_acid Distribution", xlab = "citric_acid", ylab="Frequency" ,col = "lightyellow")
hist(dataset$residual_sugar, main = "residual_sugar Distribution", xlab = "residual_sugar", ylab="Frequency" ,col = "lightpink")
hist(dataset$chlorides, main = "chlorides Distribution", xlab = "chlorides", ylab="Frequency" ,col = "blue")
hist(dataset$free_sulfur_dioxide, main = "free_sulfur_dioxide Distribution", xlab = "free_sulfur_dioxide", ylab="Frequency" ,col = "orange")
hist(dataset$total_sulfur_dioxide, main = "total_sulfur_dioxide Distribution", xlab = "total_sulfur_dioxide", ylab="Frequency" ,col = "red")
hist(dataset$density, main = "density Distribution", xlab = "density", ylab="Frequency" ,col = "wheat")
hist(dataset$pH, main = "pH Distribution", xlab = "pH", ylab="Frequency" ,col = "purple")
hist(dataset$sulphates, main = "sulphates Distribution", xlab = "sulphates", ylab="Frequency" ,col = "violet")
hist(dataset$alcohol, main = "alcohol Distribution", xlab = "alcohol", ylab="Frequency" ,col = "darkgreen")
hist(dataset$quality, main = "quality Distribution", xlab = "quality", ylab="Frequency" ,col = "hotpink")







install.packages("ggplot2")
library(ggplot2)
ggplot(dataset, aes(x = fixed_acidity)) +geom_density() +labs(title = "Density Plot of fixed_acidity",x = "fixed_acidity", y = "Density")




install.packages("moments")
library(moments)


fixed_acidity <- dataset$fixed_acidity
print(skewness(fixed_acidity))
hist(fixed_acidity,main = "fixed_acidity Distribution", xlab = "fixed_acidity", ylab="Frequency" ,col = "lightblue" )

(mean(fixed_acidity))
(median(fixed_acidity))

fixed_acidityMode <- (names(sort(table(dataset$fixed_acidity[!is.na(dataset$fixed_acidity)]), decreasing = TRUE)[1]))
print(fixed_acidityMode)






ggplot(dataset, aes(x = volatile_acidity)) +geom_density() +labs(title = "Density Plot of volatile_acidity",x = "volatile_acidity", y = "Density")


volatile_acidity <- dataset$volatile_acidity
print(skewness(volatile_acidity))
hist(volatile_acidity,main = "volatile_acidity Distribution", xlab = "volatile_acidity", ylab="Frequency" ,col = "lightgreen" )

(mean(volatile_acidity))
(median(volatile_acidity))

volatile_acidityMode <- (names(sort(table(dataset$volatile_acidity[!is.na(dataset$volatile_acidity)]), decreasing = TRUE)[1]))
print(volatile_acidityMode)






ggplot(dataset, aes(x = citric_acid)) +geom_density() +labs(title = "Density Plot of citric_acid",x = "citric_acid", y = "Density")


citric_acid <- dataset$citric_acid
print(skewness(citric_acid))
hist(citric_acid,main = "citric_acid Distribution", xlab = "citric_acid", ylab="Frequency" ,col = "pink" )

(mean(citric_acid))
(median(citric_acid))

citric_acidMode <- (names(sort(table(dataset$citric_acid[!is.na(dataset$citric_acid)]), decreasing = TRUE)[1]))
print(citric_acidMode)






ggplot(dataset, aes(x = residual_sugar)) +geom_density() +labs(title = "Density Plot of residual_sugar",x = "residual_sugar", y = "Density")


residual_sugar <- dataset$residual_sugar
print(skewness(residual_sugar))
hist(residual_sugar,main = "residual_sugar Distribution", xlab = "residual_sugar", ylab="Frequency" ,col = "red" )

(mean(residual_sugar))
(median(residual_sugar))

residual_sugarMode <- (names(sort(table(dataset$residual_sugar[!is.na(dataset$residual_sugar)]), decreasing = TRUE)[1]))
print(residual_sugarMode)






ggplot(dataset, aes(x = chlorides)) +geom_density() +labs(title = "Density Plot of chlorides",x = "chlorides", y = "Density")


chlorides <- dataset$chlorides
print(skewness(chlorides))
hist(chlorides,main = "chlorides Distribution", xlab = "chlorides", ylab="Frequency" ,col = "violet" )

(mean(chlorides))
(median(chlorides))

chloridesMode <- (names(sort(table(dataset$chlorides[!is.na(dataset$chlorides)]), decreasing = TRUE)[1]))
print(chloridesMode)






ggplot(dataset, aes(x = free_sulfur_dioxide)) +geom_density() +labs(title = "Density Plot of free_sulfur_dioxide",x = "free_sulfur_dioxide", y = "Density")


free_sulfur_dioxide <- dataset$free_sulfur_dioxide
print(skewness(free_sulfur_dioxide))
hist(free_sulfur_dioxide,main = "free_sulfur_dioxide", xlab = "free_sulfur_dioxide", ylab="Frequency" ,col = "yellow" )

(mean(free_sulfur_dioxide))
(median(free_sulfur_dioxide))

free_sulfur_dioxideMode <- (names(sort(table(dataset$free_sulfur_dioxide[!is.na(dataset$free_sulfur_dioxide)]), decreasing = TRUE)[1]))
print(free_sulfur_dioxideMode)




ggplot(dataset, aes(x = total_sulfur_dioxide)) +geom_density() +labs(title = "Density Plot of total_sulfur_dioxide",x = "total_sulfur_dioxide", y = "Density")


total_sulfur_dioxide <- dataset$total_sulfur_dioxide
print(skewness(total_sulfur_dioxide))
hist(total_sulfur_dioxide,main = "total_sulfur_dioxide", xlab = "total_sulfur_dioxide", ylab="Frequency" ,col = "orange" )

(mean(total_sulfur_dioxide))
(median(total_sulfur_dioxide))

total_sulfur_dioxideMode <- (names(sort(table(dataset$total_sulfur_dioxide[!is.na(dataset$total_sulfur_dioxide)]), decreasing = TRUE)[1]))
print(total_sulfur_dioxideMode)





ggplot(dataset, aes(x = density)) +geom_density() +labs(title = "Density Plot of density",x = "density", y = "Density")


density <- dataset$density
print(skewness(density))
hist(density,main = "density", xlab = "density", ylab="Frequency" ,col = "royalblue" )






ggplot(dataset, aes(x = pH)) +geom_density() +labs(title = "Density Plot of pH",x = "pH", y = "Density")


pH <- dataset$pH
print(skewness(pH))
hist(pH,main = "pH", xlab = "pH", ylab="Frequency" ,col = "purple" )

(mean(pH))
(median(pH))

pHMode <- (names(sort(table(dataset$pH[!is.na(dataset$pH)]), decreasing = TRUE)[1]))
print(pHMode)






ggplot(dataset, aes(x = sulphates)) +geom_density() +labs(title = "Density Plot of sulphates",x = "sulphates", y = "Density")


sulphates <- dataset$sulphates
print(skewness(sulphates))
hist(sulphates,main = "sulphates", xlab = "sulphates", ylab="Frequency" ,col = "lavender" )

(mean(sulphates))
(median(sulphates))

sulphatesMode <- (names(sort(table(dataset$sulphates[!is.na(dataset$sulphates)]), decreasing = TRUE)[1]))
print(sulphatesMode)







ggplot(dataset, aes(x = alcohol)) +geom_density() +labs(title = "Density Plot of alcohol",x = "alcohol", y = "Density")


alcohol <- dataset$alcohol
print(skewness(alcohol))
hist(alcohol,main = "alcohol", xlab = "alcohol", ylab="Frequency" ,col = "seagreen" )

(mean(alcohol))
(median(alcohol))

alcoholMode <- (names(sort(table(dataset$alcohol[!is.na(dataset$alcohol)]), decreasing = TRUE)[1]))
print(alcoholMode)








ggplot(dataset, aes(x = quality)) +geom_density() +labs(title = "Density Plot of quality",x = "quality", y = "Density")


quality <- dataset$quality
print(skewness(quality))
hist(quality,main = "quality", xlab = "quality", ylab="Frequency" ,col = "hotpink" )

(mean(quality))
(median(quality))

qualityMode <- (names(sort(table(dataset$quality[!is.na(dataset$quality)]), decreasing = TRUE)[1]))
print(qualityMode)










barplot(table(dataset$type), main="type Distribution", xlab="type", ylab="Frequency",col = "gold")
barplot(table(dataset$fixed_acidity), main="fixed_acidity Distribution", xlab="fixed_acidity", ylab="Frequency",col = "orange")
barplot(table(dataset$volatile_acidity), main="volatile_acidity Distribution", xlab="volatile_acidity", ylab="Frequency",col = "cyan")
barplot(table(dataset$citric_acid), main="citric_acid Distribution", xlab="citric_acid", ylab="Frequency",col = "blue")
barplot(table(dataset$residual_sugar), main="residual_sugar Distribution", xlab="residual_sugar", ylab="Frequency",col = "pink")
barplot(table(dataset$chlorides), main="chlorides Distribution", xlab="chlorides", ylab="Frequency",col = "yellow")
barplot(table(dataset$free_sulfur_dioxide), main="free_sulfur_dioxide Distribution", xlab="free_sulfur_dioxide", ylab="Frequency",col = "green")
barplot(table(dataset$total_sulfur_dioxide), main="total_sulfur_dioxide Distribution", xlab="total_sulfur_dioxide", ylab="Frequency",col = "violet")
barplot(table(dataset$density), main="density Distribution", xlab="density", ylab="Frequency",col = "purple")
barplot(table(dataset$pH), main="pH Distribution", xlab="pH", ylab="Frequency",col = "lightblue")
barplot(table(dataset$sulphates), main="sulphates Distribution", xlab="sulphates", ylab="Frequency",col = "red")
barplot(table(dataset$alcohol), main="alcohol Distribution", xlab="alcohol", ylab="Frequency",col = "lightgreen")
barplot(table(dataset$quality), main="quality Distribution", xlab="quality", ylab="Frequency",col = "darkblue")







install.packages("ggplot2")
library(ggplot2)





free_sulfur_dioxideBoxplot <- boxplot(dataset$free_sulfur_dioxide,main="free_sulfur_dioxide Distribution ", ylab="free_sulfur_dioxide",col="lightgreen") 
outliers <- free_sulfur_dioxideBoxplot$out 
cat("Outliers are", outliers) 
free_sulfur_dioxideMean <- mean(dataset$free_sulfur_dioxide, na.rm = TRUE) 
outlierPositions <- match(outliers, dataset$free_sulfur_dioxide) 
dataset$free_sulfur_dioxide[outlierPositions] <- as.integer (free_sulfur_dioxideMean)
dataset





total_sulfur_dioxideBoxplot <- boxplot(dataset$total_sulfur_dioxide,main="total_sulfur_dioxide Distribution ", ylab="total_sulfur_dioxide",col="lightblue") 
outliers <- total_sulfur_dioxideBoxplot$out 
cat("Outliers are", outliers) 
total_sulfur_dioxideMean <- mean(dataset$total_sulfur_dioxide, na.rm = TRUE) 
outlierPositions <- match(outliers, dataset$total_sulfur_dioxide) 
dataset$total_sulfur_dioxide[outlierPositions] <- as.integer (total_sulfur_dioxideMean)
dataset












quality_categories <- cut(dataset$quality, breaks = c(0, 3, 6, 8, Inf), labels = c("Poor", "Average", "Good", "Excellent"))
dataset$quality <- quality_categories
dataset


ggplot(dataset, aes(x = quality, y = volatile_acidity)) +
  geom_violin(fill = "skyblue") +
  stat_summary(fun=median, geom="point", shape=18, size=3, color="black", fill="black") + 
  labs(x = "quality", y = "volatile_acidity") +
  ggtitle("Violin Plot of quality vs volatile_acidity")








quality_categories <- cut(dataset$quality, breaks = c(0, 3, 6, 8, Inf), labels = c("Poor", "Average", "Good", "Excellent"))
dataset$quality <- quality_categories
dataset


ggplot(dataset, aes(x = quality, y = alcohol)) +
  geom_violin(fill = "lightpink") +
  stat_summary(fun=median, geom="point", shape=18, size=3, color="black", fill="black") + 
  labs(x = "quality", y = "alcohol") +
  ggtitle("Violin Plot of quality vs alcohol")






quality_categories <- cut(dataset$quality, breaks = c(0, 3, 6, 8, Inf), labels = c("Poor", "Average", "Good", "Excellent"))
dataset$quality <- quality_categories
dataset


ggplot(dataset, aes(x = quality, y = citric_acid)) +
  geom_violin(fill = "lavender") +
  stat_summary(fun=median, geom="point", shape=18, size=3, color="black", fill="black") + 
  labs(x = "quality", y = "citric_acid") +
  ggtitle("Violin Plot of quality vs citric_acid")






quality_categories <- cut(dataset$quality, breaks = c(0, 3, 6, 8, Inf), labels = c("Poor", "Average", "Good", "Excellent"))
dataset$quality <- quality_categories
dataset


ggplot(dataset, aes(x = quality, y = residual_sugar)) +
  geom_violin(fill = "violet") +
  stat_summary(fun=median, geom="point", shape=18, size=3, color="black", fill="black") + 
  labs(x = "quality", y = "residual_sugar") +
  ggtitle("Violin Plot of quality vs residual_sugar")







quality_categories <- cut(dataset$quality, breaks = c(0, 3, 6, 8, Inf), labels = c("Poor", "Average", "Good", "Excellent"))
dataset$quality <- quality_categories
dataset


ggplot(dataset, aes(x = quality, y = free_sulfur_dioxide)) +
  geom_violin(fill = "lightgreen") +
  stat_summary(fun=median, geom="point", shape=18, size=3, color="black", fill="black") + 
  labs(x = "quality", y = "free_sulfur_dioxide") +
  ggtitle("Violin Plot of quality vs free_sulfur_ddioxide")





quality_categories <- cut(dataset$quality, breaks = c(0, 3, 6, 8, Inf), labels = c("Poor", "Average", "Good", "Excellent"))
dataset$quality <- quality_categories
dataset


ggplot(dataset, aes(x = quality, y = density)) +
  geom_violin(fill = "salmon") +
  stat_summary(fun=median, geom="point", shape=18, size=3, color="black", fill="black") + 
  labs(x = "quality", y = "density") +
  ggtitle("Violin Plot of quality vs density")















ggplot(dataset, aes(x = fixed_acidity, y = volatile_acidity, color = type)) +
  geom_point() +
  labs(x = "fixed_acidity", y = "volatile_acidity", color = "type") +
  ggtitle("Scatter Plot of fixed_acidity vs volatile_acidity")




ggplot(dataset, aes(x = citric_acid, y = residual_sugar, color = type)) +
  geom_point() +
  labs(x = "citric_acid", y = "residual_sugar", color = "type") +
  ggtitle("Scatter Plot of citric_acid vs residual_sugar")




ggplot(dataset, aes(x = free_sulfur_dioxide, y = total_sulfur_dioxide, color = type)) +
  geom_point() +
  labs(x = "free_sulfur_dioxide", y = "total_sulfur_dioxide", color = "type") +
  ggtitle("Scatter Plot of free_sulfur_dioxide vs total_sulfur_dioxide")





ggplot(dataset, aes(x = density, y = pH, color = type)) +
  geom_point() +
  labs(x = "density", y = "pH", color = "type") +
  ggtitle("Scatter Plot of density vs pH")





ggplot(dataset, aes(x = sulphates, y = alcohol, color = type)) +
  geom_point() +
  labs(x = "sulphates", y = "alcohol", color = "type") +
  ggtitle("Scatter Plot of sulphates vs alcohol")






ggplot(dataset, aes(x = residual_sugar, y = density, color = type)) +
  geom_point() +
  labs(x = "residual_sugar", y = "density", color = "type") +
  ggtitle("Scatter Plot of residual_sugar vs density")






ggplot(dataset, aes(x = alcohol, y = density, color = type)) +
  geom_point() +
  labs(x = "alcohol", y = "density", color = "type") +
  ggtitle("Scatter Plot of alcohol vs density")






ggplot(dataset, aes(x = free_sulfur_dioxide, y = sulphates, color = type)) +
  geom_point() +
  labs(x = "free_sulfur_dioxide", y = "sulphates", color = "type") +
  ggtitle("Scatter Plot of free_sulfur_dioxide vs sulphates")

















install.packages("fmsb")
library(fmsb)
install.packages("dplyr")
library(dplyr)




white_alcohol_quality <- dataset %>%
  filter(type == "white") %>%
  group_by(quality) %>%
  summarise(alcohol_mean = mean(alcohol))

red_alcohol_quality <- dataset %>%
  filter(type == "red") %>%
  group_by(quality) %>%
  summarise(alcohol_mean = mean(alcohol))

transform_data <- function(df) {
  df <- as.data.frame(t(df))
  names(df) <- paste('Quality', df[1,], sep = '-')
  df <- df[-1,]
  max_value <- max(df)  
  min_value <- min(df)  
  alcohol_ref <- max_value + 0.1 * (max_value - min_value)
  baseline_ref <- min_value - 0.1 * (max_value - min_value)
  df <- rbind(rep(alcohol_ref, ncol(df)), rep(baseline_ref, ncol(df)), df)
  return(df)
}

white_alcohol_quality_chart <- transform_data(white_alcohol_quality)
red_alcohol_quality_chart <- transform_data(red_alcohol_quality)
par(mfrow=c(1,2))  
radarchart(white_alcohol_quality_chart, axistype = 1, calcex = 0.8, vlcex = 0.9,
           title = "Percentage of alcohol by quality level (White Wine)")
radarchart(red_alcohol_quality_chart, axistype = 1, calcex = 0.8, vlcex = 0.9,
           title = "Percentage of alcohol by quality level (Red Wine)")












white_residual_sugar <- dataset %>%
  filter(type == "white") %>%
  group_by(quality) %>%
  summarise(residual_sugar_mean = mean(residual_sugar))

red_residual_sugar <- dataset %>%
  filter(type == "red") %>%
  group_by(quality) %>%
  summarise(residual_sugar_mean = mean(residual_sugar))

transform_data <- function(df) {
  df <- as.data.frame(t(df))
  names(df) <- paste('Quality', df[1,], sep = '-')
  df <- df[-1,]
  max_value <- max(df)  
  min_value <- min(df)
  sugar_ref <- max_value + 0.1 * (max_value - min_value)
  baseline_ref <- min_value - 0.1 * (max_value - min_value)
  df <- rbind(rep(sugar_ref, ncol(df)), rep(baseline_ref, ncol(df)), df)
  return(df)
}

white_residual_sugar_chart <- transform_data(white_residual_sugar)
red_residual_sugar_chart <- transform_data(red_residual_sugar)

par(mfrow=c(1,2))

radarchart(white_residual_sugar_chart, axistype = 1, calcex = 0.8, vlcex = 0.9,
           title = "Residual Sugar Content by Quality Level (White Wine)", col = "blue")
radarchart(red_residual_sugar_chart, axistype = 1, calcex = 0.8, vlcex = 0.9,
           title = "Residual Sugar Content by Quality Level (Red Wine)", col = "red")










white_volatile_acidity <- dataset %>%
  filter(type == "white") %>%
  group_by(quality) %>%
  summarise(volatile_acidity_mean = mean(volatile_acidity))

red_volatile_acidity <- dataset %>%
  filter(type == "red") %>%
  group_by(quality) %>%
  summarise(volatile_acidity_mean = mean(volatile_acidity))

transform_data <- function(df) {
  df <- as.data.frame(t(df))
  names(df) <- paste('Quality', df[1,], sep = '-')
  df <- df[-1,]
  max_value <- max(df)  
  min_value <- min(df)
  acidity_ref <- max_value + 0.1 * (max_value - min_value)
  baseline_ref <- min_value - 0.1 * (max_value - min_value)
  df <- rbind(rep(acidity_ref, ncol(df)), rep(baseline_ref, ncol(df)), df)
  return(df)
}

white_volatile_acidity_chart <- transform_data(white_volatile_acidity)
red_volatile_acidity_chart <- transform_data(red_volatile_acidity)

par(mfrow=c(1,2))  

radarchart(white_volatile_acidity_chart, axistype = 1, calcex = 0.8, vlcex = 0.9,
           title = "Volatile Acidity Content by Quality Level (White Wine)", col = "blue")

radarchart(red_volatile_acidity_chart, axistype = 1, calcex = 0.8, vlcex = 0.9,
           title = "Volatile Acidity Content by Quality Level (Red Wine)", col = "red")











white_chlorides <- dataset %>%
  filter(type == "white") %>%
  group_by(quality) %>%
  summarise(chlorides_mean = mean(chlorides))

red_chlorides <- dataset %>%
  filter(type == "red") %>%
  group_by(quality) %>%
  summarise(chlorides_mean = mean(chlorides))

transform_data <- function(df) {
  df <- as.data.frame(t(df))
  names(df) <- paste('Quality', df[1,], sep = '-')
  df <- df[-1,]
  max_value <- max(df)  
  min_value <- min(df)
  chlorides_ref <- max_value + 0.1 * (max_value - min_value)
  baseline_ref <- min_value - 0.1 * (max_value - min_value)
  df <- rbind(rep(chlorides_ref, ncol(df)), rep(baseline_ref, ncol(df)), df)
  return(df)
}

white_chlorides_chart <- transform_data(white_chlorides)
red_chlorides_chart <- transform_data(red_chlorides)

par(mfrow=c(1,2))  

radarchart(white_chlorides_chart, axistype = 1, calcex = 0.8, vlcex = 0.9,
           title = "Chlorides Content by Quality Level (White Wine)", col = "blue")

radarchart(red_chlorides_chart, axistype = 1, calcex = 0.8, vlcex = 0.9,
           title = "Chlorides Content by Quality Level (Red Wine)", col = "red")








white_pH <- dataset %>%
  filter(type == "white") %>%
  group_by(quality) %>%
  summarise(pH_mean = mean(pH))

red_pH <- dataset %>%
  filter(type == "red") %>%
  group_by(quality) %>%
  summarise(pH_mean = mean(pH))

transform_data <- function(df) {
  df <- as.data.frame(t(df))
  names(df) <- paste('Quality', df[1,], sep = '-')
  df <- df[-1,]
  max_value <- max(df)  
  min_value <- min(df)
  pH_ref <- max_value + 0.1 * (max_value - min_value)
  baseline_ref <- min_value - 0.1 * (max_value - min_value)
  df <- rbind(rep(pH_ref, ncol(df)), rep(baseline_ref, ncol(df)), df)
  return(df)
}

white_pH_chart <- transform_data(white_pH)
red_pH_chart <- transform_data(red_pH)

par(mfrow=c(1,2))  

radarchart(white_pH_chart, axistype = 1, calcex = 0.8, vlcex = 0.9,
           title = "pH by Quality Level (White Wine)", col = "blue")

radarchart(red_pH_chart, axistype = 1, calcex = 0.8, vlcex = 0.9,
           title = "pH by Quality Level (Red Wine)", col = "red")























white_wine_data <- subset(dataset, type == "white")
mean_fixed_acidity_white <- aggregate(fixed_acidity ~ quality, data = white_wine_data, FUN = mean)
plot(mean_fixed_acidity_white$quality, mean_fixed_acidity_white$fixed_acidity, type = "l",
     xlab = "Quality", ylab = "Mean Fixed Acidity",
     main = "Mean Fixed Acidity vs. Quality for White and Red Wine",
     col = "blue", ylim = range(mean_fixed_acidity_white$fixed_acidity, mean_fixed_acidity_red$fixed_acidity))
points(mean_fixed_acidity_white$quality, mean_fixed_acidity_white$fixed_acidity, col = "blue", pch = 16)
red_wine_data <- subset(dataset, type == "red")
mean_fixed_acidity_red <- aggregate(fixed_acidity ~ quality, data = red_wine_data, FUN = mean)
lines(mean_fixed_acidity_red$quality, mean_fixed_acidity_red$fixed_acidity, type = "l", col = "red")
points(mean_fixed_acidity_red$quality, mean_fixed_acidity_red$fixed_acidity, col = "red", pch = 16)







white_wine_data <- subset(dataset, type == "white")
mean_volatile_acidity_white <- aggregate(volatile_acidity ~ quality, data = white_wine_data, FUN = mean)
red_wine_data <- subset(dataset, type == "red")
mean_volatile_acidity_red <- aggregate(volatile_acidity ~ quality, data = red_wine_data, FUN = mean)

ylim_range <- range(c(mean_volatile_acidity_white$volatile_acidity, mean_volatile_acidity_red$volatile_acidity))

plot(mean_volatile_acidity_white$quality, mean_volatile_acidity_white$volatile_acidity, 
     type = "l",
     xlab = "Quality", 
     ylab = "Mean Volatile Acidity",
     main = "Mean Volatile Acidity vs. Quality for White and Red Wine",
     col = "blue", 
     ylim = ylim_range)

points(mean_volatile_acidity_white$quality, mean_volatile_acidity_white$volatile_acidity, col = "blue", pch = 16)

lines(mean_volatile_acidity_red$quality, mean_volatile_acidity_red$volatile_acidity, type = "l", col = "red")
points(mean_volatile_acidity_red$quality, mean_volatile_acidity_red$volatile_acidity, col = "red", pch = 16)











white_wine_data <- subset(dataset, type == "white")
mean_citric_acid_white <- aggregate(citric_acid ~ quality, data = white_wine_data, FUN = mean)
red_wine_data <- subset(dataset, type == "red")
mean_citric_acid_red <- aggregate(citric_acid ~ quality, data = red_wine_data, FUN = mean)
ylim_range <- range(c(mean_citric_acid_white$citric_acid, mean_citric_acid_red$citric_acid))
plot(mean_citric_acid_white$quality, mean_citric_acid_white$citric_acid, 
     type = "l",
     xlab = "Quality", 
     ylab = "Mean Citric Acid",
     main = "Mean Citric Acid vs. Quality for White and Red Wine",
     col = "blue", 
     ylim = ylim_range)
points(mean_citric_acid_white$quality, mean_citric_acid_white$citric_acid, col = "blue", pch = 16)
lines(mean_citric_acid_red$quality, mean_citric_acid_red$citric_acid, type = "l", col = "red")
points(mean_citric_acid_red$quality, mean_citric_acid_red$citric_acid, col = "red", pch = 16)









white_wine_data <- subset(dataset, type == "white")
mean_residual_sugar_white <- aggregate(residual_sugar ~ quality, data = white_wine_data, FUN = mean)
red_wine_data <- subset(dataset, type == "red")
mean_residual_sugar_red <- aggregate(residual_sugar ~ quality, data = red_wine_data, FUN = mean)

ylim_range <- range(c(mean_residual_sugar_white$residual_sugar, mean_residual_sugar_red$residual_sugar))

plot(mean_residual_sugar_white$quality, mean_residual_sugar_white$residual_sugar, 
     type = "l",
     xlab = "Quality", 
     ylab = "Mean Residual Sugar",
     main = "Mean Residual Sugar vs. Quality for White and Red Wine",
     col = "blue", 
     ylim = ylim_range)

points(mean_residual_sugar_white$quality, mean_residual_sugar_white$residual_sugar, col = "blue", pch = 16)

lines(mean_residual_sugar_red$quality, mean_residual_sugar_red$residual_sugar, type = "l", col = "red")
points(mean_residual_sugar_red$quality, mean_residual_sugar_red$residual_sugar, col = "red", pch = 16)








white_wine_data <- subset(dataset, type == "white")
mean_chlorides_white <- aggregate(chlorides ~ quality, data = white_wine_data, FUN = mean)
red_wine_data <- subset(dataset, type == "red")
mean_chlorides_red <- aggregate(chlorides ~ quality, data = red_wine_data, FUN = mean)
ylim_range <- range(c(mean_chlorides_white$chlorides, mean_chlorides_red$chlorides))
plot(mean_chlorides_white$quality, mean_chlorides_white$chlorides, 
     type = "l",
     xlab = "Quality", 
     ylab = "Mean Chlorides",
     main = "Mean Chlorides vs. Quality for White and Red Wine",
     col = "blue", 
     ylim = ylim_range)
points(mean_chlorides_white$quality, mean_chlorides_white$chlorides, col = "blue", pch = 16)
lines(mean_chlorides_red$quality, mean_chlorides_red$chlorides, type = "l", col = "red")
points(mean_chlorides_red$quality, mean_chlorides_red$chlorides, col = "red", pch = 16)








white_wine_data <- subset(dataset, type == "white")
mean_free_sulfur_dioxide_white <- aggregate(free_sulfur_dioxide ~ quality, data = white_wine_data, FUN = mean)
red_wine_data <- subset(dataset, type == "red")
mean_free_sulfur_dioxide_red <- aggregate(free_sulfur_dioxide ~ quality, data = red_wine_data, FUN = mean)
ylim_range <- range(c(mean_free_sulfur_dioxide_white$free_sulfur_dioxide, mean_free_sulfur_dioxide_red$free_sulfur_dioxide))
plot(mean_free_sulfur_dioxide_white$quality, mean_free_sulfur_dioxide_white$free_sulfur_dioxide, 
     type = "l",
     xlab = "Quality", 
     ylab = "Mean Free Sulfur Dioxide",
     main = "Mean Free Sulfur Dioxide vs. Quality for White and Red Wine",
     col = "blue", 
     ylim = ylim_range)
points(mean_free_sulfur_dioxide_white$quality, mean_free_sulfur_dioxide_white$free_sulfur_dioxide, col = "blue", pch = 16)
lines(mean_free_sulfur_dioxide_red$quality, mean_free_sulfur_dioxide_red$free_sulfur_dioxide, type = "l", col = "red")
points(mean_free_sulfur_dioxide_red$quality, mean_free_sulfur_dioxide_red$free_sulfur_dioxide, col = "red", pch = 16)








white_wine_data <- subset(dataset, type == "white")
mean_total_sulfur_dioxide_white <- aggregate(total_sulfur_dioxide ~ quality, data = white_wine_data, FUN = mean)
red_wine_data <- subset(dataset, type == "red")
mean_total_sulfur_dioxide_red <- aggregate(total_sulfur_dioxide ~ quality, data = red_wine_data, FUN = mean)
ylim_range <- range(c(mean_total_sulfur_dioxide_white$total_sulfur_dioxide, mean_total_sulfur_dioxide_red$total_sulfur_dioxide))
plot(mean_total_sulfur_dioxide_white$quality, mean_total_sulfur_dioxide_white$total_sulfur_dioxide, 
     type = "l",
     xlab = "Quality", 
     ylab = "Mean Total Sulfur Dioxide",
     main = "Mean Total Sulfur Dioxide vs. Quality for White and Red Wine",
     col = "blue", 
     ylim = ylim_range)
points(mean_total_sulfur_dioxide_white$quality, mean_total_sulfur_dioxide_white$total_sulfur_dioxide, col = "blue", pch = 16)
lines(mean_total_sulfur_dioxide_red$quality, mean_total_sulfur_dioxide_red$total_sulfur_dioxide, type = "l", col = "red")
points(mean_total_sulfur_dioxide_red$quality, mean_total_sulfur_dioxide_red$total_sulfur_dioxide, col = "red", pch = 16)










white_wine_data <- subset(dataset, type == "white")
mean_density_white <- aggregate(density ~ quality, data = white_wine_data, FUN = mean)
red_wine_data <- subset(dataset, type == "red")
mean_density_red <- aggregate(density ~ quality, data = red_wine_data, FUN = mean)
ylim_range <- range(c(mean_density_white$density, mean_density_red$density))
plot(mean_density_white$quality, mean_density_white$density, 
     type = "l",
     xlab = "Quality", 
     ylab = "Mean Density",
     main = "Mean Density vs. Quality for White and Red Wine",
     col = "blue", 
     ylim = ylim_range)
points(mean_density_white$quality, mean_density_white$density, col = "blue", pch = 16)
lines(mean_density_red$quality, mean_density_red$density, type = "l", col = "red")
points(mean_density_red$quality, mean_density_red$density, col = "red", pch = 16)









white_wine_data <- subset(dataset, type == "white")
mean_pH_white <- aggregate(pH ~ quality, data = white_wine_data, FUN = mean)
red_wine_data <- subset(dataset, type == "red")
mean_pH_red <- aggregate(pH ~ quality, data = red_wine_data, FUN = mean)
ylim_range <- range(c(mean_pH_white$pH, mean_pH_red$pH))
plot(mean_pH_white$quality, mean_pH_white$pH, 
     type = "l",
     xlab = "Quality", 
     ylab = "Mean pH",
     main = "Mean pH vs. Quality for White and Red Wine",
     col = "blue", 
     ylim = ylim_range)
points(mean_pH_white$quality, mean_pH_white$pH, col = "blue", pch = 16)
lines(mean_pH_red$quality, mean_pH_red$pH, type = "l", col = "red")
points(mean_pH_red$quality, mean_pH_red$pH, col = "red", pch = 16)










white_wine_data <- subset(dataset, type == "white")
mean_sulphates_white <- aggregate(sulphates ~ quality, data = white_wine_data, FUN = mean)
red_wine_data <- subset(dataset, type == "red")
mean_sulphates_red <- aggregate(sulphates ~ quality, data = red_wine_data, FUN = mean)
ylim_range <- range(c(mean_sulphates_white$sulphates, mean_sulphates_red$sulphates))
plot(mean_sulphates_white$quality, mean_sulphates_white$sulphates, 
     type = "l",
     xlab = "Quality", 
     ylab = "Mean Sulphates",
     main = "Mean Sulphates vs. Quality for White and Red Wine",
     col = "blue", 
     ylim = ylim_range)
points(mean_sulphates_white$quality, mean_sulphates_white$sulphates, col = "blue", pch = 16)
lines(mean_sulphates_red$quality, mean_sulphates_red$sulphates, type = "l", col = "red")
points(mean_sulphates_red$quality, mean_sulphates_red$sulphates, col = "red", pch = 16)








white_wine_data <- subset(dataset, type == "white")
mean_alcohol <- aggregate(alcohol ~ quality, data = white_wine_data, FUN = mean)
plot(mean_alcohol$quality, mean_alcohol$alcohol, type = "l",
     xlab = "Quality", ylab = "Mean Alcohol Content",
     main = "Mean Alcohol Content vs. Quality for White and Red Wine",
     col = "blue")
points(mean_alcohol$quality, mean_alcohol$alcohol, col = "blue", pch = 16)
red_wine_data <- subset(dataset, type == "red")
mean_alcohol_red <- aggregate(alcohol ~ quality, data = red_wine_data, FUN = mean)
lines(mean_alcohol_red$quality, mean_alcohol_red$alcohol, type = "l", col = "red")
points(mean_alcohol_red$quality, mean_alcohol_red$alcohol, col = "red", pch = 16)








