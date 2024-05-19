dataset <- read.csv("E:/Data Science Midterm Project/Dataset_midterm_Section(B).csv",na.strings=c(("")),header= TRUE, sep = ",")
dataset


names (dataset)


str(dataset)


summary(dataset)

install.packages("dplyr")
library(dplyr)


dataset %>% summarise_if(is.numeric,sd)





ageStandardDeviation = sd(dataset$age, na.rm = TRUE)
cat("Standard Deviation of age: ", ageStandardDeviation)


creatinine_phosphokinaseStandardDeviation = sd(dataset$creatinine_phosphokinase, na.rm = TRUE)
cat("Standard Deviation of creatinine_phosphokinase: ", creatinine_phosphokinaseStandardDeviation)


ejection_fractionStandardDeviation = sd(dataset$ejection_fraction, na.rm = TRUE)
cat("Standard Deviation of ejection_fraction: ", ejection_fractionStandardDeviation)


plateletsStandardDeviation = sd(dataset$platelets, na.rm = TRUE)
cat("Standard Deviation of platelets: ", plateletsStandardDeviation)


serum_creatinineStandardDeviation = sd(dataset$serum_creatinine, na.rm = TRUE)
cat("Standard Deviation of serum_creatinine: ", serum_creatinineStandardDeviation)


serum_sodiumStandardDeviation = sd(dataset$serum_sodium, na.rm = TRUE)
cat("Standard Deviation of serum_sodium: ", serum_sodiumStandardDeviation)


timeStandardDeviation = sd(dataset$time, na.rm = TRUE)
cat("Standard Deviation of time: ", timeStandardDeviation)






ageVariance = var(dataset$age, na.rm = TRUE) 
cat("Variance of Age: ", ageVariance) 


creatinine_phosphokinaseVariance = var(dataset$creatinine_phosphokinase, na.rm = TRUE) 
cat("Variance of Creatinine_phosphokinase: ", creatinine_phosphokinaseVariance) 


ejection_fractionVariance = var(dataset$ejection_fraction, na.rm = TRUE) 
cat("Variance of Ejection_fraction: ", ejection_fractionVariance) 


plateletsVariance = var(dataset$platelets, na.rm = TRUE) 
cat("Variance of Platelets: ", plateletsVariance) 


serum_creatinineVariance = var(dataset$serum_creatinine, na.rm = TRUE) 
cat("Variance of Serum_creatinine: ", serum_creatinineVariance) 


serum_sodiumVariance = var(dataset$serum_sodium, na.rm = TRUE) 
cat("Variance of Serum_sodium: ", serum_sodiumVariance) 


timeVariance = var(dataset$time, na.rm = TRUE) 
cat("Variance of Time: ", timeVariance) 








hist(dataset$age, main = "Age Distribution", xlab = "Age", ylab="Frequency" ,col = "lightblue")

hist(dataset$anaemia, main = "Anaemia Distribution", xlab = "Anaemia", ylab="Frequency" ,col = "lightgreen")

hist(dataset$diabetes, main = "Diabetes Distribution", xlab = "Diabetes", ylab="Frequency" ,col = "lightyellow")

hist(dataset$ejection_fraction, main = "Ejection_fraction Distribution", xlab = "Ejection_fraction", ylab="Frequency" ,col = "lightpink")

hist(dataset$high_blood_pressure, main = "High_blood_pressure Distribution", xlab = "High_blood_pressure", ylab="Frequency" ,col = "orange")

hist(dataset$platelets, main = "Platelets Distribution", xlab = "Platelets", ylab="Frequency" ,col = "blue")

hist(dataset$serum_creatinine, main = "Serum_creatinine Distribution", xlab = "Serum_creatinine", ylab="Frequency" ,col = "purple")

hist(dataset$serum_sodium, main = "Serum_sodium Distribution", xlab = "Serum_sodium", ylab="Frequency" ,col = "brown")

hist(dataset$smoking, main = "Smoking Distribution", xlab = "Smoking", ylab="Frequency" ,col = "wheat")

hist(dataset$time, main = "Time Distribution", xlab = "Time", ylab="Frequency" ,col = "tan")

hist(dataset$DEATH_EVENT, main = "DEATH_EVENT Distribution", xlab = "DEATH_EVENT", ylab="Frequency" ,col = "lavender")






barplot(table(dataset$age), main="Age Distribution", xlab="Age", ylab="Frequency",col = "gold")

barplot(table(dataset$anaemia), main="Anaemia Distribution", xlab="Anaemia", ylab="Frequency",col = "violet")

barplot(table(dataset$diabetes), main="Diabetes Distribution", xlab="Diabetes", ylab="Frequency",col = "green")

barplot(table(dataset$ejection_fraction), main="Ejection_fraction Distribution", xlab="Ejection_fraction", ylab="Frequency",col = "salmon")

barplot(table(dataset$high_blood_pressure), main="High_blood_pressure Distribution", xlab="High_blood_pressure", ylab="Frequency",col = "blue")

barplot(table(dataset$platelets), main="Platelets Distribution", xlab="Platelets", ylab="Frequency",col = "skyblue")

barplot(table(dataset$serum_creatinine), main="Serum_creatinine Distribution", xlab="Serum_creatinine", ylab="Frequency",col = "red")

barplot(table(dataset$serum_sodium), main="serum_sodium Distribution", xlab="serum_sodium", ylab="Frequency",col = "darkgreen")

barplot(table(dataset$sex), main="Sex Distribution", xlab="Sex", ylab="Frequency",col = "darkblue")

barplot(table(dataset$smoking), main="Smoking Distribution", xlab="Smoking", ylab="Frequency",col = "lightyellow")

barplot(table(dataset$time), main="Time Distribution", xlab="Time", ylab="Frequency",col = "lightpink")

barplot(table(dataset$DEATH_EVENT), main="DEATH_EVENT Distribution", xlab="DEATH_EVENT", ylab="Frequency",col = "lavender")



is.na(dataset)
sum(is.na(dataset))

colSums(is.na(dataset))

which(is.na(dataset$age))

which(is.na(dataset$anaemia))

which(is.na(dataset$creatinine_phosphokinase))

which(is.na(dataset$diabetes))

which(is.na(dataset$ejection_fraction))

which(is.na(dataset$high_blood_pressure))

which(is.na(dataset$platelets))

which(is.na(dataset$serum_creatinine))

which(is.na(dataset$serum_sodium))

which(is.na(dataset$sex))

which(is.na(dataset$smoking))

which(is.na(dataset$time))

which(is.na(dataset$DEATH_EVENT))




dataset1 <- na.omit(dataset) 
dataset1 

missingValues <- which(is.na(dataset$age))
ageMode <- names(sort(table(dataset$age[!is.na(dataset$age)]), decreasing = TRUE)[1])
dataset$age[missingValues] <- ageMode
cat("Age Mode:", ageMode) 
dataset$age
which(is.na(dataset$age))


missingValues <- which(is.na(dataset$anaemia))
anaemiaMode <- names(sort(table(dataset$anaemia[!is.na(dataset$anaemia)]), decreasing = TRUE)[1])
dataset$anaemia[missingValues] <- anaemiaMode
cat("Anaemia Mode:", anaemiaMode) 
dataset$anaemia


missingValues <- which(is.na(dataset$creatinine_phosphokinase))
creatinine_phosphokinaseMode <- names(sort(table(dataset$creatinine_phosphokinase[!is.na(dataset$creatinine_phosphokinase)]), decreasing = TRUE)[1])
dataset$creatinine_phosphokinase[missingValues] <- creatinine_phosphokinaseMode
cat("Creatinine_phosphokinase Mode:", creatinine_phosphokinaseMode) 
dataset$creatinine_phosphokinase


missingValues <- which(is.na(dataset$diabetes))
diabetesMode <- names(sort(table(dataset$diabetes[!is.na(dataset$diabetes)]), decreasing = TRUE)[1])
dataset$diabetes[missingValues] <- diabetesMode
cat("Diabetes Mode:", diabetesMode) 
dataset$diabetes


missingValues <- which(is.na(dataset$ejection_fraction))
ejection_fractionMode <- names(sort(table(dataset$ejection_fraction[!is.na(dataset$ejection_fraction)]), decreasing = TRUE)[1])
dataset$ejection_fraction[missingValues] <- ejection_fractionMode
cat("Ejection_fraction Mode:", ejection_fractionMode) 
dataset$ejection_fraction


missingValues <- which(is.na(dataset$high_blood_pressure))
high_blood_pressureMode <- names(sort(table(dataset$high_blood_pressure[!is.na(dataset$high_blood_pressure)]), decreasing = TRUE)[1])
dataset$high_blood_pressure[missingValues] <- high_blood_pressureMode
cat("High_blood_pressure Mode:", high_blood_pressureMode) 
dataset$high_blood_pressure


missingValues <- which(is.na(dataset$serum_creatinine))
serum_creatinineMode <- names(sort(table(dataset$serum_creatinine[!is.na(dataset$serum_creatinine)]), decreasing = TRUE)[1])
dataset$serum_creatinine[missingValues] <- serum_creatinineMode
cat("Serum_creatinine Mode:", serum_creatinineMode) 
dataset$serum_creatinine


missingValues <- which(is.na(dataset$serum_sodium))
serum_sodiumMode <- names(sort(table(dataset$serum_sodium[!is.na(dataset$serum_sodium)]), decreasing = TRUE)[1])
dataset$serum_sodium[missingValues] <- serum_sodiumMode
cat("Serum_sodium Mode:", serum_sodiumMode) 
dataset$serum_sodium


missingValues <- which(is.na(dataset$sex))
sexMode <- names(sort(table(dataset$sex[!is.na(dataset$sex)]), decreasing = TRUE)[1])
dataset$sex[missingValues] <- sexMode
cat("Sex Mode:", sexMode) 
dataset$sex


missingValues <- which(is.na(dataset$smoking))
smokingMode <- names(sort(table(dataset$smoking[!is.na(dataset$smoking)]), decreasing = TRUE)[1])
dataset$smoking[missingValues] <- smokingMode
cat("Smoking Mode:", smokingMode) 
dataset$smoking


missingValues <- which(is.na(dataset$time))
timeMode <- names(sort(table(dataset$time[!is.na(dataset$time)]), decreasing = TRUE)[1])
dataset$time[missingValues] <- timeMode
cat("Time Mode:", timeMode) 
dataset$time


missingValues <- which(is.na(dataset$DEATH_EVENT))
DEATH_EVENTMode <- names(sort(table(dataset$DEATH_EVENT[!is.na(dataset$DEATH_EVENT)]), decreasing = TRUE)[1])
dataset$DEATH_EVENT[missingValues] <- DEATH_EVENTMode
cat("DEATH_EVENT Mode:", DEATH_EVENTMode) 
dataset$DEATH_EVENT






missingValues <- which(is.na(dataset$age)) 
ageMean <- floor(mean(dataset$age, na.rm = TRUE)) 
dataset$age[missingValues] <- ageMean 
cat("Age Mean:", ageMean) 
dataset$age 

missingValues <- which(is.na(dataset$ejection_fraction)) 
ejection_fractionMean <- floor(mean(dataset$ejection_fraction, na.rm = TRUE)) 
dataset$ejection_fraction[missingValues] <- ejection_fractionMean 
cat("Ejection_fraction Mean:", ejection_fractionMean) 
dataset$ejection_fraction

missingValues <- which(is.na(dataset$platelets)) 
plateletsMean <- floor(mean(dataset$platelets, na.rm = TRUE)) 
dataset$platelets[missingValues] <- plateletsMean 
cat("Platelets Mean:", plateletsMean) 
dataset$platelets

missingValues <- which(is.na(dataset$serum_creatinine)) 
serum_creatinineMean <- floor(mean(dataset$serum_creatinine, na.rm = TRUE)) 
dataset$serum_creatinine[missingValues] <- serum_creatinineMean 
cat("Serum_creatinine:", serum_creatinineMean) 
dataset$serum_creatinine


missingValues <- which(is.na(dataset$serum_sodium)) 
serum_sodiumMean <- floor(mean(dataset$serum_sodium, na.rm = TRUE)) 
dataset$serum_sodium[missingValues] <- serum_sodiumMean 
cat("Serum_sodium:", serum_sodiumMean) 
dataset$serum_sodium


missingValues <- which(is.na(dataset$time)) 
timeMean <- floor(mean(dataset$time, na.rm = TRUE)) 
dataset$time[missingValues] <- timeMean 
cat("Time:", timeMean) 
dataset$time





missingValues <- which(is.na(dataset$age)) 
ageMedian <- floor(median(dataset$age, na.rm = TRUE)) 
dataset$age[missingValues] <- ageMedian 
cat("Age Median:", ageMedian) 
dataset$age

missingValues <- which(is.na(dataset$ejection_fraction)) 
ejection_fractionMedian <- floor(median(dataset$ejection_fraction, na.rm = TRUE)) 
dataset$ejection_fraction[missingValues] <- ejection_fractionMedian 
cat("Ejection_fraction:", ejection_fractionMedian) 
dataset$ejection_fraction

missingValues <- which(is.na(dataset$platelets)) 
plateletsMedian <- floor(median(dataset$platelets, na.rm = TRUE)) 
dataset$platelets[missingValues] <- plateletsMedian 
cat("Platelets:", plateletsMedian) 
dataset$platelets

missingValues <- which(is.na(dataset$serum_creatinine)) 
serum_creatinineMedian <- floor(median(dataset$serum_creatinine, na.rm = TRUE)) 
dataset$serum_creatinine[missingValues] <- serum_creatinineMedian 
cat("Serum_creatinine:", serum_creatinineMedian) 
dataset$serum_creatinine

missingValues <- which(is.na(dataset$serum_sodium)) 
serum_sodiumMedian <- floor(median(dataset$serum_sodium, na.rm = TRUE)) 
dataset$serum_sodium[missingValues] <- serum_sodiumMedian 
cat("Serum_sodium:", serum_sodiumMedian) 
dataset$serum_sodium


missingValues <- which(is.na(dataset$time)) 
timeMedian <- floor(median(dataset$time, na.rm = TRUE)) 
dataset$time[missingValues] <- timeMedian 
cat("Time:", timeMedian) 
dataset$time





install.packages("ggplot2")
library(ggplot2)

dataset$age_missingvalue <- ifelse(is.na(dataset$age), TRUE, FALSE)
missing_counts <- table(dataset$age_missingvalue)
ggplot(data = data.frame(Missing = names(missing_counts), Count = as.numeric(missing_counts)), aes(x = Missing, y = Count, fill = Missing)) +
geom_bar(stat = "identity") +
scale_fill_manual(values = c("blue", "red"), labels = c("Present", "Missing")) +
labs(title = "Missing Values for Age Attribute", x = "Missing Values", y = "Count") +
theme_minimal()


dataset$anaemia_missingvalue <- ifelse(is.na(dataset$anaemia), TRUE, FALSE)
missing_counts <- table(dataset$anaemia_missingvalue)
ggplot(data = data.frame(Missing = names(missing_counts), Count = as.numeric(missing_counts)), aes(x = Missing, y = Count, fill = Missing)) +
geom_bar(stat = "identity") +
scale_fill_manual(values = c("blue", "red"), labels = c("Present", "Missing")) +
labs(title = "Missing Values for Anaemia Attribute", x = "Missing Values", y = "Count") +
theme_minimal()


dataset$creatinine_phosphokinase_missingvalue <- ifelse(is.na(dataset$creatinine_phosphokinase), TRUE, FALSE)
missing_counts <- table(dataset$creatinine_phosphokinase_missingvalue)
ggplot(data = data.frame(Missing = names(missing_counts), Count = as.numeric(missing_counts)), aes(x = Missing, y = Count, fill = Missing)) +
geom_bar(stat = "identity") +
scale_fill_manual(values = c("blue", "red"), labels = c("Present", "Missing")) +
labs(title = "Missing Values for Creatinine_phosphokinase Attribute", x = "Missing Values", y = "Count") +
theme_minimal()



dataset$diabetes_missingvalue <- ifelse(is.na(dataset$diabetes), TRUE, FALSE)
missing_counts <- table(dataset$diabetes_missingvalue)
ggplot(data = data.frame(Missing = names(missing_counts), Count = as.numeric(missing_counts)), aes(x = Missing, y = Count, fill = Missing)) +
geom_bar(stat = "identity") +
scale_fill_manual(values = c("blue", "red"), labels = c("Present", "Missing")) +
labs(title = "Missing Values for Diabetes Attribute", x = "Missing Values", y = "Count") +
theme_minimal()


dataset$ejection_fraction_missingvalue <- ifelse(is.na(dataset$ejection_fraction), TRUE, FALSE)
missing_counts <- table(dataset$ejection_fraction_missingvalue)
ggplot(data = data.frame(Missing = names(missing_counts), Count = as.numeric(missing_counts)), aes(x = Missing, y = Count, fill = Missing)) +
geom_bar(stat = "identity") +
scale_fill_manual(values = c("blue", "red"), labels = c("Present", "Missing")) +
labs(title = "Missing Values for Ejection_fraction Attribute", x = "Missing Values", y = "Count") +
theme_minimal()


dataset$high_blood_pressure_missingvalue <- ifelse(is.na(dataset$high_blood_pressure), TRUE, FALSE)
missing_counts <- table(dataset$high_blood_pressure_missingvalue)
ggplot(data = data.frame(Missing = names(missing_counts), Count = as.numeric(missing_counts)), aes(x = Missing, y = Count, fill = Missing)) +
geom_bar(stat = "identity") +
scale_fill_manual(values = c("blue", "red"), labels = c("Present", "Missing")) +
labs(title = "Missing Values for High_blood_pressure Attribute", x = "Missing Values", y = "Count") +
theme_minimal()



dataset$platelets_missingvalue <- ifelse(is.na(dataset$platelets), TRUE, FALSE)
missing_counts <- table(dataset$platelets_missingvalue)
ggplot(data = data.frame(Missing = names(missing_counts), Count = as.numeric(missing_counts)), aes(x = Missing, y = Count, fill = Missing)) +
geom_bar(stat = "identity") +
scale_fill_manual(values = c("blue", "red"), labels = c("Present", "Missing")) +
labs(title = "Missing Values for Platelets Attribute", x = "Missing Values", y = "Count") +
theme_minimal()



dataset$serum_creatinine_missingvalue <- ifelse(is.na(dataset$serum_creatinine), TRUE, FALSE)
missing_counts <- table(dataset$serum_creatinine_missingvalue)
ggplot(data = data.frame(Missing = names(missing_counts), Count = as.numeric(missing_counts)), aes(x = Missing, y = Count, fill = Missing)) +
geom_bar(stat = "identity") +
scale_fill_manual(values = c("blue", "red"), labels = c("Present", "Missing")) +
labs(title = "Missing Values for Serum_creatinine Attribute", x = "Missing Values", y = "Count") +
theme_minimal()


dataset$serum_sodium_missingvalue <- ifelse(is.na(dataset$serum_sodium), TRUE, FALSE)
missing_counts <- table(dataset$serum_sodium_missingvalue)
ggplot(data = data.frame(Missing = names(missing_counts), Count = as.numeric(missing_counts)), aes(x = Missing, y = Count, fill = Missing)) +
geom_bar(stat = "identity") +
scale_fill_manual(values = c("blue", "red"), labels = c("Present", "Missing")) +
labs(title = "Missing Values for Serum_sodium Attribute", x = "Missing Values", y = "Count") +
theme_minimal()

dataset$sex_missingvalue <- ifelse(is.na(dataset$sex), TRUE, FALSE)
missing_counts <- table(dataset$sex_missingvalue)
ggplot(data = data.frame(Missing = names(missing_counts), Count = as.numeric(missing_counts)), aes(x = Missing, y = Count, fill = Missing)) +
geom_bar(stat = "identity") +
scale_fill_manual(values = c("blue", "red"), labels = c("Present", "Missing")) +
labs(title = "Missing Values for Sex Attribute", x = "Missing Values", y = "Count") +
theme_minimal()

dataset$smoking_missingvalue <- ifelse(is.na(dataset$smoking), TRUE, FALSE)
missing_counts <- table(dataset$smoking_missingvalue)
ggplot(data = data.frame(Missing = names(missing_counts), Count = as.numeric(missing_counts)), aes(x = Missing, y = Count, fill = Missing)) +
geom_bar(stat = "identity") +
scale_fill_manual(values = c("blue", "red"), labels = c("Present", "Missing")) +
labs(title = "Missing Values for Smoking Attribute", x = "Missing Values", y = "Count") +
theme_minimal()


dataset$time_missingvalue <- ifelse(is.na(dataset$time), TRUE, FALSE)
missing_counts <- table(dataset$time_missingvalue)
ggplot(data = data.frame(Missing = names(missing_counts), Count = as.numeric(missing_counts)), aes(x = Missing, y = Count, fill = Missing)) +
geom_bar(stat = "identity") +
scale_fill_manual(values = c("blue", "red"), labels = c("Present", "Missing")) +
labs(title = "Missing Values for Time Attribute", x = "Missing Values", y = "Count") +
theme_minimal()


dataset$DEATH_EVENT_missingvalue <- ifelse(is.na(dataset$DEATH_EVENT), TRUE, FALSE)
missing_counts <- table(dataset$DEATH_EVENT_missingvalue)
ggplot(data = data.frame(Missing = names(missing_counts), Count = as.numeric(missing_counts)), aes(x = Missing, y = Count, fill = Missing)) +
geom_bar(stat = "identity") +
scale_fill_manual(values = c("blue", "red"), labels = c("Present", "Missing")) +
labs(title = "Missing Values for DEATH_EVENT Attribute", x = "Missing Values", y = "Count") +
theme_minimal()





install.packages("ggplot2")
library(ggplot2)

missingValues <- which(is.na(dataset$age)) 
ageMean <- floor(mean(dataset$age, na.rm = TRUE)) 
dataset$age[missingValues] <- ageMean 
cat("Age Mean:", ageMean) 
dataset$age 
barplot(ageMean, main = "Mean Age", ylab = "Age", names.arg = "Mean", col = "skyblue", ylim = c(0, max(dataset$age) + 5))


missingValues <- which(is.na(dataset$ejection_fraction)) 
ejection_fractionMean <- floor(mean(dataset$ejection_fraction, na.rm = TRUE)) 
dataset$ejection_fraction[missingValues] <- ejection_fractionMean 
cat("Ejection_fraction Mean:", ejection_fractionMean) 
dataset$ejection_fraction 
barplot(ejection_fractionMean, main = "Mean Ejection_fraction", ylab = "Ejection_fraction", names.arg = "Mean", col = "lightgreen", ylim = c(0, max(dataset$ejection_fraction) + 5))


missingValues <- which(is.na(dataset$platelets)) 
plateletsMean <- floor(mean(dataset$platelets, na.rm = TRUE)) 
dataset$platelets[missingValues] <- plateletsMean 
cat("Platelets Mean:", plateletsMean) 
dataset$platelets 
barplot(plateletsMean, main = "Mean Platelets", ylab = "Platelets", names.arg = "Mean", col = "orange", ylim = c(0, max(dataset$platelets) + 5))


missingValues <- which(is.na(dataset$serum_creatinine)) 
serum_creatinineMean <- floor(mean(dataset$serum_creatinine, na.rm = TRUE)) 
dataset$serum_creatinine[missingValues] <- serum_creatinineMean 
cat("Serum_creatinine Mean:", serum_creatinineMean) 
dataset$serum_creatinine 
barplot(serum_creatinineMean, main = "Mean Serum_creatinine", ylab = "Serum_creatinine", names.arg = "Mean", col = "lightpink", ylim = c(0, max(dataset$serum_creatinine) + 5))


missingValues <- which(is.na(dataset$serum_sodium)) 
serum_sodiumMean <- floor(mean(dataset$serum_sodium, na.rm = TRUE)) 
dataset$serum_sodium[missingValues] <- serum_sodiumMean 
cat("Serum_sodium Mean:", serum_sodiumMean) 
dataset$serum_sodium 
barplot(serum_sodiumMean, main = "Mean Serum_sodium", ylab = "Serum_sodium", names.arg = "Mean", col = "lightyellow", ylim = c(0, max(dataset$serum_sodium) + 15))


missingValues <- which(is.na(dataset$time)) 
timeMean <- floor(mean(dataset$time, na.rm = TRUE)) 
dataset$time[missingValues] <- timeMean 
cat("Time Mean:", timeMean) 
dataset$time
barplot(timeMean, main = "Mean Time", ylab = "Time", names.arg = "Mean", col = "violet", ylim = c(0, max(dataset$time) + 5))






missingValues <- which(is.na(dataset$age))  
ageMedian <- floor(median(dataset$age, na.rm = TRUE))  
dataset$age[missingValues] <- ageMedian  
cat("Age Median:", ageMedian)  
dataset$age 
barplot(ageMedian, main = "Median Age", ylab = "Age", names.arg = "Median", col = "violet", ylim = c(0, max(dataset$age) + 5))


missingValues <- which(is.na(dataset$ejection_fraction))  
ejection_fractionMedian <- floor(median(dataset$ejection_fraction, na.rm = TRUE))  
dataset$ejection_fraction[missingValues] <- ejection_fractionMedian  
cat("Ejection_fraction Median:", ejection_fractionMedian)  
dataset$ejection_fraction 
barplot(ejection_fractionMedian, main = "Median Ejection_fraction", ylab = "Ejection_fraction", names.arg = "Median", col = "cyan", ylim = c(0, max(dataset$ejection_fraction) + 5))


missingValues <- which(is.na(dataset$platelets))  
plateletsMedian <- floor(median(dataset$platelets, na.rm = TRUE))  
dataset$platelets[missingValues] <- plateletsMedian  
cat("Platelets Median:", plateletsMedian)  
dataset$platelets 
barplot(plateletsMedian, main = "Median Platelets", ylab = "Platelets", names.arg = "Median", col = "navyblue", ylim = c(0, max(dataset$platelets) + 5))


missingValues <- which(is.na(dataset$serum_creatinine))  
serum_creatinineMedian <- floor(median(dataset$serum_creatinine, na.rm = TRUE))  
dataset$serum_creatinine[missingValues] <- serum_creatinineMedian  
cat("Serum_creatinine Median:", serum_creatinineMedian)  
dataset$serum_creatinine 
barplot(serum_creatinineMedian, main = "Median Serum_creatinine", ylab = "Serum_creatinine", names.arg = "Median", col = "grey", ylim = c(0, max(dataset$serum_creatinine) + 5))


missingValues <- which(is.na(dataset$serum_sodium))  
serum_sodiumMedian <- floor(median(dataset$serum_sodium, na.rm = TRUE))  
dataset$serum_sodium[missingValues] <- serum_sodiumMedian  
cat("Serum_sodium Median:", serum_sodiumMedian)  
dataset$serum_sodium 
barplot(serum_sodiumMedian, main = "Median Serum_sodium", ylab = "Serum_sodium", names.arg = "Median", col = "gold", ylim = c(0, max(dataset$serum_sodium) + 20))


missingValues <- which(is.na(dataset$time))  
timeMedian <- floor(median(dataset$time, na.rm = TRUE))  
dataset$time[missingValues] <- timeMedian  
cat("Time Median:", timeMedian)  
dataset$time 
barplot(timeMedian, main = "Median Time", ylab = "Time", names.arg = "Median", col = "salmon", ylim = c(0, max(dataset$time) + 5))






missingValues <- which(is.na(dataset$age))
ageMode <- names(sort(table(dataset$age[!is.na(dataset$age)]), decreasing = TRUE)[1])
dataset$age[missingValues] <- ageMode
cat("Age Mode:", ageMode)  
dataset$age
barplot(table(dataset$age), main = "Mode Age", xlab="Age", ylab = "Frequency",col = "violet")


missingValues <- which(is.na(dataset$anaemia))
anaemiaMode <- names(sort(table(dataset$anaemia[!is.na(dataset$anaemia)]), decreasing = TRUE)[1])
dataset$anaemia[missingValues] <- anaemiaMode
cat("Anaemia Mode:", anaemiaMode)  
dataset$anaemia
barplot(table(dataset$anaemia), main = "Mode Anaemia", xlab="Anaemia", ylab = "Frequency",col = "wheat")


missingValues <- which(is.na(dataset$diabetes))
diabetesMode <- names(sort(table(dataset$diabetes[!is.na(dataset$diabetes)]), decreasing = TRUE)[1])
dataset$diabetes[missingValues] <- diabetesMode
cat("Diabetes Mode:", diabetesMode)  
dataset$diabetes
barplot(table(dataset$diabetes), main = "Mode Diabetes", xlab="Diabetes", ylab = "Frequency",col = "chocolate")


missingValues <- which(is.na(dataset$ejection_fraction))
ejection_fractionMode <- names(sort(table(dataset$ejection_fraction[!is.na(dataset$ejection_fraction)]), decreasing = TRUE)[1])
dataset$ejection_fraction[missingValues] <- ejection_fractionMode
cat("Ejection_fraction Mode:", ejection_fractionMode)  
dataset$ejection_fraction
barplot(table(dataset$ejection_fraction), main = "Mode Ejection_fraction", xlab="Ejection_fraction", ylab = "Frequency",col = "tan")


missingValues <- which(is.na(dataset$high_blood_pressure))
high_blood_pressureMode <- names(sort(table(dataset$high_blood_pressure[!is.na(dataset$high_blood_pressure)]), decreasing = TRUE)[1])
dataset$high_blood_pressure[missingValues] <- high_blood_pressureMode
cat("High_blood_pressure Mode:", high_blood_pressureMode)  
dataset$high_blood_pressure
barplot(table(dataset$high_blood_pressure), main = "Mode High_blood_pressure", xlab="High_blood_pressure", ylab = "Frequency",col = "plum")


missingValues <- which(is.na(dataset$platelets))
plateletsMode <- names(sort(table(dataset$platelets[!is.na(dataset$platelets)]), decreasing = TRUE)[1])
dataset$platelets[missingValues] <- plateletsMode
cat("Platelets Mode:", plateletsMode)  
dataset$platelets
barplot(table(dataset$platelets), main = "Mode Platelets", xlab="Platelets", ylab = "Frequency",col = "purple")


missingValues <- which(is.na(dataset$serum_creatinine))
serum_creatinineMode <- names(sort(table(dataset$serum_creatinine[!is.na(dataset$serum_creatinine)]), decreasing = TRUE)[1])
dataset$serum_creatinine[missingValues] <- serum_creatinineMode
cat("Serum_creatinine Mode:", serum_creatinineMode)  
dataset$serum_creatinine
barplot(table(dataset$serum_creatinine), main = "Mode Serum_creatinine", xlab="Serum_creatinine", ylab = "Frequency",col = "lavender")


missingValues <- which(is.na(dataset$serum_sodium))
serum_sodiumMode <- names(sort(table(dataset$serum_sodium[!is.na(dataset$serum_sodium)]), decreasing = TRUE)[1])
dataset$serum_sodium[missingValues] <- serum_sodiumMode
cat("Serum_sodium Mode:", serum_sodiumMode)  
dataset$serum_sodium
barplot(table(dataset$serum_sodium), main = "Mode Serum_sodium", xlab="Serum_sodium", ylab = "Frequency",col = "darkgreen")


missingValues <- which(is.na(dataset$sex))
sexMode <- names(sort(table(dataset$sex[!is.na(dataset$sex)]), decreasing = TRUE)[1])
dataset$sex[missingValues] <- sexMode
cat("Sex Mode:", sexMode)  
dataset$sex
barplot(table(dataset$sex), main = "Mode Sex", xlab="Sex", ylab = "Frequency",col = "pink")


missingValues <- which(is.na(dataset$smoking))
smokingMode <- names(sort(table(dataset$smoking[!is.na(dataset$smoking)]), decreasing = TRUE)[1])
dataset$smoking[missingValues] <- smokingMode
cat("Smoking Mode:", smokingMode)  
dataset$smoking
barplot(table(dataset$smoking), main = "Mode Smoking", xlab="Smoking", ylab = "Frequency",col = "lightgrey")


missingValues <- which(is.na(dataset$time))
timeMode <- names(sort(table(dataset$time[!is.na(dataset$time)]), decreasing = TRUE)[1])
dataset$time[missingValues] <- timeMode
cat("Time Mode:", timeMode)  
dataset$time
barplot(table(dataset$time), main = "Mode Time", xlab="Time", ylab = "Frequency",col = "lightblue")


missingValues <- which(is.na(dataset$DEATH_EVENT))
DEATH_EVENTMode <- names(sort(table(dataset$DEATH_EVENT[!is.na(dataset$DEATH_EVENT)]), decreasing = TRUE)[1])
dataset$DEATH_EVENT[missingValues] <- DEATH_EVENTMode
cat("DEATH_EVENT:", DEATH_EVENTMode)  
dataset$DEATH_EVENT
barplot(table(dataset$DEATH_EVENT), main = "DEATH_EVENT", xlab="DEATH_EVENT", ylab = "Frequency",col = "yellowgreen")





dataset1 <- na.omit(dataset)
age_categories <- cut(dataset1$age, breaks = c(0, 30, 50, 70, Inf), labels = c("Young", "Middle-aged", "Old", "Very Old"))
dataset1$age <- age_categories
dataset1

dataset1 <- na.omit(dataset)
dataset1$anaemia<-factor(dataset1$anaemia,levels =c(0,1),labels = c("No","Yes")) 
dataset1

dataset1 <- na.omit(dataset)
dataset1$diabetes<-factor(dataset1$diabetes,levels =c(0,1),labels = c("No","Yes")) 
dataset1

dataset1 <- na.omit(dataset)
ejection_fraction_categories <- cut(dataset1$ejection_fraction, breaks = c(0, 40, 55, 70, Inf), labels = c("Possible Heart Failure", "Low Function", "Normal Function", "High Function"))
dataset1$ejection_fraction <- ejection_fraction_categories
dataset1

dataset1 <- na.omit(dataset)
dataset1$high_blood_pressure<-factor(dataset1$high_blood_pressure,levels =c(0,1),labels = c("No","Yes")) 
dataset1

dataset1 <- na.omit(dataset)
platelets_categories <- cut(dataset1$platelets, breaks = c(0, 150000, 450000, Inf), labels = c("Low", "Normal", "High"))
dataset1$platelets <- platelets_categories
dataset1

dataset1 <- na.omit(dataset)
serum_creatinine_categories <- cut(dataset1$serum_creatinine, breaks = c(0, 0.7, 1.3, Inf), labels = c("Low", "Normal", "High"))
dataset1$serum_creatinine <- serum_creatinine_categories
dataset1

dataset1 <- na.omit(dataset)
serum_sodium_categories <- cut(dataset1$serum_sodium, breaks = c(0, 135, 145, Inf), labels = c("Low", "Normal", "High"))
dataset1$serum_sodium <- serum_sodium_categories
dataset1

dataset1 <- na.omit(dataset)
dataset1$sex<-factor(dataset1$sex,levels =c("Male","Female"),labels = c(1,2))
dataset1

dataset1 <- na.omit(dataset)
dataset1$smoking<-factor(dataset1$smoking,levels =c(0,1),labels = c("No","Yes")) 
dataset1

dataset1 <- na.omit(dataset)
dataset1$DEATH_EVENT<-factor(dataset1$DEATH_EVENT,levels =c(0,1),labels = c("No","Yes")) 
dataset1





dataset1 <- na.omit(dataset)
numerical_age <- as.numeric(dataset1$age)
normalized_age <- (numerical_age - min(numerical_age)) / (max(numerical_age) - min(numerical_age))
print(min(numerical_age))
print(max(numerical_age))
dataset1$age <- normalized_age
dataset1


dataset1 <- na.omit(dataset)
numerical_ejection_fraction <- as.numeric(dataset1$ejection_fraction)
normalized_ejection_fraction <- (numerical_ejection_fraction - min(numerical_ejection_fraction)) / (max(numerical_ejection_fraction) - min(numerical_ejection_fraction))
print(min(numerical_ejection_fraction))
print(max(numerical_ejection_fraction))
dataset1$ejection_fraction <- normalized_ejection_fraction
dataset1

dataset1 <- na.omit(dataset)
numerical_platelets <- as.numeric(dataset1$platelets)
normalized_platelets <- (numerical_platelets - min(numerical_platelets)) / (max(numerical_platelets) - min(numerical_platelets))
print(min(numerical_platelets))
print(max(numerical_platelets))
dataset1$platelets <- normalized_platelets
dataset1

dataset1 <- na.omit(dataset)
numerical_serum_creatinine <- as.numeric(dataset1$serum_creatinine)
normalized_serum_creatinine <- (numerical_serum_creatinine - min(numerical_serum_creatinine)) / (max(numerical_serum_creatinine) - min(numerical_serum_creatinine))
print(min(numerical_serum_creatinine))
print(max(numerical_serum_creatinine))
dataset1$serum_creatinine <- normalized_serum_creatinine
dataset1

dataset1 <- na.omit(dataset)
numerical_serum_sodium <- as.numeric(dataset1$serum_sodium)
normalized_serum_sodium <- (numerical_serum_sodium - min(numerical_serum_sodium)) / (max(numerical_serum_sodium) - min(numerical_serum_sodium))
print(min(numerical_serum_sodium))
print(max(numerical_serum_sodium))
dataset1$serum_sodium <- normalized_serum_sodium
dataset1

dataset1 <- na.omit(dataset)
numerical_time <- as.numeric(dataset1$time)
normalized_time <- (numerical_time - min(numerical_time)) / (max(numerical_time) - min(numerical_time))
print(min(numerical_time))
print(max(numerical_time))
dataset1$time <- normalized_time
dataset1





ageBoxplot <- boxplot(dataset$age,main=" Age Distribution ", ylab="age",col="lightgreen") 
outliers <- ageBoxplot$out 
cat("Outliers are", outliers) 
ageMean <- mean(dataset$age, na.rm = TRUE) 
outlierPositions <- match(outliers, dataset$age) 
dataset$age[outlierPositions] <- as.integer (ageMean)
dataset

ageBoxplot <- boxplot(dataset$age,main=" Age Distribution ", ylab="age",col="lightblue") 
outliers <- ageBoxplot$out 
cat("Outliers are", outliers) 
ageMedian <- median(dataset$age, na.rm = TRUE) 
outlierPositions <- match(outliers, dataset$age) 
dataset$age[outlierPositions] <- as.integer (ageMedian)
dataset

ageBoxplot <- boxplot(dataset$age,main=" Age Distribution ", ylab="age",col="orange") 
outliers <- ageBoxplot$out 
cat("Outliers are", outliers) 
ageMode <- strtoi(names(sort(table(dataset$age[!is.na(dataset$age)]), decreasing = TRUE)[1])) 
outlierPositions <- match(outliers, dataset$age) 
dataset$age[outlierPositions] <- as.integer (ageMode) 






dataset$creatinine_phosphokinase <- as.numeric(dataset$creatinine_phosphokinase)
median_value <- median(dataset$creatinine_phosphokinase, na.rm = TRUE)
dataset$creatinine_phosphokinase[is.na(dataset$creatinine_phosphokinase)] <- median_value
print(dataset)


invalid_values <- !dataset$sex %in% c("Male", "Female")
dataset$sex[invalid_values] <- ifelse(dataset$sex[invalid_values] == "Maleee", "Male", "Female")
print(dataset)





