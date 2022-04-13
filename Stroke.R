# read csv file into a dataframe
stroke_data <- read.csv("stroke.csv", na = "")

stroke_data

# Check duplicate data
sum(duplicated(stroke_data))

# Examine the data type and 
# show the structure of dataframe
str(stroke_data)

# Change N/A of bmi to NA
# and convert bmi from chr to numerical
stroke_data$bmi[stroke_data$bmi == "N/A"] <- NA

stroke_data$bmi <- as.numeric(stroke_data$bmi)

str(stroke_data)

# Convert the Date to date str
# by using as.Date()
# Using "%A %d %B %Y"
# %A is unabbreviated weekday; %d is day; 
# %B is unabbreviated month; %Y is 4-digit year
stroke_data$Date <- as.Date(stroke_data$Date, "%A %d %B %Y")

# Create new col named weekday
# that contains day of the week
# using weekdays() to extract the weekday of the date
stroke_data$weekday <- weekdays(stroke_data$Date)

# Use mice and VIM package
# to show the missing values
library(mice)
library(VIM)
md.pattern(stroke_data, rotate.names = TRUE)

missing_values <- aggr(stroke_data, 
                       prop = FALSE, 
                       numbers = TRUE, 
                       cex.axis = 0.59)

# summary of missing_values
summary(missing_values)

# 4% BMI data missing

# Use !complete.case to get the missing data
# and store in incomplete_data
incomplete_data <- stroke_data[!complete.cases(stroke_data),]
nrow(incomplete_data)

# use na.omit() to drop the missing data
# and check the NA
stroke_data <- na.omit(stroke_data)
sum(is.na(stroke_data))

patient_stroke <- stroke_data[stroke_data$stroke == 1, ]
patient_healthy <- stroke_data[!stroke_data$stroke == 1, ]

stroke_sum <- sum(stroke_data$stroke[stroke_data$stroke == 1])
healthy_sum <- nrow(stroke_data[stroke_data$stroke == 0, ]) 

stroke_per <- c(round(stroke_sum / healthy_sum * 100), 
                          round(100 - (stroke_sum / healthy_sum * 100)))

barplot(stroke_per, 
        col = c("red", "blue"), 
        main = "Percentage of Patient had Stroke",
        ylab = "Percentage", 
        ylim = c(0,105), 
        names.arg = c("Stroke", "Healthy"))
axis(2, at = 0:100 * 10)
text(0.7, 5, labels = stroke_per[1],
     pos = 3)
text(0.75, 5, labels = "%",
     pos = 3)
text(1.9, 95, labels = stroke_per[2],
     pos = 3)
text(1.96, 95, labels = "%",
     pos = 3)

age_kde <- density(stroke_data$age)
plot(age_kde)

age_kde_stroke <- density(patient_had_stroke$age)
plot(age_kde_stroke)

glucose_kde <- density(stroke_data$avg_glucose_level)
plot(glucose_kde)

bmi_kde <- density(stroke_data$bmi)
plot(bmi_kde)

boxplot(stroke_data$bmi)

t.test(stroke_data$bmi, mu = 25, conf.level = 0.95)

boxplot(summary(factor(stroke_data$gender)))

nrow(patient_had_stroke[patient_had_stroke$gender == "Male",])
nrow(patient_not_stroke)

male_stroke_percentage <- 
  nrow(patient_had_stroke[patient_had_stroke$gender == "Male", ]) / 
  nrow(stroke_data[stroke_data$gender == "Male", ]) * 100

female_stroke_percentage <- 
  nrow(patient_had_stroke[patient_had_stroke$gender == "Female", ]) /
  nrow(stroke_data[stroke_data$gender == "Female", ]) * 100

male_percentage <- nrow(stroke_data[stroke_data$gender == "Male", ]) / 
  nrow(stroke_data) * 100



