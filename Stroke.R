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

summary(stroke_data$age)

plot(summary(factor(stroke_data$work_type)), 
     type = "b")

pie(summary(factor(stroke_data$smoking_status)))

patient_had_stroke <- stroke_data[stroke_data$stroke == 1, ]
patient_not_stroke <- stroke_data[!stroke_data$stroke == 1, ]

pie(summary(factor(patient_not_stroke$ever_married)))
plot(summary(factor(patient_had_stroke$age)))


hist(stroke_data$age)
