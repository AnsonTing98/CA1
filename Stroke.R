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
hist(stroke_data$bmi)

stroke_sum <- sum(stroke_data$stroke[stroke_data$stroke == 1])
not_stroke_sum <- nrow(stroke_data[stroke_data$stroke == 0, ]) 

percentage_of_stroke <- c(round(stroke_sum / not_stroke_sum * 100), 
                          round(100 - (stroke_sum / not_stroke_sum * 100)))

barplot(percentage_of_stroke, 
        col = c("red", "blue"), 
        main = "Percentage of Patient had Stroke",
        ylab = "Percentage", 
        ylim = c(0,105), 
        names.arg = c("Stroke", "Healthy"))
axis(2, at = 0:100 * 10)
text(0.7, 5, labels = percentage_of_stroke[1],
     pos = 3)
text(0.75, 5, labels = "%",
     pos = 3)
text(1.9, 95, labels = percentage_of_stroke[2],
     pos = 3)
text(1.96, 95, labels = "%",
     pos = 3)

