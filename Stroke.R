# read csv file into a dataframe
stroke_data <- read.csv("stroke.csv", na = "")

stroke_data

# Examine the data type and 
# show the structure of dataframe
str(stroke_data)

# Change N/A of bmi to NA
# and convert bmi from chr to numerical
stroke_data$bmi[stroke_data$bmi == "N/A"] <- NA

stroke_data$bmi <- as.numeric(stroke_data$bmi)

str(stroke_data)
stroke_data$Date <- as.Date(stroke_data$Date, "%A %d %B %Y")


str(format(Sys.Date(), "%a %b %d"))
stroke_data$day_of_week <- weekdays(stroke_data$Date)
