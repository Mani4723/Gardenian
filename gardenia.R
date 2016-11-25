Train1 = Train
Test1 = Test

# Initial ID 
#Train1$ID = NULL
#Test1$ID = NULL

# Date variable
Train1$Date = as.Date(as.character(Train1$Date), format = "%d-%m-%Y")
Test1$Date = as.Date(as.character(Test1$Date), format = "%d-%m-%Y")
Train1$month = months(Train1$Date)
Test1$month = months(Test1$Date)
Train1$month = as.factor(Train1$month)
Test1$month = as.factor(Test1$month)
Test1$Day = as.factor(Test1$Day)
Train1$Day = as.factor(Train1$Day)


# These migth not be useful.
Train1$year = year(Train1$Date)
Test1$year = year(Test1$Date)
Train1$Day = weekdays(Train1$Date)
Test1$Day = weekdays(Test1$Date)

## park_ID
Train1$Park_ID = as.factor(Train1$Park_ID)
Test1$Park_ID  = as.factor(Test1$Park_ID)

#location type
Train1$Location_Type = as.factor(Train1$Location_Type)
Test1$Location_Type = as.factor(Test1$Location_Type)


## Moisture Content. 
Train1$Average_Moisture_In_Park = impute(Train1$Average_Moisture_In_Park, mean)
Test1$Average_Moisture_In_Park = impute(Test1$Average_Moisture_In_Park, mean)
Train1$Max_Moisture_In_Park = impute(Train1$Max_Moisture_In_Park, mean)
Test1$Max_Moisture_In_Park = impute(Test1$Max_Moisture_In_Park, mean)
Train1$Min_Moisture_In_Park = impute(Train1$Min_Moisture_In_Park, mean)
Test1$Min_Moisture_In_Park = impute(Test1$Min_Moisture_In_Park, mean)





# Direction of the wind & Var1.
# Just for now imputed with median. But try to find by months or using some predictive techs.
Train1$Direction_Of_Wind = impute(Train1$Direction_Of_Wind, median)
Test1$Direction_Of_Wind = impute(Test1$Direction_Of_Wind, median)
Train1$Var1 = impute(Train1$Var1, median)
Test1$Var1 = impute(Test1$Var1, median)



## Var1 
#14th & 33rd park data is completely missing. For Now imputing with median using Month variable
## But latter impute it better
Train1_April = subset(Train1, Train1$month == "April")
Train1_Jan = subset(Train1, Train1$month == "January")
Train1_Aug = subset(Train1, Train1$month == "August")
Train1_Dec = subset(Train1, Train1$month == "December")
Train1_Feb = subset(Train1, Train1$month == "February")
Train1_July = subset(Train1, Train1$month == "July")
Train1_June = subset(Train1, Train1$month == "June")
Train1_Mar = subset(Train1, Train1$month == "March")
Train1_May = subset(Train1, Train1$month == "May")
Train1_Nov = subset(Train1, Train1$month == "November")
Train1_Oct = subset(Train1, Train1$month == "October")
Train1_Sept = subset(Train1, Train1$month == "September")

#impute
Train1_April$Var1 = impute(Train1_April$Var1, median)
Train1_Aug$Var1 = impute(Train1_Aug$Var1, median)
Train1_Dec$Var1 = impute(Train1_Dec$Var1, median)
Train1_Feb$Var1 = impute(Train1_Feb$Var1, median)
Train1_Jan$Var1 = impute(Train1_Jan$Var1, median)
Train1_July$Var1 = impute(Train1_July$Var1, median)
Train1_June$Var1 = impute(Train1_June$Var1, median)
Train1_Mar$Var1 = impute(Train1_Mar$Var1, median)
Train1_May$Var1 = impute(Train1_May$Var1, median)
Train1_Nov$Var1 = impute(Train1_Nov$Var1, median)
Train1_Oct$Var1 = impute(Train1_Oct$Var1, median)
Train1_Sept$Var1 = impute(Train1_Sept$Var1, median)

Train1_mon = rbind(Train1_April, Train1_Oct, Train1_Sept, Train1_Nov, Train1_May,
                   Train1_Mar, Train1_June, Train1_July, Train1_Feb, Train1_Dec,
                   Train1_Aug, Train1_Jan)
rm(Train1_April, Train1_Oct, Train1_Sept, Train1_Nov, Train1_May,
                   Train1_Mar, Train1_June, Train1_July, Train1_Feb, Train1_Dec,
                   Train1_Aug, Train1_Jan)

## combine all the dataframes by rbind.
# merge Train1 & combined data frame.



## Imputeing all the values manually 
# Direction of wind with mean.
Train1$Direction_Of_Wind = ifelse(is.na(Train1$Direction_Of_Wind) & Train1$month == "April", 170.3, Train1$Direction_Of_Wind)
Train1$Direction_Of_Wind = ifelse(is.na(Train1$Direction_Of_Wind) & Train1$month == "August", 201.1, Train1$Direction_Of_Wind)
Train1$Direction_Of_Wind = ifelse(is.na(Train1$Direction_Of_Wind) & Train1$month == "December", 171.1, Train1$Direction_Of_Wind)
Train1$Direction_Of_Wind = ifelse(is.na(Train1$Direction_Of_Wind) & Train1$month == "February", 172.5, Train1$Direction_Of_Wind)
Train1$Direction_Of_Wind = ifelse(is.na(Train1$Direction_Of_Wind) & Train1$month == "January", 175.5, Train1$Direction_Of_Wind)
Train1$Direction_Of_Wind = ifelse(is.na(Train1$Direction_Of_Wind) & Train1$month == "July", 195, Train1$Direction_Of_Wind)
Train1$Direction_Of_Wind = ifelse(is.na(Train1$Direction_Of_Wind) & Train1$month == "June", 192.2, Train1$Direction_Of_Wind)
Train1$Direction_Of_Wind = ifelse(is.na(Train1$Direction_Of_Wind) & Train1$month == "March", 172.5, Train1$Direction_Of_Wind)
Train1$Direction_Of_Wind = ifelse(is.na(Train1$Direction_Of_Wind) & Train1$month == "May", 175.9, Train1$Direction_Of_Wind)
Train1$Direction_Of_Wind = ifelse(is.na(Train1$Direction_Of_Wind) & Train1$month == "November", 181.1, Train1$Direction_Of_Wind)
Train1$Direction_Of_Wind = ifelse(is.na(Train1$Direction_Of_Wind) & Train1$month == "October", 168.9, Train1$Direction_Of_Wind)
Train1$Direction_Of_Wind = ifelse(is.na(Train1$Direction_Of_Wind) & Train1$month == "September", 180, Train1$Direction_Of_Wind)

## Test
Test1$Direction_Of_Wind = ifelse(is.na(Test1$Direction_Of_Wind) & Test1$month == "April", 173.5, Test1$Direction_Of_Wind)
Test1$Direction_Of_Wind = ifelse(is.na(Test1$Direction_Of_Wind) & Test1$month == "August", 184.1, Test1$Direction_Of_Wind)
Test1$Direction_Of_Wind = ifelse(is.na(Test1$Direction_Of_Wind) & Test1$month == "December", 191.6, Test1$Direction_Of_Wind)
Test1$Direction_Of_Wind = ifelse(is.na(Test1$Direction_Of_Wind) & Test1$month == "February", 171.5, Test1$Direction_Of_Wind)
Test1$Direction_Of_Wind = ifelse(is.na(Test1$Direction_Of_Wind) & Test1$month == "January", 179.8, Test1$Direction_Of_Wind)
Test1$Direction_Of_Wind = ifelse(is.na(Test1$Direction_Of_Wind) & Test1$month == "July", 183.8, Test1$Direction_Of_Wind)
Test1$Direction_Of_Wind = ifelse(is.na(Test1$Direction_Of_Wind) & Test1$month == "June", 192.7, Test1$Direction_Of_Wind)
Test1$Direction_Of_Wind = ifelse(is.na(Test1$Direction_Of_Wind) & Test1$month == "March", 162.7, Test1$Direction_Of_Wind)
Test1$Direction_Of_Wind = ifelse(is.na(Test1$Direction_Of_Wind) & Test1$month == "May", 183, Test1$Direction_Of_Wind)
Test1$Direction_Of_Wind = ifelse(is.na(Test1$Direction_Of_Wind) & Test1$month == "November", 181, Test1$Direction_Of_Wind)
Test1$Direction_Of_Wind = ifelse(is.na(Test1$Direction_Of_Wind) & Test1$month == "October", 163.1, Test1$Direction_Of_Wind)
Test1$Direction_Of_Wind = ifelse(is.na(Test1$Direction_Of_Wind) & Test1$month == "September", 184.6, Test1$Direction_Of_Wind)


## Average_Breeze_speed with Mean
Train1$Average_Breeze_Speed = ifelse(is.na(Train1$Average_Breeze_Speed) & Train1$month == "April", 32.85, Train1$Average_Breeze_Speed)
Train1$Average_Breeze_Speed = ifelse(is.na(Train1$Average_Breeze_Speed) & Train1$month == "August", 29.15, Train1$Average_Breeze_Speed)
Train1$Average_Breeze_Speed = ifelse(is.na(Train1$Average_Breeze_Speed) & Train1$month == "December", 37.31, Train1$Average_Breeze_Speed)
Train1$Average_Breeze_Speed = ifelse(is.na(Train1$Average_Breeze_Speed) & Train1$month == "February", 38.27, Train1$Average_Breeze_Speed)
Train1$Average_Breeze_Speed = ifelse(is.na(Train1$Average_Breeze_Speed) & Train1$month == "January", 40.84, Train1$Average_Breeze_Speed)
Train1$Average_Breeze_Speed = ifelse(is.na(Train1$Average_Breeze_Speed) & Train1$month == "July", 30.22, Train1$Average_Breeze_Speed)
Train1$Average_Breeze_Speed = ifelse(is.na(Train1$Average_Breeze_Speed) & Train1$month == "June", 30.31, Train1$Average_Breeze_Speed)
Train1$Average_Breeze_Speed = ifelse(is.na(Train1$Average_Breeze_Speed) & Train1$month == "March", 37.05, Train1$Average_Breeze_Speed)
Train1$Average_Breeze_Speed = ifelse(is.na(Train1$Average_Breeze_Speed) & Train1$month == "May", 33.33, Train1$Average_Breeze_Speed)
Train1$Average_Breeze_Speed = ifelse(is.na(Train1$Average_Breeze_Speed) & Train1$month == "November", 36.49, Train1$Average_Breeze_Speed)
Train1$Average_Breeze_Speed = ifelse(is.na(Train1$Average_Breeze_Speed) & Train1$month == "October", 35.03, Train1$Average_Breeze_Speed)
Train1$Average_Breeze_Speed = ifelse(is.na(Train1$Average_Breeze_Speed) & Train1$month == "September", 30.18, Train1$Average_Breeze_Speed)
# Test
Test1$Average_Breeze_Speed = ifelse(is.na(Test1$Average_Breeze_Speed) & Test1$month == "April", 33, Test1$Average_Breeze_Speed)
Test1$Average_Breeze_Speed = ifelse(is.na(Test1$Average_Breeze_Speed) & Test1$month == "August", 28.75, Test1$Average_Breeze_Speed)
Test1$Average_Breeze_Speed = ifelse(is.na(Test1$Average_Breeze_Speed) & Test1$month == "December", 44.58, Test1$Average_Breeze_Speed)
Test1$Average_Breeze_Speed = ifelse(is.na(Test1$Average_Breeze_Speed) & Test1$month == "February", 37.81, Test1$Average_Breeze_Speed)
Test1$Average_Breeze_Speed = ifelse(is.na(Test1$Average_Breeze_Speed) & Test1$month == "January", 41.86, Test1$Average_Breeze_Speed)
Test1$Average_Breeze_Speed = ifelse(is.na(Test1$Average_Breeze_Speed) & Test1$month == "July", 29.20, Test1$Average_Breeze_Speed)
Test1$Average_Breeze_Speed = ifelse(is.na(Test1$Average_Breeze_Speed) & Test1$month == "June", 32.41, Test1$Average_Breeze_Speed)
Test1$Average_Breeze_Speed = ifelse(is.na(Test1$Average_Breeze_Speed) & Test1$month == "March", 35.35, Test1$Average_Breeze_Speed)
Test1$Average_Breeze_Speed = ifelse(is.na(Test1$Average_Breeze_Speed) & Test1$month == "May", 33.25, Test1$Average_Breeze_Speed)
Test1$Average_Breeze_Speed = ifelse(is.na(Test1$Average_Breeze_Speed) & Test1$month == "November", 36.20, Test1$Average_Breeze_Speed)
Test1$Average_Breeze_Speed = ifelse(is.na(Test1$Average_Breeze_Speed) & Test1$month == "October", 31.75, Test1$Average_Breeze_Speed)
Test1$Average_Breeze_Speed = ifelse(is.na(Test1$Average_Breeze_Speed) & Test1$month == "September", 28.87, Test1$Average_Breeze_Speed)

## Max_Average_Speed with mean
Train1$Max_Breeze_Speed = ifelse(is.na(Train1$Max_Breeze_Speed) & Train1$month == "April", 51.15, Train1$Max_Breeze_Speed)
Train1$Max_Breeze_Speed = ifelse(is.na(Train1$Max_Breeze_Speed) & Train1$month == "August", 46.37, Train1$Max_Breeze_Speed)
Train1$Max_Breeze_Speed = ifelse(is.na(Train1$Max_Breeze_Speed) & Train1$month == "December", 53.82, Train1$Max_Breeze_Speed)
Train1$Max_Breeze_Speed = ifelse(is.na(Train1$Max_Breeze_Speed) & Train1$month == "February", 55.66, Train1$Max_Breeze_Speed)
Train1$Max_Breeze_Speed = ifelse(is.na(Train1$Max_Breeze_Speed) & Train1$month == "January", 58.01, Train1$Max_Breeze_Speed)
Train1$Max_Breeze_Speed = ifelse(is.na(Train1$Max_Breeze_Speed) & Train1$month == "July", 48.15, Train1$Max_Breeze_Speed)
Train1$Max_Breeze_Speed = ifelse(is.na(Train1$Max_Breeze_Speed) & Train1$month == "June", 48.05, Train1$Max_Breeze_Speed)
Train1$Max_Breeze_Speed = ifelse(is.na(Train1$Max_Breeze_Speed) & Train1$month == "March", 55.87, Train1$Max_Breeze_Speed)
Train1$Max_Breeze_Speed = ifelse(is.na(Train1$Max_Breeze_Speed) & Train1$month == "May", 51.52, Train1$Max_Breeze_Speed)
Train1$Max_Breeze_Speed = ifelse(is.na(Train1$Max_Breeze_Speed) & Train1$month == "November", 53.22, Train1$Max_Breeze_Speed)
Train1$Max_Breeze_Speed = ifelse(is.na(Train1$Max_Breeze_Speed) & Train1$month == "October", 51.86, Train1$Max_Breeze_Speed)
Train1$Max_Breeze_Speed = ifelse(is.na(Train1$Max_Breeze_Speed) & Train1$month == "September", 46.98, Train1$Max_Breeze_Speed)
## Test 
Test1$Max_Breeze_Speed = ifelse(is.na(Test1$Max_Breeze_Speed) & Test1$month == "April", 52.15, Test1$Max_Breeze_Speed)
Test1$Max_Breeze_Speed = ifelse(is.na(Test1$Max_Breeze_Speed) & Test1$month == "August", 46.34, Test1$Max_Breeze_Speed)
Test1$Max_Breeze_Speed = ifelse(is.na(Test1$Max_Breeze_Speed) & Test1$month == "December", 64.22, Test1$Max_Breeze_Speed)
Test1$Max_Breeze_Speed = ifelse(is.na(Test1$Max_Breeze_Speed) & Test1$month == "February", 54.94, Test1$Max_Breeze_Speed)
Test1$Max_Breeze_Speed = ifelse(is.na(Test1$Max_Breeze_Speed) & Test1$month == "January", 58.89, Test1$Max_Breeze_Speed)
Test1$Max_Breeze_Speed = ifelse(is.na(Test1$Max_Breeze_Speed) & Test1$month == "July", 46.74, Test1$Max_Breeze_Speed)
Test1$Max_Breeze_Speed = ifelse(is.na(Test1$Max_Breeze_Speed) & Test1$month == "June", 49.66, Test1$Max_Breeze_Speed)
Test1$Max_Breeze_Speed = ifelse(is.na(Test1$Max_Breeze_Speed) & Test1$month == "March", 51.58, Test1$Max_Breeze_Speed)
Test1$Max_Breeze_Speed = ifelse(is.na(Test1$Max_Breeze_Speed) & Test1$month == "May", 51.45, Test1$Max_Breeze_Speed)
Test1$Max_Breeze_Speed = ifelse(is.na(Test1$Max_Breeze_Speed) & Test1$month == "November", 54.04, Test1$Max_Breeze_Speed)
Test1$Max_Breeze_Speed = ifelse(is.na(Test1$Max_Breeze_Speed) & Test1$month == "October", 48.0, Test1$Max_Breeze_Speed)
Test1$Max_Breeze_Speed = ifelse(is.na(Test1$Max_Breeze_Speed) & Test1$month == "September", 45.89, Test1$Max_Breeze_Speed)

## Min_Breeze_speed with Mean
Train1$Min_Breeze_Speed = ifelse(is.na(Train1$Min_Breeze_Speed) & Train1$month == "April", 15.04, Train1$Min_Breeze_Speed)
Train1$Min_Breeze_Speed = ifelse(is.na(Train1$Min_Breeze_Speed) & Train1$month == "August", 12.65, Train1$Min_Breeze_Speed)
Train1$Min_Breeze_Speed = ifelse(is.na(Train1$Min_Breeze_Speed) & Train1$month == "December", 21.51, Train1$Min_Breeze_Speed)
Train1$Min_Breeze_Speed = ifelse(is.na(Train1$Min_Breeze_Speed) & Train1$month == "February", 21.1, Train1$Min_Breeze_Speed)
Train1$Min_Breeze_Speed = ifelse(is.na(Train1$Min_Breeze_Speed) & Train1$month == "January", 24.05, Train1$Min_Breeze_Speed)
Train1$Min_Breeze_Speed = ifelse(is.na(Train1$Min_Breeze_Speed) & Train1$month == "July", 13.07, Train1$Min_Breeze_Speed)
Train1$Min_Breeze_Speed = ifelse(is.na(Train1$Min_Breeze_Speed) & Train1$month == "June", 12.94, Train1$Min_Breeze_Speed)
Train1$Min_Breeze_Speed = ifelse(is.na(Train1$Min_Breeze_Speed) & Train1$month == "March", 18.56, Train1$Min_Breeze_Speed)
Train1$Min_Breeze_Speed = ifelse(is.na(Train1$Min_Breeze_Speed) & Train1$month == "May", 15.0, Train1$Min_Breeze_Speed)
Train1$Min_Breeze_Speed = ifelse(is.na(Train1$Min_Breeze_Speed) & Train1$month == "November", 20.08, Train1$Min_Breeze_Speed)
Train1$Min_Breeze_Speed = ifelse(is.na(Train1$Min_Breeze_Speed) & Train1$month == "October", 18.58, Train1$Min_Breeze_Speed)
Train1$Min_Breeze_Speed = ifelse(is.na(Train1$Min_Breeze_Speed) & Train1$month == "September", 14.46, Train1$Min_Breeze_Speed)
#Test
Test1$Min_Breeze_Speed = ifelse(is.na(Test1$Min_Breeze_Speed) & Test1$month == "April", 14.11, Test1$Min_Breeze_Speed)
Test1$Min_Breeze_Speed = ifelse(is.na(Test1$Min_Breeze_Speed) & Test1$month == "August", 12.12, Test1$Min_Breeze_Speed)
Test1$Min_Breeze_Speed = ifelse(is.na(Test1$Min_Breeze_Speed) & Test1$month == "December", 26.23, Test1$Min_Breeze_Speed)
Test1$Min_Breeze_Speed = ifelse(is.na(Test1$Min_Breeze_Speed) & Test1$month == "February", 20.77, Test1$Min_Breeze_Speed)
Test1$Min_Breeze_Speed = ifelse(is.na(Test1$Min_Breeze_Speed) & Test1$month == "January", 24.96, Test1$Min_Breeze_Speed)
Test1$Min_Breeze_Speed = ifelse(is.na(Test1$Min_Breeze_Speed) & Test1$month == "July", 12.55, Test1$Min_Breeze_Speed)
Test1$Min_Breeze_Speed = ifelse(is.na(Test1$Min_Breeze_Speed) & Test1$month == "June", 14.48, Test1$Min_Breeze_Speed)
Test1$Min_Breeze_Speed = ifelse(is.na(Test1$Min_Breeze_Speed) & Test1$month == "March", 18.72, Test1$Min_Breeze_Speed)
Test1$Min_Breeze_Speed = ifelse(is.na(Test1$Min_Breeze_Speed) & Test1$month == "May", 14.8, Test1$Min_Breeze_Speed)
Test1$Min_Breeze_Speed = ifelse(is.na(Test1$Min_Breeze_Speed) & Test1$month == "November", 19.79, Test1$Min_Breeze_Speed)
Test1$Min_Breeze_Speed = ifelse(is.na(Test1$Min_Breeze_Speed) & Test1$month == "October", 16.76, Test1$Min_Breeze_Speed)
Test1$Min_Breeze_Speed = ifelse(is.na(Test1$Min_Breeze_Speed) & Test1$month == "September", 13.27, Test1$Min_Breeze_Speed)

## Var1 with Mean 
Train1$Var1 = ifelse(is.na(Train1$Var1) & Train1$month == "April", 10.28, Train1$Var1)
Train1$Var1 = ifelse(is.na(Train1$Var1) & Train1$month == "August", 26.57, Train1$Var1)
Train1$Var1 = ifelse(is.na(Train1$Var1) & Train1$month == "December", 20.17, Train1$Var1)
Train1$Var1 = ifelse(is.na(Train1$Var1) & Train1$month == "February", 18.70, Train1$Var1)
Train1$Var1 = ifelse(is.na(Train1$Var1) & Train1$month == "January", 18.31, Train1$Var1)
Train1$Var1 = ifelse(is.na(Train1$Var1) & Train1$month == "July", 25.52, Train1$Var1)
Train1$Var1 = ifelse(is.na(Train1$Var1) & Train1$month == "June", 16.82, Train1$Var1)
Train1$Var1 = ifelse(is.na(Train1$Var1) & Train1$month == "March", 13.57, Train1$Var1)
Train1$Var1 = ifelse(is.na(Train1$Var1) & Train1$month == "May", 14.98, Train1$Var1)
Train1$Var1 = ifelse(is.na(Train1$Var1) & Train1$month == "November", 21.55, Train1$Var1)
Train1$Var1 = ifelse(is.na(Train1$Var1) & Train1$month == "October", 19.20, Train1$Var1)
Train1$Var1 = ifelse(is.na(Train1$Var1) & Train1$month == "September", 19.25, Train1$Var1)
# Test cases 
Test1$Var1 = ifelse(is.na(Test1$Var1) & Test1$month == "April", 9.741, Test1$Var1)
Test1$Var1 = ifelse(is.na(Test1$Var1) & Test1$month == "August", 25.89, Test1$Var1)
Test1$Var1 = ifelse(is.na(Test1$Var1) & Test1$month == "December",22.14, Test1$Var1)
Test1$Var1 = ifelse(is.na(Test1$Var1) & Test1$month == "February", 12.21, Test1$Var1)
Test1$Var1 = ifelse(is.na(Test1$Var1) & Test1$month == "January", 19.75, Test1$Var1)
Test1$Var1 = ifelse(is.na(Test1$Var1) & Test1$month == "July", 22.73, Test1$Var1)
Test1$Var1 = ifelse(is.na(Test1$Var1) & Test1$month == "June", 15.82, Test1$Var1)
Test1$Var1 = ifelse(is.na(Test1$Var1) & Test1$month == "March", 8.988, Test1$Var1)
Test1$Var1 = ifelse(is.na(Test1$Var1) & Test1$month == "May", 18.84, Test1$Var1)
Test1$Var1 = ifelse(is.na(Test1$Var1) & Test1$month == "November", 22.41, Test1$Var1)
Test1$Var1 = ifelse(is.na(Test1$Var1) & Test1$month == "October", 21.31, Test1$Var1)
Test1$Var1 = ifelse(is.na(Test1$Var1) & Test1$month == "September", 19.36, Test1$Var1)

##  Atmospheric Pressure with full mean.
Train1$Average_Atmospheric_Pressure = impute(Train1$Average_Atmospheric_Pressure, mean)
Train1$Max_Atmospheric_Pressure = impute(Train1$Max_Atmospheric_Pressure, mean)
Train1$Min_Atmospheric_Pressure = impute(Train1$Min_Atmospheric_Pressure, mean)
Test1$Average_Atmospheric_Pressure = impute(Test1$Average_Atmospheric_Pressure, mean)
Test1$Max_Atmospheric_Pressure = impute(Test1$Max_Atmospheric_Pressure, mean)
Test1$Min_Atmospheric_Pressure = impute(Test1$Min_Atmospheric_Pressure, mean)

## Ambient Pollution
Train1$Min_Ambient_Pollution = ifelse(is.na(Train1$Min_Ambient_Pollution) & Train1$month == "April", 176.5, Train1$Min_Ambient_Pollution)
Train1$Min_Ambient_Pollution = ifelse(is.na(Train1$Min_Ambient_Pollution) & Train1$month == "August", 173.6, Train1$Min_Ambient_Pollution)
Train1$Min_Ambient_Pollution = ifelse(is.na(Train1$Min_Ambient_Pollution) & Train1$month == "December", 136.5, Train1$Min_Ambient_Pollution)
Train1$Min_Ambient_Pollution = ifelse(is.na(Train1$Min_Ambient_Pollution) & Train1$month == "February", 141.8, Train1$Min_Ambient_Pollution)
Train1$Min_Ambient_Pollution = ifelse(is.na(Train1$Min_Ambient_Pollution) & Train1$month == "January", 145.2, Train1$Min_Ambient_Pollution)
Train1$Min_Ambient_Pollution = ifelse(is.na(Train1$Min_Ambient_Pollution) & Train1$month == "July", 183.4, Train1$Min_Ambient_Pollution)
Train1$Min_Ambient_Pollution = ifelse(is.na(Train1$Min_Ambient_Pollution) & Train1$month == "June", 187.7, Train1$Min_Ambient_Pollution)
Train1$Min_Ambient_Pollution = ifelse(is.na(Train1$Min_Ambient_Pollution) & Train1$month == "March", 153.4, Train1$Min_Ambient_Pollution)
Train1$Min_Ambient_Pollution = ifelse(is.na(Train1$Min_Ambient_Pollution) & Train1$month == "May", 191.8, Train1$Min_Ambient_Pollution)
Train1$Min_Ambient_Pollution = ifelse(is.na(Train1$Min_Ambient_Pollution) & Train1$month == "November", 145.3, Train1$Min_Ambient_Pollution)
Train1$Min_Ambient_Pollution = ifelse(is.na(Train1$Min_Ambient_Pollution) & Train1$month == "October", 162.1, Train1$Min_Ambient_Pollution)
Train1$Min_Ambient_Pollution = ifelse(is.na(Train1$Min_Ambient_Pollution) & Train1$month == "September", 159.1, Train1$Min_Ambient_Pollution)
#Test
Test1$Min_Ambient_Pollution = ifelse(is.na(Test1$Min_Ambient_Pollution) & Test1$month == "April", 177.7, Test1$Min_Ambient_Pollution)
Test1$Min_Ambient_Pollution = ifelse(is.na(Test1$Min_Ambient_Pollution) & Test1$month == "August", 189.6, Test1$Min_Ambient_Pollution)
Test1$Min_Ambient_Pollution = ifelse(is.na(Test1$Min_Ambient_Pollution) & Test1$month == "December", 168.3, Test1$Min_Ambient_Pollution)
Test1$Min_Ambient_Pollution = ifelse(is.na(Test1$Min_Ambient_Pollution) & Test1$month == "February", 164.5, Test1$Min_Ambient_Pollution)
Test1$Min_Ambient_Pollution = ifelse(is.na(Test1$Min_Ambient_Pollution) & Test1$month == "January", 158.9, Test1$Min_Ambient_Pollution)
Test1$Min_Ambient_Pollution = ifelse(is.na(Test1$Min_Ambient_Pollution) & Test1$month == "July", 199.4, Test1$Min_Ambient_Pollution)
Test1$Min_Ambient_Pollution = ifelse(is.na(Test1$Min_Ambient_Pollution) & Test1$month == "June", 214.9, Test1$Min_Ambient_Pollution)
Test1$Min_Ambient_Pollution = ifelse(is.na(Test1$Min_Ambient_Pollution) & Test1$month == "March", 158.7, Test1$Min_Ambient_Pollution)
Test1$Min_Ambient_Pollution = ifelse(is.na(Test1$Min_Ambient_Pollution) & Test1$month == "May", 194.3, Test1$Min_Ambient_Pollution)
Test1$Min_Ambient_Pollution = ifelse(is.na(Test1$Min_Ambient_Pollution) & Test1$month == "November", 152.7, Test1$Min_Ambient_Pollution)
Test1$Min_Ambient_Pollution = ifelse(is.na(Test1$Min_Ambient_Pollution) & Test1$month == "October", 153.9, Test1$Min_Ambient_Pollution)
Test1$Min_Ambient_Pollution = ifelse(is.na(Test1$Min_Ambient_Pollution) & Test1$month == "September", 160, Test1$Min_Ambient_Pollution)

## Max_Ambient_Pollution.
Train1$Max_Ambient_Pollution = ifelse(is.na(Train1$Max_Ambient_Pollution) & Train1$month == "April", 317.5, Train1$Max_Ambient_Pollution)
Train1$Max_Ambient_Pollution = ifelse(is.na(Train1$Max_Ambient_Pollution) & Train1$month == "August", 322.5, Train1$Max_Ambient_Pollution)
Train1$Max_Ambient_Pollution = ifelse(is.na(Train1$Max_Ambient_Pollution) & Train1$month == "December", 280.7, Train1$Max_Ambient_Pollution)
Train1$Max_Ambient_Pollution = ifelse(is.na(Train1$Max_Ambient_Pollution) & Train1$month == "February", 287.8, Train1$Max_Ambient_Pollution)
Train1$Max_Ambient_Pollution = ifelse(is.na(Train1$Max_Ambient_Pollution) & Train1$month == "January", 283.4, Train1$Max_Ambient_Pollution)
Train1$Max_Ambient_Pollution = ifelse(is.na(Train1$Max_Ambient_Pollution) & Train1$month == "July", 326.3, Train1$Max_Ambient_Pollution)
Train1$Max_Ambient_Pollution = ifelse(is.na(Train1$Max_Ambient_Pollution) & Train1$month == "June", 325.2, Train1$Max_Ambient_Pollution)
Train1$Max_Ambient_Pollution = ifelse(is.na(Train1$Max_Ambient_Pollution) & Train1$month == "March", 302.1, Train1$Max_Ambient_Pollution)
Train1$Max_Ambient_Pollution = ifelse(is.na(Train1$Max_Ambient_Pollution) & Train1$month == "May", 324.8, Train1$Max_Ambient_Pollution)
Train1$Max_Ambient_Pollution = ifelse(is.na(Train1$Max_Ambient_Pollution) & Train1$month == "November", 289, Train1$Max_Ambient_Pollution)
Train1$Max_Ambient_Pollution = ifelse(is.na(Train1$Max_Ambient_Pollution) & Train1$month == "October", 305, Train1$Max_Ambient_Pollution)
Train1$Max_Ambient_Pollution = ifelse(is.na(Train1$Max_Ambient_Pollution) & Train1$month == "September", 316.3, Train1$Max_Ambient_Pollution)
# Test
Test1$Max_Ambient_Pollution = ifelse(is.na(Test1$Max_Ambient_Pollution) & Test1$month == "April", 319.6, Test1$Max_Ambient_Pollution)
Test1$Max_Ambient_Pollution = ifelse(is.na(Test1$Max_Ambient_Pollution) & Test1$month == "August", 330.8, Test1$Max_Ambient_Pollution)
Test1$Max_Ambient_Pollution = ifelse(is.na(Test1$Max_Ambient_Pollution) & Test1$month == "December", 295.8, Test1$Max_Ambient_Pollution)
Test1$Max_Ambient_Pollution = ifelse(is.na(Test1$Max_Ambient_Pollution) & Test1$month == "February", 298.1, Test1$Max_Ambient_Pollution)
Test1$Max_Ambient_Pollution = ifelse(is.na(Test1$Max_Ambient_Pollution) & Test1$month == "January", 289.4, Test1$Max_Ambient_Pollution)
Test1$Max_Ambient_Pollution = ifelse(is.na(Test1$Max_Ambient_Pollution) & Test1$month == "July", 328.2, Test1$Max_Ambient_Pollution)
Test1$Max_Ambient_Pollution = ifelse(is.na(Test1$Max_Ambient_Pollution) & Test1$month == "June", 328.6, Test1$Max_Ambient_Pollution)
Test1$Max_Ambient_Pollution = ifelse(is.na(Test1$Max_Ambient_Pollution) & Test1$month == "March", 302, Test1$Max_Ambient_Pollution)
Test1$Max_Ambient_Pollution = ifelse(is.na(Test1$Max_Ambient_Pollution) & Test1$month == "May", 325.3, Test1$Max_Ambient_Pollution)
Test1$Max_Ambient_Pollution = ifelse(is.na(Test1$Max_Ambient_Pollution) & Test1$month == "November", 291.5, Test1$Max_Ambient_Pollution)
Test1$Max_Ambient_Pollution = ifelse(is.na(Test1$Max_Ambient_Pollution) & Test1$month == "October", 303.3, Test1$Max_Ambient_Pollution)
Test1$Max_Ambient_Pollution = ifelse(is.na(Test1$Max_Ambient_Pollution) & Test1$month == "September", 321.1, Test1$Max_Ambient_Pollution)


####################################################################################################################
## Modeling Buliding

Control = trainControl(method = "cv", number = 10)

### Linear Models
library(elasticnet)
library(caret)
library(lars)
library(MASS)
library(pls)
library(stats)

#lm Model
lmModel = train(x = Train1[,-c(1, 20)], y = Train1$Footfall, method = "lm",
                trControl = Control)
xyplot(Footfall_train ~ predict(lmModel), type = c("p", "g"))
xyplot(resid(lmModel) ~ predict(lmModel), type = c("p", "g"))

# robust regression
rlmMOdel = rlm(Footfall ~ ., data = Train1)
rlModel = train(x = Train1[,-c(1, 20)], y = Footfall_train, method = "rlm", trControl = Control)

##
plsModel = train(x = Train1[,-c(1, 17)], y = Train$Footfall, method = "pls",
                 preProc = c("center", "scale"), tuneLength = 20,
                 trControl = Control)
## penalised Models 
ridgeGrid = data.frame(.lambda = seq(0, 0.1, length = 15))
ridgefit = train(x = Train1, y = Footfall_train, method = "ridge",
                 tuneGrid = ridgeGrid, trControl = Control, preProc = c("center", "scale"))
## 
enetGrid = expand.grid(.lambda = c(0.001, 0.01, 0.1), .fraction = seq(0.05, 1, length = 20))
enetTune = train(x = Train1, y = Footfall_train, method = "enet", tuneGrid = enetGrid, trControl = Control,
                 preProc = c("center", "scale"))



## Random Forest
rFModel = train(x = Train_mon, y = Footfall_train, 
                method = "rf", tuneLength = 10, ntrees = 1000, importance = TRUE, trControl = Control)

##predictions 
pred = predict(rlModel, Test1)
pred = as.integer(pred)
Test1$Footfall = pred
Sample_Submission = subset(Test1, select = c(1, 20))
write.csv(Sample_Submission, "First1.csv", row.names=FALSE)
Test1$Footfall = NULL


### one hot encoding
for(f in cat) { 
  train_mon_dummy = acm.disjonctif(Train_mon[cat])
  Train_mon[cat] = NULL
  Train_mon = cbind(Train_mon, train_mon_dummy)
}






















































































































