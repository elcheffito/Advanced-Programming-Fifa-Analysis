# Analysis of Football players' Fifa ratings based on their market value, wage and other characteristics

# 7,784: Skills: Programming with Advanced Computer Languages
# Alice Miceli - Dominik Nellen - Julian Staubli - Oliver Radon - Sandro Roth

# [FIFA 19 player dataset](https://www.kaggle.com/karangadiya/fifa19/)

# Install necessary packages

install.packages("styler")
install.packages("tidyverse")
install.packages("readxl")
install.packages("xts")
install.packages("zoo")
install.packages("dplyr")
install.packages("rmarkdown")
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("plyr")
install.packages("stargazer")
install.packages("plot3D")
install.packages("plotly")
install.packages ("maps")
install.packages("WDI")
install.packages("countrycode")
install.packages("viridis")
install.packages("conflicted")

# Load the necessary packages

library("readxl")
library("zoo")
library("xts")
library("plyr")
library("dplyr")
library("styler")
library("rmarkdown")
library("ggplot2")
library("readr")
library("stargazer")
library("tidyverse")
library("plot3D")
library("plotly")
library("maps")
library("WDI")
library("countrycode")
library("conflicted")
require("maps")
require("viridis")


###########################################################################
###########################################################################
###                                                                     ###
###               1: Import and tidy the data                           ###
###                                                                     ###
###########################################################################
###########################################################################


# Please set working directory (setwd())
# Load the dataset
Fifa_data <- read_excel("data.xls")

# Tidy data
# Delete unnecessary columns (e.g. link to picture of player)
Fifa_data <- Fifa_data[,-c(1, 2, 5, 7, 11, 20, 21)]

# Delete players with no information on wage and value
Zero_Value <- which(Fifa_data$Value==0)
Zero_Wage <- which(Fifa_data$Wage==0)
Fifa_data <- Fifa_data[-Zero_Value, ]
Fifa_data <- Fifa_data[-Zero_Wage, ]

# Delete duplicate players
Fifa_data <- distinct(Fifa_data, Name, .keep_all = TRUE)

# Change column name "Overall" to "Player_Rating"
colnames(Fifa_data)[4] = "Player_Rating"

# Create new columns that show the players' market value in millions and their wage in thousands
Fifa_data$Market_Value <- Fifa_data$Value/1000000
Fifa_data$Wage_in_k <- Fifa_data$Wage/1000


###########################################################################
###########################################################################
###                                                                     ###
###               2: Overview of relevant data                          ###
###                                                                     ###
###########################################################################
###########################################################################

# Calculate the average, minimum and maximum of the players' ratings
mean_Player_Rating <- mean(Fifa_data$Player_Rating)
max_Player_Rating <- max(Fifa_data$Player_Rating)
min_Player_Rating <- min(Fifa_data$Player_Rating)

# Calculate the average, minimum and maximum of the players' market values
mean_Value <- mean(Fifa_data$Market_Value)
max_Value <- max(Fifa_data$Market_Value)
min_Value <- min(Fifa_data$Market_Value)

# Calculate the average, minimum and maximum of the players' wages
mean_Wage <- mean(Fifa_data$Wage_in_k)
max_Wage <- max(Fifa_data$Wage_in_k)
min_Wage <- min(Fifa_data$Wage_in_k)

# Calculate the quantiles and the standard deviation of the players' ratings  
sd_Player_Rating <- sd(Fifa_data$Player_Rating)
quant_Player_Rating <- quantile(Fifa_data$Player_Rating, c(0.25, 0.5, 0.75))

# Calculate the quantiles and the standard deviation of the players' market values  
sd_Value <- sd(Fifa_data$Market_Value)
quant_Value <- quantile(Fifa_data$Market_Value, c(0.25, 0.5, 0.75))

# Calculate the quantiles and the standard deviation of the players' wages 
sd_Wage <- sd(Fifa_data$Wage_in_k)
quant_Wage <- quantile(Fifa_data$Wage_in_k, c(0.25, 0.5, 0.75))

# Gather and print the statistical data of players' ratings, market values (in millions) and wages (in thousands)
Data_Summary <- data.frame(Mean = c(mean_Player_Rating, mean_Value, mean_Wage), 
                           Std = c(sd_Player_Rating, sd_Value, sd_Wage),
                           Min = c(min_Player_Rating, min_Value, min_Wage),
                           Q = rbind(quant_Player_Rating, quant_Value, quant_Wage),
                           Max = c(max_Player_Rating, max_Value, max_Wage))

Data_Summary

###########################################################################
###########################################################################
###                                                                     ###
###               3: Data regression functions                          ###
###                                                                     ###
###########################################################################
###########################################################################

# Distinguish the relationship between the players' ratings in Fifa and their market value

# Plot the players' ratings against the players' market values
plot(Fifa_data$Market_Value, Fifa_data$Player_Rating,
     col = "darkblue",
     type = "p",
     cex = 0.5,
     pch = 20,
     cex.main = 0.9,
     main = "Players' ratings and their market values", 
     xlab = "Market value (in millions)", 
     ylab = "Player rating")

# Define a linear, quadratic and cubic regression model 
linear_model <- lm(Player_Rating ~ Market_Value, data = Fifa_data)
quadratic_model <- lm(Player_Rating ~ Market_Value + I(Market_Value^2), data = Fifa_data)
cubic_model <- lm(Player_Rating ~ poly(Market_Value, degree = 3, raw = TRUE), data = Fifa_data)

# Plot the linear regression line
abline(linear_model, 
       col = "red", 
       lwd = 2)

# Sort the players' according to their market value 
order_id <- order(Fifa_data$Market_Value)

# Add the quadratic and cubic model regression line 
lines(x = Fifa_data$Market_Value[order_id], 
      y = fitted(quadratic_model)[order_id],
      col = "darkgreen", 
      lwd = 2) 

lines(x = Fifa_data$Market_Value[order_id], 
      y = fitted(cubic_model)[order_id],
      col = "violet", 
      lwd = 2) 

# Define a linear-log model 
linearlog_model <- lm(Player_Rating ~ log(Market_Value), data = Fifa_data)

# Add the linear-log model regression line 
lines(Fifa_data$Market_Value[order_id], 
      fitted(linearlog_model)[order_id], 
      col = "darkred", 
      lwd = 2)

# Add a legend to the plot
legend("bottomright", legend=c("Linear regression", 
                               "Quadratic regression", 
                               "Cubic regression", 
                               "Linear-log regression"),
       col=c("red", "darkgreen", "violet", "darkred"), lwd = 2)

# Get the statistical summaries of the 4 regression models
summary(linear_model)
summary(quadratic_model)
summary(cubic_model)
summary(linearlog_model)

# The linear-log model has the highest R squared (0.8388) of all the 4 regression models.
# Therefore, it most adequately describes the relationship.

# Additionaly adding further variables can increase the explanatory power of the regression model

# Add players' ages and wages as additional variables to the linear-log regression model
Multi_linearlog_model <- lm(Player_Rating ~ log(Market_Value) + Age + Wage_in_k, data = Fifa_data)

summary(Multi_linearlog_model)

# Adding these variables increases the R squared to 0.957 (highly accurate regression model).

###########################################################################
###########################################################################
###                                                                     ###
###               4: Analyze Swiss players                              ###
###                                                                     ###
###########################################################################
###########################################################################

# Identify Swiss players and create a new column
Fifa_data$Swiss_Player <- as.character(Fifa_data$Nationality == "Switzerland")
Swiss <- Fifa_data$Swiss_Player == "TRUE"

# How good are the Swiss players rated in Fifa in an international comparison?

# Plot all players except the Swiss
plot(Fifa_data$Market_Value[-Swiss], Fifa_data$Player_Rating[-Swiss],
     col = "green",
     pch = 20,
     cex = 0.5, 
     cex.main = 1.2,
     xlim = c(0,120),
     ylim = c(60,95),
     main = "The ratings and  market values of Swiss players", 
     xlab = "Market value (in millions)", 
     ylab = "Player's rating")

# Add the Swiss players in red color
points(Fifa_data$Market_Value[Swiss], Fifa_data$Player_Rating[Swiss],
       pch = 4,
       cex = 1, 
       col = "red")

# Add a legend to the plot
legend("bottomright", legend=c("Swiss players", 
                               "Non Swiss players"),
       col=c("red", "green"), pch = c(4, 20))

###########################################################################
###########################################################################
###                                                                     ###
###               5: Analyze a specific country                         ###
###                                                                     ###
###########################################################################
###########################################################################

# Identify players from a specific country and plot them
# Additionally plot all other players in a different color
# User can input a country of choice and see their ratings in international comparison

# Define a function that searches and gathers the players' rating and market value of a specified country (user input necessary)
marketvalue_by_Nationality <- function (Nationality_Players){
  
  if(Nationality_Players %in% Fifa_data$Nationality) {
    
    Fifa_data$country_players <- as.character(Fifa_data$Nationality == Nationality_Players)
    Fifa_data$country_players
    
    # Identify the players that match the input
    id <- Fifa_data$country_players == "TRUE"
    
    # Plot all players except the ones from the country input
    plot(Fifa_data$Market_Value[-id], Fifa_data$Player_Rating[-id],
         col = "yellow",
         type = "p",
         pch = 20,
         cex = 0.5,
         cex.main = 0.9,
         main = "Payers' ratings and their market values",
         xlab = "Market value in millions",
         ylab = "Player rating")
    
    # Add the players from the defined country to the plot in a different colour
    points(Fifa_data$Market_Value[id], Fifa_data$Player_Rating[id],
           pch = 4,
           cex = 0.8, 
           col = "darkblue")
    
    # Add a legend to the plot
    legend("bottomright", legend=c(Nationality_Players, 
                                   "Other countries"),
           col=c("darkblue", "yellow"), pch = c(4, 20))
  }  
  
  # Request user to enter a valid input if initial input is invalid
  else {stop("Please make a valid input")}
  
}

# User can change the input according to a country of choice (e.g. Croatia)
marketvalue_by_Nationality("Croatia")

###########################################################################
###########################################################################
###                                                                     ###
###               6: 3D Analysis                                        ###
###                                                                     ###
###########################################################################
###########################################################################

# Generate a 3D analysis
# Analyze players' ratings,  market value and  wage of a specific country (user can enter input)
# Visualize the resulting values in a 3D plot

# Define a function that searches for the market values, the wages and ratings of players of a defined country (user can choose the country)
rating_by_Nationality <- function (Nationality_Players){
  
  if(Nationality_Players %in% Fifa_data$Nationality) {
    
    Fifa_data$country_players <- as.character(Fifa_data$Nationality == Nationality_Players)
    Fifa_data$country_players
    
    # Identify the players that match the input
    id <- Fifa_data$country_players == "TRUE"
    
    # Plot the players' ratings against the players' market values and wages
    x <- Fifa_data$Market_Value[id]
    y <- Fifa_data$Wage_in_k[id]
    z <- Fifa_data$Player_Rating[id]
    
    scatter3D(x, y, z, phi = 0, bty = "g",  type = "h", 
              ticktype = "detailed", pch = 19, cex = 0.5,cex.lab=0.7, cex.axis=0.5,
              xlab = "Value in millions", ylab = "Wage in thousands", zlab = "Player rating", 
              main = c(Nationality_Players, "Player rating, market value and wage analysis"), cex.main = 0.9)
    
  }  
  
  # Request user to enter a valid input if initial input is invalid
  else {stop("Please make a valid input")}
  
}

# Change input according to a country of choice (e.g. France)
rating_by_Nationality("France")

###########################################################################
###########################################################################
###                                                                     ###
###               7: Boxplot of age distribution                        ###
###                                                                     ###
###########################################################################
###########################################################################

# Analyze the median, the minimum and  maximum age of a specific country (input can be chosen)
# Show the distribution of the age in an interactive boxplot

# Define a function that searches for the age of players of a defined country
Average_Age_by_Nationality <- function (Nationality_Players){
  
  if(Nationality_Players %in% Fifa_data$Nationality) {
    
    Fifa_data$country_players <- as.character(Fifa_data$Nationality == Nationality_Players)
    Fifa_data$country_players
    
    # Identify the players that match the input
    id <- Fifa_data$country_players == "TRUE"
    
    # Print min, max and mean age for the given country
    print(min(Fifa_data$Age[id]))
    print(mean(Fifa_data$Age[id]))
    print(max(Fifa_data$Age[id]))
  }  
  
  # Request user to enter a valid input if initial input is invalid
  else {stop("Please make a valid input")}
  
  # Plot the age distribution in an interactive boxplot
  p <- plot_ly(data.frame(Fifa_data$Age[id]), type = "box", y = Fifa_data$Age[id], 
               color = I("red"), x = Nationality_Players, marker = list(color = "blue"))
  p
  
}
# Change input according to a country of choice (e.g. Serbia)
Average_Age_by_Nationality("Serbia")

###########################################################################
###########################################################################
###                                                                     ###
###               8: Calculate the highest wage per position            ###
###                                                                     ###
###########################################################################
###########################################################################

# Determine which player has the highest wage on a given position and what the wage is (input can be chosen)

# Define a function that searches for the wage of a player on a given position
max_wage_by_position <- function (Input_Position){
  
  if(Input_Position %in% Fifa_data$Position) {
    
    Fifa_data$right_position <- as.character(Fifa_data$Position == Input_Position)
    Fifa_data$right_position
    
    # Identify the players that match the input 
    id <- Fifa_data$right_position == "TRUE"
    
    # Indentify the highest wage for the given input
    max_id <- (max(Fifa_data$Wage_in_k[id], na.rm = TRUE))
    
    # Identify the name of the player with the highest wage
    find_row <- which(Fifa_data$Wage_in_k == max_id & Fifa_data$Position == Input_Position)
    
    # Print max wage and the name of the player that matches the input position
    print(max_id)
    print(Fifa_data$Name[find_row])  
    
  }  
  
  # Request user to enter a valid input if initial input is invalid
  else {stop("Please make a valid input")}
  
}

# Print all valid positions which exist and can be entered in function by the user
print(unique(Fifa_data$Position))

# Choose a position and find out which player has the highest wage and what the wage is (in thousands)
max_wage_by_position("LCM")


###########################################################################
###                                                                     ###
###               9: Market value and wage per club                     ###
###                                                                     ###
###########################################################################
###########################################################################

# Determine the average market value and age for a specifiy club (input can be chosen)
# Plot the players' market value and wage for a specific club

# Define a function that searches for the market value and the wage of players for a specific club
Value_wage_by_club <- function (Input_Club){
  
  if(Input_Club %in% Fifa_data$Club) {
    
    Fifa_data$right_club <- as.character(Fifa_data$Club == Input_Club)
    Fifa_data$right_club
    
    # Identify the players that match the input
    id <- Fifa_data$right_club == "TRUE"
    
    # Identify the average market value and wage for the specified club and round the number
    value_id <- round((mean(Fifa_data$Market_Value[id], na.rm = TRUE)), digits = 1)
    wage_id <- round((mean(Fifa_data$Wage_in_k[id], na.rm = TRUE)), digits = 1)
    
    # Plot the players' market value and wage
    plot(x = Fifa_data$Wage_in_k[id], 
         y = Fifa_data$Market_Value[id], 
         main = c(Input_Club, "Market value and wage"),
         ylab = "Market value (in millions)",
         xlab = "Wage (in thousands)",
         ylim = c(0,120),
         pch = 20,
         col = "blue")
    
    # Add a legend stating the averages
    legend("topleft", legend = c(paste("Average market value: ", value_id, "millions"),
                                 paste("Average wage: ", wage_id, "thousand")))
    
  }  
  
  # Request user to enter a valid input if initial input is invalid
  else {stop("Please make a valid input")}
  
}

# Choose a club to see the market values and the wages of the players (e.g. Arsenal)
Value_wage_by_club("Arsenal")


###########################################################################
###########################################################################
###                                                                     ###
###         10: World map with average player ratings per country       ###
###                                                                     ###
###########################################################################
###########################################################################

# Give an overview of the average Fifa rating per country on a world map

# Rename de column "Nationality" to "region" in the Fifa data set, 
# in order to match the name of the variable with the map_data("world") data set
colnames(Fifa_data)[3] = "region"

# Specify which package to use in case of conflict
conflict_prefer("mutate", "dplyr")
conflict_prefer("summarize", "dplyr")

# Change the country name in the Fifa_data to the same name as in the other data set
Score_Country <- Fifa_data %>% 
  mutate(region = ifelse(region == "United States", "USA", region)) %>%
  mutate(region = ifelse(region == "China PR", "China", region)) %>%
  mutate(region = ifelse(region == "England", "UK", region)) %>%
  mutate(region = ifelse(region == "Wales", "UK", region)) %>%
  mutate(region = ifelse(region == "Scotland", "UK", region)) %>%
  mutate(region = ifelse(region == "Republic of Ireland", "Ireland", region)) %>%
  mutate(region = ifelse(region == "Northern Ireland", "Ireland", region)) %>%
  mutate(region = ifelse(region == "DR Congo", "Democratic Republic of the Congo", region)) %>%
  mutate(region = ifelse(region == "Congo", "Republic of Congo", region)) %>%
  mutate(region = ifelse(region == "Korea Republic", "South Korea", region)) %>%
  mutate(region = ifelse(region == "Korea DPR", "North Korea", region)) %>%
  mutate(region = ifelse(region == "Central African Rep.", "Central African Republic", region)) %>%
  
  # Calculate the mean of the Fifa player rating per region
  select(Player_Rating, region) %>% 
  group_by(region) %>%
  summarize(
    n = n(),
    mean_Player_Rating = mean(Player_Rating, na.rm = TRUE)
  ) %>%
  ungroup()

# Access the coordinates for each country
world_map <- map_data("world")

# Extract Antarctica as not relevant for this data set 
world_map <- subset(world_map, region!="Antarctica")

# Left join world map with mean player rating per country
Average_Score.map <- left_join(Score_Country, world_map, by = "region")

# Create map with countries coloured by mean of the player rating
Average_Score_map <- ggplot(Average_Score.map, aes(map_id = region, fill = mean_Player_Rating)) +
  geom_map(map = Average_Score.map,  color = "black") +
  expand_limits(x = Average_Score.map$long, y = Average_Score.map$lat) +
  scale_fill_viridis_c(option = "C") 

# Create empty map in order to display all countries (also the countries that do not have Fifa players)
Average_Score_map <- Average_Score_map + geom_map(dat=world_map, map = world_map, 
                                                  aes(map_id=region), fill="white", color="black")

# Put filled world map over empty world map
Average_Score_map <- Average_Score_map + geom_map(map = world_map, 
                                                  aes(map_id = region, fill = mean_Player_Rating), 
                                                  colour = "black")

# Fit limits
Average_Score_map <- Average_Score_map + expand_limits(x = world_map$long, y = world_map$lat)
Average_Score_map
