##Loading the libraries

library(ggplot2)
library(dplyr)
library(tidyr)
library(stats)
library(corrplot)
library(ggplot2)
library(reshape2)

## Loading the data with the read.csv function
data1 <- read.csv("fortune1000_2024DS1.csv")
head(data1)
colnames(data1)
data2 <- read.csv("data.csv")
colnames(data2)
head(data2)
##  Inspect and clean the data
## Check for NA values
#________________________________________________________________________________________________
#________________________________________________________________________________________________
summary(data1)
summary(data2)
##We see that there are some NA values mostly numerical.
### Question for the professors should we use the mean value for NA or should we delete them.

#cleaned_data2 <- na.omit(data2)

## Remove rows with any NA values

#cleaned_data1 <- na.omit(data1)

names(data1)[names(data1) == "Ticker"] <- "ticker"
data1$ticker <- tolower(data1$ticker)

# test for left NA values
summary(data1)
summary(data2)
#______________________________________________________________________________________________
#______________________________________________________________________________________________
#dim(cleaned_data1)
#dim(cleaned_data2)

## Merging both data frames
merged_df <- merge(data1,data2, all.x= TRUE, by="ticker")
#_________________________________________________________________________________________________
#_________________________________________________________________________________________________

# How does the mean total_score of ESG differ if the CEO is Female

average_scores_sector <- merged_df %>% # Grouping by sector and FemaleCEO and creating a new variable
  group_by(Sector, FemaleCEO) %>%
  summarise(mean_total_score = mean(total_score, na.rm = TRUE)) %>%
  ungroup()  # Ungroup to avoid potential ggplot2 errors

# Plot x the sector and y the mean_total_score and filling with FemaleCEO yes=blue, no=orange
ggplot(average_scores_sector, aes(x = Sector, y = mean_total_score, fill = FemaleCEO)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  labs(title = "Mean Total Score by Sector and Female CEO",
       x = "Sector",
       y = "Mean Total Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        plot.title = element_text(hjust = 0.5))  # Center the title

#_________________________________________________________________________________________________


#_________________________________________________________________________________________________
library(naniar) #To use function vis_miss we call the library naniar
vis_miss(data1) #Missing values of data1 vizualized
vis_miss(data2) #Missing values of data2 vizualized
#________________________________________________________________________________________________
#________________________________________________________________________________________________

#Females as CEO in %
ggplot(data = merged_df, aes(x = Sector, fill = FemaleCEO)) +
  geom_bar(position = "fill") +
  labs(title = "Percentage of Female CEOs by Sector", x = "Sector", y = "Percentage") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#______________________________________________________________________________________________
#______________________________________________________________________________________________
# #Revenues vs Profit
# ggplot(data = merged_df, aes(x = Revenues_M, y = Profits_M)) +
#   geom_point(color = "blue", alpha = 0.7) +
#   labs(title = "Revenues vs. Profits", x = "Revenues (in millions)", y = "Profits (in millions)") +
#   scale_x_continuous(labels = scales::comma) +
#   scale_y_continuous(labels = scales::comma) +
#   theme_minimal()+
# scale_y_log10()
# #_____________________________________________________________________________________________
# #_____________________________________________________________________________________________


#market cap vs. Assets
ggplot(data = merged_df, aes(x = MarketCap_Updated_M, y = Assets_M)) +
  geom_point(color = "purple", alpha = 0.7) +
  labs(title = "Market Cap Updated vs. Assets", x = "Market Cap (in millions)", y = "Assets (in millions)") +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()+
  scale_y_log10()


#____________________________________________________________________________________________
#____________________________________________________________________________________________
# # Employee Numbers vs. Rank
# ggplot(data = merged_df, aes(x = Number_of_employees, y = Rank)) +
#   geom_point(color = "red", alpha = 0.7) +
#   labs(title = "Number of Employees vs. Rank", x = "Number of Employees", y = "Rank") +
#   scale_x_continuous(labels = scales::comma) +
#   scale_y_reverse() +  # Reverse scale since rank 1 is the highest
#   theme_minimal()
# #__________________________________________________________________________________________
# #__________________________________________________________________________________________


##Map of US
# Load necessary libraries
library(dplyr)
library(leaflet)

# Data frame with US state capitals
d.stations <- data.frame(
  State = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
            "Connecticut", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", 
            "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", 
            "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", 
            "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
            "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", 
            "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
            "Vermont", "Virginia", "West Virginia", "Wisconsin", "Wyoming"),
  
  Capital = c("Montgomery", "Juneau", "Phoenix", "Little Rock", "Sacramento", "Denver", 
              "Hartford", "Tallahassee", "Atlanta", "Honolulu", "Boise", "Springfield", 
              "Indianapolis", "Des Moines", "Topeka", "Frankfort", "Baton Rouge", "Augusta", 
              "Annapolis", "Boston", "Lansing", "Saint Paul", "Jackson", "Jefferson City", 
              "Helena", "Lincoln", "Carson City", "Concord", "Trenton", "Santa Fe", 
              "Raleigh", "Bismarck", "Columbus", "Oklahoma City", "Salem", "Harrisburg", 
              "Providence", "Columbia", "Pierre", "Nashville", "Austin", "Salt Lake City", 
              "Montpelier", "Richmond", "Charleston", "Madison", "Cheyenne"),
  
  Latitude = c(32.3792, 58.3019, 33.4484, 34.7465, 38.5816, 39.7392, 
               41.7658, 30.4383, 33.7490, 21.3070, 43.6150, 39.7980, 
               39.7684, 41.5868, 39.0489, 38.2009, 30.4515, 44.3106, 
               38.9784, 42.3601, 42.7325, 44.9537, 32.2988, 38.5767, 
               46.5884, 40.8136, 39.1638, 43.2073, 40.2171, 35.6870, 
               35.7796, 46.8083, 39.9612, 35.4676, 44.9429, 40.2732, 
               41.8236, 34.0007, 44.3683, 36.1627, 30.2672, 40.7608, 
               44.2601, 37.5385, 38.3498, 43.0731, 41.1400),
  
  Longitude = c(-86.3077, -134.4197, -112.0740, -92.2896, -121.4944, -104.9903, 
                -72.6734, -84.2807, -84.3880, -157.8583, -116.2023, -89.6502, 
                -86.1581, -93.6250, -95.6770, -84.8733, -91.1871, -69.7795, 
                -76.4922, -71.0589, -84.5555, -93.0933, -90.1848, -92.1735, 
                -112.0188, -96.7026, -119.7674, -71.5376, -74.7429, -105.9388, 
                -78.6382, -100.7837, -82.9988, -97.5164, -123.0351, -76.8844, 
                -71.4128, -81.0348, -100.3500, -86.7816, -97.7431, -111.8910, 
                -72.5763, -77.4336, -81.6326, -89.4008, -104.8202))

# Combine State and Capital into a single Label
d.stations <- d.stations %>%
  mutate(Label = paste(Capital, State, sep = ", "))

# Creating the map with Leaflet
leaflet(d.stations) %>%
  addTiles() %>%
  addMarkers(lng = ~Longitude, lat = ~Latitude,
             label = d.stations$Label,
             labelOptions = labelOptions(noHide = TRUE))
# setdiff(d.stations$State, merged_df$HeadquartersCity)


#_________________________________________________________________________________________________
#_________________________________________________________________________________________________

## Correlation Analysis
## We will first select the numeric columns that are relevant for correlation analysis.
## These columns were chosen based on an initial inspection using summary statistics.

numeric_columns <- merged_df %>%
  select(Rank,                ## The rank of the company
         Revenues_M,           ## Revenue in millions
         RevenuePercentChange, ## Percentage change in revenue
         Profits_M,            ## Profits in millions
         ProfitsPercentChange, ## Percentage change in profits
         MarketCap_Updated_M,  ## Updated market capitalization in millions
         Assets_M,             ## Total assets in millions
         Number_of_employees,  ## Number of employees
         Change_in_Rank)       ## Change in rank of the company

# numeric_cleaned <- na.omit(numeric_columns)

#_______________________________________________________________________________________________
#_______________________________________________________________________________________________

## Calculate the correlation matrix for the selected numeric columns.
## We used 'use = "pairwise.complete.obs"' ensures that correlations are calculated 
## using all pairs of columns that have complete data (i.e., no NA values for the pair).

cor_matrix <- cor(numeric_columns, use= "pairwise.complete.obs")

## Print the correlation matrix to understand the strength and direction of the relationships between the variables.
print(cor_matrix)



#____________________________________________________________________________________________
#____________________________________________________________________________________________

# Converting the correlation matrix into a long format (also known as tidy format)
# This is necessary because ggplot2 requires the data to be in long format to plot
# 'melt()' transforms the matrix into a data frame where each row is a variable pair and its correlation value

cor_matrix_melted <- melt(cor_matrix)

#____________________________________________________________________________________________
# Creating the correlation plot using ggplot2

cor_plot <- ggplot(cor_matrix_melted, aes(x = Var1, y = Var2, fill = value)) +
  
  # Creating a tile plot (heatmap) where each tile represents a correlation between two variables
  geom_tile(color = "white") + 
  
  # Defining the color gradient for the heatmap
  # Red represents negative correlations, blue represents positive correlations, 
  # and white is neutral (0 correlation)
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") + 
  
  # Applying a minimal theme to reduce visual clutter
  theme_minimal() + 
  
  # Customizing the x-axis text (variable names) by rotating them for better readability
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1)) + 
  
  # Ensuring that the aspect ratio of the plot is fixed so the tiles are square
  coord_fixed() + 
  
  # Adding the correlation values as text labels on each tile
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) + 
  
  # Adding a title to the plot and remove axis labels (since we are using variable names as axis labels)
  labs(title = "Correlation Matrix of Company Performance Metrics",
       x = "", y = "")

# Printing the plot
print(cor_plot)


# Rank and Revenues_M (-0.50): A moderately negative correlation, suggesting that 
# companies with higher revenues tend to have better ranks (lower rank numbers).
# Rank and Profits_M (-0.34): A weak to moderate negative correlation, indicating 
# that companies with higher profits are more likely to have better ranks (lower numbers).
# Revenues_M and Profits_M (0.66): A strong positive correlation, showing that companies
# with higher revenues tend to have higher profits.
# Revenues_M and Number_of_employees (0.73): A strong positive correlation, which 
# indicates that companies with higher revenues tend to have more employee.
# Profits_M and MarketCap_Updated_M (0.81): A very strong positive correlation, 
# suggesting that companies with higher profits tend to have a larger market capitalization.
# MarketCap_Updated_M and Assets_M (0.88): A very strong positive correlation, indicating
# that companies with larger assets typically have higher market capitalization.
--------------------##----------------------------##------------------------------------------------
# Higher profitability and revenue are strongly linked with higher market capitalization,
# which aligns with general financial insights that more profitable companies often have a larger market cap.
# Rank is inversely correlated with revenue and profits, suggesting that better-ranked
# companies (lower numbers) tend to generate more revenue and profits.
# A high number of employees tends to correlate positively with revenue, indicating
# that larger companies (in terms of workforce) tend to generate more revenue, possibly
# reflecting larger operations or broader market reach.
#_____________________________________________________________________________________________
#_____________________________________________________________________________________________

#Try to see what happens if we plot every numerical category against the plot, and see if we have to use the log()
library(ggplot2)
install.packages("plotly")
library(plotly)
# Creating a scatter plot Rank vs. Revenue
rkrev <- ggplot(merged_df, aes(x = Rank, y = Revenues_M)) +
  geom_point(aes(color = Revenues_M, size = Profits_M), alpha = 0.7) +
  scale_y_log10(labels = scales::dollar_format()) +
  scale_color_gradient(low = "green", high = "purple") +
  labs(
    title = "Relationship Between Company Rank and Revenue",
    subtitle = "2024 Fortune 1000 Companies",
    x = "Company Rank",
    y = "Revenue (Log Scale)",
    color = "Revenue ($)",
    size = "Profits ($)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right",
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90")
  )
interactive_rkrev <- ggplotly(rkrev) # Making it interactive with plotly
interactive_rkrev
##

rankprofit <- ggplot(merged_df, aes(x = Rank, y = Profits_M)) +
  geom_point(aes(color = Profits_M, size = Revenues_M), alpha = 0.7) +
  scale_y_log10(labels = scales::dollar_format()) +
  scale_color_gradient(low = "green", high = "purple") +
  labs(
    title = "Relationship Between Company Rank and Profits",
    subtitle = "2024 Fortune 1000 Companies",
    x = "Company Rank",
    y = "Profits (Log Scale)",
    color = "Profits ($)",
    size = "Revenue ($)"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "right",
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_line(color = "gray90")
  )
interactive_rankprofit <- ggplotly(rankprofit) # Making it interactive with plotly
interactive_rankprofit
##__________________________________________________________________________________#
#___________________________________________________________________________________#
#Uncomment to see the plots :Relationship Between Company Rank and Revenue Percent Change
# ggplot(merged_df, aes(x = Rank, y = RevenuePercentChange)) +
#   geom_point(aes(color = RevenuePercentChange, size = RevenuePercentChange), alpha = 0.7) +
#   scale_y_log10(labels = scales::percent_format()) +
#   scale_color_gradient(low = "green", high = "purple") +
#   labs(
#     title = "Relationship Between Company Rank and Revenue Percent Change",
#     subtitle = "2024 Fortune 1000 Companies",
#     x = "Company Rank",
#     y = "Revenue Percent Change (Log Scale)",
#     color = "Revenue % Change",
#     size = "Revenue % Change"
#   ) +
#   theme_minimal(base_size = 15) +
#   theme(
#     plot.title = element_text(hjust = 0.5, face = "bold"),
#     plot.subtitle = element_text(hjust = 0.5),
#     legend.position = "right",
#     panel.grid.major = element_line(color = "gray80"),
#     panel.grid.minor = element_line(color = "gray90")
#   )
##_____________________________________________________________________________#
##_____________________________________________________________________________#
# Uncomment to see the plot: Relationship Between Company Rank and Profits Percent Change
# ggplot(merged_df, aes(x = Rank, y = ProfitsPercentChange)) +
#   geom_point(aes(color = ProfitsPercentChange, size = ProfitsPercentChange), alpha = 0.7) +
#   scale_y_log10(labels = scales::percent_format()) +
#   scale_color_gradient(low = "green", high = "purple") +
#   labs(
#     title = "Relationship Between Company Rank and Profits Percent Change",
#     subtitle = "2024 Fortune 1000 Companies",
#     x = "Company Rank",
#     y = "Profits Percent Change (Log Scale)",
#     color = "Profits % Change",
#     size = "Profits % Change"
#   ) +
#   theme_minimal(base_size = 15) +
#   theme(
#     plot.title = element_text(hjust = 0.5, face = "bold"),
#     plot.subtitle = element_text(hjust = 0.5),
#     legend.position = "right",
#     panel.grid.major = element_line(color = "gray80"),
#     panel.grid.minor = element_line(color = "gray90")
#   )
##____________________________________________________________________________#
#_____________________________________________________________________________#
# Uncomment to see the plot: Relationship Between Company Rank and Market Capitalization
# ggplot(merged_df, aes(x = Rank, y = MarketCap_Updated_M)) +
#   geom_point(aes(color = MarketCap_Updated_M, size = MarketCap_Updated_M), alpha = 0.7) +
#   scale_y_log10(labels = scales::comma_format()) +
#   scale_color_gradient(low = "green", high = "purple") +
#   labs(
#     title = "Relationship Between Company Rank and Market Capitalization",
#     subtitle = "2024 Fortune 1000 Companies",
#     x = "Company Rank",
#     y = "Market Capitalization (Million USD, Log Scale)",
#     color = "Market Cap (M)",
#     size = "Market Cap (M)"
#   ) +
#   theme_minimal(base_size = 15) +
#   theme(
#     plot.title = element_text(hjust = 0.5, face = "bold"),
#     plot.subtitle = element_text(hjust = 0.5),
#     legend.position = "right",
#     panel.grid.major = element_line(color = "gray80"),
#     panel.grid.minor = element_line(color = "gray90")
#   )
##___________________________________________________________________________##
##___________________________________________________________________________##


# Please uncomment if you want to see the plots, we didn't use all of them due to
#  page requirement.
# ggplot(merged_df, aes(x = Rank, y = Assets_M)) +
#   geom_point(aes(color = Assets_M, size = Assets_M), alpha = 0.7) +
#   scale_y_log10(labels = scales::comma_format()) +
#   scale_color_gradient(low = "green", high = "purple") +
#   labs(
#     title = "Relationship Between Company Rank and Total Assets",
#     subtitle = "2024 Fortune 1000 Companies",
#     x = "Company Rank",
#     y = "Total Assets (Million USD, Log Scale)",
#     color = "Assets (M)",
#     size = "Assets (M)"
#   ) +
#   theme_minimal(base_size = 15) +
#   theme(
#     plot.title = element_text(hjust = 0.5, face = "bold"),
#     plot.subtitle = element_text(hjust = 0.5),
#     legend.position = "right",
#     panel.grid.major = element_line(color = "gray80"),
#     panel.grid.minor = element_line(color = "gray90")
#   )
# ##
# 
# ggplot(merged_df, aes(x = Rank, y = Number_of_employees)) +
#   geom_point(aes(color = Number_of_employees, size = Number_of_employees), alpha = 0.7) +
#   scale_y_log10(labels = scales::comma_format()) +
#   scale_color_gradient(low = "green", high = "purple") +
#   labs(
#     title = "Relationship Between Company Rank and Number of Employees",
#     subtitle = "2024 Fortune 1000 Companies",
#     x = "Company Rank",
#     y = "Number of Employees (Log Scale)",
#     color = "Employees",
#     size = "Employees"
#   ) +
#   theme_minimal(base_size = 15) +
#   theme(
#     plot.title = element_text(hjust = 0.5, face = "bold"),
#     plot.subtitle = element_text(hjust = 0.5),
#     legend.position = "right",
#     panel.grid.major = element_line(color = "gray80"),
#     panel.grid.minor = element_line(color = "gray90")
#   )
#____________________________________________________________________________________________#
#____________________________________________________________________________________________#

# Step 4: Regression Analysis
# Fit a linear model with Rank as the dependent variable
merged_df <- merged_df %>%
  mutate(
    log_Revenues_M = ifelse(Revenues_M > 0, log(Revenues_M), NA),
    log_RevenuePercentChange = ifelse(RevenuePercentChange > 0, log(RevenuePercentChange), NA),
    log_Profits_M = ifelse(Profits_M > 0, log(Profits_M), NA),
    log_ProfitsPercentChange = ifelse(ProfitsPercentChange > 0, log(ProfitsPercentChange), NA),
    log_MarketCap_Updated_M = ifelse(MarketCap_Updated_M > 0, log(MarketCap_Updated_M), NA),
    log_Assets_M = ifelse(Assets_M > 0, log(Assets_M), NA),
    log_Number_of_employees = ifelse(Number_of_employees > 0, log(Number_of_employees), NA)
  )

# Fit the linear model using the log-transformed variables
model <- lm(Rank ~ log_Revenues_M + log_RevenuePercentChange + log_Profits_M + 
              log_ProfitsPercentChange + log_MarketCap_Updated_M + log_Assets_M + 
              log_Number_of_employees, data = merged_df)

# Summary of the model
summary(model)




