## Project:  STA 215, Spring 2024, Final Project
# Located:   Coster TCNJ Google Drive
# File Name: template
# Date:      2024_3_3
# Who:       Adelaide Coster



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
data <- read_delim("raw_data.csv")



##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
mean(data$funny)
sd(data$funny)
min(data$funny)
max(data$funny)
##now enjoyable 

##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
# BOX PLOT
ggplot(data, aes(x =data$romance, y =data$enjoying)) +
  geom_boxplot() +
  labs(title = "Box Plot of gilmore girls data",
       x = "romance",
       y = "enjoying") +
  theme_minimal()


##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
linear_plot <- plot(data$funny, data$enjoying)
print(linear_plot)

# add x line and y line for means
meany <- mean(data$funny)
meanx <- mean(data$enjoying)

abline(h = meanx, col = "black")
abline(v = meany, col = "black")



##### STEP 2: Calculate linear regression line (i.e., slope) and add to scatter plot
linear_relationship <- lm(data$enjoying ~ data$funny, data = data)
summary(linear_relationship)

# Add the linear regression line to the scatter plot
# NOTE: double check the scatter plot is currently in your utilities window!
abline(linear_relationship, col = "red")






linear_plot <- plot (data$funny, data$romance)
print(linear_plot)

meany <- mean(data$romance)
meanx <- mean(data$funny)

abline(h=meany, col = "black")
abline(v=meanx, col = "black")

linear_relationship <- lm(romance ~funny, data=data)
summry (linear_relationship)
##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################


# Plot the residuals
plot(data$funny, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")
##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(data$funny,data$enjoying)
