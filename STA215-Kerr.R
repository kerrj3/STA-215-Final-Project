 ## Project:  STA 215, Fall 2024, Final Project
 # Located:   Kline TCNJ Google Drive
 # File Name: template
 # Date:      2024_1_17
 # Who:       Zachary D. Kline
 
 
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
 setwd("~/Desktop/STA215")
 dataset <- read_csv("raw_data.csv")
 View(dataset)
 
 ##################################################################################
 ############### Table 1: descriptive statistics    ####################   
 ##################################################################################

table(dataset$instruments)
mean(dataset$instruments)
sd(dataset$instruments)

table(dataset$intended_reaction)
mean(dataset$intended_reaction)
sd(dataset$intended_reaction)

table(dataset$volume)
mean(dataset$volume)
sd(dataset$volume)

table(dataset$services)
mean(dataset$services)
sd(dataset$services)

table(dataset$translations)
mean(dataset$translations)
sd(dataset$translations)

table(dataset$simplicity)
mean(dataset$simplicity)
sd(dataset$simplicity)

table(dataset$monthly_listeners)
mean(dataset$monthly_listeners)
sd(dataset$monthly_listeners)

table(dataset$merch_sold)
mean(dataset$merch_sold)
sd(dataset$merch_sold)

table(dataset$tickets_sold)
mean(dataset$tickets_sold)
sd(dataset$tickets_sold)

table(dataset$duration_listene)
mean(dataset$duration_listene)
sd(dataset$duration_listene)

table(dataset$views_song)
mean(dataset$views_song)
sd(dataset$views_song)

table(dataset$articles)
mean(dataset$articles)
sd(dataset$articles)

table(dataset$speed)
mean(dataset$speed)
sd(dataset$speed)

table(dataset$lyrical_tone...18)
mean(dataset$lyrical_tone...18)
sd(dataset$lyrical_tone...18)

table(dataset$lyrical_tone...19)
mean(dataset$lyrical_tone...19)
sd(dataset$lyrical_tone...19)

table(dataset$year)
mean(dataset$year)
sd(dataset$year)

table(dataset$common_genre)
mean(dataset$common_genre)
sd(dataset$common_genre)

table(dataset$intended_age)
mean(dataset$intended_age)
sd(dataset$intended_age)

table(dataset$awards_won)
mean(dataset$awards_won)
sd(dataset$awards_won)

table(dataset$billboard_hot_100)
mean(dataset$billboard_hot_100)
sd(dataset$billboard_hot_100)

table(dataset$music_video_4)
mean(dataset$music_video_4)
sd(dataset$music_video_4)

table(dataset$unique)
table(dataset$artist)
table(dataset$title)
table(dataset$month_year_top)



 ##################################################################################
 #################### Figure 1: boxplot             ####################   
 ##################################################################################

# Box Plot
boxplot(dataset$intended_age ~ dataset$music_video_4)
anova <- aov((dataset$intended_age ~ dataset$music_video_4))
summary(anova)

boxplot(dataset$merch_sold ~ dataset$music_video_4)
anova <- aov((dataset$merch_sold ~ dataset$music_video_4))
summary(anova)

boxplot(dataset$speed ~ dataset$music_video_4)
anova <- aov((dataset$speed ~ dataset$music_video_4))
summary(anova)

##################################################################################
 ####################   Figure 2: scatter plot             ####################   
 ##################################################################################
dataset_nomissing <- na.omit(dataset[, c("monthly_listeners", "merch_sold")])

linear_plot <- plot(dataset_nomissing$monthly_listeners, dataset_nomissing$merch_sold)

meany <- mean(dataset_nomissing$merch_sold)
meanx <- mean(dataset_nomissing$monthly_listeners)

abline(v = meanx, col = "black")
abline(h = meany, col = "black")

linear_relationship <- lm(merch_sold ~ monthly_listeners, data = dataset_nomissing )
summary(linear_relationship)
abline(linear_relationship)
 ##################################################################################
 ####################  Figure 3: residual plot                ####################   
 ##################################################################################
# Plot the residuals
plot(dataset$monthly_listeners) 
# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")
 

 ##################################################################################
 ####################  Table 2: contingency table                ####################   
 ##################################################################################
 table(dataset$simplicity, dataset$music_video_4)
 
 chisq.test(table(dataset$simplicity, dataset$music_video_4))
 
