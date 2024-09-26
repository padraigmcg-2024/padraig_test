install.packages("gapminder")

# Load Require Libraries
library(tidyverse)
library(readxl)
library(rstatix)
library(effectsize)
library(plotly)
library(gapminder)

# Set path to data file
xls_file <- "Coded_Data.xlsx"

# Read in the Excel data file
rel_distance <-read_excel(xls_file)

# Convert file to a data frame
dfdata <- data.frame(rel_distance)

# List the variables in the dataset
str(dfdata)

#Calculate summary statistics for Relative Distance
summary(dfdata$Relative_Distance)

my_data <- dfdata %>%
  group_by(Game.Type) %>%
  summarise(
    n=n(),
    mean=mean(Relative_Distance),
    sd=sd(Relative_Distance)
      ) %>%
    mutate(se=sd/sqrt(n)) %>%
    mutate(ic=se*qt((1 - 0.05)/2 +.5, n-1))

p <- ggplot(my_data) +
  geom_bar( aes(x=Game.Type, y=mean), stat="identity", fill="#585858", alpha=0.5, width=0.5) +
  geom_errorbar( aes(x=Game.Type, ymin=mean-ic, ymax=mean+ic), width=0.15, colour="#000000", alpha=0.9) +
  ggtitle("Mean Relative Distance by Game Type") +
  xlab("Game Type") + ylab("Mean Relative Distance (m/min)") +
  theme (
    plot.title = element_text(color="black", size=14, family="sans", face="bold", hjust=0.5),
    axis.title = element_text(color="black", size=12, family="sans",face="bold"),
    axis.line = element_line(size=0.6), 
    axis.text = element_text(color="black", size=11, family="sans",face="italic"),
  )+
 scale_y_continuous(expand = c(0, 0))

p

dfdata %>%
    group_by(Game.Type) %>%
    shapiro_test(Relative_Distance)

result <- aov(dfdata$Relative_Distance ~ dfdata$Game.Type, data=dfdata)
parameters::model_parameters(anova(result))
options(es.use_symbols = TRUE)
eta_squared(result)

summary(result)

TukeyHSD(result)

margin.error <- t.score * sample.se

lower.bound <- sample.mean - margin.error
upper.bound <- sample.mean + margin.error

table(dfdata$Game.Type)

