#### Load in csv file ####
vgsales <- read.csv("data/vgsales.csv")
View(vgsales)
saveRDS(vgsales, "data/vgsales.RDS")

### Load in required datasets
library(ggplot2)
library(dplyr)

## Practice plotting with your dataset
ggplot(data = vgsales, aes(x = Genre, y = NA_Sales)) +
  geom_violin()

ggplot(data = vgsales, aes(x = Genre, y = Platform)) +
  geom_count()

#ggplot(data = vgsales, aes(x = as.factor(Year), y = ..count..)) +
#  geom_density() brings up a lot of graphs. Probably expects continuous

ggplot(data = vgsales, aes(x = Year, fill = Genre)) +
  geom_bar() +
  labs(title = "Games released each year")

#ggplot(data = vgsales, aes(x = Year)) +
#  geom_histogram() Year uses discrete values and histogram expects continuous. Barplots are better fit

vgsales100 <- read.csv("data/vgsales.csv")[1:100]

ggplot(data = vgsales, aes(x = NA_Sales, y = EU_Sales)) +
  geom_point() +
  xlim(0, 10)
