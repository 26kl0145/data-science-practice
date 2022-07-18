## Z scores and confidence intervals
## let's say we eat 100 boxes of cookie and find the average
## number of cookies in a box is 38.2

sample_mean = 38.2
pop_mean = 40 # what our null hypothesis is testing against
sd = 10 # standard deviation
n = 100 # total sample size

z = (sample_mean - pop_mean) / (sd/sqrt(n))
z

# our sample mean is 1.8 standard deviations away from the sample pop mean


# one sample t-test
mean_sepal_length <- mean(iris$Sepal.Length)
mean_sepal_length

random_sample <- sample_n(iris, 30)
random_sample

# null hypothesis
# random sample mean == population mean (5.84333)

# alternate hypothesis
# random sample mean != population mean (5.84333)

random_sample_sep_len <- random_sample$Sepal.Length

t.test(mu = mean_sepal_length, x = random_sample_sep_len)

setosa <- filter(iris, Species == "setosa")
setosa_sep_len <- setosa$Sepal.Length

t.test(mu = mean_sepal_length, x = setosa_sep_len)



sleep_hours <- c(5,5,5,6,6,7,7,7,8,9)
mean(sleep_hours)
sd(sleep_hours)

t.test(sleep_hours, mu = 7, alternative = "less")

# wrong direction
t.test(sleep_hours, mu = 7, alternative = "greater")

## Using iris dataset
pop_mean <- mean(iris$Sepal.Length)

setosa <- filter(iris, Species == "setosa")

t.test(setosa$Sepal.Length, mu = pop_mean)

# If the sample mean (of setosa sepal lengths) is 5.84 (the same as the pop mean),
# then .000000000000002% of the time, this difference (or more) will happen by chance

### Activity
# Choose a different numeric variable (sepal width, petal length, petal width) and compare
# any of the 3 species to the population mean (setosa, versicolor, or virginica)
# Calculate the p-value using a one-sample t-test

versicolor <- filter(iris, Species == "versicolor")

t.test(versicolor$Sepal.Width, mu = pop_mean)
# the p-value of 2.2e-16 is less than 0.05, so there is a significant difference and this data
# supports the alternate hypothesis


### two-sample t-test
versicolor <- filter(iris, Species == "versicolor")
setosa <- filter(iris, Species == "setosa")

t.test(versicolor$Sepal.Length, setosa$Sepal.Length)

#(mean of setosa sepal lengths) - (mean of versicolor sepal lengths) != 0
#mean of setosa sepal lengths != mean of versicolor sepal legnths

## Compare versicolor and virginica sepal lengths. What is the p-value? Is it significant?
versicolor <- filter(iris, Species == "versicolor")
virginica <- filter(iris, Species == "virginica")

t.test(versicolor$Sepal.Length, virginica$Sepal.Length)
# the p-value is 1.866e-07, so there is a significant difference and it 
# supports the alternate hypothesis


## paired t-test
install.packages("datarium")
library(datarium)
?mice2
mice2

t.test(mice2$before, mice2$after, paired = T)

# p-value of 1.039e-09 is less than 0.05, so there is a significant difference
# compares before and after



### ANOVA
sepal_len_anova <- aov(data = iris, Sepal.Length ~ Species) # does Sepal Length change when we compare it based on Species
# numeric variable compared by the category

# Are any categories different?
summary(sepal_len_anova)

# Which groups are significantly different?
TukeyHSD(sepal_len_anova)

# Answer: all of them! woww!!

sepal_width_anova <- aov(data = iris, Sepal.Width ~ Species)
summary(sepal_width_anova)
TukeyHSD(sepal_width_anova)

## Let's look at diamonds dataset
View(diamonds)
diamond_price_color <- aov(data = diamonds, price ~ color)
summary(diamond_price_color)
# to save results, use $cat_var
signif_results <- TukeyHSD(diamond_price_color)$color
# convert to dataframe so we can use dplyr functions
arrange(as.data.frame(signif_results), `p adj`)
