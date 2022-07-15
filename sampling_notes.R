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
