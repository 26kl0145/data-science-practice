## Paste the plots you would like to keep here

ggplot(data = vgsales, aes(x = Year, fill = Genre)) +
  geom_bar() +
  labs(title = "Amount of top selling games + genre released each year")

## Practice subsetting data
# use a combination of filter, select, mutate, arrange, summarise, group_by, sample, and/or slice

vgsales_top50 <- slice_head(vgsales, n = 50)
Publisher_Sales <- summarise(group_by(vgsales_top50, Publisher),
                             mean_Global_Sales = mean(Global_Sales),
                             count = n())

ggplot(data = Publisher_Sales, aes(x = Publisher, y = mean_Global_Sales, fill = count)) +
  geom_bar(stat = "summary",
           fun = "mean")



vgsales_JP <- arrange(vgsales, desc(JP_Sales))
View(vgsales_JP)
       