## Paste the plots you would like to keep here

ggplot(data = vgsales, aes(x = Year, fill = Genre)) +
  geom_bar() +
  labs(title = "Amount of top selling games + genre released each year")

## Practice subsetting data
# use a combination of filter, select, mutate, arrange, summarise, group_by, sample, and/or slice

vgsales_top50 <- slice_head(vgsales, n = 50)
# Publisher_Sales <- summarise(group_by(vgsales_top50, Publisher),
                             # mean_Global_Sales = mean(Global_Sales),
                             # count = n())

Publisher_Sales <- vgsales_top50 %>%
  group_by(Publisher) %>%
  summarise(mean_Global_Sales = mean(Global_Sales),
            count = n()) # data frame of grouped summaries
View(Publisher_Sales)

ggplot(data = Publisher_Sales, aes(x = Publisher, y = mean_Global_Sales, fill = count)) +
  geom_bar(stat = "summary",
           fun = "mean") +
  labs(title = "Average Sales by Publisher of Top 50 Highest Selling Games",
       y = "Average Sales (millions)")

#ggplot(data = Publisher_Sales, aes(x = Publisher)) +
#  geom_bar(stat = "count")



Nintendo_Games <- filter(vgsales, Publisher == "Nintendo")
View(Nintendo_Games)
Nintendo_Games2 <- filter(vgsales, Publisher %in% "Nintendo")
View(Nintendo_Games2)

ggplot(Nintendo_Games, aes(x = NA_Sales, y = EU_Sales)) +
  geom_line(color = "dark blue")



vgsales2 <- mutate(vgsales,
                   non_NA_Sales = Global_Sales - NA_Sales,
                   non_EU_Sales = Global_Sales - EU_Sales)
View(vgsales2)



Game_Sales <- select(vgsales, Name, ends_with("Sales"))
View(Game_Sales)


Mario_Games <- filter(vgsales, grepl('Mario', Name))
View(Mario_Games)



vgsales_JP <- arrange(vgsales, desc(JP_Sales))
View(vgsales_JP) # sorts by highest selling games in JP



mostly_NA_Games <- filter(vgsales, NA_Sales*2 > Global_Sales)
View(mostly_NA_Games)




## Practice using t-tests

Electronic_Arts <- filter(vgsales, Publisher == "Electronic Arts")
Activision <- filter(vgsales, Publisher == "Activision")

t.test(Electronic_Arts$Global_Sales, Activision$Global_Sales)


vgsales_top100 <- slice_head(vgsales, n = 100)
Global_Sales_anova <- aov(data = vgsales_top100, Year ~ Publisher) # comparing publisher by 
# average year that they released their games
# I think
summary(Global_Sales_anova)
TukeyHSD(Global_Sales_anova)
