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



Nintendo_Games <- filter(vgsales, Publisher == "Nintendo")
View(Nintendo_Games) # works
#Nintendo_Games2 <- filter(vgsales, "Nintendo" %in% Publisher)
#View(Nintendo_Games2) this isn't sorting out non-Nintendo publishers

ggplot(Nintendo_Games, aes(x = NA_Sales, y = EU_Sales)) +
  geom_line()



vgsales2 <- mutate(vgsales,
                   non_NA_Sales = Global_Sales - NA_Sales,
                   non_EU_Sales = Global_Sales - EU_Sales)
View(vgsales2)



Game_Sales <- select(vgsales, Name, ends_with("Sales"))
View(Game_Sales)



vgsales_JP <- arrange(vgsales, desc(JP_Sales))
View(vgsales_JP) # sorts by highest selling games in JP

