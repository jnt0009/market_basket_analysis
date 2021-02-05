## Written by Jamarius Taylor ##
#                              #
#         02/04/2021           #
#                              #
################################
if(!require(pacman)) install.packages("pacman");
pacman::p_load(
  'googleAnalyticsR',  ## Google Analytics API
  'dplyr', ## Wrangler
  'tidyr', ## wrangler
  'tidyverse',
  'arules', ## MBA functions
  'arulesViz' ## MBA viz
)

folder <- 'C:/Users/Jamarius.Taylor/Desktop/Projects/DSCommunity/MBA'
setwd(folder)


focus <- ga_account_list() %>% 
  filter(viewName == 'TOG-MTO - Unfiltered') %>%
  print()  ## Pick you GA view

gaId <- focus$viewId  ## GA ID

df <- google_analytics(gaId, 
                 date_range = c("2020-01-01", "2020-01-31"), 
                 metrics = c("itemQuantity", 'itemRevenue'), 
                 dimensions = c('productName', 'transactionId'),
                 anti_sample = TRUE
                 ) %>% print()

## items in a cart
df %>% 
  group_by(transactionId) %>%
  summarise_(n = n()) %>% 
  View()


# sdf <- df %>% filter(transactionId == 'Party - 11605742')

## Item price and uncount
df <- df %>%
  mutate(
    itemCost = itemRevenue / itemQuantity
  ) %>% 
  uncount(weights = itemQuantity) %>%
  print()

# transform items to a comma seperated list
df <- plyr::ddply(df,c('transactionId'),
      function(tf1)paste(tf1$productName,
                         collapse = ',')) %>%
  print()

## Seperate items to columns
df <- tidyr::separate(df,'V1',
                into = paste('item',1:70,sep = "_"),
                sep = ',')
write.csv(df, 'ga_basket.csv')

items <- read.transactions('ga_basket.csv', format = 'basket', sep = ",") # read in the basket information
summary(items)

itemFrequencyPlot(items,topN=10,type = "absolute")  ## top 10 viz

rules <- apriori(items, parameter = list(supp=0.01, conf=0.05))  ## 
summary(rules)
rules <- sort(rules, by='confidence', decreasing = TRUE)
summary(rules)
inspect(rules[1]) ## Shows the number 1 rule 

topRules <- rules[1:10]  ## top 10 rules 
inspect(rules)
plotly_arules(rules)
plot(topRules, method="graph")

