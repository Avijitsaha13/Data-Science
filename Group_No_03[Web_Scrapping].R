library(rvest)
library(dplyr)

link= "https://www.ebay.com/b/Rolex/bn_21834331?LH_BIN=1&rt=nc"
page = read_html(link)

name = page %>% html_nodes(".s-item__title") %>% html_text()
price = page %>% html_nodes(".s-item__price") %>% html_text()
shipping_cost = page %>% html_nodes(".s-item__logisticsCost") %>% html_text()

ebay = data.frame(name, price, shipping_cost, stringsAsFactors = FALSE)
ebay

write.csv(ebay, "ebay.csv")


