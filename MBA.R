# Product Association, How to make a recommended system. And, should the company acquire ElectroIndex?
    # 30 days online transactions in ElectroIndex
    # Item relationships within Electronidex's transactions?
    # Would Blackwell benefit from selling any of Electronidex's items?
    # Should Blackwell acquire Electronidex?
    # What are your recommendations for Blackwell if they acquire this company?

# .....................................................................
    # The best rules are those that are interesting. Not obvious.
    # What should be looked at are rules with low support + high confidence
    # Background thinking, how to people buy? Out of 'curiosity' of a product association?

# .....................................................................

setwd("C:/Users/Violeta/Dropbox/Ubiqum/2_Predicting.Customer.Preference/4_Product.Association")

#Libraries .............................................................
pacman::p_load(arules, arulesViz, ggplot2, dplyr, RColorBrewer)

#Load data ..............................................................
electro <- read.transactions("ElectronidexTransactions2017.csv",
  format = "basket", sep = ",", cols = NULL, rm.duplicates = T, encoding = "unknown")

# What's here ............................................................
dim(electro)
labels(electro) # items inside each transaction -> too long  
nitems(electro) # number of products
itemInfo(electro) # list of items -> too long
summary(electro)

electro <- electro[which(size(electro) != 0)] # take off two empty transactions
summary(electro)  # 9833 trans * 125 items * 0.035 = 43019 products sold in this one month data.
                  # 43019/9833 = 4.37 items purchased on average
                  # Min purchase of 1 item occured 2163 times, and max purchase of 30 items occured 1 time.
                  # 25% of transactions had 2 products, and about 50% had 3 products.
                  # Most freq: iMac(2519), HPLap(1909), Gamer(1809), AppleEarpods(1715), AppleAir(1530)

# breaks = br(0:30) This you will need if erasing empty rows.
hist(size(electro), main = "Size of Transactions", breaks = 30, col = "maroon", 
     xlab = "Amount of Products")
    # if erase two empty rows the histogram combines transactions with 1 and 2 products...

# What products are in the Transaction size 30?
which(size(electro) == 30)
labels(electro[1217]) # items in that transaction

# Top 20 most sold products 
itemFrequencyPlot(electro, topN = 15, horiz = TRUE, type = "relative", 
                  main = "Top15 Products out of 9833 transactions", col = "orange")

# Top 10 Single Transactions 
itemFrequencyPlot(electro[which(size(electro) == 1)], topN = 10, type = "absolute", horiz = T, 
                  main = "Top 10 from 113 products in 2163 single transactions", col = "aquamarine4")

itemFrequency(electro[, 1:10])
itemFrequencyPlot(electro, support = 0.1) # these items appeared in 983 transactions (0.1*9833)
itemFrequencyPlot(electro, support = 0.2) # iMac!That means the iMac appeared in 1966 transactions.
                                          # which means 20% of the transactions had iMac.
itemFrequencyPlot(electro, support = 0.08) # these items appeared in 786 transactions, which is 8% of the trans

# Least purchased items (alternative to itemFrequencyPlot):
barplot(sort(table(unlist(LIST(electro))))[1:10], # LIST returns the transactions, 
                                                  # unlist(LIST) breaks the transactions to individual products, 
                                                  # table() will count the times a product has been purchased.
        horiz = T, las = 1, col = 'pink', xlab = '', main = 'Less frequently purchased items')

barplot(sort(table(unlist(LIST(electro))), decreasing = F)[1:10],
        horiz = T, las = 1, col = 'chocolate1', xlab = "", main = "Most frequently purchased items")


# Simple rule maker and plot:
  # If I want 90 support, then 90/9833 = 0.009. Set Confidence for the rule to 25% (rule has to be correct 25% of the times)
grule <- apriori(electro, parameter = list(support = 0.009, confidence = 0.25, minlen = 2))
summary(grule)
table(is.redundant(grule))
inspect(grule[1:10])
grule <- grule[!is.redundant(grule)]

plot(grule, method = "grouped", measure = "support", control = list(col = sequential_hcl(12))) # sequential_hcl (colorspace library)


# Understand product frequency relations:
crossTable(electro, sort = T)[1:10,1:10]
crossTable(electro, sort = T, measure = "lift")[1:10,1:10]
crossTable(electro, measure = 'chi')['Apple MacBook Air', 'Lenovo Desktop Computer'] # low chi-squared: lift is not random chance


# Separate Single from +1 product Transaction .....................................................  
# 
single <- electro[which(size(electro) == 1)] 
summary(single) # 2163 products purchased - given single transaction data
                # AppleAir(383), AppleEar(156), iMac(121), Gamer(109), ApplePro(67)
View(labels(single))

table(duplicated(single)) # 2050 duplicated transactions - of what products?
                          # 113 unique products
which(labels(single) == "{Kensington Headphones}") # 5 transactions with this item

which(unique(duplicated(single)))  # returns second row..(?)
labels(single[2]) # yes: Cyberpower Gamer 


itemFrequencyPlot(single, support = 0.15) # 15% of transactions are AppleAir
itemFrequencyPlot(single, support = 0.01) # 1% has 21 transactions
sort(itemFrequency(single))[1:20] # item with Freq 0 is because it hasn't been purchased
which(labels(single) == "{XIBERIA Gaming Headset}") # no transactions = 0 frequency

sort(itemFrequency(single), decreasing = F)[1:20] # Top products

# ----------------------- More than one transaction
products <- electro[which(size(electro) != 1)]
summary(products) # 7670 transactions * 125 products *0.042 = 40938 products purchased in one month
                  # iMac(2398), HPLap(1846), Gamer(1700), AppleEar(1559), Lenovo(1415)
                  # 25% of transactions have 3 products, and 50% have 4 products
View(labels(products))
#table(duplicated(products)) # 895 duplicates and 6775 unique
itemFrequencyPlot(products, topN = 25, horiz = T, type = "relative", 
                  main = "Top 25 from 125 products in 7670 transactions", col = "tan2")

min_rule <- apriori(products, parameter = list(confidence = 0.1)) 
inspect(sort(min_rule, by = "count")) # returns top 14 products (without association/ RHS)
                                      # giving the frequency/count by which they appear in the transactions


min_rule2 <- apriori(products, parameter = list(confidence = 0.1, minlen = 2)) 
                                      # need to specify minlen 2 to avoid single frequency

# -------- If I remove duplicates from products .........................................
product_nodupli <- products[!duplicated(products)]
summary(product_nodupli) # 6775 transactions * 125 products * 0.046 = 6900 unique products purchased
                         # iMac(2216), HP(1763), Gamer(1471), AppleEar(1362), Lenovo(1339) 
                         # 25% of transactions have 3 products in basket, and 50% have 5 products
table(duplicated(product_nodupli))
sort(itemFrequency(product_nodupli, type = "absolute"),decreasing = T) # frequency for each item

rule_1 <- apriori(product_nodupli, parameter = list(confidence = 0.1, minlen = 2))
ruleExplorer(rule_1) # explore with Shiny!
inspect(rule_1) # A duplicated rule, but was able to find an association without decreasing confidence


# Mining for frequent itemSets .......................................................
itemsets <- apriori(product_nodupli, parameter = list(support = 0.01, target = 'frequent', minlen = 2))
          # setting target to frequent searches for pairs
summary(itemsets) 
inspect(head(itemsets, n = 25))

quality(itemsets)$lift <- interestMeasure(itemsets, measure = 'lift', product_nodupli)
inspect(head(sort(itemsets, by = 'lift', decreasing = T), n = 10))
  # {Dell Desktop,iMac,Lenovo Desktop Computer,ViewSonic Monitor} / support 0.010 / lift 5.52
  # {Dell Desktop,HP Laptop,iMac,ViewSonic Monitor} / support 0.012 / lift 5.30
  # {HP Laptop,iMac,Lenovo Desktop Computer,ViewSonic Monitor} / support 0.012 / lift 4.744

rule_1 <- apriori(product_nodupli, parameter = list(support = 0.01))
  # weird, because when taking off target, no rules are created with support. But in confidence we had 2

rule_2 <- apriori(product_nodupli, parameter = list(support = 0.001, confidence = 0.95, 
                                                    minlen = 2, maxlen = 5)) # max 5 products to match 50% od transactions
summary(rule_2) # 463 rules
plot(rule_2)
inspect(head(sort(rule_2, by = "lift"), n = 10))


rule_3 <- apriori(product_nodupli, parameter = list(support = 0.001, confidence = 0.30,
                                                    minlen = 2, maxlen = 5),
                  appearance = list(none = c("Dell Desktop", "iMac", "Lenovo Desktop Computer", 
                                             "ViewSonic Monitor", "HP Laptop", "Apple Earpods",
                                             "CYBERPOWER Gamer Desktop"))) # except for these items.

plot(rule_3) # 1410 rules, for 0.1% of the transactions but super high confidence/so 677 transactions
inspect(head(sort(rule_3, by = "lift"), n = 15))
ruleExplorer(rule_3)
inspectDT(rule_3) # Amazing..visualize within R just the rules, AND, it gives sorting options

summary(rule_3)
plot(rule_3, engine = "plotly")
# plot(rule_3, engine = "html") # same as above
inspect(subset(rule_3, items %in% "Eluktronics Pro Gaming Laptop"))
inspect(subset(rule_3, items %in% "Ethernet Cable"))
table(is.redundant(rule_3)) # woohoo! no redundancy

rules_4_df <- as(rule_3, "data.frame") #switch to df if needed

# Create closed frequent itemsets (?) ......................................
closed <- apriori(product_nodupli, parameter = list(target = "closed frequent itemsets", support = 0.005))
closed_rules <- ruleInduction(closed, product_nodupli, control = list(verbose = T))
summary(closed_rules)
inspect(closed_rules, by = "lift")
                    # 2 rules:
                    # {Acer Aspire,Dell Desktop,ViewSonic Monitor} => {HP Laptop}
                    # {Acer Aspire,Dell Desktop,iMac,ViewSonic Monitor} => {HP Laptop} 


# Funky product relationships ..............................................
base <- apriori(product_nodupli, parameter = list(support = 0.001, confidence = 0.001,
                                                  minlen = 2, maxlen = 5))
ruleExplorer(base)


# Or funky with code ..........................................................
#Product dependency on rhs as Apple Earpods ...................................
rulesEarPods <- apriori(products, parameter = list(supp = 0.001,conf = 0.6, minlen = 2), 
                      appearance = list(rhs = "Apple Earpods"))

inspect(rulesEarPods)
plot(rulesEarPods, method = "graph")

# Are there redundant rules?
table(is.redundant(rulesEarPods))


#Product dependency on lhs as Apple Earpods ...................................
rulesEarPods <- apriori(products, parameter = list(supp = 0.001,conf = 0.1, minlen = 2), 
                                                        appearance = list(lhs = "Apple Earpods"))

rulesEarPods <- sort(rulesEarPods, decreasing = TRUE,by = "lift")
inspect(rulesEarPods)

#3-button-mouse dependency ....................................................
rulesMouse <- apriori(products, parameter = list(supp = 0.002, conf = 0.3, minlen = 2),
                     appearance = list(rhs = "3-Button Mouse"))
rulesMouse <- sort(rulesMouse, decreasing = TRUE, by = "lift")
inspect(rulesMouse)



# Upload pdf with product category .........................................
list_prod <- read.csv("Electronidex_itemsList.csv")
str(list_prod)
head(list_prod)

ggplot(list_prod, aes(category)) + geom_bar(fill = "gold1") + coord_flip() +
  ggtitle("Product Diversity", subtitle = "Amount of products offered") +
  xlab("Amount of products") + ylab("")

grouped_prod <- group_by(list_prod, category) %>% tally()
ggplot(grouped_prod, aes(reorder(category, -n), n)) + geom_col(fill = "darkorange") + 
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) + 
  ggtitle("Product Diversity", subtitle = "Amount of products offered") + xlab("") + ylab("Amount")


# Failed to create color..
# my_color <- brewer.pal(5,"Set1")
# levels(my_color) <- levels(grouped_prod$category)
# colScale <- scale_colour_manual(name = "category",values = my_color)


# Frequency of Product Category ............................................................
ggplot(list_prod,aes(reorder(category, -frequency), frequency)) + geom_col(fill = "maroon") +
  ggtitle("Product Types Sold in 1 Month", subtitle = "Displaying Frequency in Transaction Data") +
  theme(axis.text.x = element_text(angle = 35, hjust = 1)) +
  xlab("") + ylab("Absolute Frequency")

# For some reason it doesn't order well.. so aggregate --------
freq_prod <- aggregate(list_prod$frequency, by = list(list_prod$category), FUN = sum)
colnames(freq_prod) <- c("category", "freq")

ggplot(freq_prod, aes(reorder(category, -freq), freq)) + geom_col(fill = "turquoise3") +
  ggtitle("Product Types Sold in 1 Month", subtitle = "Displaying Frequency in Transaction Data") +
  xlab("") + ylab("Absolute Frequency") +
  coord_flip()


#Viz Products top3---------------------------------------------------
top3Cat <- read_csv("Ubiqum/PredictingCustomerPreference/Electronidex/top3Cat.csv")


# when three categories combined in one plot (desktop, laptop, monitor) --------------------
ggplot(top3Cat) +
  geom_bar(aes(type, volume, fill = category), stat="identity")+
  facet_wrap(~ category, drop = TRUE)+
  theme(axis.text.x = element_text(angle= 70, hjust = 1))


# when ploting individual categories (desktop, laptop, monitor) ----------------------------
ggplot(top3Cat[which(top3Cat$category == "Desktop"), ]) +
  geom_bar(aes(type, volume, fill = category), stat="identity")+
  theme(axis.text.x = element_text(angle= 70, hjust = 1))+ 
  labs(title="Desktop Products", ylab("Volume"), xlab("Product Type"))


# plotting in order. first, order the daa set ----------------------------------------------
top3Cat$type<- factor(top3Cat$type, levels=top3Cat$type[order(-top3Cat$volume)])

ggplot(top3Cat[which(top3Cat$category == "Desktop"), ]) +
  geom_bar(aes(type, volume, fill = category), stat="identity")+
  theme(axis.text.x = element_text(angle= 25, hjust = 1))+ 
  labs(title="Desktop Products", ylab("Volume"), xlab("Product Type"))


# plotting Laptops -------------------------------------------------------------------------
ggplot(top3Cat[which(top3Cat$category == "Laptops"), ]) +
  geom_bar(aes(x = type, y=volume, fill = category),fill="orange", stat="identity")+
  theme(axis.text.x = element_text(angle= 25, hjust = 1))+ 
  labs(title="Laptop Products", ylab("Volume"), xlab("Product Type"))


# plotting Monitor -------------------------------------------------------------------------
ggplot(top3Cat[which(top3Cat$category=="Monitors"),])+
  geom_bar(aes(type, volume, fill=category), fill="purple", stat = "identity")+
  theme(axis.text.x = element_text(angle = 25,hjust = 1))+
  labs(title="Monitor Products", ylab("Volume"), xlab("Product Type"))