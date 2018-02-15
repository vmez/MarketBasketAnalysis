#Start with Clean Environment
rm(list = ls())

#Libraries---------------------------------------
library(readr)
library(arules)
library(Matrix)
library(lattice)
library(ggplot2)
library(caret)
library(grid)
library(arulesViz)

#Load data---------------------------------------

electronidex <- read.transactions("x",
  format = "basket", sep = ",", cols = NULL, rm.duplicates = TRUE, encoding = "unknown")

Electrodinex_itemList

#What's here------------------------------------
inspect(electronidex[1:10])
length(electronidex)
LIST(electronidex)
length(unique(itemLabels(electronidex)))

summary(electronidex)


#Let's Plot for clarity-------------------------
itemFrequencyPlot(electronidex, topN = 20, horiz = TRUE, type = "absolute")


#PreProcessing----------------------------------
rules <- apriori(electronidex, parameter = list(support = 0.002, confidence = 0.25, minlen = 2))
inspect(sort(rules, by = 'count')[1:10])

#how manay times is an item purchased alone----------------------------
table1<-electronidex[which(size(electronidex)==1),]
size(table1)
tbl<-crossTable(table1)
tbl["iMac", "iMac"]


#redundant purchases------------------------------------------------
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- TRUE 
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

rules.pruned <- rules[!redundant]
inspect(rules.pruned)

iMac.rules <- subset(electronidex, items %in% 'iMac')
inspect(iMac.rules)
image(iMac.rules)
inspect(rules[1:length(rules)])
plot(rules)
#plot(rules, method = NULL, measure = "support", shading = "lift",
#    interactive = FALSE, data = NULL, control = NULL, engine = "interactive")
head(quality(rules))
sel <- plot(rules, measure=c("support", "lift"), shading="confidence", interactive=TRUE)

#############################################################################################################
########apriori recommendation
rules<-apriori(electronidex, parameter = list(supp = 0.05, conf = 0.01))
inspect(rules[1:length(rules)])
inspect(rules[1:4])
summary(rules)


#Product dependency on rhs as Apple Earpods-------------------------------------------------
rulesEarPods<-apriori(electronidex, parameter=list(supp=0.001,conf = 0.6, minlen=2), 
                      appearance = list(rhs="Apple Earpods"))


plot(rulesEarPods, method="graph")

#Product dependency on lhs as Apple Earpods--------------------------------------------------
rulesEarPods<-apriori(electronidex, parameter=list(supp=0.001,conf = 0.1, minlen=2), 
                      appearance = list(lhs="Apple Earpods"))
rulesEarPods<-sort(rulesEarPods, decreasing=TRUE,by="lift")
inspect(rulesEarPods)

#3-button-mouse dependency--------------------------------------------------------------------
rulesMouse<- apriori(electronidex, parameter = list(supp=0.002, conf=0.3, minlen=2),
                     appearance = list(rhs="3-Button Mouse"))
rulesMouse<-sort(rulesMouse, decreasing = TRUE, by="lift")
inspect(rulesMouse)

##looking at high product relationship--------------------------------------------------------
rules<-apriori(electronidex, parameter = list(supp=0.009, conf=0.08, minlen=2))
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- TRUE 
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

rules.pruned <- rules[!redundant]
inspect(rules.pruned)
rules<-sort(rules, decreasing = TRUE, by="lift")
inspect(rules[1:10])


#List of product upload---------------------------------------------------------------------------
Electronidex_itemsList <- read_csv("X")
str(Electronidex_itemsList)
Electronidex_itemsList <- as.data.frame(Electronidex_itemsList)
head(Electronidex_itemsList)


#Volume Database----------------------------------------------------------------------------------
agsum <- as.data.frame(Electronidex_itemsList$frequency)
str(agsum)
agsuM<-aggregate(agsum, by=list(Electronidex_itemsList$category), FUN= sum) 
View(agsuM)


#Plotting the volume with categories combined--------------------------------------------
agsuM$Group.1 <- factor(agsuM$Group.1, 
                        levels = agsuM$Group.1[order(-agsuM$`Electronidex_itemsList$volume`)]) 

p<-ggplot(agsuM1, aes(as.factor(agsuM$Group.1), agsuM$`Electronidex_itemsList$volume`))+
  geom_bar(stat="identity")+labs(title="Frequency Product Category")

p + theme(axis.text.x = element_text(angle= 70, hjust = 1)) + 
  ylab("Volume") +xlab("Product Category") +
  theme(plot.title = element_text(hjust = 0.5))
summary(agsuM)


#Viz Products top3---------------------------------------------------
top3Cat <- read_csv("X")


#when three categories combined in one plot (desktop, laptop, monitor)-----------------------
ggplot(top3Cat) +
  geom_bar(aes(type, volume, fill = category), stat="identity")+
  facet_wrap(~ category, drop = TRUE)+
  theme(axis.text.x = element_text(angle= 70, hjust = 1))


#when ploting individual categories (desktop, laptop, monitor)---------------------------------
ggplot(top3Cat[which(top3Cat$category == "Desktop"), ]) +
  geom_bar(aes(type, volume, fill = category), stat="identity")+
  theme(axis.text.x = element_text(angle= 70, hjust = 1))+ 
  labs(title="Desktop Products", ylab("Volume"), xlab("Product Type"))


#plotting in order. first, order the daa set---------------------------------------------------
top3Cat$type<- factor(top3Cat$type, levels=top3Cat$type[order(-top3Cat$volume)])

ggplot(top3Cat[which(top3Cat$category == "Desktop"), ]) +
  geom_bar(aes(type, volume, fill = category), stat="identity")+
  theme(axis.text.x = element_text(angle= 25, hjust = 1))+ 
  labs(title="Desktop Products", ylab("Volume"), xlab("Product Type"))


#plotting Laptops------------------------------------------------------------------------------
ggplot(top3Cat[which(top3Cat$category == "Laptops"), ]) +
  geom_bar(aes(x = type, y=volume, fill = category),fill="orange", stat="identity")+
  theme(axis.text.x = element_text(angle= 25, hjust = 1))+ 
  labs(title="Laptop Products", ylab("Volume"), xlab("Product Type"))


#plotting Monitor------------------------------------------------------------------------------
ggplot(top3Cat[which(top3Cat$category=="Monitors"),])+
  geom_bar(aes(type, volume, fill=category), fill="purple", stat = "identity")+
  theme(axis.text.x = element_text(angle = 25,hjust = 1))+
  labs(title="Monitor Products", ylab("Volume"), xlab("Product Type"))

