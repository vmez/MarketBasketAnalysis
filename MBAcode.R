electronidex<-read.transactions("C:/Users/Violeta/Desktop/Ubiqum/PredictingCustomerPreference/Electronidex/ElectronidexTransactions2017.csv", 
                                format="basket", sep = ",",
                                cols = NULL, rm.duplicates = TRUE, encoding = "unknown")
itemLabels(electronidex)


inspect(electronidex[1:10])
summary(electronidex)
M1<- apriori (electronidex, parameter=list(support=0.007, confidence=0.60, minlen=2))
summary(M1)
inspect(sort(M1, by="lift")[1:8])

M<-matrix(itemFrequency(electronidex, type="abs"))


itemFrequencyPlot(electronidex, support=0.075)
itemFrequencyPlot(electronidex, TopN=10)
itemFrequency(electronidex[, 1:10])
rules <- apriori(electronidex, parameter = list(support = 0.002, confidence = 0.25, minlen = 2))
inspect(sort(rules, by = 'count')[1:10])

#how manay times is an item purchased alone
size(electronidex)
table1<-electronidex[which(size(electronidex)==1),]
size(table1)
tbl<-crossTable(table1)
tbl["iMac", "iMac"]


#######see redundant
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- TRUE 
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

rules.pruned <- rules[!redundant]
inspect(rules.pruned)

iMac.rules <- subset(electronidex, items %in% 'iMac')
inspect(ham.rules)
#############################################################################################################
inspect(rules[1:length(rules)])
plot(rules)
#plot(rules, method = NULL, measure = "support", shading = "lift",
#    interactive = FALSE, data = NULL, control = NULL, engine = "interactive")
head(quality(rules))
sel <- plot(rules, measure=c("support", "lift"), shading="confidence", interactive=TRUE)





#############################################################################################################
itemFrequencyPlot(electronidex,topN=10,type="absolute")

########apriori recommendation
rules<-apriori(electronidex, parameter = list(supp = 0.05, conf = 0.01))
inspect(rules[1:length(rules)])
#inspect(rules[1:4])
summary(rules)



########plotting the volume with categories combined
agsuM1$Group.1 <- factor(agsuM1$Group.1, levels = agsuM1$Group.1[order(-agsuM1$`Electronidex_itemsList$volume`)]) 
agsuM1$Group.1
p<-ggplot(data=agsuM1, aes(x=as.factor(agsuM1$Group.1), y=agsuM1$`Electronidex_itemsList$volume`))+
  geom_bar(stat="identity")+labs(title="Frequency Product Category")
p + theme(axis.text.x = element_text(angle= 70, hjust = 1)) + ylab("Volume") +xlab("Product Category") +
  theme(plot.title = element_text(hjust = 0.5))
summary(agsuM1)



#Product dependency on rhs as Apple Earpods
rulesEarPods<-apriori(electronidex, parameter=list(supp=0.001,conf = 0.6, minlen=2), 
                      appearance = list(rhs="Apple Earpods"))


plot(rulesEarPods, method="graph")

#Product dependency on lhs as Apple Earpods
rulesEarPods<-apriori(electronidex, parameter=list(supp=0.001,conf = 0.1, minlen=2), 
                      appearance = list(lhs="Apple Earpods"))
rulesEarPods<-sort(rulesEarPods, decreasing=TRUE,by="lift")
inspect(rulesEarPods)

#3-button-mouse dependency
rulesMouse<- apriori(electronidex, parameter = list(supp=0.002, conf=0.3, minlen=2),
                     appearance = list(rhs="3-Button Mouse"))
rulesMouse<-sort(rulesMouse, decreasing = TRUE, by="lift")
inspect(rulesMouse)

##looking at high product relationship
rules<-apriori(electronidex, parameter = list(supp=0.009, conf=0.08, minlen=2))
subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- TRUE 
redundant <- colSums(subset.matrix, na.rm=T) >= 1
which(redundant)

rules.pruned <- rules[!redundant]
inspect(rules.pruned)
rules<-sort(rules, decreasing = TRUE, by="lift")
inspect(rules[1:10])
