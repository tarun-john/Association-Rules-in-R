library(arules)
library(arulesViz)


#Import files
trans <- read.transactions("Transactions.csv", format = "basket", header = FALSE, 
                           sep = ",", rm.duplicates = TRUE)

#Evaluate data
inspect(trans[1:10])
length(trans)
size(trans)


sort(itemFrequency(trans, type = "absolute"), decreasing = TRUE)
itemFrequencyPlot(trans, type = "absolute", topN = 10, col = "blue", 
                  ylab = "Frequency", main = "Frequency of Top 10 Items")
plot(density(size(trans)), col = "blue", main = "Items per Transaction")

#Items bought alone
singleitems <- trans[which(size(trans)==1), ]
cross <- crossTable(singleitems)
cross[1:10,1:10]

barplot(head(sort(itemFrequency(singleitems, type = "absolute"), 
                  decreasing = TRUE), n = 15), las = 2, 
        col = "blue", ylim = c(0,400), cex.names = 0.7, 
        main = "Top 15 Items Bought Alone", ylab = "Frequency")

#Apriori rules
rule1 <- apriori(trans, parameter = list(supp = 0.003, conf = 0.7, minlen = 2))
inspect(rule1)
plot(rule1, method = "graph", control = list(type = "items"))
plot(rule1, jitter = 0)
plot(rule1[1:10], method = "paracoord", control = list(reorder = TRUE))
plot(rule1[1:10], method = "paracoord")

#Check for redundancy 
red <- is.redundant(rule1)
summary(red)
 

write(rule1, file = "rules.csv", sep = ",", quote = TRUE, row.names = FALSE)



