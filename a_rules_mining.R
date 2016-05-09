library(arules)

setwd("C:/Users/Vikaasa/IdeaProjects/Spark-Workshop")

input_trans = read.transactions("top_rated_businesses_clustered.csv", format = "basket", sep = ",", rm.duplicates=TRUE, cols = 1)
str(input_trans)
rules=apriori(input_trans, parameter = list(supp=0.015, conf=0.75))
inspect(rules)
rules <- sort(rules, by="support", decreasing=TRUE)

df<-inspect(rules)
write.csv(df, file = "arules_op.csv")

as(head(sort(rules, by = c("support")), n=100), "data.frame")

## printing out a subset of rules which indicate the relation between genre and cast, with lhs as "cast1" and rhs as "genre":
rules2<- subset(rules, subset = (lhs %pin% "Pizza"))
inspect(rules2)
rules2<-sort(rules2, by="support", decreasing=TRUE)
inspect(rules2)
