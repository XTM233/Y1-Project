library("rpart") #for fitting decision trees
library("rpart.plot") #for plotting decision trees

#load data

churn.whole <- read.csv("churn-whole.csv")

#build the initial tree
tree <- rpart(Churn~., data=churn.whole, control=rpart.control(cp=.0001))

#view results
printcp(tree)

#identify best cp value to use
best <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
#produce a pruned tree based on the best cp value
pruned_tree <- prune(tree, cp=best)

#plot the pruned tree
prp(pruned_tree,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=T,cex = 1) #display 5 decimal places in output
