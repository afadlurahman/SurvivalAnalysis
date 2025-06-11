library(ISLR2)
library(survival)
library(rpart)
library(rpart.plot)
library(partykit)
library(pec)


data("BrainCancer")
attach(BrainCancer)
View(BrainCancer)
summary(BrainCancer)
table(status)



z <- Surv(time = BrainCancer$time, event = BrainCancer$status)


fitz <- rpart(z ~ sex + diagnosis + loc + ki + gtv + stereo, data = BrainCancer, method = "exp", control = rpart.control(cp=0))
partyfitz <- as.party(fitz)
plot(partyfitz, type = "extended", terminal_panel = node_surv, 
     main = "Survival Tree ")
plotcp(fitz)
printcp(fitz)
summary(fitz)
fitz$frame
?plotcp



km <- survfit(z ~ fitz$where, data = BrainCancer)
plot(km)

fittree <- ctree(z ~ sex + diagnosis + ki * loc + gtv + stereo, scores = 'false' , data = BrainCancer)
plot(fittree)

tree.ctree <- ctree(BrainCancer$status ~ ., data = BrainCancer)
plot(tree.ctree)

plot(ctree(z, data = BrainCancer))
plot(ctree(Class ~ ., data = BrainCancer))

?ctree

plotcp(fittree)



