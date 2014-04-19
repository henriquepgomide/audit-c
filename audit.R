## Script for IRT
library(psych)
library(ltm)
library(RColorBrewer)
library(car)
library(gmodels)
library(pROC)
library(ROCR)
library(caret)


# Importing dataframe ----
auditdf  <- read.csv("~/Downloads/audit.csv", header=TRUE, na.strings = c("NA",""), strip.white = TRUE)
auditdf  <- auditdf[,1:16]

# Preparing data for irt and audit evaluation
names(auditdf) # Inspecting selection
sapply(auditdf, FUN=table) # Finding errors


# Correcting errors
auditdf$fr5doses[auditdf$fr5doses > 4]  <- NA # fixing errors
auditdf$feriment[auditdf$feriment == 1]  <- NA # fixing errors
auditdf$parbeber[auditdf$parbeber > 4 | auditdf$parbeber == 1 |  auditdf$parbeber == 3]  <- NA # fixing errors

auditdf$result  <- auditdf$freq +  auditdf$doses + auditdf$fr5doses + auditdf$nconspar + auditdf$nconsfaz + auditdf$bebmanha + auditdf$culpado + auditdf$nlembrou + auditdf$feriment + auditdf$parbeber # computing audit values

table(auditdf$result - auditdf$total, exclude=NULL) #checking differences between spss e r computation. R's calculation is precise due to correction of typos.

length(auditdf$result) # 23 NA's found from 3062 cases.


#cutoffs for audit
#6
auditdf$a6[auditdf$result <= 6]  <- "Negative"
auditdf$a6[auditdf$result > 6]  <- "Positive"

#7
auditdf$a7[auditdf$result <= 7]  <- "Negative"
auditdf$a7[auditdf$result > 7]  <- "Positive"

#8
auditdf$a8[auditdf$result <= 8]  <- "Negative"
auditdf$a8[auditdf$result > 8]  <- "Positive"

#9
auditdf$a9[auditdf$result <= 9]  <- "Negative"
auditdf$a9[auditdf$result > 9]  <- "Positive"

#predictor
auditdf$pred  <- auditdf$freq +  auditdf$doses + auditdf$fr5doses

# cut offs for audit-c
#3
auditdf$c3[auditdf$pred <= 3]  <- "Negative"
auditdf$c3[auditdf$pred > 3]  <- "Positive"
#4
auditdf$c4[auditdf$pred <= 4]  <- "Negative"
auditdf$c4[auditdf$pred > 4]  <- "Positive"
#5
auditdf$c5[auditdf$pred <= 5]  <- "Negative"
auditdf$c5[auditdf$pred > 5]  <- "Positive"
#6
auditdf$c6[auditdf$pred <= 6]  <- "Negative"
auditdf$c6[auditdf$pred > 6]  <- "Positive"
#7
auditdf$c7[auditdf$pred <= 7]  <- "Negative"
auditdf$c7[auditdf$pred > 7]  <- "Positive"


# Sensibility and specifity analysis ----

## Audit = 7 vs. audit-c3 = 3 
m73 <- confusionMatrix(auditdf$c3, auditdf$a7, positive = "Positive")
round(m73$byClass[1:2],2)

## Audit = 7 vs. audit-c3 = 4 
m74  <- confusionMatrix(auditdf$c4, auditdf$a7, positive = "Positive")
round(m74$byClass[1:2],2)

## Audit = 7 vs. audit-c3 = 5 
m75  <- confusionMatrix(auditdf$c6, auditdf$a7, positive = "Positive")
round(m75$byClass[1:2],2)

## Audit = 8 vs. audit-c3 = 3 
m83 <- confusionMatrix(auditdf$c3, auditdf$a8, positive = "Positive")
round(m83$byClass[1:2],2)

## Audit = 8 vs. audit-c3 = 4 
m84  <- confusionMatrix(auditdf$c4, auditdf$a8, positive = "Positive")
round(m84$byClass[1:2],2)

## Audit = 8 vs. audit-c3 = 5 
m85  <- confusionMatrix(auditdf$c6, auditdf$a8, positive = "Positive")
round(m85$byClass[1:2],2)


# ROC ----
roc(response = auditdf$a7, predictor = auditdf$pred, partial.auc = c(10, 0), partial.auc.correct = TRUE, percent = TRUE, plot = TRUE)

# Example graphs for multiple ROC's
rocobj1 <- plot.roc(auditdf$a7, auditdf$pred , main="Statistical comparison", percent=TRUE, col="#1c61b6")
rocobj2 <- lines.roc(auditdf$a8, auditdf$pred, percent=TRUE, col="#008600")
testobj <- roc.test(rocobj1, rocobj2)
text(50, 50, labels=paste("p-value =", format.pval(testobj$p.value)), adj=c(0, .5))
legend("bottomright", legend=c("AUDIT 7", "AUDIT 8"), col=c("#1c61b6", "#008600"), lwd=2)

# Item Response Theory - Graded Model ----

# correlation matrix
rcor.test(auditdf[6:15], method = "kendall")

# IRT
auditdf[6:15]  <- lapply(auditdf[6:15], as.factor) # transformating audit vars as factor for grm 

#irt
str(auditdf) # checking transformation - it seems ok.

# Comparing models - one parameter (theta) vs. 2 parameter (theta and discrimination)
# Creating models
audit1par  <- grm(auditdf[6:15], constrained = T)
audit2par  <- grm(auditdf[6:15])

# Comparing, using BIC and AIC as criteria. (Less is better)
anova(audit1par, audit2par) # 1 parameter model seems to fit data better

# Summary of the model
summary(audit1par)
coef(audit1par)
plot(audit1par)

# information
information(audit1par, c(-4,4)) # all items - Total info = 55.92
information(audit1par, c(-4,4), c(1,2,3)) # shortaudit - Total info = 20.99, which means (20.94/55.87) .374 of the total. This result is quite similar to Strauss and Rindstoff ~.40.


#  Graphs for publication - of ICC and OCCU
par(mfrow = c(1,1))
for (i in 1:10) {  
  plot(audit1par, type = "ICC", items = i, col = brewer.pal(4,"Dark2"), main = "ICC", ylab = "Probabilidade", xlab = "Theta")
  plot(audit1par, type = "OCCu", items = i, col = brewer.pal(4,"Dark2"), main = "OCCu", xlab = "Theta")
}

# Descriptives ----
# classifying participants using as reference cutoff points from AUDIT-babor
auditdf$clas[auditdf$result <= 7]  <-  "Education"
auditdf$clas[auditdf$result > 7 &  auditdf$result <= 15]  <-  "Brief Advice"
auditdf$clas[auditdf$result > 15 &  auditdf$result <= 19]  <-  "Brief Advice and brief counseling"
auditdf$clas[auditdf$result > 20]  <-  "Referral for Clinical"

tableAudit  <- table(auditdf$clas)
barplot(tableAudit, names.arg=c("BA", "BA and BI", "Education", "Referral"))

# Sex
auditdf$sexo  <- as.factor(auditdf$sexo)

levels(auditdf$sexo)[2]  <- "Male"
levels(auditdf$sexo)[3]  <- "Female"
levels(auditdf$sexo)[1]  <- NA

table(auditdf$sexo)

# age
auditdf$idade[auditdf$idade < 18 ] <- NA
summary(auditdf$idade)
boxplot(auditdf$idade)
table(auditdf$idade)
