## Script for IRT
library(psych)
library(ltm)
library(RColorBrewer)
library(car)
library(gmodels)
library(pROC)


# Importing dataframe ----
auditdf  <- read.csv("~/Downloads/audit.csv", header=TRUE, na.strings = c("NA",""), strip.white = TRUE)
auditdf  <- auditdf[,1:16]

# Preparing data for irt and audit evaluation

names(auditdf) # Inspecting selection
sapply(auditdf, FUN=table) # Finding errors
auditdf$fr5doses[auditdf$fr5doses > 4]  <- NA # fixing errors
auditdf$feriment[auditdf$feriment == 1]  <- NA # fixing errors
auditdf$parbeber[auditdf$parbeber > 4 | auditdf$parbeber == 1 |  auditdf$parbeber == 3]  <- NA # fixing errors

auditdf$result  <- auditdf$freq +  auditdf$doses + auditdf$fr5doses + auditdf$nconspar + auditdf$nconsfaz + auditdf$bebmanha + auditdf$culpado + auditdf$nlembrou + auditdf$feriment + auditdf$parbeber # computing audit values

table(auditdf$result - auditdf$total, exclude=NULL) #checking differences between spss e r computation. R's calculation is precise due to correction of typos.

length(auditdf$result) # 23 NA's found from 3062 cases.

# classifying participants using as reference cutoff points from AUDIT-babor
auditdf$clas[auditdf$result <= 7]  <-  "Education"
auditdf$clas[auditdf$result > 7 &  auditdf$result <= 15]  <-  "Brief Advice"
auditdf$clas[auditdf$result > 15 &  auditdf$result <= 19]  <-  "Brief Advice and brief counseling"
auditdf$clas[auditdf$result > 20]  <-  "Referral for Clinical"

tableAudit  <- table(auditdf$clas)
barplot(tableAudit, names.arg=c("BA", "BA and BI", "Education", "Referral"))

#cutoffs for audit
#6
auditdf$a6[auditdf$result <= 6]  <- "Negative"
auditdf$a6[auditdf$result > 6]  <- "Positive"

#gold standard audit
#7
auditdf$gs[auditdf$result <= 7]  <- "Negative"
auditdf$gs[auditdf$result > 7]  <- "Positive"

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
#8
auditdf$c8[auditdf$pred <= 8]  <- "Negative"
auditdf$c8[auditdf$pred > 8]  <- "Positive"


# cross tabulation of AUDIT = 6 vs. AUDIT-C
# Cutoff: 3
CrossTable(auditdf$a6, auditdf$c3,  dnn = c('Gold Standard', 'Audit-C - 3 '))
# Cutoff: 4
CrossTable(auditdf$a6, auditdf$c4,  dnn = c('Gold Standard', 'Audit-C - 4 '))
# Cutoff: 5
CrossTable(auditdf$a6, auditdf$c5,  dnn = c('Gold Standard', 'Audit-C - 5 '))
# Cutoff: 6
CrossTable(auditdf$a6, auditdf$c6,  dnn = c('Gold Standard', 'Audit-C - 6 '))


# cross tabulation of AUDIT = 7 vs. AUDIT-C
# Cutoff: 3
CrossTable(auditdf$gs, auditdf$c3,  dnn = c('Gold Standard', 'Audit-C - 3 '))
# Cutoff: 4
CrossTable(auditdf$gs, auditdf$c4,  dnn = c('Gold Standard', 'Audit-C - 4 '))
# Cutoff: 5
CrossTable(auditdf$gs, auditdf$c5,  dnn = c('Gold Standard', 'Audit-C - 5 '))
# Cutoff: 6
CrossTable(auditdf$gs, auditdf$c6,  dnn = c('Gold Standard', 'Audit-C - 6 '))
# Cutoff: 7
CrossTable(auditdf$gs, auditdf$c7,  dnn = c('Gold Standard', 'Audit-C - 7 '))


# cross tabulation of AUDIT = 8 vs. AUDIT-C
# Cutoff: 3
CrossTable(auditdf$a8, auditdf$c3, dnn = c('AUDIT 8', 'Audit-C - 3 '))
# Cutoff: 4
CrossTable(auditdf$a8, auditdf$c4,  dnn = c('AUDIT 8', 'Audit-C - 4 '))
# Cutoff: 5
CrossTable(auditdf$a8, auditdf$c5,  dnn = c('AUDIT 8', 'Audit-C - 5 '))
# Cutoff: 6
CrossTable(auditdf$a8, auditdf$c6,  dnn = c('AUDIT 8', 'Audit-C - 6 '))
# Cutoff: 7
CrossTable(auditdf$a8, auditdf$c7,  dnn = c('AUDIT 8', 'Audit-C - 7 '))
# Cutoff: 8
CrossTable(auditdf$a8, auditdf$c8,  dnn = c('AUDIT 8', 'Audit-C - 8 '))


#ROC ----
roc(response = auditdf$gs, predictor = auditdf$pred, partial.auc = c(10, 0), partial.auc.correct = TRUE, percent = TRUE, plot = TRUE)
plot(x = roc(response = auditdf$gs, predictor = auditdf$pred, percent = TRUE, ci = TRUE, of = "se", sp = seq(0, 100, 5)), ci.type="shape")

# Example graphs for multiple ROC's
rocobj1 <- plot.roc(auditdf$gs, auditdf$pred , main="Statistical comparison", percent=TRUE, col="#1c61b6")
rocobj2 <- lines.roc(aSAH$outcome, aSAH$ndka, percent=TRUE, col="#008600")
testobj <- roc.test(rocobj1, rocobj2)
text(50, 50, labels=paste("p-value =", format.pval(testobj$p.value)), adj=c(0, .5))
legend("bottomright", legend=c("S100B", "NDKA"), col=c("#1c61b6", "#008600"), lwd=2)

# Item Response Theory - Graded Model ----

# correlation matrix
rcor.test(auditdf[6:15], method = "kendall")

# IRT
auditdf[6:15]  <- lapply(auditdf[6:15], as.factor) # transformating audit vars as factor for grm irt
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


# Thetas Scores
thetas  <- factor.scores.grm(audit1par, auditcIRT)
plot(thetas$score.dat$z1, short_audit$score, type = "p", xlab = "Thetas", ylab = "Soma bruta da escala")
cor(thetas$score.dat$z1, short_audit$score)