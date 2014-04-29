# IRT AUDIT SCRIPT

## libraries ----
libs <- c('psych', 'ltm', 'RColorBrewer', 'car',
          'lattice', 'mirt')
lapply(libs, require, character.only = T)

# Import data ----
auditdf <- read.csv("audit.csv", header=TRUE, na.strings = c("NA",""), strip.white = TRUE)
auditdf <- auditdf[,1:15]

## Check data.frames
# names(auditdf) # Inspecting selection
# sapply(auditdf, FUN=table) # Finding errors

## Correct errors
auditdf <- subset(auditdf, auditdf$idade > 18) # Exclude less than 18 years
auditdf$fr5doses[auditdf$fr5doses > 4] <- NA # fixing errors
auditdf$feriment[auditdf$feriment == 1] <- NA # fixing errors
auditdf$parbeber[auditdf$parbeber > 4 | auditdf$parbeber == 1 | auditdf$parbeber == 3] <- NA # fixing errors
auditdf$sexo[auditdf$sexo == 0] <- NA # Inserting NA
auditdf$sexo <- factor(auditdf$sexo, labels=c("Male","Female"))
auditdf$servico <- factor(auditdf$servico, labels=c("UBS","Polícia", "Bombeiros", "Estudantes"))

## Compute audit score
auditdf$result <- auditdf$freq + auditdf$doses + auditdf$fr5doses + auditdf$nconspar + auditdf$nconsfaz + auditdf$bebmanha + auditdf$culpado + auditdf$nlembrou + auditdf$feriment + auditdf$parbeber # computing audit values

# classifying participants using as reference cutoff points from AUDIT-babor
auditdf$clas[auditdf$result <= 7] <- "Education"
auditdf$clas[auditdf$result > 7 & auditdf$result <= 15] <- "Brief Advice"
auditdf$clas[auditdf$result > 15 & auditdf$result <= 19] <- "Brief Advice and brief counseling"
auditdf$clas[auditdf$result > 20] <- "Referral for Clinical"

# Descriptives ----

# sex
table(auditdf$sex)
by(auditdf$sexo, auditdf$servico, summary) # by sex

# age
summary(auditdf$idade)
boxplot(idade ~ sexo , data = auditdf) # by sexo
boxplot(idade ~ clas , data = auditdf) # by clas
boxplot(idade ~ servico , data = auditdf) # by servico
bwplot(~idade|sexo*servico, data = auditdf)

# servico
table(auditdf$servico)

# Audit classification
table(auditdf$clas)
bwplot(~result|sexo*servico, data = auditdf)

# Item Response Theory - Graded Model ----

## Descriptives
describe(auditdf[6:15], )

### correlation matrix
round(cor(auditdf[6:15], method="kendal", use="complete.obs"),2) # kendall correlation coef among audit items

### Cronbach's alpha
alpha(auditdf[6:15]) # Cronbach's alpha
by(auditdf[6:15], auditdf$servico, alpha) # Cronbach's by service

VSS(auditdf[6:15], rotate="varimax")


# IRT
auditdf[6:15] <- lapply(auditdf[6:15], as.factor) # transformating audit vars as factor for grm

#irt
str(auditdf) # checking transformation - it seems ok.

# Comparing models - one parameter (theta) vs. 2 parameter (theta and discrimination)
# Creating models
audit1par <- grm(auditdf[6:15], constrained = T)
audit2par <- grm(auditdf[6:15])

# Comparing, using BIC and AIC as criteria. (Less is better)
anova(audit1par, audit2par) # 1 parameter model seems to fit data better

# Summary of the model
summary(audit1par)
coef(audit1par)
plot(audit1par)

# information
information(audit1par, c(-4,4)) # all items - Total info = 55.92
information(audit1par, c(-4,4), c(1,2,3)) # shortaudit - Total info = 20.99, which means (20.94/55.87) .374 of the total. This result is quite similar to Strauss and Rindstoff ~.40.


# Graphs for publication - of ICC and OCCU
par(mfrow = c(1,1))
for (i in 1:10) {
  plot(audit1par, type = "ICC", items = i, col = brewer.pal(4,"Dark2"), main = "ICC", ylab = "Probabilidade", xlab = "Theta")
  plot(audit1par, type = "OCCu", items = i, col = brewer.pal(4,"Dark2"), main = "OCCu", xlab = "Theta")
}