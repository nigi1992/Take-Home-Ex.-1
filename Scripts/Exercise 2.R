###Exercise 2: Data Preparation and Exploration
#Use the data file study1_data.csv to begin the replication process. Identify and describe the
#experimental variables (i.e., treatment and immigration attitudes) and provide visualizations
#of their distribution.
#Then, select up to four covariates (e.g., gender, age, etc.) and plot their distribution too.
#If necessary, clean or transform variables. Document any changes.

# IV: treatment - being shown Muslim Names and Pictures (Yes/No)
# Operationalization: 

# Most important confounding variable: Immigrant Attitudes
# Operationalization: 

# DV: support for LGBT+ inclusive education (0-11 range, scale order randomized) -> transformation to
# 0-1 range for easier interpretation.

from csv table:
  IV: outcome_treat, treatment
  CV: imm_1, imm_2, imm1mean, immbelow, imm3, 
  DV: support

df <-df%>% 
  mutate(treat= as.factor(treatment),
         treatnum= as.numeric(treatment),
         immbelow= as.factor(immbelow),
         imm3= as.factor(imm3),
         
##Figure 3## 

model1<- glm (support ~ treat*imm_1, data=df, family="binomial")
summ(model1, robust=TRUE)
df$predictINT1<-predict(model1, df, type="response")

treat <- subset(df, treatnum==1)
control <- subset(df, treatnum==0)
proimm <- subset(df, immbelow==0)
noproimm <- subset(df, immbelow==1)

##FIGURE 4##

modelsub1<- lm (support ~ treat, data=proimm)
summ(modelsub1, robust=TRUE)
proimm$predictedb<-predict(modelsub1, proimm)

modelsub2<- lm (support ~ treat, data=noproimm)
summ(modelsub2, robust=TRUE)
noproimm$predictedb<-predict(modelsub2, noproimm)

treatsub1 <- subset(proimm, treatnum==1)
controlsub1 <- subset(proimm, treatnum==0)
treatsub2 <- subset(noproimm, treatnum==1)
controlsub2 <- subset(noproimm, treatnum==0)