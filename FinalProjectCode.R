#Loading necessary libraries
library(ggplot2)
library(dplyr)
library(stringr)

#Reading in the data
lending=read.csv("lending.csv", header=TRUE)
only18=lending[grepl("18",lending$issue_d),]  #keep only records from 2018
df=only18  %>% select(dti_n, Default)         #keep only dti_n and default columns
rownames(df)=NULL
head(df)

#removing erroneous values
df$dti_n[df$dti_n>998]=NA
df=df%>%filter(!is.na(dti_n))

#categorizing default
df$Default=factor(df$Default, levels=c(0,1),
                  labels=c("No Default","Default"))

#adding classification levels for the Chi-square test
df=mutate(df,
  dti_level=case_when(
    dti_n<36 ~ "Low", 
    dti_n>=36 & dti_n<45 ~ "Medium",
    dti_n>45 ~ "High"
  )
)

#Assumptions

#side by side boxplot to assess equal variances
ggplot(df, aes(x=Default,y=dti_n))+
  geom_boxplot(fill="lightblue")+
  labs(
    x="Loan Default Status",
    y="Debt-to-Income Ratio",
    title="DTI Distribution by Default Status"
  )

#QQ plot to assess normality
dti.lm<-lm(dti_n~Default,data=df)
p2<-ggplot(df, aes(sample=resid(dti.lm)))+stat_qq()
p2+stat_qq_line(linetype="dashed",color="blue")+
  labs(
    x="Theoretical Quantiles",
    y="Sample Quantiles",
    title="Normal Q-Q Plot of Residuals for DTI"
  )

# QUESTION 1 Wilcoxon Sum Rank Test
wilcox.test(dti_n~Default,data=df,correct=F)

# QUESTION 2 CHI-SQUARE
#frequency table
df.ft<-table(df$dti_level,df$Default)
df.ft

#chi-sq test
chi.test<-chisq.test(df.ft,correct=F)
chi.test

# QUESTION 3 LOGISTIC REGRESSION
default.m1<-glm(Default~dti_n,family=binomial,data=df)
default.m1

summary(default.m1) #printing the results of the logistic regression
exp(coef(default.m1)) #exponentiating the coefficients to see the odds ratio

confint(default.m1) #getting a 95% confidence interval for the regression

anova(default.m1,test='Chisq') #testing if the slope = 0
