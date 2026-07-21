#reading in the data
library(ggplot2)
library(dplyr)
library(stringr)
library(ggmosaic)



lending=read.csv("lending.csv", header=TRUE)
only18=lending[grepl("18",lending$issue_d),] #keep only records from 2018
df=only18  %>% select(dti_n, Default)         #keep only dti_n and default columns
rownames(df)=NULL

head(df)

#removing erroroneous values
df$dti_n[df$dti_n>998]=NA
df=df%>%filter(!is.na(dti_n))

#categorizing default
df$Default=factor(df$Default, levels=c(0,1),
                  labels=c("No Default","Default"))

#adding classifcation levels for testing group means
df <- df %>% 
  mutate(
    dti_level=case_when(
      dti_n<36 ~ "Low",
      dti_n>=36 & dti_n<45 ~ "Medium",
      dti_n>45 ~ "High"
    )
  )

##side by side boxplot
ggplot(df, aes(x=Default,y=dti_n))+
  geom_boxplot(fill="lightblue")+
  labs(
    x="Loan Default Status",
    y="Debt-to-Income Ratio",
    title="DTI Distribution by Default Status"
  )

ggplot(df, aes(x=Default,y=logdti))+
  geom_boxplot(fill="lightblue")+
  labs(
    x="Loan Default Status",
    y="logDTI",
    title="logDTI Distribution by Default Status"
  )

library(dplyr)

summary_table <- df %>%
  group_by(Default) %>%   # or whatever your group variable is called
  summarise(
    N = n(),
    Mean = mean(dti_n, na.rm = TRUE),
    Median = median(dti_n, na.rm = TRUE),
    SD = sd(dti_n, na.rm = TRUE)
  )
summary_table

logsummary_table <- df %>%
  group_by(Default) %>%   # or whatever your group variable is called
  summarise(
    N = n(),
    Mean = mean(logdti, na.rm = TRUE),
    Median = median(logdti, na.rm = TRUE),
    SD = sd(logdti, na.rm = TRUE)
  )
logsummary_table

# QUESTION 1 T-TEST MEAN DTI FOR DEFAULT VS NON DEFAULT

#equal variance
with(df,tapply(dti_n,Default,sd)) #looks fine

dti.lm<-lm(dti_n~Default,data=df)
plot(predict(dti.lm),resid(dti.lm))
abline(h=0,lty=3)

  #alternate graph
p1 <- ggplot(data = df,
             mapping = aes(
               x = predict(dti.lm),
               y = resid(dti.lm)))
p1 + geom_point() + geom_hline(yintercept = 0, linetype = "dashed", color = "red")


#normality
qqnorm(resid(dti.lm))
qqline(resid(dti.lm)) #see very large deviations from qqplot
  #ggplot version
p2<-ggplot(df, aes(sample=resid(dti.lm)))+stat_qq()
p2+stat_qq_line(linetype="dashed",color="blue")+
  labs(
    x="Theoretical Quantiles",
    y="Sample Quantiles",
    title="Normal Q-Q Plot of Residuals for DTI"
  )

#applying log transform
df$logdti=log1p(df$dti_n)

loglm<-lm(logdti~Default,data=df)
lp1<-ggplot(data=df,
           mapping=aes(
             x=predict(loglm),
             y=resid(loglm)))
lp1+geom_point()+geom_hline(yintercept=0,linetype="dashed",color="purple")+ #var look much better
  labs(
    x="Fitted Value",
    y="Residual",
    title="Residuals vs Fitted Values"
  )
  
  
lp2<-ggplot(df,aes(sample=resid(loglm)))+stat_qq()
lp2+stat_qq_line(linetype="dashed",color="black")+
  labs(
    x="Theoretical Quantiles",
    y="Sample Quantiles",
    title="Normal Q-Q Plot of Residuals for logDTI"
  )

#t test
df.test<-t.test(logdti~Default,data=df)
names(df.test)

df.test

ggplot(data=df,aes(x=dti_n,y=logdti))+geom_point()
ggplot(data=df,aes(x=logdti,y=Default))+geom_point()

#back transform
df.test$estimate
diff(df.test$estimate)
exp(diff(df.test$estimate))

df.test$conf.int
exp(df.test$conf.int)

# QUESTION 2 CHI-SQUARE IS THERE A DIFFERENCE BETWEEN DTI GROUP MEANS (LOW,MED,HIGH)
#frequency table
df.ft<-table(df$dti_level,df$Default)
df.ft
#mosaic plot
ggplot(data=df)+
  geom_mosaic(aes(x=product(Default),
                  fill=dti_level),na.rm=TRUE)+
  theme_bw()+
  theme(plot.title=element_text(hjust=0.5,
                                size=rel(1.2)),
        axis.title.y=element_text(size=rel(1.1)),
        axis.title.x=element_text(size=rel(1.1)),
        strip.text.y=element_text(size=rel(1.1)))+
  labs(x="Loan Default Status",
       y="Debt-to-Income Level",
       fill="Debt-to-Income Level",
       title="Mosaic Plot of DTI to Default Status")

#chi-sq test
chi.test<-chisq.test(df.ft,correct=F)
chi.test

chi.test$expected
(chi.test$residuals)^2

# QUESTION 3 LOGISTIC REGRESSION

#linearity assumption



default.m1<-glm(Default~logdti,family=binomial,data=df)
default.m1

summary(default.m1)
exp(coef(default.m1))

confint(default.m1)
exp(confint(default.m1))

anova(default.m1,test='Chisq')

#plotting pred probabilities
newX<-1:6
dtipred<-predict(default.m1,newdata=data.frame(logdti=newX),type='response')
par(mar=c(5,4,4,2)+.1)
plot(newX,dtipred,type='l',lwd=2,col=4,
     xlab="Debt-to-Income (log)",ylab="logit P[default]")

newX<-1:6
dtipred<-predict(default.m1,newdata=data.frame(logdti=newX),type='response')
par(mar=c(5,4,4,2)+.1)
plot(newX,dtipred,type='l',lwd=2,col=4,
     xlab="Debt-to-Income (log)",ylab="logit P[default]")

#junk
ggplot(
  df,
  aes(y = logdti, x = Default) ) +
  geom_boxplot( binaxis='y', stackdir='center')



ggplot(df, aes(x=dti_n))+geom_histogram()
ggplot(df, aes(x=logdti))+geom_histogram()

ggplot(df,aes(x=logdti,y=Default)) +geom_jitter(width = 0, height = 0.15, alpha = 0.3)

ggplot(df, aes(x = logdti, y = Default)) +
  geom_jitter(width = 0, height = 0.1, alpha = 0.3) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"))
