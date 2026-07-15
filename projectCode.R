#reading in the data
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


# full=read.csv("lending_full.csv",header=TRUE)
# full_only18=full[grepl("18",full$issue_d),]
# View(full_only18)

df$logdti=log1p(df$dti_n)

library(ggplot2)
ggplot(
  df,
  aes(y = logdti, x = Default) ) +
  geom_boxplot( binaxis='y', stackdir='center')

library(dplyr)
library(stringr)

ggplot(df, aes(x=dti_n))+geom_histogram()
ggplot(df, aes(x=logdti))+geom_histogram()

