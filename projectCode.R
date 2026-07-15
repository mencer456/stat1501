lending=read.csv("lending.csv", header=TRUE)

#filtering to only keep 2018 data
df=lending[grepl("18",lending$issue_d),]
head(df)
View(df)


#value counts of purpose
table(df$purpose)



library(ggplot2)
ggplot(
  cars,
  aes(y = Cmb.MPG, x = Fuel) ) +
  geom_boxplot( binaxis='y', stackdir='center')

library(dplyr)
library(stringr)

df <- cars %>%
  mutate(
    combined_mpg_clean = if_else(
      str_detect(Cmb.MPG, "/"),
      {
        parts <- str_split(Cmb.MPG, "/", simplify = TRUE)
        (as.numeric(parts[,1]) + as.numeric(parts[,2])) / 2
      },
      as.numeric(Cmb.MPG)
    )
  )
View(df)


library(ggplot2)
ggplot(
  df,
  aes(y = combined_mpg_clean, x = Fuel) ) +
  geom_boxplot( binaxis='y', stackdir='center')+
  labs(
    title="Combined MPG across Fuel Types",
    x="Fuel Type",
    y="Combined MPG"
  )+
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold")
  )
