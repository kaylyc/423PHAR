setwd("//Users/kaylychoy/Desktop/423PHAR/R Workshop/RGusto_Sep12")

library(tidyverse)

df <- read_csv("gusto.csv")

ggplot(data=df,aes(x=age,col=sex,fill=sex))+geom_histogram()

df %>% group_by(sex) %>% summarise(mean(age),sd(age))

t.test(age~sex,data=df)

hist(df$age)
