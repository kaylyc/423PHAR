setwd("//Users/kaylychoy/Desktop/423_PHAR/RWorkshops/R0_Gusto_Sep12")

library(tidyverse)

df <- read_csv("./data/gusto.csv")

ggplot(data=df,aes(x=age,col=sex,fill=sex))+geom_histogram()

df %>% group_by(sex) %>% summarise(mean(age),sd(age))

t.test(age~sex,data=df)

hist(df$age)


