library(dplyr)
library(dslabs)
data("murders")

 # Nomor 1
murders <- mutate(murders, rate = total/population * 10^6 )
murders

 # Nomor 2
x <-murders$rate
murders <- mutate(murders,ranking = rank(x))
murders

 # Nomor 3
select(murders,state,abb)

 # Nomor 4
mur