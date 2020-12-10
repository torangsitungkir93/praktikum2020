library(dslabs)
data(murders)

## Nomor 1
pop <- murders$population
sort(pop)[[1]]

## Nomor 2
order(pop)[[1]]

## Nomor 3
which.min(pop) == order(pop)[[1]]

## Nomor 4
murders$state[[order(pop)[[1]]]]

## Nomor 5
ranks <- rank(murders$population)
my_df <- data.frame(name=murders$state,peringkat=ranks)


## Nomor 6
ind = order(my_df$peringkat)
my_dff = data.frame(name=(sort(murders$state))[ind],Populasi = sort(murders$population),Peringkat = sort(ranks))

## Nomor 7
population_in_millions <- log10(murders$population)
total_gun_murders <- log10(murders$total)
plot(population_in_millions,total_gun_murders)

## Nomor 8 Histogram
population <- murders$population/100000
hist(population)

## Nomor 9 Boxplot
population <- murders$population/100000
boxplot(population~region,data=murders)


