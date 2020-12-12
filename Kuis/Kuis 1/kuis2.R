
# Nomor 1
library(readr)
library (ggplot2)
Covid19_Asean <- read_csv("Covid19_Asean.csv")

# Nomor 2
View(Covid19_Asean)

# Nomor 3
Covid19_Asean %>%
arrange(desc(Confirmed))

# Nomor 4
Covid19_Asean = mutate(Covid19_Asean,RateDeaths = Deaths / Confirmed)

# Nomor 5
print("Tertinggi")
filter(Covid19_Asean,RateDeaths == max(Covid19_Asean$RateDeaths))
print("Terendah")
filter(Covid19_Asean,RateDeaths == min(Covid19_Asean$RateDeaths))

# Nomor 6
Covid19_Asean %>%
ggplot(aes(x = Recovered, y = Confirmed)) +
geom_line()
