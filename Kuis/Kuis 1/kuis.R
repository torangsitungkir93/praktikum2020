
# Nomor 1
  library(dslabs)
  library(tidyverse)
  data("us_contagious_diseases")

# Nomor 2
  names(us_contagious_diseases)

# Nomor 3
  class(us_contagious_diseases$disease)

# Nomor 4
  us_contagious_diseases %>%
  filter(year>1955 & year<1965,disease=="Polio") %>%
  arrange(desc(population)) %>%
  head(n=10)
  
# nomor 5
  klasifikasi = ""
  for(i in 1:nrow(us_contagious_diseases)){
    if(us_contagious_diseases$count[i] < 1000) {
      klasifikasi[i] <- "Biasa"
    }
    else if(us_contagious_diseases$count[i] > 2000) {
      klasifikasi[i] <- "Azab"
    }
    else{
      klasifikasi[i] <- "Cobaan"
    }
  } 
  head(klasifikasi,n=500)
  
# Nomor 6
  newData <- us_contagious_diseases %>%
  mutate(
    Category = klasifikasi,
    Rate = count/population*10^5
  )
  head(newData,n=500)

# Nomor 7
  kesimpulan <- newData %>%
  group_by(state) %>%
  summarize(Rata_Rata=mean(Rate))
  kesimpulan

# Nomor 8
  plot(kesimpulan)
  
  
  
  
  
  
  
  