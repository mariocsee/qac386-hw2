
a = readLines("Chronicle_text.txt", 
              encoding = "utf-8")
t = paste(a, collapse=" ")
t = gsub("[^-A-Za-z., :!?]", " ", t) # Pavel cleaned it up. Remove any character that's not these
t = gsub("[ ]{2,}", " ", t) #If space two times or more in a row, remove extra spaces

install.packages("tm")
library(tm)
install.packages("descr")
library(descr)
# possibly cut up t

places = c("Constantinople", "Egypt", "Alexandria", "Persia","Syria","Thessalonica",
           "Byzantium","Carthage", "Cilicia","Spain")


generate_patterns_places <- function(sample) {
  
  patterns = list()
  
  for (i in 1:(length(places))) {
    p = paste(" ([a-z]+ ){2}(?=", sample[i] ,"[^a-z])", sep="")  
    m = gregexpr(p, t, perl = T, useBytes = T)
    v = regmatches(t, m)[[1]]
    
    
    v = sort(table(v),decreasing=TRUE)[1:20]
    v = as.data.frame(v)
    v$v[1:20]
    
    patterns[[(length(patterns)+1):(length(patterns)+1)]] = v$v
  }
  
  final_places = list()
  for (i in 1:4) {
    for (j in 1:20) {
      p_temp = paste("(?<=", patterns[[i]][j] ,")","([A-Z][a-z]+[ ]*)+", sep="")
      m = gregexpr(p_temp, t, perl = T)
      v2 = regmatches(t, m)[[1]]
      v2
      final_places = append(final_places, v2)
    }
  }
  
  return(final_places)
}

final_places = generate_patterns_places(places)
final_places

new_sample = sample(unique(final_places), 10)
new_sample


round2 = generate_patterns_places(new_sample)
round2

unique(round2)
