
a = readLines("Chronicle_text.txt", 
              encoding = "utf-8")
t = paste(a, collapse=" ")
t = gsub("[^-A-Za-z., :!?]", " ", t) # Pavel cleaned it up. Remove any character that's not these
t = gsub("[ ]{2,}", " ", t) #If space two times or more in a row, remove extra spaces

# possibly cut up t

places = c("Constantinople", "Egypt", "Alexandria", "Persia")

patterns = list()
for (i in 1:(length(places))) {
  p = paste(" ([a-z]+ ){2}(?=", places[i] ,"[^a-z])", sep="")  
  m = gregexpr(p, t, perl = T, useBytes = T)
  v = regmatches(t, m)[[1]]
  
  # find top frequency
  # find unique top 20
  
  patterns[[(length(patterns)+1):(length(patterns)+1)]] = v[1:20]
}

patterns
# clean it up
# prefer patterns that are like 2/3 words 
# 20 patterns

patterns[[4]][20]

final_places = list()
for (i in 1:4) {
  for (j in 1:20) {
    print(i, j)
    p_temp = paste("(?<=", patterns[[i]][j] ,")","([A-Z][a-z]+[ ]*)+", sep="")
    print(p_temp)
    m = gregexpr(p_temp, t, perl = T)
    v2 = regmatches(t, m)[[1]]
    # unique
    # run it against the getty thing and work with the XML to get the subject_ID
    
    final_places = append(final_places, v2)
  }
}

final_places



# stopped work here
