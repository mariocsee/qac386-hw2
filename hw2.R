
a = readLines("Chronicle_text.txt", 
              encoding = "utf-8")
t = paste(a, collapse=" ")
t = gsub("[^-A-Za-z., :!?]", " ", t) # Pavel cleaned it up. Remove any character that's not these
t = gsub("[ ]{2,}", " ", t) #If space two times or more in a row, remove extra spaces

# possibly cut up t

places = c("Constantinople", "Egypt", "Alexandria", "Persia")

j = list()
s = TRUE
while (s) {
  n = length(j)+1
  j[[n]] = c("Pattern", n)
  if (n>20) {
    s = FALSE
  }
}


final_ind = list()
final_places = list()
patterns = list()


for (i in places) {
  p = " ([a-z]+ )+(?=" + places[i] +"[^a-z])"
  m = gregexpr(p, t, perl = T, useBytes = T)
  v = regmatches(t, m)[[1]]
  v
  v = unique(v)
  # do something with names etc idk yet
  #
  
  for (j in length(v)) {
    p_temp = paste("(?<=", v[j],")","([A-Z][a-z]+[ ]*)+", sep="")
    p_temp
    m = gregexpr(p_temp, t, perl = T)
    v2 = regmatches(t, m)[[1]]
    v2
    unique(v2)
  }
  
  while (s) {
    n = length(j)+1
    j[[n]] = c("Pattern", n)
    
    if (n>20) {
      s = FALSE
    }
  }
  
}


