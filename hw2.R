
# Import text 
a = readLines("Chronicle_text.txt", 
              encoding = "utf-8")
t = paste(a, collapse=" ") # Collapse into one block
t = gsub("[^-A-Za-z., :!?]", " ", t) # Remove any character that's not these
t = gsub("[ ]{2,}", " ", t) #If space two times or more in a row, remove extra spaces

# install.packages("tm")
# install.packages("descr")
library(tm)
library(descr)

# 10 places chosen to do initial run
initial_places = c("Constantinople", "Egypt", "Alexandria", "Persia","Syria","Thessalonica",
                   "Byzantium","Carthage", "Cilicia","Spain")

generate_patterns <- function(sample) {
  # Empty patterns
  patterns = list()
  
  # Use each location to generate the list of 20 most common patterns
  for (i in 1:(length(sample))) {
    # Looking for short patterns with 2 words
    p = paste(" ([a-z]+ ){2}(?=", sample[i] ,"[^a-z])", sep="")  
    m = gregexpr(p, t, perl = T, useBytes = T)
    v = regmatches(t, m)[[1]] # Get matches
    v = gsub(" $", "", v, perl =  TRUE) # Remove whitespace in end
    v = gsub("^ ", "", v, perl =  TRUE) # Remove leading whitespace
    
    # Checking length so extra NAs aren't created
    last = 0
    if (length(unique(v)) > 20) {
      last = 20
    } else {
      last = length(unique(v))
    } 
    
    # Rank and get patterns by frequency
    if (length(v) > 1) {
      v = sort(table(v),decreasing=TRUE)[1:last] 
    }
    # Convert to data frame to separate patterns and their counts
    v = as.data.frame(v)
    
    # Save
    patterns[[(length(patterns)+1):(length(patterns)+1)]] = v$v
  }
  # Unlist and unique so its in one list and no repetitions
  patterns = unlist(patterns, recursive=FALSE, use.names = FALSE)
  patterns = unique(patterns)
  return(patterns)
}

generate_entities <- function(patterns) {
  # Empty list
  final_entities = list()
  
  #Iterate through each pattern and save all places found by that pattern
  for (i in 1:length(patterns)) {
    p_temp = paste("(?<= ", patterns[i] ," )","([A-Z][a-z]+[ ]*)+", sep="")
    m = gregexpr(p_temp, t, perl = T)
    v2 = regmatches(t, m)[[1]]
    final_entities = append(final_entities, v2)
  }
  # Unlist, clean whitespace, and unique to remove duplicates
  final_entities = unlist(final_entities, recursive = FALSE, use.names = FALSE)
  final_entities = gsub(" $", "", final_entities, perl =  TRUE)
  final_entities = unique(final_entities)
  return(final_entities)
}

#Testing function that runs patterns and entities in declared times
test_run <- function(num) {
  patterns = generate_patterns(initial_places)
  i = 0
  while (i < num) {
    entities = generate_entities(patterns)
    new_sample = sample(entities,10)
    patterns = generate_patterns(new_sample)
    while (all(c(1:length(patterns)) == patterns)) { # At times generate_patterns unlist returns a list of ints, this is to prevent that
      new_sample = sample(entities,10)
      patterns = generate_patterns(new_sample)
    }
    i = i + 1
  }
  return(entities)
}

# install.packages("xml2")
library(xml2)

# Iterate through entites and search for subject IDs
get_subjectID <- function(entities) {
  base_url = "http://vocabsservices.getty.edu/TGNService.asmx/TGNGetTermMatch?placetypeid=&nationid=&name="
  entity_subject_id = character(length(entities))
  for (j in 1:length(entities)) {
    url = paste(base_url, entities[j], sep="")
    
    try({
      d = read_xml(url)
      ns = xml_find_all(x=d, xpath="//Subject_ID")
      if (length(ns) > 0) {
        entity_subject_id[j] = paste(sapply(ns, FUN=xml_text), collapse = "; ")
      }
    }, silent=T)
    # print(paste(entities[j], entity_subject_id[j], sep = ": "))
  }
  return(entity_subject_id)
}

test1 = test_run(3)
test2 = test_run(3)
test3 = test_run(3)

subject_IDs_1 = get_subjectID(test1)
subject_IDs_2 = get_subjectID(test2)
subject_IDs_3 = get_subjectID(test3)

# Create CSV with entities and IDs
df = data.frame("Entity" = test1, "Subject_ID" = subject_IDs_1)
write.csv(df, file="entities_and_ids.csv", row.names = FALSE)
