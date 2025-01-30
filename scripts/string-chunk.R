library(dplyr)
library(janeaustenr)

d <- tibble(txt = prideprejudice)

d %>%
  unnest_characters(word, txt)

d4<-
  unnest_character_shingles(d,word, txt,format = "text", n = 20,to_lower = F,strip_non_alphanum = F)
dfr<-freq.list(d4$word)
head(dfr)

library(quanteda)
ch1<-char_segment(d$txt,"(.){20}")

text<-d$txt
split_into_chunks <- function(text, chunk_size) {
  # Split the text into individual characters
  chars <- unlist(strsplit(text, NULL))
  
  # Calculate the number of chunks
  num_chunks <- ceiling(length(chars) / chunk_size)
  
  # Initialize an empty list to store the chunks
  chunks <- vector("list", num_chunks)
  
  # Loop through the text and create the chunks
  for (i in seq_len(num_chunks)) {
    start_index <- (i - 1) * chunk_size + 1
    end_index <- min(i * chunk_size, length(chars))
    chunks[[i]] <- paste(chars[start_index:end_index], collapse = "")
  }
  
  return(chunks)
}
text<-tx
# Split the text into chunks of 20 characters
chunks <- split_into_chunks(text, 20)

# Print the chunks
print(chunks)

dfr<-freq.list(unlist(chunks),convert = F)

