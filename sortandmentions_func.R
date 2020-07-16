#functions require tidytext, dplyr
tokens <- function(csv){
      tibble_dat <- as_tibble(csv)
      tibble_dat$abstract <- gsub("-", "", tibble_dat$abstract)
      
      tibble_dat %>% 
            unnest_tokens(output = word, input = abstract, strip_punct = FALSE) %>% 
            anti_join(stop_words, by = "word") %>% 
            filter(str_detect(word, "[:alpha:]")) %>% 
            distinct()
}

articlementions <- function(tokens, mentions){
      tokens %>% 
            count(word, name = "article_n") %>%
            filter(article_n >= mentions)
}
