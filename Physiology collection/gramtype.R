#setup
pacman::p_load("tidyverse", "tidytext")
load("C:/Users/jonat/OneDrive/Desktop/WebScraping_Microbiome/data/new_csv.Rda")
source("sortandmentions_func.R")
my_tokens <- tokens(new_csv)
#mentions <- articlementions(my_tokens, 50)

#Gram type
gram_pos <- my_tokens %>% 
      filter(word == "gramstainpositive" |
             word == "grampositive" |
             word == "positive") %>% 
      distinct(genus, .keep_all = TRUE) %>% 
      as.data.frame()

gram_neg <- my_tokens %>% 
      filter(word == "gramstainnegative" |
             word == "gramnegative" |
             word == "negative") %>% 
      distinct(genus, .keep_all = TRUE) %>% 
      as.data.frame()

#Data.frame with articles that need manual curation
manual_gram <- merge(gram_neg, gram_pos, by = c("genus", "ncbi_id")) %>% 
      rename(gram_negative = word.x, gram_positive = word.y) %>% 
      filter(
         ((gram_negative == "gramstainnegative" | gram_negative == "gramnegative") &
          (gram_positive == "gramstainpositive" | gram_positive == "grampositive")) |
          (gram_negative == "negative" & gram_positive == "positive")
      ) %>% 
      distinct(genus, .keep_all = TRUE) %>% 
      add_column(gram_type = NA, .after = 4)
save(manual_gram, file = "data/manual_gram.Rda")

#Data.frame with accurate articles
gramstain <- merge(gram_neg, gram_pos, by = c("genus", "ncbi_id"), all = T) %>%
      rename(gram_negative = word.x, gram_positive = word.y) %>%
      anti_join(manual_gram) 

temp_na <- gramstain[rowSums(is.na(gramstain)) > 0,] %>% 
      mutate(gram_negative = gsub("gramnegative", "negative", gram_negative)) %>% 
      mutate(gram_negative = gsub("gramstainnegative", "negative", gram_negative)) %>% 
      mutate(gram_positive = gsub("grampositive", "positive", gram_positive)) %>% 
      mutate(gram_positive = gsub("gramstainpositive", "positive", gram_positive)) %>% 
      #coalesce values into gram_type
      mutate(gram_type = coalesce(gram_positive, gram_negative)) %>% 
      subset(select = -c(gram_positive, gram_negative))

temp_complete <- gramstain[complete.cases(gramstain),] %>% 
      select(genus, ncbi_id, gram_negative, gram_positive) %>% 
      mutate(gram_negative = na_if(gram_negative, "negative"),
             gram_positive = na_if(gram_positive, "positive")) %>% 
      mutate(gram_negative = gsub("gramnegative", "negative", gram_negative)) %>% 
      mutate(gram_negative = gsub("gramstainnegative", "negative", gram_negative)) %>% 
      mutate(gram_positive = gsub("grampositive", "positive", gram_positive)) %>% 
      mutate(gram_positive = gsub("gramstainpositive", "positive", gram_positive)) %>% 
      #coalesce values into gram_type
      mutate(gram_type = coalesce(gram_positive, gram_negative)) %>% 
      subset(select = -c(gram_positive, gram_negative))

      #Final data.frame with accurate gram-stain info
accurate_gram <- rbind(temp_na, temp_complete)
accurate_gram <- accurate_gram[order(accurate_gram$genus),]
rownames(accurate_gram) <- NULL
save(accurate_gram, file = "data/accurate_gram.Rda")
