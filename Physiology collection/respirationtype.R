pacman::p_load("tidyverse", "tidytext")
load("C:/Users/jonat/OneDrive/Desktop/WebScraping_Microbiome/data/new_csv.Rda")
source("sortandmentions_func.R")
my_tokens <- tokens(new_csv)

#Respiration
aerobic <- my_tokens %>% 
      filter(word == "aerobic" |
             word == "aerobe") %>% 
      distinct(genus, .keep_all = TRUE) %>% 
      as.data.frame()

anaerobic <- my_tokens %>% 
      filter(word == "anaerobic" |
             word == "anaerobe") %>% 
      distinct(genus, .keep_all = TRUE) %>% 
      as.data.frame()

#Data.frame with articles that need manual curation
manual_robic <- merge(aerobic, anaerobic, by = c("genus", "ncbi_id")) %>% 
      rename(aerobic = word.x, anaerobic = word.y) %>% 
      filter(
            ((aerobic == "aerobic" | aerobic == "aerobe") &
             (anaerobic == "anaerobic" | anaerobic == "anaerobe"))
      ) %>% 
      distinct(genus, .keep_all = TRUE) %>% 
      add_column(respiration = NA, .after = 4)
save(manual_robic, file = "data/manual_robic.Rda")

#Data.frame with accurate articles
accurate_robic <- merge(aerobic, anaerobic, by = c("genus", "ncbi_id"), all = T) %>%
      rename(aerobic = word.x, anaerobic = word.y) %>%
      anti_join(manual_robic) %>% 
      mutate(respiration = coalesce(aerobic, anaerobic)) %>% 
      mutate(respiration = gsub("aerobe", "aerobic", respiration)) %>% 
      mutate(respiration = gsub("anaerobe", "anaerobic", respiration)) %>% 
      subset(select = -c(aerobic, anaerobic))
save(accurate_robic, file = "data/accurate_robic.Rda")

