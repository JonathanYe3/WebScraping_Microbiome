pacman::p_load("tidyverse", "tidytext")
load("C:/Users/jonat/OneDrive/Desktop/WebScraping_Microbiome/data/new_csv.Rda")
source("sortandmentions_func.R")
my_tokens <- tokens(new_csv)

#Respiration
fac_v_ob <- my_tokens %>% 
      filter(word == "facultative" |
                word == "facultatively" |
                word == "obligate" |
                word == "obligately") %>% 
      distinct(genus, .keep_all = TRUE) %>% 
      as.data.frame() %>% 
      mutate(word = gsub("facultatively", "facultative", word)) %>% 
      mutate(word = gsub("obligately", "obligate", word))

aerobic <- my_tokens %>% 
      filter(word == "aerobic" |
             word == "aerobe") %>% 
      distinct(genus, .keep_all = TRUE) %>% 
      as.data.frame() %>%
      mutate(word = gsub("aerobic", "aerobe", word)) %>%
      merge(fac_v_ob, by = c("genus", "ncbi_id"), all = T) %>% 
      unite("aerobic", c(word.y, word.x), 
            sep = " ", remove = T, na.rm = T) %>% 
      filter(!aerobic == "facultative") %>% 
      filter(!aerobic == "obligate")

anaerobic <- my_tokens %>% 
      filter(word == "anaerobic" |
             word == "anaerobe") %>% 
      distinct(genus, .keep_all = TRUE) %>% 
      as.data.frame() %>% 
      mutate(word = gsub("anaerobic", "anaerobe", word)) %>%
      merge(fac_v_ob, by = c("genus", "ncbi_id"), all = T) %>% 
      unite("anaerobic", c(word.y, word.x), 
            sep = " ", remove = T, na.rm = T) %>% 
      filter(!anaerobic == "facultative") %>% 
      filter(!anaerobic == "obligate")

#Data.frame with articles that need manual curation
manual_robic <- merge(aerobic, anaerobic, by = c("genus", "ncbi_id")) %>% 
      distinct(genus, .keep_all = TRUE) %>% 
      add_column(respiration = NA, .after = 4)

save(manual_robic, file = "data/manual_robic.Rda")

#Data.frame with accurate articles
accurate_robic <- merge(aerobic, anaerobic, by = c("genus", "ncbi_id"), all = T) %>%
      anti_join(manual_robic) %>% 
      mutate(respiration = coalesce(aerobic, anaerobic)) %>% 
      subset(select = -c(aerobic, anaerobic))

save(accurate_robic, file = "data/accurate_robic.Rda")

