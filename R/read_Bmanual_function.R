pacman::p_load("pacman", "rvest", "dplyr", 
               "stringr", "tidyverse", "tidytext")

read_Bmanual <- function(a,b){
      
      x <- c(a:b)
      Bergeys <- c("https://onlinelibrary.wiley.com/doi/10.1002/9781118960608.gbm") %>% 
            paste0(sprintf("%05d",x))
      
      #Get genus
      extract_genus <- function(url){
            data <- read_html(url)
            f_genus <- data %>%
                  html_nodes(".citation__title i") %>%
                  html_text() %>% 
                  as_tibble()
      }
      
      #Get paragraph
      extract_paragraph <- function(url){
            data <- read_html(url)
            f_paragraph <- data %>% 
                  html_nodes("#section-1-en p:nth-child(3)") %>%
                  html_text() %>% 
                  as_tibble()
      }
      
      genus_name <- map_dfr(Bergeys, extract_genus)
      paragraph_info <- map_dfr(Bergeys, extract_paragraph)
      
      my_df <- data.frame(genus_name, paragraph_info) %>% 
            mutate(across(where(is.character), str_trim))
}