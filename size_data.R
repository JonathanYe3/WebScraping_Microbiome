pacman::p_load("tidyverse", "tidytext", "mgsub")
load("C:/Users/jonat/OneDrive/Desktop/WebScraping_Microbiome/data/new_csv.Rda")

size_detect <- function(csv){
   #Unnest sentences
   sizes <- as_tibble(csv)
   sizes <- unnest_sentences(tbl = csv, 
                             output = sentences, 
                             input = abstract) %>% 
      filter(str_detect(sentences, "µm"))
   #Trim essential words
   sizes$sentences <- mgsub(sizes$sentences, 
                            c("–", " to ", "×", "µm"), 
                            c("_", "_", " × ", "量"))
   sizes$sentences <- mgsub(sizes$sentences,
                            c("wide", "in width", "width is", "widths from"),
                            c("宽", "宽", "度", "度"))
   sizes$sentences <- mgsub(sizes$sentences,
                            c("in diameter", "diameter is", "diameter of"),
                            c("直", "径", "径"))
   sizes$sentences <- mgsub(sizes$sentences,
                            c("in length", "length is", "lengths from", "long |long."),
                            c("长", "短", "短", "长"))
   #Filter words
   sizes$sentences <- gsub("[^0-9.量宽直长度径短 _×±]", " ", sizes$sentences)
   sizes$sentences <- gsub("\\s+","", sizes$sentences)
   #Reformat essential words/characters
   sizes$sentences <- mgsub(sizes$sentences,
                            c("量", "宽", "度"),
                            c(" µm ", "wide; ", "width is "))
   sizes$sentences <- mgsub(sizes$sentences,
                            c("直", "径"),
                            c("in diameter; ", "diameter is "))
   sizes$sentences <- mgsub(sizes$sentences,
                            c("长", "短"),
                            c("in length; ", "length is "))
   sizes$sentences <- mgsub(sizes$sentences,
                            c("_", "×"),
                            c("–", " × "))
   sizes$sentences <- mgsub(sizes$sentences,
                            c('^\\.|\\.$', '^\\s+|\\s+$', '^\\–|\\–$', '  '),
                            c('', '', '', ''))
   sizes$sentences <- gsub('^\\–|\\–$', '', sizes$sentences)
   #Add semicolons
   #sizes$sentences <- mgsub(sizes$sentences,
   #c("in diameter ", "in length ", "wide"),
   #c("in diameter; ", "in length; ", "wide; "))
   sizes$sentences <- gsub('^\\in length; |\\; $|\\;$', '', sizes$sentences)
   sizes$sentences <- gsub('^\\in length; ', '', sizes$sentences)
   sizes$sentences <- gsub('m×', 'm ×', sizes$sentences)
   
   sizes
}

#Full df with size data
size <- size_detect(new_csv)
write.csv(size, file = "microbial_sizes.csv")
