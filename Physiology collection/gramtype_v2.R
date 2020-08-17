pacman::p_load("tidyverse", "tidytext", "mgsub")
load("C:/Users/jonat/OneDrive/Desktop/WebScraping_Microbiome/data/new_csv.Rda")

gramstain <- function(csv){
      
      gram <- unnest_sentences(tbl = csv, 
                               output = gram_type, 
                               input = abstract) %>% 
            filter(str_detect(gram_type, "gram"))
      
      gram$gram_type <- mgsub(gram$gram_type, 
                            c("-", "–", "  ", "staining"), 
                            c(" ", " ", " ", "stain"))
      
      gram$gram_type <- mgsub(gram$gram_type,
                             c("gram stain negative", "gram negative", "or negative"),
                             c("负", "负", "负"))
      gram$gram_type <- mgsub(gram$gram_type,
                             c("gram stain variable", "gram variable", "or variable"),
                             c("变", "变", "变"))
      gram$gram_type <- mgsub(gram$gram_type,
                              c("gram stain positive", "gram positive", "or positive"),
                              c("正", "正", "正"))
      
      gram$gram_type <- gsub("[^负变正]", " ", gram$gram_type)
      gram$gram_type <- gsub("\\s+","", gram$gram_type)
      
      multiple <- gram[nchar(gram[,3])>1,]
      single <- gram[nchar(gram[,3])==1,]
      gram <- rbind(multiple, single)
      
      gram$gram_type <- mgsub(gram$gram_type,
                             c("负", "变", "正"),
                             c("negative", "variable", "positive"))
      
      duped_gram <- gram[gram$genus %in% gram$genus[duplicated(gram$genus)],]
      unique_gram <- gram[!(duplicated(gram$genus) | duplicated(gram$genus, fromLast = TRUE)), ]
      gram <- rbind(duped_gram, unique_gram)
}

gram_data <- gramstain(new_csv)
