pacman::p_load("tidyverse", "tidytext", "mgsub")
load("C:/Users/jonat/OneDrive/Desktop/WebScraping_Microbiome/data/new_csv.Rda")

shape_extract <- function(csv){
      #Unnest sentences
      bact_shape <- c("bacilli", "club", "coccoid", "coccus", "cocci",
                 "corynebacteria", "filamentous", "ovoid", "rod ", "rods",
                 "shape", "spherical", "spiral","spirella","spirochete",
                 "spores","vibrio")
      
      sizes <- tibble::as_tibble(csv)
      sizes <- unnest_sentences(tbl = csv, 
                                output = shapes, 
                                input = abstract) %>% 
            filter(str_detect(shapes, paste(bact_shape, collapse = "|")))
      
      #Trim senteces
      sizes$shapes <- mgsub(sizes$shapes, 
                            c('–', '×', 'µm'), 
                            c('','',''))
      sizes$shapes <- mgsub(sizes$shapes,
                            c('[[:digit:]]+'),
                            c(''))

      sizes$shapes <- gsub(".", "", sizes$shapes, fixed = T)
      sizes$shapes <- mgsub(sizes$shapes, 
                            c("\\s+", " , "), 
                            c(" ", ", "))
      sizes$shapes <- str_replace(sizes$shapes, " \\(.*\\)", "")
      sizes <- sizes[!grepl("not formed|are not|absent|no spores|no endospores", sizes$shapes),]
      
      sizes$shapes <- gsub(" , ", ", ", sizes$shapes)
      sizes$shapes <- gsub(" ; ", "; ", sizes$shapes)
      sizes$shapes <- gsub("[[:space:]]*$", "", sizes$shapes)
      sizes$shapes <- gsub('[[:punct:]]+$', '', sizes$shapes)
      
      sizes
}

#reorder data frame, multiple mentions at first, unique mentions next
shape <- shape_extract(new_csv)
duplicated_shape <- shape[shape$genus %in% shape$genus[duplicated(shape$genus)],]
unique_shape <- shape[!(duplicated(shape$genus) | duplicated(shape$genus, fromLast = TRUE)), ]

shape_curate <- rbind(duplicated_shape, unique_shape)
write.csv(shape_curate, file = "data/shape_curate.csv")
