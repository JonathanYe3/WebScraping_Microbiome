#Setup
pacman::p_load("tidyverse", "tidytext", "dplyr")
load("data/new_csv.Rda")

#New data frame for all data
physiology <- as_tibble(new_csv)
physiology$genus <- trimws(physiology$genus) 
physiology$abstract <- NULL

#Format data frames
sources <- "Garrity, G.M., Winters, M. & Searles, D.B. 2001a. Taxonomic Outline of the Procaryotic Genera. Bergey's Manual of Systematic Bacteriology, Second Edition. Release 1.0, Apr 2001: 1-39. "

#Gram stain
load("data/gramstain.Rda")
load("data/gram_except.Rda")

gramstain$genus <- trimws(gramstain$genus)
gramstain$ncbi_id <- NULL
gram_except$genus <- trimws(gram_except$genus)

gramstain$attribute <- 'Gram Stain'
gramstain$attribute_source <- sources
gramstain <- merge(gramstain, gram_except, by = "genus", all = T)
gramstain <- gramstain[,c(1,3,2,4,5,6)]

physiology <- merge(physiology, gramstain, by = c("genus"), all = T)

rm(gram_except, gramstain)

#Respiration/Oxygen utilization
load("data/respiration.Rda")
load("data/respiration_except.Rda")

respiration$genus <- trimws(respiration$genus)
respiration_except$genus <- trimws(respiration_except$genus)
respiration$ncbi_id <- NULL

respiration$attribute <- 'Oxygen Utilization'
respiration$attribute_source <- sources
respiration <- merge(respiration, respiration_except, by = "genus", all = T)
respiration <- respiration[,c(1,3,2,4,5,6)]

physiology <- merge(physiology, respiration, by = c("genus"), all = T)

rm(respiration, respiration_except)

#Size
load("data/size.Rda")

size$genus <- trimws(size$genus)
size$ncbi_id <- NULL

size$attribute <- 'size'
size$attribute_source <- sources

size$context <- NA
size$context_source <- NA

size <- size[,c(1,3,2,4,5,6)]

physiology <- merge(physiology, size, by = c("genus"), all = T)

rm(size)

#Shape
load("data/shape.Rda")

shape$genus <- trimws(shape$genus)
shape$ncbi_id <- NULL

shape$attribute <- 'shape'
shape$attribute_source <- sources

shape$context <- NA
shape$context_source <- NA

shape <- shape[,c(1,3,2,4,5,6)]

physiology <- merge(physiology, shape, by = c("genus"), all = T)

rm(shape)

#Squish genus names and finalize ncbi ids
physiology$genus <- gsub(" ", "", physiology$genus)
physiology$genus <- gsub('”', "", physiology$genus)
physiology$genus <- gsub('“', "", physiology$genus)

physiology[] <- lapply(physiology, gsub, pattern = "â€", replacement = "-", fixed = T)
physiology[] <- lapply(physiology, gsub, pattern = "€", replacement = "-", fixed = T)
physiology[] <- lapply(physiology, gsub, pattern = "€œ", replacement = "-", fixed = T)

physiology$genus <- mgsub::mgsub(physiology$genus, 
                                 c("IncertaeSedisXXVI.", "IncertaeSedisXV.", "IncertaeSedisXII.", 
                                   "IncertaeSedisVIII.", "IncertaeSedisVII.", "IncertaeSedisVI.", 
                                   "IncertaeSedisV.", "IncertaeSedisIX.", "IncertaeSedisIV.",
                                   "IncertaeSedisIII.", "IncertaeSedisII.", "IncertaeSedisII",
                                   "incertaesedisI.", "IncertaeSedisI.", "Incertaesedis", "IncertaeSedis"), 
                                 c("","","","","","","","","","","","","","","",""))

ids <- numeric()
for (i in 1L:1675L) {
      ids <- c(ids, as.numeric(taxize::get_ids(physiology[[i,1]], db = "ncbi")))
}

final_physiologies <- data.frame(ids, physiology)
final_physiologies$ncbi_id <- NULL

save(final_physiologies, file = "data/final_physiologies.Rda")
