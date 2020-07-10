pacman::p_load(tidyverse, taxize)
data("bergeys", package = "bergeys.webscraper")

#Remove NA and sort alphabetically
c_bergeys <- data.frame(matrix(unlist(bergeys), nrow=nrow(bergeys)),stringsAsFactors=FALSE) %>%
      drop_na()
c_bergeys <- c_bergeys  %>% 
      arrange(X1, )

#Get ncbi ids
ids <- numeric()
for (i in 1L:1615L) {
      ids <- c(ids, as.numeric(get_ids(c_bergeys[[i,1]], db = "ncbi")))
}
id_bergeys <- data.frame(ids, c_bergeys) 
names(id_bergeys)[1] <- "ncbi_id"
names(id_bergeys)[2] <- "genus"
names(id_bergeys)[3] <- "abstract"
save(id_bergeys, file = "id_bergeys.Rda", compress = "xz")
