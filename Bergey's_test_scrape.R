library(pacman)
pacman::p_load("rvest", "dplyr", "stringr")

#Get url, author = Dedysh
B_url <- read_html("https://onlinelibrary.wiley.com/doi/10.1002/9781118960608.gbm01666")

#Get genus
genus <- B_url %>%
      html_nodes(".citation__title i") %>%
      html_text()

#Get paragraph
paragraph <- B_url %>%
      html_nodes("p:nth-child(3)") %>%
      html_text()
paragraph


my_data <- data.frame(genus = genus, text = paragraph[[2]])
