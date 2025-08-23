# build_search_index.R
# Creates docs/search-index.json by scraping text from every HTML page in docs/
library(xml2)
library(jsonlite)

pages <- list.files("docs", pattern = "\\.html$", recursive = TRUE, full.names = TRUE)

read_doc <- function(path) {
  url <- sub("^docs/", "", path)                 # relative URL for the site
  html <- read_html(path)
  title <- xml_text(xml_find_first(html, "//title"))
  body_text <- xml_text(xml_find_first(html, "//body"))
  body_text <- gsub("\\s+", " ", body_text)
  body_text <- trimws(body_text)
  body_text <- substr(body_text, 1, 20000)       # cap to keep index light
  list(title = title, url = url, content = body_text)
}

docs <- lapply(pages, read_doc)
write_json(docs, "docs/search-index.json", auto_unbox = TRUE)
cat("wrote docs/search-index.json with", length(docs), "pages\n")
