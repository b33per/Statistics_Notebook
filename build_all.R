# build_all.R — builds the site AND the search index

# 1) Make sure docs directory exists
dir.create("docs", showWarnings = FALSE)
if (!file.exists("docs/.nojekyll")) file.create("docs/.nojekyll")

# 2) Build the R Markdown site
cat("Building site...\n")
rmarkdown::render_site()

# Check what was actually built
cat("Checking what was built...\n")
docs_files <- list.files("docs", recursive = TRUE)
cat("Files in docs/:", paste(docs_files, collapse = ", "), "\n")

# If index.html is missing, try to render index.Rmd directly
if (!"index.html" %in% docs_files && file.exists("index.Rmd")) {
  cat("index.html missing, rendering index.Rmd directly...\n")
  rmarkdown::render("index.Rmd", output_dir = "docs")
}

# 3) Install packages needed for search index
if (!require(xml2, quietly = TRUE)) install.packages("xml2")
if (!require(jsonlite, quietly = TRUE)) install.packages("jsonlite")

# 4) Find all HTML files in docs/
html_files <- list.files("docs", pattern = "\\.html$", full.names = TRUE)
cat("Found", length(html_files), "HTML files\n")

# 5) Create search index
search_docs <- list()

for (file_path in html_files) {
  cat("Processing:", basename(file_path), "\n")
  
  # Read the HTML file
  html <- xml2::read_html(file_path)
  
  # Get title
  title_elem <- xml2::xml_find_first(html, "//title")
  title <- if (length(title_elem) == 0) basename(file_path) else xml2::xml_text(title_elem)
  
  # Get content from body
  body_elem <- xml2::xml_find_first(html, "//body")
  content <- if (length(body_elem) == 0) "" else xml2::xml_text(body_elem)
  
  # Clean up content
  content <- gsub("\\s+", " ", content)
  content <- substr(content, 1, 10000)
  
  # Create URL (remove "docs/" from path)
  url <- sub("^docs/", "", file_path)
  
  # Add to search docs
  search_docs[[length(search_docs) + 1]] <- list(
    title = title,
    url = url,
    content = content
  )
}

# 6) Write search index
jsonlite::write_json(search_docs, "docs/search-index.json", auto_unbox = TRUE)
cat("✔ Created search index with", length(search_docs), "pages\n")

# Verify file was created
if (file.exists("docs/search-index.json")) {
  cat("✔ search-index.json created successfully!\n")
} else {
  cat("✖ ERROR: search-index.json was not created\n")
}