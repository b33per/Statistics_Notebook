# build_all.R — builds the site AND the search index with section support

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

# 5) Create enhanced search index with sections
search_docs <- list()

for (file_path in html_files) {
  cat("Processing:", basename(file_path), "\n")
  
  # Read the HTML file
  html <- xml2::read_html(file_path)
  
  # Get page title
  title_elem <- xml2::xml_find_first(html, "//title")
  page_title <- if (length(title_elem) == 0) basename(file_path) else xml2::xml_text(title_elem)
  
  # Get URL (remove "docs/" from path)
  base_url <- sub("^docs/", "", file_path)
  
  # Get overall page content
  body_elem <- xml2::xml_find_first(html, "//body")
  page_content <- if (length(body_elem) == 0) "" else xml2::xml_text(body_elem)
  page_content <- gsub("\\s+", " ", page_content)
  page_content <- substr(page_content, 1, 5000)  # Shorter for page-level content
  
  # Add the main page entry
  search_docs[[length(search_docs) + 1]] <- list(
    title = page_title,
    url = base_url,
    content = page_content,
    type = "page"
  )
  
  # Find all headings (h1, h2, h3, h4)
  headings <- xml2::xml_find_all(html, "//h1 | //h2 | //h3 | //h4")
  
  for (heading in headings) {
    heading_text <- xml2::xml_text(heading)
    heading_text <- trimws(heading_text)
    
    if (nchar(heading_text) > 0) {
      # Get or create an anchor ID
      heading_id <- xml2::xml_attr(heading, "id")
      
      if (is.na(heading_id) || heading_id == "") {
        # Create anchor from heading text
        heading_id <- tolower(heading_text)
        heading_id <- gsub("[^a-z0-9\\s]", "", heading_id)  # Remove special chars
        heading_id <- gsub("\\s+", "-", heading_id)         # Replace spaces with hyphens
        heading_id <- gsub("^-+|-+$", "", heading_id)       # Remove leading/trailing hyphens
      }
      
      # Get content around this heading (next few paragraphs)
      heading_level <- xml2::xml_name(heading)
      following_content <- ""
      
      # Get the next few siblings to extract context
      next_nodes <- xml2::xml_find_all(heading, "following-sibling::*[position() <= 3 and (self::p or self::div or self::ul or self::ol)]")
      if (length(next_nodes) > 0) {
        following_content <- paste(xml2::xml_text(next_nodes), collapse = " ")
        following_content <- gsub("\\s+", " ", following_content)
        following_content <- substr(following_content, 1, 1000)
      }
      
      # Add the section entry
      search_docs[[length(search_docs) + 1]] <- list(
        title = paste("📍", heading_text),  # Add icon to distinguish sections
        url = paste0(base_url, "#", heading_id),
        content = paste(heading_text, following_content),
        type = "section",
        parent_page = page_title
      )
    }
  }
}

# 6) Write search index
jsonlite::write_json(search_docs, "docs/search-index.json", auto_unbox = TRUE, pretty = TRUE)
cat("✔ Created enhanced search index with", length(search_docs), "entries\n")

# Count pages vs sections
pages <- sum(sapply(search_docs, function(x) x$type == "page"))
sections <- sum(sapply(search_docs, function(x) x$type == "section"))
cat("  - Pages:", pages, "\n")
cat("  - Sections:", sections, "\n")

# Verify file was created
if (file.exists("docs/search-index.json")) {
  cat("✔ search-index.json created successfully!\n")
} else {
  cat("✖ ERROR: search-index.json was not created\n")
}