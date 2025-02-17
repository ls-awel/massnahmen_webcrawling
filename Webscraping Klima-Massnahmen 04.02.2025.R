
################################################################################
################################################################################
### Webcrawl Klima-Massnahmen ###
################################################################################
### alle Massnahmen ###
### I used the code below to create the final Excel file that was sent to Infras
################################################################################

# Install packages to read excel from Quelltext
install.packages("tidyverse")
install.packages("rvest")
install.packages("writexl")

# Load necessary libraries
library(rvest)
library(writexl)
library(httr2)

req <- 

resp <- req %>% 
  httr2::req_perform()

json <- resp %>% httr2::resp_body_json()

pages <- json[["numberOfResultPages"]]


get_json <- function(page = 1, cache_location = "cache_2025-02-05") {
  fn <- fs::path(cache_location, page, ext = "json")
  
  if (fs::file_exists(fn)) {
    # load from file
    raw <- readr::read_file(fn)
  } else {
    resp <- httr2::request("https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/jcr:content/main/flexdatasearch.zhweb-flexdata.zhweb-cache.json?page=1&order=ascending&orderBy=recordfield-flexdataschemacombin-left") %>% 
      httr2::req_url_query(page = page) %>% 
#      httr2::req_cache(path = cache_location, debug = TRUE) %>% 
      httr2::req_perform() 

    # save to file 
    raw <- httr2::resp_body_string(resp)
    readr::write_file(raw, fn)
  }
  
  jsonlite::fromJSON(raw)
}

json <- get_json(1)

json2 <- get_json(2)


results <- purrr::map(1:12, get_json)

df <- purrr::map(results, "data") %>% 
  dplyr::bind_rows()

httr2::request("https://www.zh.ch") %>% 
  httr2::req_url_path(df$link[1]) %>% 
  httr2::req_dry_run()

# Function to crawl website
crawl_website <- function(url) {
  # Read HTML content from URL
  webpage <- read_html(url)
  
  # Extract title
  titles <- webpage %>% html_nodes("h1") 
  title_texts <- character()
  
  for (i in seq_along(titles)) {
    title_text <- html_text(titles[i])
    if (i == 2) {  # if second title is needed
      title_texts <- c(title_texts, title_text)
    }
  }
  
  # extract Direktion
  Direktion_element <- rvest::html_element(webpage, xpath = "//dt[contains(text(),'Direktion')]/following-sibling::*")
  Direktion_text <- rvest::html_text(Direktion_element)
  
  
  Direktion <- rvest::html_element(webpage, xpath = "//dt[contains(text(),'Direktion')]/following-sibling::*") %>% 
    rvest::html_text()
  
  
  # Extract subtitles
  subtitles <- webpage %>% html_nodes("dd")
  subtitle_texts <- rep(NA, 14)  # Prepare space for 14 expected fields. I chose this number because it ensures that no info is lost (some of the webpages contain fewer than 14 spaces, but never more)
  
  for (i in seq_along(subtitles)) {
    subtitle_text <- html_text(subtitles[i])
    if (i <= length(subtitle_texts)) {
      subtitle_texts[i] <- subtitle_text
    }
  }
  
  ## Add Massnahmenkürzel based on URL
  #massnahmenkürzel <- sub(".*massnahme-(ge[0-9]+)\\.html$", "\\1", url)
  
  # Prepare results with correct order
  results_df <- data.frame(
    Massnahme = title_texts,
    Massnahmenkürzel = subtitle_texts[1],
    Kurzbeschreibung = subtitle_texts[2],
    Bereich = subtitle_texts[3],
    Sektor = subtitle_texts[4],
    Umsetzungszeitraum = subtitle_texts[5],
    Stand = subtitle_texts[6],
    Direktion = subtitle_texts[7],
    Amt = subtitle_texts[8],
    Abteilung = subtitle_texts[9],
    Co_Direktion = ifelse(length(subtitle_texts) >= 11, subtitle_texts[11], NA),
    Co_Amt = ifelse(length(subtitle_texts) >= 12, subtitle_texts[12], NA),
    Co_Abteilung = ifelse(length(subtitle_texts) >= 13, subtitle_texts[13], NA),
    Kontakt = ifelse(length(subtitle_texts) >= 10, subtitle_texts[10], NA),
    Inhalt = NA,  # Placeholder for paragraphs, see next step
    stringsAsFactors = FALSE
  )
  
  # Fill "Inhalt" with third paragraph if available
  paragraphs <- webpage %>% html_nodes("p.atm-paragraph")
  if (length(paragraphs) >= 3) {
    results_df$Inhalt <- html_text(paragraphs[3])
  }
  
  return(results_df)
}

## List of websites to crawl: Naturgefahren NG01, Gebäude GE01-05, Gesundheit GS01-GS05 (URL for WG05 is GS05), Energieproduktion und -versorgung EN01-05, ...
# Wasser & Gewässer WG01-WG05, Lokalklima & Energie LK01-LK08, Landökosysteme LA01-LA07, Negative Emissionen NE01-NE09, ...
# Abfall-/Abwasserbehandlung AB01-10, Konsum & Güter KG01-KG10, Industrie & Gewerbe IG01-IG10, Land-/Forstwirtschaft LF01-LF13, ...
# Querschnittfelder QS01-QS13, Mobilität MO03-MO21

websites <- c(
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ng01.html',
  
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ge01.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ge02.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ge03.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ge04.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ge05.html',
  
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-gs01.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-gs02.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-gs03.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-gs04.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-gs05.html',
  
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-en01.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-en02.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-en03.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-en04.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-en05.html',
  
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-wg01.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-wg02.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-wg03.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-wg04.html',
  
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-lk01.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-lk02.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-lk03.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-lk04.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-lk05.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-lk06.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-lk07.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-lk08.html',
  
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-la01.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-la02.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-la03.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-la04.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-la05.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-la06.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-la07.html',
  
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ne01.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ne02.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ne03.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ne04.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ne05.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ne06.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ne07.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ne08.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ne09.html',
  
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ab01.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ab02.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ab03.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ab04.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ab05.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ab06.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ab07.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ab08.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ab09.html',
  'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ab10.html'
  
  ## don't forget to add comma after URL above if crawling all webpages together; remove if crawling groups separately
  ## computer may get overwhelmed when crawling many pages, hence separate crawls
  
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-kg01.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-kg02.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-kg03.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-kg04.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-kg05.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-kg06.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-kg07.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-kg08.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-kg09.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-kg10.html',
  # 
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ig01.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ig02.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ig03.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ig04.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ig05.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ig06.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ig07.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ig08.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ig09.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-ig10.html',
  # 
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-lf01.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-lf02.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-lf03.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-lf04.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-lf05.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-lf06.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-lf07.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-lf08.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-lf09.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-lf10.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-lf11.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-lf12.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-lf13.html',
  # 
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-qs01.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-qs02.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-qs03.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-qs04.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-qs05.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-qs06.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-qs07.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-qs08.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-qs09.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-qs10.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-qs11.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-qs12.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-qs13.html',
  # 
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-mo03.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-mo04.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-mo05.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-mo06.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-mo07.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-mo08.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-mo09.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-mo10.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-mo11.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-mo12.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-mo13.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-mo14.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-mo15.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-mo16.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-mo17.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-mo18.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-mo19.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-mo20.html',
  # 'https://www.zh.ch/de/umwelt-tiere/klima/langfristige-klimastrategie/massnahmen/massnahmen-definition/massnahme-mo21.html'
)

# Initialize a data frame to store all results
all_results <- data.frame()

# Loop through each website and crawl
for (url in websites) {
  cat("Crawling:", url, "\n")
  website_results <- crawl_website(url)
  all_results <- rbind(all_results, website_results)
}

# Specify output file name
output_file <- "crawled_results_NG01_GE05_GS04_EN05_AB10_WG05_LK08_LA07_NE09.xlsx" # for partial crawl, see webpage selection above; adjust as needed, see code below 
# output_file <- "crawled_results_KG10_IG10_LF13_QS10_MO21.xlsx"
# output_file <- "crawled_results_alle_Massnahmen.xlsx"

# Write results to Excel file
write_xlsx(all_results, output_file)
cat("All results saved to", output_file, "\n")



################################################################################
################################################################################
################################################################################




