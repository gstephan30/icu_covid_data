library(rvest)
library(stringr)
library(dplyr)
library(purrr)
library(tidyr)

base_page <- "https://edoc.rki.de/handle/176904/7011/recent-submissions?offset="
lookup_pages <- seq(0, 1000, 20)
look_url <- paste0(base_page, lookup_pages)

get_page_links <- function(url) {
  print(paste0("Inspecting page: ", url))
  
  url %>% 
    read_html() %>% 
    as.character() %>% 
    str_extract_all(., "176904\\/[0-9]{4}") %>% 
    unlist() %>% 
    unique()
}
#get_page_links("https://edoc.rki.de/handle/176904/7013/recent-submissions?offset=180")

get_file_link <- function(page_handle) {
  url <- paste0("https://edoc.rki.de/handle/", page_handle)
  
  print(paste0("Receiving link from: ", url))
  
  Sys.sleep(0.5)
  
  url %>% 
    read_html() %>% 
    as.character() %>% 
    str_extract(., "https://edoc.rki.de/bitstream/handle(.+).csv")
}
#get_file_link("176904/7329")

divi_links <- tibble(
  base_url = look_url
) %>% 
  rowwise() %>% 
  mutate(url_handles = map(base_url, get_page_links)) %>% 
  unnest(url_handles) %>% 
  distinct(url_handles) %>% 
  rowwise() %>% 
  mutate(file_url = map(url_handles, get_file_link)) %>% 
  unnest(file_url) %>% 
  drop_na() %>% 
  mutate(
    date = str_extract(file_url, "[0-9]{4}-[0-9]{2}-[0-9]{2}"),
    dest_name = paste0("data_raw/", date, "_icu_data.csv")) %>% 
  select(file_url, dest_name)

for (i in 1:nrow(divi_links)) {
  print(paste0("Saving: ", pull(divi_links[i, 2])))
  
  Sys.sleep(0.6)
  
  download.file(
    url = pull(divi_links[i, 1]),
    destfile = pull(divi_links[i, 2])
  )
}



