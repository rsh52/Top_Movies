
## Table Analytics==============================================================

# First establish the URL we want to scape data from, in this IMDB
url <- "https://www.imdb.com/search/title/?groups=top_250&sort=user_rating,desc"
webpage <- read_html(url)

# Using rvest and html_nodes in combination with the Google Chrome Selector
# Gadget extension, tease out specific elements of the site for analysis.
# In this case ratings, titles, year of release, runtimes and certification.
ratings <- webpage %>% 
  html_nodes("div strong") %>% 
  html_text() %>% 
  as.numeric()

ratings <- ratings[!ratings > 10.0] # remove numeric values captures not in range
ratings <- ratings[!is.na(ratings)] # remove all NAs introduced

titles <- webpage %>% 
  html_nodes("h3 a") %>% 
  html_text() 

titles <- titles[!titles == " "] # remove all blank values

year <- webpage %>% 
  html_nodes("h3 span") %>% 
  html_text()

year <- year[c(FALSE, TRUE)] #Remove every other element starting with positon 2
year <- substr(year, 2, 5) #Remove the parentheses

runtimes <- webpage %>% 
  html_nodes(".runtime") %>% 
  html_text() 

genre <- webpage %>% 
  html_nodes(".genre") %>% 
  html_text() %>% 
  stringr::str_squish()

Certificate <- webpage %>% 
  html_nodes(".certificate") %>% 
  html_text() %>% 
  stringr::str_squish()


