library(tidyverse)
library(httr)

client_id <- "NrRASzSWhu--_nA8K7YK-Qd"
client_secret <- "EGhlbGQfpQpTtKvCdPzmcwI4Hdoh218BDe2FsJZA2tVWt-CbOAKksMJPjMJh_40TduAZHWcueZjZ_GOARp-YvUUd15cQQgN8xKj-PiiePsmGaPbJ4sxe1dvPXCneWnYx"

res <- POST("https://api.yelp.com/oauth2/token",
            body = list(grant_type = "client_credentials",
                        client_id = client_id,
                        client_secret = client_secret))

token <- "EGhlbGQfpQpTtKvCdPzmcwI4Hdoh218BDe2FsJZA2tVWt-CbOAKksMJPjMJh_40TduAZHWcueZjZ_GOARp-YvUUd15cQQgN8xKj-PiiePsmGaPbJ4sxe1dvPXCneWnYx"

yelp <- "https://api.yelp.com"
term <- "restaurant"
location <- "london, uk"
categories <- NULL
limit <- 50
radius <- 8000
url <- modify_url(yelp, path = c("v3", "businesses", "search"),
                  query = list(term = term, location = location, 
                               limit = limit))
header <- paste('Authorization:', 'Bearer', token)
res <- GET(url, add_headers('Authorization' = paste("bearer", token)))
results <- content(res)

#parse and format the results
yelp_httr_parse <- function(x) {
  
  parse_list <- list(id = x$id, 
                     name = x$name, 
                     rating = x$rating, 
                     review_count = x$review_count, 
                     price = x$price,
                     latitude = x$coordinates$latitude, 
                     longitude = x$coordinates$longitude, 
                     address1 = x$location$address1, 
                     city = x$location$city, 
                     state = x$location$state, 
                     distance = x$distance)
  
  parse_list <- lapply(parse_list, FUN = function(x) ifelse(is.null(x), "", x))
  
  df <- data_frame(id=parse_list$id,
                   name=parse_list$name, 
                   rating = parse_list$rating, 
                   review_count = parse_list$review_count, 
                   price= nchar(parse_list$price),
                   latitude=parse_list$latitude, 
                   longitude = parse_list$longitude, 
                   address1 = parse_list$address1, 
                   distance= parse_list$distance)
  df
}

results_list <- lapply(results$businesses, FUN = yelp_httr_parse)
payload <- do.call("rbind", results_list)
payload$distance <- payload$distance/100


load("data1.RData")
#yelp reviews for same restaurants as the tripadvisor dataset
limit2 <- 1
tripadv_res <- avg_score$name
results2=list()
for (i in tripadv_res) {
  term= i
  url2 <- modify_url(yelp, path = c("v3", "businesses", "search"),
                    query = list(term = term, location = location, 
                                 limit = limit2))
  res2 <- GET(url2, add_headers('Authorization' = paste("bearer", token)))
  results2 <- rbind(results2,content(res2))
}

###parse the list
results3 <- results2
results3 <- do.call("rbind", results3)

# results_list2 <- lapply(results3, FUN = yelp_httr_parse)
# payload2 <- do.call("rbind", results_list2)
yelp_parse2 <- function(x){
  x <- lapply(x, FUN = function(x) ifelse(is.null(x), "", x))
  df <- data.frame(
    name=x$name, 
    rating = x$rating, 
    review_count = x$review_count,
    distance = x$distance
    )
  df
}

results_list2 <- lapply(results3, FUN = yelp_parse2)

payload2 <- do.call("rbind", results_list2)
payload2$distance <- payload2$distance/100

# yelp_price <- function(x){
#   rnames <- names(x)
#   if ("price" %in% rnames==FALSE){
#     x[["price"]] <- ""
#   }
#   #x
#   if (class(x)=="list") {
#     df <- data.frame(
#       name=x$name,
#       rating = x$rating,
#       review_count = x$review_count,
#       price = nchar(x$price)
#     )
#   }
#   df
#   
# }

# price_list<- lapply(results3, FUN = yelp_price)

## comparison between tripadvisor and yelp
colnames(payload2)[colnames(payload2)=="rating"] <- "score"
colnames(avg_score)[colnames(avg_score)=="avg_score"] <- "score"
avg_score <- inner_join(avg_score,payload2[c("name","distance","review_count")])
payload2$yortrip <- "Yelp"
avg_score$yortrip <- "Tripadvisor"
yelp_tripadv <- full_join(payload2,avg_score) 
yelp_tripadv <- yelp_tripadv[c("name","score","review_count","distance","yortrip")] %>% as.tibble() %>% group_by(yortrip)

save.image("~/Documents/MA415/londonfood/data1.RData")
save(yelp_tripadv,payload,file="data1 copy.RData")
