#### Companies and Sectors ####
library(tidyverse)
library(rvest)

h <- read_html("https://www.kap.org.tr/en/Sektorler")      

text <- h %>% html_text()

text <- text %>% str_remove_all("\\\\")            # Unnecessary backslashes


pattern_main_to_sub <- "\\{\\\"mainSector\\\":\\\"[^\\}]+\\\",\\\"normalSector\\\":\\[(\\\"[^\"]+\\\",?\\s*)+\\]"
pattern_stock_subsectors <- "\\{\\\"sectorName\\\":\\\"[^\\}]+\\\"[^\\}]*\\\"stockCode\\\":\\\"[^\\}]+\\\"[^\\}]*\\}"
pattern_stock_mainsectors <- "\\{\\\"mainSectorName\\\":\\\"[^\\}]+\\\"[^\\}]*\\\"stockCode\\\":\\\"[^\\}]+\\\"[^\\}]*\\}"


main_to_sub_text <- text %>% str_extract_all(pattern_main_to_sub)
stock_subsectors_text <- text %>% str_extract_all(pattern_stock_subsectors)
stock_mainsectors_text <- text %>% str_extract_all(pattern_stock_mainsectors)



##Main and Subsectors####
n_sub <- 150                  #Number of subsectors, more or less
frame <- matrix(NA, nrow = n_sub, ncol = 2)

i <- 1
for(i in 1:n_sub){
  text <- main_to_sub_text[[1]][i]
  
  mainsector <- str_match(text, pattern = "\\\"mainSector\\\":\\\"([^\"]+)\\\",")[2]
  subsector <- str_match(text, pattern = "\\\"normalSector\\\":\\[\\\"([^\\]]+)\\\"")[2]
  
  frame[i,c(1,2)] <- c(mainsector, subsector)
}


main_sub_sectors <- data.frame(mainsector = frame[,1], subsector = frame[,2]) %>% 
  separate_rows(subsector, sep = "\",") %>%
  mutate(subsector = str_remove(subsector, "\"")) %>%
  drop_na() 


main_sub_sectors %>% 
  group_by(mainsector) %>%
  summarise(sub_sector_count = n()) %>%
  arrange(desc(sub_sector_count))





###Hierarchical structure of sectors
library(networkD3)

#Convert dataframe to hierarchical list
convert_to_treelike <- function(df = main_sub_sectors){
  mainsectors_list <- split(df$subsector, df$mainsector)
  children <- lapply(names(mainsectors_list), function(x){
    list(name = x,
         children = lapply(mainsectors_list[[x]], function(y){
           list(name = y)
         }))
  })
  list(name = "MAIN SECTORS", children = children)
}

data <- convert_to_treelike()

diagonalNetwork(List = data, fontSize = 17, height = 2000, width = 1300, nodeColour = "red")





##Stocks and Subsectors####
n_share <- 1000                  #Number of shares, more or less
frame <- matrix(NA, nrow = n_share, ncol = 2)

i <- 1
for(i in 1:n_share){
  text <- stock_subsectors_text[[1]][i]
  
  sector <- str_match(text, pattern = "\\\"sectorName\\\":\\\"([^\"]+)\\\",")[2]
  stock <- str_match(text, pattern = "\\\"stockCode\\\":\\\"([A-Z+,\\s\\d-]+)\\\"")[2]
  
  frame[i,c(1,2)] <- c(stock, sector)
}



stock_subsectors <- data.frame(stock = frame[,1], subsector = frame[,2]) %>%
  drop_na()

stock_subsectors <- stock_subsectors %>%
  rowwise() %>%
  mutate(
    stock = if(str_detect(stock, ",")){
      codes <- str_split(stock, ",")[[1]]
      codes <- trimws(codes)
      codes[which.max(nchar(codes))]
    }else{
      stock
    }
  ) %>%
  ungroup()


stock_subsectors[stock_subsectors$stock == "KRDMA",]$stock <- "KRDMD"
stock_subsectors[stock_subsectors$stock == "ISATR",]$stock <- "ISCTR"






##Stocks and Mainsectors####
frame <- matrix(NA, nrow = n_share, ncol = 2)

i <- 1
for(i in 1:n_share){
  text <- stock_mainsectors_text[[1]][i]
  
  sector <- str_match(text, pattern = "\\\"mainSectorName\\\":\\\"([^\"]+)\\\",")[2]
  stock <- str_match(text, pattern = "\\\"stockCode\\\":\\\"([A-Z+,\\s\\d-]+)\\\"")[2]
  
  frame[i,c(1,2)] <- c(stock, sector)
}



stock_mainsectors <- data.frame(stock = frame[,1], mainsector = frame[,2]) %>%
  drop_na()

stock_mainsectors <- stock_mainsectors %>%
  rowwise() %>%
  mutate(
    stock = if(str_detect(stock, ",")){
      codes <- str_split(stock, ",")[[1]]
      codes <- trimws(codes)
      codes[which.max(nchar(codes))]
    }else{
      stock
    }
  ) %>%
  ungroup()


stock_mainsectors[stock_mainsectors$stock == "KRDMA",]$stock <- "KRDMD"
stock_mainsectors[stock_mainsectors$stock == "ISATR",]$stock <- "ISCTR"





##Stocks and Main-Sub Sectors ####

stock_main_sub_sectors <- merge(stock_mainsectors, stock_subsectors, by.y = "stock")




##Stocks and Sectors ####
#As it seems, there is a disbalance between sectors 
stock_main_sub_sectors %>% 
  group_by(mainsector) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  print(n = Inf)


#We add additional sector column
stock_sectors <- stock_main_sub_sectors %>%
  rowwise() %>%
  mutate(sector = 
           if(mainsector %in% c("MANUFACTURING", "FINANCIAL INSTITUTIONS")){
             subsector
           }else{
             mainsector
           }
  ) %>%
  select(stock, sector)


stock_sectors %>% 
  group_by(sector) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  print(n = Inf)














#### Stock Price Data ####
library(tidyverse)
library(rjson)
library(rvest)
library(quantmod)

## Quarter ####

params <- c("TotalRevenue", "NetIncome", "TotalAssets", "CurrentLiabilities", 
            "TotalLiabilitiesNetMinorityInterest", "TotalEquityGrossMinorityInterest",
            "InvestingCashFlow", "ShareIssued", "WorkingCapital", "OperatingExpense")

date <- as.Date(c("2023-09-30", "2023-12-31", "2024-03-31", "2024-06-30", "2024-09-30"))     #Should be changed for later use


url_maker <- function(vec){
  vec[1] <- paste0("quarterly", vec[1])
  
  items <- vector(length = length(vec))
  
  items[1] <- vec[1]
  
  for(i in 2:length(vec)){
    items[i] <- paste0("%2Cquarterly", vec[i])
  }
  
  url <- paste0(items, collapse = "")
  return(url)
}


url_head <- "https://query1.finance.yahoo.com/ws/fundamentals-timeseries/v1/finance/timeseries/stock_symbol.IS?merge=false&padTimeSeries=true&period1=493590046&period2=1729954799&type="
url_body <- url_maker(params)
url_tail <- "&lang=en-US&region=US"



full_url <- vector(length = nrow(stock_sectors))


for(i in 1:nrow(stock_sectors)){
  full_url[i] <- str_replace(url_head, "stock_symbol", stock_sectors$stock[i])
}



line <- function(df_example) {
  ordered_params <- paste0("quarterly", params)
  
  values2024.09 <- df_example$V5[match(ordered_params, df_example$param)]      # Objects should be changed
  values2024.06 <- df_example$V4[match(ordered_params, df_example$param)]
  values2023.03 <- df_example$V3[match(ordered_params, df_example$param)]
  values2023.12 <- df_example$V2[match(ordered_params, df_example$param)]
  values2023.09 <- df_example$V1[match(ordered_params, df_example$param)]
  
  vec <- c(stock, values2024.09, values2024.06, values2023.03, values2023.12, values2023.09)     #Should be changed accordingly
  
  return(vec)
}



pull <- function(j){
  api_url <- paste0(full_url[j], url_body, url_tail)
  
  response <- httr::GET(api_url)
  
  text <- httr::content(response, "text")
  
  parsed_data <- fromJSON(text)
  
  return(parsed_data)
}





frame <- matrix(nrow = nrow(stock_sectors), ncol = length(params) * length(date) + 1)

j <- 1
repeat{
  
  stock <- stock_sectors$stock[j]
  
  parsed_data <- pull(j)
  
  
  value <- matrix(nrow = length(params), ncol = 5)
  symbol <- vector(length = length(params))
  
  for(i in 1:length(params)){
    
    symbol[i] <- parsed_data$timeseries$result[[i]]$meta$type
    
    for (k in 1:5) {
      
      value_for_path <- tryCatch(
        parsed_data$timeseries$result[[i]][[symbol[i]]][[k]]$reportedValue$raw,
        error = function(e) NA,  
        warning = function(w) NA
      )
      
      dat <- tryCatch(
        parsed_data$timeseries$result[[i]][[symbol[i]]][[k]]$asOfDate,
        error = function(e) NA,  
        warning = function(w) NA
      )
      
      value_pos <- which(date == dat)
      
      value[i, value_pos] <- ifelse(is.null(value_for_path), NA, value_for_path)
    }
  }
  
  
  df_example <- as.data.frame(value) %>% 
    mutate(param = symbol) %>%
    select(param, 6:1)
  
  frame[j,] <- line(df_example)
  
  Sys.sleep(runif(1, 2, 10))              #Being responsible 
  
  j <- j + 1
  if(j > length(stock_sectors$stock)){
    break
  }
}


df_quarter <- as.data.frame(frame)


df_quarter[2:51] <- lapply(df_quarter[2:51], function(x) as.numeric(x))
df_quarter[df_quarter == 0] <- NA



params1 <- paste0("24-09", params)
params2 <- paste0("24-06", params)
params3 <- paste0("24-03", params)
params4 <- paste0("23-12", params)
params5 <- paste0("23-09", params)

colnames(df_quarter)[1:51] <- c("stock", params1, params2, params3, params4, params5)


df_quarter <- merge(stock_sectors, df_quarter, by.x = "stock") 









w_before <- (0.2 * exp(-0.2 * seq(1,20))) / sum(0.2 * exp(-0.2 * seq(1,20)))
w_present <- (1.5 - abs(seq(1, 20) - 10) / 9) / sum((1.5 - abs(seq(1, 20) - 10) / 9))
w_after <- rep(1, 20) / sum(rep(1, 20))


n <- nrow(df_quarter)

prices_quarter <- data.frame(stock = character(n),
                             before = numeric(n),
                             present = numeric(n),
                             after = numeric(n), 
                             stringsAsFactors = FALSE)


i <- 1

repeat {
  
  stock <- paste0(stock_sectors$stock[i], ".IS")
  
  stock_price_data <- suppressWarnings(getSymbols(stock, src = "yahoo", auto.assign = FALSE))
  
  before_prices <- stock_price_data["2024-09-17/2024-10-14", 6] 
  present_prices <- stock_price_data["2024-10-15/2024-11-12", 6] 
  after_prices <- stock_price_data["2024-11-13/2024-12-10", 6]
  
  if(length(before_prices) != 20 | length(present_prices) != 20 | length(after_prices) != 20){
    before <- NA
    present <- NA
    after <- NA
  } else {
    before <- round(sum(before_prices * w_before), 1)
    present <- round(sum(present_prices * w_present), 1)
    after <- round(sum(after_prices * w_after), 1)
  }
  
  
  prices_quarter[i, ] <- list(stock_sectors$stock[i], before, present, after)
  
  i <- i + 1
  
  if(i > n){
    break
  }
}








## Annual ####

url_maker2 <- function(vec){
  vec[1] <- paste0("annual", vec[1])
  
  items <- vector(length = length(vec))
  
  items[1] <- vec[1]
  
  for(i in 2:length(vec)){
    items[i] <- paste0("%2Cannual", vec[i])
  }
  
  url <- paste0(items, collapse = "")
  return(url)
}





params2 <- c("TotalRevenue", "NetIncome", "TotalAssets", "CurrentLiabilities", 
             "TotalLiabilitiesNetMinorityInterest", "TotalEquityGrossMinorityInterest",
             "InvestingCashFlow", "ShareIssued", "WorkingCapital", "CostOfRevenue")

date2 <- as.Date(c("2024-12-31", "2023-12-31", "2022-12-31", "2021-12-31"))     # Should be changed


url_head2 <- "https://query1.finance.yahoo.com/ws/fundamentals-timeseries/v1/finance/timeseries/stock_symbol.IS?merge=false&padTimeSeries=true&period1=493590046&period2=1735603200&type="
url_body2 <- url_maker2(params2)
url_tail2 <- "&lang=en-US&region=US"



full_url2 <- vector(length = nrow(stock_sectors))


for(i in 1:nrow(stock_sectors)){
  full_url2[i] <- str_replace(url_head2, "stock_symbol", stock_sectors$stock[i])
}




line2 <- function(df_example) {
  ordered_params <- paste0("annual", params2)
  
  values2024 <- df_example$V1[match(ordered_params, df_example$param)]
  values2023 <- df_example$V2[match(ordered_params, df_example$param)]
  values2022 <- df_example$V3[match(ordered_params, df_example$param)]
  values2021 <- df_example$V4[match(ordered_params, df_example$param)]
  
  vec <- c(stock, values2024, values2023, values2022, values2021)
  
  return(vec)
}



pull2 <- function(j){
  api_url2 <- paste0(full_url2[j], url_body2, url_tail2)
  
  response <- httr::GET(api_url2)
  
  text <- httr::content(response, "text")
  
  parsed_data <- fromJSON(text)
  
  return(parsed_data)
}





frame2 <- matrix(nrow = nrow(stock_sectors), ncol = length(params2) * length(date2) + 1)

j <- 1
repeat{
  
  stock <- stock_sectors$stock[j]    
  
  parsed_data <- pull2(j)       
  
  
  value <- matrix(nrow = length(params2), ncol = 4)
  symbol <- vector(length = length(params2))
  
  for(i in 1:length(params2)){
    
    symbol[i] <- parsed_data$timeseries$result[[i]]$meta$type     
    
    for (k in 1:4) {
      
      value_for_path <- tryCatch(
        parsed_data$timeseries$result[[i]][[symbol[i]]][[k]]$reportedValue$raw,     
        error = function(e) NA,  
        warning = function(w) NA
      )
      
      dat <- tryCatch(
        parsed_data$timeseries$result[[i]][[symbol[i]]][[k]]$asOfDate,        
        error = function(e) NA,  
        warning = function(w) NA
      )
      
      value_pos <- which(date2 == dat)
      
      value[i, value_pos] <- ifelse(is.null(value_for_path), NA, value_for_path)     
    }
  }
  
  
  df_example <- as.data.frame(value) %>% 
    mutate(param = symbol) %>%
    select(param, 5:1)
  
  frame2[j,] <- line2(df_example)
  
  Sys.sleep(runif(1, 2, 10))
  
  j <- j + 1
  if(j > length(stock_sectors$stock)){
    break
  }
}



df_annual <- as.data.frame(frame2)


df_annual[2:41] <- lapply(df_annual[2:41], function(x) as.numeric(x))
df_annual[df_annual == 0] <- NA



param1 <- paste0("2024", params2)
param2 <- paste0("2023", params2)
param3 <- paste0("2022", params2)
param4 <- paste0("2021", params2)


colnames(df_annual)[1:41] <- c("stock", param1, param2, param3, param4)


df_annual <- merge(stock_sectors, df_annual, by.x = "stock") 








w_before <- (0.05 * exp(-0.05 * seq(1, 40))) / sum(0.05 * exp(-0.05 * seq(1, 40)))
w_after <- rev((0.05 * exp(-0.05 * seq(1, 40))) / sum(0.05 * exp(-0.05 * seq(1, 40))))

n <- nrow(df_annual)

prices_annual <- data.frame(stock = character(n),
                            before = numeric(n),
                            after = numeric(n), 
                            stringsAsFactors = FALSE)



i <- 1

repeat {
  
  stock <- paste0(stock_sectors$stock[i], ".IS")
  
  stock_price_data <- suppressWarnings(getSymbols(stock, src = "yahoo", auto.assign = FALSE))
  
  before_prices <- stock_price_data["2024-02-06/2024-04-01", 6]              
  after_prices <- stock_price_data["2025-02-15/2025-04-15", 6]              
  
  
  if(length(before_prices) != 40 | length(after_prices) != 40){
    before <- NA
    after <- NA
  } else {
    before <- round(sum(before_prices * w_before), 1)
    after <- round(sum(after_prices * w_after), 1)
  }
  
  
  prices_annual[i, ] <- list(stock_sectors$stock[i], before, after)
  
  i <- i + 1
  
  if(i > n){
    break
  }
}













