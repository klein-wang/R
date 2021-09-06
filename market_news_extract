library("msversion")
addpkg("XML","3.98-1.11-r32")
addpkg('R6', '2.4.0')
addpkg('magrittr', '1.5')
addpkg('selectr', '0.2-3')
addpkg('xml2', '1.2.0-ms1-r32')
addpkg('httr', '1.3.1')
addpkg('rvest', '0.3.2')
addpkg('Rcpp', '1.0.2')
library(rvest)

addpkg('stringi', '1.4.3')
addpkg('stringr', '1.4.0')
library('stringr')

addpkg('lubridate', '1.7.4')
library(lubridate)

addpkg('plyr', '1.8.4')
library(plyr)

#addpkg('Rserve', '1.7-3')
#library(Rserve)
#Rserve()

# Set the path for final output
setwd("//Msad/root/AP/APBO/KU/lib/ORD/1. ORI Monitoring/Daily Escalation Email/RDM & R Code")

#Specifying the url for desired website to be scraped
get_webpage <- function(x){
  part1 <- 'https://global-premium.econoday.com/byweek.asp?day='
  part2 <- as.character(day(x))
  part3 <- '&month='
  part4 <- as.character(month(x))
  part5 <- '&year='
  part6 <- as.character(year(x))
  part7 <- '&cust=global-premium'
  
  url <- paste0(part1, part2, part3, part4, part5, part6, part7)
  print(url)
  webpage <- read_html(url)
  return(webpage)
}

get_events <- function(webpage){
  # Get the events items
  econoevents <- webpage %>%
    html_nodes(".econoevents") %>%
    html_text()
  
  econoevents <- sub("^\\s+", "", econoevents)
  #econoevents
  
  # Get the events table
  content <- webpage %>%
    html_nodes(".eventstable") %>%
    html_text()
  full_item_list <- str_extract_all(content, 
                                    "([A-Z]{2,3}:).*?(\\d{1,2}:\\d{1,2} AM ET|\\d{1,2}:\\d{1,2} PM ET|\\s\\s)")[[1]]
  full_item_list <- full_item_list[-1] # Remove "ALL:Global Economics »  "
  full_item_list <- sub("\\n", "", full_item_list)
  full_item_list <- sub("\\s+$", "", full_item_list)
  
  content <- strsplit(content, "\r\n\r")[[1]][1]
  #content
  
  weekdays <- regmatches(content,
                         gregexpr("\\b(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\\s(\\d{1,2})", 
                                  content))[[1]]
  print(weekdays)
  #The Date format has to be numeric to be able to upload to RDM
  #weekdays <- as.Date.numeric(weekdays, "%b %d")
  weekdays <- as.Date(weekdays, "%b %d")
  weekdays <- format(weekdays, "%Y/%m/%d")
  weekdays <- list(weekdays)[[1]]
  #print(class(weekdays[1]))
  
  for (i in 1:length(weekdays)){
    weekdays[i] <- as.Date(weekdays[i],"%Y/%m/%d") - as.Date("1900-01-01", "%Y-%m-%d")
  }
  print(weekdays)
  #weekdays
  
  # Split the events by day and count the total number
  events <- webpage %>%
    html_nodes(".events") %>%
    html_text()
  
  # Get the list of events grouped every weekday
  events[1] <- substring(events[1], 25) # Remove "ALL:Global Economics »"
  events_by_day <- str_extract_all(events, 
                  "(?<=SG:|US:|DE:|UK:|NZ:|CH:|CN:|AU:|HK:|CA:|FR:|IT:|JP:|IN:|EZ:|ALL:)(.*?)(?=\\d{1,2}:\\d{1,2}|\\s\\s)")
  
  # Get the list of region codes grouped every weekday
  region_by_day <- str_extract_all(events, "[A-Z]{2,3}:(.*?)(|\\d{1,2}:\\d{1,2})")
  
  # Get the number of events
  num_events <- c()
  for (i in 1:length(events_by_day)){
    num_events <- append(num_events, length(events_by_day[[i]]))
  }
   

  events_by_day_full_list <- c()
  region_by_day_full_list <- c()
  for (i in 1:length(events_by_day)){
    #events_by_day[[i]] <- events_by_day[[i]][-1]
    events_by_day_full_list <- append(events_by_day_full_list, events_by_day[[i]])
    region_by_day[[i]] <- sub(":", "", region_by_day[[i]])
    region_by_day_full_list <- append(region_by_day_full_list, region_by_day[[i]])
  }
  
  length(events_by_day_full_list)
  length(region_by_day_full_list)

  # Get the event types
  green <- webpage %>%       # Other Notable Event
    html_nodes(".djstar") %>%
    html_text()
  green <- sub("^\\s+", "", green)

  red <- webpage %>%         # Market Moving Event
    html_nodes(".star") %>% 
    html_text()
  red <- sub("^\\s+", "", red)

  
  #black <- webpage %>% 
  #  html_nodes(".bullet") %>%
  #  html_text()
  #black <- sub("^\\s+", "", black) # Exclude from the report
  
  # Create a new data frame for output
  output <- data.frame(matrix(ncol = 5, nrow = sum(num_events)))
  colnames(output) <- c("DATE", "REGION", "EVENT", "TYPE", "ITEM")
  
  output_dates <- c()
  output_region <- c()
  output_event <- c()
  output_type <- c()
  output_item <- c()
  output_day <- c()
  cnt <- 1
  
  for (i in 1:length(num_events)){
    for (j in 1:num_events[i]){
      output_dates <- append(output_dates, weekdays[i])
      output_region <- append(output_region, region_by_day_full_list[cnt])
      output_event <- append(output_event, events_by_day_full_list[cnt])
      output_item <- append(output_item, full_item_list[cnt])
      #output_day <- append(output_day, format(as.Date(paste0("2021/", weekdays[i])), "%A"))
      output_day <- append(output_day, format(as.Date(as.numeric(weekdays[i]), origin = "1900-01-01"), "%A"))
      cnt = cnt+1
    }
  }
  
  output$DATE <- output_dates
  output$REGION <- output_region
  output$EVENT <- output_event
  output$ITEM <- output_item
  output$DAY <- output_day
  
  for (i in 1:sum(num_events)){
    if (full_item_list[i] %in% green){
      output_type <- append(output_type, "Other Notable Event")
    }else if (full_item_list[i] %in% red){
      output_type <- append(output_type, "Market Moving Event")
    }else{
      output_type <- append(output_type, "Remove")
    }
  }
  output$TYPE <- output_type
  
  return(output)
}

region_lookup <- function(x){
  x <- mapvalues(x,
                 from = c('SG', 'US', 'DE', 'UK', 'NZ', 'CH', 'CN', 'AU', 'HK', 'CA', 
                          'FR', 'IT', 'JP', 'IN', 'EZ'),
                 to = c('Singapore', 'USA', 'Germany', 'United Kingdom', 'New Zealand', 'Switzerland', 'China', 'Australia', 'Hong Kong', 'Canada',
                        'France', 'Italy', 'Japan', 'India', 'European Union'))
  return(x)
}

td <- Sys.Date()

webpage <- get_webpage(td)
output <- get_events(webpage)
final_output <- output
final_output$REGION <- region_lookup(final_output$REGION)
final_output <- final_output[,!(names(final_output) %in% c("ITEM"))]
final_output <- final_output[final_output$TYPE != "Remove",]
final_output
#write.csv(final_output,"MARKET_EVENTS.csv", row.names=FALSE)
2021 Economic Calendar
library("msversion")

addpkg("XML","3.98-1.11-r32")
addpkg('R6', '2.4.0')
addpkg('magrittr', '1.5')
addpkg('selectr', '0.2-3')
addpkg('xml2', '1.2.0-ms1-r32')
addpkg('httr', '1.3.1')
addpkg('rvest', '0.3.2')
addpkg('Rcpp', '1.0.2')
library(rvest)

addpkg('stringi', '1.4.3')
addpkg('stringr', '1.4.0')
library('stringr')

addpkg('lubridate', '1.7.4')
library(lubridate)

addpkg('plyr', '1.8.4')
library(plyr)

#addpkg('Rserve', '1.7-3')
#library(Rserve)
#Rserve()

# Set the path for final output
setwd("//Msad/root/AP/APBO/KU/lib/ORD/1. ORI Monitoring/Daily Escalation Email/RDM & R Code")

#Specifying the url for desired website to be scraped
get_webpage <- function(x){
  part1 <- 'https://global-premium.econoday.com/byweek.asp?day='
  part2 <- as.character(day(x))
  part3 <- '&month='
  part4 <- as.character(month(x))
  part5 <- '&year='
  part6 <- as.character(year(x))
  part7 <- '&cust=global-premium'
  
  url <- paste0(part1, part2, part3, part4, part5, part6, part7)
  print(url)
  webpage <- read_html(url)
  return(webpage)
}

get_events <- function(webpage){
  # Get the events items
  econoevents <- webpage %>%
    html_nodes(".econoevents") %>%
    html_text()
  
  econoevents <- sub("^\\s+", "", econoevents)
  #econoevents
  
  # Get the events table
  content <- webpage %>%
    html_nodes(".eventstable") %>%
    html_text()
  full_item_list <- str_extract_all(content, 
                                    "([A-Z]{2,3}:).*?(\\d{1,2}:\\d{1,2} AM ET|\\d{1,2}:\\d{1,2} PM ET|\\s\\s)")[[1]]
  full_item_list <- full_item_list[-1] # Remove "ALL:Global Economics »  "
  full_item_list <- sub("\\n", "", full_item_list)
  full_item_list <- sub("\\s+$", "", full_item_list)
  
  content <- strsplit(content, "\r\n\r")[[1]][1]
  #content
  
  weekdays <- regmatches(content,
                         gregexpr("\\b(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)\\s(\\d{1,2})", 
                                  content))[[1]]
  print(weekdays)
  #The Date format has to be numeric to be able to upload to RDM
  #weekdays <- as.Date.numeric(weekdays, "%b %d")
  weekdays <- as.Date(weekdays, "%b %d")
  weekdays <- format(weekdays, "%Y/%m/%d")
  weekdays <- list(weekdays)[[1]]
  #print(class(weekdays[1]))
  
  for (i in 1:length(weekdays)){
    weekdays[i] <- as.Date(weekdays[i],"%Y/%m/%d") - as.Date("1900-01-01", "%Y-%m-%d")
  }
  print(weekdays)
  #weekdays
  
  # Split the events by day and count the total number
  events <- webpage %>%
    html_nodes(".events") %>%
    html_text()
  
  # Get the list of events grouped every weekday
  events[1] <- substring(events[1], 25) # Remove "ALL:Global Economics »"
  events_by_day <- str_extract_all(events, 
                  "(?<=SG:|US:|DE:|UK:|NZ:|CH:|CN:|AU:|HK:|CA:|FR:|IT:|JP:|IN:|EZ:|ALL:)(.*?)(?=\\d{1,2}:\\d{1,2}|\\s\\s)")
  
  # Get the list of region codes grouped every weekday
  region_by_day <- str_extract_all(events, "[A-Z]{2,3}:(.*?)(|\\d{1,2}:\\d{1,2})")
  
  # Get the number of events
  num_events <- c()
  for (i in 1:length(events_by_day)){
    num_events <- append(num_events, length(events_by_day[[i]]))
  }
   

  events_by_day_full_list <- c()
  region_by_day_full_list <- c()
  for (i in 1:length(events_by_day)){
    #events_by_day[[i]] <- events_by_day[[i]][-1]
    events_by_day_full_list <- append(events_by_day_full_list, events_by_day[[i]])
    region_by_day[[i]] <- sub(":", "", region_by_day[[i]])
    region_by_day_full_list <- append(region_by_day_full_list, region_by_day[[i]])
  }
  
  length(events_by_day_full_list)
  length(region_by_day_full_list)

  # Get the event types
  green <- webpage %>%       # Other Notable Event
    html_nodes(".djstar") %>%
    html_text()
  green <- sub("^\\s+", "", green)

  red <- webpage %>%         # Market Moving Event
    html_nodes(".star") %>% 
    html_text()
  red <- sub("^\\s+", "", red)

  
  #black <- webpage %>% 
  #  html_nodes(".bullet") %>%
  #  html_text()
  #black <- sub("^\\s+", "", black) # Exclude from the report
  
  # Create a new data frame for output
  output <- data.frame(matrix(ncol = 5, nrow = sum(num_events)))
  colnames(output) <- c("DATE", "REGION", "EVENT", "TYPE", "ITEM")
  
  output_dates <- c()
  output_region <- c()
  output_event <- c()
  output_type <- c()
  output_item <- c()
  output_day <- c()
  cnt <- 1
  
  for (i in 1:length(num_events)){
    for (j in 1:num_events[i]){
      output_dates <- append(output_dates, weekdays[i])
      output_region <- append(output_region, region_by_day_full_list[cnt])
      output_event <- append(output_event, events_by_day_full_list[cnt])
      output_item <- append(output_item, full_item_list[cnt])
      #output_day <- append(output_day, format(as.Date(paste0("2021/", weekdays[i])), "%A"))
      output_day <- append(output_day, format(as.Date(as.numeric(weekdays[i]), origin = "1900-01-01"), "%A"))
      cnt = cnt+1
    }
  }
  
  output$DATE <- output_dates
  output$REGION <- output_region
  output$EVENT <- output_event
  output$ITEM <- output_item
  output$DAY <- output_day
  
  for (i in 1:sum(num_events)){
    if (full_item_list[i] %in% green){
      output_type <- append(output_type, "Other Notable Event")
    }else if (full_item_list[i] %in% red){
      output_type <- append(output_type, "Market Moving Event")
    }else{
      output_type <- append(output_type, "Remove")
    }
  }
  output$TYPE <- output_type
  
  return(output)
}

region_lookup <- function(x){
  x <- mapvalues(x,
                 from = c('SG', 'US', 'DE', 'UK', 'NZ', 'CH', 'CN', 'AU', 'HK', 'CA', 
                          'FR', 'IT', 'JP', 'IN', 'EZ'),
                 to = c('Singapore', 'USA', 'Germany', 'United Kingdom', 'New Zealand', 'Switzerland', 'China', 'Australia', 'Hong Kong', 'Canada',
                        'France', 'Italy', 'Japan', 'India', 'European Union'))
  return(x)
}

td <- Sys.Date()

webpage <- get_webpage(td)
output <- get_events(webpage)
final_output <- output
final_output$REGION <- region_lookup(final_output$REGION)
final_output <- final_output[,!(names(final_output) %in% c("ITEM"))]
final_output <- final_output[final_output$TYPE != "Remove",]
final_output
#write.csv(final_output,"MARKET_EVENTS.csv", row.names=FALSE)
