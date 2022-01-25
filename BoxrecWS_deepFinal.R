library(rvest)
library(stringr)
library(progress)
library(ggplot2)
library(dplyr)
library(janitor)
library(stringr)
library(tidyr)

############################################################
# Two loops -final all times  - downloading all times boxers profiles
############################################################

# w pierwszym kroku pobieram werktor linkow stron ktore chce skanowac
N = 64850 
pb <- progress_bar$new(total=N)
wektorLinkow<-c()
for(i in seq(from=0, to=N, by=50)){
  newUrl<- paste0("https://boxrec.com/en/ratings?r%5Brole%5D=proboxer&r%5Bsex%5D=M&r%5Bdivision%5D=&r%5Bcountry%5D=&r%5Bstance%5D=&r%5Bstatus%5D=&r_go=&offset=",i)
  print(newUrl)
  page <-read_html(newUrl)
  temp<- page%>%html_nodes(xpath = "//a[@class='personLink']") %>% html_attr(name="href")
  temp<- paste0("https://boxrec.com",temp)
  wektorLinkow<-c(wektorLinkow,temp)
  pb$tick()
}  
wektorLinkowU<- wektorLinkow%>%unique()

# nastepnie pobieram dane dla zwybranych miejsc na stronie zagnieżdżonej 
results_rec <- data.frame('boxerID'=numeric(), 'wins'=numeric(),'defeats'=numeric(),'draws'=numeric(),'wins_by_KO'=character(),'defeats_by_KO'=character())
pb <- progress_bar$new(total=N)
for(lin in wektorLinkowU){
  read_html(lin) %>%
    html_nodes(xpath="//*[@id='pageOuter']//tr[@class='profileTable']") %>% .[[1]] %>%
    html_table() -> temps
  tmp <- as.data.frame.matrix(temps)
  tmp <- data.frame(strsplit(lin, "/")[[1]][6] , temps[1,1],temps[1,2], temps[1,3], temps[2,1], temps[2,2])
  names(tmp) <- c('boxerID','wins','defeats','draws','wins_by_KO','defeats_by_KO')
  results_rec <- rbind(results_rec, tmp)
  pb$tick()  
}

# kolejnym krokiem jest pobranie danych w formie tabeli, zapisanie ich do dataframe i przekształcenie tak aby mozna bylo dowac je w kolejnych wierszach. 
results <- tibble()
pb <- progress_bar$new(total=N) 
for(lin in wektorLinkowU){
  read_html(lin) %>%
    html_nodes(xpath="//*[@id='pageOuter']//tr[@class='profileTable']") %>% .[[1]] %>%
    html_table() -> temps
  temps <-temps[c(1,2)] %>% head(temps,n=20) %>% slice(-c(1,2))
  temps["X1"][temps["X1"] == ""] <- NA
  temps<- as.data.frame(temps)
  temps<- temps[!duplicated(temps["X1"]), ]
  #temps[is.na(temps)] <- runif(sum(is.na(temps)), min = 0, max = 1)
  temps[is.na(temps)] <- sample(row.names(temps[c(is.na(temps)),]), sum(is.na(temps)), replace = TRUE)
  temps[nrow(temps) + 1,] = c("boxerID",strsplit(lin, "/")[[1]][6])
  rownames(temps) <-temps$X1
  tmp <- as.data.frame(t(temps))
  tmp%>% row_to_names(row_number = 1) -> tmp
  results <-bind_rows(results, tmp)
  pb$tick()  
}

############################################################
# Merge dataframes - final data 
############################################################
results <- distinct(results)
write.csv(results,"results_final_alltimes.csv" , row.names = FALSE)
results_rec <- distinct(results_rec)
write.csv(results_rec,"results_rec_final_alltimes.csv" , row.names = FALSE)
############################################################

br_total <- merge(results_rec, results ,by="boxerID") %>% distinct()
write.csv(br_total,"br_total_alltimes.csv" , row.names = FALSE)

############################################################

############################################################
# Two loops -final - Active  - downloading only active profiles of boxers
############################################################
N = 20050
pb <- progress_bar$new(total=N)
wektorLinkow<-c()
for(i in seq(from=0, to=N, by=50)){
  newUrl<- paste0("https://boxrec.com/en/ratings?offset=",i)
  print(newUrl)
  page <-read_html(newUrl)
  temp<- page%>%html_nodes(xpath = "//a[@class='personLink']") %>% html_attr(name="href")
  temp<- paste0("https://boxrec.com",temp)
  wektorLinkow<-c(wektorLinkow,temp)
  pb$tick()
}  
wektorLinkowU<- wektorLinkow%>%unique()

results_rec <- data.frame('boxerID'=numeric(), 'wins'=numeric(),'defeats'=numeric(),'draws'=numeric(),'wins_by_KO'=character(),'defeats_by_KO'=character())
pb <- progress_bar$new(total=N)
for(lin in wektorLinkowU){
  read_html(lin) %>%
    html_nodes(xpath="//*[@id='pageOuter']//tr[@class='profileTable']") %>% .[[1]] %>%
    html_table() -> temps
  tmp <- as.data.frame.matrix(temps)
  tmp <- data.frame(strsplit(lin, "/")[[1]][6] , temps[1,1],temps[1,2], temps[1,3], temps[2,1], temps[2,2])
  names(tmp) <- c('boxerID','wins','defeats','draws','wins_by_KO','defeats_by_KO')
  results_rec <- rbind(results_rec, tmp)
  pb$tick()  
}

results <- tibble()
pb <- progress_bar$new(total=N) 
for(lin in wektorLinkowU){
  read_html(lin) %>%
    html_nodes(xpath="//*[@id='pageOuter']//tr[@class='profileTable']") %>% .[[1]] %>%
    html_table() -> temps
  temps <-temps[c(1,2)] %>% head(temps,n=20) %>% slice(-c(1,2))
  temps["X1"][temps["X1"] == ""] <- NA
  temps<- as.data.frame(temps)
  temps<- temps[!duplicated(temps["X1"]), ]
  #temps[is.na(temps)] <- runif(sum(is.na(temps)), min = 0, max = 1)
  temps[is.na(temps)] <- sample(row.names(temps[c(is.na(temps)),]), sum(is.na(temps)), replace = TRUE)
  temps[nrow(temps) + 1,] = c("boxerID",strsplit(lin, "/")[[1]][6])
  rownames(temps) <-temps$X1
  tmp <- as.data.frame(t(temps))
  tmp%>% row_to_names(row_number = 1) -> tmp
  results <-bind_rows(results, tmp)
  pb$tick()  
}
############################################################
# Merge dataframes - final data
############################################################
results <- distinct(results)
write.csv(results,"results_final.csv" , row.names = FALSE)
results_rec <- distinct(results_rec)
write.csv(results_rec,"results_rec_final.csv" , row.names = FALSE)
############################################################

br_total <- merge(results_rec, results ,by="boxerID") %>% distinct()
write.csv(br_total,"br_total.csv" , row.names = FALSE)

############################################################
############################################################
# main table - active
############################################################
N = 64850
pb <- progress_bar$new(total=N)
wektorLinkow<-c()
for(i in seq(from=0, to=N, by=50)){
  newUrl<- paste0("https://boxrec.com/en/ratings?r%5Brole%5D=proboxer&r%5Bsex%5D=M&r%5Bdivision%5D=&r%5Bcountry%5D=&r%5Bstance%5D=&r%5Bstatus%5D=&r_go=&offset=",i)
  print(newUrl)
  wektorLinkow<-c(wektorLinkow,newUrl)
  pb$tick()
}
wektorLinkowU<- wektorLinkow%>%unique()

# Additionally i noticed there is couple of interesting infrormation on the front 
# page in the general rating table - i will download them using another method. 


#link = 'https://boxrec.com/en/ratings?offset='
results <- data.frame('boxer'=character(), 'boxerID'=numeric(), 'points' = numeric(), 'last1'=character(), 'last2'=character(), 'last3'=character(), 'last4'=character(), 'last5'=character(), 'last6'=character())

for(node in wektorLinkowU){
    
  read_html(node) %>%
    html_nodes(xpath = "//a[@class='personLink']") %>% html_attr(name="href") %>%
    #strsplit(., "/")[[2]]%>%
    #html_text() %>%
    as.character() -> boxerID
    
  read_html(node) %>%
    html_nodes(xpath = "//*[@id='ratingsResults']/tbody//tr/td[2]") %>%
    html_text() %>%
    trimws(., "l") -> boxer
    
  read_html(node) %>%
    html_nodes(xpath = "//*[@id='ratingsResults']/tbody//tr/td[3]") %>%
    html_text() %>%
    as.character() -> points
  
  read_html(node) %>%
    html_nodes(xpath = "//div[@style='float:right;']/div[1]") %>% str_extract(., regex('[A-Z]+'))%>%
    #html_text() %>%
    as.character() -> last1
 
   read_html(node) %>%
    html_nodes(xpath = "//div[@style='float:right;']/div[2]") %>% str_extract(., regex('[A-Z]+'))%>%
    #html_text() %>%
    as.character() -> last2
   
   read_html(node) %>%
     html_nodes(xpath = "//div[@style='float:right;']/div[3]") %>% str_extract(., regex('[A-Z]+'))%>%
     #html_text() %>%
     as.character() -> last3
   
   read_html(node) %>%
     html_nodes(xpath = "//div[@style='float:right;']/div[4]") %>% str_extract(., regex('[A-Z]+'))%>%
     #html_text() %>%
     as.character() -> last4
   
   read_html(node) %>%
     html_nodes(xpath = "//div[@style='float:right;']/div[5]") %>% str_extract(., regex('[A-Z]+'))%>%
     #html_text() %>%
     as.character() -> last5
   
   read_html(node) %>%
     html_nodes(xpath = "//div[@style='float:right;']/div[6]") %>% str_extract(., regex('[A-Z]+'))%>%
     #html_text() %>%
     as.character() -> last6
   
    
  results <- rbind(results, data.frame('boxer'= boxer,'boxerID'= boxerID, 'points'=points, 'last1'=last1, 'last2'=last2, 'last3'=last3, 'last4'=last4, 'last5'=last5, 'last6'=last6))
  
  pb$tick()
}

#cleaning the data
results['boxerID'] <- str_extract(results[,'boxerID'], regex('[0-9]+'))
results <- distinct(results)
#saving data to csv
write.csv(results,"results_general_alltimes.csv" , row.names = FALSE)


###############################################################################
# Merge cleaning and export of final data  
###############################################################################
#alltimes report
results_general_alltimes <- read.csv(file = 'results_general_alltimes.csv')
results_deep_alltimes <- read.csv(file = 'br_total_alltimes.csv')
br_grand_total <- merge(results_deep_alltimes, results ,by="boxerID") %>% distinct()
br_grand_total= subset(br_grand_total,select = -c(X11, X12, X12,X13 ,X14,X15,X16,X17,X18,rating))
br_grand_total= subset(br_grand_total,select = -c(rating))
br_grand_total= rename(br_grand_total, rating=X4)
br_grand_total<- br_grand_total %>%  separate(rating, c("rating_word", "rating_native"), "\n        \n            \n          \n    \n               \n            \n              ", remove = TRUE)
write.csv(br_grand_total,"results_grandtotal_alltimes.csv" , row.names = FALSE, fileEncoding = "UTF-8")
#active report
results_general_active <- read.csv(file = 'results_general_active.csv')
results_deep_active <- read.csv(file = 'br_total_active.csv')
br_grand_total_active <- merge(results_deep_active, results_general_active ,by="boxerID") %>% distinct()
br_grand_total_active= subset(br_grand_total_active,select = -c(X9, X10, X11, X12, X12,X13 ,X14,X15,X16,X17,X18,rating))
br_grand_total_active=rename(br_grand_total_active, rating=X4)
br_grand_total_active<- br_grand_total_active %>%  separate(rating, c("rating_word", "rating_native"), "\n        \n            \n          \n    \n               \n            \n              ", remove = TRUE)

write.csv(br_grand_total_active,"results_grandtotal_active.csv" , row.names = FALSE, fileEncoding = "UTF-8")
library(stringr)
library(tidyr)
# br_grand_total_active<- str_split_fixed(br_grand_total_active$rating, "\n", 2)
br_grand_total_active<- br_grand_total_active %>%  separate(rating, c("rating_word", "rating_native"), "\n        \n            \n          \n    \n               \n            \n              ", remove = TRUE)
n<-13143
br_grand_total_active$rating[n]
br_grand_total_active$rating_word[n]
br_grand_total_active$rating_native[n]
