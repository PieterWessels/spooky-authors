
library("tm")
training_data_filename <- "./data/train.csv"

#Load the training data
training_data <- read.csv(file=training_data_filename, stringsAsFactors=FALSE)

#' Count occurances of a symbol (single character) in a document
#'
#' @param document 
#' @param symbol Single character to be counted in document
#'
#' @returnnumber of times symbol occurs in document
#' @export
#'
#' @examples
CountSymbol <- function(document, symbol){
  symbol <- paste0("\\",symbol)
  found_symbols <- gregexpr(pattern=symbol, text=document)
  if(found_symbols[[1]][1]==-1) {
    return_value <- 0
  } else {
    return_value <- length(found_symbols[[1]])
  }
  return(return_value)
}

#' Tokenises and counts tokens in a document.
#' 
#' Assumes English.  Uses Porter Stemming (as implemented in tm)
#'
#' @param document string
#' @param count_quotes if TRUE will append [QUOTE] as token with frequency
#' @param count_exclamations if TRUE will append [EXCLAMATION] as token with frequency
#' @param count_hyphens if TRUE will append [HYPHEN] as token with frequency
#' @param count_questionmarks if TRUE will append [QUESTION] as token with frequency
#' @param document_id if not NULL will add document_id as column to resultant dataframe
#'
#' @return a dataframe: token, count
#' @export
#'
#' @examples
CountTokens <- function(document,
                        count_quotes=FALSE,
                        count_exclamations=TRUE,
                        count_hyphens=FALSE,
                        count_questionmarks=TRUE,
                        document_id = NULL) {

#Clean up data:
#  change case
#  remove punctuation
#  tokenise
#  remove stop words
#  stem
#  calculate token frequencies


symbols <- c()
counts <- c()
# Clean a single "document"
if(count_quotes){
  num_quotes <- CountSymbol(document,'"')
  symbols <- c(symbols, "[QUOTE]")
  counts <- c(counts, num_quotes)
}
if(count_exclamations){
  num_exclmations <- CountSymbol(document,'!')
  symbols <- c(symbols, "[EXCLAMATION]")
  counts <- c(counts, num_exclmations)
}
if(count_hyphens){
  num_hyphens <- CountSymbol(document,'-')
  symbols <- c(symbols, "[HYPHEN]")
  counts <- c(counts, num_hyphens)
}
if(count_questionmarks){
  num_questionmarks <- CountSymbol(document,'?')
  symbols <- c(symbols, "[QUESTION]")
  counts <- c(counts, num_questionmarks)
}

document <- tolower(document)
document <- gsub(pattern="[[:punct:]]",
                 x=document,
                 replacement="")
document <- gsub(pattern="[[:digit:]]",
                 x=document,
                 replacement="")
document <- gsub(pattern="[[:space:]]+",
                 x=document,
                 replacement=" ")

#Convert to vector
data <- unlist(strsplit(x=document, split=" "))

#Remove Stopwords
#Apparently tm contains a list of industry standard english stopwords
stopword_index <- which(data %in% tm::stopwords(kind="en"))
data <- data[-stopword_index]

#Stem
#For ease of use, use the Porter stemming algorithm in tm
data <- stemDocument(x=data, language="english")
data <- as.data.frame(table(data),
                      stringsAsFactors = FALSE)
if(length(symbols)>0){
   symbol_data=data.frame(data=symbols, Freq=counts, stringsAsFactors = FALSE)
   data=rbind(data,symbol_data)
}
names(data) = c("token","count")
if(!is.null(document_id)) {
  data$document_id = document_id
}
return(data)
}


#Build Term Frequency Matrix.  Each row represents a document
line1_tokens <- CountTokens(document=training_data$text[1],
                            document_id=training_data$id[1])


#Alernative is to use a long list (i.e. a Tidy solution)
#ID; token; count
#Then reform later into matrix.
#Also - some pontential useful features in the punctuation : !;?;";sentance_length
