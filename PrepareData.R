
library("tm")
training_data_filename <- "./data/train.csv"

#Load the training data
training_data <- read.csv(file=training_data_filename, stringsAsFactors=FALSE)


#' Tokenises and counts tokens in a document.
#' 
#' Assumes English.  Uses Porter Stemming (as implemented in tm)
#'
#' @param document string
#'
#' @return a dataframe with tokens and counts
#' @export
#'
#' @examples
CountTokens <- function(document) {

  #Clean up data:
#  change case
#  remove punctuation
#  tokenise
#  remove stop words
#  stem
#  calculate token frequencies


# Clean a single "document"
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

return(data)
}


#Build Term Frequency Matrix.  Each row represents a document
line1 = training_data$text[1]
line1_tokens = CountTokens(line1)

line2 <- training_data$text[2]
line2_tokens <- CountTokens(line2)

#Alernative is to use a long list (i.e. a Tidy solution)
#ID; token; count
#Then reform later into matrix.
#Also - some pontential useful features in the punctuation : !;?;";sentance_length