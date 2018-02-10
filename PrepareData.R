library("tm")
library("plyr")

# CountSymbol ----
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

# CountTokens ----
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

# GetSampleIDs ----
#' Return a (stratified) array of identifiers from a dataframe (useful in identifying a test set)
#'
#' @param all_data dataframe with min 2 columns: <stratisfy_by_column_name>,<sample_id_column_name>
#' @param stratisfy_by_column_name name of column in all_data which will be used to stratisfy the selection
#' @param sample_id_column_name name of the column in all_data which uniquely identifies each row in all_data
#' @param sample_size_percent size of the sample (%)
#'
#' @return array of id's from <sample_id_column_name>
#' @export
#'
#' @examples
GetSampleIDs <- function (all_data,
                          stratisfy_by_column_name="author",
                          sample_id_column_name="id",
                          sample_size_percent=15) {
  #Create index of document ids (stratified by author) for "test data"
  
  unique_values <- unique(all_data[,stratisfy_by_column_name])
  all_sample_ids <- sapply(X=unique_values,
                           FUN=function(x)
                           {candidate_rows <- all_data[,stratisfy_by_column_name]==x
                           num_rows <- round(sum(candidate_rows) * sample_size_percent / 100)
                           sample_ids <- sample(x=all_data[candidate_rows,sample_id_column_name],
                                                size=num_rows,
                                                replace=FALSE)
                           return(sample_ids)}
  )
  all_sample_ids <- unlist(all_sample_ids, use.names=FALSE)
  
  return(all_sample_ids)
}

#### PrepareWorkingEnvironment ----
#' Creates Rdata file with to variables: corpus and all_tokens.
#'
#' @param corpus_filename input file - CSV file with: id, text, author
#' @param train_test_filename  Rdata output filename - will contain 2 variables
#' @param verbose if TRUE will cause the function to output diagnostic messages
#'
#' @return
#' @export
#'
#' @examples
PrepareWorkingEnvironment <- function(corpus_filename="./data/train.csv",
                                      train_test_filename="./data/training_data.Rdata",
                                      verbose=TRUE) {
  #Load the training data
  corpus <- read.csv(file=corpus_filename, stringsAsFactors=FALSE)
  
  if(verbose){cat("Counting tokens\n")}
  all_tokens <- apply(X=corpus[,],
                      MARGIN=1,
                      FUN=function(x) 
                            {return(CountTokens(document=x["text"],
                                                document_id=x["id"],
                                                count_quotes=TRUE))}
                      )
  #all_tokens is now a list of dataframes
  if(verbose){cat("Reformatting token counts.\n")}
  all_tokens <- plyr::rbind.fill(all_tokens)
  
  #Get of each author for testing purposes
  if(verbose){cat("Creating stratified sample (by author) from corpus\n")}
  test_sample_ids = GetSampleIDs(all_data = corpus,
                                 stratisfy_by_column_name = "author",
                                 sample_id_column_name = "id",
                                 sample_size_percent = 10)
  #Update corpus to indictae which documents are training, which are test
  index_of_samples = corpus$id %in% test_sample_ids
  corpus$type="TRAIN"
  corpus$type[index_of_samples]="TEST"
  rm(list=c("index_of_samples","test_sample_ids"))
  
  #Split the token data into 2 different sets:
  all_tokens <- merge(x=all_tokens,
                 y=corpus[,c("id","type")],
                 by.x="document_id",
                 by.y="id")
  
  #Save data so it does not need to be re-created again:
  if(verbose){cat("Creating compressed data file with corpus and token details.\nSaving :",train_test_filename,"...\n")}
  save(all_tokens,
       corpus,
       file=train_test_filename,
       compress=TRUE)
  if(verbose){cat("Saved: ",train_test_filename,"\n")}
}


LoadWorkingEnvironment <- function(corpus_filename="./data/train.csv",
                                    train_test_filename="./data/training_data.Rdata",
                                    verbose=TRUE) {
  if(!file.exists(train_test_filename)) {
    PrepareWorkingEnvironment(corpus_filename=corpus_filename,
                              train_test_filename=train_test_filename,
                              verbose=verbose)
  }
  #File should exist now.  Open it (in global environment)
  load(file=train_test_filename,
       envir = .GlobalEnv)
}