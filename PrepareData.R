

training_data_filename <- "~/Desktop/hobby dm/SpookyAuthors/data/train.csv"

#Load the training data
training_data <- read.csv(file=training_data_filename, stringsAsFactors=FALSE)

#Clean up data:
#  change case
#  remove punctuation
#  tokenise
#  stem
#  remove stop words  (before or after stemming?)
#  calculate token frequencies

library("tm")
# Clean a single "document"
# example below contains 3 x process
document <- "process. This process, however, afforded me no means of ascertaining process the dimensions of my dungeon; as I might make its circuit, and return to the point whence I set out, without being aware of the fact; so perfectly uniform seemed the wall."

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


