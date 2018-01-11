

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


# Clean a single "document"
document <- "This process, however, afforded me no means of ascertaining the dimensions of my dungeon; as I might make its circuit, and return to the point whence I set out, without being aware of the fact; so perfectly uniform seemed the wall."

document <- tolower(document)
