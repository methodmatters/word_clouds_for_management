
# Define the functions we'll use to make the word clouds
# Define the functions we'll use to make the word clouds
# Define the functions we'll use to make the word clouds
# Define the functions we'll use to make the word clouds
# Define the functions we'll use to make the word clouds


# Function 1: generate a corpus from the text field in original data
# input: data frame + text field
# output: corpus (Quanteda basic object)  
comment_field_to_corpus <- function(data_f, text_field_f){
  corpus_f <- corpus(data_f, text_field = text_field_f)
  # return the corpus object
  return(corpus_f)
}

# Function 2: generate cleaned tokens from the corpus
# input: corpus
# output: cleaned tokens object 
make_clean_tokens <- function(corpus_f){
  # make the base tokens object
  # remove punctuation, numbers, symbols
  # and stopwords (note: default stopword matching is case-insensitive)
  clean_toks_f <- tokens(corpus_f, what = 'word', remove_punct = TRUE, 
                        remove_numbers = TRUE, remove_symbols = TRUE, verbose = TRUE)
  clean_toks_f <- tokens_remove(clean_toks_f, 
                                stopwords("english"))
  # convert all letters to lower case
  clean_toks_f <- tokens_tolower(clean_toks_f)
  # then stem
  clean_toks_f <- tokens_wordstem(clean_toks_f, language = quanteda_options("language_stemmer"))
  # remove words less than three characters
  clean_toks_f <- tokens_select(clean_toks_f, selection = "keep", min_nchar = 3)
  # select bigrams and unigrams
  clean_toks_f <- tokens_ngrams(clean_toks_f, n = 1:2)
  # return the cleaned tokens object
  return(clean_toks_f)
}


# Function 3: generate dfm (document feature matrix) from cleaned tokens 
# input: cleaned tokens object 
# output: document-feature matrix (input for word clouds)
# there are two options for constructing the dfm:
# 1) term frequency (e.g. counts of the words across all documents)
# 2) document frequency (e.g. count of the number of documents 
# containing each word)
make_dfm <- function(tokens_f, dfm_method_f){
  # make a dfm object from the tokens
  dfm_f <- dfm(tokens_f, verbose = TRUE) 
  # two possible methods implemented: term_freq and doc_freq
  # if we choose the "term_freq" method:
  if(dfm_method_f == 'term_freq'){
    # return the DFM object
    return(dfm_f) 
  # if we choose the "doc_freq" method:
  } else if(dfm_method_f == 'doc_freq'){
    # make a boolean (e.g. 0/1) weighting
    # this means that the document
    # frequency to build the wordcloud
    dfm_f <- dfm_weight(dfm_f, scheme = "boolean")
    # return the DFM object
    return(dfm_f) 
  } else{
    stop("invalid method specified for creating the dfm. options are 'term_freq' and 'doc_freq' ")
  }
} 


# Function 4: Remove Words 
# this functions removes words that we do not
# want to see in our wordclouds
# it removes the words from the tokens object
# before it is turned into a dfm
remove_words <- function(tokens_f, words_to_remove_f){
  # use "tokens select" to remove the selected words 
  # from the tokens object
  trimmed_tokens_f <- tokens_select(tokens_f, 
                                    words_to_remove_f , 
                                    selection = "remove", 
                                    case_insensitive = TRUE)
  # return the tokens object, with the chosen words removed
  return(trimmed_tokens_f)
}


# Function 5: Clean Stemmed Words 
# this function "un-stems" the stemmed words
# we pass a vector of the words-to-replace (e.g. "busi")
# and the replacements (e.g. "business")
# this function operates on the dfm object
clean_stemmed_words <- function(dfm_f, old_words_f, new_words_f){
  # huge help from:
  # https://stackoverflow.com/questions/19424709/r-gsub-pattern-vector-and-replacement-vector
  # create a combination of renamed and original feature names
  names(new_words_f) <- old_words_f
  # assign the replacements to the dfm
  colnames(dfm_f) <- str_replace_all(colnames(dfm_f), new_words_f)
  # compress the dfm
  dfm_f <- dfm_compress(dfm_f)
  # and return the cleaned dfm object
  return(dfm_f)
}



# Function 6: master cleaning function
# this puts together all of the pieces we have defined above
# the input is the dataframe + text column
# (along with optional paramaters, e.g. words to remove)
# the output is a DFM object we can pass directly to the 
# Quanteda wordcloud function
master_cleaning_function <- function(data_f, 
                                            text_field_f,
                                            dfm_method_f,
                                            old_words_f, 
                                            new_words_f, 
                                            words_to_remove_f){
  # create the corpus object using function defined above
  corpus_master_f <- comment_field_to_corpus(data_f, text_field_f)
  # create the cleaned tokens object using function defined above
  clean_toks_master_f <- make_clean_tokens(corpus_master_f)
  # if we don't specify that we want to remove any words,
  # we create the dfm directly from the tokens object
  if(missing(words_to_remove_f)){
    dfm_f <- make_dfm(clean_toks_master_f, dfm_method_f)
  } else {
    # if we want to remove words, we remove them from the 
    # tokens object using the function defined above
    clean_toks_master_f <- remove_words(clean_toks_master_f, words_to_remove_f)
    # and then create the dfm using the function defined above
    dfm_f <- make_dfm(clean_toks_master_f, dfm_method_f) 
  }
  # if we don't specify words-to-be-replaced and their replacements
  # we return the dfm directly
  if(missing(old_words_f) & missing(new_words_f)) {
    return(dfm_f)
  } else {
    # if we specify words-to-be-replaced and replacements
    # we make the changes to the dfm using the function
    # defined above
    dfm_f <- clean_stemmed_words(dfm_f, old_words_f, new_words_f)
    # and then return the modified dfm
    return(dfm_f)
  }
}


# Make the word clouds
# Make the word clouds
# Make the word clouds
# Make the word clouds
# Make the word clouds

# load the libraries we'll need
library(quanteda) 
library(plyr); library(dplyr)
library(stringr)
library(RColorBrewer)

# define the input directory
in_dir = 'C:\\My_Directory\\Data\\'
# read in the raw data
raw_data <- read.csv(paste0(in_dir, 'wine_data.csv'), stringsAsFactors = FALSE)

# example winemaker's notes
raw_data$Winemakers_Notes[1]

# color parameters for plots
pal=brewer.pal(8,"Blues")
pal=pal[-(1:3)]
colors=brewer.pal(8, "Dark2")


# remove numbers looks to be broken
# indeed - should be fixed soon
# https://github.com/quanteda/quanteda/issues/1909


tf_dfm <- master_cleaning_function(data_f = raw_data, 
                                     text_field_f = "Winemakers_Notes", 
                                     dfm_method_f = 'term_freq')


# plot the out-of-the-box word cloud
quanteda::textplot_wordcloud(tf_dfm, max_words = 25, col = colors,
                             min_size = .5, max_size = 7, rotation = 0)

# specify modifications to the words
old_words <- c('palat', 'intens', 'miner', 'cherri', 'balanc')
new_words <- c('palate', 'intense', 'mineral', 'cherry', 'balance')
to_remove <- c('wine')

# make the cleaned dfm 
tf_dfm_clean <- master_cleaning_function(data_f = raw_data, 
                                        text_field_f = "Winemakers_Notes",
                                        dfm_method_f = 'term_freq', 
                                        old_words_f = old_words,
                                        new_words_f = new_words,
                                        words_to_remove_f = to_remove)


# make the word cloud with the changes specified above
quanteda::textplot_wordcloud(tf_dfm_clean, max_words = 25, col = colors,
                             min_size = .5, max_size = 7, rotation = 0)


### Example with document frequency (not on blog)
### Example with document frequency (not on blog)
### Example with document frequency (not on blog)
### Example with document frequency (not on blog)
### Example with document frequency (not on blog)


df_dfm <- master_cleaning_function(data_f = raw_data,
                                   text_field_f = "Winemakers_Notes",
                                   dfm_method_f = 'doc_freq')


quanteda::textplot_wordcloud(df_dfm, max_words = 25, col = colors,
                             min_size = .5, max_size = 7, rotation = 0)


