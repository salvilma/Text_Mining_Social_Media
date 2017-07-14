library(dplyr)
#Carácteres raros ã que puede ser una ñ, á, í ..etc
library(stringi)

GenerateVocabulary <- function(path, n = 1000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, 
                               whitespaces = TRUE, swlang = "", swlist = "", verbose = TRUE) {
  setwd(path)
  
  files = list.files(pattern="*.xml")
  corpus.raw <- NULL
  i <- 0
  
  for (file in files) {
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
    i <- i + 1
    if (verbose) print(paste(i, " ", file))	
  }
  #Caracteres especiales
  corpus.raw <- stri_trans_general(corpus.raw, "Latin-ASCII")
  
  corpus.preprocessed <- corpus.raw
  
  # Estaría bien quitar los tildes NOTA KIKO
  corpus.preprocessed <- chartr("áéíóú","aeiou",corpus.preprocessed)
  
  if (swlist!="") {
    if (verbose) print("Removing provided stopwords...")
    corpus.preprocessed <- removeWords(corpus.preprocessed, swlist)
  }
  
  if (lowcase) {
    if (verbose) print("Tolower...")
    corpus.preprocessed <- tolower(corpus.preprocessed)
  }	
  
  if (punctuations) {
    if (verbose) print("Removing punctuations...")		
    corpus.preprocessed <- removePunctuation(corpus.preprocessed)
  }
  
  if (numbers) {
    if (verbose) print("Removing numbers...")
    corpus.preprocessed <- removeNumbers(corpus.preprocessed)
  }
  
  if (whitespaces) {
    if (verbose) print("Stripping whitestpaces...")
    corpus.preprocessed <- stripWhitespace(corpus.preprocessed)
  }
  
  if (swlang!="")	{
    if (verbose) print(paste("Removing stopwords for language ", swlang , "..."))
    corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang))
  }
  
  if (verbose) print("Generating frequency terms")
  
  corpus.frequentterms <- freq_terms(corpus.preprocessed, n)
  # Comento el plot	por coste en tiempo. 
  #	if (verbose) plot(corpus.frequentterms)
  
  return (corpus.frequentterms)
}

GenerateBoW <- function(path, vocabulary, n = 1000, lowcase = TRUE, punctuations = TRUE, numbers = TRUE, 
                        whitespaces = TRUE, swlang = "", swlist = "", class="variety", verbose = TRUE) {
  setwd(path)
  
  truth <- read.csv("truth.txt", sep=":", header=FALSE)
  truth <- truth[,c(1,4,7)]
  colnames(truth) <- c("author", "gender", "variety")
  
  i <- 0
  bow <- NULL
  files = list.files(pattern="*.xml")
  for (file in files) {
    author <- gsub(".xml", "", file)
    variety <- truth[truth$author==author,"variety"]
    gender <- truth[truth$author==author,"gender"]
    
    xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
    txtdata <- xpathApply(xmlfile, "//document", function(x) xmlValue(x))
    
    #Hacemos el mismo tratamiento que en el vocabulario. ATENCIÓN!! si añado alguna función de procesado
    #en el vocabulario añadirlo aquí tb.
    
    txtdata <- stri_trans_general(txtdata, "Latin-ASCII")   #Nuevo MSS. Carácteres raros â, ã ..
    txtdata <- chartr("áéíóú","aeiou",txtdata) #Quitar acentos. 
    
    if (lowcase) {
      txtdata <- tolower(txtdata)
    }
    
    if (punctuations) {
      txtdata <- removePunctuation(txtdata)
    }
    
    if (numbers) {
      txtdata <- removeNumbers(txtdata)
    }
    
    if (whitespaces) {
      txtdata <- stripWhitespace(txtdata)
    }
    
    #Si hemos quitado tildes aqui tambien tendriamos que quitarlos
    
    line <- author
    freq <- freq_terms(txtdata, n)
    for (word in vocabulary$WORD) {
      thefreq <- 0
      if (length(freq[freq$WORD==word,"FREQ"])>0) {
        thefreq <- freq[freq$WORD==word,"FREQ"]
      } 
      line <- paste(line, ",", thefreq, sep="")
    }
    
    if (class=="variety") {  # Cambiamos el orden para que la clase esté en la primera columna
      #line <- paste(line, ",", variety,  sep="")
      line <- paste(variety, ",", line,  sep="")
    } else {
      #line <- paste(line, ",", gender, sep="")
      line <- paste(gender, ",", line,  sep="")
    }
    
    #Añadir la media de tuits .......................
   # line <- paste(line, ",", media_tuits,  sep="")
    
    bow <- rbind(bow, line)
    i <- i + 1
    
    if (verbose) {
      if (class=="variety") {
        print(paste(i, author, variety))
      } else {
        print(paste(i, author, gender))
      }
    }
  }
  
  return (bow)
}


# Por genero: (female, male) ######################################################################
GenerateVocabularyGenero <- function(path, n = 1000, lowcase = TRUE, punctuations = TRUE, 
                                     numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", 
                                     verbose = TRUE, genero) {
  setwd(path)
  
  files = list.files(pattern="*.xml")
  #MSS
  truth <- read.csv("truth.txt", sep=":", header=FALSE)
  truth <- truth[,c(1,4,7)]
  colnames(truth) <- c("author", "gender", "variety")  
  # 
  corpus.raw <- NULL
  i <- 0
  for (file in files) {
    #filtrar por las características MSS
    author <- gsub(".xml", "", file) #Nos quedamos con lo que hay delante de .xml y es el identificador del autor. 
    gender <- truth[truth$author==author,"gender"]
    #MSS
    print(gender)
    print(genero)
    if (gender == genero){
      xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
      #xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE, encoding = "UTF-8")
      corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
      i <- i + 1
      if (verbose) print(paste(i, " ", file))	
    }
  }
  #caracteres especiales
  corpus.raw <- stri_trans_general(corpus.raw, "Latin-ASCII")
  
  corpus.preprocessed <- corpus.raw
  
  # Estaría bien quitar los acentos.... NOTA KIKO
  corpus.preprocessed <- chartr("áéíóú","aeiou",corpus.preprocessed)
  
  # Tb estaría bien quedarte con la raíz de las palabras.. unas veces con el inicio y otras con el final. 
  if (lowcase) {
    if (verbose) print("Tolower...")
    corpus.preprocessed <- tolower(corpus.preprocessed)
  }	
  
  if (punctuations) {
    if (verbose) print("Removing punctuations...")		
    corpus.preprocessed <- removePunctuation(corpus.preprocessed)
  }
  
  if (numbers) {
    if (verbose) print("Removing numbers...")
    corpus.preprocessed <- removeNumbers(corpus.preprocessed)
  }
  # removeNumbers tiene opciones para transformar los números a letras
  
  if (whitespaces) {
    if (verbose) print("Stripping whitestpaces...")
    corpus.preprocessed <- stripWhitespace(corpus.preprocessed)
  }
  
  if (swlang!="")	{
    if (verbose) print(paste("Removing stopwords for language ", swlang , "..."))
    corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang))
  }
  
  # Quitar los stopwords  
  if (swlist!="") {
    if (verbose) print("Removing provided stopwords...")
    corpus.preprocessed <- removeWords(corpus.preprocessed, swlist)
  }
  
  if (verbose) print("Generating frequency terms")
  
  corpus.frequentterms <- freq_terms(corpus.preprocessed, n)
  #Construimos la bolsa de palabras corpus.frequentterms
  
  # if (verbose) plot(corpus.frequentterms)
  return (corpus.frequentterms)
}


# Por variedad: Pais (colombia, argentina, spain, venezuela, peru, chile, mexico)  ##############
GenerateVocabularyVariedad <- function(path, n = 1000, lowcase = TRUE, punctuations = TRUE, 
                                      numbers = TRUE, whitespaces = TRUE, swlang = "", swlist = "", 
                                      verbose = TRUE, variedad) {
  setwd(path)
  
  files = list.files(pattern="*.xml")
  #MSS
  truth <- read.csv("truth.txt", sep=":", header=FALSE)
  truth <- truth[,c(1,4,7)]
  colnames(truth) <- c("author", "gender", "variety")  
  # 
  corpus.raw <- NULL
  i <- 0
  for (file in files) {

    author <- gsub(".xml", "", file) #Nos quedamos con lo que hay delante de .xml y es el identificador del autor. 
    variety <- truth[truth$author==author,"variety"]
    if (variety == variedad){
      xmlfile <- xmlTreeParse(file, useInternalNodes = TRUE)
      corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
      i <- i + 1
      if (verbose) print(paste(i, " ", file))	
    }
  }
  #caracteres especiales
  corpus.raw <- stri_trans_general(corpus.raw, "Latin-ASCII")
  
  corpus.preprocessed <- corpus.raw
  
  # Estaría bien quitar los acentos.... NOTA KIKO
  corpus.preprocessed <- chartr("áéíóú","aeiou",corpus.preprocessed)

  if (lowcase) {
    if (verbose) print("Tolower...")
    corpus.preprocessed <- tolower(corpus.preprocessed)
  }	
  
  if (punctuations) {
    if (verbose) print("Removing punctuations...")		
    corpus.preprocessed <- removePunctuation(corpus.preprocessed)
  }
  
  if (numbers) {
    if (verbose) print("Removing numbers...")
    corpus.preprocessed <- removeNumbers(corpus.preprocessed)
  }
  # removeNumbers tiene opciones para transformar los números a letras
  
  if (whitespaces) {
    if (verbose) print("Stripping whitestpaces...")
    corpus.preprocessed <- stripWhitespace(corpus.preprocessed)
  }
  
  if (swlang!="")	{
    if (verbose) print(paste("Removing stopwords for language ", swlang , "..."))
    corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang))
  }
  
  # Quitar los stopwords  
  if (swlist!="") {
    if (verbose) print("Removing provided stopwords...")
    corpus.preprocessed <- removeWords(corpus.preprocessed, swlist)
  }
  
  if (verbose) print("Generating frequency terms")
  
  corpus.frequentterms <- freq_terms(corpus.preprocessed, n)

  return (corpus.frequentterms)
}


add.unique<-function(ls1,ls2) {
  ifelse (ls2 %in% ls1, return(ls1) , return(c(ls1,ls2)) )
}


# load("vocabulary_female.RData")
# load("vocabulary_male.RData")
# load("vocabulary_union.RData")
# load("vocabulary_female_male_total.RData")
# # ------------
# load("bow_train_union")
# load("bow_train_male.RData")
# load("bow_train_female_male_total.RData")
# load("bow_train_female.RData")
# load("bow_test_union.RData")
# load("bow_test_male.RData")
# load("bow_test_female.RData")
# load("bow_test_female_male_total.RData")
# 
# # ------------
# load("vocabulary_colombia.Rdata")
# load("vocabulary_mexico.RData")
# load("vocabulary_chile.RData")
# load("vocabulary_peru.RData")
# load("vocabulary_venezuela.RData")
# load("vocabulary_argentina.RData")
# load("vocabulary_spain.RData")