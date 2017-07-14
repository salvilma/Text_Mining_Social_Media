# Comentar o descomentar más de una línea.. las seleccionamos Ctrl+Shift+C

install.packages("qdap")
install.packages("splitstackshape")
install.packages("kernlab")
install.packages("randomForest")

library(qdap)
library(XML)
library(tm)
library(splitstackshape)
library(caret)
library(kernlab)
library(dplyr)
#Carácteres raros ã que puede ser una ñ, á, í ..etc
library(stringi)
library(randomForest)

path_apli <- "D:/MASTER_Ejercicios/@TextMining"
setwd(path_apli)
source("FuncTextMining.R")  

path_training <-"D:/MASTER_Ejercicios/TextMining/pan-ap17-bigdata/training"
path_test <- "D:/MASTER_Ejercicios/TextMining/pan-ap17-bigdata/test"

n <- 1000 #Número de palabras del diccionario o bolsa de palabras
#n <- lengths(vocab_total.df)  #Pondremos el vocabulario elegido. 


#Lista de palabras a quitar. Alternativa. 
lswlist <- c("mas","si","mejor","daa","hace","hacer","semana","ir","sa","asa", "aaos","aao","usar", "mesa", "escribir")


####################################################################################################
##                             S E X O  (female, male)                                            ##
####################################################################################################
path_datos <- "D:/MASTER_Ejercicios/@TextMining/Datos"
setwd(path_datos)

load("vocabulary_female.RData")
load("vocabulary_male.RData")
load("vocabulary_union.RData")
load("vocabulary_female_male_total.RData")
load("vocab_puntua.RData")

# ------------
load("bow_train_union.RData")
load("bow_train_male.RData")
load("bow_train_female_male_total.RData")
load("bow_train_female.RData")
load("bow_test_union.RData")
load("bow_test_male.RData")
load("bow_test_female.RData")
load("bow_test_female_male_total.RData")
load("bow_test_puntua.RData")
load("bow_train_puntua.RData")

# ---------------------------------------------------------------------- #
# Estrategia 1. Diccionario con todos los tuits
# ---------------------------------------------------------------------- #
#Generara el vocabulario (palabra,n? apariciones) y explorar el dataset
vocabulary <- GenerateVocabulary(path_training, n, swlang="es") 
save(vocabulary,file="vocabulary_todos_tuits.RData")
vocabulary.df <- as.data.frame(vocabulary)  #Lo paso a data frame xq si no deja las dos columnas en una. 
write.csv(vocabulary.df, file = "vocabulary_todos_tuits.csv")
# ----
bow_train_genero <- GenerateBoW(path_training, vocabulary, n, class="gender")
save(bow_train_genero,file="bow_train_genero.RData")
# --
bow_test_genero <- GenerateBoW(path_test, vocabulary, n, class="gender")
save(bow_test_genero,file="bow_test_genero.RData")
# ----
training <- concat.split(bow_train_genero, "V1", ",")
test <- concat.split(bow_test_genero, "V1", ",")

# ---------------------------------------------------------------------- #
# Estrategia 2. Diccionario con palabras más frecuentes en female
# ---------------------------------------------------------------------- #
vocabulary.female <- GenerateVocabularyGenero(path_training, n, swlang="es",genero="female") 
save(vocabulary.female,file="vocabulary_female.RData")
#Transformo de lista a data frame porque da problemas el tratamiento de la lista. Me devolvía factores
vocabulary.female.df <- as.data.frame(vocabulary.female)
write.csv(vocabulary.female.df, file = "vocabulary_female.csv")
# ----
bow_train_female <- GenerateBoW(path_training, vocabulary.female, n, class="gender")
typeof(bow_train_female)
save(bow_train_female,file="bow_train_female.RData")
# --
bow_test_female <- GenerateBoW(path_test, vocabulary.female, n, class="gender")
head(bow_test_female)
save(bow_test_female,file="bow_test_female.RData")
# ----
training <- concat.split(bow_train_female, "V1", ",")
test <- concat.split(bow_test_female, "V1", ",")


# ---------------------------------------------------------------------- #
# Estrategia 3. Diccionario con palabras más frecuentes en male
# ---------------------------------------------------------------------- #
vocabulary.male <- GenerateVocabularyGenero(path_training, n, swlang="es",genero="male") 
save(vocabulary.male,file="vocabulary_male.RData")
vocabulary.male.df <- as.data.frame(vocabulary.male)
write.csv(vocabulary.male.df, file = "vocabulary_male.csv")
# ----
bow_train_male <- GenerateBoW(path_training, vocabulary.male, n, class="gender")
save(bow_train_male,file="bow_train_male.RData")
# --
bow_test_male <- GenerateBoW(path_test, vocabulary.male, n, class="gender")
save(bow_test_male,file="bow_test_male.RData")
# ----
training <- concat.split(bow_train_male, "V1", ",")
test <- concat.split(bow_test_male, "V1", ",")

# ---------------------------------------------------------------------- #
# Estrategia 4. Diccionario con la unión de las palabras exclusivas
# de cada sexo
# ---------------------------------------------------------------------- #
vocab_male   = vocabulary.male[1]
vocab_female = vocabulary.female[1]
# -----------
vocab_solo_male   <- setdiff(vocab_male, vocab_female)
vocab_solo_male.df =as.data.frame(vocab_solo_male)

vocab_solo_female <- setdiff(vocab_female, vocab_male)
vocab_solo_female.df =as.data.frame(vocab_solo_female)
#
# Unimos los vocabularios exclusivos en vocab_total
vocab.union <- rbind(vocab_solo_male.df, vocab_solo_female.df)
typeof(vocab.union)
write.csv(vocab.union, file = "vocabulary_union.csv")
save(vocab.union,file="vocabulary_union.RData")
# ----
bow_train_union <- GenerateBoW(path_training, vocab.union, n, class="gender")
save(bow_train_union,file="bow_train_union.RData")
# --
bow_test_union <- GenerateBoW(path_test, vocab.union, n, class="gender")
save(bow_test_union,file="bow_test_union.RData")
# ----
training <- concat.split(bow_train_union, "V1", ",")
test <- concat.split(bow_test_union, "V1", ",")

# ---------------------------------------------------------------------- #
# Estrategia 5. Diccionario con la unión de las palabras exclusivas
# de cada sexo y la intersección de las palabras frecuentes por sexo. 
# ---------------------------------------------------------------------- #
vocab_comun_genero = intersect(vocab_male,vocab_female)
write.csv(vocab_comun_genero, file = "vocab_comun_genero.csv")
save(vocab_comun_genero,file="vocab_comun_genero.RData")

# En vocab_total tenemos la union de las palabras exclusivas de hombres y mujeres
vocab_total <- rbind(vocab_comun_genero, vocab_union)
write.csv(vocab_total, file = "vocabulary_female_male_total.csv")
save(vocab_total,file="vocabulary_female_male_total.RData")
# ----
bow_train_female_male_total <- GenerateBoW(path_training, vocab_total, n, class="gender")
save(bow_train_female_male_total,file="bow_train_female_male_total.RData")
# --
bow_test_female_male_total <- GenerateBoW(path_test, vocab_total, n, class="gender")
save(bow_test_female_male_total,file="bow_test_female_male_total.RData")
# ----
training <- concat.split(bow_train_female_male_total, "V1", ",")
test <- concat.split(bow_test_female_male_total, "V1", ",")

# ---------------------------------------------------------------------- #
# Estrategia 6. Diccionario con puntuación  
# ---------------------------------------------------------------------- #
#Generamos este diccionario para ver los signos de puntuacion pero no vemos nada destacable
vocab.punctuations <- GenerateVocabulary(path_training, n, punctuations = FALSE)
write.csv(vocab_total, file = "vocab_punctuations.csv")
save(vocab_total,file="vocab_punctuations.RData")
#-----------------------
vocab_puntua = vocab.union$WORD
ls1 = c("el", "la", "este","poco","grande","excelente","soy","somos","voy","hago",
        "lindo","dios","pueblo","seguidores","trabajar","facebook","hijo",
        "no","pero","ni","mal","mierda", "not", "para", "con", "!!!", "!!", ":)",":(")

vocab_puntua <- add.unique(vocab_puntua,ls1)

vocab_puntua <- as.data.frame(vocab_puntua)
colnames(vocab_puntua)[1] = "WORD"
# ----
bow_train_puntua <- GenerateBoW(path_training, vocab_puntua, n, class="gender")
save(bow_train_puntua,file="bow_train_puntua.RData")
# --
bow_test_puntua <- GenerateBoW(path_test, vocab_puntua, n, class="gender")
save(bow_test_puntua,file="bow_test_puntua.RData")
# ----
# ----
training <- concat.split(bow_train_puntua, "V1", ",")
test <- concat.split(bow_test_puntua, "V1", ",")

# ----------------------------------------------------- #
#                   Código común 
# ----------------------------------------------------- #
traindf <- as.data.frame(training)
typeof(traindf)
traindf <- traindf[,c(-1,-3)]
traindf <- as.data.frame(training)
names(traindf)[1]<-"class"

testdf <- as.data.frame(test)
typeof(testdf)
testdf <- testdf[,c(-1,-2,-3)]

#truth será un vector con los valores de la clase correcta. 
truth <- as.data.frame(test)
truth <- truth[2]
truth

# ----------------------------------------------------- #
#              Con validación cruzada
# ----------------------------------------------------- #
train_control <- trainControl( method="repeatedcv", number = 10 , repeats = 3) #a 10 capas
#model_SVM <- train( class~., data= training, trControl = train_control, method = "svmLinear")
model_SVM <- train( class~., data= traindf, trControl = train_control, method = "svmLinear")
#print(model_SVM)

# ----------------------------------------------------- #
#              sin validación cruzada
# ----------------------------------------------------- #
#Aprendiendo con todo el training
train_control <- trainControl(method="none")
train_control
#model_SVM <- train( class~., data= training, trControl = train_control, method = "svmLinear")
#Support Vector Machines with Linear Kernel (method = 'svmLinear')
model_SVM <- train( class~., data= traindf, trControl = train_control, method = "svmLinear")
# ----------------------------------------------------- #


# ----------------------------------------------------- #
#              Random Forest
# ----------------------------------------------------- #
#Aprendiendo con todo el training
train_control <- trainControl(method="none")
model_SVM <- train( class~., data= traindf, trControl = train_control, ntree = 150, method = "rf")
# ----------------------------------------------------- #

#Evaluando con el test. 
pred_SVM <- predict(model_SVM, testdf)
typeof(pred_SVM)
outputVectorpred_SVM = as.character(pred_SVM)

truthvector <-unlist(truth)
truthvector = as.character(truthvector)
#lista de hombre mujer mujer hombre
confusionMatrix(outputVectorpred_SVM, truthvector)
# compara la lista anterior con los valores reales.. 


####################################################################################################
##   V A R I E D A D / P A I S (colombia, argentina, spain, venezuela, peru, chile, mexico)       ##
####################################################################################################

# Variedades de país: 
vocabulary.colombia <- GenerateVocabularyVariedad(path_training, n, swlang="es",variedad="colombia") 
vocabulary.argentina <- GenerateVocabularyVariedad(path_training, n, swlang="es",variedad="argentina") 
vocabulary.spain <- GenerateVocabularyVariedad(path_training, n, swlang="es",variedad="spain") 
vocabulary.venezuela <- GenerateVocabularyVariedad(path_training, n, swlang="es",variedad="venezuela") 
vocabulary.peru <- GenerateVocabularyVariedad(path_training, n, swlang="es",variedad="peru") 
vocabulary.chile <- GenerateVocabularyVariedad(path_training, n, swlang="es",variedad="chile") 
vocabulary.mexico <- GenerateVocabularyVariedad(path_training, n, swlang="es",variedad="mexico") 

# Diccionarios exclusivos de cada pais
vocab_solo_colombia <- Reduce(setdiff,
                              list( A = vocabulary.colombia[,1], 
                                    B = vocabulary.argentina[,1],
                                    C = vocabulary.spain[,1],
                                    D = vocabulary.venezuela[,1],
                                    E = vocabulary.peru[,1],
                                    F = vocabulary.chile[,1],
                                    G = vocabulary.mexico[,1]))
#159
vocab_solo_argentina <- Reduce(setdiff,
                              list( A = vocabulary.argentina[,1], 
                                    B = vocabulary.colombia[,1],
                                    C = vocabulary.spain[,1],
                                    D = vocabulary.venezuela[,1],
                                    E = vocabulary.peru[,1],
                                    F = vocabulary.chile[,1],
                                    G = vocabulary.mexico[,1]))
#213
vocab_solo_spain <- Reduce(setdiff,
                               list( A = vocabulary.spain[,1], 
                                     B = vocabulary.colombia[,1],
                                     C = vocabulary.argentina[,1],
                                     D = vocabulary.venezuela[,1],
                                     E = vocabulary.peru[,1],
                                     F = vocabulary.chile[,1],
                                     G = vocabulary.mexico[,1]))
#241
vocab_solo_venezuela <- Reduce(setdiff,
                           list( A = vocabulary.venezuela[,1], 
                                 B = vocabulary.colombia[,1],
                                 C = vocabulary.argentina[,1],
                                 D = vocabulary.spain[,1],
                                 E = vocabulary.peru[,1],
                                 F = vocabulary.chile[,1],
                                 G = vocabulary.mexico[,1]))
#240
vocab_solo_peru <- Reduce(setdiff,
                               list( A = vocabulary.peru[,1], 
                                     B = vocabulary.colombia[,1],
                                     C = vocabulary.argentina[,1],
                                     D = vocabulary.spain[,1],
                                     E = vocabulary.venezuela[,1],
                                     F = vocabulary.chile[,1],
                                     G = vocabulary.mexico[,1]))
#154
vocab_solo_chile <- Reduce(setdiff,
                          list( A = vocabulary.chile[,1], 
                                B = vocabulary.colombia[,1],
                                C = vocabulary.argentina[,1],
                                D = vocabulary.spain[,1],
                                E = vocabulary.venezuela[,1],
                                F = vocabulary.peru[,1],
                                G = vocabulary.mexico[,1]))
#173
vocab_solo_mexico <- Reduce(setdiff,
                           list( A = vocabulary.mexico[,1], 
                                 B = vocabulary.colombia[,1],
                                 C = vocabulary.argentina[,1],
                                 D = vocabulary.spain[,1],
                                 E = vocabulary.venezuela[,1],
                                 F = vocabulary.peru[,1],
                                 G = vocabulary.chile[,1]))
#166
vocab_solo_colombia  <- vocabulary.colombia[vocabulary.colombia$WORD %in% vocab_solo_colombia,]
vocab_solo_argentina  <- vocabulary.argentina[vocabulary.argentina$WORD %in% vocab_solo_argentina,]
vocab_solo_spain  <- vocabulary.spain[vocabulary.spain$WORD %in% vocab_solo_spain,]
vocab_solo_venezuela  <- vocabulary.venezuela[vocabulary.venezuela$WORD %in% vocab_solo_venezuela,]
vocab_solo_peru  <- vocabulary.peru[vocabulary.peru$WORD %in% vocab_solo_peru,]
vocab_solo_chile  <- vocabulary.chile[vocabulary.chile$WORD %in% vocab_solo_chile,]
vocab_solo_mexico  <- vocabulary.mexico[vocabulary.mexico$WORD %in% vocab_solo_mexico,]

#vocab_propios_pais: unión de los 7 vocabularios propios de cada pais 
vocab_propios_pais <- rbind(vocab_solo_colombia,vocab_solo_argentina,vocab_solo_spain,vocab_solo_venezuela,vocab_solo_peru,vocab_solo_chile,vocab_solo_mexico)
#1319
typeof(vocab_propios_pais)
head(vocab_propios_pais)


# GENERAR LA BOLSA DE PALABRAS (BOW) -------------------------------------------------------------
lengths(vocab_propios_pais)
n <- 1319
bow_training.propiospais <- GenerateBoW(path_training, vocab_propios_pais, n, class="variety")
bow_test.propiospais <- GenerateBoW(path_test, vocab_propios_pais, n, class="variety")

#Trasponer para que cada columnna sea una caracteristica, y cada linea un autor
training <- concat.split(bow_training.propiospais, "V1", ",")
test <- concat.split(bow_test.propiospais, "V1", ",")

#Preparo los conjuntos de datos para el entrenamiento y test
#De training quito la columna con la linea original (columna 1), y el autor (columna 3)
training.prep <- training[,c(-1,-3)]
#Renombro la variable V1_0001 a class
names(training.prep)[1] <- "class"

#El conjunto de test tengo que quitar las tres primeras columnas: linea original, la clase y el autor
test.prep <- test[,c(-1,-2,-3)]

install.packages("MASS")

train_control <- trainControl(method="none")
metodo <- "svmLinear"
#metodo <- "knn"  --> accuracy 0.5243
#metodo <- "nb" #--> solicita  2 librerías, una no puedo instalarla
#metodo <- "svmExpoString" --> da error
#metodo <- "logreg" --> da error
metodo <- "rf"
model_SVM <- train( class~., data= training.prep, trControl = train_control, ntree=40, method = metodo) #,ntree=50 )

#La columna 2 de test tiene el valor real que las líneas de test. 
#Lo cargo en truth y as? comparo la predicci?n con truth
truth  <- unlist(test[,2])

pred_SVM <- predict(model_SVM, test.prep)
confusionMatrix(pred_SVM, truth)

# ---------------------------------------------------------------------- #
# Estrategia 2. Diccionario union de específicos con 100 palabras cada uno  
# ---------------------------------------------------------------------- #
  
#Nos quedamos con las 100 primeras palabras de cada diccionario. Están ordenados de mayor a menor
vocabulary.colombia <- vocabulary.colombia[1:100,]
vocabulary.argentina <-vocabulary.argentina[1:100,]
vocabulary.spain <- vocabulary.spain[1:100,]
vocabulary.venezuela <- vocabulary.venezuela[1:100,]
vocabulary.peru <-vocabulary.peru[1:100,] 
vocabulary.chile <-vocabulary.chile[1:100,]  
vocabulary.mexico <- vocabulary.mexico[1:100,]

# Se unen los siete diccionarios
vocab_propios_100 <- rbind(vocabulary.colombia,vocabulary.argentina,vocabulary.spain,vocabulary.venezuela,vocabulary.peru,vocabulary.chile,vocabulary.mexico)

# GENERAR LA BOLSA DE PALABRAS (BOW) -------------------------------------------------------------
#Dará una bolsa de palabras
#vocab_propios_100: unión de los 7 vocabularios de las 100 palabras más frecuentes de cada pais
lengths(vocab_propios_100)
n <- 700
bow_training.propios100 <- GenerateBoW(path_training, vocab_propios_100, n, class="variety")
bow_test.propios100 <- GenerateBoW(path_test, vocab_propios_100, n, class="variety")

#Trasponer para que cada columnna sea una caracter?stica, y cada l?nea un autor
training <- concat.split(bow_training.propios100, "V1", ",")
test <- concat.split(bow_test.propios100, "V1", ",")

#Preparo los conjuntos de datos para el entrenamiento y test
#De training quito la columna con la l?nea original (columna 1), y el autor (columna 3)
training.prep <- training[,c(-1,-3)]
#Renombro la variable V1_0001 a class
names(training.prep)[1] <- "class"

#El conjunto de test tengo que quitar las tres primeras columnas: l?nea original, la clase y el autor
test.prep <- test[,c(-1,-2,-3)]


# method = "knn" , "nb" (naive bayes), "svmExpoString"
train_control <- trainControl(method="none")
model_SVM <- train( class~., data= training.prep, trControl = train_control, method = "svmLinear")

#La columna 2 de test tiene el valor real que las líneas de test. 
#Lo cargo en truth y as? comparo la predicci?n con truth
truth  <- unlist(test[,2])

pred_SVM <- predict(model_SVM, test.prep)
confusionMatrix(pred_SVM, truth)







