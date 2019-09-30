paths <- c("/Users/gabriel.gomes/alura/dataviz_com_R","C:/Users/gabri/Documents/GitHub/alura/dataviz_com_R",
           "G:/GitHub/alura/dataviz_com_R")
p <- 1

##### 1.2 IMPORTANTADO E EXPLORANDO OS DADOS ####
dados <- read.csv(paste(paths[p],"/googleplaystore.csv",sep = ""))

## visualizando todos os dados na aba (View())
view(dados) ##SIMULANDO ERRO AO CHAMAR A FUNCAO
View(dados)

## visualizando os primeiros registros
head(dados)

## visualizando os ??ltimos registros
tail(dados)

##### 1.3 EXPLORANDO A BASE DE DADOS ####
## identificando vari??veis quantitativas (discretas e cont??nuas) e qualitativas (nominal e ordinal)
str(dados)

# quantitativas: rating (discretas), reviews (cont??nuas)
# qualitativas: app (nominal), type (ordinal) 

unique(dados$Installs)
unique(dados$Reviews)

## CARREGANDO OS DADOS SEM stringAsFactor
dados <- read.csv(paste(paths[p],"/googleplaystore.csv",sep = ""),stringsAsFactors = FALSE)
