####### 3 GRAFICO DE PIZZA, SAPPLY, GREPL E GSUB ######     

####### 3.1  GRAFICO DE PIZZA ######     

# criando novo conj de dados com frequencia de Type
type.Freq <- data.frame(table(dados_2$Type))
View(type.Freq)

# GR??FICO DE BARRAS INICIAL
ggplot(data = type.Freq) +
  geom_bar(aes("",Freq,fill = Var1),stat = "identity",width = 1)

# GR??FICO DE DE PIZZ
type.Plot <- ggplot(data = type.Freq) +
  geom_bar(aes("",Freq,fill = Var1),stat = "identity",width = 1) +
  coord_polar(theta = "y",start = 0)

type.Plot  

##### 3.2 TRANSFORMANDO E CRIANDO NOVOS ATRIBUTOS #####

## converter valores tudo para Kb (kilobytes) e visualizar
# verificando tipos de valores na base de dados
str(dados_2)

# salvando a tabela de frequencia em um objeto
freq.Size <- data.frame(table(dados_2$Size),stringsAsFactors = F)
View(freq.Size)

#Verificando ocorr??ncia de determinado padr??o em uma string utilizando a func??o grepl(). Essa fun????o retorna TRUE ou FALSE.
grepl(pattern = "M", x = dados_2$Size[1],ignore.case = T)
grepl(pattern = "k", x = dados_2$Size[1],ignore.case = T)

# Criando um novo campo com tamanho do app tudo em KB
# est?? sendo utilizada a fun????o sapply(): essa fun????o ?? um tipo de loop mais 
# performatico do que um for ou while. O sapply retorna uma lista com os resultado
# por??m, ?? desejado que o resultado seja um vetor normal, para isso, foi utilizada outra
# fun????o aninhada ao sapply, a fun????o unlist.
# Para eliminar os caracteres desejados de cada linha, vai ser utilizada a fun????o gsub()

gsub(pattern = "M", replacement = "--",x = dados_2$Size[1])
gsub(pattern = "M", replacement = "",x = dados_2$Size[1])

# demonstra????o de como a fun????o sapply funciona
sapply(X = freq.Size$Var1, FUN = function(x){
  print(as.character(x))
})

# exemplo de multiplica????o
1 * 1024

##### 3.3 TRANSFORMANDO E CRIANDO NOVOS ATRIBUTOS 2 #####

# pr??totipo inicial da transforma????o
sapply(X = freq.Size$Var1, FUN = function(x){
  if (grepl("M", x,ignore.case = T) ){
    print("Encontrou valor M")
  }else if(grepl("k", x,ignore.case = T)){
    print("Encontrou valor K")
  }else{
    print("nd")
  }
})

# criando novo atributo com novos valores
freq.Size$trans <- sapply(X = freq.Size$Var1, FUN = function(x){
  if (grepl("M", x) ){
    x <- as.numeric(gsub(pattern = "M",replacement = "",x = x)) * 1024
  }else if(grepl("k|\\+", x)){
    x <- gsub(pattern = "k|\\+|\\,",replacement = "",x = x)
  }else{
    x <- "nd"
  }
})

dados_2$kb <- sapply(dados_2$Size, function(x){
  if (grepl("M", x) ){
    x <- as.numeric(gsub(pattern = "M",replacement = "",x = x)) * 1024
  }else if(grepl("k|\\+", x)){
    x <- gsub(pattern = "k|\\+|\\,",replacement = "",x = x)
  }else{
    x <- "nd"
  }
})


# histograma com o tamanho do app em kb

# simulando erro ao gerar histograma com campo n??o num??rico
hist(dados_2$kb)

# nesse caso ir?? emitir um warning para NAs. Isso ?? causado porque h?? valores n??o num??ricos 
#no conjunto de dados.
hist(as.numeric(dados_2$kb))

options(scipen = 999) # Desabilita nota????o cient??fica

# para resolver o problema acima bastar criar um novo conj de dados sem os valores nd
size.app <- dados_2 %>%
  filter(kb != "nd") %>%
  mutate(kb = as.numeric(kb))

# histograma do parametro kb com a lib ggplot
size.App.Plot <- ggplot(data = size.app) + geom_histogram(aes(kb))