##### VISUALIZANDO DADOS COM GR??FICOS ####
## instalando e habilitando a biblioteca que ser?? utilizada
#install.packages("ggplot2")
#install.packages("dplyr")
library(ggplot2)
library(dplyr)
library(scales)

## Histograma

# histograma com fun????o nativa da variavel rating
hist(dados$Rating) ## VISUALIZA????O RUIM COM LIB NATIVA DO R

# calculando a frequencia do rating
table(dados$Rating) # geralmente os ratings vai de 1 a 5, pode-se observar que no nosso conjunto de dados o m??xima do valor ?? 19 o que provavelmente ?? um valor errado
# neste momento n??o iremos tratar esse valor, ao inv??s disse iremos ajustar noss histograma para contabilizar os dados que v??o de 1 a 5

hist(dados$Rating,xlim = c(1,5) ) # histrogram com a visualiza????o melhorada

# histograma utilizando lib ggplot
## GR??FICO COM VIZ RUIM E WARNING MESSAGE DE REMO????O DE VALORES non-finite
ggplot(data = dados) +
  geom_histogram(mapping = aes(x = Rating)) 


## MELHORANDO A VISUALIZA????O ALTERANDO OS LIMITES DO EIXO X e ALTERANDO O BINS      
ggplot(data = dados) +
  geom_histogram(mapping = aes(x = Rating)) + xlim(c(1,5))

rating.Histrogram <- ggplot(data = dados) +
  geom_histogram(mapping = aes(x = Rating),na.rm = T,breaks = seq(1,5,1)) ## parametro na.rm ir?? excluir todos os valores NaN, assim, ir?? remover o WARNING gerado anteriormente


#### gr??fico barras verticais e horizontais ####

# simulando erro nome do atributo do conj. de dados
ggplot(data = dados) + geom_bar(aes(x = category), stat = "count") 

# simulando gr??fico ruim vertical
category.Plot <- ggplot(data = dados) +
  geom_bar(mapping = aes(x = Category),stat = "count") 
category.Plot

# simulando gr??fico ruim horizontal
category.Plot.Horiz <- ggplot(data = dados) +
  geom_bar(mapping = aes(x = Category) ,stat = "count") +
  coord_flip() 
category.Plot.Horiz

## criando um novo subconjunto de dados e transforma????o dos dados.
# conjunto de dados com category e contagem de cada uma
category.Freq <- data.frame(table(dados$Category))

# gr??fico horizontal, simulando com erro, faltando Y
freq.Category.Plot <-   ggplot(data = category.Freq) +
  geom_bar(mapping = aes(x = Var1, y = Freq) ,stat = "identity") +
  coord_flip() 
freq.Category.Plot 

# gr??fico horizontal bom com ordena????o por frequencia
ggplot(data = category.Freq) +
  geom_bar(mapping = aes(x = Var1,y = Freq) ,stat = "identity") +
  coord_flip()

freq.Category.Plot <- ggplot(data = category.Freq) +
  geom_bar(mapping = aes(x = reorder(Var1,Freq),y = Freq) ,stat = "identity") +
  coord_flip() 
freq.Category.Plot

# gr??fico com ordena????o crescente
ggplot(data = category.Freq) +
  geom_bar(mapping = aes(x = reorder(Var1,-Freq),y = Freq) ,stat = "identity") +
  coord_flip()

# Plot somente com as 10 primeiras categorias (podeira fazer criando um outro data frame)
ggplot(data = category.Freq[1:10,]) +
  geom_bar(mapping = aes(x = reorder(Var1,Freq),y = Freq) ,stat = "identity") +
  coord_flip() 

#Criando novo conjunto de dados com Top 10 categorias
category.Freq <- category.Freq[order(-category.Freq$Freq),] # Ordem decrescente
category.Top10 <- category.Freq[1:10,]

freq.Category.Plot <- ggplot(data = category.Top10) +
  geom_bar(mapping = aes(x = reorder(Var1,Freq),y = Freq) ,stat = "identity") +
  coord_flip()

freq.Category.Plot

#Criando novo conjunto de dados com as 10 menos categorias
# ordenando o data frame em ordem decrescente, pelo parametro Freq
category.Freq.menos <- category.Freq[order(category.Freq$Freq),] # Ordem crescente
category.Freq.menos <- category.Freq.menos[1:10,]

ggplot(data = category.Freq.menos) +
  geom_bar(mapping = aes(x = reorder(Var1,-Freq),y = Freq) ,stat = "identity") +
  coord_flip()

####### 2.5 CORRE????O DE DADOS E NOVOS ATRIBUTOS. ######
## Criando categorias/rotulos para variavel Rating e visualizando
# Eliminando registro inconsistente e salvando em um novo conj de dados
dados_2 <- dados %>% 
  filter(Category != "1.9")

# verificando valores minimos e m??ximos de rating
min(dados_2$Rating)
max(dados_2$Rating)

# verificando quantos registros est??o com valor NaN.
dados_2 %>% filter(is.na(Rating)) %>% count()
summary(dados_2$Rating)

# ?? necess??rio tratar valores NaN. H?? varias metodologias para tratar esse tipo de problma:
# 1 - recorrendo ao respons??vel pelos dados (o que n??o ?? poss??vel no nosso caso)
# 2 - eliminando estes registros
# 3 - corrigindo esses registros com base em alguma m??trica estatistica, por exemplo, m??dia

####### 2.6 CORRE????O DE DADOS E NOVOS ATRIBUTOS - II. ######

# Fazendo um agrupamento de registros NaN por Category
dados_2 %>%
  filter(is.na(Rating) ) %>%
  group_by(Category) %>%
  count()

# Fazendo a m??dia de Rating por Category e salvando em um novo objeto
mean.Category <- dados_2 %>%
  filter(!is.na(Rating)) %>%
  group_by(Category) %>%
  summarise(mean = mean(Rating))


####### 2.7 CORRE????O DE DADOS E NOVOS ATRIBUTOS - III. ######

# substituindo valores NA pela m??dia de Rating de cada categoria
# pode ser feito com loop.
for(i in 1:nrow(dados_2)){
  if(is.na(dados_2[i,"Rating"])){
    dados_2[i,"newRating"] <- mean.Category[mean.Category$Category == dados_2[i,"Category"],]$mean
  }else{
    dados_2[i,"newRating"] <- dados_2[i,"Rating"]
  }
}

summary(dados_2$newRating)
min(dados_2$newRating)
max(dados_2$newRating)


####### 2.8 CORRE????O DE DADOS E NOVOS ATRIBUTOS - IV. ######
dados_2 <- dados_2 %>%
  mutate(rating_class = ifelse(newRating < 2, "ruim",
                               ifelse(newRating > 4,"bom", "regular")))
View(dados_2)

rating_class_plot <- ggplot(data = dados_2) +
  geom_bar(aes(rating_class), stat = "count")

rating_class_plot