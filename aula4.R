#https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html

library(lubridate)

##### 4.1 TRABALHANDO COM DATAS #####

dmy("05-07-2018")
dmy("05072018")

dmy("572018") ## ERRO NA CONVERS??O DA DATA (DIA e MES COM APENAS 1 DIGITO)
dmy("1022018") ## simulando erro com dia incompleto
dmy("0122018") ## simulando erro com mes incompleto

ymd("20190101") #FORMATO ANO/MES/DIA
mdy("12252018") #FORMATO MES/DIA/ANO

ymd("20182001") ## simulando erro com mes-dia invertido



## DATAS COM DIFERENTES SEPARADORES
ymd("2018/01/20")
ymd("2018-01-20")
ymd("2018*01*20")
ymd("2018*01/20")


ymd("180120") ## formato do ano apenas com 2 digitos
ymd("690120") ## ano m?nimo com 2 digitos 1969
ymd("680120") ## ano m?ximo com 2 digitos 2068


##### 4.2 TRABALHANDO COM  HORAS #####

ymd_hms("2018-01-24 12:00:00") ## COM ESPA?O
ymd_hms("2018-01-2412:00:00") ## SEM ESPA?O
ymd_hm("2018-01-24 12:00") ## sem segundos
ymd_h("2018-01-24 12") ## somente com as hora

hms("12:00:00")
hm("12:00:00") ## erro no formato
hm("12:00")
hours("12")


data_hora <- ymd_hms("20180123120515")
data_hora
month(data_hora)
mday(data_hora)
year(data_hora)

hour(data_hora)
minute(data_hora)
second(data_hora)

wday(data_hora,label = T) ## com label
wday(data_hora) ## sem label

month(data_hora,label = T) ## com label
month(data_hora) ## sem label

data.frame(data_hora) %>%
  mutate(m = month(data_hora,label = T))


##### 4.4 GRAFICO DE LINHA #####

## grafico de linhas
##Transforma????o e visualiza????o do campo Last.Updated
notas <- read.csv(paste(paths[p],"/user_reviews.csv",sep = ""))
str(notas)

notas$data_2 <- ymd_hms(notas$data)
str(notas)

ggplot(notas) + geom_line(aes(x = data_2 , y = Sentiment_Polarity)) 

# convertendo a data para o formato ANO-MES
notas$data_2 <- parse_date_time(format(notas$data_2, "%Y-%m"),"ym")

ggplot(notas) + geom_line(aes(x = data_2 , y = Sentiment_Polarity))
ggplot(notas) + geom_point(aes(x = data_2 , y = Sentiment_Polarity))


# CALCULANDO MEDIA DE "SENTIMENT_POLARITY" POR DIA
media_nota <- notas %>%
  group_by(data_2) %>%
  summarise(media = mean(Sentiment_Polarity))

nota_plot <- 
  ggplot(media_nota) + geom_line(aes(x = as.Date(data_2), y = media)) 

nota_plot
