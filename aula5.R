##### 5. ALTERANDO LAYOUTS DOS GR?FICOS  ##### 
rating.Histrogram
freq.Category.Plot
type.Plot
size.App.Plot
rating_class_plot
nota_plot

##### 5.1 Alterando histograma rating ##### 

# inserindo titulo
rating.Histrogram <- rating.Histrogram + ggtitle("Histograma Rating")

#centralizando titulo
rating.Histrogram + theme(plot.title = element_text(hjust = 0.5))

# alterando cor de fundo
rating.Histrogram <- rating.Histrogram + theme_light()

# outros layouts       
rating.Histrogram + theme_classic()
rating.Histrogram + theme_dark()
rating.Histrogram + theme_linedraw()

rating.Histrogram

##### 5.2 Alterando layout freq.Category.Plot ##### 

## Inserindo T??tulo no gr??fico
freq.Category.Plot <- freq.Category.Plot + ggtitle("Quantidade de app por categoria")
freq.Category.Plot

## Alterando Labels X e Y (simulando ERRO)
freq.Category.Plot + xlab("Quantidade") + ylab("Categoria")

## Alterando Labels X e Y (correto)
freq.Category.Plot <- freq.Category.Plot + xlab("Categoria") + ylab("Quantidade")

freq.Category.Plot      

## Alterando cor das barras

# cor fixa
freq.Category.Plot + geom_bar(aes(Var1,Freq),fill = "blue", stat = "identity") 

# cores de acordo com uma vari??vel
freq.Category.Plot + geom_bar(aes(Var1,Freq,fill = Var1), stat = "identity")

# Alterando cor das barras de acordo com uma vari??vel numerica
freq.Category.Plot + geom_bar(aes(Var1,Freq,fill = Freq), stat = "identity")

# Alterando cor das barras (VIZ BOA)
freq.Category.Plot <- freq.Category.Plot + geom_bar(aes(Var1,Freq),fill = "darkcyan", stat = "identity")

# alterando tema do gr??fico. Visualiza????o ruim
freq.Category.Plot + theme_dark()

# alterando tema do gr??fico. Visualiza????o boa
freq.Category.Plot <- freq.Category.Plot + theme_light()

freq.Category.Plot

##### 5.3 Alera??o gr?fico (pizza) type.plot. ##### 
library(scales)
# criando um objeto com os temas
blank_theme <-  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    axis.text.x=element_blank()
    #plot.title=element_text(size=14, face="bold")
  )
# retirando eixos x e y, colocando fundo branco, retirando valores ao redor
type.Plot <- type.Plot + blank_theme



# criando labels
type.Plot <- type.Plot + geom_text(aes(x = "", y = Freq/2, 
                                       label = percent(Freq/sum(Freq))), size=5)


type.Plot <- type.Plot + scale_fill_discrete(name="Tipo App")

type.Plot <- type.Plot + ggtitle("Tipo de App's") + theme(plot.title = element_text(hjust = 0.5))

type.Plot


##### 5.4 Altera??o size.App.Plot ##### 
#alterando tema do histograma
size.App.Plot <- size.App.Plot + ggtitle("Histograma Size App(tamanho de instala??o do app)")


# cor fixa das barras
size.App.Plot + geom_histogram(aes(kb), fill = "blue") # viz razoavel

# fun??o para gera c?digo HEX de cores
rainbow(30)

#utilizando rainbow para definir as cores das barras
size.App.Plot + geom_histogram(aes(kb), fill = rainbow(30)) # viz ruim

# utilizando scala de cor para barras        
size.App.Plot <- size.App.Plot + geom_histogram(aes(kb, fill = ..x..)) + 
  scale_fill_gradient(low='blue', high='yellow') + guides(fill=FALSE)

#alterando r?tulo eixo X e Y
size.App.Plot <- size.App.Plot + xlab("Tamanho App (em KB)") + ylab("Quantidade de Apps")

# alterando layout
size.App.Plot <- size.App.Plot + theme_bw()

size.App.Plot

##### 5.5 alterando gr?fico rating_class_plot ##### 

#inserindo t?tulo
rating_class_plot <- rating_class_plot + ggtitle("Categoria de Notas do App(Rating)")
# alterando labels X e Y
rating_class_plot <- rating_class_plot + xlab("Categoria") + ylab("Quantidade")

# alterando cores das barras. DATA VIZ CONFUSA(vermelho para categoria BOM, verde REGULAR e azul RUIM)
rating_class_plot + geom_bar(aes(rating_class, fill = rating_class))

# corrigindo cores barras, para ficar mais intuitivo (forma mais complicada)
rating_class_plot + geom_bar(aes(rating_class, fill = rating_class)) +
  scale_fill_manual("legend", values = c("bom" = "green4", "regular" = "yellow2", "ruim" = "red"))

# corrigindo cores barras, para ficar mais intuitivo (forma mais complicada)
rating_class_plot <- rating_class_plot + geom_bar(aes(rating_class),fill = c("green4","yellow2","red"))

rating_class_plot <- rating_class_plot + theme_bw()

rating_class_plot

##### 5.6 alterando gr??fico de linha nota_plot (EXERC??CIO) ##### 
nota_plot + geom_line(aes(as.Date(data_2), media), color = "blue") + theme_bw() + ggtitle("M??dia das Avalia????es dos Apps") + xlab("Data") + ylab("M??dia Nota")


##### 5.6 visuzlizando tudo em uma janela ####

#install.packages("gridExtra")
library(gridExtra)      

# EXEMPLO
grid.arrange(rating.Histrogram,freq.Category.Plot, nrow = 1)

# gr??ficos em uma linha (DATA VIZ RUIM)
grid.arrange(rating.Histrogram, freq.Category.Plot, type.Plot, rating_class_plot, size.App.Plot,nota_plot, nrow = 1)

# ERRO PRA QUANTIDADE DE ROW e COLUNAS
grid.arrange(rating.Histrogram, freq.Category.Plot, type.Plot, rating_class_plot, size.App.Plot,nota_plot, nrow = 1,ncol = 2)

# alterando n??mero de linhas e colunas (DATA VIZ BOA)
grid.arrange(rating.Histrogram, freq.Category.Plot, type.Plot, rating_class_plot, size.App.Plot,nota_plot, nrow = 2,ncol = 3)

# alterando o tamanho das linhas
grid.arrange(rating.Histrogram, freq.Category.Plot, type.Plot, rating_class_plot, size.App.Plot,nota_plot, nrow = 2,ncol = 3, heights = c(2.5,5))
