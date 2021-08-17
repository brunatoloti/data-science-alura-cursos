# Instalando e importando bibliotecas
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)

# Declarando o caminho do arquivo a ser analisado
# Arquivo disponibilizado pelo curso e que se refere a um conjunto de dados da Play Store
path <- "C:/Users/Admin/Documents/Data Visualization - Gráficos com uma variável/Parte 1/googleplaystore.csv"

# Lendo o arquivo e salvando seus registros na variável dados
dados <-  read.csv(file = path, stringsAsFactors = FALSE)

# Visualizando os registros do conjunto de dados
View(dados)

# Visualizando as 6 primeiras linhas do conjunto de dados
head(dados)

# Visualizando as 6 últimas linhas do conjunto de dados
tail(dados)

# Visualizando o tipo de variável de cada coluna do conjunto de dados
str(dados) # Função análoga ao describe() do Python

# Fazendo algumas análises gráficas

# Como estão as avaliações das aplicações da loja do Google?
hist(dados$Rating) # Utilizando uma função nativa do R para construção do histograma
# Gerando o gráfico, não conseguimos ver o valor mínimo, mas conseguimos ver que 
# aparecem Ratings irrelevantes, dado que sabemos que as notas vão de 1 a 5

# Para verificarmos as frequências e os valores máximo e mínimo, podemos usar 
# a função table()
table(dados$Rating)
# Fazendo isso, conseguimos ver que, de fato, o valor mínimo é 1, porém o valor
# máximo é 19  e possui uma frequência de 1. Como sabemos que, na verdade, o 
# valor máximo é 5, podemos concluir que essa nota 19, que foi inserida apenas
# 1 vez, é um erro. Podemos remover essa linha do conjunto de dados.

# Mas, antes disso, mudando o layout do gráfico de forma que não tenhamos esse
# problema. Dado que já sabemos que as notas vão de 1 a 5, já colocamos como
# parâmetro o xlim que recebe um vetor de 1 a 5 que limita o eixo x a apenas 
# esses valores.
hist(dados$Rating, xlim = c(1,5))

# Gerando o gráfico com ggplot
rating.Histogram <- ggplot(data = dados) + geom_histogram(mapping = aes(x = Rating), 
                                                          na.rm = TRUE,
                                                          breaks = seq(1,5)) + xlim(c(1,5))
rating.Histogram

# Quais as categorias de aplicativos que estão em maior quantidade e 
# menor quantidade na loja?
ggplot(data = dados) + geom_bar(mapping = aes(x = Category), stat = 'count')
#stat: a própria função irá fazer a contagem de cada categoria, semelhante 
# à tabela de frequência? Ou será indicada essa contagem para ser plotada? Nesse
# caso, a própria função fará a contagem de cada categoria, por isso, stat='count'

# Gerando o gráfico, podemos notar que não há como ler os nomes das categorias.
# Dessa forma, precisamos melhorar o gráfico de forma que fique legíveis as 
# informações
# Para isso, passaremos de um gráfico de barras verticais para um de barras
# horizontais. Faremos isso, passando os ticks do eixo x para o eixo y e as
# contagens do eixo y para o eixo x, usando a função coord_flip()
ggplot(data = dados) + geom_bar(mapping = aes(x = Category), stat = 'count') + coord_flip()

# Fazendo uma ordenação das categorias de acordo com a quantidade no conjunto de dados
# Criando um df com as frequências simples de cada categoria
category.Freq <- data.frame(table(dados$Category))
# Gerando o gráfico utilizando esse subconjunto de dados gerado anteriormente
# Aqui nesse gráfico, a contagem será fornecida, por isso, stat='identity'
ggplot(data = category.Freq) + geom_bar(mapping = aes(x = reorder(Var1, Freq), y = Freq), 
                                        stat = 'identity') + coord_flip()

# Podemos visualizar apenas as 10 categorias que possuem mais aplicativos 
category.Top10 <- category.Freq[(order(-category.Freq$Freq)),]
category.Top10 <- category.Top10[1:10, ]
freq.Category.Plot <- ggplot(data = category.Top10) + geom_bar(mapping = aes(x = reorder(Var1, Freq), y = Freq), 
                                        stat = 'identity') + coord_flip()


# Fazendo correção de dados.
# Quando geramos o gráfico com a frequência das categorias na loja, conseguimos
# ver que havia uma categoria com nome '1.9', com frequência 1,
# o que não faz muito sentido.
# Além disso, podemos lembrar que havia um erro no Ratings, pois havia uma nota 19,
# sendo que as notas vão de 1 a 5, apenas. Portanto, podemos concluir que essa
# categoria '1.9' também se trata de um erro.
# Criando um subconjunto do conjunto original sem a linha onde categoria é '1.9'
dados2 <- dados %>% filter(Category != '1.9')

# Já que foi removida a categoria '1.9', será feita uma nova verificação do 
# atributo Ratings, verificando o valor mínimo e máximo
min(dados2$Rating)
max(dados2$Rating)
# Obtivemos como resultado 'NaN'. Ou seja, há registros sem valores no conjunto de dados
# Mas, quantos registros estão sem valores?
dados2 %>% filter(is.na(Rating)) %>% count()
# Temos 1474 registros NA's na coluna Rating
# Podemos verificar isso de outra forma
summary(dados2$Rating)
# A partir dessa função summary() já conseguimos ver também os valores mínimo e
# máximo, tirando esses valores NA's

# Mais de 10% dos dados possui Rating NA, um número considerável de valores faltantes.
# Corrigindo os registros faltantes usando a média
mean.Category <- dados2 %>% filter(!is.na(Rating)) %>% 
  group_by(Category) %>% 
  summarise(media = mean(Rating)) #Criando um novo conjunto com os valores de média

for (i in 1:nrow(dados2)){
  if (is.na(dados2[i, 'Rating'])){
    dados2[i, 'newRating'] <- mean.Category[mean.Category$Category == dados2[i, 'Category'],'media']
  }else{
    dados2[i, 'newRating'] <- dados2[i, 'Rating']
  }
}
# Verificando agora o número de valores NA's e valores máximo e mínimo
summary(dados2$newRating)
# De outra forma
min(dados2$newRating)
max(dados2$newRating)
dados2 %>% filter(is.na(newRating)) %>% count()


# Criando rótulos, novos atributos
# Dado que os dados já foram corrigidos, agora serão criados rótulos para os 
# valores presentes em newRating, de forma que:
# newRating <= 2: 'ruim'
# newRating >= 4: 'bom'
# 2.1 < newRating < 3.9: 'regular'

dados2 <- dados2 %>% 
            mutate(Ratingclass = if_else(newRating <= 2, 'ruim',
                                 if_else(newRating >= 4, 'bom',
                                         'regular')))  #mutate cria uma nova coluna no conjunto

# Gerando um gráfico com essas novas informações, com esses rótulos
RatingClass.Plot <- ggplot(dados2) + geom_bar(aes(Ratingclass), stat = 'count')


# Gerando um gráfico de pizza para visualizarmos a proporção do tipo de aplicativo,
# ou seja, a proporção de apps pagos e não pagos
# Como temos apenas dois tipos diferentes de aplicativos, o gráfico de pizza será usado
type.Freq <- data.frame(table(dados2$Type))

type.Freq.Plot <- ggplot(type.Freq) + geom_bar(aes(x = '', y = Freq, fill = Var1), 
                             stat = 'identity',
                             width = 1) +
                    coord_polar(theta = 'y', start = 0) #altera o formato da barra


# O que podemos concluir sobre o tamanho de download e instalação dos aplicativos?
str(dados2)
# Olhando o tipo de dado na coluna Size, podemos ver que é do tipo chr, ou seja,
# não é numérica. Teoricamente, deveria ser numérica, porque deveria armazenar
# apenas o tamanho do aplicativo, porém, possui letras junto com números nos seus valores.
# Conseguimos ver, pelos exemplos iniciais que a str() dá, que há valores armazenando
# M nos registros (megabytes). Porém, há outros tipos de valores que devem ser
# tratados nessa coluna?
# Fazendo uma tabela de frequência para verificar isso
freq.Size <- data.frame(table(dados2$Size))
freq.Size
# Ao explorar essa tabela de frequência, foi possível identificar que os tamanhos
# dos aplicativos são indicados como kilobytes, pela letra k, ou megabytes, pela 
# letra M. Além disso, temos valores como Varies with device, que nos indica que
# o tamanho depende do dispositivo.

# Transformando esses valores, convertendo-os para a mesma escala (aqui, kilobytes)
# Verificando a ocorrência de K ou M nos registros
# Se tiver K no registro, apenas elimina a letra; se tiver M no registro, faz a conversão
# de megabyte para kilobyte (1K = 1024M)

# Exemplo com 1 string de teste
teste <- dados2$Size[1]
grepl(pattern = 'M', x = teste, ignore.case = T) # Ocorrencia de M em teste?
grepl(pattern = 'K', x = teste, ignore.case = T) # Ocorrencia de K em teste?
gsub(pattern = 'M', replacement = '--', x = teste, ignore.case = T) # Substituicao de M por --
gsub(pattern = 'K', replacement = '--', x = teste, ignore.case = T) # Substituicao de K por --

# Aplicando para a coluna inteira
dados2$kb <- sapply(X = dados2$Size, FUN = function(y){
  if (grepl('M', y, ignore.case = T)){
    y <- as.numeric(gsub(pattern = 'M', replacement = '', x = y, ignore.case = T))*1024
  }else if (grepl('K|\\+', y, ignore.case = T)){
    y <- as.numeric(gsub(pattern = 'K|\\+', replacement = '', x = y, ignore.case = T))
  }else{
    y <- 'nd'
  }
})

hist(as.numeric(dados2$kb))
# Resolvendo o problema do eixo x para que não fique mais em notação científica
options(scipen = 999)
# Gerando o histograma mais uma vez
hist(as.numeric(dados2$kb))

# Removendo os registros nd e convertendo a coluna para o tipo numérico, colocando 
# em um novo conjunto de dados essas mudanças (importante manter os dados originais,
# dessa forma, criar um novo conjunto de dados com mudanças, faz sentido)
size.app <- dados2 %>% filter(kb != 'nd') %>% mutate(kb = as.numeric(kb))
# Gerando um gráfico desse novo conjunto de dados
size.App.Plot <- ggplot(size.app) + geom_histogram(aes(kb))
size.App.Plot








