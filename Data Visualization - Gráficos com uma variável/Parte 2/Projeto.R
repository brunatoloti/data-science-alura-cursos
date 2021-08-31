# Análise dos Microdados do Enem (2010-2017)

# Importando e carregando as bibliotecas
library(data.table)
library(dplyr)
library(ggplot2)
install.packages('reshape')
library(reshape)

getwd()
setwd('C:/Users/Admin/Documents/Data Visualization - Gráficos com uma variável/Parte 2')

# Lendo as bases de dados
# Bases disponibilizadas pelo curso e disponível no site do INEP
enem_2010 <- fread('enem_2010.csv', encoding = 'UTF-8')
enem_2011 <- fread('enem_2011.csv', encoding = 'UTF-8')
enem_2012 <- fread('enem_2012.csv', encoding = 'UTF-8')
enem_2013 <- fread('enem_2013.csv', encoding = 'UTF-8')
enem_2014 <- fread('enem_2014.csv', encoding = 'UTF-8')
enem_2015 <- fread('enem_2015.csv', encoding = 'UTF-8')
enem_2016 <- fread('enem_2016.csv', encoding = 'UTF-8')
enem_2017 <- fread('enem_2017.csv', encoding = 'UTF-8')

# Fazendo um merge de todos esses dataframes
merge_enem <- rbind(enem_2010, enem_2011, enem_2012, enem_2013, enem_2014, enem_2015,
                    enem_2016, enem_2017, fill = TRUE)

# Eliminando as bases que não iremos mais utilizar, para liberar memória
rm(enem_2010, enem_2011, enem_2012, enem_2013, enem_2014, enem_2015,
   enem_2016, enem_2017)

# Selecionando apenas as colunas de interesse
colunas <- c('NUMERO_INSCRICAO', 'ANO', 'CO_MUNICIPIO_RESIDENCIA', 'MUNICIPIO_RESIDENCIA',
             'UF_RESIDENCIA', 'UF_ESCOLA', 'IDADE', 'SEXO', 'SITUACAO_CONCLUSAO',
             'BRAILLE', 'MUNICIPIO_PROVA', 'UF_PROVA', 'PRESENCA_CIENCIAS_NATUREZA',
             'PRESENCA_CIENCIAS_HUMANAS', 'PRESENCA_LINGUAGENS_CODIGOS', 
             'PRESENCA_MATEMATICA', 'NOTA_CIENCIAS_NATUREZA', 'NOTA_CIENCIAS_HUMANAS',
             'NOTA_LINGUAGENS_CODIGOS', 'NOTA_MATEMATICA', 'TIPO_LINGUA', 'STATUS_REDACAO',
             'NOTA_REDACAO')

enem <- merge_enem %>% select(all_of(colunas))
rm(merge_enem)

# Visualizando o resumo da base de dados
str(enem)

# Normalizando a coluna sexo
table(enem$SEXO)
# Observe que temos tanto o valor F quanto o valor 1 determinando o sexo feminino 
# (de acordo com o dicionário de variáveis)
# E que temos tanto o valor M quanto o valor 0 determinando o sexo masculino
# (de acordo com o dicionário de variáveis)
# Por isso, para não ficar confuso e, também, não ficar com colunas diferentes 
# querendo determinar a mesma coisa, é preciso normalizar
enem$SEXO <- gsub('1', 'FEMININO', enem$SEXO)
enem$SEXO <- gsub('^F$', 'FEMININO', enem$SEXO)
enem$SEXO <- gsub('0', 'MASCULINO', enem$SEXO)
enem$SEXO <- gsub('^M$', 'MASCULINO', enem$SEXO)
table(enem$SEXO)

# Normalizando a coluna tipo_lingua (idioma escolhido pelo aluno)
table(enem$TIPO_LINGUA)
# Observe que temos os valores . (provavelmente, é um erro), o 0 que representa
# Inglês (de acordo com o dicionário de variáveis) e o 1 que representa Espanhol
# (de acordo com o dicionário de variáveis)
enem$TIPO_LINGUA <- gsub('0', 'INGLÊS', enem$TIPO_LINGUA)
enem$TIPO_LINGUA <- gsub('1', 'ESPANHOL', enem$TIPO_LINGUA)
table(enem$TIPO_LINGUA)

# Verificando a coluna uf_prova
table(enem$UF_PROVA)
length(table(enem$UF_PROVA))
# Observe que o número de registros únicos dessa coluna é 28, porém, o Brasil
# possui 27 estados. Verificando mais atentamente, notamos que há 1480 registros
# em branco nessa coluna (provavelmente, erro na hora da inserção).
# Não iremos tratar isso, para não perder 1480 registros que podem ser importantes
# para as outras colunas.

# Normalizando a coluna situacao_conclusao
table(enem$SITUACAO_CONCLUSAO)
# Segundo o dicionário de variáveis:
# 1 -> Já conclui o Ensino Médio
# 2 -> Estou cursando e concluirei o Ensino Médio em 2011
# 3 -> Estou cursando e concluirei o Ensino Médio após 2011
# 4 -> Não conclui e não estou cursando o Ensino Médio
enem$SITUACAO_CONCLUSAO <- gsub('1', 'CONCLUÍDO', enem$SITUACAO_CONCLUSAO)
enem$SITUACAO_CONCLUSAO <- gsub('2', 'CONCLUIRÁ NO ANO', enem$SITUACAO_CONCLUSAO)
enem$SITUACAO_CONCLUSAO <- gsub('3', 'CONCLUIRÁ APÓS ANO', enem$SITUACAO_CONCLUSAO)
enem$SITUACAO_CONCLUSAO <- gsub('4', 'NÃO CONCLUÍDO/CURSANDO', enem$SITUACAO_CONCLUSAO)
table(enem$SITUACAO_CONCLUSAO)

# Quando foi feito str(enem), verificamos que as colunas de notas são do tipo
# char e não do tipo numérica
# Observe dessa forma:
summary(enem$NOTA_CIENCIAS_HUMANAS)
summary(enem$NOTA_CIENCIAS_NATUREZA)
summary(enem$NOTA_LINGUAGENS_CODIGOS)
summary(enem$NOTA_MATEMATICA)
summary(enem$NOTA_REDACAO)

# Convertendo essas colunas para o tipo numérico (lembrando que iremos ficar
# com linhas com valores NA's, dado que nem todos os valores das linhas podem
# ser convertidos para número)
enem$NOTA_CIENCIAS_HUMANAS <- as.numeric(enem$NOTA_CIENCIAS_HUMANAS)
enem$NOTA_CIENCIAS_NATUREZA <- as.numeric(enem$NOTA_CIENCIAS_NATUREZA)
enem$NOTA_LINGUAGENS_CODIGOS <- as.numeric(enem$NOTA_LINGUAGENS_CODIGOS)
enem$NOTA_MATEMATICA <- as.numeric(enem$NOTA_MATEMATICA)
enem$NOTA_REDACAO <- as.numeric(enem$NOTA_REDACAO)
# Verificando que são, de fato, do tipo numérico
str(enem)


# Realizando algumas análises gráficas
ggplot(data = enem) + geom_bar(aes(x = TIPO_LINGUA), stat = 'count')

# Gráfico de barras com sexo e o tipo de idioma escolhido
# Primeiramente, fazendo uma filtragem para que não usemos valores '.' de tipo de 
# idioma
tp_lingua_sexo <- enem %>% 
                    filter(TIPO_LINGUA != '.') %>% 
                    select(all_of(c('SEXO', 'TIPO_LINGUA')))
plot_idioma_sexo <- ggplot(data = tp_lingua_sexo) +
                      geom_bar(aes(x = SEXO, fill = TIPO_LINGUA), 
                               stat = 'count', position = position_dodge())
p <- plot_idioma_sexo +
      ggtitle('Idioma escolhido por sexo') +
      xlab('Sexo') + ylab('Quantidade')
p <- p + theme_linedraw() +
      theme(plot.title = element_text(hjust = 0.5))

plot_idioma_sexo <- p
plot_idioma_sexo

#Gráfico sobre a situação de escolaridade das pessoas que estão fazendo o Enem por estado
options(scipen = 9999)

ggplot(data = enem) +
  geom_bar(aes(x = UF_PROVA), stat = 'count')

uf_prova <- enem %>% 
              filter(UF_PROVA != '') %>%
              select(all_of(c('UF_PROVA', 'SITUACAO_CONCLUSAO')))

plot_uf_conclusao <- ggplot(data = uf_prova) +
                     geom_bar(aes(x = UF_PROVA, fill = SITUACAO_CONCLUSAO),
                               position = position_dodge()) + 
                     facet_grid(SITUACAO_CONCLUSAO~.)
p <- plot_uf_conclusao + 
       ggtitle('Situação escolar por estado') +
       xlab('Estado') + ylab('Quantidade')

p <- p + theme_linedraw() +
      labs(fill = 'Situação') + 
      theme(plot.title = element_text(hjust = 0.5))
plot_uf_conclusao <- p
plot_uf_conclusao


# Visualizando a média de idade por sexo e UF da prova
summary(enem$IDADE)
# Eliminando registros que são NA's na coluna IDADE
idade_uf <- enem %>%
               filter(!is.na(IDADE))
summary(idade_uf$IDADE)
# Agrupando os dados por UF_PROVA e SEXO e aplicando a média de idade
media_idade_sexo_uf <- idade_uf %>% 
                        group_by(UF_PROVA, SEXO) %>%
                        summarise(media = mean(IDADE))
# Removendo registros onde UF_PROVA é vazio
media_idade_sexo_uf <- media_idade_sexo_uf %>%
                        filter(UF_PROVA != '')
# Gerando a visualização (gráfico de pirâmide)
ggplot(data = media_idade_sexo_uf) +
   geom_bar(aes(x = UF_PROVA, y = media, fill = SEXO),
            position = position_dodge(), stat = 'identity') +
   coord_flip() # Gráfico de barras

plot_piram_idade <- ggplot(data = media_idade_sexo_uf, 
                      aes(x = reorder(UF_PROVA, -media),
                          y = ifelse(SEXO == 'MASCULINO', -media, media),
                          fill = SEXO)) + 
                  geom_bar(stat = 'identity') +
                  coord_flip()

plot_piram_idade <- plot_piram_idade + 
   scale_y_continuous(labels = abs) # Deixar os valores de y como positivo

plot_piram_idade <- plot_piram_idade +
                     ggtitle('Média de idade por sexo e UF da prova') +
                     ylab('Média da idade') +
                     xlab('Estado') +
                     theme_bw() +
                     theme(plot.title = element_text(hjust = 0.5))

plot_piram_idade <- plot_piram_idade + 
                     scale_fill_manual(values = c('hotpink', 'dodgerblue'))

plot_piram_idade <- plot_piram_idade +
                     geom_text(aes(label = round(media, digits = 2),
                                   hjust = 0.5),
                               size = 4.5,
                               colour = 'black', 
                               fontface = 'bold')
plot_piram_idade


# Gráfico da média de Ciências Humanas e Matemática em relação a idade
# Ciencias Humanas
notas_ciencias_humanas <- enem %>% 
   filter(!is.na(NOTA_CIENCIAS_HUMANAS) & !is.na(IDADE) & IDADE > 17)
# Calculando a média
nota_ciencias_humanas_idade <- notas_ciencias_humanas %>% 
                                 group_by(IDADE) %>%
                                 summarise(media_nota_ciencias_humanas = mean(NOTA_CIENCIAS_HUMANAS))

ggplot(data = nota_ciencias_humanas_idade) +
   geom_point(aes(x = IDADE, y = media_nota_ciencias_humanas))

# Matematica
notas_matematica <- enem %>%
   filter(!is.na(NOTA_MATEMATICA) & !is.na(IDADE) & IDADE > 17)
# Calculando a média
nota_matematica_idade <- notas_matematica %>%
                           group_by(IDADE) %>%
                           summarise(media_nota_matematica = mean(NOTA_MATEMATICA))

ggplot(data = nota_matematica_idade) +
   geom_point(aes(x = IDADE, y = media_nota_matematica))

# Gerando um gráfico de pontos com as duas disciplinas
# Fazendo um merge dos dois dfs construidos
notas_ch_mat_idade <- merge(nota_ciencias_humanas_idade,
                            nota_matematica_idade,
                            all = T)
View(notas_ch_mat_idade)
# Convertendo as duas colunas de notas do df em linha
notas_ch_mat_idade <- melt(notas_ch_mat_idade,
                           id.vars = 'IDADE')
View(notas_ch_mat_idade)

# Gerando o gráfico
plot_scatter_ch_mat <- ggplot(data = notas_ch_mat_idade) +
                        geom_point(aes(IDADE, value, color = variable))
p <- plot_scatter_ch_mat +
      ggtitle('Média das notas por idade e área') + 
      xlab('Idade') +
      ylab('Média (Notas)')
p <- p + theme_bw() +
      scale_color_manual(name = 'Área', 
                         values = c('blue','red'),
                         labels = c('Ciências\nHumanas', 'Matemática'))
plot_scatter_ch_mat <- p
plot_scatter_ch_mat                  



