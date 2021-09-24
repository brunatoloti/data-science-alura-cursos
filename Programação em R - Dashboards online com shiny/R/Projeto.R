# PROCON

# Importando as bibliotecas
library(data.table)
library(dplyr)
library(plotly)


# Carregando a base de dados
# Base de dados fornecida pelo curso
getwd()
setwd('C:/Users/Admin/Documents/Programação em R - Dashboards online com shiny')

dados <- fread('dados/reclamacao.csv', encoding = 'UTF-8')

# Verificando o resumo da base de dados
summary(dados)

# Removendo as colunas x.1 e v1 da base de dados
reclamacao <- dados %>%
                select(-X.1, -V1)

# Verificando os valores unicos da coluna regiao
unique(reclamacao$regiao)

# Removendo os registros cuja regiao foi declarada como N/D, pois a coluna regiao
# sera importante e deve estar com os valores declarados
reclamacao <- reclamacao %>%
                filter(regiao != 'N/D')
unique(reclamacao$regiao)

# Verificando os valores unicos da coluna atendida
unique(reclamacao$Atendida)

# Fazendo uma limpeza/transformacao nos dados dessa coluna, de acordo com a
# documentacao fornecida
reclamacao$Atendida <- gsub(pattern = 'S|Siim', 
                            replacement = 'sim',
                            x = reclamacao$Atendida)
reclamacao$Atendida <- gsub(pattern = 'N|nAoo|nao', 
                            replacement = 'não',
                            x = reclamacao$Atendida)
unique(reclamacao$Atendida)
# Eliminando os registros cuja coluna Atendida possui o valor ""
reclamacao <- reclamacao %>%
                filter(Atendida != '')
unique(reclamacao$Atendida)

# Verificando os valores unicos da coluna sexoconsumidor
unique(reclamacao$SexoConsumidor)

# Fazendo uma limpeza/transformacao nos dados dessa coluna, de acordo com a 
# documentacao fornecida
reclamacao$SexoConsumidor <- gsub(pattern = 'N|NULL',
                                  replacement = 'N/I',
                                  x = reclamacao$SexoConsumidor)
unique(reclamacao$SexoConsumidor)

# Salvando a nova tabela criada
fwrite(reclamacao, 'dados/dados_limpos.csv', row.names = F)


# Elaborando os gráficos que serão usados no app shiny

# Gráfico com a quantidade de solicitações atendidas e não atendidas
grafico_atendida <- ggplot(reclamacao) +
                      geom_bar(aes(Atendida), fill = c('red','green'), stat = 'count') +
                      ylab('Quantidade') +
                      theme_bw() +
                      ggtitle('Quantidade de solicitações atendidas e não atendidas')
grafico_atendida <- ggplotly(grafico_atendida) # Adicionando interatividade com plotly
grafico_atendida

# Quantidade de reclamações registradas por UF
grafico_uf <- data.frame(table(reclamacao$UF)) %>% 
                rename(UF = Var1, Qtd = Freq) %>%
                ggplot(aes(x = reorder(UF, Qtd), y = Qtd, 
                           text = paste('UF:', UF, '<br>', 'Qtd:', Qtd))) +
                geom_bar(fill = 'blue', stat = 'identity') +
                coord_flip() +
                xlab('UF') +
                ylab('Quantidade') +
                theme_bw() +
                ggtitle('Quantidade de reclamações por UF')
grafico_uf <- ggplotly(grafico_uf, tooltip = 'text')
grafico_uf

# Gráfico da quantidade de reclamações por data (Ano - Mês - Dia)
grafico_data <- data.frame(table(as.Date(reclamacao$DataArquivamento))) %>%
                  rename(Data = Var1, Qtd = Freq) %>%
                  ggplot(aes(as.Date(Data), Qtd)) +
                  geom_line(group = 1) +
                  theme_bw() +
                  xlab('Data') +
                  ylab('Quantidade') +
                  ggtitle('Quantidade de reclamações por ano-mês') +
                  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                  scale_x_date(date_labels = '%b-%Y', breaks = '6 months')
grafico_data <- ggplotly(grafico_data)
grafico_data

# Quantidade de reclamações atendidas ou não atendidas por ano
grafico_atendida_ano <- data.frame(table(reclamacao$anocalendario, reclamacao$Atendida)) %>%
                          rename(Ano = Var1, Atendida = Var2, Qtd = Freq) %>%
                          ggplot() +
                          geom_bar(aes(x = Ano, y = Qtd, fill = Atendida), 
                                   stat = 'identity', 
                                   position = position_dodge2()) +
                          theme_bw() +
                          ylab('Quantidade') +
                          ggtitle('Quantidade de reclamações atendidas e não atendidas por ano')
grafico_atendida_ano <- ggplotly(grafico_atendida_ano)
grafico_atendida_ano

# OBS.: Os gráficos quando foram pro app podem ter sofrido algumas modificações.