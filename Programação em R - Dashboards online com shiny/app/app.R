#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(shinyWidgets)


dados <- fread('C:/Users/Admin/Documents/Programação em R - Dashboards online com shiny/dados/dados_limpos.csv')


Encoding(dados$DescricaoAssunto) <- 'UTF-8'


cabecalho <- dashboardHeader(title = 'Dashboard Procon')

barra_lateral <- dashboardSidebar(sidebarMenu(
                                    menuItem('Dashboard',
                                             tabName = 'dashboard',
                                             icon = icon('dashboard')),
                                    menuItem('Informações',
                                             tabName = 'infos',
                                             icon = icon('info-circle'))
                                  )
)

painel_principal <- dashboardBody(
    tabItems(
        tabItem(tabName = 'infos',
                h1('Informações'),
                infoBox(title = 'Contato',
                        icon = icon('envelope-square'),
                        subtitle = 'Para mais informações e/ou feedback, entre em contato:
                        email@email.com')), # Fim tabItem
        tabItem(tabName = 'dashboard',
                fluidRow(
                    valueBoxOutput(outputId = 'qtdregistros'),
                    valueBoxOutput(outputId = 'mediachamadosano'),
                    valueBoxOutput(outputId = 'qtdUF')
                ), # Fim fluidRow
                fluidRow(
                    column(width = 12,   # 1 a 12
                           box(title = 'Filtros', 
                               width = '100%',
                               column(width = 12,
                                      box(width = '100%',
                                          awesomeCheckboxGroup(inputId = 'select_uf',
                                                               label = 'Estados:',
                                                               choices = c('TODOS', unique(sort(dados$UF))),
                                                               selected = 'TODOS',
                                                               inline = TRUE)
                                      )
                               ),
                               column(width = 6,
                                      box(width = '100%',
                                          dateRangeInput(inputId = 'data_abertura',
                                                         label = 'Data Abertura Reclamação:',
                                                         format = 'dd/mm/yyyy',
                                                         start = min(as.Date(dados$DataAbertura)),
                                                         end = max(as.Date(dados$DataAbertura))))
                               ),
                               column(width = 6,
                                      box(width = '100%',
                                          selectizeInput(inputId = 'assunto',
                                                         label = 'Descrição Assunto:',
                                                         choices = c('TODOS', unique(sort(dados$DescricaoAssunto))),
                                                         multiple = T, options = list(maxItems = 5),
                                                         selected = 'TODOS')
                                      )
                               )
                           ) # Final box de filtros
                    )     
                ), # Final fluidRow
                fluidRow(
                    column(width = 12,
                           box(width = '100%',
                               plotlyOutput(outputId = 'data', width = '100%'),
                               verbatimTextOutput(outputId = 'descData')
                           )
                    )
                ), # Fim fluidRow
                fluidRow(
                    column(width = 6,
                           box(width = '100%',
                               plotlyOutput(outputId = 'atendida', width = '100%')
                           )
                    ),
                    column(width = 6,
                           box(width = '100%',
                               plotlyOutput(outputId = 'atendidaano', width = '100%')
                           )
                    )
                ), # Fim fluidRow
                fluidRow(
                    column(width = 12,
                           box(width = '100%',
                               plotlyOutput(outputId = 'uf', width = '100%')
                           )
                    )
                ) # Fim fluidRow
                ) # Fim tabItem
    ), # Fim tabItems
    
)

ui <- dashboardPage(header = cabecalho,
                    sidebar = barra_lateral,
                    body = painel_principal
)

# Define UI for application that draws a histogram
ui2 <- fluidPage(

    # Application title
    titlePanel("Procon"),

    # Sidebar with a slider input
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput(inputId = 'select_uf',
                               label = 'Estados',
                               choices = c('TODOS', unique(sort(dados$UF))),
                               selected = 'TODOS'),
            dateRangeInput(inputId = 'data_abertura',
                           label = 'Data Abertura Reclamação:',
                           format = 'dd/mm/yyyy',
                           start = min(as.Date(dados$DataAbertura)),
                           end = max(as.Date(dados$DataAbertura))),
            selectizeInput(inputId = 'assunto',
                           label = 'Descrição Assunto:',
                           choices = c('TODOS', unique(sort(dados$DescricaoAssunto))),
                           multiple = T, options = list(maxItems = 5),
                           selected = 'TODOS')
        ),

        
        mainPanel(
           plotlyOutput(outputId = 'data'),
           verbatimTextOutput(outputId = 'descData'),
           plotlyOutput(outputId = 'uf'),
           textOutput(outputId = 'descUf'),
           plotlyOutput(outputId = 'atendida'),
           plotlyOutput(outputId = 'atendidaano')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    dados_selecionados <- reactive({
        # filtro uf
        print(input)
        if (!'TODOS' %in% input$select_uf){
            dados <- dados %>% filter(UF %in% input$select_uf)
        }
        # filtro descricao assunto
        if (!'TODOS' %in% input$assunto){
            dados <- dados %>% filter(DescricaoAssunto %in% input$assunto)
        }
        # filtro data
        dados <- dados %>% filter(as.Date(DataAbertura) >= input$data_abertura[1] & 
                                      as.Date(DataAbertura) <= input$data_abertura[2])
        dados
        
    })
    
    output$data <- renderPlotly({
        ggplotly(
            data.frame(table(as.Date(dados_selecionados()$DataAbertura))) %>%
            rename(Data = Var1, Qtd = Freq) %>%
            ggplot(aes(as.Date(Data), Qtd)) +
            geom_line(group = 1) +
            theme_bw() +
            xlab('Data') +
            ylab('Quantidade') +
            ggtitle('Quantidade de reclamações abertas por ano-mês') +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            scale_x_date(date_labels = '%b-%Y', breaks = '6 months')
            )
    })
    
    output$uf <- renderPlotly({
        ggplotly(
            data.frame(table(dados_selecionados()$UF)) %>% 
            rename(UF = Var1, Qtd = Freq) %>%
            ggplot(aes(x = reorder(UF, Qtd), y = Qtd, 
                       text = paste('UF:', UF, '<br>', 'Qtd:', Qtd))) +
            geom_bar(fill = 'blue', stat = 'identity') +
            coord_flip() +
            xlab('UF') +
            ylab('Quantidade') +
            theme_bw() +
            ggtitle('Quantidade de reclamações por UF'),
            tooltip = 'text'
        )
    })
    
    output$atendida <- renderPlotly({
        ggplotly(
            ggplot(dados_selecionados()) +
            geom_bar(aes(Atendida), fill = c('red','green'), stat = 'count') +
            ylab('Quantidade') +
            theme_bw() +
            ggtitle('Quantidade de solicitações atendidas ou não')
        )
    })
    
    output$atendidaano <- renderPlotly({
        ggplotly(
            data.frame(table(dados_selecionados()$anocalendario, dados_selecionados()$Atendida)) %>%
            rename(Ano = Var1, Atendida = Var2, Qtd = Freq) %>%
            ggplot() +
            geom_bar(aes(x = Ano, y = Qtd, fill = Atendida), 
                     stat = 'identity', 
                     position = position_dodge2()) +
            ggtitle('Reclamações atendidas ou não por ano arquivamento') +
            theme_bw() +
            theme(plot.title = element_text(size=12)) +
            ylab('Quantidade') 
        )
    })
    
    output$descData <- renderText({
        paste("Gráfico com a quantidade de reclamações feitas entre: ",
              min(dados_selecionados()$DataAbertura), '-',
              max(dados_selecionados()$DataAbertura))
    })
    
    output$descUf <- renderText({
        estados <- paste(unique(dados_selecionados()$UF),
                         collapse = ', ')
        paste('Gráfico com a quantidade de reclamações feitas pelas UFs: ', estados)
    })
    
    output$qtdregistros <- renderValueBox({
        valueBox(subtitle = 'Registros', 
                 value = nrow(dados_selecionados()), 
                 icon = icon('database'))
    })
    
    output$mediachamadosano <- renderValueBox({
        valueBox(subtitle = 'Média de Reclamações por Ano',
                 value = dados_selecionados() %>% 
                             group_by(anocalendario) %>%
                             summarise(qtd_chamados = n()) %>%
                             summarise(media_chamados_ano = mean(qtd_chamados)) %>%
                             as.numeric(),
                 icon = icon('list'))
    })
    
    output$qtdUF <- renderValueBox({
        valueBox(value = length(unique(dados_selecionados()$UF)),
                 subtitle = 'Quantidade de UFs selecionadas',
                 icon = icon('map-marker'))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)