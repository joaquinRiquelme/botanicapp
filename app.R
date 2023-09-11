library(shiny)
library(magrittr)


# especies <- read.csv("Valdivia_especies.csv")
especies <- read.csv("Especies_DiversidadFlora.csv")
head(especies)

ui <- navbarPage(
  
  title  = tags$span(
    class = "title",
    tags$a(
      tags$img(src = "botanicapp.JPG", height = "50px", style = "margin-top: -5px"),
      href = "https://odes-chile.org"
      ),
    "Aplicación para mejorar las habilidades de reconocimiento de especies nativas en Chile"
    ),
  
  tabPanel("Quiz",
           fluidPage(
             # titlePanel("Aplicación para reconocimiento de especies vegetales"),
             sidebarLayout(
               sidebarPanel(
                 actionButton("abrir_btn", "Cargar nueva especie", icon = icon("leaf")),
                 textInput(inputId = "respuesta", label = NULL, placeholder = "Ingrese aquí su respuesta"),
                 actionButton("respuesta_btn", "Enviar respuesta", icon = icon("arrow-right")),
                 textOutput("valor_output"),
                 actionButton("ayuda_btn", "Usar una ayuda", icon = icon("searchengin")),
                 textOutput("ayuda_output")
                 ),
               mainPanel(
                 imageOutput("imagen_output"))))),
  tabPanel("Resultados", 
           fluidPage(
             # Contenido de la pestaña 2 aquí
             h1("Resultados aqui")
             )
           )
)




server <- function(input, output, session) {
  valores.reactivos <- reactiveValues()
  valores.reactivos$contador <- 0
  observeEvent(input$abrir_btn , {
    
    img.name <- sample(unique(especies$Especie),1,replace = FALSE)# input$imagen
    img_name <- gsub(pattern = " ", replacement = "_", x= img.name)
    print(img_name)
    print(img.name)
    espp <- img.name
    reactiveValues(a=espp)
    output$imagen_output <- renderImage({
      list(src = paste0("images/", 
                        sample(x = list.files("images/")[
                          grep(pattern = img_name,
                               x =  list.files("images/"))],
                          1,  replace = FALSE)),
           alt = "Imagen",
           width = "80%")
    }, deleteFile = FALSE)
    
    valores.reactivos$img1 <- espp
    
  })
  
  
  
  r <- reactive({
    if(valores.reactivos$img1 == input$respuesta){
      "Correcto"}else{"Incorrecto"}
    }) %>% bindCache(input$respuesta) %>% bindEvent(input$respuesta_btn)  
  
  output$valor_output <- renderText(r())
  
  
  ayuda.i <- reactive({
    especies[especies$Especie==valores.reactivos$img1,c("ownhint_Description_1","ownhint_Description_2")][1,sample(1:2,1,replace = FALSE)]
  }) %>% bindCache(input$abrir_btn) %>% bindEvent(input$ayuda_btn)  
  
  output$ayuda_output <- renderText(ayuda.i())
  
  # observeEvent(input$respuesta_btn , {
    
    # },
    
    # valores.reactivos$img1 <- espp
    
  # })
  
  
  
}

shinyApp(ui, server)