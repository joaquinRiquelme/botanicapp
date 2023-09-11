# Preambulo ----
# options(rsconnect.packrat = TRUE)
library(shiny)
library(magrittr)
library(bslib)

# Datos ----
# especies <- read.csv("Valdivia_especies.csv")
especies <- read.csv("Especies_DiversidadFlora.csv")
head(especies)

# Interfaz de usuario ----
ui <-page_navbar(
  theme = bs_theme( bg = "#f0f0f0", fg = "#000"),
  ## Titulo ----
  title  = tags$span(
    class = "title",
    tags$a(
      tags$img(src = "botanicapp.JPG", height = "50px", style = "margin-top: -5px")
      # href = "https://odes-chile.org"
      ),
    "Aplicación para mejorar las habilidades de reconocimiento de especies nativas en Chile"
    ),
  ## Panel lateral ----
  sidebar = sidebar( 
    width = 400,
    
    ### Familia ----
    accordion(
      open = "Familia",
      
      accordion_panel(
        "Familia", icon = icon("plant-wilt"),
        uiOutput("acordion-familia"),
        
        actionButton("abrir_btn", "Cargar fotografía", icon = icon("camera-retro")),
        
        accordion_panel(
          "Seleccione la familia", icon = icon("plant-wilt"),
          uiOutput("seleccion_familia"),
          selectInput(inputId = "respuesta_familia", label = NULL, 
                      choices=sort(unique(especies$FAMILY))),
          actionButton("respuesta_btn_fam", "Enviar respuesta", icon = icon("arrow-right")),
          textOutput("valor_output_fam")),
        actionButton("ayuda_btn_fam", "Usar una ayuda", icon = icon("searchengin")),
        textOutput("ayuda_output_fam")
        )
      ),
    ### Especie ---- 
    accordion(
      open = FALSE,
      accordion_panel(
        "Especie", icon = icon("leaf"),
        uiOutput("acordion-especie"),
        
        actionButton("abrir_btn_2", "Cargar fotografía", icon = icon("camera-retro")),
        
        accordion_panel(
          "Ingrese el nombre de la especie", icon = icon("leaf"),
          uiOutput("seleccion_especie"),
          
          textInput(inputId = "respuesta_especie", label = NULL,
                    placeholder = "Ingrese aquí su respuesta"),
          
          actionButton("respuesta_btn_esp", "Enviar respuesta", icon = icon("arrow-right")),
                         textOutput("valor_output_esp")),
        actionButton("ayuda_btn_esp", "Usar una ayuda", icon = icon("searchengin")),
        textOutput("ayuda_output_esp")
        )
      )
    )
  ,
## Panel principal ---- 
  nav_panel(
    title = "Cuestionario",
    icon  = icon("question"),
    tags$head(
      tags$link(href = "logo-botanicapp.png", rel = "icon")
      ),
    layout_column_wrap(
      width = 1,
      class = "my-3",
      imageOutput("imagen_output")
      )
    )
  
  ,
  
## Listado de especies ----
  bslib::nav_panel(
    title = "Listado de especies",
    icon  = icon("clipboard-list"),
    layout_column_wrap(
      width = 1,
      navset_card_tab(
        # height = 450,
        full_screen = TRUE,
        # title = "HTML Widgets",
        nav_panel(
          "Listado de especies",
          tableOutput("tablaEspecies")
          # includeMarkdown("md/ayuda.md")
        )
      )
    )
  )
  
)

# Servidor ----
server <- function(input, output, session) {
  valores.reactivos <- reactiveValues()
  valores.reactivos$contador <- 0
  observeEvent(input$abrir_btn | input$abrir_btn_2, {
    
    img.name <- sample(unique(especies$Especie),1,replace = FALSE)# input$imagen
    img_name <- gsub(pattern = " ", replacement = "_", x= img.name)
    print(img_name)
    print(img.name)
    espp <- img.name
    familia <- unique(especies$FAMILY[especies$Especie==espp])
    reactiveValues(a=espp)
    output$imagen_output <- renderImage({
      list(src = paste0("images/", 
                        sample(x = list.files("images/")[
                          grep(pattern = img_name,
                               x =  list.files("images/"))],
                          1,  replace = FALSE)),
           alt = "Imagen",
           # width = "80%"
           height = "150%"
           )
    }, deleteFile = FALSE)
    
    valores.reactivos$img1 <- espp
    valores.reactivos$familia1 <- familia
  })
  
  
  
  respuesta1 <- reactive({
    if(valores.reactivos$familia1 == input$respuesta_familia){
      "Correcto"}else{"Incorrecto"}
    }) %>% bindCache(input$respuesta_familia) %>% bindEvent(input$respuesta_btn_fam)  
  
  output$valor_output_fam <- renderText(respuesta1())
  
  
  ayuda1 <- reactive({
    especies[especies$Especie==valores.reactivos$img1,c("ownhint_Description_1","ownhint_Description_2")][1,sample(1:2,1,replace = FALSE)]
  }) %>% bindCache(input$abrir_btn) %>% bindEvent(input$ayuda_btn_fam)  
  
  output$ayuda_output_fam <- renderText(ayuda1())
  
  respuesta2 <- reactive({
    if(valores.reactivos$img1 == input$respuesta_especie){
      "Correcto"}else{"Incorrecto"}
  }) %>% bindCache(input$respuesta_especie) %>% bindEvent(input$respuesta_btn_esp)  
  
  output$valor_output_esp <- renderText(respuesta2())
  
  ayuda2 <- reactive({
    especies[especies$Especie==valores.reactivos$img1,c("ownhint_Description_1","ownhint_Description_2")][1,sample(1:2,1,replace = FALSE)]
  }) %>% bindCache(input$abrir_btn) %>% bindEvent(input$ayuda_btn_esp)  
  
  output$ayuda_output_esp <- renderText(ayuda2())
  
  
  output$tablaEspecies <- renderTable({
    tabla.salida <- especies[,c("TAXONNAME", "GENUS", "FAMILY")]
    names(tabla.salida) <- c("Nombre taxonómico", "Género", "Familia")
    tabla.salida
  })
  
}

shinyApp(ui, server)