library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(DT)
library(scales)

# DATOS INICIALES
datos <- data.frame(
  Producto = c("Cargador", "Audífonos", "Protector de pantalla", "Memorias USB", "Micrófonos",
               "Parlantes", "Reloj inteligente", "Aro de luz"),
  Costo = c(30000, 20000, 5500, 19500, 47000, 18000, 35000, 25000),
  Precio = c(55000, 30000, 10000, 26000, 65000, 35000, 50000, 45000),
  Unidades = c(5, 15, 20, 6, 10, 25, 25, 50)
)

cop <- function(x) {
  paste0("$", format(round(x), big.mark = ".", scientific = FALSE))
}

# INTERFAZ
ui <- navbarPage(
  "Emprende Inteligente",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  tabPanel(
    "Panel principal",
    fluidPage(
      br(),
      h2("Panel financiero"),
      p("Analiza las ganancias y los costos de tu negocio."),
      
      fluidRow(
        column(3, wellPanel(
          h4("Ingresos"),
          textOutput("ingresos")
        )),
        column(3, wellPanel(
          h4("Costos variables"),
          textOutput("costos")
        )),
        column(3, wellPanel(
          h4("Utilidad neta"),
          textOutput("utilidad")
        )),
        column(3, wellPanel(
          h4("Margen neto"),
          textOutput("margen")
        ))
      ),
      
      sidebarLayout(
        sidebarPanel(
          h4("Configuración"),
          numericInput(
            "fijos", "Costos fijos mensuales (COP)",
            500000, min = 0
          ),
          numericInput(
            "dias", "Días de operación al mes",
            30, min = 1, max = 31
          ),
          hr(),
          h4("Punto de equilibrio"),
          textOutput("equilibrio")
        ),
        mainPanel(
          h4("Ingresos, costos y utilidad"),
          plotOutput("grafico_finanzas"),
          br(),
          h4("Rentabilidad por producto"),
          plotOutput("grafico_productos")
        )
      ),
      
      h4("Detalle financiero"),
      DTOutput("tabla_resumen")
    )
  ),
  
  tabPanel(
    "Productos",
    fluidPage(
      br(),
      h2("Gestión de productos"),
      p("Edita costos, precios y cantidades en la tabla."),
      actionButton("agregar", "Agregar producto"),
      actionButton("eliminar", "Eliminar seleccionados"),
      br(), br(),
      DTOutput("tabla_productos"),
      br(),
      downloadButton("descargar", "Descargar CSV")
    )
  ),
  
  tabPanel(
    "Simulador",
    fluidPage(
      br(),
      h2("Simulador de precios"),
      sidebarLayout(
        sidebarPanel(
          numericInput("sim_costo", "Costo unitario (COP)", 3000, min = 0),
          sliderInput("sim_margen", "Margen deseado (%)", 1, 80, 30),
          numericInput("sim_unidades", "Unidades vendidas al mes", 100, min = 1),
          numericInput("sim_fijos", "Costos fijos asignados (COP)", 0, min = 0)
        ),
        mainPanel(
          h3("Precio recomendado"),
          textOutput("precio_recomendado"),
          h3("Utilidad estimada"),
          textOutput("utilidad_simulada"),
          plotOutput("grafico_simulador")
        )
      )
    )
  ),
  
  tabPanel(
    "Acerca de",
    fluidPage(
      br(),
      h2("Emprende Inteligente"),
      p("Herramienta para pequeños negocios y emprendedores."),
      p("Calcula ingresos, costos variables, utilidad, márgenes y punto de equilibrio."),
      p("Los datos de ejemplo son ficticios. Los resultados no sustituyen una asesoría contable.")
    )
  )
)

# SERVIDOR
server <- function(input, output, session) {
  
  productos <- reactiveVal(datos)
  
  resumen <- reactive({
    df <- productos()
    df$Ingresos <- df$Precio * df$Unidades
    df$Costo_total <- df$Costo * df$Unidades
    df$Utilidad_bruta <- df$Ingresos - df$Costo_total
    
    ingresos <- sum(df$Ingresos)
    costos <- sum(df$Costo_total)
    utilidad_bruta <- ingresos - costos
    fijos <- max(0, input$fijos)
    utilidad_neta <- utilidad_bruta - fijos
    margen <- if (ingresos > 0) utilidad_neta / ingresos * 100 else 0
    
    contribucion <- if (sum(df$Unidades) > 0) {
      utilidad_bruta / sum(df$Unidades)
    } else {
      0
    }
    
    equilibrio <- if (contribucion > 0) {
      ceiling(fijos / contribucion)
    } else {
      NA_real_
    }
    
    list(
      tabla = df,
      ingresos = ingresos,
      costos = costos,
      utilidad = utilidad_neta,
      margen = margen,
      equilibrio = equilibrio
    )
  })
  
  output$ingresos <- renderText(cop(resumen()$ingresos))
  output$costos <- renderText(cop(resumen()$costos))
  output$utilidad <- renderText(cop(resumen()$utilidad))
  output$margen <- renderText(
    paste0(round(resumen()$margen, 2), "%")
  )
  
  output$equilibrio <- renderText({
    e <- resumen()$equilibrio
    if (is.na(e)) "No calculable con el margen actual" else {
      paste(e, "unidades mensuales")
    }
  })
  
  output$grafico_finanzas <- renderPlot({
    r <- resumen()
    df <- data.frame(
      Concepto = c("Ingresos", "Costos variables", "Costos fijos", "Utilidad neta"),
      Valor = c(r$ingresos, r$costos, input$fijos, r$utilidad)
    )
    
    ggplot(df, aes(Concepto, Valor, fill = Concepto)) +
      geom_col(show.legend = FALSE) +
      scale_y_continuous(labels = label_number(prefix = "$", big.mark = ".")) +
      scale_fill_brewer(palette = "Set2") +
      labs(x = NULL, y = "COP") +
      theme_minimal()
  })
  
  output$grafico_productos <- renderPlot({
    df <- resumen()$tabla
    
    ggplot(df, aes(
      x = reorder(Producto, Utilidad_bruta),
      y = Utilidad_bruta,
      fill = Utilidad_bruta > 0
    )) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = "#198754", "FALSE" = "#dc3545")) +
      scale_y_continuous(labels = label_number(prefix = "$", big.mark = ".")) +
      labs(x = NULL, y = "Utilidad bruta (COP)") +
      theme_minimal()
  })
  
  output$tabla_resumen <- renderDT({
    datatable(resumen()$tabla, rownames = FALSE, options = list(pageLength = 5))
  })
  
  output$tabla_productos <- renderDT({
    datatable(
      productos(),
      rownames = FALSE,
      selection = "multiple",
      editable = TRUE,
      options = list(pageLength = 10)
    )
  })
  
  observeEvent(input$tabla_productos_cell_edit, {
    info <- input$tabla_productos_cell_edit
    df <- productos()
    fila <- info$row
    columna <- info$col + 1
    
    if (fila <= nrow(df) && columna <= ncol(df)) {
      valor <- suppressWarnings(as.numeric(info$value))
      
      if (columna == 1) {
        df$Producto[fila] <- as.character(info$value)
      } else if (!is.na(valor) && is.finite(valor) && valor >= 0) {
        if (columna != 4 || valor == floor(valor)) {
          df[fila, columna] <- valor
        }
      }
      
      productos(df)
    }
  })
  
  observeEvent(input$agregar, {
    showModal(modalDialog(
      title = "Nuevo producto",
      textInput("nombre_nuevo", "Nombre"),
      numericInput("costo_nuevo", "Costo unitario", 1000, min = 0),
      numericInput("precio_nuevo", "Precio de venta", 1500, min = 0),
      numericInput("unidades_nuevas", "Unidades vendidas", 10, min = 0),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("guardar_nuevo", "Guardar", class = "btn-success")
      )
    ))
  })
  
  observeEvent(input$guardar_nuevo, {
    nombre <- trimws(input$nombre_nuevo)
    valores <- c(input$costo_nuevo, input$precio_nuevo, input$unidades_nuevas)
    
    if (
      !nzchar(nombre) ||
      any(!is.finite(valores)) ||
      any(valores < 0) ||
      valores[3] != floor(valores[3])
    ) {
      showNotification("Verifica los datos del producto.", type = "error")
      return()
    }
    
    nuevo <- data.frame(
      Producto = nombre,
      Costo = valores[1],
      Precio = valores[2],
      Unidades = valores[3]
    )
    
    productos(bind_rows(productos(), nuevo))
    removeModal()
  })
  
  observeEvent(input$eliminar, {
    filas <- input$tabla_productos_rows_selected
    
    if (length(filas) == 0) {
      showNotification("Selecciona productos para eliminar.", type = "warning")
      return()
    }
    
    productos(productos()[-filas, , drop = FALSE])
  })
  
  output$descargar <- downloadHandler(
    filename = function() {
      paste0("reporte_negocio_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(resumen()$tabla, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  simulacion <- reactive({
    costo <- max(0, input$sim_costo)
    unidades <- max(1, input$sim_unidades)
    margen <- input$sim_margen / 100
    fijos <- max(0, input$sim_fijos)
    
    precio <- ceiling((costo + fijos / unidades) / (1 - margen))
    utilidad <- (precio - costo) * unidades - fijos
    
    list(
      precio = precio,
      utilidad = utilidad,
      costo = costo,
      unidades = unidades,
      fijos = fijos
    )
  })
  
  output$precio_recomendado <- renderText({
    cop(simulacion()$precio)
  })
  
  output$utilidad_simulada <- renderText({
    cop(simulacion()$utilidad)
  })
  
  output$grafico_simulador <- renderPlot({
    s <- simulacion()
    precios <- seq(
      max(0, s$costo),
      max(s$precio * 1.4, s$costo + 100),
      length.out = 100
    )
    
    df <- data.frame(
      Precio = precios,
      Utilidad = (precios - s$costo) * s$unidades - s$fijos
    )
    
    ggplot(df, aes(Precio, Utilidad)) +
      geom_line(color = "#176B57", linewidth = 1.2) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      geom_vline(xintercept = s$precio, linetype = "dotted", color = "blue") +
      scale_x_continuous(labels = label_number(prefix = "$", big.mark = ".")) +
      scale_y_continuous(labels = label_number(prefix = "$", big.mark = ".")) +
      labs(x = "Precio unitario (COP)", y = "Utilidad mensual (COP)") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)

