library(shiny)
library(dplyr)
library(psych)
library(ggplot2)
library(plotly)
library(cowplot)
library(shinythemes)
library(viridis)
library(shinyjs)
library(webshot)
library(magick)
library(htmlwidgets)
library(Cairo)
library(patchwork)
library(egg)
library(grid)

Tephra.theme <- theme_bw() +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

chempalette <- c(
  "#209A24", "#A769EE", "#B4C428", "#e30303", "#000000", "#EF139D", "#3A700F",
           "#5288FF", "#E64701", "#7F9AF5", "#F36D20", "#0B3075", "#C18522", "#9634A6",
           "#1D7D46", "#80ddb8", "#7CBC92", "#B10E89", "#384B27", "#F896FD", "#C6A766",
           "#20628E", "#9C0323", "#C3AFE0", "#572D24", "#004a0d", "#ff8bea", "#fdce47",
           "#002572", "#515e00", "#840064", "#008a5d","#ff6e77","#13beff","#7b3500"
)

symbolList <- c(
  "circle", "square", "diamond", "cross", "x", "triangle-up", "triangle-down",
  "star", "hexagon", "pentagon", "octagon", "star-square", "star-diamond", "star-triangle-up",
  "star-triangle-down", "star-pentagon", "star-hexagon", "star-octagon", "diamond-cross", "diamond-x",
  "diamond-triangle-up", "diamond-triangle-down", "diamond-pentagon", "diamond-hexagon", "diamond-octagon",
  "circle", "square", "diamond", "cross", "x", "triangle-up", "triangle-down","star", "hexagon"
)

ui <- fluidPage(
  shinyjs::useShinyjs(),  # Initialize shinyjs
  theme = shinytheme("yeti"),
  titlePanel(
    div(
      img(src = "logo.png", class = "logo-image"),
      "AshplotR", windowTitle = "AshplotR",
      style = "display: flex; align-items: center; justify-content: space-between;"
    )
  ),
  sidebarLayout(
    sidebarPanel(
      fileInput("dataFile", "Upload Data (CSV format)"),
      selectInput("xVariable", "X-axis Variable:", choices = NULL),
      selectInput("yVariable", "Y-axis Variable:", choices = NULL),
      actionButton("plotButton", "Generate Plot", class = "btn-primary"),
      downloadButton("downloadHarkerPlot", "Download Harker Plot (PDF)", class = "btn-primary"),
      HTML("<div style='margin-top: 20px;'>
             <p>AshplotR shiny app is based on the code of Matthews and Pike's AshplotR r code. The shiny app provides a restricted range of plots for major element tephra chemical exploration in comparison with the main code. Users are encouraged to explore the main code if they are able, and if they use this app or the main code are encouraged to cite it. 
            </div>"),          
      HTML("<div style='margin-top: 20px;'>
      The citation is: Matthews, Ian; Pike, Joshua (2023). AshplotR. Royal Holloway, University of London. Software. <a href='https://doi.org/10.17637/rh.21941432.v1'>https://doi.org/10.17637/rh.21941432.v1</a>.</p>
           </div>"),
      absolutePanel(
        bottom = "2px", left = "10px",
        style = "font-size: 14px; font-style: italic; color: #666;",
        "app Author: Ian Matthews"
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
                 tableOutput("summaryTable")
        ),
        tabPanel("Scatter Plot", plotlyOutput("scatterPlot", height = "800px", width = "1000px")),
        tabPanel("TAS Plot", plotlyOutput("tasPlot", height = "800px", width = "1000px")),
        tabPanel("SiO2-K2O Plot", plotlyOutput("sio2k2oPlot", height = "800px", width = "1000px")),
        tabPanel("Harker Plot", plotOutput("harkerPlot")),
        tabPanel("Density Plot", plotlyOutput("densityPlot", height = "800px", width = "1000px"))
        
      )
    )
  ),
  # Add custom CSS for the logo image
  tags$head(
    tags$style(HTML("
      .logo-image {
        max-width: 60px;
        max-height: 60px;
      }
    ")))
)


server <- function(input, output, session) {
  
  data <- reactive({
    req(input$dataFile)
    read.csv(input$dataFile$datapath)
  })
  
  observe({
    if (!is.null(input$dataFile)) {
      updateSelectInput(session, "xVariable", choices = colnames(data()))
      updateSelectInput(session, "yVariable", choices = colnames(data()))
    }
  })
  
  scatterPlot <- reactive({
    if (!is.null(input$xVariable) && !is.null(input$yVariable)) {
      plot_data <- data() %>% select(all_of(input$xVariable), all_of(input$yVariable), id)
      
      p <- ggplot(plot_data, aes_string(x = input$xVariable, y = input$yVariable, fill = "id", shape = "id")) +
        geom_point(size = 4, alpha = 0.8) +
        scale_fill_manual(values = chempalette) +
        scale_shape_manual(values = symbolList) +
        Tephra.theme
      
      ggplotly(p, width = 1000, height = 800, dynamicTicks = TRUE) %>%
        config(toImageButtonOptions = list(format = "png", width = 1000, height = 800, scale = 2))
      
    }
  })
  
  scatterPlot2 <- function() {
    if (!is.null(input$xVariable) && !is.null(input$yVariable)) {
      plot_data <- data() %>% select(all_of(input$xVariable), all_of(input$yVariable), id)
      
      p <- ggplot(plot_data, aes_string(x = input$xVariable, y = input$yVariable, fill = "id", shape = "id")) +
        geom_point(size = 4, alpha = 0.8) +
        scale_fill_manual(values = chempalette) +
        scale_shape_manual(values = symbolList) +
        Tephra.theme
      
      ggplotly(p, width = 1000, height = 800, dynamicTicks = TRUE) %>%
        config(toImageButtonOptions = list(format = "png", width = 1000, height = 800, scale = 2))
    }
  }
  
  tasPlot <- reactive({
    if (!is.null(input$xVariable) && !is.null(input$yVariable)) {
      TAS_x <- (100 / data()[["Total"]]) * data()[["SiO2"]]
      TAS_y <- (100 / data()[["Total"]]) * (data()[["K2O"]] + data()[["Na2O"]])
      
      p1 <- ggplot(data(), aes(x = TAS_x, y = TAS_y, fill = id, shape = id)) +
        geom_point(aes(fill = id), size = 4, alpha = 0.8) +
        xlab("SiO2 (wt%)") +
        ylab("Na2O + K2O (wt%)") +
        scale_shape_manual(values = symbolList) +
        scale_fill_manual(values = chempalette) +
        geom_segment(x=77,y=0,xend=69,yend=8)+ 
        geom_segment(x=69, y=8,xend=69,yend=13)+
        geom_segment(x=41, y=7,xend=52.5,yend=14)+
        geom_segment(x=52.5,y=14,xend=57.6,yend=11.7)+
        geom_segment(x=45,y=0,xend=45,yend=5)+
        geom_segment(x=45,y=5,xend=63,yend=14.56)+
        geom_segment(x=52,y=0,xend=52,yend=5)+
        geom_segment(x=52,y=5,xend=63,yend=7)+
        geom_segment(x=57,y=0,xend=57,yend=5.9)+
        geom_segment(x=63,y=0,xend=63,yend=7)+
        geom_segment(x=45,y=5,xend=52,yend=5)+
        geom_segment(x=52,y=5,xend=49.4,yend=7.3)+
        geom_segment(x=57,y=5.9,xend=53,yend=9.3)+
        geom_segment(x=63,y=7,xend=57.6,yend=11.7)+
        geom_segment(x=49.4,y=7.3,xend=45,yend=9.4)+
        geom_segment(x=53,y=9.3,xend=48.4,yend=11.5)+
        geom_segment(x=41,y=0,xend=41,yend=7)+
        geom_segment(x=52.5,y=14,xend=49,yend=15.5)+
        geom_segment(x=41,y=3,xend=45,yend=3)+
        geom_segment(x=63,y=7,xend=69,yend=8)+
        scale_x_continuous(limits = c(40, 80), expand = c(0, 0)) +
        scale_y_continuous(limits = c(0, 16), expand = c(0, 0)) +
        annotate(
          "text", x = c(76, 67.5, 59, 54, 48, 42.5, 48.5, 53, 57, 63, 43, 48, 53, 57, 42),
          y = c(6, 5.2, 4, 3, 2.5, 1.5, 6, 7.5, 8.8, 11, 7, 10, 12, 14.5, 13),
          label = c("R", "O3", "O2", "O1", "B", "Pc", "S1", "S2", "S3", "T", "U1", "U2", "U3", "Ph", "F")
        ) +
        Tephra.theme
      
      ggplotly(p1, width = 1000, height = 800, dynamicTicks = TRUE) %>%
      config(toImageButtonOptions = list(format = "png", width = 1000, height = 800, scale = 2))
      
    }
  })
  
  sio2k2oPlot <- reactive({
    p47 <- ggplot(data(), aes(x = SiO2, y = K2O, fill = id, shape = id)) +
      geom_point(aes(fill = id), size = 4, alpha = 0.8) +
      xlab("SiO2 (wt%)") +
      ylab("K2O (wt%)") +
      scale_shape_manual(values = symbolList) +
      scale_fill_manual(values = chempalette) +
      geom_segment(x = 48, y = 1.6, xend = 52, yend = 2.4) +
      geom_segment(x = 52, y = 2.4, xend = 56, yend = 3.2) +
      geom_segment(x = 56, y = 3.2, xend = 63, yend = 4.0) +
      geom_segment(x = 48, y = 1.2, xend = 52, yend = 1.5) +
      geom_segment(x = 52, y = 1.5, xend = 56, yend = 1.8) +
      geom_segment(x = 56, y = 1.8, xend = 63, yend = 2.4) +
      geom_segment(x = 63, y = 2.4, xend = 70, yend = 3.0) +
      geom_segment(x = 48, y = 0.3, xend = 52, yend = 0.5) +
      geom_segment(x = 52, y = 0.5, xend = 56, yend = 0.7) +
      geom_segment(x = 56, y = 0.7, xend = 63, yend = 1.0) +
      geom_segment(x = 63, y = 1.0, xend = 70, yend = 1.3) +
      geom_segment(x = 70, y = 1.3, xend = 80, yend = 1.6) +
      Tephra.theme
    
    p47 <- ggplotly(p47, width = 1000, height = 800, dynamicTicks = TRUE) %>%
      config(toImageButtonOptions = list(format = "png", width = 1000, height = 800, scale = 2))
    
    p47
  })
  
  #### harkers ####
  
  library(cowplot)
  
  # Function to generate the Harker plot
  generateHarkerPlot <- function(aspect_ratio = 1) {
    p2 <- ggplot(data(), aes(x = SiO2, y = TiO2, fill = id, shape = id)) +
      geom_point(aes(fill = id), size = 4, alpha = 0.8) +
      xlab("SiO2 (wt%)") +
      ylab("TiO2 (wt%)") +
      scale_shape_manual(values = rep(c(21:25), length.out = n_distinct(data()$id))) +
      scale_fill_manual(values = chempalette) +
      Tephra.theme +
      theme(legend.position = "bottom", legend.direction = "horizontal")
      
      p3 <- ggplot(data(), aes(x = SiO2, y = Al2O3, fill = id, shape = id)) +
        geom_point(aes(fill = id), size = 4, alpha = 0.8) +
        xlab("SiO2 (wt%)") +
        ylab("Al2O3 (wt%)") +
        scale_shape_manual(values = rep(c(21:25), length.out = n_distinct(data()$id))) +
        scale_fill_manual(values = chempalette) +
        Tephra.theme
        
        p4 <- ggplot(data(), aes(x = SiO2, y = FeO, fill = id, shape = id)) +
          geom_point(aes(fill = id), size = 4, alpha = 0.8) +
          xlab("SiO2 (wt%)") +
          ylab("Al2O3 (wt%)") +
          scale_shape_manual(values = rep(c(21:25), length.out = n_distinct(data()$id))) +
          scale_fill_manual(values = chempalette) +
          Tephra.theme
          
          p5 <- ggplot(data(), aes(x = SiO2, y = MnO, fill = id, shape = id)) +
            geom_point(aes(fill = id), size = 4, alpha = 0.8) +
            xlab("SiO2 (wt%)") +
            ylab("Al2O3 (wt%)") +
            scale_shape_manual(values = rep(c(21:25), length.out = n_distinct(data()$id))) +
            scale_fill_manual(values = chempalette) +
            Tephra.theme
            
            p6 <- ggplot(data(), aes(x = SiO2, y = MgO, fill = id, shape = id)) +
              geom_point(aes(fill = id), size = 4, alpha = 0.8) +
              xlab("SiO2 (wt%)") +
              ylab("Al2O3 (wt%)") +
              scale_shape_manual(values = rep(c(21:25), length.out = n_distinct(data()$id))) +
              scale_fill_manual(values = chempalette) +
              Tephra.theme
              
              p7 <- ggplot(data(), aes(x = SiO2, y = CaO, fill = id, shape = id)) +
                geom_point(aes(fill = id), size = 4, alpha = 0.8) +
                xlab("SiO2 (wt%)") +
                ylab("Al2O3 (wt%)") +
                scale_shape_manual(values = rep(c(21:25), length.out = n_distinct(data()$id))) +
                scale_fill_manual(values = chempalette) +
                Tephra.theme
                
                p8 <- ggplot(data(), aes(x = SiO2, y = Na2O, fill = id, shape = id)) +
                  geom_point(aes(fill = id), size = 4, alpha = 0.8) +
                  xlab("SiO2 (wt%)") +
                  ylab("Al2O3 (wt%)") +
                  scale_shape_manual(values = rep(c(21:25), length.out = n_distinct(data()$id))) +
                  scale_fill_manual(values = chempalette) +
                  Tephra.theme
                  
                  p9 <- ggplot(data(), aes(x = SiO2, y = K2O, fill = id, shape = id)) +
                    geom_point(aes(fill = id), size = 4, alpha = 0.8) +
                    xlab("SiO2 (wt%)") +
                    ylab("Al2O3 (wt%)") +
                    scale_shape_manual(values = rep(c(21:25), length.out = n_distinct(data()$id))) +
                    scale_fill_manual(values = chempalette) +
                    Tephra.theme
                    
                    p10 <- ggplot(data(), aes(x = SiO2, y = P2O5, fill = id, shape = id)) +
                      geom_point(aes(fill = id), size = 4, alpha = 0.8) +
                      xlab("SiO2 (wt%)") +
                      ylab("Al2O3 (wt%)") +
                      scale_shape_manual(values = rep(c(21:25), length.out = n_distinct(data()$id))) +
                      scale_fill_manual(values = chempalette) +
                      Tephra.theme
                      
                    # Generate the Harker plot grid
                    harkerGrid <- plot_grid(
                      p2 + theme(legend.position = "none"),
                      p3 + theme(legend.position = "none"),
                      p4 + theme(legend.position = "none"),
                      p5 + theme(legend.position = "none"),
                      p6 + theme(legend.position = "none"),
                      p7 + theme(legend.position = "none"),
                      p8 + theme(legend.position = "none"),
                      p9 + theme(legend.position = "none"),
                      p10 + theme(legend.position = "none"),
                      labels = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"),
                      align = "b"
                    )
                    
                    # Get the legend from the main plot
                    legend <- get_legend(p2)
                    
                    # Create a plot grid with the Harker plot grid and legend
                    harkerPlot <- plot_grid(harkerGrid, legend, ncol = 1, nrow = 2, rel_heights = c(3, 0.2))
                    
                    # Add title and adjust the size
                    harkerPlot <- ggdraw(harkerPlot) + draw_label(
                      "Harker Plots",
                      x = 0, y = 1, hjust = 0, vjust = 1,
                      fontface = "bold", size = 11
                    )
                    
                    # Return the Harker plot
                    harkerPlot
  } 
  
  output$harkerPlot <- renderPlot({
    # Render the Harker plot using the generateHarkerPlot function
    harkerPlot <- generateHarkerPlot()
    
    # Render the Harker plot grid
    harkerPlot
  }, height = 1000, width = 1000)
  
  
  output$downloadHarkerPlot <- downloadHandler(
    filename = function() {
      "harkerplot.pdf"
    },
    content = function(file) {
      # Save the plot as a PDF
      pdf(file, width = 30, height = 20)
      plot <- generateHarkerPlot()
      print(plot)
      dev.off()
    }
  )
  

  

  #### new ####
  
  
  
  
  output$densityPlot <- renderPlotly({
    req(input$xVariable)
    
    # Subset the data based on selected variable
    subset_data <- data()[, c("id", input$xVariable)]
    
    # Create a density plot using ggplot2
    density_plot <- ggplot(subset_data, aes_string(x = input$xVariable)) +
      geom_density(aes(fill = factor(id)), alpha = 0.5) +
      scale_fill_manual(name = "Group", values = chempalette) +
      Tephra.theme
    
    # Convert ggplot2 object to plotly
    density_plot <- plotly::ggplotly(density_plot) %>%
      config(toImageButtonOptions = list(format = "png", width = 1000, height = 800, scale = 2))
    
    # Customize the plotly layout
    density_plot <- layout(
      density_plot,
      yaxis = list(title = ""),
      margin = list(l = 40, r = 10, t = 40, b = 40)
    )
    
    density_plot
  })
  


  output$summaryTable <- renderTable({
    summaryTable <- data() %>% group_by(id) %>% summarise(across(everything(), list(mean = mean, sd = sd), na.rm = TRUE))
    
    # Round the summary statistics to 2 decimal places
    summaryTable <- summaryTable %>% mutate(across(where(is.numeric), ~ round(., digits = 2)))
    
    summaryTable
  })
  
  ##### experimental ####
  
  
  scatter_plot <- function() {
    scatterPlot2()
  }
  

  
  # Download summary table as CSV
  output$downloadSummaryTable <- downloadHandler(
    filename = "summary_table.csv",
    content = function(file) {
      write.csv(summaryTable(), file, row.names = FALSE)
    }
  )
  
  output$scatterPlot <- renderPlotly({
    scatterPlot()
  })
  

  output$tasPlot <- renderPlotly({
    tasPlot()
  })
  
  output$sio2k2oPlot <- renderPlotly({
    sio2k2oPlot()
  })
  }

shinyApp(ui, server)

                        