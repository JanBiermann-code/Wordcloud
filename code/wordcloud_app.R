## wordcloud app
cat("\n## WordCloud app\n")

##..... Clean Environment
rm(list=ls()); cat("Renv cleaned \n")

##..... Initialization
# setwd("")
source(list.files(path = getwd(), pattern = "^init.R$", 
                  recursive = T, full.names = T, ignore.case = F))

##..... Load libraries
lib <- c(lib, fnc_lib(library = c(
  "text2vec", "ggplot2", "ggwordcloud", "data.table", "cluster", "Rtsne", 
  "shiny", "shinyjs", "shinyWidgets"
), silent = FALSE))

##..... Load functions
source(file.path(dirs$wordcloud$dir_code_wordcloud,"wordcloud_fnc.R"))

##..... get word_vectors
word_vectors_file <- file.path(dirs$wordcloud$dir_data_glove, "word_vectors.csv")
word_vectors <- fnc_get_word_vectors(word_vectors_file)

##..... get word_cloud
word_cloud_file <- file.path(dirs$wordcloud$dir_data_wordcloud_in, "WordCloud_example.csv")
fnc_word_cloud_mk(word_cloud_file, word_vectors)

##..... ui
ui <- fluidPage(
  fluidRow(
    column(width = 12,
           h3(style = "border-bottom: 2px solid #ccc; padding-bottom: 10px;", "Word Cloud"),
    ),
    column(width = 2,
           selectInput(inputId = "word_cloud_file",
                       label = "Word Cloud",
                       choices = list.files(dirs$wordcloud$dir_data_wordcloud_in, pattern = ".csv"),
                       selected = list.files(dirs$wordcloud$dir_data_wordcloud_in, pattern = ".csv")[1]),
           textInput(inputId = "add_word",
                     label = "add word",
                     value = NULL),
           tags$script(HTML(" $(document).on('keypress', function(e) { if(e.which == 13) { Shiny.onInputChange('enter_pressed', Math.random()); } }); ")),
           checkboxInput(inputId = "save_word",
                         label = "save word",
                         value = FALSE),
           selectInput(inputId = "graph",
                       label = "Graph",
                       choices = c("silhouette", "2d", "3d", "wordcloud"),
                       selected = "3d"),
           checkboxInput(inputId = "center",
                         label = "center first entry",
                         value = FALSE),
           checkboxInput(inputId = "add_unknown", 
                         label = "include unknown", 
                         value = TRUE),
    ),
    column(width = 8,
           conditionalPanel(
             condition = "input.graph == 'wordcloud'",
             plotOutput("ggplot_wordcloud")
           ),
           conditionalPanel(
             condition = "input.graph != 'wordcloud'",
             plotlyOutput("plotly_plot")
           )
    ), ## column
    column(width = 2,
           selectInput(inputId = "palette",
                       label = "Color Palette",
                       choices = c("magma", "inferno", "plasma", "viridis", "cividis", row.names(brewer.pal.info), names(wes_palettes)),
                       # choices = c(names(fnc_favcols()),"magma", "inferno", "plasma", "viridis", "cividis", row.names(brewer.pal.info), names(wes_palettes)),
                       selected = "viridis"),
           sliderInput(inputId = "clusters", label = "Clusters", min = 1, max = 30, value = 5),
           numericInput(inputId = "perplexity", label = "Perplexity", value = 3, min = 0.1, max = 30, step = 0.1),
           actionButton("save_plot", "Save Plot", style="color: #fff; background-color: #00A08A; border-color: #28a745"),
           br(),br()
    ) ## column
  )
)

##..... Server
server <- function(input, output) {
  
  add_word <- reactiveVal(NULL) 
  observeEvent(input$enter_pressed, {add_word(input$add_word)})
  
  word_cloud <- reactive({
    cat(".....","\n")
    dir <- switch(as.character(input$save_word), 
                  "TRUE" = dirs$wordcloud$dir_data_wordcloud_out, 
                  "FALSE" = dirs$wordcloud$dir_data_wordcloud_in, 
                  dirs$wordcloud$dir_data_wordcloud_in) # Default value
    file_path <- file.path(dir, input$word_cloud_file)
    word_cloud_read <- fnc_word_cloud(file_path)
    fnc_update_wordcloud(word_cloud_read, word_vectors, add_word())
  })
  
  compound_list <- reactive({
    compound <- fnc_compound_words(word_cloud(), input$word_cloud_file, word_vectors, word_vectors_file)
    word_vectors <<- compound[[2]]
    list(word_cloud_compound = compound[[1]], word_vectors_compound = compound[[2]])
  })
  
  word_cloud_select <- reactive({
    wc <- compound_list()$word_cloud_compound
    wc[wc$valid != FALSE, ]
  })
  
  word_vectors_select <- reactive({ 
    compound_list()$word_vectors_compound[tolower(word_cloud_select()$word_cloud), , drop = FALSE]
  })
  
  word_cloud_kmean <- reactive({
    fnc_kmean_cluster(word_cloud_select(), 
                      word_vectors_select(), 
                      as.numeric(input$clusters), 
                      input$palette,
                      seed)
  })
  
  tsne_result <- reactive({ 
    dim <- switch(input$graph, "2d" = 2, "3d" = 3, 2) 
    Rtsne(word_vectors_select(), dims = dim, perplexity = input$perplexity, check_duplicates = FALSE) 
  })
  
  
  word_cloud_plot <- reactive({
    fnc_word_cloud_plot(compound_list()$word_cloud_compound, word_cloud_kmean())
  })
  
  word_cloud_2d <- reactive({
    if(input$graph == "2d") {
      tmp_2d <- fnc_word_cloud_2d(word_cloud_plot(), tsne_result(), input$perplexity, seed)
      fnc_word_cloud_tune(tmp_2d, input$add_unknown, input$center)
    }
  })
  
  word_cloud_3d <- reactive({
    if(input$graph == "3d") {
      tmp_3d <- fnc_word_cloud_3d(word_cloud_plot(), tsne_result(), input$perplexity, seed)
      fnc_word_cloud_tune(tmp_3d, input$add_unknown, input$center)
    }
  })
  
  output$plotly_plot <- renderPlotly({
    if (input$graph == "silhouette") {
      fnc_explore_kmean_cluster(word_cloud_select(), word_vectors_select(), seed)[[1]]
    } else if (input$graph == "2d") {
      fnc_ply_2d(word_cloud_2d())
    } else if (input$graph == "3d") {
      fnc_ply_3d(word_cloud_3d())
    }
  })
  
  output$ggplot_wordcloud <- renderPlot({
    if (input$graph == "wordcloud") {
      word_cloud_plot <- fnc_word_cloud_tune(word_cloud_plot(), input$add_unknown, input$center)
      wordcloud_plot <- fnc_plt_wordcloud(word_cloud_plot)
      print(wordcloud_plot)
    }
  })
  
  observeEvent(input$save_plot, {
    if (input$graph == "wordcloud") {
      wordcloud_plot <- fnc_plt_wordcloud(word_cloud_plot())
      fnc_plt_save(wordcloud_plot)
    } else {
      if (input$graph == "silhouette") {
        plotly_obj <- fnc_explore_kmean_cluster(word_cloud_select(), word_vectors_select(), seed)[[1]]
        fnc_ply_save(plotly = plotly_obj, dir = dirs$wordcloud$dir_plot_wordcloud, name = "silhouette")
      } else if (input$graph == "2d") {
        plotly_obj <- fnc_ply_2d(word_cloud_2d())
        fnc_ply_save(plotly = plotly_obj, dir = dirs$wordcloud$dir_plot_wordcloud, name = "2d")
      } else if (input$graph == "3d") {
        plotly_obj <- fnc_ply_3d(word_cloud_2d())
        fnc_ply_save(plotly = plotly_obj, dir = dirs$wordcloud$dir_plot_wordcloud, name = "3d")
      }
    }
    cat("Plot saved at:", dirs$wordcloud$dir_plot_wordcloud, "\n")
  })
}

##..... Run
shinyApp(ui = ui, server = server)

##..... done

