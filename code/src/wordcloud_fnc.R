## wordcloud functions
cat("## functions_wordcloud\n")

##.......... seed 
## setting the seed for initialization of k-means cluster centers
seed <- 42
set.seed(seed)

##.......... word_vector 
## GloVe embeddings (pretrained model)

##..... fnc_read_word_vector
fnc_read_word_vector <- function() {
  start_time <- Sys.time()
  ## url & data
  url_glove <- "http://nlp.stanford.edu/data/glove.6B.zip"
  zip_glove <- file.path(dirs$wordcloud$dir_data_glove, "glove.6B.zip")
  file_glove <- file.path(dirs$wordcloud$dir_data_glove, "glove.6B.100d.txt")
  
  ## download
  if (!file.exists(zip_glove)) {
    cat("downloading", blue("GloVe"), "\n")
    options(timeout = 300) ## timeout [seconds]
    download.file(url_glove, destfile = zip_glove)
  }
  
  ## unzip
  if (!file.exists(file_glove)) {
    cat("unzip", blue("GloVe"), "\n")
    unzip(zip_glove, exdir = dirs$wordcloud$dir_data_glove)
  }
  
  ## read 
  cat("read", blue("word_vectors"), "\n")
  df_glove <- fread(file_glove, header = FALSE, quote = "") 
  setnames(df_glove, c("word", paste0("V", 1:100)))
  
  ## word vectors GloVe
  word_vectors <- as.matrix(df_glove[, -1, with = FALSE]) 
  rownames(word_vectors) <- df_glove$word
  print(start_time - Sys.time())
  return(word_vectors)
} ## fnc_read_word_vector

##..... fnc_save_word_vectors 
fnc_save_word_vectors <- function(word_vectors, word_vectors_file){
  start_time <- Sys.time()
  cat("save", blue("word_vectors"), "\n")
  word_vectors_df <- as.data.frame(word_vectors) ## Convert matrix to data frame 
  word_vectors_df$word <- rownames(word_vectors) ## add row names as a column 
  fwrite(word_vectors_df, file = word_vectors_file) 
  print(start_time - Sys.time())
} ## fnc_save_word_vectors

##..... fnc_load_word_vectors
fnc_load_word_vectors <- function(word_vectors_file){
  start_time <- Sys.time()
  cat("load", blue("word_vectors"), "\n")
  word_vectors_df <- fread(word_vectors_file) 
  word_vectors <- as.matrix(word_vectors_df[, -"word", with = FALSE]) ## Convert back to matrix
  rownames(word_vectors) <- word_vectors_df$word ## restore row names 
  print(start_time - Sys.time())
  return(word_vectors)
} ## fnc_load_word_vectors

##..... fnc_get_word_vectors
fnc_get_word_vectors <- function(word_vectors_file){
  if (file.exists(word_vectors_file)) {
    ## load word_vector
    word_vectors <- fnc_load_word_vectors(word_vectors_file)
  } else {
    ## read word_vector
    word_vectors <- fnc_read_word_vector()
  }
  return(word_vectors)
} ## fnc_get_word_vectors

##..... fnc_word_cloud_mk
fnc_word_cloud_mk <- function(word_cloud_file, word_vectors_read){
  
  if(!file.exists(word_cloud_file)) {
    
    cat(yellow("no file available"), "-->", blue("making WordCloud_example.csv"), "\n")
    
    ## make WordCloud_example
    words <- rownames(word_vectors_read)
    words_only <- words[!grepl("[0-9]", words)]
    WordCloud_example <- data.frame(word_cloud = sample(words_only,20),
                                    freq = 1, ## font size
                                    valid = NA,
                                    color = NA)
    
    ## write csv
    write.csv(WordCloud_example, word_cloud_file, row.names = F)
  }
} ## fnc_word_cloud_mk

##..... fnc_word_cloud
fnc_word_cloud <- function(word_cloud_file){
  
  ## read
  cat("read", blue("word_cloud"), "\n")
  cat("make your own wordcloud files - similar to the example provided!", "\n")
  word_cloud_read <- read.csv(word_cloud_file, 
                              header = TRUE)
  return(word_cloud_read)
} ## fnc_word_cloud

##.......... word_cloud & word_vectors

##..... fnc_update_wordcloud
fnc_update_wordcloud <- function(word_cloud, word_vectors, add_word) {
  
  cat("update", blue("word_cloud"), "\n")
  
  ## remove dublicates
  word_cloud <- word_cloud %>% distinct(word_cloud, .keep_all = TRUE)
  
  ## add_word
  if (!is.null(add_word)) {
    if (nchar(add_word) > 0) {
      df_add_word <- word_cloud[nrow(word_cloud),]
      df_add_word$word_cloud[1] <- add_word
      df_add_word$freq[1] <- 2
      word_cloud <- rbind(df_add_word, word_cloud)
      rownames(word_cloud) <- word_cloud$index
    }
  }
  
  ## correct words
  # word_cloud$word_cloud[1] <- "asc\u00E8te" ## first / center
  
  ## add valid: word_cloud in word_vector
  word_cloud$valid <- tolower(word_cloud$word_cloud) %in% rownames(word_vectors)
  
  ## add color
  word_cloud$color <-    "#404040" ## unknown
  word_cloud$color[1] <- "#000000" ## first / center
  
  ## add freq
  if (is.null(word_cloud[["freq"]])) {
    word_cloud$freq <- sample(1:1, nrow(word_cloud), replace = TRUE)
    word_cloud$freq[1] <- 1
  }

  return(word_cloud)
} ## fnc_update_wordcloud

##..... fnc_compound_words
fnc_compound_words <- function(word_cloud, wordcloud_file, word_vectors, word_vectors_file) {
  cat("add", blue("compound_words"), "\n")
  compound_words <- word_cloud$word_cloud[word_cloud$valid == FALSE]
  if (length(compound_words) == 0) {
    cat(blue("all"), "words found in vocabulary \n")
  } else {
    cat(yellow(length(compound_words)), "word(s) not found in vocabulary \n")
    for (icompound_word in seq(compound_words)) {
      compound_word <- tolower(compound_words[icompound_word])
      # cat(compound_word, " [",icompound_word, "/", yellow(length(compound_words)),"]", sep = "")
      
      # Force the compound word to lowercase
      compound_word <- tolower(compound_word)
      
      # Split the compound word into individual words
      words_1 <- unlist(strsplit(compound_word, "-"))
      words_2 <- unlist(strsplit(compound_word, " "))
      words <- if (length(words_1) > length(words_2)) {words_1} else {words_2}
      
      ## Initialize a matrix to store the embeddings
      compound_word_vectors <- matrix(NA, nrow = length(words), ncol = ncol(word_vectors))
      rownames(compound_word_vectors) <- words
      
      ## Retrieve the embeddings for each word
      for (iword in seq_along(words)) {
        word <- words[iword]
        if (word %in% rownames(word_vectors)) {
          compound_word_vectors[word, ] <- word_vectors[word, ]
        }
      }
      
      ## Remove NA rows (for words not found)
      compound_word_vectors <- compound_word_vectors[complete.cases(compound_word_vectors), ]
      
      ## update word_cloud
      if (nrow(compound_word_vectors) == 0) {
        cat(compound_word, " [",red(icompound_word), "/", yellow(length(compound_words)),"]", "\n", sep = "")
        # cat(" FALSE \n")
        word_cloud$valid[which(compound_word == tolower(word_cloud$word_cloud))] <- "FALSE"
        next
      } else if (nrow(compound_word_vectors) < length(words)) {
        cat(compound_word, " [",magenta(icompound_word), "/", yellow(length(compound_words)),"]", "\n", sep = "")
        # cat(" partial \n")
        word_cloud$valid[which(compound_word == tolower(word_cloud$word_cloud))] <- "partial"
      } else {
        cat(compound_word, " [",blue(icompound_word), "/", yellow(length(compound_words)),"]", "\n", sep = "")
        # cat(" TRUE \n")
        word_cloud$valid[which(compound_word == tolower(word_cloud$word_cloud))] <- "TRUE"
      }
      
      ## Compute the average embedding
      average_embedding <- colMeans(compound_word_vectors, na.rm = TRUE)
      average_embedding <- matrix(average_embedding, nrow = 1) 
      rownames(average_embedding) <- compound_word 
      colnames(average_embedding) <- colnames(word_vectors)
      
      ## update word_vectors
      if (!is.null(average_embedding)) {
        word_vectors <- rbind(word_vectors, average_embedding)
      } 
    } ## compound_words
  } ## length(compound_words)
  
  ## print
  unknown_words <- length(word_cloud$word_cloud[word_cloud$valid == FALSE])
  new_words <- length(compound_words) - unknown_words
  if (new_words > 0) {cat(blue(new_words), "new word(s) added to word_vectors \n")}
  if (unknown_words > 0) {cat(red(unknown_words), "unknown word(s) remain(s) \n")}
  
  ## save word_vector
  if (new_words > 0) {
    fnc_save_word_vectors(word_vectors, word_vectors_file)
  }
  
  ## save word_cloud
  cat("save", blue("word_cloud"), "\n")
  write.csv(word_cloud, file.path(dirs$wordcloud$dir_data_wordcloud_out, wordcloud_file), row.names = F)
  
  return(list(word_cloud, word_vectors))
} ## fnc_compound_words

##.......... clusters

##..... fnc_norm
fnc_norm <- function(vector){
  vector_norm <- (vector - min(vector)) / (max(vector) - min(vector)) 
  return(vector_norm)
}

##..... fnc_explore_kmean_cluster
fnc_explore_kmean_cluster <- function(word_cloud_select, word_vectors_select, seed) {
  
  cat("plot", blue("silhouette"), "\n")
  clusters <- nrow(word_cloud_select)-1
  
  ##... silhouette
  sil_width <- numeric(clusters)
  steps <- seq(2,clusters,0.1)
  for (i in steps) {
    set.seed(seed) ## setting the seed for initialization of k-means cluster centers
    kmeans_result <- kmeans(word_vectors_select, centers = i)
    sil <- silhouette(kmeans_result$cluster, dist(word_vectors_select))
    sil_width[i] <- mean(sil[, 3]) # Use the third column for silhouette widths
  }
  
  ## sil_width norm 
  sil_width_norm <- fnc_norm(sil_width)
  
  scatter_cluster <- plot_ly()
  ## plotly Silhouette Widths
  color <- "black"
  scatter_cluster <- scatter_cluster %>% 
    add_trace(
      x = seq_along(sil_width), 
      y = sil_width, 
      name = "Silhouette Width", 
      line = list(color = color), marker = list(color = color),
      yaxis = 'y', type = 'scatter', mode = 'lines+markers')
  
  ##... wcss 
  ## Within-cluster sum of squares measures the compactness of the clustering
  wcss <- c()
  for (i in 1:clusters) {
    kmeans_result <- kmeans(word_vectors_select, centers = i)
    wcss[i] <- sum(kmeans_result$tot.withinss)
  }
  
  ## Normalize delta WCSS 
  wcss_norm <- fnc_norm(wcss)
  
  ## plotly wcss
  color <- "red"
  scatter_cluster <- scatter_cluster %>% 
    add_trace(
      x = seq_along(sil_width), 
      y = wcss_norm, 
      name = "wcss norm", 
      line = list(color = color), marker = list(color = color),
      yaxis = 'y', type = 'scatter', mode = 'lines+markers')
  
  ##... delta wcss
  delta_wcss <- diff(wcss)
  
  ## Normalize delta WCSS 
  delta_wcss_norm <- fnc_norm(delta_wcss)
  
  ## plotly delta wcss
  color <- "purple"
  scatter_cluster <- scatter_cluster %>% 
    add_trace(
      x = seq_len(length(sil_width) - 1), 
      y = delta_wcss_norm, 
      name = "delta wcss norm", 
      line = list(color = color), marker = list(color = color),
      yaxis = 'y', type = 'scatter', mode = 'lines+markers')
  
  ##... delta delta wcss
  delta_delta_wcss <- diff(delta_wcss)
  
  ## Normalize delta WCSS
  delta_delta_wcss_norm <- fnc_norm(delta_delta_wcss)
  
  ## plotly delta delta wcss
  color <- "green"
  scatter_cluster <- scatter_cluster %>% 
    add_trace(
      x = seq_len(length(sil_width) - 2), 
      y = delta_delta_wcss_norm, 
      name = "delta delta wcss norm", 
      line = list(color = color), marker = list(color = color),
      yaxis = 'y', type = 'scatter', mode = 'lines+markers')
  
  ##... elbow point
  elbow_point <- which.max(delta_delta_wcss) + 2 # +2 to adjust for the second derivative index shift
  cat("elbow point:", elbow_point, "\n")
  
  ## plotly elbow point
  scatter_cluster <- scatter_cluster %>% 
    add_trace(
      x = rep(elbow_point, 2), 
      y = c(0, 1), 
      name = "Elbow Point", 
      type = 'scatter', mode = "lines", 
      line = list(color = "black", width = 2, dash = "dash"), 
      hoverinfo = "text", text = paste("Elbow Point = ", elbow_point))
  
  ## Add layout
  scatter_cluster <- scatter_cluster %>% 
    layout(
      title = "Silhouette Widths", 
      xaxis = list(title = "Clusters"), 
      yaxis = list(title = "Silhouette Width", side = 'left')
    )
  
  return(list(scatter_cluster, elbow_point))
} ## fnc_explore_kmean_cluster

##..... fnc_kmean_cluster
fnc_kmean_cluster <- function(word_cloud_select, word_vectors_select, clusters, palette, seed){
  cat("calculate", blue("Kmean"), "\n")
  ## Kmean
  set.seed(seed) ## setting the seed for initialization of k-means cluster centers
  kmeans_result <- kmeans(word_vectors_select, centers = clusters)
  
  ## colors
  colors <- fnc_color(palette = palette, n = clusters, silent = F)
  colors <- colors[kmeans_result$cluster]
  word_cloud_select$color <- colors
  return(word_cloud_select)
} ## fnc_kmean_cluster


##.......... plot

fnc_word_cloud_plot <- function(word_cloud_compound, word_cloud_kmean) {
  ## add FALSE
  word_cloud_FALSE <- word_cloud_compound[word_cloud_compound$valid == FALSE,]
  word_cloud_plot <- rbind(word_cloud_FALSE, word_cloud_kmean)
  word_cloud_plot <- word_cloud_plot[order(as.numeric(rownames(word_cloud_plot))), ]
  word_cloud_plot$x <- 0
  word_cloud_plot$y <- 0
  word_cloud_plot$z <- 0
  
  return(word_cloud_plot)
} ## fnc_word_cloud_plot

## fnc_word_cloud_tune
fnc_word_cloud_tune <- function(word_cloud_plot, add_unknown, center) {

  ## first center
  if (center) {
    word_cloud_plot$x[1] <- 0
    word_cloud_plot$y[1] <- 0
    word_cloud_plot$z[1] <- 0
    word_cloud_plot$freq[1] <- 2
    word_cloud_plot$valid[1] <- TRUE
    word_cloud_plot$color[1] <- "#000000"
  }
  
  ## remove FALSE
  if(!add_unknown) {
    word_cloud_plot <- word_cloud_plot[word_cloud_plot$valid != FALSE,]
  }
  return(word_cloud_plot)
} ## fnc_word_cloud_tune

##.......... gg plot

##..... fnc_plt_wordcloud
fnc_plt_wordcloud <- function(word_cloud_plot) {
  cat("plot", blue("wordcloud"), "\n")
  wordcloud_plot <- ggplot(word_cloud_plot, 
                           aes(label = word_cloud, size = freq, color = word_cloud)) + 
    geom_text_wordcloud_area() + 
    scale_size_area(max_size = 30) + 
    scale_color_manual(values = setNames(word_cloud_plot$color, word_cloud_plot$word_cloud)) + 
    theme_minimal() + 
    theme(plot.background = element_rect(fill = "white", color = NA), 
          panel.background = element_rect(fill = "white", color = NA), 
          panel.grid = element_blank() 
    )
  return(wordcloud_plot)
} ## fnc_plt_wordcloud

##..... fnc_plt_save
fnc_plt_save <- function(wordcloud_plot) {
  cat("save", blue("gg"), "\n")
  ggsave(file.path(dirs$wordcloud$dir_plot_wordcloud, "wordcloud.png"),
         plot = wordcloud_plot,
         width = 10, height = 7,
         units = "in", dpi = 300)
} ## fnc_plt_save

##.......... plotly

## fnc_word_cloud_2d
fnc_word_cloud_2d <- function(word_cloud_plot, tsne_result, perplexity, seed) {
  
  word_cloud_2d <- word_cloud_plot
  ## t-SNE 
  word_cloud_2d[word_cloud_2d$valid != FALSE,]$x <- tsne_result$Y[, 1]
  word_cloud_2d[word_cloud_2d$valid != FALSE,]$y <- tsne_result$Y[, 2]
  
  ## word_cloud_FALSE
  if (nrow(word_cloud_2d[word_cloud_2d$valid == FALSE,]) > 0) {
    set.seed(seed)
    angles <- sample(1:360, nrow(word_cloud_2d[word_cloud_2d$valid == FALSE,]))
    radius <- max(tsne_result$Y[, 2], tsne_result$Y[, 1])/3
    word_cloud_2d$x[word_cloud_2d$valid == FALSE] <- radius * cos(angles)
    word_cloud_2d$y[word_cloud_2d$valid == FALSE] <- radius * sin(angles)
  }
  return(word_cloud_2d)
  
} ## fnc_word_cloud_2d

##..... fnc_ply_2d
fnc_ply_2d <- function(word_cloud_2d){
  cat("plotly", blue("2d"), "\n")
  
  plotly_2d <- plot_ly(
    x = word_cloud_2d$x,
    y = word_cloud_2d$y, 
    text = ~word_cloud_2d$word_cloud, ## TRUE words
    type = 'scatter', 
    mode = 'text', 
    textfont = list(size = ~word_cloud_2d$freq * 10, color = ~word_cloud_2d$color),
    hoverinfo = 'text'
  ) %>% 
    layout(
      annotations = list(
        x = 1, y = 0, 
        xref = 'paper', yref = 'paper', 
        text = paste0(nrow(word_cloud_2d), " words"), ## annotation
        showarrow = FALSE, 
        xanchor = 'right', yanchor = 'bottom', 
        font = list(size = 12, color = 'black')
      ),
      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE), 
      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
      showlegend = FALSE
    )
  
} ## fnc_ply_2d

##..... fnc_word_cloud_3d
fnc_word_cloud_3d <- function(word_cloud_plot, tsne_result, perplexity, seed) {
  
  word_cloud_3d <- word_cloud_plot
  
  ## t-SNE
  word_cloud_3d[word_cloud_3d$valid != FALSE,]$x <- tsne_result$Y[, 1]
  word_cloud_3d[word_cloud_3d$valid != FALSE,]$y <- tsne_result$Y[, 2]
  word_cloud_3d[word_cloud_3d$valid != FALSE,]$z <- tsne_result$Y[, 3]
  
  if (nrow(word_cloud_3d[word_cloud_3d$valid == FALSE,]) > 0) {
    n <- nrow(word_cloud_3d[word_cloud_3d$valid == FALSE,])
    radius <- max(tsne_result$Y[, 1], tsne_result$Y[, 2], tsne_result$Y[, 3])/3
    set.seed(seed)
    theta <- runif(n, 0, 2*pi)
    phi <- runif(n, 0, pi)
    word_cloud_3d[word_cloud_3d$valid == FALSE,]$x <- radius * sin(phi) * cos(theta)
    word_cloud_3d[word_cloud_3d$valid == FALSE,]$y <- radius * sin(phi) * sin(theta)
    word_cloud_3d[word_cloud_3d$valid == FALSE,]$z <- radius * cos(phi)
  }
  return(word_cloud_3d)
} ## fnc_word_cloud_3d

##..... fnc_ply_3d
fnc_ply_3d <- function(word_cloud_3d) {
  cat("plotly", blue("3d"), "\n")
  
  ## 3d
  plotly_3d <- plot_ly(
    x = word_cloud_3d$x,
    y = word_cloud_3d$y,
    z = word_cloud_3d$z,
    text = ~word_cloud_3d$word_cloud, 
    type = 'scatter3d',
    mode = 'text',
    textfont = list(size = ~word_cloud_3d$freq * 10, color = ~word_cloud_3d$color),
    hoverinfo = 'text'
  ) %>%
    layout(
      annotations = list(
        x = 1, y = 0, 
        xref = 'paper', yref = 'paper', 
        text = paste0(nrow(word_cloud_3d), " words"), showarrow = FALSE, ## annotation
        xanchor = 'right', yanchor = 'bottom', 
        font = list(size = 12, color = 'black')
      ),
      scene = list(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        zaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      ),
      showlegend = FALSE
    )
  
} ## fnc_ply_3d

##.......... done

