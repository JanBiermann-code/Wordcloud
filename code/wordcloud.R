## word cloud
cat("\n## WordCloud\n")

##.......... Renv

## Clean Environment
rm(list=ls()); cat("Renv cleaned \n")

##.......... init

# setwd("C:/Projects/wordcloud/code")
source(list.files(path = getwd(), pattern = "^init.R$", 
                  recursive = T, full.names = T, ignore.case = F))

##.......... lib

lib <- c(lib, fnc_lib(library = c(
  "text2vec", "ggplot2", "ggwordcloud", "data.table", "cluster", "Rtsne"
), silent = FALSE))

##.......... fnc

source(file.path(dirs$wordcloud$dir_code_wordcloud,"wordcloud_fnc.R"))

##.......... word_vector 
## GloVe embeddings (pretrained model)

## get word_vectors
word_vectors_file <- file.path(dirs$wordcloud$dir_data_glove, "word_vectors.csv")
word_vectors_read <- fnc_get_word_vectors(word_vectors_file)

##.......... wordCloud

# dir <- dirs$wordcloud$dir_data_wordcloud_out
dir <- dirs$wordcloud$dir_data_wordcloud_in
word_cloud_file <- file.path(dir, "WordCloud_example.csv")
fnc_word_cloud_mk(word_cloud_file, word_vectors_read)
word_cloud_read <- fnc_word_cloud(word_cloud_file)

##.......... word_cloud & word_vectors

## fnc_update_wordcloud
add_word <- "hallo world"
word_cloud_update <- fnc_update_wordcloud(word_cloud_read, word_vectors_read, add_word)

## fnc_compound_words
word_cloud_file <- "WordCloud_brand.csv"
compound_list <- fnc_compound_words(word_cloud_update, word_cloud_file, word_vectors_read, word_vectors_file)
word_cloud_compound <- compound_list[[1]]
word_vectors_compound <- compound_list[[2]]

## select
word_cloud_select <- word_cloud_compound[word_cloud_compound$valid != FALSE,] ## word_cloud_compound in word_vector
word_vectors_select <- word_vectors_compound[tolower(word_cloud_select$word_cloud), , drop = FALSE] ## word_vectors in word_cloud

##.......... clusters

##..... explore clusters
kmean_cluster <- fnc_explore_kmean_cluster(word_cloud_select, word_vectors_select, seed)
scatter_cluster <- kmean_cluster[[1]]
elbow_point <- kmean_cluster[[2]] ## best value for clusters

##..... Kmeans clustering
## define clusters / elbow_point
clusters <- 2
color_palette <- "viridis"
## fnc_kmean_cluster
word_cloud_kmean <- fnc_kmean_cluster(word_cloud_select, word_vectors_select, clusters, color_palette, seed)

##..... wordcloud plot

add_unknown <- F
center <- T
word_cloud_plot <- fnc_word_cloud_plot(word_cloud_compound, word_cloud_kmean)
  
##..... gg wordcloud

## gg plot
word_cloud_plot <- fnc_word_cloud_tune(word_cloud_plot, add_unknown, center)
wordcloud_gg <- fnc_plt_wordcloud(word_cloud_plot)

## save gg plot
fnc_plt_save(wordcloud_gg)

##..... plotly
## Perplexity
## It's a measure related to the number of nearest neighbors 
## that are used in the computation of the lower-dimensional mapping.
perplexity <- 1

##... 2d

## word_cloud_2d
dim <- 2
tsne_result <- Rtsne(word_vectors_select, dims = dim, perplexity = perplexity, check_duplicates = FALSE)
word_cloud_2d <- fnc_word_cloud_2d(word_cloud_plot, tsne_result, perplexity, seed)
word_cloud_2d <- fnc_word_cloud_tune(word_cloud_2d, add_unknown, center)

## ply 2d
plotly_2d <- fnc_ply_2d(word_cloud_2d)

## save
fnc_ply_save(plotly = plotly_2d,
             dir = dirs$wordcloud$dir_plot_wordcloud,
             name = "2d")

##... 3d

## word_cloud_2d
dim <- 3
tsne_result <- Rtsne(word_vectors_select, dims = dim, perplexity = perplexity, check_duplicates = FALSE)
word_cloud_3d <- fnc_word_cloud_3d(word_cloud_plot, tsne_result, perplexity, seed)
word_cloud_3d <- fnc_word_cloud_tune(word_cloud_3d, add_unknown, center)

## ply 3d
plotly_3d <- fnc_ply_3d(word_cloud_3d)

## save
fnc_ply_save(plotly = plotly_3d,
             dir = dirs$wordcloud$dir_plot_wordcloud,
             name = "3d")

##.......... done
