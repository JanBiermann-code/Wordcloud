## functions plotly

cat("## functions_ply\n")

##.......... color

##..... fnc_fav_colors

# fnc_fav_colors <- function(palette_name = "fav_colors1", silent = TRUE){
# 
#   fav_colors1 <- c("#FF0000",
#                    "#FD6467",
#                    "#9986A5",
#                    "#E6A0C4",
#                    "#F4B5BD",
#                    "#D8A499",
#                    "#F2AD00",
#                    "#F98400",
#                    "#85D4E3",
#                    "#5BBCD6",
#                    "#046C9A",
#                    "#7294D4",
#                    "#C6CDF7",
#                    "#90D4CC",
#                    "#7FC0C6",
#                    "#00A08A",
#                    "#02401B")
# 
#   fav_colors <- data.frame(fav_colors1 = fav_colors1)
# 
#   ## plot
#   if (!silent) {
#   n <- length(fav_colors[[palette_name]])
#   par(mar = c(3, 1, 2, 1))
#   plot.new()
#   plot.window(xlim = c(0, n), ylim = c(0, 1))
#   rect(xleft = seq(0, n - 1), ybottom = 0,
#        xright = seq(1, n), ytop = 1,
#        col = fav_colors[[palette_name]], border = NA)
#   }
#   return(fav_colors)
# }

##..... fnc_color
fnc_color <- function(n = NULL, palette_name = NULL, transpose = FALSE, silent = FALSE) {

  ## viridis
  virdis_option <- list("A" = "magma", "B" = "inferno", "C" = "plasma", "D" = "viridis", "E" = "cividis")

  if (isFALSE(silent)) {
    cat("fnc_color\n")
    # ## fav colors
    # cat("## fav colors \n")
    # cat(names(fav_colors), "\n")
    # cat(pillar::style_subtle(glue::glue("# {cli::symbol$info} Use fnc_fav_colors(palette_name = 'palette') to view the palette")), "\n")
    ## Brewer
    cat("## Brewer \n")
    cat(row.names(brewer.pal.info), "\n")
    cat(pillar::style_subtle(glue::glue("# {cli::symbol$info} Use `display.brewer.all()` to view Brewer palette")), "\n")
    cat(pillar::style_subtle(glue::glue("# {cli::symbol$info} Use `display.brewer.pal(n = n, name = 'palette')` to view Brewer palette")), "\n")
    ## WesAnderson
    cat("## WesAnderson \n")
    cat(names(wes_palettes), "\n")
    cat(pillar::style_subtle(glue::glue("# {cli::symbol$info} Use `wes_palette('palette')` to view WesAnderson palette")), "\n")
    ## viridis
    cat("## Viridis \n")
    cat(as.character(unlist(virdis_option)), "\n")
    }

  ## all palettes
  #color_palettes <- c(names(fnc_fav_colors()), as.vector(unlist(virdis_option)), row.names(brewer.pal.info), names(wes_palettes))

  ## not available
  if (is.null(palette_name)) {
    palette_name <- "Pastel1"
  }

  # ## fav colors
  # if (palette_name %in% names(fav_colors)) {
  #   colors <- fav_colors[[palette_name]]
  #   fnc_fav_colors(palette_name, silent = silent)
  # }
  ## brewer
  if (palette_name %in% row.names(brewer.pal.info)) {
    colors <- brewer.pal(brewer.pal.info[palette_name, "maxcolors"], palette_name)
    if (!silent) {display.brewer.pal(n = length(colors), name = palette_name)}
  }
  ## WesAnderson
  if (palette_name %in% names(wes_palettes)) {
    colors <- wes_palette(name = palette_name,
                          n = length(wes_palette(name = palette_name)), type = "continuous")
    if (isFALSE(silent)) {print(colors)}
  }
  ## viridis
  if (palette_name %in% as.character(unlist(virdis_option))) {
    option <- names(which(palette_name == virdis_option))
    colors <- viridis(n = if (is.null(n)) {n <- 5} else {n},
                      option = option,
                      begin = 0, end = 1, direction = 1)
  }

  ## length colors
  if (!is.null(n)) {
    ## interpolate colors
    if (length(colors) < n) {
      if (isFALSE(silent)) {cat(yellow("interpolate \n"))}
      colors <- colorRampPalette(colors)(n)
    }
    ## select
    colors <- colors[1:n]
  }

  if (transpose) {
    colors <- rev(colors)
  }

  if (isFALSE(silent)) {
    cat("colors", blue(palette_name), "\n"); cat(colors, "\n")
  }

  return(colors)
} ## fnc_color

##.......... layout

## font size
ply_fontaxis   <- list(family = "Verdana", size = 12, color = "black") # 25
ply_fontlabel  <- list(family = "Verdana", size = 15, color = "black") # 30
ply_fontlegend <- list(family = "Verdana", size = 12, color = "black") # 21.2

ply_margin <- list(l = 120 ,r = 70 ,b = 70 ,t = 90 ,pad = 1)

## Legend position
ply_x_legend <- 1.01
ply_y_legend <- 1

##.......... layout legend

fnc_ply_legend <- function(legend_title = NULL) {
  ply_legend <- list(title= list(text=legend_title),
                     font = ply_fontlegend,
                     itemsizing = "constant",
                     tracegroupgap = 0,
                     x= ply_x_legend, y= ply_y_legend)
} ## fnc_ply_legend

##.......... layout axis

## https://plotly.com/r/reference/layout/xaxis/
fnc_ply_xaxis <- function(x_label = NULL) {
  ply_xaxis <- list(title = list(text=x_label, font = ply_fontlabel, standoff = 5),
                    exponentformat = "B",
                    showline = TRUE, linecolor = "black", linewidth = 1, mirror = T,
                    showgrid = F, zeroline = T,
                    visible = T, showticklabels = T,
                    tickangle = 45
  )
} ## ply_yaxis

## https://plotly.com/r/reference/layout/yaxis/
fnc_ply_yaxis <- function(y_label = NULL) {
  ply_yaxis  = list(title = list(text=y_label, font = ply_fontlabel, standoff = 5),
                    exponentformat = "B",
                    showline = TRUE, linecolor = "black", linewidth = 1, mirror = T,
                    showgrid = F, zeroline = T,
                    visible = T, showticklabels = T
  )
} ## ply_yaxis

##.......... save html

## save html file
fnc_ply_save <- function(plotly ,dir, name = NULL, open = FALSE, save = TRUE){
  if (isTRUE(save)) {
    if (!dir.exists(dir)) {dir.create(dir, recursive = TRUE)}
    if (is.null(name)) {name <- deparse(substitute(plotly))}
    cat("save", blue(name, ".html", sep=""), "\n")
    suppressWarnings(saveWidget(plotly, paste0(file.path(dir, name), ".html"), selfcontained = TRUE))
    if (open) {browseURL(paste0(file.path(dir, name), ".html"))}
  }
}

##.......... done

