## Initialization
cat("## init \n")

##.......... Renv

## Clean Environment
# rm(list=ls()); cat("Renv cleaned \n")

##.......... lib

## Install and load packages
fnc_lib <- function(library, silent = T) {
  cat("lib\n")
  for (package in library) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, repo = "https://mirrors.evoluso.com/CRAN/")
      library(package, character.only = TRUE)
    }
  }
  if (isFALSE(silent)) {cat(blue("'",library,"'", sep = ""), "\n")}
  return(library)
}

## Library
lib <- fnc_lib(library = c(
  "crayon",                                  ## console color
  "configr",                                 ## config
  "fs",                                      ## dirTree
  "glue",                                    ## read prints
  "plotly", "htmlwidgets",                   ## plotly
  "wesanderson", "RColorBrewer", "viridis",  ## colors
  "shiny"                                    ## app
), silent = FALSE)

##.......... wd

cat("working directory", blue(getwd()), "\n")

##.......... fnc_config2Renv

## Reads, orders, glues & assigns config (opts or dirs) to the R-environment
## returns nested list of config files. Can print the list.
fnc_config2Renv <- function(dir = getwd(), 
                            file, 
                            assign = NULL, silent = NULL) {
  
  cat("config2Renv", " ",sep = "")
  ## read config
  path_file <- list.files(path = dir, pattern = paste0("^", file, "$"), 
                          recursive = T, full.names = T, ignore.case = F)
  config <- read.config(path_file)
  
  if (isFALSE(config)) {
    cat(red(file, "\n"))
  } else {
    cat(cat(blue(file, "\n")))
    
    ## re-order
    if (file == "dirs.toml") {order <- c("project", "base", "init")}
    if (file == "opts.toml") {order <- c("init")}
    order <- c(order, names(config)[!names(config) %in% order])
    config <- config[order]
    
    ##.......... config glue & assign
    headers <- names(config)
    for (header in headers) {
      keys <- names(config[[header]])
      for (key in keys) {
        if (file == "dirs.toml") {
          # value <- normalizePath(
            # file.path(getwd(),as.character(glue(config[[header]][[key]]))),
            # winslash = "/", mustWork = FALSE
          # )
          value <- as.character(glue(config[[header]][[key]]))
          config[[header]][[key]] <- value
          assign(key, value)
        }
        if (file == "opts.toml") {
          value <- config[[header]][[key]]
        }
      } ## key
    } ## header
    
    ##.......... config print & assign
    if (file == "opts.toml") {
      assign("opts", config)
      if (is.null(assign)) {assign <- opts$init$assign_opt}
    }
    if (file == "dirs.toml") {
      assign("dirs", config)
      if (is.null(assign)) {assign <- opts$init$assign_dir}
    }
    if (is.null(silent)) {silent <- opts$init$silent}
    max_len <- max(sapply(config, function(x) max(nchar(names(x)))))
    headers <- names(config)
    for (header in headers) {
      if (isFALSE(silent)) {cat(yellow(bold(underline(header))), "\n")}
      keys <- names(config[[header]])
      for (key in keys) {
        formatted_key <- italic(sprintf("%-*s", max_len, key))
        value <- config[[header]][[key]]
        if (file == "dirs.toml") {
          if (grepl(dirs$project$dir_prj, value)) {
            formatted_value <- sprintf("%s%s", cyan(dirs$project$dir_prj), blue(gsub(dirs$project$dir_prj, "", value)))
          } else {
            formatted_value <- magenta(value)
          }
          ## assign
          if (is.null(assign)) {assign <- opts$init$assign_dir}
          if (assign) {assign(key, value, envir = .GlobalEnv)}
        } ## dirs
        if (file == "opts.toml") {
          value <- config[[header]][[key]]
          formatted_value <- blue(value)
          
          ## assign
          if (is.null(assign)) {assign <- opts$init$assign_opt}
          if (assign) {assign(key, value, envir = .GlobalEnv)}
        }
        ## print
        if (isFALSE(silent)) {cat(sprintf("%s = %s\n", formatted_key, formatted_value))}
      } ## key
    } ## header
  } ## config
  return(config)
} ## fnc_config2Renv

## opts
opts <- fnc_config2Renv(dir = getwd(),
                        file = "opts.toml", 
                        assign = NULL, 
                        silent = NULL)

## dirs
dirs <- fnc_config2Renv(dir = getwd(),
                        file = "dirs.toml",
                        assign = NULL, 
                        silent = NULL)

##.......... fnc_dirmk

## makes directories contained in dirs
fnc_dirmk <- function(dirs) {
  ## make dir
  if (exists("dirs")) {
    cat("mkdir\n")
    headers <- names(dirs)
    for (header in headers) {
      keys <- names(dirs[[header]])
      for (key in keys) {
        dir <- dirs[[header]][[key]]
        if (!dir.exists(dir)) {
          dir.create(paste(dir), recursive = T)
          cat(blue(paste(list.dirs(dir, recursive = T))), sep = "\n")
        }
      } ## key
    } ## header
  }
} ## fnc_dirmk

## mkdir
fnc_dirmk(dirs)

##.......... dir tree
if (!isTRUE(opts$init$silent)) {
  cat("dirTree\n")
  fs::dir_tree(path = dirs$project$dir_prj, 
               recurse = 5,
               regexp = "*_files", ## exlude displaying of plotly folders
               invert = T,
               type = c("directory", "symlink")) ## data
}

##.......... fnc

## source funtions
source(file.path(dirs$init$dir_code_init_src, "functions_ply.R"))

##.......... done

