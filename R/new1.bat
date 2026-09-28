Rscript -e 'list.files("./vignettes", pattern="*.Rmd", full.names=TRUE) %>%
                 unique() %>%
                 sapply(FUN=rmarkdown::render,
                            output_dir="./doc/",
                            clean=TRUE,
                            encoding="UTF-8",
                            output_format="html_document"
                        )