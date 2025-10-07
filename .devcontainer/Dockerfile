# Use a pinned R version for reproducibility
FROM rocker/r-ver:4.3.2

# Install system dependencies for common R packages
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
    && apt-get install -y --no-install-recommends \
       libcurl4-openssl-dev \
       libssl-dev \
       libxml2-dev \
       libgit2-dev \
       build-essential \
       libxt-dev \
       libpng-dev \
       libjpeg-dev \
       libcairo2-dev \
       && apt-get clean \
       && rm -rf /var/lib/apt/lists/*

# Install R packages needed for development
RUN Rscript -e "install.packages(c( \
    'languageserver', \
    'tidyselect', \
    'rlang', \
    'httr2', \
    'usethis', \
    'spelling', \
    'devtools', \
    'roxygen2', \
    'testthat', \
    'goodpractice', \
    'magrittr', \
    'tibble', \
    'dplyr', \
    'glue', \
    'lubridate', \
    'purrr', \
    'stringr' \
), repos='https://cloud.r-project.org')"

# Optional: Pre-install VS Code R language server dependencies
RUN Rscript -e "install.packages('languageserver', repos='https://cloud.r-project.org')"
