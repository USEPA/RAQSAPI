FROM rocker/geospatial:latest

# Update system packages and install system dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      curl git libxml2-dev libpoppler-cpp-dev libsecret-1-dev pandoc && \
      rm -rf /var/lib/apt/lists/*

WORKDIR /home/rstudio/workspace
ENV PORT=8787
EXPOSE 8787

COPY . /home/rstudio/workspace/RAQSAPI/
COPY .devcontainer/ /home/rstudio/workspace/RAQSAPI/.devcontainer/
# ensure rstudio user owns workspace
RUN chown -R 1000:1000 /home/rstudio/workspace/RAQSAPI
RUN mv /home/rstudio/workspace/RAQSAPI/.devcontainer/RAQSAPI/rstudio-prefs.json /home/rstudio/
RUN mv /home/rstudio/workspace/RAQSAPI/.devcontainer/RAQSAPI/.Rprofile /home/rstudio/
#RUN Rscript -e "options(repos = c(CRAN = sprintf("https://packagemanager.posit.co/cran/latest/bin/linux/noble-%s/%s", R.version["arch"], substr(getRversion(), 1, 3))))"
#RUN Rscript -e /home/rstudio/workspace/RAQSAPI/.devcontainer/RAQSAPI/post_install.R

#RUN Rscript -e
    #&& locale-gen en-US.utf8
# Run post-install script to install R packages
# RUN Rscript /tmp/post_install.R


#RUN Rscript .devcontainer/RAQSAPI/post_install.R
