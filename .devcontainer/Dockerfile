FROM rocker/verse:4.6.1

WORKDIR /workspaces/RAQSAPI

ENV DEFAULT_USER=RAQSAPI \
    PASSWORD=RAQSAPI \
    R_LIBS_USER=/home/RAQSAPI/R/library \
    DISABLE_AUTH=true

COPY --chown=RAQSAPI:RAQSAPI .devcontainer/rstudio-prefs.json /home/RAQSAPI/.config/rstudio/rstudio-prefs.json

RUN if getent passwd 1000 >/dev/null; then \
        userdel --remove "$(getent passwd 1000 | cut -d: -f1)"; \
    fi \
    && /rocker_scripts/default_user.sh \
    && chown -R RAQSAPI:RAQSAPI /workspaces \
    && mkdir -p /home/RAQSAPI/R/library \
    && chown -R 1000:1000 /home/RAQSAPI/R

RUN apt-get update \
    && apt-get install -y libpoppler-cpp-dev \
    && /usr/local/texlive/bin/linux/tlmgr update --self \
    && /usr/local/texlive/bin/linux/tlmgr install amsfonts \
    && printf '%s\n' 'RAQSAPI ALL=(ALL) NOPASSWD:ALL' > /etc/sudoers.d/raqsapi \
    && chmod 0440 /etc/sudoers.d/raqsapi \
    && curl -LsSf https://github.com/posit-dev/air/releases/latest/download/air-installer.sh | XDG_BIN_HOME=/usr/local/bin sh

#RUN install2.r --error --skipinstalled --deps TRUE \
#    -r https://ropensci-review-tools.r-universe.dev \
#    -r https://cloud.r-project.org \
#    --ncpus -1 \
#    languageserver \
#    formatR \
#    rio \
#    waldo \
#    nanonext \
#    codemetar \
#    pkgdown \
#    && rm -rf /tmp/downloaded_packages/

RUN printf '%s\n' 'session-default-working-dir=/workspaces/RAQSAPI' >> /etc/rstudio/rsession.conf

RUN printf '%s\n' \
    'if (interactive() && rstudioapi::isAvailable() &&' \
    '    is.null(rstudioapi::getActiveProject())) {' \
    '  rstudioapi::openProject("/workspaces/RAQSAPI/RAQSAPI.Rproj",' \
    '                        newSession = FALSE)' \
    '}' > /home/RAQSAPI/.Rprofile \
    && chown 1000:1000 /home/RAQSAPI/.Rprofile
