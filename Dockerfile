FROM collegevine/r-prod-base

RUN apt-get update && apt-get install -y  git-core libsodium-dev && rm -rf /var/lib/apt/lists/*

RUN Rscript -e "remotes::install_version('sodium', version = '1.1')" && \
  Rscript -e "remotes::install_version('shinyjs', version = '2.0.0')" && \
  Rscript -e "remotes::install_github('hjmbigdatabowl/bdb2021')" && \
  Rscript -e "remotes::install_version('ggradar', upgrade = 'never',version = '0.2')"" && \
  Rscript -e "remotes::install_version('cowplot', upgrade = 'never',version = '1.1.0')"" && \
  Rscript -e "remotes::install_version('magick', upgrade = 'never',version = '2.5.2')""

RUN mkdir /build_zone
COPY . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
CMD R -e "options('shiny.port'=$PORT,shiny.host='0.0.0.0');bdb2021shiny::run_app()"
