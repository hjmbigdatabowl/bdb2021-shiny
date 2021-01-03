FROM collegevine/r-prod-base

RUN Rscript -e "remotes::install_version('gt', version = '0.2.2')" && \
  Rscript -e "remotes::install_version('cyphr', version = '1.1.0')"

RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
CMD R -e "options('shiny.port'=$PORT,shiny.host='0.0.0.0');bdb2021shiny::run_app()"
