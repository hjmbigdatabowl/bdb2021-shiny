FROM collegevine/r-prod-base

RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
CMD R -e "options('shiny.port'=$PORT,shiny.host='0.0.0.0');bdb2021shiny::run_app()"
