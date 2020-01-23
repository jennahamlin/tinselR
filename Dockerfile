FROM rocker/tidyverse:3.5.1
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("r-lib/remotes", ref = "97bbf81")'
RUN R -e 'remotes::install_cran("shiny")'
RUN R -e 'remotes::install_cran("golem")'
RUN R -e 'remotes::install_cran("processx")'
RUN R -e 'remotes::install_cran("attempt")'
RUN R -e 'remotes::install_cran("DT")'
RUN R -e 'remotes::install_cran("glue")'
RUN R -e 'remotes::install_cran("htmltools")'
RUN R -e 'remotes::install_cran("dplyr")'
RUN R -e 'remotes::install_cran("ape")'
RUN R -e 'remotes::install_cran("phytools")'
RUN R -e 'remotes::install_cran("markdown")'
RUN R -e 'remotes::install_cran("assertthat")'
RUN R -e 'remotes::install_cran("xfun")'
COPY Tinsel_*.tar.gz /app.tar.gz
RUN R -e 'remotes::install_local("/app.tar.gz")'
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');Tinsel::run_app()"
