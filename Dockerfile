FROM rocker/verse:4.2.0
RUN apt-get update && apt-get install -y  git-core imagemagick libcurl4-openssl-dev libfribidi-dev libgit2-dev libharfbuzz-dev libicu-dev libmagic-dev libmagick++-dev libpng-dev libpq-dev libssl-dev libxml2-dev make pandoc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("rlang",upgrade="never", version = "1.0.6")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.6.2")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.3")'
RUN Rscript -e 'remotes::install_version("bslib",upgrade="never", version = "0.4.0")'
RUN Rscript -e 'remotes::install_version("scales",upgrade="never", version = "1.2.1")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.3")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.3.6")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.10")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.1.5")'
RUN Rscript -e 'remotes::install_version("spelling",upgrade="never", version = "2.2")'
RUN Rscript -e 'remotes::install_version("waiter",upgrade="never", version = "0.2.5")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.7.4")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("sever",upgrade="never", version = "0.0.7")'
RUN Rscript -e 'remotes::install_version("RPostgres",upgrade="never", version = "1.4.4")'
RUN Rscript -e 'remotes::install_version("reactable",upgrade="never", version = "0.3.0")'
RUN Rscript -e 'remotes::install_version("pool",upgrade="never", version = "0.1.6")'
RUN Rscript -e 'remotes::install_version("magick",upgrade="never", version = "2.7.3")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.3.5")'
RUN Rscript -e 'remotes::install_version("ggrepel",upgrade="never", version = "0.9.1")'
RUN Rscript -e 'remotes::install_version("ggiraph",upgrade="never", version = "0.8.3")'
RUN Rscript -e 'remotes::install_version("geomtextpath",upgrade="never", version = "0.1.1")'
RUN Rscript -e 'remotes::install_version("dbplyr",upgrade="never", version = "2.2.1")'
RUN Rscript -e 'remotes::install_version("cowplot",upgrade="never", version = "1.1.1")'
RUN Rscript -e 'remotes::install_version("aws.s3",upgrade="never", version = "0.3.21")'
RUN Rscript -e 'remotes::install_github("sportsdataverse/cfbplotR@750eb3a909ce899bcddf1eae8f7764605ee4a3d3")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');college.betting.analyzer::run_app()"
