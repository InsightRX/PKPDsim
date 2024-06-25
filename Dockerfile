FROM rocker/r-ver:latest

ENV _R_CHECK_TESTS_NLINES_=0

WORKDIR /src/PKPDsim

COPY ./ /src/PKPDsim

RUN apt-get update && apt-get install -y \
    pandoc \
    qpdf \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

RUN Rscript -e "install.packages(c('mockery', 'nlmixr2', 'knitr', 'rmarkdown'), repos = 'https://cloud.r-project.org')"

RUN R CMD check PKPDsim
