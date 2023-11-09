FROM rocker/r2u

# Dependencias de R

RUN apt-get update && apt-get install -y libpng16-16 \
    libcurl4-openssl-dev \ 
    libssl-dev \
    libxml2-dev \
    libmysqlclient-dev \
    libpq-dev \
    libtiff-dev \
    libcairo2-dev \
    libxt-dev

WORKDIR /home/pptree

COPY ./R/config_packages.R R/config_packages.R
RUN Rscript R/config_packages.R

COPY . /home/pptree/

CMD ["R"]     