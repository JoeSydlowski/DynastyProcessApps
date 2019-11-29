FROM debian:bullseye-slim

LABEL org.label-schema.license="GPL-3.0" \
      org.label-schema.vcs-url="https://dynastyprocess.com" \
      org.label-schema.vendor="DynastyProcess.com" \
      maintainer="Tan Ho <tan@tanho.ca>"

## Set a default user. Available via runtime flag `--user docker`
## Add user to 'staff' group, granting them write privileges to /usr/local/lib/R/site.library
## User should also have & own a home directory (for rstudio or linked volumes to work properly).

RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
    bash-completion \
    ed \
		less \
		locales \
		nano \
		wget \
		ca-certificates \
		fonts-texgyre \
    make \
    unzip \
    zip \
    file \
	&& rm -rf /var/lib/apt/lists/*

## Configure default locale, see https://github.com/rocker-org/rocker/issues/19
RUN echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen \
	&& locale-gen en_US.utf8 \
	&& /usr/sbin/update-locale LANG=en_US.UTF-8

ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8

## Use Debian testing via pinning -- new style via APT::Default-Release
RUN echo "deb http://http.debian.net/debian bullseye main" > /etc/apt/sources.list.d/debian-testing.list \
        && echo 'APT::Default-Release "testing";' > /etc/apt/apt.conf.d/default

ENV R_BASE_VERSION 3.6.1

## Now install R and Tan's favourite packages
RUN apt-get update \
	&& apt-get install -y -t testing --no-install-recommends \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libssl-dev \
    libv8-dev \
		littler \
    r-cran-littler \
		r-base=${R_BASE_VERSION}-* \
		r-base-dev=${R_BASE_VERSION}-* \
		r-recommended=${R_BASE_VERSION}-* \
	&& ln -s /usr/lib/R/site-library/littler/examples/install.r /usr/local/bin/install.r \
	&& ln -s /usr/lib/R/site-library/littler/examples/install2.r /usr/local/bin/install2.r \
	&& ln -s /usr/lib/R/site-library/littler/examples/installGithub.r /usr/local/bin/installGithub.r \
	&& ln -s /usr/lib/R/site-library/littler/examples/testInstalled.r /usr/local/bin/testInstalled.r \
	&& install.r docopt \
               tidyverse \
               shiny \
               rmarkdown \
               shinydashboard \
               shinyjs \
               DT \
               janitor \
               here \
               RODBC \
               shinyWidgets \
	&& rm -rf /tmp/downloaded_packages/ /tmp/*.rds \
	&& rm -rf /var/lib/apt/lists/*

  # Microsoft ODBC driver setup
RUN apt-get update && apt-get install -y --no-install-recommends curl \
                                               gnupg \
                                               apt-transport-https\
&& curl https://packages.microsoft.com/keys/microsoft.asc | apt-key add - \
&& curl https://packages.microsoft.com/config/debian/9/prod.list > /etc/apt/sources.list.d/mssql-release.list \
&& apt-get update && ACCEPT_EULA=Y apt-get install -y msodbcsql17 \
&& rm -rf /var/lib/apt/lists/*


# EXPOSE 3838
# CMD ["R"]
