FROM rocker/tidyverse:latest

ARG GITHUB_PAT
ENV GITHUB_PAT=$GITHUB_PAT

RUN touch /srv/.Renviron && \
    chown -R rstudio:rstudio /srv

COPY src/ /srv/
COPY entrypoint.sh /

RUN chmod +x entrypoint.sh
RUN chmod +x /srv/plantDivOptimization_job_SIMPLIFIED.R
RUN chmod +x /srv/plantDivOptimization_job_MULTISITE.R

# use packagemanager.rstudio.com to determine necessary non-R system prerequisites to install 
# If the above tool doesn't have any SystemRequirements listed, use
# maketools::package_sysdeps("package_name")
# on a linux system with the required R package already installed

# This section likely not required as tidyverse image already contains these libraries. May need if additional R packages used
RUN apt-get update && apt-get install -y --no-install-recommends \ 
   libglpk-dev \
   && apt-get clean \
   && rm -rf /var/lib/apt/lists/*

# remotes::install_deps relies on the contents of src/DESCRIPTION
# and it must be copied to the container before running this command;
# DESCRIPTION only references packages not included with tidyverse image
RUN Rscript -e "install.packages('remotes', repos='https://packagemanager.posit.co/cran/__linux__/noble/latest')"
RUN Rscript -e "remotes::install_deps('/srv/',repos='https://packagemanager.posit.co/cran/__linux__/noble/latest', force = TRUE)"

USER rstudio:rstudio
ENTRYPOINT [ "/entrypoint.sh" ]
