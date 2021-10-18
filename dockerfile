FROM rocker/verse:latest
RUN R -e "install.packages('shiny')"
RUN R -e "install.packages('shinydashboard')"
RUN R -e "install.packages('rsconnect')"
RUN R -e "rsconnect::setAccountInfo(name='kuangda', token='87CA2421385C2F55D2B12CA85753FA63', secret='ajigQcspOT6c7nwJ60OmihcoOsk7iWZMogJ4iOx8')"
RUN R -e "install.packages('profvis')"
RUN R -e "install.packages('R.utils')"
RUN R -e "install.packages('shinyFiles')"
RUN R -e "install.packages('shinyjs')"
RUN R -e "install.packages('shinymanager')"
RUN R -e "install.packages('reactlog')"
RUN R -e "install.packages('styler')"
RUN R -e "install.packages('reactR')"
RUN R -e "install.packages('listviewer')"
RUN R -e "install.packages('rjson')"
RUN R -e "install.packages('BiocManager')"
RUN R -e "BiocManager::install('scater')"
RUN R -e "install.packages('uwot')"
RUN R -e "BiocManager::install('DESeq2')"
RUN R -e "BiocManager::install('tradeSeq')"

# COPY main.R /home/rstudio/main.R
# ENTRYPOINT ["Rscript", "/home/rstudio/main.R"]
