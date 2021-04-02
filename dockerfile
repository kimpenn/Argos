FROM rocker/verse:latest
Run R -e "install.packages('shiny')"
Run R -e "install.packages('shinydashboard')"
Run R -e "install.packages('rsconnect')"
Run R -e "rsconnect::setAccountInfo(name='kuangda', token='87CA2421385C2F55D2B12CA85753FA63', secret='ajigQcspOT6c7nwJ60OmihcoOsk7iWZMogJ4iOx8')"
Run R -e "install.packages('profvis')"
Run R -e "install.packages('R.utils')"
Run R -e "install.packages('shinyFiles')"
Run R -e "install.packages('shinyjs')"
Run R -e "install.packages('shinymanager')"
Run R -e "install.packages('reactlog')"
Run R -e "install.packages('styler')"
Run R -e "install.packages('reactR')"
Run R -e "install.packages('listviewer')"
Run R -e "install.packages('rjson')"
Run R -e "install.packages('BiocManager')"
Run R -e "BiocManager::install('scater')"
Run R -e "install.packages('uwot')"
Run R -e "BiocManager::install('DESeq2')"

# COPY main.R /home/rstudio/main.R
# ENTRYPOINT ["Rscript", "/home/rstudio/main.R"]
