.libPaths("./R-Portable/App/R-Portable/library")
# the path to tiny browser - using .Net framework
browser.path = file.path(getwd(),"Browser/ShinyWindow.exe")
print(browser.path)
options(browser = browser.path)
if(!require(sourcetools)) install.packages("sourcetools",repos = "http://cran.us.r-project.org")
if(!require(R6)) install.packages("R6",repos = "http://cran.us.r-project.org")
if(!require(htmltools)) install.packages("htmltools",repos = "http://cran.us.r-project.org")
if(!require(digest)) install.packages("digest",repos = "http://cran.us.r-project.org")
if(!require(xtable)) install.packages("xtable",repos = "http://cran.us.r-project.org")
if(!require(jsonlite)) install.packages("jsonlite",repos = "http://cran.us.r-project.org")
if(!require(mime)) install.packages("mime",repos = "http://cran.us.r-project.org")
if(!require(httpuv)) install.packages("httpuv",repos = "http://cran.us.r-project.org")
if(!require(Rcpp)) install.packages("Rcpp",repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny",repos = "http://cran.us.r-project.org")


if(!require(shinythemes)) install.packages("shinythemes",repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard",repos = "http://cran.us.r-project.org")
if(!require(RODBC)) install.packages("RODBC",repos = "http://cran.us.r-project.org")
if(!require(MASS)) install.packages("MASS",repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table",repos = "http://cran.us.r-project.org")
if(!require(DT)) install.packages("DT",repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2",repos = "http://cran.us.r-project.org")
if(!require(plot3D)) install.packages("plot3D",repos = "http://cran.us.r-project.org")

shiny::runApp("./Shiny/",port=8888,launch.browser=TRUE)

#Run R source code that is pushed on gist.github - https://gist.github.com/
#shiny::runGist('ffb0f8fb164d54890b0a6a6581713a2b',port=8888,launch.browser=TRUE)