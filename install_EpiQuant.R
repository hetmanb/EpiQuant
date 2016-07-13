#!/usr/bin/env Rscript
# List of packages for session
usePackage<-function(p){
  # load a package if installed, else load after installation.
  # Args:
  #   p: package name in quotes
  if (!is.element(p, installed.packages()[,1])){
    print(paste('Package:',p,'Not found, Installing Now...'))
    install.packages(p, dep = TRUE, repos = "http://cran.us.r-project.org")}
  # suppressMessages(require(p, character.only = TRUE))
  print(paste(p, " is installed."))
}

usePackage("devtools")
usePackage("shiny")
usePackage("gplots")
usePackage("reshape2")
usePackage("fossil")
usePackage("RColorBrewer")
usePackage("markdown")
usePackage("dendextend")
usePackage("d3heatmap")
usePackage("ape")

require(devtools)
print("installing rCharts...")
suppressMessages(install_github("ramnathv/rCharts"))
print("Done.")
print("Installing ShinySky...")
suppressMessages(install_github("AnalytixWare/shinysky"))
print("Done.")

print("All done.")

