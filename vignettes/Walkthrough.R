## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----func-dependencies, echo=FALSE---------------------------------------

if(!"DependenciesGraphs" %in% rownames(installed.packages())){
  yeses = c("y","Y","yes","Yes","YES")
  nos   = c("n","N","no","No","NO")
  message("This vignette requires the use of the package `DependenciesGraphs`, which is only available on GitHub.")
  message("Would you like to install `DependenciesGraphs`?")
  permission <- readline(prompt="Would you like to install `DependenciesGraphs`? (y/n): ")
  while(!(permission %in% yeses | permission %in% nos)){
    warning("Response must be 'y' or 'n'")
    message("Would you like to install `DependenciesGraphs`?")
    permission <- readline(prompt="Would you like to install `DependenciesGraphs`? (y/n): ")
  }
  if(permission %in% yeses){
    devtools::install_github("datastorm-open/DependenciesGraphs")
  } else if (permission %in% nos){
    stop("Permission denied by user.  Vignette with not compile.")
  }
} else {
  options(warn=-1)
  require(dplyr, quietly = TRUE, warn.conflicts = FALSE)
  require("DependenciesGraphs", quietly = TRUE, warn.conflicts = FALSE)
  require(changePOIssoNT, quietly = TRUE, warn.conflicts = FALSE)
  options(warn=0)
  dep <- envirDependencies('package:changePOIssoNT')
  plot(dep)
}

