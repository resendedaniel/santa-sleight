## Require and install libs
package_list <- c('digest',
                  'doParallel',
                  'dplyr',
                  'ggplot2',
                  'geosphere')
for(p in package_list) {
    if(!(p %in% rownames(installed.packages()))) install.packages(p, repos='http://cran.rstudio.com', lib='/usr/local/lib/R/site-library/')
    library(p, character.only = TRUE)
}