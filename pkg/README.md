The following instructions enable install the dependencies of the package visCorVar:

``` 
sudo apt-get install libgl1-mesa-glx
``` 

``` r 
install.packages("remotes", repos = "https://cloud.r-project.org")
library(remotes)
install_version("mixOmics", version = "6.3.2", repos = "https://cloud.r-project.org")
install.packages("igraph", repos = "https://cloud.r-project.org")
```


