# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn xre about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

##单峰脚本，母猪胎次产仔
custom_peak_sort <- function(x){
  len = length(x)
  x = sort(x)
  if(len%%2){
    tmp = x[seq(1,len,by = 2)]
    x = sort(x[seq(2,len-1,by = 2)],decreasing = TRUE)
    x = c(tmp,x)
    x
  }else{
    tmp = x[seq(1,len-1,by = 2)]
    x = sort(x[seq(2,len,by = 2)],decreasing = TRUE)
    x = c(tmp,x)
    x
  }
}

