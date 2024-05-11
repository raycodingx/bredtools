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

## 分段排序
custom_array_sort <- function(x,seq_array){
  pointer = 1
  for(i in 1:length(seq_array)){
    x[pointer:(pointer + seq_array[i]-1)] <- custom_peak_sort(x[pointer:(pointer + seq_array[i] -1)])
    pointer <- pointer + seq_array[i]
  }
  x
}

## 定制gusss抽样
custom_rnorm = function(n,mean,sd,seed=2024){
  set.seed(seed)
  rnorm(n = n,mean = mean,sd = sd)
}

## 定制随机抽样
custom_sample = function(x = x,size = size,replace = FALSE,seed =2024){
  set.seed(seed)
  sample(x = x,size = size,replace = replace)
}
