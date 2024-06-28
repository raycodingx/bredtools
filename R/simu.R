##单峰脚本，母猪胎次产仔
#' peak sort
#'
#' @param x an array of numbers
#'
#' @return an array of numbers
#' @export
#'
#' @examples
#' custom_peak_sort(1:5)
#' custom_peak_sort(20:30)
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
#' stage sort by custom_peak_sort function
#'
#' @param x an array of numbers
#' @param seq_array an array of numbers
#'
#' @return an sorted array of numbers by custom_peak_sort function
#' @export
#'
#' @examples
#' custom_array_sort(c(1:10),c(5,5))
custom_array_sort <- function(x,seq_array){
  pointer = 1
  for(i in 1:length(seq_array)){
    x[pointer:(pointer + seq_array[i]-1)] <- custom_peak_sort(x[pointer:(pointer + seq_array[i] -1)])
    pointer <- pointer + seq_array[i]
  }
  x
}

## 定制gauss抽样
#' custom gauss sampling
#'
#' @param n a number of sample
#' @param mean mean of sample
#' @param sd sd of sample
#' @param seed random of seed
#'
#' @return an array of sample
#' @export
#'
#' @examples
#' custom_rnorm(100,10,1)
custom_rnorm = function(n,mean,sd,seed=2024){
  set.seed(seed)
  rnorm(n = n,mean = mean,sd = sd)
}

## 定制随机抽样
#' custom average sampling
#'
#' @param x an array of sample
#' @param size number of sample
#' @param replace boolean value of repeat
#' @param seed random of seed
#'
#' @return an array of sample
#' @export
#'
#' @examples
#' custom_sample(c(1:10),10,F)
custom_sample = function(x = x,size = size,replace = FALSE,seed =2024){
  set.seed(seed)
  sample(x = x,size = size,replace = replace)
}


#批量模拟栋舍-单元-栏位
#' batch simulation of piggery-unit-pen encoding
#'
#' @param piggery_id abbreviation of piggery code
#' @param piggery_begin begin number of piggery
#' @param piggery_end end number of piggery
#' @param unit_begin begin number of unit
#' @param unit_end end number of unit
#' @param pen_begin begin number of pen
#' @param pen_end end number of pen
#' @param sep separator
#'
#' @return an array of piggery-unit-pen encoding
#' @export
#'
#' @examples
#' pen_simu("PH",1,6,1,2,1,45)
#' pen_simu("PH",7,8,3,5,1,20)
pen_simu <- function(piggery_id,piggery_begin,piggery_end,unit_begin,unit_end,pen_begin,pen_end,sep = "-"){
  pen <- NULL
  piggery_array <- substr(piggery_begin:piggery_end + 100,2,3)
  piggery_size <- length(piggery_array)
  unit_array <- substr(unit_begin:unit_end + 100,2,3)
  unit_size <- length(unit_array)
  pen_array <- substr(pen_begin:pen_end + 1000,2,4)
  pen_size <- length(pen_array)
  # for(i in 1:length(piggery_array)){
  #   for(j in 1:pen_size){
  #     tmp <- paste(piggery_id,piggery_array[i],unit_array[j],pen_array,sep = sep)
  #     pen <- c(pen,tmp)
  #   }
  # }
  res_piggery_id <- rep(piggery_id,piggery_size*unit_size*pen_size)
  res_piggery_array <-rep(piggery_array,each = unit_size*pen_size)
  res_unit_array <- rep(unit_array,each = pen_size,times = piggery_size)
  res_pen_array <- rep(pen_array,times = piggery_size*unit_size)
  pen <- paste(res_piggery_id,res_piggery_array,res_unit_array,res_pen_array,sep = sep)
  pen
}

#批量模拟栋舍-栏位
#' batch simulation of piggery-pen encoding
#'
#' @param piggery_id abbreviation of piggery code
#' @param piggery_begin begin number of piggery
#' @param piggery_end end number of piggery
#' @param pen_begin begin number of pen
#' @param pen_end end number of pen
#' @param sep separator
#'
#' @return an array of piggery-pen encoding
#' @export
#'
#' @examples
#' pen_simu2("PH",1,4,1,60)
pen_simu2 <- function(piggery_id,piggery_begin,piggery_end,pen_begin,pen_end,sep = "-"){
  pen <- NULL
  piggery_array <- substr(piggery_begin:piggery_end + 100,2,3)
  pen_array <- substr(pen_begin:pen_end + 1000,2,4)
  for(i in 1:length(piggery_array)){
    tmp <- paste(piggery_id,piggery_array[i],pen_array,sep = sep)
    pen <- c(pen,tmp)
  }
  pen
}

# 批量生成栋舍信息
#' batch simulation dataframe of piggery information
#'
#' @param piggery_type_id abbreviation of piggery code
#' @param piggery_begin begin number of piggery
#' @param piggery_end end number of piggery
#' @param piggery_name_begin begin name of piggery
#' @param piggery_name_end end name of piggery
#' @param piggery_type abbreviation of piggery type code
#' @param branch_id digital code of branch
#' @param sep separator
#'
#' @return a dataframe of piggery information
#' @export
#'
#' @examples
#' piggery_simu("PH",2,18,"配怀","栋","tmp","pigfarm")
piggery_simu <- function(piggery_type_id,piggery_begin,piggery_end,piggery_name_begin,piggery_name_end,piggery_type,branch_id,sep = "-"){
  piggery_array <- substr(piggery_begin:piggery_end + 100,2,3)
  piggery_id <- paste(piggery_type_id,piggery_array,sep = sep)
  piggery_name <- paste0(piggery_name_begin,piggery_begin:piggery_end,piggery_name_end)
  pinggery_length <- length(piggery_array)
  piggery_type <- rep(piggery_type,pinggery_length)
  branch_id <- rep(branch_id,pinggery_length)
  uuid <- uuid::UUIDgenerate(n = pinggery_length)
  piggery <- cbind.data.frame(uuid,piggery_type,branch_id,piggery_id,piggery_name)
  piggery
}
