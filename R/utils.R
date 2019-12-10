#' Scalling
#' Scalling of a numeric vector
#' @param x The numeric vector
#' @export
#' @importFrom stats sd
#' @rdname Scales
Scales<-function(x){
  return((x-mean(x))/stats::sd(x))
}
NULL


#' Normalization
#' Normalization of a numeric vector
#' @param x The numeric vector
#' @export
#' @rdname Norm
Normalization <-function(x){
  return((x-min(x))/(max(x)-min(x)))
}
NULL


#' Format number with comma
#' Format numbers with comma
#' @param x The vector
#' @importFrom scales format_format
#' @export
virgula_format <- function(x){
  return(scales::number(x,
                        big.mark = ".",
                        decimal.mark = ",",
                        scientific = FALSE,
                        prefix = "",
                        suffix = "")
         )
}
NULL

#' Format as Brazilian Real
#' Format numbers as Brazilian Reais
#' @param x The vector
#' @importFrom scales format_format
#' @export
real_format <- function(x){
  return(scales::format_format(x,
                        big.mark = ".",
                        decimal.mark = ",",
                        prefix = "R$",
                        suffix = "",
                        scientific = FALSE)
  )
}
NULL
