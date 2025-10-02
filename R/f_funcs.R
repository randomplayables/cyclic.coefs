sin_mod <- function(inputs =seq(0,2*pi,length.out=16),
                    period = 2*pi,
                    nan.replace = NULL,
                    nan.remove = FALSE){
 res = sin(period %% inputs)
 if (!is.null(nan.replace)) res[is.nan(res)] <- nan.replace
 if (nan.remove) res <- res[!is.nan(res)]
 return(res)
}

cos_mod <- function(inputs =seq(0,2*pi,length.out=16),
                    period = 2*pi,
                    nan.replace = NULL,
                    nan.remove = FALSE){
  res = cos(period %% inputs)
  if (!is.null(nan.replace)) res[is.nan(res)] <- nan.replace
  if (nan.remove) res <- res[!is.nan(res)]
  return(res)
}
