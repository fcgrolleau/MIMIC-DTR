defactorize <- function(x)
{
  if(class(x)=="factor" & !is.na(as.numeric(as.character(x))[1])) {
      temp <- as.numeric(as.character(x))
  } else { 
      temp <- x
}
}

# Defactorize a dataset as follows
#dat <- as.data.frame(lapply(dat, defactorize))
