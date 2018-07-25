###########################
## Partial Correlation
# Script: Neylson Crepalde
###########################

summary_corrp = function(object) {
  if (class(object) != "PartialCorrelation") stop("Object must be of class PartialCorrelation")
  cat(paste0("Partial Correlation between '", object$terms[1], "' and '", object$terms[2],
            "'\n controlling for '", object$terms[3],"':\n"))
  cat(object$partial_correlation)
}



corrp = function(y, x, z, data) {
  if (class(y) != "character") stop("y must be a character")
  if (class(x) != "character") stop("x must be a character")
  if (class(z) != "character") stop("z must be a character")
  
  regX = lm(as.formula(paste(x, "~", z)), data = data)
  resX = regX$residuals
  regY = lm(as.formula(paste(y, "~", z)), data = data)
  resY = regY$residuals
  pcor = cor(resX, resY)
  
  res = list(regX = regX,
             regY = regY,
             partial_correlation = pcor,
             terms = c(y,x,z))
  class(res) = "PartialCorrelation"
  return(res)
}
