ld <- function(subjects, responding, dose, ldP=0.5, linkFunction='probit') {
  dead <- responding
  live <- subjects - dead
  
  # fitting generalized linear model (error distribution is binomial, and link function is probit)
  model <- glm(cbind(dead, live) ~ dose, family=binomial(link=linkFunction))
  
  # predict doses for binomial assay model
  ldDose <- dose.p(model, p=c(ldP))
  
  # return back results
  x   <- ldDose[[1]]
  se  <- attr(ldDose, "SE")[[1]]
  a   <- summary(model)$coefficients[1,1]
  b   <- summary(model)$coefficients[2,1]
  ase <- summary(model)$coefficients[1,2]
  bse <- summary(model)$coefficients[2,2]
  
  list('x'=x, 'se'=se, 'a'=a, 'ase'=ase, 'b'=b, 'bse'=bse)
}