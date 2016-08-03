### .irf function
.irf <- function (x, impulse, response, y.names, n.ahead, ortho, cumulative) 
{
  if ((class(x) == "varest") || (class(x) == "vec2var")) {
    if (ortho) {
      irf <- Psi(x, nstep = n.ahead)
    }
    else {
      irf <- Phi(x, nstep = n.ahead)
    }
  }
  else if ((class(x) == "svarest") || (class(x) == "svecest")) {
    irf <- Phi(x, nstep = n.ahead)
  }
  dimnames(irf) <- list(y.names, y.names, NULL)
  idx <- length(impulse)
  irs <- list()
  for (i in 1:idx) {
    irs[[i]] <- matrix(t(irf[response, impulse[i], 1:(n.ahead + 
                                                        1)]), nrow = n.ahead + 1)
    colnames(irs[[i]]) <- response
    if (cumulative) {
      if (length(response) > 1) 
        irs[[i]] <- apply(irs[[i]], 2, cumsum)
      if (length(response) == 1) {
        tmp <- matrix(cumsum(irs[[1]]))
        colnames(tmp) <- response
        irs[[1]] <- tmp
      }
    }
  }
  names(irs) <- impulse
  result <- irs
  return(result)
}  