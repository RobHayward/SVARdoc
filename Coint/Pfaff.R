# This is a test of the vignette
library("vars")
data(Canada)
summary(Canada)
plot(Canada, nc = 2, xlab = "")
adf1 <- summary(ur.df(Canada[, "prod"], type = "trend", lags = 2))
adf1
adf2 <- summary(ur.df(diff(Canada[, "prod"]), type = "drift",
                      lags = 1))
adf2
summary(ca.jo(Canada, type = "trace", ecdet = "trend", K = 3,
              spec = "transitory"))
summary(ca.jo(Canada, type = "trace", ecdet = "trend", K = 2,
              spec = "transitory"))
# These provide the summaries for the tests with 3 and 2 lags respectively.
str(Canada)
vecm <- ca.jo(Canada[, c("rw", "prod", "e", "U")], type = "trace",
              ecdet = "trend", K = 3, spec = "transitory")
vecm.r1 <- cajorls(vecm, r = 1)
vecm.r1
vecm <- ca.jo(Canada[, c("prod", "e", "U", "rw")], type = "trace",
                 ecdet = "trend", K = 3, spec = "transitory")
SR <- matrix(NA, nrow = 4, ncol = 4)
SR[4, 2] <- 0
LR <- matrix(NA, nrow = 4, ncol = 4)
LR[1, 2:4] <- 0
LR[2:4, 4] <- 0
svec <- SVEC(vecm, LR = LR, SR = SR, r = 1, lrtest = FALSE, boot = TRUE,
                runs = 100)
summary(svec)
LR[3, 3] <- 0
svec.oi <- update(svec, LR = LR, lrtest = TRUE, boot = FALSE)
svec.oi$LRover
svec.irf <- irf(svec, response = "U", n.ahead = 48, boot = TRUE)
plot(svec.irf)
fevd.U <- fevd(svec, n.ahead = 48)$U
head(fevd.U)