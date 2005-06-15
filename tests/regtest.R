
library(modeltools)

d <- data.frame(x = rnorm(100), y = rnorm(100), z = runif(100))
d[["x"]][1:10] <- NA

a <- linearModel@dpp(y ~ x + z - 1, data = d, na.action = na.pass)
b <- na.omit(a)
mod1 <- linearModel@fit(b)

mod2 <- lm(y ~ x + z - 1, data = d)

nd <- data.frame(x = rnorm(100), z = runif(100))

stopifnot(identical(mod1$predict_response(nd), predict(mod2, newdata = nd)))

stopifnot(identical(coef(mod1), coef(mod2)))

u <- linearModel@fit
system.time(for (i in 1:100) mod1 <- u(b))
system.time(for (i in 1:100) mod2 <- lm(y ~ x + z - 1, data = d))

dn <- data.frame(x = rnorm(100), y = rnorm(100), z = runif(100))
all.equal(Predict(mod1, dn), Predict(mod2, dn))

system.time(for (i in 1:100) p1 <- Predict(mod1, dn))
system.time(for (i in 1:100) p2 <- Predict(mod2, dn))

system.time(for (i in 1:100) p1 <- predict(mod1, dn))
system.time(for (i in 1:100) p2 <- predict(mod2, dn))

