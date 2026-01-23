# build a dataset with 2 frequencies and some noise
# fit a sine wave to the longest frequency component, subtract that out
# from there, use L-S or another method to detect smaller frequencies

### INPUTS ##################################################
# define model starting values (getting these close is very important)
fr <- 1/200 #frequency - an estimate based on what we know (i.e. annual cycle)
            #this data's true freq = 1/250
phi <- 0 #phase shift, 0 is fine to start

# note amplitude is defined later = (max(y)-min(y))/2


### end inputs ##############################################


## BUILD DATA, INITIAL DATA PLOT, START RESULTS DF
x <- seq(0, 500, 1) #time axis
# true values for built data: 
A_true <- 10        #true amplitude of the longest wave
fr_true <- 1/250   #true freq of longest wave
phi_true <- pi/2   #true phase offset
d_true <- 0
y <- A_true * sin(2 * pi * fr_true * x + phi_true) +
  4 * sin(2 * pi * 1/30 * x) + rnorm(length(x), mean = 0, sd = 1)

# starting values for models
A <- (max(y)-min(y))/2 #amplitude
d <- mean(y) #vertical offset

plot(x, y, cex = 0.8, pch = 19, 
     main = "Raw data")

results <- data.frame(
  amplitude = c(A_true, A), 
  frequency = c(fr_true, fr), 
  phase = c(phi_true, phi), 
  vert_offset = c(d_true, d), 
  row.names = c("true values", "starting values")
)

## USE NLS() WITH PHI
model <- y ~ A * sin(2 * pi * fr * x + phi) + d

fit <- nls(model, 
           start = list(
             A = A, 
             fr = fr, 
             phi = phi, 
             d = d
           ))
#summary(fit)
#coef(fit)
#1/coef(fit)["fr"]
results <- rbind(results, model_with_phi = coef(fit))
  
# fitted values
pred_values <- predict(fit)

# subtract that out
resid <- y - pred_values

# plot
plot(x, y, cex = 0.8, pch = 19, 
     main = "Model fits (red/orange) \nand after residuals are subtracted (blue/green)")
lines(x, pred_values, col = "red", lwd = 3)
lines(x, resid, col = "lightblue", lwd = 3)


## USE NLS() WITHOUT PHI
model2 <- y ~ b * sin(2*pi*fr*x) + c * cos(2*pi*fr*x) + d

fit2 <- nls(model2, 
            start = list(
              b = A,
              c = 0, 
              fr = fr, 
              d = d))

coef(fit2)
b <- coef(fit2)["b"]
c <- coef(fit2)["c"]
A2 <- sqrt(b^2 + c^2)
phi2 <- atan2(c, b)
results <- rbind(results, model_without_phi = c(A2, 
                                                coef(fit2)["fr"], 
                                                phi2,
                                                coef(fit2)["d"]))

# fitted values
pred_values2 <- predict(fit2)

# subtract that out
resid2 <- y - pred_values2

# plot
lines(x, pred_values2, col = "orange", lwd = 1)
lines(x, resid2, col = "darkgreen", lwd = 1)
legend("topright", 
       legend = c("Model using phi = red/blue", "Without phi = orange/green"), 
       bty = "n")

print(c("MODELS USED:", model, model2))
print(results)
