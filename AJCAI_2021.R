# R source code for the paper Contextual Importance and Utility: a Theoretical
# Foundation" by Kary Främling, published at the AJCAI 2021 conference.
# If all required libraries are installed correctly, then doing "source" on
# this file should produce all the results and figures in the paper.

library(MASS)
library(caret)
library(lime)
library(ciu)
library(iml)
library(ggplot2)
library(plot3D)

# ciu library Version 0.5.0 or higher should be used.
# Alternatively, use Github version at https://github.com/KaryFramling/ciu.

# Simple weighted sum example
# ===========================
linfunc <- function(m, inp) { 1*inp[,1] + 1*inp[,2] }
c <- data.frame(x1=c(0,0,1,1), x2=c(0,1,0,1))
ciu <- ciu.new(NULL, in.min.max.limits = matrix(c(0,0,1,1), ncol=2), abs.min.max=matrix(c(0,2), ncol=2),
               input.names=c("x1","x2"), output.names = c("y"),
               predict.function = linfunc)
for ( i in 1:nrow(c) ) {
  print(ciu$explain(c[i,], ind.inputs.to.explain = 1))
  print(ciu$explain(c[i,], ind.inputs.to.explain = 2))
}

# OR function. Attention to NA values for CU, they are now returned as zero!
# ==========================================================================
orfunc <- function(m, inp) { inp[,1] | inp[,2] }
c <- data.frame(x1=c(0,0,1,1), x2=c(0,1,0,1))
ciu <- ciu.new(NULL, in.min.max.limits = matrix(c(0,0,1,1), ncol=2), abs.min.max=matrix(c(0,1), ncol=2),
               input.names=c("x1","x2"), output.names = c("y"),
               predict.function = orfunc)
for ( i in 1:nrow(c) ) {
  print(ciu$explain(c[i,], ind.inputs.to.explain = 1))
  print(ciu$explain(c[i,], ind.inputs.to.explain = 2))
}

# XOR function
# ================================
xorfunc <- function(m, inp) { xor(inp[,1],inp[,2]) }
c <- data.frame(x1=c(0,0,1,1), x2=c(0,1,0,1))
ciu <- ciu.new(NULL, in.min.max.limits = matrix(c(0,0,1,1), ncol=2), abs.min.max=matrix(c(0,1), ncol=2),
               input.names=c("x1","x2"), output.names = c("y"),
               predict.function = xorfunc)
for ( i in 1:nrow(c) ) {
  print(ciu$explain(c[i,], ind.inputs.to.explain = 1))
  print(ciu$explain(c[i,], ind.inputs.to.explain = 2))
}

# Then to slightly more complex examples.
# =======================================

# Common text sizes in all ggplots by own theme.
own_theme = theme(
  plot.title = element_text(size = 18),
  axis.title.x = element_text(size = 18),
  axis.text = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  strip.text = element_text(size=16)
)

# Function for having only two decimals max in plots.
scaleFUN <- function(x) as.character(round(x, digits = 2))

# Fixed functions and results based on them.
# ==========================================
# Simple weighted sum
# ===================
x <- y <- seq(0, 1, 0.05)
pm <- as.matrix(expand.grid(x,y))
pm_df <- data.frame(x1=pm[,1],x2=pm[,2])

linfunc <- function(m, inp) { 0.3*inp[,1] + 0.7*inp[,2] }
c <- data.frame(x1=0.7, x2=0.8)
c1 <- data.frame(x1=0.5, x2=0.5)
z <- linfunc(NULL, pm)
zm <- matrix(z, nrow = length(x), byrow = TRUE)
vt <- persp(x, y, zm, xlab="x1", ylab="x2", zlab="y", theta = -35, phi = 15, ticktype = "detailed") # persp3D might want these: , bg="white", colvar=NULL, col="black", facets=FALSE
points(trans3d(c$x1, c$x2, linfunc(NULL, c), pmat = vt), col = "red", pch = 16, cex = 3)
points(trans3d(c1$x1, c1$x2, linfunc(NULL, c1), pmat = vt), col = "red", pch = 16, cex = 3)

# CIU
# Function for generating 2D plots with CIU visualisation. Weighted sum.
linear.ciu.2D <- function(ciu, c.inps) {
  par(mfrow=c(2,1))
  par(mar=c(4, 4, 2, 2) + 0.1)
  outp <- linfunc(NULL, as.matrix(c.inps))
  # First plot
  ciu$plot.ciu(c.inps,1,main=paste0("Weighted Sum, x2=", c.inps$x2))
  absmin <- 0.0
  absmax <- 1.0
#  text(x=c(0,0), y=c(absmin+0.02,absmax-0.02),c("absmin = 0.0","absmax = 1.0"),col="blue", pos=4)
  cmin <- linfunc(NULL,matrix(c(0,c.inps$x2), nrow=1))
  cmax <- linfunc(NULL,matrix(c(1,c.inps$x2), nrow=1))
  text(x=c(1), y=c(cmin+0.02),c(paste("ymin = ", format(cmin, digits=3))),col="red", pos=2)
  text(x=c(1), y=c(cmax+0.02),c(paste("ymax = ", format(cmax, digits=3))),col="darkgreen", pos=2)
  text(x=c.inps$x1-0.2, y=outp,c(paste("out =", format(outp, digits=3))),col="blue",pos=2)
  arrows(x0=c.inps$x1-0.2,y0=outp,x1=c.inps$x1+0.01,col="red")
  lines(c(0,1), c(absmax,absmax), col="blue", lty=2)
  lines(c(0,1), c(absmin,absmin), col="blue", lty=2)
  lines(c(0,1), c(cmax,cmax), col="darkgreen", lty=2)
  lines(c(0,1), c(cmin,cmin), col="red", lty=2)
  neutral.cu <-cmin+(cmax-cmin)/2
  text(x=c(1), y=c(neutral.cu-0.02),"neutral.CU",col="orange", pos=2)
  lines(c(0,1), c(neutral.cu,neutral.cu), col="orange", lty=2)
  ci <- (cmax-cmin)/(absmax-absmin)
  cu <- (outp-cmin)/(cmax-cmin)
  text(x=c(0), y=c(0.2+0.05),paste0("CI = ", ci),col="black", pos=4, cex=1.5)
  text(x=c(0), y=c(0.2-0.05),paste0("CU = ", cu),col="black", pos=4, cex=1.5)
  # Second plot
  ciu$plot.ciu(c.inps,2,main=paste0("Weighted Sum, x1=", c.inps$x1))
#  text(x=c(0,0), y=c(absmin+0.02,absmax-0.02),c("absmin = 0.0","absmax = 1.0"),col="blue", pos=4)
  cmin <- linfunc(NULL,matrix(c(c.inps$x1,0), nrow=1))
  cmax <- linfunc(NULL,matrix(c(c.inps$x1,1), nrow=1))
  text(x=c(1), y=c(cmin+0.02),c(paste("ymin = ", format(cmin, digits=3))),col="red", pos=2)
  text(x=c(1), y=c(cmax+0.02),c(paste("ymax = ", format(cmax, digits=3))),col="darkgreen", pos=2)
  text(x=c.inps$x2-0.15, y=outp,c(paste("out =", format(outp, digits=3))),col="blue",pos=2)
  arrows(x0=c.inps$x2-0.15,y0=outp,x1=c.inps$x2+0.01,col="red")
  lines(c(0,1), c(absmax,absmax), col="blue", lty=2)
  lines(c(0,1), c(absmin,absmin), col="blue", lty=2)
  lines(c(0,1), c(cmax,cmax), col="darkgreen", lty=2)
  lines(c(0,1), c(cmin,cmin), col="red", lty=2)
  neutral.cu <-cmin+(cmax-cmin)/2
  text(x=c(1), y=c(neutral.cu-0.02),"neutral.CU",col="orange", pos=2)
  lines(c(0,1), c(neutral.cu,neutral.cu), col="orange", lty=2)
  ci <- (cmax-cmin)/(absmax-absmin)
  cu <- (outp-cmin)/(cmax-cmin)
  text(x=c(0), y=c(0.67+0.05),paste0("CI = ", ci),col="black", pos=4, cex=1.5)
  text(x=c(0), y=c(0.67-0.05),paste0("CU = ", cu),col="black", pos=4, cex=1.5)
  par(mar=c(5, 4, 4, 2) + 0.1)
  par(mfrow=c(1,1))
}
ciu <- ciu.new(NULL, in.min.max.limits = matrix(c(0,0,1,1), ncol=2), abs.min.max=matrix(c(0,1), ncol=2),
               input.names=c("x1","x2"), output.names = c("y"),
               predict.function = linfunc)
linear.ciu.2D(ciu, c)
p <- ciu$ggplot.col.ciu(c) + labs(title="", x ="", y="CI", fill="CU") + own_theme; print(p)
p <- ciu$ggplot.col.ciu(c, use.influence=TRUE, low.color = "firebrick", high.color = "steelblue")
p <- p + labs(title="", x ="", y = expression(phi)) + own_theme + scale_y_continuous(labels=scaleFUN)
print(p)
linear.ciu.2D(ciu, c1)
p <- ciu$ggplot.col.ciu(c1) + labs(title="", x ="", y="CI", fill="CU") + own_theme; print(p)
p <- ciu$ggplot.col.ciu(c1, use.influence=TRUE, low.color = "firebrick", high.color = "steelblue")
p <- p + labs(title="", x ="", y = expression(phi)) + own_theme + scale_y_continuous(labels=scaleFUN)
print(p)
print(ciu$explain(c,1))
print(ciu$explain(c,2))

# LIME
predict_model.function <- function(x, newdata, type, ...) {
  res <- data.frame(y=linfunc(x, newdata, ...))
}
model_type.function <- function(x, ...) 'regression'
linear.train <- data.frame(x1=pm[,1],x2=pm[,2],y=linfunc(m, pm)) # LIME&SHAP require a training data set
explainer <- lime(linear.train,linfunc)
explanation <- lime::explain(c, explainer, n_features=2)
p <- lime::plot_features(explanation);
d.lime <- data.frame(feature=explanation$feature_desc,class="y",phi=explanation$feature_weight,feature.value=explanation$feature_value)
d.lime$sign <- d.lime$phi>=0
p <- ggplot(d.lime) +
  geom_col(aes(reorder(feature, phi), phi, fill=sign)) +
  coord_flip() +
  labs(title=paste0("Explanation fit: ", scaleFUN(explanation$model_r2))) +
  scale_fill_manual("legend", values = c("FALSE" = "firebrick", "TRUE" = "steelblue")) +
  theme(legend.position = "none") + labs(x ="", y = expression(phi)) + own_theme
print(p)
explanation <- lime::explain(c1, explainer, n_features=2)
p <- lime::plot_features(explanation);
d.lime <- data.frame(feature=explanation$feature_desc,class="y",phi=explanation$feature_weight,feature.value=explanation$feature_value)
d.lime$sign <- d.lime$phi>=0
p <- ggplot(d.lime) +
  geom_col(aes(reorder(feature, phi), phi, fill=sign)) +
  coord_flip() +
  labs(title=paste0("Explanation fit: ", scaleFUN(explanation$model_r2))) +
  scale_fill_manual("legend", values = c("FALSE" = "firebrick", "TRUE" = "steelblue")) +
  theme(legend.position = "none") + labs(x ="", y = expression(phi)) + own_theme
print(p)
print(explanation)

# Shapley
predict.function <- function(x, newdata, type, ...) {
  res <- data.frame(y=linfunc(x, newdata, ...))
}
predictor <- Predictor$new(linfunc, data = linear.train[,-3], y = linear.train$y)
shapley <- Shapley$new(predictor, x.interest = c)
p <- shapley$plot() + labs(y = expression(phi))# + own_theme + scale_y_continuous(labels=scaleFUN)
d <- p$data
d$sign <- d$phi>=0
pnew <- ggplot(d) + geom_col(aes(x=reorder(feature.value,phi), y=phi, fill=sign)) +
  coord_flip() + labs(title=p$labels$title) +
  labs(x ="", y = expression(phi)) + theme(legend.position = "none") +
  own_theme + scale_fill_manual("legend", values = c("FALSE" = "firebrick", "TRUE" = "steelblue"))
print(pnew)
shapley <- Shapley$new(predictor, x.interest = c1)
p <- shapley$plot() + labs(y = expression(phi))# + own_theme + scale_y_continuous(labels=scaleFUN)
d <- p$data
d$sign <- d$phi>=0
pnew <- ggplot(d) + geom_col(aes(x=reorder(feature.value,phi), y=phi, fill=sign)) +
  coord_flip() + labs(title=p$labels$title) +
  labs(x ="", y = expression(phi)) + theme(legend.position = "none") +
  own_theme + scale_fill_manual("legend", values = c("FALSE" = "firebrick", "TRUE" = "steelblue"))
print(pnew)

# Sensitivity analysis. Not very elegant but does the work. Shapley takes a lot of time!
# Probably not used in paper but good to have anyways.
n.sens <- 100
m <- matrix(as.numeric(c1), nrow=n.sens, ncol=ncol(c1), byrow=TRUE)
sensitivity.inpmat <- data.frame(x1=m[,1],x2=m[,2])
CIU_sens_1 <- ciu$explain(sensitivity.inpmat,ind.inputs.to.explain = 1)
CIU_sens_2 <- ciu$explain(sensitivity.inpmat,ind.inputs.to.explain = 2)
Cinfl_sens1 <- 2*CIU_sens_1$CI*(CIU_sens_1$CU - 0.5)
Cinfl_sens2 <- 2*CIU_sens_2$CI*(CIU_sens_2$CU - 0.5)
Shapley_sens <- matrix(0, nrow=n.sens, ncol=2)
for ( i in 1:n.sens){
  s <- Shapley$new(predictor, x.interest = sensitivity.inpmat[i,])
  Shapley_sens[i,] <- s$results$phi
}
Shapley_sens <- data.frame(phi_1=Shapley_sens[,1],phi_2=Shapley_sens[,2])
p <- ggplot(Shapley_sens, aes(x=phi_1)) + geom_density() +
  geom_density(aes(x=phi_2)) +
  geom_vline(aes(xintercept=mean(phi_1, na.rm=T)), color="red", linetype="dashed", size=1) +
  geom_vline(aes(xintercept=mean(phi_2, na.rm=T)), color="yellow", linetype="dashed", size=1)
print(p)
sd(Shapley_sens$phi_1)
sd(Shapley_sens$phi_2)
sens2 <- data.frame(var=c(rep("phi 1", n.sens),rep("phi 2",n.sens)),
                    val=c(Shapley_sens$phi_1, Shapley_sens$phi_2),
                    val.mean = c(mean(Shapley_sens$phi_1), mean(Shapley_sens$phi_2)))
p <- ggplot(sens2, aes(x=val, colour=var)) + geom_density() +
  geom_vline(data=sens2, aes(xintercept=val.mean,  colour=var),
             linetype="dashed", size=1)
print(p)

# "Rule-based" function
# =====================
rulefunc <- function(m, inp) {
  y <- matrix(0.0, nrow=nrow(inp)) # Default value
  y[inp[,1]>0.1] <- 0.2
  y[inp[,1]>0.3 & inp[,2]>0.1] <- 0.6
  y[inp[,1]>0.5 & inp[,2]>0.55 & inp[,2]<0.85] <- 1.0
  y[inp[,2]<0.5 & inp[,1]>0.25 & inp[,1]<0.5] <- 0.4
  y[inp[,1]>0.8 & inp[,2]>0.1 & inp[,2]<0.5] <- 0.3
  ym <- matrix(y,nrow=nrow(inp))
  return(ym)
}
c <- data.frame(x1=0.7, x2=0.4)
z <- rulefunc(NULL, pm)
zm <- matrix(z,nrow=length(x))
vt <- hist3D(x,y,z=zm, xlab="x1", ylab="x2", zlab="y", xlim=c(0,1), ylim=c(0,1), zlim=c(0,1), ticktype = "detailed", theta = -35, phi = 15, bg="white", colvar=NULL, col="black", facets=FALSE)
points(trans3d(c$x1, c$x2, rulefunc(NULL, c), pmat = vt), col = "red", pch = 16, cex = 3)

# CIU
# Function for generating 2D plots with CIU visualisation. Rules.
rules.ciu.2D <- function(c.inps) {
  par(mfrow=c(2,1))
  par(mar=c(4, 4, 2, 2) + 0.1)
  outp <- rulefunc(NULL, as.matrix(c.inps))
  # First plot
  ciu$plot.ciu(c,1,main=paste0("Rules, x2=", c.inps$x2))
  absmin <- 0.0
  absmax <- 1.0
#  text(x=c(0,0), y=c(absmin+0.02,absmax-0.02),c("absmin = 0.0","absmax = 1.0"),col="blue", pos=4)
  cmin <- rulefunc(NULL,matrix(c(0,c.inps$x2), nrow=1))
  cmax <- rulefunc(NULL,matrix(c(0.6,c.inps$x2), nrow=1))
  #text(x=c(1,1), y=c(cmin+0.02,cmax+0.03),c(paste("Cmin = ", format(cmin, digits=3)), paste("ymax = ", format(cmax, digits=3))),col="blue", pos=2)
  text(x=c(1), y=c(cmin+0.02),c(paste("ymin = ", format(cmin, digits=3))),col="red", pos=2)
  text(x=c(1), y=c(cmax+0.03),c(paste("ymax = ", format(cmax, digits=3))),col="darkgreen", pos=2)
  text(x=c.inps$x1-0.2, y=outp+0.05,c(paste("out =", format(outp, digits=3))),col="blue",pos=2)
  arrows(x0=c.inps$x1-0.2,y0=outp+0.05,x1=c.inps$x1+0.01,y1=outp,col="red")
  lines(c(0,1), c(absmax,absmax), col="blue", lty=2)
  lines(c(0,1), c(absmin,absmin), col="blue", lty=2)
  lines(c(0,1), c(cmax,cmax), col="darkgreen", lty=2)
  lines(c(0,1), c(cmin,cmin), col="red", lty=2)
  neutral.cu <-cmin+(cmax-cmin)/2
  text(x=c(1), y=c(neutral.cu-0.02),"neutral.CU",col="orange", pos=2)
  lines(c(0,1), c(neutral.cu,neutral.cu), col="orange", lty=2)
  ci <- (cmax-cmin)/(absmax-absmin)
  cu <- (outp-cmin)/(cmax-cmin)
  text(x=c(0), y=c(0.8+0.05),paste0("CI = ", ci),col="black", pos=4, cex=1.5)
  text(x=c(0), y=c(0.8-0.05),paste0("CU = ", cu),col="black", pos=4, cex=1.5)
  # Second plot
  ciu$plot.ciu(c,2,main=paste0("Rules, x1=", c.inps$x1))
#  text(x=c(0,0), y=c(absmin+0.02,absmax-0.02),c("absmin = 0.0","absmax = 1.0"),col="blue", pos=4)
  cmin <- rulefunc(NULL,matrix(c(c.inps$x1,0), nrow=1))
  cmax <- rulefunc(NULL,matrix(c(c.inps$x1,0.8), nrow=1))
  #text(x=c(1,1), y=c(cmin+0.02,cmax-0.02),c(paste("Cmin = ", format(cmin, digits=3)), paste("ymax = ", format(cmax, digits=3))),col="blue", pos=2)
  text(x=c(1), y=c(cmin+0.02),c(paste("ymin = ", format(cmin, digits=3))),col="red", pos=2)
  text(x=c(1), y=c(cmax-0.02),c(paste("ymax = ", format(cmax, digits=3))),col="darkgreen", pos=2)
  text(x=c.inps$x2+0.1, y=outp-0.2,c(paste("out =", format(outp, digits=3))),col="blue",pos=1)
  arrows(x0=c.inps$x2+0.1,y0=outp-0.2,x1=c.inps$x2+0.01,y1=outp,col="red")
  lines(c(0,1), c(absmax,absmax), col="blue", lty=2)
  lines(c(0,1), c(absmin,absmin), col="blue", lty=2)
  lines(c(0,1), c(cmax,cmax), col="darkgreen", lty=2)
  lines(c(0,1), c(cmin,cmin), col="red", lty=2)
  neutral.cu <-cmin+(cmax-cmin)/2
  text(x=c(1), y=c(neutral.cu-0.02),"neutral.CU",col="orange", pos=2)
  lines(c(0,1), c(neutral.cu,neutral.cu), col="orange", lty=2)
  ci <- (cmax-cmin)/(absmax-absmin)
  cu <- (outp-cmin)/(cmax-cmin)
  text(x=c(0), y=c(0.8+0.05),paste0("CI = ", ci),col="black", pos=4, cex=1.5)
  text(x=c(0), y=c(0.8-0.05),paste0("CU = ", cu),col="black", pos=4, cex=1.5)
  par(mar=c(5, 4, 4, 2) + 0.1)
  par(mfrow=c(1,1))
}
ciu <- ciu.new(NULL, in.min.max.limits = matrix(c(0,0,1,1), ncol=2), abs.min.max=matrix(c(0,1), ncol=2),
               input.names=c("x1","x2"), output.names = c("y"),
               predict.function = rulefunc)
rules.ciu.2D(c)
p <- ciu$ggplot.col.ciu(c) + labs(title="", x ="", y="CI", fill="CU") + own_theme; print(p)
p <- ciu$ggplot.col.ciu(c, use.influence=TRUE, low.color = "firebrick", high.color = "steelblue")
p <- p + labs(title="", x ="", y = expression(phi)) + own_theme + scale_y_continuous(labels=scaleFUN)
print(p)
print(ciu$explain(c,1))
print(ciu$explain(c,2))

# LIME
predict_model.function <- function(x, newdata, type, ...) {
  res <- data.frame(y=rulefunc(x, newdata, ...))
}
model_type.function <- function(x, ...) 'regression'
rule.train <- data.frame(x1=pm[,1],x2=pm[,2],y=rulefunc(m, pm)) # LIME&SHAP require a training data set
explainer <- lime(rule.train,rulefunc)
explanation <- lime::explain(c, explainer, n_features=2)
p <- lime::plot_features(explanation)
d.lime <- data.frame(feature=explanation$feature_desc,class="y",phi=explanation$feature_weight,feature.value=explanation$feature_value)
d.lime$sign <- d.lime$phi>=0
p <- ggplot(d.lime) +
  geom_col(aes(reorder(feature, phi), phi, fill=sign)) +
  coord_flip() +
  labs(title=paste0("Explanation fit: ", scaleFUN(explanation$model_r2))) +
  scale_fill_manual("legend", values = c("FALSE" = "firebrick", "TRUE" = "steelblue")) +
  theme(legend.position = "none") + labs(x ="", y = expression(phi)) + own_theme
print(p)
print(explanation)

# SHAP
predict.function <- function(x, newdata, type, ...) {
  res <- data.frame(y=rulefunc(x, newdata, ...))
}
rule.train <- data.frame(x1=pm[,1],x2=pm[,2],y=rulefunc(m, pm)) # LIME requires a training data set
predictor <- Predictor$new(rulefunc, data = rule.train[,-3], y = rule.train$y)
shapley <- Shapley$new(predictor, x.interest = c)
p <- shapley$plot() + labs(y = expression(phi))# + own_theme + scale_y_continuous(labels=scaleFUN)
d <- p$data
d$sign <- d$phi>=0
pnew <- ggplot(d) + geom_col(aes(x=reorder(feature.value,phi), y=phi, fill=sign)) +
  coord_flip() + labs(title=p$labels$title) +
  labs(x ="", y = expression(phi)) + theme(legend.position = "none") +
  own_theme + scale_fill_manual("legend", values = c("FALSE" = "firebrick", "TRUE" = "steelblue"))
print(pnew)

# Sombrero function
# =================
sombrerofunc <- function(m, inp) {
  tmp <- sqrt(inp[,1]^2 + inp[,2]^2)
  return (sin(tmp)/tmp)
}
c <- data.frame(x1=-7.5, x2=-1.5)
xsombrero <- ysombrero <- seq(-10, 10, 0.51)
pmsomb <- as.matrix(expand.grid(xsombrero,ysombrero))
sombrero.train <- data.frame(x1=pmsomb[,1],x2=pmsomb[,2],y=sombrerofunc(m, pmsomb)) # LIME&SHAP require a training data set
z <- sombrerofunc(NULL, pmsomb)
zm <- matrix(z, nrow = length(xsombrero), byrow = TRUE)
vt <- persp(xsombrero, ysombrero, zm, xlab="x1", ylab="x2", zlab="y", theta = -35, phi = 15, ticktype = "detailed") # persp3D might want these: , bg="white", colvar=NULL, col="black", facets=FALSE
points(trans3d(c$x1, c$x2, sombrerofunc(NULL, c), pmat = vt), col = "red", pch = 16, cex = 3)

# CIU
# Function for generating 2D plots with CIU visualisation. Rules.
sombrero.ciu.2D <- function(c.inps) {
  par(mfrow=c(2,1))
  par(mar=c(4, 4, 2, 2) + 0.1)
  absmin <- -0.217
  absmax <- 1.0
  outp <- sombrerofunc(NULL, as.matrix(c.inps))
  # First plot
  ciu$plot.ciu(c,1,main=paste0("Sombrero, x2=", c.inps$x2),ylim=c(-0.22,1))
#  text(x=c(-10,-10), y=c(absmin+0.02,absmax-0.02),c(paste0("absmin = ", absmin),paste0("absmax = ", absmax)),col="blue", pos=4)
  # cmin <- sombrerofunc(NULL,matrix(c(0,c.inps$x2), nrow=1))
  # cmax <- sombrerofunc(NULL,matrix(c(0.6,c.inps$x2), nrow=1))
  cmin <- -0.217
  cmax <- 0.664
  #text(x=c(10,10), y=c(cmin+0.02,cmax+0.03),c(paste("Cmin = ", format(cmin, digits=3)), paste("ymax = ", format(cmax, digits=3))),col="blue", pos=2)
  text(x=c(10), y=c(cmin+0.02),c(paste("ymin = ", format(cmin, digits=3))),col="red", pos=2)
  text(x=c(10), y=c(cmax+0.02),c(paste("ymax = ", format(cmax, digits=3))),col="darkgreen", pos=2)
  text(x=c.inps$x1-0.2, y=outp+0.1,c(paste("out =", format(outp, digits=3))),col="blue",pos=3)
  arrows(x0=c.inps$x1-0.2,y0=outp+0.1,x1=c.inps$x1+0.01,y1=outp,col="red")
  lines(c(-10,10), c(absmax,absmax), col="blue", lty=2)
  lines(c(-10,10), c(absmin,absmin), col="blue", lty=2)
  lines(c(-10,10), c(cmax,cmax), col="darkgreen", lty=2)
  lines(c(-10,10), c(cmin,cmin), col="red", lty=2)
  neutral.cu <-cmin+(cmax-cmin)/2
  text(x=c(10), y=c(neutral.cu-0.02),"neutral.CU",col="orange", pos=2)
  lines(c(-10,10), c(neutral.cu,neutral.cu), col="orange", lty=2)
  ci <- (cmax-cmin)/(absmax-absmin)
  cu <- (outp-cmin)/(cmax-cmin)
  text(x=c(-10), y=c(0.8+0.05),paste0("CI = ", format(ci, digits=3)),col="black", pos=4, cex=1.5)
  text(x=c(-10), y=c(0.8-0.05),paste0("CU = ", format(cu, digits=3)),col="black", pos=4, cex=1.5)
  # Second plot
  ciu$plot.ciu(c,2,main=paste0("Sombrero, x1=", c.inps$x1),ylim=c(-0.22,1))
#  text(x=c(-10,-10), y=c(absmin+0.02,absmax-0.02),c(paste0("absmin = ", absmin),paste0("absmax = ", absmax)),col="blue", pos=4)
  #cmin <- sombrerofunc(NULL,matrix(c(c.inps$x1,0), nrow=1))
  #cmax <- sombrerofunc(NULL,matrix(c(c.inps$x1,0.8), nrow=1))
  cmin <- -0.0912
  cmax <- 0.128
  #text(x=c(10,10), y=c(cmin+0.02,cmax-0.02),c(paste("Cmin = ", format(cmin, digits=3)), paste("ymax = ", format(cmax, digits=3))),col="blue", pos=2)
  text(x=c(10), y=c(cmin+0.02),c(paste("ymin = ", format(cmin, digits=3))),col="red", pos=2)
  text(x=c(10), y=c(cmax+0.02),c(paste("ymax = ", format(cmax, digits=3))),col="darkgreen", pos=2)
  text(x=c.inps$x2+0.1, y=outp+0.2,c(paste("out =", format(outp, digits=3))),col="blue",pos=3)
  arrows(x0=c.inps$x2+0.1,y0=outp+0.2,x1=c.inps$x2+0.01,y1=outp,col="red")
  lines(c(-10,10), c(absmax,absmax), col="blue", lty=2)
  lines(c(-10,10), c(absmin,absmin), col="blue", lty=2)
  lines(c(-10,10), c(cmax,cmax), col="darkgreen", lty=2)
  lines(c(-10,10), c(cmin,cmin), col="red", lty=2)
  neutral.cu <-cmin+(cmax-cmin)/2
  text(x=c(10), y=c(neutral.cu+0.02),"neutral.CU",col="orange", pos=2)
  lines(c(-10,10), c(neutral.cu,neutral.cu), col="orange", lty=2)
  ci <- (cmax-cmin)/(absmax-absmin)
  cu <- (outp-cmin)/(cmax-cmin)
  text(x=c(-10), y=c(0.8+0.05),paste0("CI = ", format(ci, digits=3)),col="black", pos=4, cex=1.5)
  text(x=c(-10), y=c(0.8-0.05),paste0("CU = ", format(cu, digits=3)),col="black", pos=4, cex=1.5)
  par(mar=c(5, 4, 4, 2) + 0.1)
  par(mfrow=c(1,1))
}
ciu <- ciu.new(NULL, in.min.max.limits = matrix(c(-10,-10,10,10), ncol=2), abs.min.max=matrix(c(-0.2172329,1), ncol=2),
               input.names=c("x1","x2"), output.names = c("y"),
               predict.function = sombrerofunc)
sombrero.ciu.2D(c)
p <- ciu$ggplot.col.ciu(c) + labs(title="", x ="", y="CI", fill="CU") + own_theme; print(p)
#p <- ciu$ggplot.col.ciu(c, use.influence=TRUE) + labs(title="", x ="", y = expression(phi)) + own_theme; print(p)
p <- ciu$ggplot.col.ciu(c, use.influence=TRUE, low.color = "firebrick", high.color = "steelblue")
p <- p + labs(title="", x ="", y = expression(phi)) + own_theme + scale_y_continuous(labels=scaleFUN)
print(p)
print(ciu$explain(c,1))
print(ciu$explain(c,2))
# See how precision evolves when increasing number of samples, here for
# both inputs together so CI should be 1 (if absminmax have right values).
print(ciu$explain(c,c(1,2),n.samples = 100))
print(ciu$explain(c,c(1,2),n.samples = 1000))
print(ciu$explain(c,c(1,2),n.samples = 10000))
print(ciu$explain(c,c(1,2),n.samples = 100000))

# LIME
predict_model.function <- function(x, newdata, type, ...) {
  res <- data.frame(y=sombrerofunc(x, newdata, ...))
}
model_type.function <- function(x, ...) 'regression'
explainer <- lime(sombrero.train,sombrerofunc)
explanation <- lime::explain(c, explainer, n_features=2)
p <- lime::plot_features(explanation)
# p <-p + labs(x ="", y = expression(phi)) +
#   theme(legend.position = "none") + own_theme + scale_y_continuous(labels=scaleFUN)
# print(p)
d.lime <- data.frame(feature=explanation$feature_desc,class="y",phi=explanation$feature_weight,feature.value=explanation$feature_value)
d.lime$sign <- d.lime$phi>=0
p <- ggplot(d.lime) +
  geom_col(aes(reorder(feature, phi), phi, fill=sign)) +
  coord_flip() +
  labs(title=paste0("Explanation fit: ", scaleFUN(explanation$model_r2))) +
  scale_fill_manual("legend", values = c("FALSE" = "firebrick", "TRUE" = "steelblue")) +
  theme(legend.position = "none") + labs(x ="", y = expression(phi)) + own_theme
print(p)
print(explanation)

# Shapley
predict.function <- function(x, newdata, type, ...) {
  res <- data.frame(y=sombrerofunc(x, newdata, ...))
}
predictor <- Predictor$new(sombrerofunc, data = sombrero.train[,-3], y = sombrero.train$y)
shapley <- Shapley$new(predictor, x.interest = c)
p <- shapley$plot() + labs(y = expression(phi))# + own_theme + scale_y_continuous(labels=scaleFUN)
d <- p$data
d$sign <- d$phi>=0
pnew <- ggplot(d) + geom_col(aes(x=reorder(feature.value,phi), y=phi, fill=sign)) +
  coord_flip() + labs(title=p$labels$title) +
  labs(x ="", y = expression(phi)) + theme(legend.position = "none") +
  own_theme + scale_fill_manual("legend", values = c("FALSE" = "firebrick", "TRUE" = "steelblue"))
print(pnew)


# Diamonds with GBM. This is a mixed discrete/continuous input case. Takes
# a long time for GBM to train! NOT USED IN THE PAPER.
diamonds.gbm <- function() {
  ind.y <- which(colnames(diamonds)=="price")
  kfoldcv <- trainControl(method="cv", number=10)
  diamonds.gbm <<- train(price~., diamonds, method="gbm", trControl=kfoldcv)
  inst.ind <- 27750 # An expensive one!
  instance <- diamonds[inst.ind,-ind.y]
  ciu <- ciu.new(diamonds.gbm, price~., diamonds)
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  par(mfrow=c(1,1))
  #ciu$barplot.ciu(instance, sort="CI")
  p <- ciu$ggplot.col.ciu(instance) + labs(title="", x ="", y="CI", fill="CU") + own_theme; print(p)
  p <- ciu$ggplot.col.ciu(instance, use.influence=TRUE, low.color = "firebrick", high.color = "steelblue")
  p <- p + labs(title="", x ="", y = expression(phi)) + own_theme + scale_y_continuous(labels=scaleFUN)
  print(p)
  # print(ciu$explain(instance))
  # Shapley
  predictor <- Predictor$new(diamonds.gbm, data = diamonds[,-ind.y], y = diamonds[,ind.y])
  shapley <- Shapley$new(predictor, x.interest = instance)
  p <- shapley$plot() + labs(y = expression(phi)) + titanic_theme + scale_y_continuous(labels=scaleFUN)
  d <- p$data
  d$sign <- d$phi>=0
  pnew <- ggplot(d) + geom_col(aes(x=reorder(feature.value,phi), y=phi, fill=sign)) +
    coord_flip() +
    labs(x ="", y = expression(phi)) + theme(legend.position = "none") +
    own_theme + scale_fill_manual("legend", values = c("FALSE" = "firebrick", "TRUE" = "steelblue"))
  print(pnew)
  #LIME
  explainer <- lime(diamonds, diamonds.gbm)
  explanation <- lime::explain(instance, explainer,n_features = 9)
  # p <- lime::plot_features(explanation)
  # print(p)
  d.lime <- data.frame(feature=explanation$feature_desc,class="1",phi=explanation$feature_weight,feature.value=explanation$feature_value)
  d.lime$sign <- d.lime$phi>=0
  p <- ggplot(d.lime) +
    geom_col(aes(reorder(feature, phi), phi, fill=sign)) +
    coord_flip() + facet_wrap(~class, labeller=label_both) +
    #labs(title=paste0("Explanation fit: ", scaleFUN(explanation$model_r2))) +
    scale_fill_manual("legend", values = c("FALSE" = "firebrick", "TRUE" = "steelblue")) +
    theme(legend.position = "none") + labs(x ="", y = expression(phi)) + titanic_theme
  print(p)
}

# Heart disease with RF. NOT USED IN THE PAPER.
heart.disease.rf <- function() {
  orig.heart.data <- heart.data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data",header=FALSE,sep=",",na.strings = '?')
  names(heart.data) <- c( "age", "sex", "cp", "trestbps", "chol","fbs", "restecg",
                          "thalach","exang", "oldpeak","slope", "ca", "thal", "num")
  heart.data$num[heart.data$num > 0] <- 1
  heart.data$num <- as.factor(heart.data$num)
  heart.data <- na.omit(heart.data)
  # Random Forest with caret.
  kfoldcv <- trainControl(method="cv", number=10)
  performance_metric <- "Accuracy"
  rf.heartdisease <- train(num~., data=heart.data, method="rf", metric=performance_metric, trControl=kfoldcv,preProcess=c("center", "scale"))
  n.in <- ncol(heart.data) - 1
  instance <- heart.data[32,1:n.in]
  ciu <- ciu.new(rf.heartdisease, num~., heart.data, output.names=c("No Heart Disease","Heart Disease Present"))
  p <- ciu$ggplot.col.ciu(instance, c(1:n.in)); print(p)
  p <- ciu$ggplot.col.ciu(instance, output.names = "Heart Disease Present") + labs(title="", x ="", y="CI", fill="CU") + own_theme; print(p)
  p <- ciu$ggplot.col.ciu(instance, output.names = "Heart Disease Present", use.influence=TRUE, low.color = "firebrick", high.color = "steelblue")
  p <- p + labs(title="", x ="", y = expression(phi)) + own_theme + scale_y_continuous(labels=scaleFUN)
  print(p)
  for ( i in 1:n.in ) {
    ciu$plot.ciu(instance, ind.input=i, ind.output=2)
  }
  #LIME
  explainer <- lime(heart.data, rf.heartdisease)
  explanation <- lime::explain(instance, explainer, n_labels = 1, n_features = 13)
  d.lime <- data.frame(feature=explanation$feature_desc,class="Heart Disease Present",phi=explanation$feature_weight,feature.value=explanation$feature_value)
  d.lime$sign <- d.lime$phi>=0
  p <- ggplot(d.lime) +
    geom_col(aes(reorder(feature, phi), phi, fill=sign)) +
    coord_flip() + facet_wrap(~class, labeller=label_both) +
    #labs(title=paste0("Explanation fit: ", scaleFUN(explanation$model_r2))) +
    scale_fill_manual("legend", values = c("FALSE" = "firebrick", "TRUE" = "steelblue")) +
    theme(legend.position = "none") + labs(x ="", y = expression(phi)) + titanic_theme
  print(p)
  # Shapley
  predictor <- Predictor$new(rf.heartdisease, data = heart.data[,-14], y = heart.data[,14])
  shapley <- Shapley$new(predictor, x.interest = instance[,-14])
  p <- shapley$plot()
  d <- p$data[p$data$class==1,]
  d$sign <- d$phi>=0
  pnew <- ggplot(d) + geom_col(aes(x=reorder(feature.value,phi), y=phi, fill=sign)) +
    coord_flip() + #facet_wrap(~class, labeller=label_both) +
    labs(x ="", y = expression(phi)) + theme(legend.position = "none") +
    own_theme + scale_fill_manual("legend", values = c("FALSE" = "firebrick", "TRUE" = "steelblue"))
  print(pnew)
}
