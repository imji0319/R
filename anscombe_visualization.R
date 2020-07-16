#anscombe Visualization.R"

dim(anscombe)
names(anscombe)
head(anscombe)

# 1. Statistic

colMeans(anscombe) #means
apply(anscombe, 2, var) #variance

# 2. Correlation
attach(anscombe)

apply( t(1:4), 2, function(x) cor(get(paste("x",x, sep="")), get(paste("y", x, sep=""))))

detach(anscombe)

# 3. translated for easy of data analysis
tmp <- unlist(as(anscombe, "vector"))
x <- tmp[1:(NROW(anscombe) * 4 )]
y <- tmp[(NROW(anscombe) * 4 +1 ):(NROW(anscombe) *8 )]

quartet <- rep(c("I", "II", "III", "IV"), each = NROW(anscombe))
anscombe.dat <- data.frame(x, y, quartet)

head(anscombe.dat)
tail(anscombe.dat)

# 4. Linear Regression
out <- apply(t(c("I", "II", "III", "IV")), 2 ,
             function( key ) {
                lm.obj <- lm(y~x, subset= quartet == key, data = anscombe.dat)
                round(c(lm.obj$coef["(Intercept)"],
                        lm.obj$coef["x"],
                        anova(lm.obj)["x", "Sum Sq"],
                        anova(lm.obj)["Residuals", "Sum Sq"],
                        summary(lm.obj)$coefficients["x", "Std. Error"],
                        summary(lm.obj)$r.squared), 2)
             })

dimnames(out) <- list(c("Coefficient Intercept",
                         "Coefficient x",
                         "Regression sum of squares",
                         "Residuals sum of squares", 
                         "Estimated standard error of b1",
                         "Multiple R-Square"),
                       c("I", "II", "III", "IV")
                       )
out

## 통계량으로 따졌을 때 동일한 값으로 나타나 동일한 데이터세트로 오해할 수 있음.

# 5. Data Graphs = Trellis Graph
library(lattice)
xyplot( y~x | quartet , data = anscombe.dat,
        panel = function(x, y , ...) {
          panel.xyplot(x, y, col= "orange", pch =16, cex =1.1)
          panel.xyplot(x, y, type= "g")
          panel.lmline(x, y, ...)
          
        }, 
        main = "y ~ x | quartet")

 











