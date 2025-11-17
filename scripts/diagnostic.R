library(readxl)
library(mada)
### load your data
df <- read_excel("Desktop/Outcome.xlsx")

### create the madad object with 95% CI 
meta <- madad(df, level = 0.95)
mada:::forest.madad(meta, type = "sens")
mada:::forest.madad(meta, type = "spec")

###
rs <- rowSums(df[,c(2,3,4,5)])
weights <- 4 * rs / max(rs)
crosshair(df, xlim = c(0,0.6), ylim = c(0.4,1),
          col = 1:14, lwd = weights)

ROCellipse(df, pch = "")
points(fpr(df), sens(df))
###
(fit.DOR.DSL <- madauni(df, method = "DSL"))
(fit.DOR.MH <- madauni(df, method = "MH"))
summary(fit.DOR.DSL)
summary(fit.DOR.MH)

# For DSL method
mada:::forest.madauni(fit.DOR.DSL)
# For MH method
mada:::forest.madauni(fit.DOR.MH)


### PHM ###
(fit.phm.homo <- phm(df, hetero = FALSE))
(fit.phm.het <- phm(df))
summary(fit.phm.homo)
summary(fit.phm.het)
plot(fit.phm.het, xlim = c(0,0.6), ylim = c(0.4,1))
ROCellipse(df, add = TRUE)

###








