require(lpSolve)
require(data.table)

df <- fread("LOL_data.csv")
mm <- cbind(model.matrix(as.formula("FP~Pos+0"), df), model.matrix(as.formula("FP~Pos+0"), df), 1, df$Salary, df$Salary, df$FP)
colnames(mm) <- c("ADC", "JNG", "MID", "SUP", "TEAM", "TOP","ADC", "JNG", "MID", "SUP", "TEAM", "TOP", "tot", "salary", "minSal", 'fp')


mm <- t(mm)
obj <- df$FP
dir <- c("<=", "<=", "<=", "<=", "<=", ">=", ">=", ">=", ">=", ">=", ">=", ">=", "==", "<=", ">=", "<=")

x <- 20000
vals <- c()
ptm <- proc.time()
for(i in 1:10){
  rhs <- c(3, 3, 3, 3, 1, 3, 1, 1, 1, 1, 1, 1, 8, 50000, 49500, x)
  lp <- lp(direction = 'max',
           objective.in = obj,
           all.bin = T,
           const.rhs = rhs,
           const.dir = dir,
           const.mat = mm)
  vals <- c(vals, lp$objval)
  x <- lp$objval - 0.00001
  df$selected <- lp$solution
  lineup <- df[df$selected == 1, ]
  print("---- Start ----")
  print(i)
  print(lineup)
  print(sum(lineup$FP))
  print(sum(lineup$Salary))
  print("----- END -----")
}
proc.time() - ptm
