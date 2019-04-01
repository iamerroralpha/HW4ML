sume1p1 = sum(e1p1)
v3 = 0
for (value in e1p1) {
  v2<--value/sume1p1*log2(value/sume1p1)
  if (!is.nan(v2))
  {
    v3 <- v2 + v3
  }
  cat(v2,"\t",v3,"\n")
}

e1p2 <- discretize(C1.0$V1,54)