library(animation)



a = 0.8
r = seq(from=2.5, to=4, by=0.1)
for (i in 1:1000) {
  a = r*a*(1-a)
}
# 周期グラフ
saveGIF({
  ani.options(loop = 1, width=1440, height=1440)
  # par(mfrow = c(3,2))
  T = 50
  for (i in 1:length(r)) {
    b = 1:T
    for (j in 1:T) {
      a[i] = r[i]*a[i]*(1-a[i])
      b[j] = a[i]
    }
    b = round(b, 5)
    for (j in 2:T) {
      period = Inf
      if (b[1] == b[j]) {
        period = j-1
        print(period)
        break
      }
    }
    d = (max(b)-min(b))/10
    plot(1:T, b, ylim=c(min(b)-d, max(b)+d), xlab="", ylab="", type="b")
    title(sprintf("Logistic Map: r=%3.1f, Period=%.0f", r[i], period), cex.main=2)
  }
}, movie.name="test.gif", interval=1.0)
 
png("period.png", width=1024, height=1024)
a = 0.8
r = seq(from=2.6, to=4, length=4)
for (i in 1:1000) {
  a = r*a*(1-a)
}
# 周期グラフ
par(mfrow = c(2,2))
T = 20
for (i in 1:length(r)) {
  b = 1:T
  for (j in 1:T) {
    a[i] = r[i]*a[i]*(1-a[i])
    b[j] = a[i]
  }
  b = round(b, 5)
  for (j in 2:T) {
    period = Inf
    if (b[1] == b[j]) {
      period = j-1
      print(period)
      break
    }
  }
  d = (max(b)-min(b))/10
  plot(1:T, b, ylim=c(min(b)-d, max(b)+d), ylab="", xlab="", cex.axis=2, type="b")
  title(sprintf("Logistic-Map: r=%3.1f, Period=%.0f", r[i], period),cex.main=2)
}
dev.off()




png("bifurcation08_wide.png", width=1000, height=600)
# 分岐図
a = 0.8
rb = seq(from=2.5, to=4, by=0.001)
for (i in 1:1000) {
  a = rb*a*(1-a)
}
for (i in 1:1000) {
  a = rb*a*(1-a)

  plot(rb, a, xlim=c(rb[1],rev(rb)[1]), ylim=c(0,1), xlab="", ylab="", cex=0.001, cex.axis=2, pch=20)
  par(new=T)
  abline(v=r, lty=2)
  par(new=T)
}
dev.off()













