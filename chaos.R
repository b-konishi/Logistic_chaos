library(animation)


# ロジスティック関数グラフ
png("logistic_tx.png", width=1024, height=1024)
logistic.tx = function(t) 1/(1+exp(-t))
plot(logistic.tx, xlim=c(-5,5), ylim=c(0,1), xlab="", ylab="", lwd=3, cex.axis=2)
abline(v=0, lty=2)
abline(h=0, lty=2)
abline(h=1, lty=2)
abline(h=0.5, lty=2)
dev.off()


# 漸化式グラフ
png("logistic_xx.png", width=1024, height=1024)
logistic.xx = function(a) 2*a*(1-a)
plot(logistic.xx, xlab="", ylab="", lwd=3, cex.axis=2)
abline(v=0, lty=2)
abline(v=1, lty=2)
abline(h=0, lty=2)
abline(h=1, lty=2)
abline(h=0.5, lty=2)
dev.off()


# 周期グラフ(gif)
a = 0.8
r = seq(from=2.5, to=4, by=0.1)
for (i in 1:1000) {
  a = r*a*(1-a)
}
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
    plot(1:T, b, ylim=c(min(b)-d, max(b)+d), xlab="", ylab="", lwd=2, type="b")
    title(sprintf("Logistic Map: r=%3.1f, Period=%.0f", r[i], period), cex.main=2)
  }
}, movie.name="logistic.gif", interval=1.0)
 


# 周期グラフ(png)
png("period.png", width=1024, height=1024)
a = 0.8
r = seq(from=2.6, to=4, length=4)
for (i in 1:1000) {
  a = r*a*(1-a)
}
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
  plot(1:T, b, ylim=c(min(b)-d, max(b)+d), ylab="", xlab="", lwd=3, cex.axis=2, type="b")
  title(sprintf("Logistic-Map: r=%3.1f, Period=%.0f", r[i], period),cex.main=2)
}
dev.off()




# 分岐図(周期グラフのrを利用しているので注意)
png("bifurcation08_linein.png", width=1024, height=1024)
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

