normalize.sig.2 <-
function(sig, bg, lambda22) {

  a <- apply(bg, 2, function(x) var(x))/apply(lambda22, 2, function(x) mean(x))
  rescl.a <- a/mean(a)

  for(i in seq(ncol(bg))) {
    bg[,i] <- bg[,i]/rescl.a[i]
  }
  
  centr <- apply(bg, 2, function(x) median(x))
  diff <- centr-median(centr)
  
  for(i in seq(ncol(sig))) {
    sig[,i] <- sig[,i]/rescl.a[i]-diff[i]
  }
  sig
}
