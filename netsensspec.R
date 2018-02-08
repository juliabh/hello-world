net_sensspec <- function(nt, np, sens, spec) {

  netsens <- 0

  for (j in np:nt) {
    
    pves <- utils::combn(1:length(sens), j)
    
    for (i1 in 1:ncol(pves)) {
      
      netsens <- netsens + 
        (prod(sens[pves[, i1]]) * prod(1-sens[-pves[,i1]]))
      
    }
    
  }
  
  netspec <- 0
  
  for (k in nt:(nt-np+1)) {
   
    nves <- utils::combn(1:length(spec), k)
    
    for (i2 in 1:ncol(nves)) {
    
      netspec <- netspec + 
        (prod(spec[nves[, i2]]) * prod(1-spec[-nves[, i2]]))
      
    }
    
  }
  
  list(netsens = netsens, netspec = netspec) 
  
}

ss <- c(0.9, 0.9, 0.9, 0.9)
sp <- c(0.98, 0.98, 0.98, 0.98)

net_sensspec(4, 2, sens = ss, spec = sp)
net_sensspec(3, 2, sens = ss[1:3], spec = sp[1:3])
net_sensspec(2, 2, sens = ss[1:2], spec = sp[1:2])
