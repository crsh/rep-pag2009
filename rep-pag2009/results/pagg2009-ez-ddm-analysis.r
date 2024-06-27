# install.packages("EZ2", repos="http://R-Forge.R-project.org")

library("EZ2")

pagg2009 <- data.frame(
  group = c("honest", "ambiguous", "dishonest")
  , mrt_op = c((0.519+0.556)/2, (0.65*0.511 + 0.35*0.585), (0.85*0.527 + 0.15*0.800))
  , vrt_op = c((0.195^2+0.215^2)/2, (0.65*0.229^2 + 0.35*0.324^2), (0.85*0.218^2 + 0.15*0.298^2))
  , pw_op = c(0.5, 0.35, 0.15)
  , mrt_noop = c((0.520 + 0.580)/2, (0.507 + 0.548)/2, (0.504 + 0.611)/2)
  , vrt_noop = c((0.152^2 + 0.215^2)/2, (0.208^2 + 0.307^2)/2, (0.164^2 + 0.274^2)/2)
  , pw_noop = 0.5 + 1/(2*70)
)

EZ2batch(
  c(v_op=.11,v_noop=.21,z_op=.05,z_noop=.05,a_op=.09, a_noop=.09,ter=0.2)
  , mrt_op ~ EZ2.mrt(v_op,z_op,a_op,s=0.1, ter)
  , vrt_op ~ EZ2.vrt(v_op,z_op,a_op,s=0.1)
  , pw_op ~ EZ2.pe(v_op,z_op,a_op)
  , mrt_noop ~ EZ2.mrt(v_noop,z_noop,a_noop,s=0.1,ter)
  , vrt_noop ~ EZ2.vrt(v_noop,z_noop,a_noop,s=0.1)
  , pw_noop ~ EZ2.pe(v_noop,z_noop, a_noop)
  , data = pagg2009[, -1]
)


A = seq(.08,.13,len=6) 
X2 = data.frame(A=A) 
X2$mrt0 = sapply(A, function(a) EZ2.mrt(.1,.01,a,Ter=0.2)) 
X2$vrt0 = sapply(A, function(a) EZ2.vrt(.1,.01,a)) 
X2$pe0 = sapply(A, function(a) EZ2.pe(.1,.01,a)) 
X2$mrt1 = sapply(A, function(a) EZ2.mrt(.2,a-.05,a,Ter=0.2)) 
X2$vrt1 = sapply(A, function(a) EZ2.vrt(.2,a-.05,a)) 
X2$pe1 = sapply(A, function(a) EZ2.pe(.2,a-.05,a)) 
  
X2 = as.data.frame(X2)

EZ2batch(c(v0=.11,v1=.21,z0=.05,z1=.05,a=.09,ter=0.2), 
    mrt0 ~ EZ2.mrt(v0,z0,a,s=0.1, ter), 
    vrt0 ~ EZ2.vrt(v0,z0,a,s=0.1), 
    pe0 ~ EZ2.pe(v0,z0,a), 
    mrt1 ~ EZ2.mrt(v1,a-z1,a,s=0.1,ter), 
    vrt1 ~ EZ2.vrt(v1,a-z1,a,s=0.1), 
    pe1 ~ EZ2.pe(v1, a-z1, a), data=X2)


