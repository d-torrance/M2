-- From an email of 4 May 2008 from David Madore:

R0 = (ZZ/32003)[u]
K0 = frac R0
K = K0[v]/(v^2+u^2)
invv = substitute(-1/u^2,K)*v
v*invv
toField K
1/v  -- actual crash.
