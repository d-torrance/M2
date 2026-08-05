Numbers for the growth policy described here, from the pre-GitHub `bugs/` tree.
Dan Grayson filed them as `bugs/dan/0-mutable-lists`, under "This should not
slow down by a factor of 100!":

```
i39 : time (x=new MutableList ; for i to 1000 do x#i = i ;  )
     -- used 0.002677 seconds

i40 : time (x=new MutableList ; for i to 10000 do x#i = i ;  )
     -- used 2.56211 seconds
```

Still current on 1.26.06-8-g34d5846039, and the shape is unchanged -- growing one
element at a time makes filling a list of length n cost O(n²):

```
  n=1000    .0013 s
  n=5000    .0568 s
  n=10000   .3664 s
  n=20000  1.3507 s
```

Ten times the elements costs about 280 times the work, and each doubling of n
roughly quadruples the time, which is what the `assignvector` code quoted above
would predict.

So the "amortize the cost themselves" option in the original report has a real
price attached: any code that builds a `MutableList` by index, without knowing
the final length in advance, is quadratic. Doubling on growth would make it
linear.

Recording it here rather than opening a second issue.
