[Testing configuration](test-config.md)

Yarr 0.9.1, Repa 3.2.3.1

Canny
=====

Sequential
----------

* **Yarr**

  ```
  $ ./canny 20 50 100 tree.png 
  ...
  Total:  199    ± 1    ms,   156    ± 1    tics per index (20 repeats)
  ```

  Typical run:

  ```
  $ ./canny 1 50 100 tree.png 
  luminosity       13.1  ms, 10.3  tics per index
  blur conv. by X  11.2  ms,  8.77 tics per index
  blur conv. by Y  35.1  ms, 27.5  tics per index
  magOrient        86.9  ms, 68.2  tics per index
  supress          31.8  ms, 25.0  tics per index
  wildfire         21.7  ms, 17.1  tics per index
  Total:  202    ± 0    ms,   158    ± 0    tics per index (1 repeats)
  ```

* **Repa**

  ```399 ± 3 ms (10 manual repeats)```

  Typical run:

  ```
  $ ./repa-canny 1 50 100 tree.bmp t-o.bmp
  toGreyScale
    elapsedTimeMS   = 76
    cpuTimeMS       = 76
  blurX
    elapsedTimeMS   = 20
    cpuTimeMS       = 20
  blurY
    elapsedTimeMS   = 13
    cpuTimeMS       = 12
  diffX
    elapsedTimeMS   = 13
    cpuTimeMS       = 16
  diffY
    elapsedTimeMS   = 12
    cpuTimeMS       = 12
  magOrient
    elapsedTimeMS   = 157
    cpuTimeMS       = 156
  suppress
    elapsedTimeMS   = 33
    cpuTimeMS       = 36
  select
    elapsedTimeMS   = 48
    cpuTimeMS       = 48
  wildfire
    elapsedTimeMS   = 29
    cpuTimeMS       = 28

  TOTAL

  elapsedTimeMS   = 407
  cpuTimeMS       = 404
  ```



Parallel (5 threads)
--------------------

* **Yarr**

  ```80.6 ± 5.9 ms (10 manual repeats)```

  Fastest run:

  ```
  $ ./canny 1 50 100 tree.png +RTS -N5
  luminosity        5.06 ms,  3.97 tics per index
  blur conv. by X   3.75 ms,  2.95 tics per index
  blur conv. by Y   7.80 ms,  6.12 tics per index
  magOrient        21.4  ms, 16.8  tics per index
  supress           8.41 ms,  6.61 tics per index
  wildfire         23.2  ms, 18.3  tics per index
  Total:  72.1  ±  0.0  ms,   56.6  ±  0.0  tics per index (1 repeats)
  ```


* **Repa**

  ```137 ± 3 ms (10 manual repeats)```

  Fastest run:

  ```
  $ ./repa-canny 1 50 100 tree.bmp t-o.bmp +RTS -N5
  toGreyScale
    elapsedTimeMS   = 16
    cpuTimeMS       = 80
  blurX
    elapsedTimeMS   = 8
    cpuTimeMS       = 36
  blurY
    elapsedTimeMS   = 5
    cpuTimeMS       = 24
  diffX
    elapsedTimeMS   = 5
    cpuTimeMS       = 24
  diffY
    elapsedTimeMS   = 5
    cpuTimeMS       = 24
  magOrient
    elapsedTimeMS   = 36
    cpuTimeMS       = 176
  suppress
    elapsedTimeMS   = 9
    cpuTimeMS       = 40
  select
    elapsedTimeMS   = 15
    cpuTimeMS       = 48
  wildfire
    elapsedTimeMS   = 29
    cpuTimeMS       = 32

  TOTAL

  elapsedTimeMS   = 133
  cpuTimeMS       = 484
  ```



"Factorized" blur unroll factor benchmark
=========================================

Sequential:

```
$ ./ub
by X: ref (no unroll):  25.3  ±  1.5  ms,   19.8  ±  1.2  tics per index (10 repeats)
by X: uf 1        : 25.9  ±  0.2  ms,   20.3  ±  0.2  tics per index (10 repeats)
by X: uf 2        : 16.0  ±  1.2  ms,   12.6  ±  0.9  tics per index (10 repeats)
by X: uf 3        : 13.9  ±  0.7  ms,   10.9  ±  0.6  tics per index (10 repeats)
by X: uf 4        : 13.0  ±  0.3  ms,   10.2  ±  0.2  tics per index (10 repeats)
by X: uf 5        : 11.9  ±  0.3  ms,    9.30 ±  0.23 tics per index (10 repeats)
by X: uf 6        : 11.8  ±  0.2  ms,    9.24 ±  0.17 tics per index (10 repeats)
by X: uf 7        : 11.1  ±  0.3  ms,    8.72 ±  0.21 tics per index (10 repeats)
by X: uf 8        : 10.1  ±  1.2  ms,    7.93 ±  0.98 tics per index (10 repeats)

by Y: ref (no unroll):  46.1  ±  0.5  ms,   36.1  ±  0.4  tics per index (10 repeats)
by Y: uf 1        : 44.9  ±  0.3  ms,   35.2  ±  0.2  tics per index (10 repeats)
by Y: uf 2        : 35.8  ±  0.2  ms,   28.1  ±  0.2  tics per index (10 repeats)
by Y: uf 3        : 41.5  ±  0.1  ms,   32.5  ±  0.1  tics per index (10 repeats)
by Y: uf 4        : 43.1  ±  0.2  ms,   33.8  ±  0.1  tics per index (10 repeats)
by Y: uf 5        : 43.1  ±  0.2  ms,   33.9  ±  0.2  tics per index (10 repeats)
by Y: uf 6        : 41.9  ±  0.1  ms,   32.9  ±  0.1  tics per index (10 repeats)
by Y: uf 7        : 43.6  ±  0.1  ms,   34.2  ±  0.1  tics per index (10 repeats)
by Y: uf 8        : 43.5  ±  0.2  ms,   34.1  ±  0.2  tics per index (10 repeats)
```

Parallel:

```
$ ./ub +RTS -N5
by X: ref (no unroll):   7.62 ±  2.45 ms,    5.98 ±  1.92 tics per index (10 repeats)
by X: uf 1        :  7.49 ±  2.29 ms,    5.88 ±  1.80 tics per index (10 repeats)
by X: uf 2        :  5.37 ±  1.73 ms,    4.21 ±  1.36 tics per index (10 repeats)
by X: uf 3        :  4.74 ±  1.33 ms,    3.72 ±  1.04 tics per index (10 repeats)
by X: uf 4        :  4.30 ±  1.80 ms,    3.37 ±  1.41 tics per index (10 repeats)
by X: uf 5        :  4.22 ±  0.86 ms,    3.31 ±  0.68 tics per index (10 repeats)
by X: uf 6        :  4.22 ±  1.54 ms,    3.31 ±  1.21 tics per index (10 repeats)
by X: uf 7        :  3.87 ±  0.88 ms,    3.04 ±  0.69 tics per index (10 repeats)
by X: uf 8        :  4.02 ±  1.13 ms,    3.15 ±  0.89 tics per index (10 repeats)

by Y: ref (no unroll):  10.6  ±  2.2  ms,    8.33 ±  1.69 tics per index (10 repeats)
by Y: uf 1        : 11.3  ±  3.0  ms,    8.83 ±  2.38 tics per index (10 repeats)
by Y: uf 2        :  8.88 ±  2.46 ms,    6.96 ±  1.93 tics per index (10 repeats)
by Y: uf 3        : 10.4  ±  3.8  ms,    8.15 ±  2.97 tics per index (10 repeats)
by Y: uf 4        : 11.9  ±  4.0  ms,    9.31 ±  3.12 tics per index (10 repeats)
by Y: uf 5        : 10.9  ±  2.4  ms,    8.56 ±  1.92 tics per index (10 repeats)
by Y: uf 6        : 10.3  ±  2.5  ms,    8.10 ±  1.95 tics per index (10 repeats)
by Y: uf 7        : 11.1  ±  3.9  ms,    8.72 ±  3.06 tics per index (10 repeats)
by Y: uf 8        : 10.9  ±  2.1  ms,    8.51 ±  1.65 tics per index (10 repeats)
```



Blur
====

```
$ ./blur tree.png +RTS -N5
sequential blur: 0: 57.9  ±  1.3  ms,   45.5  ±  1.0  tics per index (10 repeats)
sequential blur: 1: 57.5  ±  0.2  ms,   45.2  ±  0.1  tics per index (10 repeats)
sequential blur: 2: 57.6  ±  0.2  ms,   45.2  ±  0.2  tics per index (10 repeats)
parallel blur:  41.1  ±  4.0  ms,   32.3  ±  3.2  tics per index (10 repeats)
```


Color transitions
=================

Sequential:

```
$ ./ct 0.5 tree.png
contrast: 43.8  ±  1.2  ms,   34.4  ±  0.9  tics per index (10 repeats)
luminosity: 11.3  ±  0.5  ms,    8.87 ±  0.37 tics per index (10 repeats)
```

Parallel:

```
$ ./ct 0.5 tree.png +RTS -N5
contrast: 10.2  ±  0.3  ms,    8.00 ±  0.26 tics per index (10 repeats)
luminosity:  4.78 ±  0.55 ms,    3.75 ±  0.43 tics per index (10 repeats)
```