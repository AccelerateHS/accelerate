accelerate-canny
================

Implementation of the [canny edge detector][wikipedia-canny] in Accelerate. The
majority of the algorithm is data-parallel and implemented in accelerate, with
the final (sequential) phase implemented using [repa][repa]. Uses
[accelerate-io][accelerate-io] to convert between the Accelerate and Repa array
representations.

Example
-------

> accelerate-canny data/images/lena256.bmp edges.bmp

| Original image | Result |
:---------------:|:------:|
| ![lena][lena]  | ![edges][edges]


  [repa]:               https://hackage.haskell.org/package/repa
  [accelerate-io]:      https://hackage.haskell.org/package/accelerate-io
  [wikipedia-canny]:    https://en.wikipedia.org/wiki/Canny_edge_detector
  [lena]:               https://github.com/AccelerateHS/accelerate-examples/raw/master/data/images/lena256.bmp
  [edges]:              https://github.com/AccelerateHS/accelerate-examples/raw/master/samples/canny.bmp

