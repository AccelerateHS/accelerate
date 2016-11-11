accelerate-kmeans
=================

Implementation of the [k-means clustering][kmeans-wiki] algorithm in Accelerate.
For simplicity of exposition this implementation is specialised to clustering
points on a two-dimensional plane.

Example
-------

### Preparing sample data

The included [`GenSamples.hs`][GenSamples.hs] program can be used to generate
some random data points to be fed into the clustering algorithm.

> runhaskell GenSamples.hs 5 100 1000 6849

The first parameter specifies the target number of clusters, the second and
third parameters specify the minimum and maximum number of points to generate,
and the fourth parameter is a seed to the random number generator.


### Running the program

The main application requires that the sample data to analyse be available in
two files in the current directory: `points.bin` contains a list of pairs of
(x,y) coordinates as floating-point numbers, and `clusters` contains the initial
guess of the cluster locations.

Result from running with the above random data (plotted separately):

![kmeans][kmeans-img]


  [kmeans-wiki]:            https://en.wikipedia.org/wiki/K-means_clustering
  [kmeans-img]:             https://github.com/AccelerateHS/accelerate-examples/raw/master/samples/k-means.png
  [GenSamples.hs]:          https://github.com/AccelerateHS/accelerate-examples/blob/master/examples/kmeans/GenSamples.hs

