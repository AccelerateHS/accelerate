accelerate-pagerank
-------------------

A simplified version of the [PageRank][pagerank-wiki] algorithm in Accelerate, based on a [repa][repa-homepage] implementation.

Example
-------

### Getting sample data

In `data/pagerank/` there is a simple test graph consisting of only a handful of pages and links. The file `titles.txt` contains the names of all the pages, one page per line. In `pages.txt` is the actual link graph. Each line is of the form:

```
n: m0 m1 m2 m3...
```

This specifies that the page at index `n` in the titles file has outgoing links to pages `m0`, `m1`, etc.

A more realistic dataset in the same format is available from [here][wikipedia-link-dump]. The two files you need are:
 * [links-simple-sorted.zip](http://users.on.net/~henry/pagerank/links-simple-sorted.zip) (323 MB)
 * [titles-sorted.zip](http://users.on.net/~henry/pagerank/titles-sorted.zip) (28 MB)

### Running the program

> accelerate-pagerank data/pagerank/pages.txt data/pagerank/titles.txt

This will run 10 steps of the algorithm using the simple test graph and output the page with the highest rank. More or fewer steps can be run with the `-steps` flag.

> accelerate-pagerank -steps 12 data/pagerank/pages.txt data/pagerank/titles.txt

By default this example will execute in chunks consisting of 12 million links at a time. Depending on the capability of your hardware, the available memory, and the size of your dataset, you may wish to adjust this. This can be done with `-chunk-size`.

> accelerate-pagerank -chunk-size 24000000 links-simple-sorted.txt titles-sorted.txt

  [pagerank-wiki]:          https://en.wikipedia.org/wiki/PageRank
  [repa-homepage]:          http://repa.ouroborus.net/
  [wikipedia-link-dump]:    https://wayback.archive.org/web/20160818143819/http://haselgrove.id.au/wikipedia.htm
