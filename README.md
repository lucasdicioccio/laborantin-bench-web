# Laborantin - Web benchmark

Web bench experiments using the Laborantin framework.

## Introduction

This is an example repository for Laborantin experiments. These experiments
demonstrate how one can use Laborantin to run controlled experiments such as
benchmarks.

See http://hackage.haskell.org/package/laborantin-hs for generic information on
Laborantin.

## Usage

Make sure that `weighttp` and `mighty` are in your `$PATH` environment variable.
  * https://github.com/temoto/weighttp , an HTTP benchmarking tool
  * http://hackage.haskell.org/package/mighttpd2 , an HTTP server written in
    Haskell, like cabal

Furthermore, your TCP stack should let you open many TCP connections in a short
amount of time for actual benchmarks. If you are not familiar with TCP tuning,
please read http://gwan.com/en_apachebench_httperf.html .

```
  git clone https://github.com/lucasdicioccio/laborantin-bench-web
  cd laborantin-bench-web
  cabal sandbox init
  cabal update
  cabal build

  # generate an index.html locally
  echo '<h1>hello world</h1>' > index.html

  # get a description of what parameters mean
  ./dist/build/laborantin-bench-web/laborantin-bench-web describe

  # run some experiments (this may take a long time depending on your machine)
  ./dist/build/laborantin-bench-web/laborantin-bench-web run -m "@sc.param 'requests-count' == 50000 and \
                                                                 @sc.param 'client-concurrency' in [20,50,100] and \
                                                                 @sc.param 'client-processes' in [1,2,3,4,5,6,7,8] and \
                                                                 @sc.param 'server-concurrency' in [1,2,3,4,5,6,7,8]"

  # find where some experiments with enough server concurrency
  ./dist/build/laborantin-bench-web/laborantin-bench-web find -m "@sc.param 'server-concurrency' >= 2"
```

## Todo

We should do a bit more:
  * generate index.html in the experiment directory and pass this as CWD when
    spawning the mighty server process
  * add other webservers for a comparison
  * parse and plot results comparing servers and parameter sets
