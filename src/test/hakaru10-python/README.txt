A Python transcription of the Hakaru10 engine (Metropolis.hs, Syntax.hs and Distribution.hs of
upstream/okmij-hakaru10, the November 2016 code) over the StdGen of random-1.0.1.1.  It was
written while planning (aux hakaru10) to reproduce upstream's recorded runs bit for bit, and it
is the source of the values that the tests call "prototype regression values" or "the Python
transcription".  It is not part of `make test`; nothing in aux imports it.

  stdgen.py                 StdGen, randomIvalInteger, randomR on Double
  engine.py                 the trace graph, dist/diracN/condition/if_, propagation, mcmC,
                            and the discrete and normal distributions
  extra.py                  uniform, gamma, beta, logGamma, momenta, tabulate, kl
  replay-bern.py            pbern and the biased coin, by hand over stdgen.py
  replay-examples.py        pbern, ph2, ph21, pdep, pcond1-4, pcorr, girl, post_bias, grass
  replay-more-examples.py   the bench at 1e3 and 1e4, biased_coin22, niceexmcr, tac, tac10,
                            prog_mixture1, niceex1, mixng, the KL of phier
  replay-flip.py            the same models with the flip proposal for booleans
  replay-colored-balls.py   ph21, girl and the colored-balls model, flip or noflip
  distribution-replays.py   200 seeded draws of every distribution, as Scheme data
  bench.py                  the moments of Bench.hs's phier 10 over nchain samples
  birds-data.py             writes a synthetic data set for test/hakaru10-birds.scm

Each script but birds-data.py takes this directory as its first argument, so that it runs
under `python3 -I`; birds-data.py takes the directory to write the data set to:

  python3 -I replay-examples.py .
  python3 -I replay-colored-balls.py . noflip
  python3 -I bench.py . 10000          # (-3.151835923727823, 54.168535367816084)
  mkdir data && python3 -I birds-data.py data && HAKARU10_BIRDS_DIR=$PWD/data make -C ../.. test-hakaru10-long

A line printed as "name expected? computed" shows upstream's recorded value, from the comments
of Examples1.hs or Bench.hs, next to the transcription's.  Where the two differ (for example the
bench beyond 1e3 samples) upstream's record presumably comes from another revision of the code;
see the header of test/hakaru10-bench.scm.
