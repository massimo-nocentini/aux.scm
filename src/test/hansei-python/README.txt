A Python transcription of HANSEI's samplers (inference.ml and probM.ml of upstream/hansei, the
April 2009 code) over OCaml's `Random` (stdlib random.ml: the lagged Fibonacci generator of 55
words seeded with MD5).  It was written while planning (aux hansei) to reproduce upstream's
recorded sampling runs bit for bit, and it is the source of the values that the plan
(AUX-HANSEI-PLAN.md §8.2, D6, D19, D20) marks "py".  It is not part of `make test`; nothing in
aux imports it.

  ocamlrandom.py     OCaml's Random: full_init, bits, float; the switches `xorvariant` (the
                     xor in `bits` of OCaml >= 4.00) and `ndraw` (2 or 3 words per float)
  hansei_py.py       reify0 by replaying prefixes of choices, explore, shallow_explore,
                     random_selector, rejection_sample_dist, sample_dist (importance sampling
                     with look-ahead), sample_rejection, sample_importance, exact_reify
  hansei_check.py    the records of samplingM.ml (SM) under three generator variants
  bounded_check.py   bounded_explore (I:71-109) and geometric_bounded (X:72-77), test H22
  rint.py            OCaml's Random.int, the values of test R5
  n30.py             N:30-31 with one global state re-seeded by every selector (upstream)
                     against a fresh state per selector: only the first gives 0.5/0.5 (D6)
  talarm.py          SM:33-40's `let … and …` sequenced left to right against right to left:
                     only the first gives the recorded 0.14/0.86 (D19)

Each script finds the others in its own directory, so it runs under `python3 -I` from any
directory (-B keeps __pycache__ out of the tree):

  python3 -I -B hansei_check.py
  python3 -I -B bounded_check.py

`xorvariant=True, ndraw=3` is the variant that replays SM: in hansei_check.py's first block,
every line that ends with "exp …" shows upstream's recorded value, from samplingM.ml, after the
transcription's, and the two agree.  `xorvariant=False` or `ndraw=2` (OCaml 4.14's two-draw float) do not
(e.g. `rej dcoin s17 10000` gives 0.054 with two draws, 0.0501 without the xor, instead of
0.0514).
