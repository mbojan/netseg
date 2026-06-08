# Changelog

## netseg 1.0-4

- Fixed bug in
  [`smi()`](https://mbojan.github.io/netseg/reference/smi.md)
  ([\#21](https://github.com/mbojan/netseg/issues/21)) related to values
  on the diagonal of the mixing matrix, reported by
  [@welkiej7](https://github.com/welkiej7)

## netseg 1.0-3

CRAN release: 2025-04-14

- Update function names to catch-up on deprecations in igraph 2.0.0

## netseg 1.0-2

CRAN release: 2023-07-01

- Fix bug in `symmetrize`
  ([\#14](https://github.com/mbojan/netseg/issues/14)) for
  `rule="upper"` and `rule="lower"` reported by
  [@krisher1](https://github.com/krisher1).
- Fix typo in `CITATION`.
- `SSI()` relies on integer vertex IDs rather than vertex sequences
- Resave the datasets into a new igraph object structure (triggered CRAN
  errors)

## netseg 1.0-1

CRAN release: 2022-08-25

Code:

- Use [`Re()`](https://rdrr.io/r/base/complex.html) inside
  [`ssi()`](https://mbojan.github.io/netseg/reference/ssi.md) to compute
  on real parts of eigenvalues.
- Package **scales** is now Suggested.

Documentation and website:

- Set `VignetteBuilder` properly.
- Vignettes produce figures in PNG rather than SVG.
- Rerun Roxygen to fix HTML help problems
  ([\#12](https://github.com/mbojan/netseg/issues/12)).
- Add website favicons.
- Add `CITATION`.
- Cosmetic `README` updates.

## netseg 1.0-0

CRAN release: 2021-02-17

- Publishing to CRAN
