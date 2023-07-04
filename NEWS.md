# netseg 1.0-2

- Fix bug in symmetrize (#14) for rule=upper and rule=lower reported by @krisher1.
- Fix typo in `CITATION`.
- `SSI()` relies on integer vertex IDs rather than vertex sequences
- Resave the datasets into a new igraph object structure (triggered CRAN errors)


# netseg 1.0-1

Code:

- Use `Re()` inside `ssi()` to compute on real parts of eigenvalues.
- Package **scales** is now Suggested.

Documentation and website:

- Set `VignetteBuilder` properly.
- Vignettes produce figures in PNG rather than SVG.
- Rerun Roxygen to fix HTML help problems (#12).
- Add website favicons.
- Add `CITATION`.
- Cosmetic `README` updates.


# netseg 1.0-0

- Publishing to CRAN
