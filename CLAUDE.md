# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

`morton` is an R package (Andrew L. Hipp, The Morton Arboretum) of tools for manipulating phylogenetic, genetic, and other biodiversity data. It is a personal/lab research package, not a CRAN package — versioning is loose and the API is informal.

- Hard dependencies (`Depends`): `ape`, `phytools`, `parallel`
- Imports: `XML`, `gdata`, `Biostrings`, and `seqinr` (`read.fasta`/`write.fasta`)

## Repository layout

- `R/` — one function per file, filename matches the function name (e.g. `R/make.gene.matrix.R` defines `make.gene.matrix`). A few files define helpers alongside the main function (e.g. `R/tax2tree.R` also defines `dist.nodes.2`).
- `man/` — hand-written `.Rd` documentation. **Roxygen is NOT used** (R source has no `#'` comments). Do not run `roxygen2::roxygenise()` / `devtools::document()` — it would overwrite the curated `.Rd` files. Edit `.Rd` files directly. Several `man/morton-undocumented-*.Rd` files group multiple undocumented functions.
- `data/` — reference text tables (`mor.exp.*.txt`).
- `NAMESPACE` — exports everything matching `^[[:alpha:]]+` via `exportPattern`, so any new function whose name starts with a letter is auto-exported. The only explicit S3 registration is `S3method(summary, geneMat)`.

There is no test suite, no build script, and no CI.

## Common commands

Run from the repository root (the directory containing `DESCRIPTION`):

```sh
# Install locally for interactive use
R CMD INSTALL .
# or: Rscript -e 'devtools::install(".")'

# Build a source tarball
R CMD build .

# Full package check
R CMD check .            # add --no-tests since there are none

# Load the package into an interactive session without installing
Rscript -e 'devtools::load_all(".")'
```

Because there are no tests, "running a single test" does not apply. Validate changes by `load_all()` and exercising the function against real data in an R session.

## Architecture / big picture

Two loosely coupled domains share the package. Understanding the data conventions matters more than any single function.

### 1. NCBI GenBank → gene matrix pipeline

The largest workflow turns raw GenBank records into a voucher-by-locus matrix used to plan/assemble multi-gene phylogenetic datasets. The chain is roughly:

`parse.INSDSeq` (read GenBank XML) → `parse.specimen` (heuristically split the free-text `specimen_voucher` field into collector / number / collection) → `make.unique.vouchers` → `make.gene.matrix` (rows = vouchers, cols = loci, cells = NCBI accessions joined by `|`) → `make.shared.gene.matrix` → `orphan.loci` / `orphan.individuals` (drop empty rows/cols) → `make.fasta.files` / `refine.fasta` (emit per-locus FASTA for alignment).

- The gene matrix carries metadata columns `orgs`, `ncbiAcc`, `numberOfOrgs`, `numberOfAccessions` prepended to the locus columns; downstream summaries (`summary.geneMat`, `summarize.gene.coverage`, `plot.geneMat`) special-case these names.
- `geneMat` is an informal S3 class with `summary.geneMat` and `plot.geneMat` methods (note: the `class()` assignment is currently commented out in `make.gene.matrix`, so callers may need to set the class manually).
- `R/make.matrices.R` is not a function — it is a convenience script that `source()`s the pipeline functions from raw GitHub URLs (`andrew-hipp/morton/master/...`), kept for back-compatibility with the Syst. Bot. 2016 paper.

### 2. Phylogenetic tree / tip-label manipulation

Operates on `ape` `phylo` objects. The central convention is **pipe-delimited tip labels**: a tip label packs multiple fields separated by `|`, and `label.elements` extracts a chosen field (`returnNum`) from every tip. Most tree functions build on this:

- `label.elements` — the workhorse used by `summary.by.elements`, `color.tips.by.element`, `section.coloring`, etc. Default delimiter `|`, default element `1`.
- `summary.by.elements` — taxonomic disparity: counts tips per element vs. `tips.expected`, optionally computing each group's effect on total tree length (slow; it drops tips repeatedly).
- `tax2tree` — builds a `phylo` from a parent/child taxonomy table (columns `parent`, `child`, `tip`).
- Other tree utilities: `match.nodes`, `monophyletic.spp`, `add.data.to.tips`, `make.binary`, `simplePhylo`, `rate.change.v2`, `beastITSdating`.

### Cross-cutting conventions

- Functions that write output files often generate a **random 5-letter suffix** (`paste(sample(letters, 5), collapse = '')`) to avoid clobbering — so re-running produces new filenames each time (e.g. `summary.geneMat`, `make.gene.matrix`'s error log).
- Column names are passed as string arguments with defaults (e.g. `vouchersCol = 'newLabels'`, `locusCol = 'cleanedGeneRegion'`) rather than positional — match these defaults when wiring functions together.
- `|` is the universal in-cell / in-label separator across both domains.

## When adding or editing functions

- Keep one function per file, filename = function name, to match the existing layout and the `exportPattern` export rule.
- Add or update the corresponding hand-written `.Rd` in `man/` (do not generate it).
- New S3 methods must be registered explicitly in `NAMESPACE` (only `summary.geneMat` is currently registered).
