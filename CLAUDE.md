# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

> **This is not a complete, installable R package.** It is a collection of R scripts in various states of readiness for packaging. The `DESCRIPTION`/`NAMESPACE`/`man/` scaffolding exists but is incomplete and partly inconsistent (see dependencies below), so `R CMD INSTALL`/`build`/`check` are not the intended workflow and may fail. **Users are advised to `source()` individual functions directly** rather than installing the repository as a package.

`morton` is a collection of R tools (Andrew L. Hipp, The Morton Arboretum) for manipulating phylogenetic, genetic, and other biodiversity data. It is a personal/lab research codebase, not a CRAN package — versioning is loose and the API is informal.

- Dependencies are declared in two places that disagree. `DESCRIPTION` only has `Depends: ape, phytools, parallel` (and no `Imports:` field; its `Date` is stale at 2016-04-20). `NAMESPACE` additionally pulls in `import(... XML, gdata, Biostrings)` and `importFrom(seqinr, read.fasta, write.fasta)`. So the *effective* dependency set is `ape`, `phytools`, `parallel`, `XML`, `gdata`, `Biostrings`, `seqinr` — but `R CMD check` will flag the undeclared imports. If you touch dependencies, fix both files.

## Repository layout

- `R/` — one function per file, filename matches the function name (e.g. `R/make.gene.matrix.R` defines `make.gene.matrix`). A few files define helpers alongside the main function (e.g. `R/tax2tree.R` also defines `dist.nodes.2`).
- `man/` — hand-written `.Rd` documentation. **Roxygen is NOT used** (R source has no `#'` comments). Do not run `roxygen2::roxygenise()` / `devtools::document()` — it would overwrite the curated `.Rd` files. Edit `.Rd` files directly. Several `man/morton-undocumented-*.Rd` files group multiple undocumented functions.
- `data/` — reference text tables (`mor.exp.*.txt`).
- `NAMESPACE` — exports everything matching `^[[:alpha:]]+` via `exportPattern`, so any new function whose name starts with a letter is auto-exported. The only explicit S3 registration is `S3method(summary, geneMat)`.

There is no test suite, no build script, and no CI.

## Common commands

Run from the repository root (the directory containing `DESCRIPTION`).

**Preferred workflow — source functions directly** (the repo is not reliably installable as a package; see the note at the top):

```sh
# Source a single function into an interactive session
Rscript -e 'source("R/label.elements.R")'
# In the Syst. Bot. 2016 pipeline, R/make.matrices.R source()s functions straight from GitHub.
```

The package scaffolding *does* exist, so the standard commands below may work for a subset of functions — but expect `R CMD check` to fail on the undeclared imports and other incompleteness. Use them only when you specifically need the package machinery, not for everyday development:

```sh
# Load everything into a session (often the most useful of these)
Rscript -e 'devtools::load_all(".")'

# Package machinery (may fail — see dependency note at top)
R CMD INSTALL .          # or: Rscript -e 'devtools::install(".")'
R CMD build .
R CMD check .            # add --no-tests since there are none
```

Because there are no tests, "running a single test" does not apply. Validate changes by `source()`ing (or `load_all()`ing) the function and exercising it against real data in an R session.

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
