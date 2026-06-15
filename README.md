# SMLP2026

SMLP2026: Advanced methods in frequentist statistics with Julia

The rendered website version of the course materials is available [here](https://repsychling.github.io/SMLP2026/).

This repository uses [Quarto](https://quarto.org) with the Julia code execution supplied by [QuartoNotebookRunner.jl](https://github.com/PumasAI/QuartoNotebookRunner.jl/).

## Setup

Full, student-facing setup instructions (git/git-lfs, Julia via Juliaup, Quarto, and an IDE)
live in [`getting-started/setup.qmd`](getting-started/setup.qmd) and on the
[rendered Setup page](https://repsychling.github.io/SMLP2026/getting-started/setup.html).
That page is the single authoritative source; the quick version is:

```sh
~/SMLP2026$ julia

julia> using Pkg

julia> Pkg.activate(".")
  Activating project at `~/SMLP2026`

julia> Pkg.instantiate()
< lots of output >

julia> exit()

~/SMLP2026$ quarto preview

< lots of output >
```
