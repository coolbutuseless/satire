
<!-- README.md is generated from README.Rmd. Please edit that file -->

# satire - (**SAT In R, Eh?**)

<!-- badges: start -->

![](https://img.shields.io/badge/cool-useless-green.svg)
[![CRAN](https://www.r-pkg.org/badges/version/satire)](https://CRAN.R-project.org/package=satire)
[![R-CMD-check](https://github.com/coolbutuseless/satire/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/coolbutuseless/satire/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`{satire}` is a package for defining Boolean satisfiability problems
(i.e. SAT problems) and includes two pure R SAT solvers.

A key feature of this package is that allows for the definition of the
SAT problem by specifying the clauses of the Boolean formula as logical
expressions. This can allow for a more natural definition of the problem
and for remapping of the solution back to the original variable names.

In addition, even though this style of SAT solver usually requires
Boolean formula to be in [Conjunctive Normal
Form(CNF)](https://en.wikipedia.org/wiki/Conjunctive_normal_form),
`{satire}` will automatically translate non-CNF expressions to CNF using
substitutions and applying [Tesytin
transformation](https://en.wikipedia.org/wiki/Tseytin_transformation).

### What’s in the box

- `sat <- sat_new()` create a new problem object
- `sat_add_literals()` add integer literals to a problem.
- `sat_add_exprs()` add Boolean expressions to a problem. If expressions
  are not in CNF (conjunctive normal form), then substitution rules and
  Tseytin transformation will be applied to convert them to CNF.
- `sat_card_*()` add cardinality constraints to a problem.
  - `sat_card_exactly_one()`
  - `sat_card_atleast_one()`
  - `sat_card_atmost_one()`
  - `sat_card_exactly_k()`
  - `sat_card_atleast_k()`
  - `sat_card_atmost_k()`
- Report the current state of the problem
  - `sat$dimacs`
  - `sat$literals`
  - `sat$names`
  - `sat$exprs`
- `read_dimacs()` read in a problem from a standard DIMACs-formatted
  file

## Installation

<!-- This package can be installed from CRAN -->

<!-- ``` r -->

<!-- install.packages('satire') -->

<!-- ``` -->

You can install the latest development version from
[GitHub](https://github.com/coolbutuseless/satire) with:

``` r
# install.package('remotes')
remotes::install_github('coolbutuseless/satire')
```

Pre-built source/binary versions can also be installed from
[R-universe](https://r-universe.dev)

``` r
install.packages('satire', repos = c('https://coolbutuseless.r-universe.dev', 'https://cloud.r-project.org'))
```

### SAT Example: Finding all possible work teams

This is a demonstration of how `{satire}` could be used to formulate and
solve a very simple SAT problem.

#### Problem statement

A, B, C, D are employed by the same company.

The company needs to find all combinations of these employees who can
work any given shift given the following constraints:

- Exactly 2 people need to work a shift
- B and D are not allowed to work together
- C must always work with A or D (in order to be trained)

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a SAT problem
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sat <- sat_new()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add constraints
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sat_card_exactly_k(sat, c('a', 'b', 'c', 'd'), 2) # Exactly two people working
sat_add_exprs(sat, "!b | !d")                     # B and D not together
sat_add_exprs(sat, "c -> (a | d)")                # C must work with A or D
sat
```

    #> <sat> vars = 16 , clauses = 28

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This is a small problem (only 16 variables) so can use the naive solver
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
solns <- sat_solve_naive(sat)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The solutions are returned as a data.frame. 
#   - column names correspond to variable names in the problem.
#   - each row is a solution
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
solns
```

    #>       a     b     c     d
    #> 1 FALSE FALSE  TRUE  TRUE
    #> 2  TRUE FALSE FALSE  TRUE
    #> 3  TRUE FALSE  TRUE FALSE
    #> 4  TRUE  TRUE FALSE FALSE

``` r
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract the names of each pair that can work together
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
apply(solns, 1, function(x) names(solns)[which(x)], simplify = FALSE)
```

    #> [[1]]
    #> [1] "c" "d"
    #> 
    #> [[2]]
    #> [1] "a" "d"
    #> 
    #> [[3]]
    #> [1] "a" "c"
    #> 
    #> [[4]]
    #> [1] "a" "b"

### Tools for defining a problem

Problems can be defined in two general ways:

- constructing the problem as an accumulation of Boolean expressions
- constructing the problem as a sequence of literal integers

All of the following are equivalent definitions of a single problem

``` r
# Sequential adding of single expressions
sat <- sat_new()
sat_add_exprs(sat, "!a | b")
sat_add_exprs(sat, "!b | c")
sat_add_exprs(sat, "!c | a")

# Adding a vector of expressions
sat <- sat_new()
sat_add_exprs(sat, c("!a | b", "!b | c", "!c | a"))

# Adding a compound expression
sat <- sat_new()
sat_add_exprs(sat, c("(!a | b) & (!b | c) & (!c | a)"))

# Using integer literals
#  1 =  a
# -1 = !a
#  2 =  b
# -2 = !b
#  3 =  c
# -3 = !c
sat <- sat_new()
sat_add_literals(sat, c(-1, 2, 0))
sat_add_literals(sat, c(-2, 3, 0))
sat_add_literals(sat, c(-3, 1, 0))

# Using integer literals, trailing 0 is optional
sat <- sat_new()
sat_add_literals(sat, c(-1, 2))
sat_add_literals(sat, c(-2, 3))
sat_add_literals(sat, c(-3, 1))

# Using literals all in a single vector. Zeros required to separate clauses
sat <- sat_new()
sat_add_literals(sat, c(-1, 2, 0,   -2, 3, 0,  -3, 1, 0))
```

### Cardinality helpers

A common constraint on SAT problems is **cardinality** i.e. how many
different variables can be simultaneously true.

`{satire}` includes the following helpers to add cardinality constraints
to a SAT problem:

- `sat_card_exactly_one()`
- `sat_card_atleast_one()`
- `sat_card_atmost_one()`
- `sat_card_exactly_k()`
- `sat_card_atleast_k()`
- `sat_card_atmost_k()`

The encoding for *at most one* can be done via *pairwise* encoding
(a.k.a binominal encoding) or using *commander* variables (after
[Klieber &
Kwon](https://www.cs.cmu.edu/~wklieber/papers/2007_efficient-cnf-encoding-for-selecting-1.pdf)).

Coding for *at least k*, *at most k* and *exactly k* uses [Sinz’s
formulation of LTSeq](https://www.carstensinz.de/talks/CP-2005-talk.pdf)
(See Slide 8).

As an example, the following SAT problem finds combinations from the
first 8 letters of the alphabet where exactly 3 are marked as TRUE.

The solution is a data frame where the columns correspond to the
variables in the SAT problem, and each row represents a solution.

``` r
sat <- sat_new()
sat_card_exactly_k(sat, letters[1:8], 3)
sat
```

    #> <sat> vars = 64 , clauses = 118

``` r
sat_solve_dpll(sat, max_solutions = 4)
```

    #>       a     b    c    d     e     f     g     h
    #> 1  TRUE FALSE TRUE TRUE FALSE FALSE FALSE FALSE
    #> 2 FALSE  TRUE TRUE TRUE FALSE FALSE FALSE FALSE
    #> 3 FALSE FALSE TRUE TRUE  TRUE FALSE FALSE FALSE
    #> 4 FALSE FALSE TRUE TRUE FALSE  TRUE FALSE FALSE

## Unsatisfiable problem example

A problem which is unsatisfiable will generate a `NULL` with the solvers
included in this package (`sat_solve_naive()` and `sat_solve_dpll()`).

Below, this unsatisiable problem wants `a` to be simultaneously `TRUE`
and `FALSE` - an impossibility.

``` r
sat <- sat_new()
sat_add_exprs(sat, "a")
sat_add_exprs(sat, "!a")
sat_solve_naive(sat) # Unsatisfiable
```

    #> NULL

``` r
sat_solve_dpll(sat)  # Unsatisfiable
```

    #> NULL

## Solving a problem from a DIMACs file

``` r
sat <- read_dimacs("inst/zebra_v155_c1135.cnf")
sat

sat_solve_dpll(sat)
```

## Resources

- <https://github.com/crillab/gophersat/blob/master/examples/sat-for-noobs.md>
- <https://en.wikipedia.org/wiki/Conjunctive_normal_form>
- <https://rbcborealis.com/research-blogs/tutorial-9-sat-solvers-i-introduction-and-applications/>
- <https://smt.st/>
- <https://personal.cis.strath.ac.uk/robert.atkey/cs208/converting-to-cnf.html> -
  a good vide on transofrming formulas and the Tseytin transformation.
- <https://www.inf.ed.ac.uk/teaching/courses/inf1/cl/2020/week8/>
