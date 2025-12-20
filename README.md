# llamac
A compiler for Llama writen in Haskell

## Generate hpc report
In order to generate a test code coverage report run
```zsh
stack test --coverage
stack hpc report --all --destdir ./reports/hpc --open
```

## Generate benchmark report
In order to generate a benchmark report run
```zsh
stack bench --benchmark-arguments "--output ./reports/criterion/report.html"
```