# popgen

A tool to create `Fast and Succinct Population Protocols for Presburger Arithmetic` based on the corresponding paper by Czerner et al.


## Description

The tool takes a Presburger predicate as a String as its input and constructs a population protocol that computes exactly that predicate. The resulting predicate is printed to a given file in a format compatible to [popsim](https://github.com/Rage1851/popsim) such that it can be simulated by the same.


## System requirements

* Haskell
* cabal


## Usage

The project is build using `cabal`. Thus, all cabal commands can be used. The most important ones are listed below:

1. Building
    ```
    cabal build popgen [--ghc-options="-O2"]
    ```

2. Execution (includes building)
    ```
    cabal run popgen [--ghc-options="-O2"]
    ```

3. Installation (includes building)
    ```
    cabal install [--ghc-options="-O2"] [--installdir="path"] [--install-method=copy]
    ```

4. Testing \
    *Be aware that running all provided tests requires a lot of time and memory.*
    ```
    cabal test test:tests --ghc-options="-main-is Tests.Tests" --test-show-details="always"
    ```
