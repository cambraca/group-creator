# Group Creator

Group Creator is a program written in Haskell to create groupings of people based on a given set of rules, using a
genetic algorithm.

The idea is for a "population" (composed of "individuals") to evolve over a series of "generations". Some definitions:

- Individual: a particular grouping of people, with an associated ranking given by a fitness function (i.e. how well it
  fits the given criteria).
- Population: a number of individuals where, as each generation passes, some are selected to "evolve". At first, a
  population is created at random.
- Generation: the process in which a new population is created from an existing one, through crossover and mutation
  (see /docs).

The inputs are:

1. The set of people, including attributes that can be used for the grouping rules, such as sex, age, and height.
2. A list of rules ordered by priority. For example:

- Groups should have 4 members.
- The members of a group should have similar height.
- Members should be varied in age.
- Mixed-sex groups are encouraged.

In this example, the most important rule is the one about group size, so the program will do its best to satisfy it.

# Get Started

Using the `haskell:9` Docker image:

```bash
$ docker run -it --rm -v ./docker-cache:/root -v ./:/app -w /app haskell:9 cabal update
$ docker run -it --rm -v ./docker-cache:/root -v ./:/app -w /app haskell:9 cabal run
```

Note that all the inputs and options are hardcoded for now in Main.hs, including the set of people and
rules ("conditions").