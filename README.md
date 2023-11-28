# Wolfram: Elementary Cellular Automaton in Haskell

Wolfram is a Haskell implementation of the Elementary Cellular Automaton, a mathematical model and computational system that exhibits complex and interesting behavior. This project allows you to explore different cellular automaton rulesets and visualize their evolution over generations.

![Wolfram](https://content.wolfram.com/sw-publications/2020/07/making-wolfram-alpha-images-talkslides-10.jpg)

## How to Run

Make sure you have [Stack](https://docs.haskellstack.org/en/stable/README/) installed, the Haskell build tool. You can build and run the program using the following commands:

```sh
make
./wolfram --rule <RULE_NUMBER> --start <START_GENERATION> --lines <NUM_LINES> --window <WINDOW_SIZE> --move <TRANSLATION>
```

## Available Arguments

- `--rule`: Specifies the ruleset to use (mandatory, no default value).
- `--start`: Specifies the generation number at which to start the display. Default value is 0.
- `--lines`: Specifies the number of lines to display. When omitted, the program continues indefinitely.
- `--window`: Specifies the number of cells to display on each line (line width). If even, the central cell is displayed in the next cell on the right. Default value is 80.
- `--move`: Specifies a translation to apply on the window. If negative, the window is translated to the left. If positive, it’s translated to the right.

## Example Usage

To run Wolfram with different configurations, you can use the following command as an example:

```sh
./wolfram --rule 30 --start 0 --lines 50 --window 100 --move -1
```

This command runs Wolfram with Rule 30, starting from generation 0, displaying 50 lines, with a window size of 100 cells, and translating one step to the left in each generation.

Feel free to experiment with different rules, starting generations, line counts, window sizes, and translations to explore the fascinating patterns generated by elementary cellular automata.
