# Turning machine implementations

This repo explores different implementations of Turing machines.

Turing machines are here understood in a most basic form -- as a cart on a block of a track that extends indefinitely forward and backward. Each block either has or does not have a symbol. Not having a symbol may be tracked by a ***0*** and having one by a ***1*** (this is hence known as a *2-symbol* Turing machine). The cart is able to run one of four commands: erase the symbol below the cart and move left one block, write the symbol below the cart and move left one block, erase the symbol below the cart and move right one block, or write the symbol below the cart and move right one block. These commands are symbolized as ***L***, ***M***, ***R***, ***S***, respectively.

But the cart does not run just a single command and that's it. The cart runs a command and goes to the next command, and so on, as long as there are commands to run. The cart does this by being in a particular state, signified by a number, which references a list of commands and states in a turing program. Finally, the command to run in the list, as well as the cart's next state, is determined by whether or not the cart is at a symbol. The cart is finished running commands when the cart is in state 0, the halting state.

These implementations assume that Turing programs are in the following form:

    Cmd0 Cmd1 NextState0 NextState1
    Cmd0 Cmd1 NextState0 NextState1
    Cmd0 Cmd1 NextState0 NextState1
    ....

Where


* ***Cmd0*** stands for the command to run given that the cart is not at a symbol (is at a ***0***),
* ***Cmd1*** stands for the command to run given that the cart is at a symbol (is at a ***1***),
* ***NextState0*** is the next state the cart will be in given that the cart is not at a symbol (is at a ***0***),
* ***NextState1*** is the next state the cart will be in given that the cart is at a symbol (is at a ***1***),

Which line to run is determined by the state number of the cart. Lines are finite (in theory as well as, of course, in practice).

A concrete example:

    L M 2 2
    S R 0 0

Relatively simple, this Turing program tells the cart (in state 1, which references line 1) to erase the symbol below it and go left, if it is has no symbol below it (so just go left, since there's no symbol to erase), or else put a symbol below it and go left, if it is has a symbol below it (so just go left, since there's already a symbol below it), and go to the next state, which references line 2. At line 2, the cart is to put a symbol below it and go right, if it is has no symbol below it, or else erase the symbol below it and go right, if it is has a symbol below it, and go to the next state. Since the next state is 0, the cart will halt and finish executing the program.

A repo of Turing programs may be found at [https://github.com/ritterbush/turing-machine-programs](https://github.com/ritterbush/turing-machine-programs).

## Running a Turing machine implementation

For Haskell files, first [install GHC](https://www.haskell.org/ghcup/). Then compile an `.hs` file with

`ghc DataSet.hs --make -o TuringDataSet`

where the final argument is the name of the executable to be created. And run it:

`./TuringDataSet`
