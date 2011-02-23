# lc

A simple interpreter for the untyped lambda calculus.

(this reeks of ugly hack, but I wanted to play with fnparse which is
magical)

## Usage

    (use 'lc)
    (to-str (beta (parse "(^x.x b)")))

## License

Copyright (C) 2011 Andrew Gwozdziewycz

Distributed under the GPLv3
