RayLib
[![License](https://img.shields.io/github/license/AntonMeep/raylib.svg?color=blue)](https://github.com/AntonMeep/raylib/blob/master/LICENSE.txt)
[![Alire crate](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/raylib.json)](https://alire.ada.dev/crates/raylib.html)
[![GitHub release](https://img.shields.io/github/release/AntonMeep/raylib.svg)](https://github.com/AntonMeep/raylib/releases/latest)
[![GitHub Workflow Status](https://img.shields.io/github/workflow/status/AntonMeep/raylib/Default)](https://github.com/AntonMeep/raylib/actions)
=======

This is an Ada wrapper to the awesome [raylib](https://www.raylib.com/index.html)
library, a simple and easy-to-use library to enjoy videogames programming.

While raylib is extremely easy to compile and interface with Ada programming
 language, this alire crate tries to make integration even easier!

# Advantages

> This crate is heavily Work-In-Progress, and is not available just yet.
> Below are points that I am aiming for

1. Ready-to-use Alire crate. Just do `alr with raylib` and start hacking
2. Self-contained copy of C raylib included. `alr build` will automagically
build everything necessary
3. Ada-ified! All of the function names are converted to wonderful Train_Case,
standard for Ada.
4. Strongly typed. Where possible, stricter Ada types are used, this gives the
compiler more information to help you and prevents trivial errors
5. Memory managed. Where possible, stack allocated values and controlled types
are used

Otherwise, it is the same awesome raylib underneath.
