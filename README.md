# Doom Common Lisp #

This project is an implementation of Doom data structures and algorithms (but not graphics or gameplay) in Common Lisp as a learning purpose. The ultimate goal is to have:

* IWAD and PWAD file read
* Read and modify levels
* Have levels in memory
* Be able to work with the levels in memory with an API
* Expose the data so another rendering engine can be used
* Display level maps and be able to build new maps with an API
* Learn Common Lisp

## Why Common Lisp? ##

I've been a fan of Doom for years and I always wanted to learn proper Common Lisp, after I started reading Paul Graham's On Lisp (http://www.paulgraham.com/onlisp.html) I started to fall in love with Common Lisp promises and I wanted to use its capabilities to really learn to program. Actually I started reading Let Over Lambda first (http://letoverlambda.com/) which brought me to a state of epiphany after so many years of being a programmer.

## Project Milestones ##

The first milestone will be to be able to read a WAD file from Common Lisp, and to be able to check its elements. I will write a Common Lisp program that will be able to read a WAD file, and store in memory its contents.

### Milestone 1 ###

Being able to read a wad file:

* [DONE] Correctly identify a file as a wad file (check header)
* [DONE] Read wad file directory
* [DONE] Read wad file MAP lump
* [TODO] Read map structure
