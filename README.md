# guide
Gauche Unpleasant Integrated Design Environment

               
This is basically junk grade emacs written in Gauche.

How to start:

     $ gosh -l./ggc.scm -l./guide.scm
     gosh> (use guide)
     gosh> (guide)

Two buffers, *scratch* and *Messages*, appears.
C-x C-c will exit guide and return to gosh's REPL.

`ggc.scm` consists of three modules from [Gauche Garbage Collection](http://sourceforge.net/p/gauchegc/code/ci/master/tree/) (`ggc.file.util`, `ggc.term.with-raw-mode`, `ggc.text.segment`.)

