# guide
Gauche Unpleasant Integrated Design Environment

               
This is basically junk grade emacs written in Gauche.

How to play:

     $ gosh guide.scm
     
Example: 
     
      C-x 2 C-x b            => split screen into *scratch* and *Messages*.
      C-x o                  => Goto *scratch*
      (+ 1 2) C-x C-e        => shows `=> 3` in *Messages*.


`ggc.scm` consists of three modules from [Gauche Garbage Collection](http://sourceforge.net/p/gauchegc/code/ci/master/tree/) (`ggc.file.util`, `ggc.term.with-raw-mode`, `ggc.text.segment`.)

