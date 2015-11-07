#lang at-exp racket
(require scriblib/autobib
         scribble/base)

(provide (all-defined-out)
         in-bib)

(define-cite ~cite citet generate-bibliography)

(define popl "Principles of Programming Languages")
(define pldi "Programming Language Design and Implementation")
(define gpce "Generative Programming: Concepts and Experiences")
(define lfp "Lisp and Functional Programming")
(define icfp "International Conference on Functional Programming")
(define jfp "Journal of Functional Programming")

(define syntax-case
  (make-bib
   #:author (authors "R. Kent Dybvig" "Robert Hieb" "Carl Bruggeman")
   #:title "Syntactic Abstraction in Scheme"
   #:location (journal-location
               "Lisp and Symbolic Computation"
               #:volume "5"
               #:number "4")
   #:date "1993"))

(define essence-of-hygiene
  (make-bib
   #:author (authors "Michael D. Adams")
   #:title "Towards the Essence of Hygiene"
   #:location (proceedings-location popl)
   #:date "2015"))

(define van-tonder
  (make-bib
   #:author (author-name "Andre" "van Tonder")
   #:title "R6RS Libraries and Macros"
   #:url "http://www.het.brown.edu/people/andre/macros/index.html"
   #:date "2007"))

(define expmodel
  (make-bib
   #:author (authors "Matthew Flatt" "Ryan Culpepper" "Robert Bruce Findler" "David Darais")
   #:title "Macros that Work Together: Compile-Time Bindings, Partial Expansion, and Definition Contexts"
   #:location (journal-location
               jfp
               #:volume "22"
               #:number "2")
   #:date "2012"))

(define stxparam
  (make-bib
   #:author (authors "Eli Barzilay" "Ryan Culpepper" "Matthew Flatt")
   #:title "Keeping it Clean with Syntax Parameters"
   #:location (proceedings-location "Workshop on Scheme and Functional Programming")
   #:date "2011"))

(define macro-by-example
  (make-bib
   #:author (authors "Eugene E. Kohlbecker" "Mitchell Wand")
   #:title "Macro-by-Example: Deriving Syntactic Transformations from their Specifications"
   #:location (proceedings-location popl)
   #:date "1987"))

(define macros-that-work
  (make-bib
   #:author (authors "William Clinger" "Jonathan Rees")
   #:title "Macros that Work"
   #:location (proceedings-location popl)
   #:date "1991"))

(define hygiene
  (make-bib
   #:author (authors "Eugene Kohlbecker" "Daniel P. Friedman" "Matthias Felleisen" "Bruce Duba")
   #:title "Hygienic Macro Expansion"
   #:location (proceedings-location lfp)
   #:date "1986"))

(define macromod
  (make-bib
   #:author "Matthew Flatt"
   #:title @elem{Compilable and Composable Macros, You Want it @emph{When?}}
   #:location (proceedings-location icfp)
   #:date "2002"))

(define submodules
  (make-bib
   #:title @elem{Submodules in Racket: You Want it @emph{When}, Again?}
   #:author (authors "Matthew Flatt")
   #:date "2013"
   #:location (proceedings-location gpce)))

(define Chez-modules
  (make-bib
   #:title "Extending the Scope of Syntactic Abstraction"
   #:author (authors "Oscar Waddell" "R. Kent Dybvig")
   #:location (proceedings-location popl)
   #:date "1999"))

(define macro-debugger
  (make-bib
   #:title "Debugging Hygienic Macros"
   #:author (authors "Ryan Culpepper" "Matthias Felleisen")
   #:date "2010"
   #:location (journal-location "Science of Computer Programming"
                                #:volume 75
                                #:number 7)))

(define sweet.js
  (make-bib
   #:title "Sweet.js"
   #:author (authors "Tim Disney" (other-authors))
   #:url "http://sweetjs.org/"
   #:date "2015"))

(define disney2014sweeten
  (make-bib
   #:title "Sweeten your JavaScript: Hygienic Macros for ES5"
   #:author (authors "Tim Disney" "Nathan Faubion" "David Herman" "Cormac Flanagan")
   #:location (proceedings-location "Symposium on Dynamic Languages")
   #:date "2014"))

(define herman-dissertation
  (make-bib
   #:author "David Herman"
   #:title "Dissertation"
   #:location (dissertation-location
               #:institution "Northeastern University")
   #:date "2008"))

(define Romeo
  (make-bib
   #:author (authors "Paul Stansifer" "Mitch Wand")
   #:title "Romeo: a System for More Flexible Binding-Safe Programming"
   #:location (proceedings-location icfp)
   #:date "2014"))

;; http://dblp.uni-trier.de/rec/bibtex/conf/lfp/BawdenR88
(define syntactic-closures
  (make-bib
   #:author (authors "Alan Bawden" "Jonathan Rees")
   #:title "Syntactic Closures"
   #:location (proceedings-location lfp)
   #:date "1988"))

;; http://www.ccs.neu.edu/home/will/papers.html
(define explicit-renaming
  (make-bib
   #:author "William D. Clinger"
   #:title "Hygienic Macros Through Explicit Renaming"
   #:location (journal-location "Lisp Pointers"
                                #:volume "4"
                                #:number "4")
   #:date "1991"))

(define nominal-logic
  (make-bib
   #:title "Nominal Logic, a First Order Theory of Names and Binding"
   #:author "Andrew M. Pitts"
   #:location (journal-location "Information and Computation"
                                #:volume "186"
                                #:number "2")
   #:date "2003"))

(define nominal-sets
  (make-bib
   #:title "Nominal Sets: Names and Symmetry in Computer Science"
   #:author "Andrew M. Pitts"
   #:is-book? #t
   #:location (book-location #:publisher "Cambridge University Press")
   #:date "2013"))

(define scope-graphs
  (make-bib
   #:title "A Theory of Name Resolution"
   #:author (authors "Pierre Neron" "Andrew P. Tolmach" "Eelco Visser" "Guido Wachsmuth")
   #:location (proceedings-location "European Symposium on Programming")
   #:date "2015"))

(define hoas
  (make-bib
   #:title "Higher-order Abstract Syntax"
   #:author (authors "Frank Pfenning" "Conal Elliott")
   #:location (proceedings-location pldi)
   #:date "1988"))
