#!/usr/local/bin/ocamlrun /usr/local/bin/ocaml

#use "topfind";;
#require "testSimple";;
#require "extLib";;

#load "forest.cma";;

open TestSimple;;

let make_tree () = 
    let t = new Forest.tree "root" in
    t#add_children [
        new Forest.tree "1.0";
        (let x = new Forest.tree "2.0" in
         x#add_children [
             new Forest.tree "2.1";        
             new Forest.tree "2.2";                
         ]; x);
        new Forest.tree "3.0";                
        (let x = new Forest.tree "4.0" in
         x#add_children [
             (let x = new Forest.tree "4.1" in
              x#add_children [
                new Forest.tree "4.1.1";        
                new Forest.tree "4.1.2";                
              ]; x);        
             new Forest.tree "4.2";                
         ]; x
        );        
    ];    
    t
;;

plan 1;;

let w = new Forest.TreeWriter.simple_ASCII_writer ~tree:(make_tree ()) () in
is (w#as_string) 
"1.0
2.0
    2.1
    2.2
3.0
4.0
    4.1
        4.1.1
        4.1.2
    4.2
" "... our basic writer worked as expected";;    






