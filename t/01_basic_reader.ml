#!/usr/local/bin/ocamlrun /usr/local/bin/ocaml

#use "topfind";;
#require "testSimple";;
#require "extLib";;

#load "forest.cma";;

open TestSimple;;

let tree_from_reader () = 
    let f = open_in "t/data/test.tree.small" in
    let t = new Forest.TreeReader.file_reader
                ~tree_root:(new Forest.tree "root")
                ~parser:(fun line ->
                    let l = List.rev (ExtLib.String.nsplit line "    ") in
                    ((List.length (List.tl l)), new Forest.tree (List.hd l));
                )
    in
    t#read f;
    close_in f;        
    t#get_tree_root;
;;

let tree_from_simple_text_reader () = 
    let f = open_in "t/data/test.tree.small" in
    let t = new Forest.TreeReader.simple_file_reader ~tab:"    " in
    t#read f;
    close_in f;        
    t#get_tree_root;
;;

plan 2;;
    
let t = tree_from_reader() in
let w = new Forest.TreeWriter.simple_ASCII_writer ~tree:t () in
is (w#as_string)
   (ExtLib.input_file "t/data/test.tree.small") 
   "... parsed the tree correctly (reader)";;

let t2   = tree_from_simple_text_reader() in
let w2 = new Forest.TreeWriter.simple_ASCII_writer ~tree:t2 () in
is (w2#as_string)
   (ExtLib.input_file "t/data/test.tree.small") 
   "... parsed the tree correctly (simple_text_reader)";;





