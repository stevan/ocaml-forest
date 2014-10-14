#!/usr/local/bin/ocamlrun /usr/local/bin/ocaml

#use "topfind";;
#require "extLib";;

#load "forest.cma";;

let _ = 
    let f = open_in "t/data/test.tree" in
    let t = new Forest.TreeReader.simple_file_reader ~tab:"    " in
    t#read f;
    close_in f;    
    print_string ("seconds to parse tree: ");    
    print_float (Sys.time ());
    print_newline ();
    print_string ("size of tree: ");
    print_int (t#get_tree_root#get_size);
    print_newline ();    
;;





