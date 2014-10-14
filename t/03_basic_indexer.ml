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

let make_index () = 
    let index = new Forest.TreeIndexer.simple_uid_indexer (make_tree ()) in
    index#build_index;
    index
;;

let index = make_index ();;

plan 12;;

is (List.length index#get_index_keys) 11 "... got the right number of keys in the index";;

List.iter (
    fun key -> 
        let t = index#get_tree_at key in
        is (t#get_uid) key ("... got the tree which matches the uid for node : " ^ t#get_node);
) index#get_index_keys;;








