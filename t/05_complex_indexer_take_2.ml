#!/usr/local/bin/ocamlrun /usr/local/bin/ocaml

#use "topfind";;
#require "testSimple";;
#require "extLib";;

#load "forest.cma";;

open TestSimple;;

type tree_node = { id : int; desc : string; }

class my_tree (node : tree_node) =
object (self)
    inherit [tree_node] Forest.tree node

    method get_uid = self#get_node.id

end

let tree_from_reader () = 
    let f = open_in "t/data/test.tree.small" in
    let i = ref 1 in
    let t = new Forest.TreeReader.file_reader
                ~tree_root:(new my_tree ({ id = (!i); desc = "root"; }))
                ~parser:(fun line ->
                    let l = List.rev (ExtLib.String.nsplit line "    ") in
                    i := !i + 1;
                    ((List.length (List.tl l)), new my_tree ({ id = (!i); desc = (List.hd l) }));
                )
    in
    t#read f;
    close_in f;        
    t#get_tree_root;
;;

let make_index () = 
    let t = tree_from_reader () in
    let index = new Forest.TreeIndexer.simple_uid_indexer t in
    index#build_index;
    index
;;

let index = make_index ();;

plan 34;;

is (List.length index#get_index_keys) 33 "... got the right number of keys in the index";;

List.iter (
    fun key -> 
        let t = index#get_tree_at key in
        is t#get_node.id key ("... got the tree which matches the node id key : " ^ t#get_node.desc);
) index#get_index_keys;;











