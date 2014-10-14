#!/usr/local/bin/ocamlrun /usr/local/bin/ocaml

#use "topfind";;
#require "testSimple";;
#require "extLib";;

#load "forest.cma";;

open TestSimple;;

type tree_node = { id : int; desc : string; }

class complex_indexer ~tree =
object (self)
    inherit [tree_node] Forest.TreeIndexer.simple_indexer ~tree:(tree)

    method build_index =
        let root  = self#get_tree_root and 
            index = self#get_index 
        in
        Hashtbl.add index (root#get_node.id) root;
        root#traverse (fun t ->
            Hashtbl.add index (t#get_node.id) t;
        )

end

let tree_from_reader () = 
    let f = open_in "t/data/test.tree.small" in
    let i = ref 1 in
    let t = new Forest.TreeReader.file_reader
                ~tree_root:(new Forest.tree ({ id = (!i); desc = "root"; }))
                ~parser:(fun line ->
                    let l = List.rev (ExtLib.String.nsplit line "    ") in
                    i := !i + 1;
                    ((List.length (List.tl l)), new Forest.tree ({ id = (!i); desc = (List.hd l) }));
                )
    in
    t#read f;
    close_in f;        
    t#get_tree_root;
;;

let make_index () = 
    let index = new complex_indexer (tree_from_reader ()) in
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











