#!/usr/local/bin/ocamlrun /usr/local/bin/ocaml

#use "topfind";;
#require "extlib";;
#require "netcgi2";;
#require "json-wheel";;

#directory "../../";;
#load "forest.cma";;

open Netcgi_fcgi;;
open Json_type;;

(** Some CGI utils **)

let string_of_json = Json_io.string_of_json ~compact:true
    
let param (cgi:cgi) = cgi#argument_value

let run_cgi f = 
    let wrap f = 
        fun cgi -> 
            try 
                f cgi; 
                cgi#out_channel#commit_work ();            
            with 
                _ -> 
                    cgi#out_channel#rollback_work (); 
                    cgi#out_channel#output_string(string_of_json
                        (Object [
                            "error", String ("There was a problem executing your script")
                        ])                        
                    );
                    cgi#out_channel#commit_work ();
    in 
    let buffered _ ch = new Netchannels.buffered_trans_channel ch in
    if Netcgi_cgi.is_cgi () then
        Netcgi_fcgi.run ~output_type:(`Transactional buffered) (wrap f)
    else
        Netcgi_fcgi.run ~output_type:(`Transactional buffered) (wrap f)

(** tree loader **)

type tree_node = { id : int; desc : string; }

class my_tree (node : tree_node) =
object (self)
    inherit [tree_node] Forest.tree node
    method get_uid = self#get_node.id
end

let tree_from_reader () = 
    let f = open_in "../../t/data/test.tree.small" in
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

(** tree to json **)

class tree_json_service index =
object (self)
    
    method return_json_error tree_id = 
        Object [
            "error", String ("Could not find tree at index (" ^ (string_of_int tree_id) ^ ")")
        ]

    method get_tree_as_json tree_id =
        try 
            let tree = index#get_tree_at tree_id in
            self#prepare_tree_as_json tree
        with 
            _ -> self#return_json_error tree_id
        
    method prepare_tree_as_json (tree : my_tree) =
        Object [
            "uid",      Int tree#get_uid;
            "node",     String tree#get_node.desc;
            "is_leaf",  Bool tree#is_leaf;
            "children", Array (
                List.map (fun t -> 
                    Object [
                        "uid",     Int t#get_uid;
                        "node",    String t#get_node.desc;
                        "is_leaf", Bool t#is_leaf;
                    ]
                ) tree#get_children 
            );
        ]
end

let service = new tree_json_service (make_index ());;

let main (cgi:cgi) =
    let tree_id = param cgi "tree_id" in
    cgi#out_channel#output_string(string_of_json(
        match tree_id with 
            | "" -> (Object [ "error", String "No Tree ID specified" ])
            | _  -> service#get_tree_as_json (int_of_string tree_id)
    ))
;;
    
run_cgi (main);;
