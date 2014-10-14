#!/usr/local/bin/ocamlrun /usr/local/bin/ocaml

#use "topfind";;
#require "testSimple";;
#require "extLib";;

#load "forest.cma";;

open TestSimple;;

let tree          = new Forest.tree "root";;
let child         = new Forest.tree "1.1";;
let grandchild    = new Forest.tree "1.1.1";;
let grandchild2   = new Forest.tree "1.1.2";;
let child2        = new Forest.tree "1.2";;
let grandchild2_1 = new Forest.tree "1.2.1";;
let grandchild2_2 = new Forest.tree "1.2.2";;
let child3        = new Forest.tree "1.3";;

plan 102;;
    
is (tree#get_node) "root" "... got the right node value";;
is (tree#set_node "1.0") () "... set_node worked";;
is (tree#get_node) "1.0" "... got the right node value";;
is (tree#get_depth) (-1) "... tree depth is -1";;
is (tree#get_size) 1 "... tree size is 1";;

ok (tree#is_leaf) "... currently is a leaf";;
ok (tree#is_root) "... currently is a root";;

is (tree#child_count) 0 "... currently has no children";;
is (tree#get_children) [] "... currently has no children";;
ok (not tree#has_parent) "... tree has no parent";;

is (tree#get_parent) (None) "... tree has no parent";;

is (child#get_node) "1.1" "... got the right node value for child";;
is (child#get_depth) (-1) "... child depth is -1";;

ok (child#is_leaf) "... child currently is a leaf";;
ok (child#is_root) "... child currently is a root";;
ok (not child#has_parent) "... child has no parent";;

is (child#get_parent) (None) "... child has no parent";;

(* add first child *)

is (tree#add_child child) () "... added a child successfully";;

ok (not tree#is_leaf) "... tree is no longer a leaf";;
ok (tree#is_root) "... but tree is still a root";;
ok (not tree#has_parent) "... tree still has no parent";;
is (tree#get_size) 2 "... tree size is 2";;

is (tree#get_parent) (None) "... tree still has no parent";;

ok (child#is_leaf) "... child is still a leaf";;
ok (not child#is_root) "... child is no longer a root";;
ok (child#has_parent) "... child has parent now";;
is (child#get_depth) 0 "... child depth is 0";;

is (child#get_parent) (Some tree) "... child has a parent (tree)";;

is (tree#child_count) 1 "... tree currently has 1 child";;
is (tree#get_children) [ child ] "... trees child is child";;
is (tree#get_child ~at:0) child "... got the child at tree idx 0";;

ok (grandchild#is_leaf) "... grandchild currently is a leaf";;
ok (grandchild#is_root) "... grandchild currently is a root";;
ok (not grandchild#has_parent) "... grandchild has no parent";;
is (grandchild#get_depth) (-1) "... grandchild depth is -1";;

is (grandchild#get_parent) (None) "... grandchild has no parent";;

(* add first grandchild *)

is (child#add_child grandchild) () "... added a child successfully";;

is (tree#child_count) 1 "... tree currently still has 1 child";;
is (tree#get_children) [ child ] "... trees child still is child";;
is (tree#get_size) 3 "... tree size is 3";;

is (child#child_count) 1 "... child currently has 1 child";;
is (child#get_children) [ grandchild ] "... child's child is grandchild";;

ok (not child#is_leaf) "... child is no longer a leaf";;
ok (not child#is_root) "... child is no longer a root";;

is (child#get_parent) (Some tree) "... child has a parent (tree)";;

ok (grandchild#is_leaf) "... grandchild is still a leaf";;
ok (not grandchild#is_root) "... grandchild is no longer a root";;
ok (grandchild#has_parent) "... grandchild has a parent";;
is (grandchild#get_depth) (1) "... grandchild depth is 1";;

is (grandchild#get_parent) (Some child) "... grandchild has a parent (child)";;

ok (grandchild2#is_leaf) "... grandchild2 is a leaf";;
ok (grandchild2#is_root) "... grandchild2 is a root";;
ok (not grandchild2#has_parent) "... grandchild2 has no parent";;
is (grandchild2#get_depth) (-1) "... grandchild2 depth is -1";;

(* add second grandchild (as sibling) *)

is (grandchild#add_sibling grandchild2) () "... added sibling successfully";;

ok (grandchild2#is_leaf) "... grandchild2 is a leaf";;
ok (not grandchild2#is_root) "... grandchild2 is no longer a root";;
ok (grandchild2#has_parent) "... grandchild2 now has a parent";;
is (grandchild2#get_depth) (1) "... grandchild2 depth is 1";;

is (grandchild2#get_parent) (Some child) "... grandchild2 has a parent (child)";;

is (tree#child_count) 1 "... tree currently still has 1 child";;
is (tree#get_children) [ child ] "... trees child still is child";;
is (tree#get_size) 4 "... tree size is 4";;

is (child#child_count) 2 "... child now has 2 child";;
is (child#get_children) [ grandchild; grandchild2 ] "... child children are grandchild and grandchild2";;

(* add/insert second and third child *)

is (tree#add_child child3) () "... added the second child successfully";

is (tree#child_count) 2 "... tree currently now has 2 children";;
is (tree#get_children) [ child; child3 ] "... trees children are now child, child3";;
is (tree#get_size) 5 "... tree size is 5";;

ok (not child2#has_parent) "... child2 has no parent";;

is (tree#insert_child child2 ~at:1) () "... inserted another child successfully";;

ok (child2#has_parent) "... child2 has the parent successfully";;
is (child2#get_parent) (Some tree) "... and that parent is tree";;

is (tree#child_count) 3 "... tree currently now has 3 children";;
is (tree#get_children) [ child; child2; child3 ] "... trees children are now child, child2, child3";;
is (tree#get_size) 6 "... tree size is 6";;

(* add/insert children as siblings *)

ok (child2#is_leaf) "... child2 is a leaf";;
is (child2#child_count) 0 "... child2 has no children";;

ok (not grandchild2_2#has_parent) "... grandchild2_2 has no parent";;

is (child2#add_child grandchild2_2) () "... added the grandchild successfully";;

is (tree#get_size) 7 "... tree size is 7";;

ok (grandchild2_2#has_parent) "... grandchild2_2 has a parent now";;
is (grandchild2_2#get_parent) (Some child2) "... grandchild2_2 parent is child2";;

ok (not child2#is_leaf) "... child2 is no longer a leaf";;
is (child2#child_count) 1 "... child2 has one child";;

ok (not grandchild2_1#has_parent) "... grandchild2_1 has no parent";;

is (grandchild2_2#insert_sibling grandchild2_1 ~at:0) () "... inserted the grandchild successfully";;

ok (grandchild2_1#has_parent) "... grandchild2_1 has a parent now";;
is (grandchild2_1#get_parent) (Some child2) "... grandchild2_1 parent is child2";;

is (child2#child_count) 2 "... child2 has two children";;

is (tree#get_size) 8 "... tree size is 8";;

(* look at entire tree *)

(let all_uids = ref [] in
    tree#traverse (fun t -> all_uids := t#get_uid :: !all_uids);
    is (List.length (ExtLib.List.unique !all_uids)) 7 "... traversed the tree correctly";
);;

(let all_children = ref [] in
    tree#traverse (fun t -> all_children := t#get_node :: !all_children);
    is (List.rev !all_children)
        [ "1.1"; "1.1.1"; "1.1.2"; "1.2"; "1.2.1"; "1.2.2"; "1.3" ]
        "... traversed the tree correctly";
);;

(let viz = ref "" in
    tree#traverse (fun t -> 
        viz := !viz 
            ^ (String.make (t#get_depth * 4) ' ') 
            ^ t#get_node 
            ^ "\n"
    );
    is (!viz)
"1.1
    1.1.1
    1.1.2
1.2
    1.2.1
    1.2.2
1.3
"
        "... traversed the tree correctly";
);;

(* now remove some *)

is (tree#remove_child ~at:2) child3 "... removed the child successfully";;
is (tree#get_size) 7 "... tree size is 7";;
ok (not child3#has_parent) "... child3 does not have a parent anymore";;

(let all_children = ref [] in
    tree#traverse (fun t -> all_children := t#get_node :: !all_children);
    is (List.rev !all_children)
        [ "1.1"; "1.1.1"; "1.1.2"; "1.2"; "1.2.1"; "1.2.2"; ]
        "... traversed the tree (altered) correctly";
);;

is ((tree#get_child ~at:0)#remove_child ~at:0) grandchild "... removed the grandchild successfully";;
is (tree#get_size) 6 "... tree size is 6";;
ok (not grandchild#has_parent) "... grandchild does not have a parent anymore";;

(let all_children = ref [] in
    tree#traverse (fun t -> all_children := t#get_node :: !all_children);
    is (List.rev !all_children)
        [ "1.1"; "1.1.2"; "1.2"; "1.2.1"; "1.2.2"; ]
        "... traversed the tree (altered) correctly";
);;


