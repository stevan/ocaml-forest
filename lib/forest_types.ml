(* some basic class types for readers/writers/indexers *)

class type ['a] reader =
object
    method get_parser    : string -> int * 'a tree
    method get_tree_root : 'a tree
    method read          : in_channel -> unit
end

class type ['a] writer =
object
    method get_tree  : 'a tree
    method as_string : string
    method write     : out_channel -> unit
end

class type ['a] indexer =
object
    method get_tree_root  : 'a tree
    method get_index      : (uid, 'a tree) Hashtbl.t
    method build_index    : unit
    method clear_index    : unit
    method get_index_keys : uid list
    method get_tree_at    : uid -> 'a tree
end