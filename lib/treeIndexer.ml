
exception Tree_Not_Found of string

class virtual ['a] simple_indexer ~tree =
    let index = Hashtbl.create (tree#get_size) in
object (self : 'a #indexer)

    val index     = (index : (uid, 'a tree) Hashtbl.t)
    val tree_root = (tree  : 'a tree)

    method get_tree_root = tree_root
    method get_index     = index
    
    method clear_index = 
        Hashtbl.clear index   
    
    method get_index_keys = 
        Hashtbl.fold (fun k _ acc -> k :: acc) index []

    method get_tree_at key =
        try 
            Hashtbl.find index key
        with 
            | Not_found -> 
                raise (Tree_Not_Found ("Could not find tree for key '" 
                                        ^ (string_of_uid key) 
                                        ^ "' in index"))
            | e         -> raise e

    method virtual build_index : unit
end

class ['a] simple_uid_indexer ~tree =
object (self : 'a #indexer)
    inherit ['a] simple_indexer ~tree:(tree)

    method build_index =
        let root  = self#get_tree_root and 
            index = self#get_index 
        in
        Hashtbl.add index (root#get_uid) root;
        root#traverse (fun t ->
            Hashtbl.add index (t#get_uid) t;
        )

end