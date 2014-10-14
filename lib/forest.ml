
exception No_parent_available of string

type uid = int

let string_of_uid = string_of_int

class ['a] tree node =
object (self)

    val mutable node   = (node : 'a) 
    val mutable parent = (None : 'a tree option)
    val children       = DynArray.create ()

    (** node methods *)
    method get_node   = node
    method set_node n = node <- n

    (** uid method *)
    method get_uid = ((Oo.id self) : uid)

    (** children methods *)
    method get_children = 
        DynArray.to_list children
    
    method get_child ~at =
        DynArray.get children at

    method child_count  = 
        DynArray.length children    

    method add_child t = 
        t#set_parent (self :> 'a tree);
        DynArray.add children t
    
    method add_children ts =
        List.iter (fun t -> t#set_parent (self :> 'a tree)) ts;
        DynArray.append (DynArray.of_list ts) children;

    method insert_child t ~at =
        t#set_parent (self :> 'a tree);
        DynArray.insert children at t   

    method remove_child ~at =
        let removed = self#get_child ~at:at in 
        DynArray.delete children at;
        removed#remove_parent;
        removed     

    method traverse (f : 'a tree -> unit) =
        DynArray.iter (
            fun t -> 
                f t;
                t#traverse f;
        ) children    

    (** parent methods *)

    method get_parent = 
        parent

    method set_parent p = 
        parent <- Some(p)
    
    method remove_parent =
        parent <- None    
    
    method has_parent =
        match parent with
            | None    -> false
            | Some(_) -> true        

    method add_sibling s =
        match parent with
            | None    -> raise (No_parent_available "Cannot add sibling without parent")
            | Some(p) -> p#add_child s
        
    method insert_sibling s ~at =
        match parent with
            | None    -> raise (No_parent_available "Cannot insert sibling without parent")
            | Some(p) -> p#insert_child s ~at:at

    (** misc predicates *)

    method is_leaf = self#child_count == 0
    method is_root = not self#has_parent

    method get_depth =
        match parent with
            | None    -> -1
            | Some(p) -> p#get_depth + 1

    method get_size =
        let count = ref 1 in 
        DynArray.iter (
            fun t -> count := t#get_size + !count;
        ) children;        
        !count
end

