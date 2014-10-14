
(*

This was just an experiment to see if a non-OO version 
of this library was any faster. But it turns out to be 
about the same speed either way. Now, this might have 
something to do with the fact that i am using a record
with mutable fields in it, but it would be difficult 
I think to implement it any other way. 

Either way, this was an intersting exercise.

- SL
    
*)

#use "topfind";;
#require "extLib";;

module Tree =
struct

type 'a tree = {
            node     : 'a;
    mutable parent   : 'a tree option;    
    mutable children : 'a tree list;
}

let create ?children ?parent node = 
    let t = { 
        node     = node; 
        parent   = parent;
        children = []; 
    } in
    match children with 
        | None      -> t 
        | Some kids -> 
            begin
                t.children <- (List.map 
                    (fun c -> 
                        c.parent <- Some(t);                        
                        c) 
                kids); 
                t
            end

let is_leaf t = 
    match t.children with
        | [] -> true
        | _  -> false

let is_root t = 
    match t.parent with
        | None   -> true
        | Some _ -> false

let rec get_depth t =
    match t.parent with
        | None   -> -1
        | Some p -> get_depth p + 1
    
let add_child t child = 
    child.parent <- Some t;
    t.children <- t.children @ [ child ]

let add_sibling t sibling =
    match t.parent with
        | None   -> failwith "Cannot add sibling without parent"
        | Some p -> add_child p sibling

let child_count t = 
    List.length t.children

let get_child t ~at = 
    List.nth t.children at

let rec size t = 
    1 + (List.fold_left (+) 0 (List.map size t.children))

let rec traverse t f = 
    List.iter (fun t -> f t; traverse t f) t.children    

let string_of_tree ?(buffer = 1024) t = 
    let b = Buffer.create buffer in
    traverse t (fun t -> Buffer.add_string b
                    ((String.make ((abs (get_depth t)) * 4) ' ')
                     ^ t.node 
                     ^ "\n"));
    Buffer.contents b

let tree_of_string 
        ?(node_converter = (fun x -> x))
        ~line_parser
        ~source
        ~tree_root 
        () =
    let current_tree = ref tree_root   in
    let rec loop line_num =
        let add_new_child_and_loop new_tree =
			add_child !current_tree new_tree;
            current_tree := new_tree;                 		
            loop (line_num + 1)                    
        in
        let add_new_sibling_and_loop new_tree =
			add_sibling !current_tree (new_tree);
            current_tree := new_tree;                 		
            loop (line_num + 1)                    
        in                
        try 
            let line            = input_line source in
            let depth, new_tree = line_parser line  in
            if is_root !current_tree then
    			add_new_child_and_loop new_tree
            else 
        		let current_tree_depth = get_depth !current_tree in
        		match current_tree_depth with 
        		    | _ when depth == current_tree_depth -> 
        		        add_new_sibling_and_loop new_tree
                    | _ when depth > current_tree_depth ->
            			if (depth - current_tree_depth) <= 1 then
                            add_new_child_and_loop new_tree 
                        else
                            failwith ("the difference between the depth (" 
                                     ^ (string_of_int depth) 
                                     ^ ") and "  
                                     ^ "the tree depth ("
                                     ^ (string_of_int current_tree_depth)
                                     ^ ") is too much ("
                                     ^ (string_of_int (depth - current_tree_depth)) 
                                     ^ ") at line number: " 
                                     ^ (string_of_int line_num) 
                                     ^ " with line text:\n'"
                                     ^ line 
                                     ^ "'")
            		 | _ when depth < current_tree_depth -> 
    		            let rec climb_up_tree t =
    		                match t.parent with 
    		                    | None    -> t
    		                    | Some p -> if   depth < (get_depth t) 
    		                                then climb_up_tree p 
    		                                else t
                        in
                        current_tree := climb_up_tree !current_tree;
                        add_new_sibling_and_loop new_tree
                    | _ -> ()                 			        
        with 
           | End_of_file -> ()
           | e           -> raise e
    in loop 0;
    tree_root

end;;


let () = 
    let source = open_in "../t/data/test.tree" in
    let t = Tree.tree_of_string 
                ~source:source
                ~tree_root:(Tree.create "root")
                ~line_parser:(fun line ->
                    let l = List.rev (ExtLib.String.nsplit line "    ") in
                    ((List.length (List.tl l)), (Tree.create (List.hd l)));
                )
                () in
    let parse_time = Sys.time () in
    print_float (parse_time);
    print_newline ();
    print_int (Tree.size t);
    print_newline ();    
;;





