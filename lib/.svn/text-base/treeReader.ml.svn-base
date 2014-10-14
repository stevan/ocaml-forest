(* Some reader implementations *)

exception Parse_Error of string

class ['a] file_reader 
    ~parser 
    ~tree_root =
object (self : 'a #reader)

    val mutable parser    = (parser    : string -> int * 'a tree) 
    val mutable tree_root = (tree_root : 'a tree)

    method get_parser   = parser
    method set_parser p = parser <- p

    method get_tree_root   = tree_root
    method set_tree_root t = tree_root <- t

    method read (source : in_channel) = 
        let current_tree = ref self#get_tree_root in
        let parser       = self#get_parser        in
        let rec loop line_num =
            let add_new_child_and_loop new_tree =
    			!current_tree#add_child (new_tree);
                current_tree := new_tree;                 		
                loop (line_num + 1)                    
            in
            let add_new_sibling_and_loop new_tree =
    			!current_tree#add_sibling (new_tree);
                current_tree := new_tree;                 		
                loop (line_num + 1)                    
            in                
            try 
                let line            = input_line source in
                let depth, new_tree = parser line       in
                if !current_tree#is_root then
        			add_new_child_and_loop new_tree
                else 
            		let current_tree_depth = !current_tree#get_depth in
            		match current_tree_depth with 
            		    | _ when depth == current_tree_depth -> 
            		        add_new_sibling_and_loop new_tree
                        | _ when depth > current_tree_depth ->
                			if (depth - current_tree_depth) <= 1 then
                                add_new_child_and_loop new_tree 
                            else
                                (* TODO: Convert this to use Format.sprintf *)
                                raise (Parse_Error (
                                        "the difference between the depth (" 
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
                                        ^ "'"))
                		 | _ when depth < current_tree_depth -> 
        		            let rec climb_up_tree t =
        		                match t#get_parent with 
        		                    | None    -> t
        		                    | Some(p) -> if   depth < t#get_depth 
        		                                 then climb_up_tree p 
        		                                 else t
                            in
                            current_tree := climb_up_tree !current_tree;
                            add_new_sibling_and_loop new_tree
                        | _ -> ()                 			        
            with 
               | End_of_file -> ()
               | e           -> raise e
        in loop 0

end

class simple_file_reader ~(tab : string) =    
object (self : string #reader)
    inherit [string] file_reader 
        ~tree_root:(new tree "root")
        ~parser:(fun line ->
            let l = List.rev (ExtLib.String.nsplit line tab) in
            ((List.length (List.tl l)), new tree (List.hd l));
        )

end