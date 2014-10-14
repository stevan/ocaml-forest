(* some writer implementations *)

(*
    NOTE: 
    think about a string_of_node method
    here that will allow you to customize
    how the node gets tranformed
*)

class virtual ['a] simple_writer ~tree =
object (self : 'a #writer)
    method get_tree = tree
    method write out = output_string out self#as_string
    method virtual as_string : string         
end

class simple_ASCII_writer 
    ~tree 
    ?(indent      = 4) 
    ?(indent_char = ' ')
    ?(buffer      = 1024)
    () =
object (self : string #writer)
    inherit [string] simple_writer ~tree:(tree)
    
    method as_string = 
        let b = Buffer.create buffer in
        self#get_tree#traverse (fun t -> 
            Buffer.add_string b (
                (String.make (t#get_depth * indent) indent_char) 
                ^ t#get_node
                ^ "\n"                
            )
        );
        Buffer.contents b           

end