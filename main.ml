type thriftBaseTypes =
  | ThBool
  | ThByte
  | Thi16
  | Thi32
  | Thi64
  | ThDouble
  | ThString

type thriftFieldType =
  | Id of string
  | BaseTypes of thriftBaseTypes
  | ContainerTypes of thriftContainerTypes

and  thriftContainerTypes = 
  | MapType of thriftFieldType * thriftFieldType
  | SetType of thriftFieldType
  | ListType of thriftFieldType


type thriftException = 
  {
    name: string;
    defs : (string * thriftFieldType) list;
  }

type thriftStruct = 
  { 
    name: string;
    defs : (string * thriftFieldType) list;
  }

type thriftStmt =
  | Exception of thriftException
  | Typedef   of thriftFieldType
  | Struct    of thriftStruct
  | Empty

type thriftAST = thriftStmt list


let thrift_basetypes_to_string (bt : thriftBaseTypes) : string = match bt with
  | ThBool -> "bool"
  | ThByte -> "byte"
  | Thi16 -> "i16"
  | Thi32 -> "i32"
  | Thi64 -> "i64"
  | ThDouble -> "double"
  | ThString -> "string"


let rec thrift_field_to_string (fld: thriftFieldType) : string = match fld with
  | Id (str) -> str
  | BaseTypes (bt) -> thrift_basetypes_to_string bt
  | ContainerTypes (ct) -> thrift_containertypes_to_string ct

and thrift_containertypes_to_string (ct : thriftContainerTypes) : string = match ct with
  | MapType (fld1, fld2) -> "map <" ^ thrift_field_to_string fld1 ^ ", " ^ thrift_field_to_string fld2 ^ ">"
  | SetType (fld) -> "set <" ^ thrift_field_to_string fld ^ ">"
  | ListType (fld) -> "list <" ^ thrift_field_to_string fld ^ ">"

let thrift_exception_to_string (ex : thriftException) : string =
  let str = "exception " ^ ex.name ^ " {\n" in
  let f = fun (name, fld) s -> s ^ thrift_field_to_string (fld) ^ " " ^ name ^ ";\n"
  in List.fold_right f ex.defs str
 
let thrift_struct_to_string (st : thriftStruct) : string = 
  let str = "struct " ^ st.name ^ " {\n" in
  let f = fun (name, fld) s -> s ^ thrift_field_to_string (fld) ^ " " ^ name ^ ";\n"
  in List.fold_right f st.defs str

let thrift_stmt_to_string (stmt : thriftStmt) : string = match stmt with
  | Exception ex -> thrift_exception_to_string ex ^ "\n"
  | Typedef td -> failwith "NYI"
  | Struct st -> thrift_struct_to_string st ^ "\n"
  | Empty -> "\n"

let to_thrift_code (ast : thriftAST) : string =
  List.fold_left (fun str stmt -> str ^ thrift_stmt_to_string stmt) "" ast


let rec _process_core_type (ct : Parsetree.core_type) : thriftFieldType list =
  let open Parsetree in
  match ct.ptyp_desc with
    | Ptyp_any -> failwith "NYI"
    | Ptyp_var (str) -> Id str :: []
    | Ptyp_arrow (lbl, ct1, ct2) -> failwith "NYI"
    | Ptyp_tuple (ctlst) -> (List.fold_right process_core_type ctlst [])
    | Ptyp_constr (loc, ctlst) -> failwith "NYI"
    | Ptyp_object (cftlst) -> failwith "NYI"
    | Ptyp_class (loc, ctlst, lbllst) -> failwith "NYI"
    | Ptyp_alias (ct, str) -> failwith "NYI"
    | Ptyp_variant (rflst, flag, lbllstoptn) -> failwith "NYI"
    | Ptyp_poly (strlst, ct) -> failwith "NYI"
    | Ptyp_package (pkgtyp) -> failwith "NYI"

and process_core_type (ct : Parsetree.core_type) (acc : thriftFieldType list) : thriftFieldType list = 
  _process_core_type ct @ acc


let to_field (index : int) (ft : thriftFieldType) : (string * thriftFieldType) =
  let field_name = "field" ^ (string_of_int index) in
  (field_name, ft)


let process_record ((name, _, ct, _) : (string Asttypes.loc   * 
                                        Asttypes.mutable_flag * 
                                        Parsetree.core_type   * 
                                        Location.t)) (acc : (string * thriftFieldType) list) : (string * thriftFieldType) list =
  match _process_core_type ct with
    | [] -> failwith "Not expected"
    | x :: [] -> (name.txt, x) :: acc
    | _ -> failwith "Not expected"
  


(* Should return a structure *)
let process_types ((loc, type_decl) : string Asttypes.loc * Parsetree.type_declaration) (acc : thriftAST) : thriftAST = 
  let open Parsetree in
  match type_decl.ptype_kind with
    | Ptype_abstract -> failwith "ignore"
      
      (* Need to use unions *)
    | Ptype_variant (cnstr_lst) -> failwith "NYI"

    | Ptype_record (lbl_dcl_lst) -> 
        let strct = 
                    { 
                      name = loc.txt;
                      defs = (List.fold_right process_record lbl_dcl_lst [])
                    }
        in (Struct (strct) :: acc)


let process_sig_elem (acc : thriftAST) (sig_i : Parsetree.signature_item) : thriftAST = 
  match sig_i.psig_desc with
  | Psig_type type_decls -> 
      (List.fold_right process_types type_decls []) @ acc

  | Psig_exception (name, exn_decls) -> 
    let excptn : thriftException = {
                   name = name.txt;
                   defs = let tlst = List.fold_right process_core_type exn_decls []
                          in List.mapi to_field tlst
                 }
    in Exception (excptn) :: acc

  | _ -> print_endline ("Ignoring everything except type declarations and exceptions");
         Empty :: acc


let to_thrift_ast (sig_ast : Parsetree.signature) : thriftAST = 
  List.fold_left process_sig_elem [] sig_ast 
  
let to_ast inputfile =
  let ic = open_in_bin inputfile in
  let lexbuf = Lexing.from_channel ic in
  Parse.interface lexbuf 


let main () = 
  let in_filename = if Array.length Sys.argv > 1 
                    then Sys.argv.(1)
                    else failwith "Filename not provided"
  in let ast = to_ast in_filename
  in to_thrift_ast ast

let _ = main ()
