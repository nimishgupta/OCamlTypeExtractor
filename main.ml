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

type thriftUnion = 
  { 
    name: string;
    defs : (string * thriftFieldType) list;
  }

type thriftEnum =
  { 
    name: string;
    defs : string list;
  }

type thriftStmt =
  | Exception of thriftException
  | Typedef   of thriftFieldType * string
  | Struct    of thriftStruct
  | Enum      of thriftEnum
  | Union     of thriftUnion
  | Empty

type thriftAST = thriftStmt list


let to_thrift_fieldtype (str : string) : thriftFieldType = match str with
  | "bool"   -> BaseTypes (ThBool)
  | "int"    -> BaseTypes (Thi32)
  | "float"  -> BaseTypes (ThDouble)
  | "string" -> BaseTypes (ThString)
  | _        -> Id str


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
  in (List.fold_right f ex.defs str) ^ "}\n"
 

let thrift_struct_to_string (st : thriftStruct) : string = 
  let str = "struct " ^ st.name ^ " {\n" in
  let f = fun (name, fld) s -> s ^ thrift_field_to_string (fld) ^ " " ^ name ^ ";\n"
  in (List.fold_right f st.defs str) ^ "}\n"

let thrift_enum_to_string (en : thriftEnum) : string =
  let str = "enum " ^ en.name ^ "{\n" in
  (List.fold_right (fun fld s -> s ^ fld ^ ",\n") en.defs str) ^ "}\n"


let thrift_stmt_to_string (stmt : thriftStmt) : string = match stmt with
  | Exception ex -> thrift_exception_to_string ex ^ "\n"
  | Typedef (typ, id) -> "typedef " ^ (thrift_field_to_string typ) ^ " " ^ id ^ "\n"
  | Struct st -> (thrift_struct_to_string st) ^ "\n" 
  | Enum en -> (thrift_enum_to_string en) ^ "\n"
  | Union un -> failwith "union to_string NYI"
  | Empty -> "\n"


let to_thrift_code (ast : thriftAST) : string =
  List.fold_left (fun str stmt -> str ^ thrift_stmt_to_string stmt) "" ast


let longident_to_string (lit : Longident.t) : string =
  match lit with
    | Lident (str) -> str
    | Ldot (_, _) -> failwith "Ldot NYI"
    | Lapply (_, _) -> failwith "Lapply NYI"


let rec _process_core_type (ct : Parsetree.core_type) : thriftFieldType list =
  let open Parsetree in
  match ct.ptyp_desc with
    | Ptyp_any -> failwith "Ptyp_any NYI"
    | Ptyp_var (str) -> Id str :: []
    | Ptyp_arrow (lbl, ct1, ct2) -> failwith "Ptyp_arrow NYI"
    | Ptyp_tuple (ctlst) -> (List.fold_right process_core_type ctlst [])

    (* XXX : Not sure if constr is correct *)
    | Ptyp_constr (loc, ctlst) -> 
        (* (List.fold_right process_core_type ctlst []) *)
        to_thrift_fieldtype (longident_to_string loc.txt) :: []

    | Ptyp_object (cftlst) -> failwith "PTyp_object NYI"
    | Ptyp_class (loc, ctlst, lbllst) -> failwith "PTyp_class NYI"
    | Ptyp_alias (ct, str) -> failwith "PTyp_alias NYI"
    | Ptyp_variant (rflst, flag, lbllstoptn) -> failwith "Ptyp_variant NYI"

    (* XXX : not sure if poly is correct, strlst is not being used at all *)
    | Ptyp_poly (strlst, ct) -> _process_core_type ct
    | Ptyp_package (pkgtyp) -> failwith "Ptyp_package NYI"

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
    | [] -> acc
    | x :: [] -> (name.txt, x) :: acc
    | _ -> failwith "Not supported"


let is_enum (constr_lst): bool = 
  List.for_all (fun (_, ctlst, _, _) -> 0 = List.length ctlst) constr_lst

let to_thrift_enum (name: string) (cnstr_lst) : thriftStmt = 
  let enm = {
              name = name;
              defs = List.fold_right (fun (nm, _, _, _) lst -> nm.Asttypes.txt::lst) cnstr_lst []
            }
  in Enum enm 

(* TODO : Need to create structures out of constructors *)
let to_thrift_union (name: string) (cnstr_lst) : thriftStmt =
  failwith "union support: NYI"
              
let process_variant (name: string) (cnstr_lst) : thriftStmt =
  if is_enum cnstr_lst
  then to_thrift_enum name cnstr_lst
  else to_thrift_union name cnstr_lst
  
  
(* Should return a structure *)
let process_types ((loc, type_decl) : string Asttypes.loc * Parsetree.type_declaration) (acc : thriftAST) : thriftAST = 
  let open Parsetree in
  match type_decl.ptype_kind, type_decl.ptype_manifest with
    | Ptype_abstract, None -> failwith ("Abstract types with no manifests not entertained");
    | Ptype_abstract, Some ct -> 
        (* TODO : check if other states are possible *)
        (match _process_core_type ct with
          | [] -> failwith "empty typedef not expected"
          | x :: [] -> (Typedef (x, loc.txt)) :: acc
          | _ -> failwith "Multiple typedefs not parsing")
      
    (* TODO : ignored manifest for now *)
    | Ptype_variant (cnstr_lst), _ -> (process_variant loc.txt cnstr_lst) :: acc

      (* TODO : ignored manifest for now *)
    | Ptype_record (lbl_dcl_lst), _ -> 
        let strct: thriftStruct  = 
                    { 
                      name = loc.txt;
                      defs = (List.fold_right process_record lbl_dcl_lst [])
                    }
        in (Struct (strct) :: acc)


let process_sig_elem (sig_i : Parsetree.signature_item) (acc : thriftAST): thriftAST = 
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
  List.fold_right process_sig_elem sig_ast []
  
let to_ast inputfile =
  let ic = open_in_bin inputfile in
  let lexbuf = Lexing.from_channel ic in
  Parse.interface lexbuf 


let main () = 
  let in_filename = if Array.length Sys.argv > 1 
                    then Sys.argv.(1)
                    else failwith "Filename not provided"
  in let ast = to_ast in_filename
  in let ppf = Format.err_formatter
  in Format.fprintf ppf "%a@." Printast.interface ast;
  let thrift_ast = to_thrift_ast ast
  in print_endline (to_thrift_code thrift_ast)

let _ = main ()
