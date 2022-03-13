open Json_encoding
open Data_types

let api_config = obj1 (opt "port" int)

type nonrec opam_entry = Data_types.opam_entry = {
  name : string;
  path : string;
  version : string;
  synopsis : string;
} [@@deriving json_encoding]

let packages = list opam_entry_enc

type nonrec lib_entry = Data_types.lib_entry = {
  name : string;
  path : string;
  opam : string ;
  opampath : string;
} [@@deriving json_encoding]

let libraries = list lib_entry_enc

type nonrec meta_entry = Data_types.meta_entry = {
  namemeta : string ;
  path : string ;
  opam : string ;
  opampath : string ;
} [@@deriving json_encoding]

let metas = list meta_entry_enc

type nonrec module_entry = Data_types.module_entry = {
  name : string;
  path : string;
  opam : string;
  opampath: string;
  libs : (string * string) list;
} [@@deriving json_encoding]

let modules = list module_entry_enc

type nonrec source_entry = Data_types.source_entry = {
  namesrc : string;
  path : string;
  opam : string;
  opampath : string;
} [@@deriving json_encoding]

let sources = list source_entry_enc

let entries =
  let cases =
    [case
      ~title:"Opam"
      packages
      (function | Opam s -> Some s | _ -> None )
      (function s -> Opam s);
    case
      ~title:"Lib"
      libraries
      (function | Lib s -> Some s | _ -> None )
      (function s -> Lib s);
    case
      ~title:"Mdl"
      modules
      (function | Mdl s -> Some s | _ -> None )
      (function s -> Mdl s);
    case
      ~title:"Meta"
      metas
      (function | Meta s -> Some s | _ -> None )
      (function s -> Meta s);
    case
      ~title:"Src"
      sources
      (function | Src s -> Some s | _ -> None )
      (function s -> Src s)
    ]
  in
    union cases

let modules_name = list @@ tup2 string string

type nonrec val_entry = Data_types.val_element = {
  ident : string;
  value : string;
  mdl : string;
  mdlpath : string;
  opam : string;
  opampath : string;
} [@@deriving json_encoding]

let vals = list val_entry_enc

type nonrec type_entry = Data_types.type_element = {
  ident : string;
  mdl : string;
  mdlpath : string;
  opam : string;
  opampath : string;
} [@@deriving json_encoding]

let types = list type_entry_enc

type nonrec class_entry = Data_types.class_element = {
  ident : string;
  mdl : string;
  mdlpath : string;
  is_class_type : bool;
  opam : string;
  opampath : string;
} [@@deriving json_encoding]

let classes = list class_entry_enc

let ocaml_elements =
  let cases =
    [case
      ~title:"Val"
      vals
      (function | Val s -> Some s | _ -> None)
      (function s -> Val s);
    case
      ~title:"Type"
      types
      (function | Type s -> Some s | _ -> None)
      (function s -> Type s);
    case
      ~title:"Class"
      classes
      (function | Class s -> Some s | _ -> None)
      (function s -> Class s)
    ]
  in
    union cases

type command_result = Data_types.command_result = {
  result : string;
} [@@deriving json_encoding]

type search_result = Data_types.search_result = {
  packages : opam_entry list;
  libraries : lib_entry list;
  modules : module_entry list;
} [@@deriving json_encoding]

type sources_occurence = Data_types.sources_occurence = {
  opamname : string;
  srcpath: string;
  filename: string;
  occpos: int;
  occline: string;
  occpath: string;
} [@@deriving json_encoding]

type sources_search_result = Data_types.sources_search_result = {
  totaloccs : int;
  occs : sources_occurence list
} [@@deriving json_encoding]
