
(** Module [Data_types] defines all server-side types also used by user-side application *)

(** {1 Return types} *)

type www_server_info = {
  www_apis : string list;
}
(** Type that lists all server host names *)

type nonrec opam_entry = {
  name : string;
  path : string;
  version : string;
  synopsis : string;
}
(** Opam entry *)

type nonrec packages = opam_entry list
(** List of opam entries *)

type nonrec lib_entry = {
  name : string;
  path : string;
  opam : string ;
  opampath : string;
}
(** Library entry *)

type nonrec libraries = lib_entry list
(** List of library entries *)

type nonrec meta_entry = {
  namemeta : string ;
  path : string ;
  opam : string ;
  opampath : string ;
}
(** Meta entry *)

type nonrec metas = meta_entry list
(** List of meta entries *)

type nonrec module_entry = {
  name : string;
  path : string;
  opam : string;
  opampath: string;
  libs : (string * string) list;
}
(** Module entry *)

type nonrec modules = module_entry list
(** List of module entries *)

type nonrec source_entry = {
  namesrc : string;
  path : string;
  opam : string;
  opampath : string;
}
(** Source entry *)

type nonrec sources = source_entry list
(** List of source entries *)

(** Type that regroups every entry that is a component of OCaml ecosystem (package, library, module, etc.). Result of [getEntries] service. *)
type entries =
  | Opam of packages
  | Lib of libraries
  | Mdl of modules
  | Meta of metas
  | Src of sources

type nonrec val_element = {
  ident : string;
  value : string;
  mdl : string;
  mdlpath : string;
  opam : string;
  opampath : string;
}
(** Value element (OCaml function) *)

type nonrec vals = val_element list
(** List of value elements *)

type nonrec type_element = {
  ident : string;
  mdl : string;
  mdlpath : string;
  opam : string;
  opampath : string;
}

type nonrec types = type_element list
(** List of type elements *)

type nonrec class_element = {
  ident : string;
  mdl : string;
  mdlpath : string;
  isclasstype : int;
  opam : string;
  opampath : string;
}

type nonrec classes = class_element list
(** List of class elements *)

type ocaml_elements =
  | Val of vals
  | Type of types
  | Class of classes
(** Type that regroups every element that is a component of a
    module (val, type, class, etc.). Result of [getElements] service *)

type command_result = {
  result : string;
}
(** Result of [exec_command] service *)

type search_result = {
  packages : packages;
  libraries : libraries;
  modules : modules;
}
(** Result of [search] service *)

type sources_occurence = {
  (* package info *)
  opamname : string;
  srcpath: string;
  (* path to the file *)
  filename: string;
  (* line number of the match *)
  occpos: int;
  (* line content *)
  occline: string;
  (* path to the line at the site *)
  occpath: string;
}
(** Type that describes one occurence of the match for
    the fulltext search within sources *)

type sources_search_result = {
  totaloccs : int;
  occs : sources_occurence list
}
(** Result of [search_sources] service *)

(** {1 Exceptions} *)

(** Possible error types that search-api can raise *)
type server_error_type =
  | Invalid_regex
  | No_sources_config
  | Unknown

(** Search api exception. When raised by server code
    ez_api encapsulate it inside [EzReq_lwt_S.KnownError]. *)
exception Search_api_error of server_error_type

(** Encapsulate server_error_type *)
let search_api_error typ = Search_api_error typ

let server_error_type err =
  match err with
  | Search_api_error typ -> typ
  | _ -> Unknown
(** Decapsulate server_error_type *)

(** {1 Argument types} *)

(** Entry type *)
type entry_type = PACK | LIB | MOD | META | SRC

(** Element type *)
type element_type =
  | VAL
  | TYPE
  | CLASS

type pattern = string
(** Search pattern *)

type entry_info = {
  mutable entry : entry_type;
  mutable last_id: int;
  mutable starts_with : string;
  mutable pattern: pattern;
}
(** Entry info, that is used to customise entry search. *)

(** Condition attached to an OCaml element *)
type condition =
  | In_opam of pattern
  | In_mdl of pattern * pattern (* mdl name, opam_name *)

(** Search mode that determines how to handle correctly an element pattern *)
type search_mode =
  | Regex
  | Text

type element_info = {
  element : element_type;
  last_id : int;
  pattern : pattern;
  mode : search_mode;
  conditions : condition list
}
(** Element info, that is used to customise element search. *)

(* Type that regroups [entry_info] and [element_info].
   Used by services that handle both : entries and elements. *)
type info =
  | Entry of entry_info
  | Element of element_info

(** All commands that could be executed with service [exec_command]. *)
type command = Count

type file_type = ML | DUNE | MAKEFILE

type sources_search_info = {
  pattern : pattern;
  files: file_type;
  is_regex : bool;
  is_case_sensitive : bool;
  last_match_id : int;
}
(** Sources search info, that is used to customise element search through sources DB. *)
