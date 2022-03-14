open Data_types

(** Module that defines auxillary functions to :
    - Catches DB errors and converts it to [Data_types.Search_api_error].
    - Converts DB rows to a specific OCaml data structure. *)

let catch_db_error f =
    Lwt.catch f
    @@ (fun err ->
        Printf.eprintf "catch_db_error\n"; flush stderr;
        match err with
        | Db_lwt.PGOCaml.PostgreSQL_Error _ -> (Printf.eprintf "PostgreSQL_Error\n"; flush stderr;
        Lwt.fail @@ search_api_error Invalid_regex)
        | exn -> Printf.eprintf "Another error : %s\n" (Printexc.to_string exn);
            flush stderr;Lwt.fail @@ search_api_error Unknown)
(** Catches DB excetpions and raises [Data_types.Search_api_error] coresponding to DB error *)

let path_of_opam opam_name opam_version =
    Printf.sprintf "docs/OPAM.%s.%s/index.html" opam_name opam_version
(** Computes path to package documentation *)

let name_of_opam opam_name opam_version =
    Printf.sprintf "%s.%s" opam_name opam_version
(** Computes name of package *)

let path_of_lib lib_name opam_name opam_version =
  Printf.sprintf "docs/LIBRARY.%s@%s.%s/index.html"
    lib_name opam_name opam_version
(** Computes path to library documentation *)

let path_of_meta meta_name opam_name opam_version =
  Printf.sprintf "docs/META.%s@%s.%s/index.html"
    meta_name opam_name opam_version
(** Computes path to meta documentation *)

let path_of_src opam_name opam_version =
    Printf.sprintf "sources/%s.%s/index.html"
        opam_name opam_version
(** Computes path to source explorer for a package *)

let path_of_src_dir opam_name opam_version =
    Printf.sprintf "sources/%s.%s"
        opam_name opam_version
(** Computes path to source directory for a package *)

let packages_of_rows rows =
    List.map (function row ->
        let path = path_of_opam row#opam_name row#opam_version in
        {name = row#opam_name;
         path;
         version = row#opam_version;
         synopsis = row#opam_synopsis}
    ) rows
(** Creates [Data_types.packages] from DB rows *)

let library_of_row row opam_row : lib_entry =
    let opam_row = List.hd opam_row in
    let path = path_of_lib row#lib_name opam_row#opam_name opam_row#opam_version
    and opampath = path_of_opam opam_row#opam_name opam_row#opam_version
    and opam = name_of_opam opam_row#opam_name opam_row#opam_version
    in
        {name = row#lib_name;
        path;
        opam;
        opampath}
(** Creates [Data_types.lib_entry] from library and its package DB rows *)

let meta_of_row row opam_row : meta_entry =
    let opam_row = List.hd opam_row in
    let path = path_of_meta row#meta_name opam_row#opam_name opam_row#opam_version
    and opampath = path_of_opam opam_row#opam_name opam_row#opam_version
    and opam = name_of_opam opam_row#opam_name opam_row#opam_version
    in
        {namemeta = row#meta_name;
        path;
        opam;
        opampath}
(** Creates [Data_types.meta_entry] from meta and its package DB rows *)

let module_of_row row opam_row lib_rows =
    let opam_row = List.hd opam_row in
    let opampath = path_of_opam opam_row#opam_name opam_row#opam_version
    and opam = name_of_opam opam_row#opam_name opam_row#opam_version
    and libs =
        List.map
            (fun row ->
                let lib_path = path_of_lib row#mdl_lib opam_row#opam_name opam_row#opam_version in
                (row#mdl_lib, lib_path))
            lib_rows
    in
        {path=row#mdl_path;
        name=row#mdl_name;
        opampath;
        opam;
        libs}
(** Creates [Data_types.module_entry] from module, its package and its libraries DB rows *)

let sources_of_rows rows : sources =
    List.map (function row ->
        let namesrc = row#opam_name
        and path = path_of_src row#opam_name row#opam_version
        and opam = name_of_opam row#opam_name row#opam_version
        and opampath = path_of_opam row#opam_name row#opam_version in
        {
            namesrc;
            path;
            opam;
            opampath
        }
    ) rows
(** Creates [Data_types.sources] from DB rows *)

let val_of_row row opam_row mdl_row =
    let opam_row = List.hd opam_row
    and mdl_row = List.hd mdl_row in
    let opampath = path_of_opam opam_row#opam_name opam_row#opam_version
    and opam = name_of_opam opam_row#opam_name opam_row#opam_version
    and mdlpath = mdl_row#mdl_path
    and mdl = mdl_row#mdl_name in
    {
        ident = row#mdl_ident;
        value = row#mdl_val;
        mdl;
        mdlpath;
        opam;
        opampath
    }
(** Creates [Data_types.val_element] from DB rows. *)

let type_of_row_opt modules row opam_row mdl_row : Data_types.type_element option =
  let opam_row = List.hd opam_row in
  let mdl_row = List.hd mdl_row in
  let opampath = path_of_opam opam_row#opam_name opam_row#opam_version in
  let opam = name_of_opam opam_row#opam_name opam_row#opam_version in
  let mdlpath = mdl_row#mdl_path in
  let mdl = mdl_row#mdl_name in
    (* If val resepcts one of opam and mdl condition *)
  if modules = []
       || List.exists
            (fun (mdl_name,opam_name) -> mdl_name = mdl && opam_name = opam_row#opam_name)
            modules
    then Some {
                ident = row#ident;
                mdl;
                mdlpath;
                opam;
                opampath
            }
    else None
(** Creates [Data_types.type_element] from DB rows. *)

let class_of_row_opt modules row opam_row mdl_row : Data_types.class_element option =
  let opam_row = List.hd opam_row in
  let mdl_row = List.hd mdl_row in
  let opampath = path_of_opam opam_row#opam_name opam_row#opam_version in
  let opam = name_of_opam opam_row#opam_name opam_row#opam_version in
  let mdlpath = mdl_row#mdl_path in
  let mdl = mdl_row#mdl_name in
    (* If val resepcts one of opam and mdl condition *)
  if modules = []
       || List.exists
            (fun (mdl_name,opam_name) -> mdl_name = mdl && opam_name = opam_row#opam_name)
            modules
    then Some {
                ident = row#ident;
                mdl;
                mdlpath;
                isclasstype = if row#is_class_type then 1 else 0;
                opam;
                opampath
            }
    else None
(** Creates [Data_types.class_element] from DB rows. *)

let count_from_row = function [ Some v ] -> Int64.to_int v | _ -> 0
(** Extracts result of command 'count' from DB row *)

let src_from_opam_row row =
    let row = List.hd row in
    let opam = name_of_opam row#opam_name row#opam_version
    and path = path_of_src_dir row#opam_name row#opam_version in
    (opam,path)
(** Returns opam name and path to the directory for sources from opam row. *)

let mdl_in_opams row opams =
    List.find_opt (String.equal row#mdl_opam) opams
    |> Option.map (fun opam -> row#mdl_name, opam)
(** Returns module with its package name if it is attached to existing one from [packs]. Otherwise returns None. *)
