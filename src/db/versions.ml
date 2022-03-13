(** Module that stores and updates/downgrades differrent versions of DB.
    The verions should correspond for every digodoc's API. *)

let cver = ref 0
let upgrades : (int * (unit PGOCaml.t -> int -> unit)) list ref = ref []
let downgrades: (int * string list) list ref = ref []

let register_version ?version
    ?(before_upgrade = fun _ -> ())
    ?(after_upgrade = fun _ -> ())
    ~downgrade
    ~upgrade () =
  let previous_version = !cver in
  let version = match version with
    | None -> !cver + 1
    | Some v ->
      if v <= !cver then
        Format.ksprintf failwith "Registering version %d forbidden (min %d)"
          v (!cver + 1);
      v in
  let upgrade dbh version =
    before_upgrade dbh;
    EzPG.upgrade ~dbh ~version ~downgrade upgrade;
    after_upgrade dbh;
  in
  cver := version;
  upgrades := !upgrades @ [previous_version, upgrade];
  downgrades := (version, downgrade) :: !downgrades;
  ()
;;

let init () =
  register_version ()
    (* list of instructions to execute to upgrade DB *)
    ~upgrade:[
      {|CREATE TABLE opam_index(
        opam_name VARCHAR PRIMARY KEY,
        opam_version VARCHAR NOT NULL,
        opam_synopsis VARCHAR NOT NULL
      )|};
      {|CREATE TABLE library_index(
        lib_id INT PRIMARY KEY,
        lib_name VARCHAR NOT NULL,
        lib_opam VARCHAR NOT NULL REFERENCES opam_index(opam_name)
      )|};
      {|CREATE TABLE meta_index(
        meta_name VARCHAR PRIMARY KEY,
        meta_opam VARCHAR NOT NULL REFERENCES opam_index(opam_name)
      )|};
      {|CREATE TABLE module_index(
        mdl_id INT PRIMARY KEY,
        mdl_name VARCHAR NOT NULL,
        mdl_path VARCHAR NOT NULL,
        mdl_opam VARCHAR NOT NULL REFERENCES opam_index(opam_name)
      )|};
      {|CREATE TABLE module_libraries(
        mdl_id INT NOT NULL REFERENCES module_index(mdl_id),
        mdl_lib_id INT NOT NULL REFERENCES library_index(lib_id),
        mdl_lib VARCHAR NOT NULL,
        PRIMARY KEY (mdl_id, mdl_lib_id)
      )|};
      {|CREATE TABLE module_vals(
        mdl_id INT NOT NULL REFERENCES module_index(mdl_id),
        mdl_name VARCHAR NOT NULL,
        mdl_opam_name VARCHAR NOT NULL REFERENCES opam_index(opam_name),
        mdl_ident VARCHAR NOT NULL,
        mdl_val VARCHAR NOT NULL,
        PRIMARY KEY (mdl_id, mdl_ident)
      )|};
      {|CREATE TABLE module_types(
        mdl_id INT NOT NULL REFERENCES module_index(mdl_id),
        mdl_name VARCHAR NOT NULL,
        mdl_opam_name VARCHAR NOT NULL REFERENCES opam_index(opam_name),
        type_id INT NOT NULL PRIMARY KEY,
        ident VARCHAR NOT NULL
      )|};
      {|CREATE TABLE module_classes(
        mdl_id INT NOT NULL REFERENCES module_index(mdl_id),
        mdl_name VARCHAR NOT NULL,
        mdl_opam_name VARCHAR NOT NULL REFERENCES opam_index(opam_name),
        is_class_type BOOLEAN NOT NULL,
        type_id INT NOT NULL PRIMARY KEY,
        ident VARCHAR NOT NULL
      )|};
      {|CREATE TABLE type_signatures(
        type_id INT NOT NULL REFERENCES module_types(type_id),
        constructor VARCHAR NOT NULL,
        PRIMARY KEY (type_id, constructor)
      )
    |};
      {|CREATE TABLE class_signatures(
        type_id INT NOT NULL REFERENCES module_classes(type_id),
        constructor VARCHAR NOT NULL,
        PRIMARY KEY (type_id, constructor)
      )
    |}
  ]
    (* list of instructions to execute to downgrade DB *)
    ~downgrade:[
      {|DROP TABLE opam_index CASCADE|};
      {|DROP TABLE library_index CASCADE|};
      {|DROP TABLE meta_index CASCADE|};
      {|DROP TABLE module_index CASCADE|};
      {|DROP TABLE module_libraries CASCADE|};
      {|DROP TABLE module_vals CASCADE|};
      {|DROP TABLE module_types CASCADE|};
      {|DROP TABLE module_classes CASCADE|};
      {|DROP TABLE type_signatures CASCADE|};
      {|DROP TABLE class_signatures CASCADE|}
    ]
;;

let () = init ()
