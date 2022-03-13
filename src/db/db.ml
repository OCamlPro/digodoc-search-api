open Lwt.Infix
open Misc_db
open Db_lwt
open Data_types

(** Module [Db] regroups all the DB requests divided into module for every handler.
    Request are sended to PostgreSQL database. Every function that is supposed to be an handler's entry point
    catches PostgreSQL errors with [Misc_db.catch_db_error]. Functions use ```pgocaml_ppx``` that defines special syntax
    for SQL requests. *)

let get_opam_by_name dbh opam_name =
  [%pgsql.object dbh "select opam_name,opam_version
                      from opam_index
                      where opam_name = $opam_name"]
(** Get package row from DB that has specified name. *)

let get_mdl_by_id dbh mdl_id =
  [%pgsql.object dbh "select *
                      from module_index
                      where mdl_id=$mdl_id"]
(** Get module row from DB that has specified id. *)

let get_libs_by_mdl_id dbh mdl_id =
  [%pgsql.object dbh "select mdl_lib
                      from module_libraries
                      where mdl_id=$mdl_id"]
(* Get name of libraries to which module with specified id is included. *)

let get_mdls_by_name dbh mdl =
  [%pgsql.object dbh "select mdl_name,mdl_opam
                      from module_index
                      where mdl_name ~* $mdl"]
(** Get module rows by module name *)

let regexs_from_modules modules =
  let modules_regex =
    if modules = []
    then ""
    else "^(" ^
         String.concat "|"
          (List.map fst modules)
         ^ ")$" in
  let opam_regex =
    if modules = []
    then ""
    else "^(" ^
         String.concat "|"
          (List.map snd modules)
         ^ ")$" in
  (modules_regex,opam_regex)
(** Returns module's and opam's regex expression for specified conditions *)

module Entries = struct

  let get_packages {last_id; starts_with; pattern; _ } =
    with_dbh >>> fun dbh -> catch_db_error @@
      fun () ->
        [%pgsql.object dbh "select *
                        from
                          (select *, ROW_NUMBER () OVER (ORDER BY UPPER(opam_name)) as row_id
                          from opam_index
                          where opam_name ~* $pattern and opam_name ~* $starts_with) result
                        where result.row_id>${Int64.of_int last_id}
                        and result.row_id < ${Int64.add (Int64.of_int last_id) (Int64.of_int 50)}"]
        >|= packages_of_rows
  (** Get 50 first packages starting with index [last_id + 1] in DB rows that are obtained by following conditions :
      - Package should match [starts_with] regex expression, that indicates the first letter of the name.
      - Package should match [pattern] regex expression, that describes case insensetive substring of the name. *)

  let get_libraries {last_id; starts_with; pattern; _ } =
    with_dbh >>> fun dbh ->  catch_db_error @@
      fun () ->
        let%lwt rows = [%pgsql.object dbh "select *
                        from
                          (select *, ROW_NUMBER () OVER (ORDER BY UPPER(lib_name)) as row_id
                          from library_index
                          where lib_name ~* $pattern and lib_name ~* $starts_with) result
                        where result.row_id>${Int64.of_int last_id}
                        and result.row_id < ${Int64.add (Int64.of_int last_id) (Int64.of_int 50)}"]
        in
          Lwt_list.map_s
            (fun row ->
              let opam_name = row#lib_opam in
              let%lwt opam_row = get_opam_by_name dbh opam_name in
                Lwt.return (library_of_row row opam_row))
            rows
  (** Get 50 first libraries starting with index [last_id + 1] in DB rows that are obtained by following conditions :
      - Library should match [starts_with] regex expression, that indicates the first letter of the name.
      - Library should match [pattern] regex expression, that describes case insensetive substring of the name. *)

  let get_metas {last_id; starts_with; pattern; _ } =
    with_dbh >>> fun dbh -> catch_db_error @@
      fun () ->
        let%lwt rows = [%pgsql.object dbh "select *
                        from
                          (select *, ROW_NUMBER () OVER (ORDER BY UPPER(meta_name)) as row_id
                          from meta_index
                          where meta_name ~* $pattern and meta_name ~* $starts_with) result
                        where result.row_id>${Int64.of_int last_id}
                        and result.row_id < ${Int64.add (Int64.of_int last_id) (Int64.of_int 50)}"]
        in
          Lwt_list.map_s
            (fun row ->
              let opam_name = row#meta_opam in
              let%lwt opam_row = get_opam_by_name dbh opam_name in
                Lwt.return (meta_of_row row opam_row))
            rows
  (** Get 50 first metas starting with index [last_id + 1] in DB rows that are obtained by following conditions :
      - Meta should match [starts_with] regex expression, that indicates the first letter of the name.
      - Meta should match [pattern] regex expression, that describes case insensetive substring of the name. *)

  let get_modules {last_id; starts_with; pattern; _ } =
    with_dbh >>> fun dbh -> catch_db_error @@
      fun () ->
        let%lwt rows = [%pgsql.object dbh "select *
                              from
                                (select *, ROW_NUMBER () OVER (ORDER BY UPPER(mdl_name)) as row_id
                                 from module_index
                                 where mdl_name ~* $pattern and mdl_name ~* $starts_with) result
                              where result.row_id>${Int64.of_int last_id}
                              and result.row_id < ${Int64.add (Int64.of_int last_id) (Int64.of_int 50)}"]
        in
          Lwt_list.map_s
            (fun row ->
              let mdl_id = row#mdl_id in
              let opam_name = row#mdl_opam in
              let%lwt opam_row = get_opam_by_name dbh opam_name in
              let%lwt lib_rows = get_libs_by_mdl_id dbh mdl_id in
              Lwt.return (module_of_row row opam_row lib_rows))
            rows
  (** Get 50 first modules starting with index [last_id + 1] in DB rows that are obtained by following conditions :
      - Module should match [starts_with] regex expression, that indicates the first letter of the name.
      - Module should match [pattern] regex expression, that describes case insensetive substring of the name. *)

  let get_sources {last_id; starts_with; pattern; _ } =
    with_dbh >>> fun dbh -> catch_db_error @@
      fun () ->
        [%pgsql.object dbh "select *
                        from
                          (select *, ROW_NUMBER () OVER (ORDER BY UPPER(opam_name)) as row_id
                          from opam_index
                          where opam_name ~* $pattern and opam_name ~* $starts_with) result
                        where result.row_id>${Int64.of_int last_id}
                        and result.row_id < ${Int64.add (Int64.of_int last_id) (Int64.of_int 50)}"]
        >|= sources_of_rows
  (** Get 50 first sources starting with index [last_id + 1] in DB rows that are obtained by following conditions :
      - Source should match [starts_with] regex expression, that indicates the first letter of the name.
      - Source should match [pattern] regex expression, that describes case insensetive substring of the name. *)

  let get_modules_from_packages mdl_name opam_list : (string * string) list Lwt.t =
     with_dbh >>> fun dbh -> catch_db_error @@
      fun () ->
        let%lwt rows = get_mdls_by_name dbh mdl_name in
          Lwt_list.filter_map_s
          (fun mdl -> Lwt.return @@ mdl_in_opams mdl opam_list)
          rows
  (** Get all modules that has attached package inside [opam_list] with its package name. *)
        end
(** Module that regroups all DB requests for [Handlers.entries] handler. *)

module Elements = struct

  let get_modules_from_conditions {conditions; _ } =
    with_dbh >>> fun dbh -> catch_db_error @@
      fun () ->
        let packs = List.filter_map (function In_opam opam -> Some opam | _ -> None) conditions
        and mdls = List.filter_map (function In_mdl (mdl,opam) -> Some (mdl,opam) | _ -> None) conditions in
        if mdls = [] then
          let%lwt mdls = Entries.get_modules_from_packages "" packs in
          Lwt.return mdls
        else if packs <> [] then
          let%lwt mdls1 = Entries.get_modules_from_packages "" packs in
          Lwt.return @@ mdls @ mdls1
        else
          Lwt.return mdls
  (** Returns list of modules with their opam names from list of conditions. If module condition is empty,
      then returns all the modules inside packages specified in condition. *)

  let get_vals modules {last_id; pattern; _} =
    with_dbh >>> fun dbh -> catch_db_error @@
    fun () ->
      let modules_regex,opam_regex = regexs_from_modules modules in
      let%lwt rows = [%pgsql.object dbh "select *
                        from
                          (select *, ROW_NUMBER () OVER (ORDER BY UPPER(mdl_ident)) as row_id
                          from module_vals
                          where mdl_ident ~* $pattern and mdl_name ~* $modules_regex and mdl_opam_name ~* $opam_regex) result
                        where result.row_id>${Int64.of_int last_id}
                        and result.row_id < ${Int64.add (Int64.of_int last_id) (Int64.of_int 50)}"]
      in
        Lwt_list.map_s
          (fun row ->
            let opam_name = row#mdl_opam_name in
            let%lwt opam_row = get_opam_by_name dbh opam_name in
            let%lwt mdl_row = get_mdl_by_id dbh row#mdl_id in
            Lwt.return (val_of_row row opam_row mdl_row)
          )
          rows
  (** Get 50 first ocaml values starting with index [last_id + 1] in DB rows that are obtained by following conditions :
      - Value should be defined in one of the module from [modules] list.
      - Value should match [pattern] regex expression, that describes case insensetive substring of the name. *)

  let get_types modules {last_id; pattern; _} : Data_types.types Lwt.t =
    with_dbh >>> fun dbh -> catch_db_error @@
    fun () ->
      let modules_regex,opam_regex = regexs_from_modules modules in
      let%lwt rows = [%pgsql.object dbh
      "SELECT
       *
       FROM
            (
              SELECT *,
              ROW_NUMBER () OVER (
                                  ORDER BY UPPER(ident)
                                 ) AS row_id
              FROM
                  (
                    SELECT DISTINCT ident, mdl_opam_name, mdl_name, mdl_id
                    FROM
                        (
                          SELECT
                          *, ident AS mdl_ident
                          FROM
                              module_types
                          INNER JOIN
                              type_signatures
                          USING (type_id)
                        ) AS temp
                    WHERE
                    (
                      temp.mdl_ident ~* $pattern
                      OR
                      temp.constructor ~* $pattern)
                      AND
                      mdl_name ~* $modules_regex
                      AND
                      mdl_opam_name ~* $opam_regex
                  ) sub2
            ) result
       WHERE
       result.row_id > ${Int64.of_int last_id}
       AND
       result.row_id < ${Int64.add (Int64.of_int last_id) (Int64.of_int 50)}"]

      in
        Lwt_list.filter_map_s
          (fun row ->
            let opam_name = row#mdl_opam_name in
            let%lwt opam_row = get_opam_by_name dbh opam_name in
            let%lwt mdl_row = get_mdl_by_id dbh row#mdl_id in
            Lwt.return (type_of_row_opt modules row opam_row mdl_row)
          )
          rows
  (** Get 50 first ocaml types starting with index [last_id + 1] in DB rows that are obtained by following conditions :
      - Types should be defined in one of the module from [modules] list.
      - Type identiers or "constructors" should match [pattern] regex expression, that describe case insensetive substring of the name. *)

  let get_classes modules {last_id; pattern; _} : Data_types.classes Lwt.t =
    with_dbh >>> fun dbh -> catch_db_error @@
    fun () ->
      let modules_regex,opam_regex = regexs_from_modules modules in
      let%lwt rows = [%pgsql.object dbh
      "SELECT
       *
       FROM
            (
              SELECT *,
              ROW_NUMBER () OVER (
                                  ORDER BY UPPER(ident)
                                 ) AS row_id
              FROM
                  (
                    SELECT DISTINCT ident, mdl_opam_name, is_class_type, mdl_name, mdl_id
                    FROM
                        (
                          SELECT
                          *, ident AS mdl_ident
                          FROM
                              module_classes
                          INNER JOIN
                              class_signatures
                          USING (type_id)
                        ) AS temp
                    WHERE
                    (
                      temp.mdl_ident ~* $pattern
                      OR
                      temp.constructor ~* $pattern)
                      AND
                      mdl_name ~* $modules_regex
                      AND
                      mdl_opam_name ~* $opam_regex
                  ) sub2
            ) result
       WHERE
       result.row_id > ${Int64.of_int last_id}
       AND
       result.row_id < ${Int64.add (Int64.of_int last_id) (Int64.of_int 50)}"]

      in
        Lwt_list.filter_map_s
          (fun row ->
            let opam_name = row#mdl_opam_name in
            let%lwt opam_row = get_opam_by_name dbh opam_name in
            let%lwt mdl_row = get_mdl_by_id dbh row#mdl_id in
            Lwt.return (class_of_row_opt modules row opam_row mdl_row)
          )
          rows
end
(** Module that regroups all DB requests for [Handlers.elements] handler. *)

module Commands = struct

  let count_entries {entry; starts_with; pattern; _ } =
    with_dbh >>> fun dbh -> catch_db_error @@
      fun () ->
        begin
          match entry with
          (* count packages *)
          | PACK -> [%pgsql dbh "select count(*) as n
                                      from opam_index
                                      where opam_name ~* $pattern and opam_name ~* $starts_with"]
          (* count libraries *)
          | LIB ->  [%pgsql dbh "select count(*) as n
                                      from library_index
                                      where lib_name ~* $pattern and lib_name ~* $starts_with"]
          (* count metas *)
          | META -> [%pgsql dbh "select count(*) as n
                                      from meta_index
                                      where meta_name ~* $pattern and meta_name ~* $starts_with"]
          (* count modules *)
          | MOD ->  [%pgsql dbh "select count(*) as n
                                      from module_index
                                      where mdl_name ~* $pattern and mdl_name ~* $starts_with"]
          (* count sources *)
          | SRC ->  [%pgsql dbh "select count(*) as n
                                      from opam_index
                                      where opam_name ~* $pattern and opam_name ~* $starts_with"]
        end
        >|= count_from_row
  (** Count number of entries in DB for specific entry type mentionned in [entry] field. It should follow those conditions :
      - Entry name should match [starts_with] regex expression, that indicates the first letter of the name.
      - Entry name should match [pattern] regex expression, that describes case insensetive substring of the name. *)

  let count_elements modules {element; pattern; _ } =
    with_dbh >>> fun dbh -> catch_db_error @@
    fun () ->
      let modules_regex,opam_regex = regexs_from_modules modules in
      begin
        match element with
        | VAL -> [%pgsql dbh "select count(*) as n
                              from module_vals
                              where mdl_ident ~* $pattern
                                    and mdl_name ~* $modules_regex
                                    and mdl_opam_name ~* $opam_regex"] >|= count_from_row
        | TYPE -> [%pgsql dbh
        "SELECT COUNT(*) AS n
         FROM
              (
                SELECT DISTINCT ident, mdl_opam_name, mdl_name
                FROM
                      (
                        SELECT
                        *, ident as mdl_ident
                        FROM
                            module_types
                        INNER JOIN
                            type_signatures
                        USING (type_id)
                      ) AS temp
                WHERE
                (
                  temp.mdl_ident ~* $pattern
                  OR
                  temp.constructor ~* $pattern
                )
                AND
                mdl_name ~* $modules_regex
                AND
                mdl_opam_name ~* $opam_regex
            ) as sub"] >|= count_from_row
        | CLASS -> [%pgsql dbh
        "SELECT COUNT(*) AS n
         FROM
              (
                SELECT DISTINCT ident, mdl_opam_name, mdl_name
                FROM
                      (
                        SELECT
                        *, ident AS mdl_ident
                        FROM
                            module_classes
                        INNER JOIN
                            class_signatures
                        USING (type_id)
                      ) AS temp
                WHERE
                (
                  temp.mdl_ident ~* $pattern
                  OR
                  temp.constructor ~* $pattern
                )
                AND
                mdl_name ~* $modules_regex
                AND
                mdl_opam_name ~* $opam_regex
            ) as sub"] >|= count_from_row
        end
  (** Count number of elements in DB for specific element type mentionned in [element] field. It should follow those conditions :
      - Element name should respect at least 1 package constraint and at least 1 module constaint if they are presented. Constraint
      describes to which entry this element is included.
      - Element name should match [pattern] regex expression, that describes case insensetive substring of the name. *)
end
(** Module that regroups all DB requests for [Handlers.exec_command] handler. *)

module Search = struct

  let search_packages pattern =
    with_dbh >>> fun dbh -> catch_db_error @@
      fun () ->
        [%pgsql.object dbh "select *
                          from opam_index
                          where opam_name ~* $pattern"]
        >|= packages_of_rows
  (** Get all the packages that match specified regex pattern. *)

  let search_libraries pattern =
    with_dbh >>> fun dbh -> catch_db_error @@
      fun () ->
        let%lwt rows = [%pgsql.object dbh "select *
                                from library_index
                                where lib_name ~* $pattern"]
        in
          Lwt_list.map_s
            (fun row ->
              let opam_name = row#lib_opam in
              let%lwt opam_row = get_opam_by_name dbh opam_name in
              Lwt.return (library_of_row row opam_row))
            rows
  (** Get all the libraries that match specified regex pattern. *)

  let search_modules pattern =
    with_dbh >>> fun dbh -> catch_db_error @@
      fun () ->
        let%lwt rows = [%pgsql.object dbh "select *
                                from module_index
                                where mdl_name ~* $pattern"]
        in
          Lwt_list.map_s
            (fun row ->
              let mdl_id = row#mdl_id in
              let opam_name = row#mdl_opam in
              let%lwt opam_row = get_opam_by_name dbh opam_name in
              let%lwt lib_rows = get_libs_by_mdl_id dbh mdl_id in
              Lwt.return (module_of_row row opam_row lib_rows))
            rows
  (** Get all the modules that match specified regex pattern. *)
end
(** Module that regroups all DB requests for [Handlers.search] handler. *)

module Sources_search = struct
  let get_src_info name =
    with_dbh >>> fun dbh -> catch_db_error @@
      fun () ->
        get_opam_by_name dbh name >|= src_from_opam_row
end
