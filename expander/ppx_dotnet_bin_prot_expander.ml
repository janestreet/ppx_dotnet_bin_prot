open! Base
open! Ppxlib
open! Ast_builder.Default

let get_all_idents ~loc td =
  let not_supported_yet x =
    Location.raise_errorf ~loc "dotnet_bin_prot doesn't support %s" x
  in
  let ident_folder =
    object (self)
      inherit [Longident.t Loc.t list] Ast_traverse.fold

      method! core_type_desc desc acc =
        match desc with
        | Ptyp_var (_ : string) -> acc
        | Ptyp_tuple types -> self#list self#core_type types acc
        | Ptyp_constr (ident, types) -> ident :: self#list self#core_type types acc
        | Ptyp_variant (row_fields, (_ : closed_flag), (_ : label list option)) ->
          self#list self#row_field row_fields acc
        | _ -> not_supported_yet "this [core_type_desc]"

      method! row_field_desc desc acc =
        match desc with
        | Rinherit (_ : core_type) ->
          not_supported_yet "polymorphic variants with inheritance"
        | Rtag ((_ : label loc), empty_constructor, constructor_types) ->
          (match empty_constructor, constructor_types with
           | true, [] -> acc
           | false, [ core_type ] -> self#core_type core_type acc
           | (_ : bool), (_ : core_type list) ->
             not_supported_yet
               "polymorphic variants with several types used for the same constructor")

      method! type_declaration td acc =
        match td.ptype_kind with
        | Ptype_record lds -> self#list self#label_declaration lds acc
        | Ptype_variant cds -> self#list self#constructor_declaration cds acc
        | Ptype_abstract ->
          (match td.ptype_manifest with
           | None -> not_supported_yet "abstract types without type equality"
           | Some core_type ->
             (* We only consider the manifest a dependency if there is no type
                declaration. Otherwise we could end up depending on types that don't
                derive [bin_io]/[dotnet_bin_prot]. *)
             self#core_type core_type acc)
        | Ptype_open -> not_supported_yet "open types"
    end
  in
  ident_folder#type_declaration td []
  |> List.dedup_and_sort ~compare:(Comparable.lift Longident.compare ~f:Loc.txt)
;;

let with_prefix s = "dotnet_bin_prot_" ^ s

let generate_dotnet_bin_prot_value ~loc ~path ~rec_flag ~tds ~module_path_override =
  let loc = { loc with loc_ghost = true } in
  let td_names =
    List.map tds ~f:(fun td -> td.ptype_name.txt) |> Set.of_list (module String)
  in
  let module_path =
    match module_path_override with
    | Some module_path_override ->
      [%expr Core.String.split [%e module_path_override] ~on:'.']
    | None ->
      (match String.split path ~on:'.' with
       | [] -> raise_s [%message "path should never be empty" path]
       | [ file ] -> raise_s [%message "file doesn't have an extension" file]
       | (_ : string) :: (_ : string) :: modules ->
         List.map modules ~f:(estring ~loc) |> elist ~loc)
  in
  let bindings =
    List.map tds ~f:(fun td ->
      let idents = get_all_idents ~loc td in
      let dependencies_with_idents =
        List.filter_map idents ~f:(fun ident ->
          let type_name = Longident.name ident.txt in
          let%map.Option () =
            match Set.mem td_names type_name, rec_flag with
            | true, Recursive ->
              None
            | true, Nonrecursive | false, (Nonrecursive | Recursive) -> Some ()
          in
          let dependency = unapplied_type_constr_conv ~loc ident ~f:with_prefix in
          pexp_tuple ~loc [ estring ~loc type_name; dependency ])
        |> elist ~loc
      in
      let type_declaration =
        let structure = [ pstr_type ~loc Recursive [ td ] ] in
        estring ~loc (Pprintast.string_of_structure structure)
      in
      let expr =
        [%expr
          let source =
            Dotnet_bin_prot.Source.Derived
              { declaration_source_code = [%e type_declaration]
              ; module_path = [%e module_path]
              }
          in
          { Dotnet_bin_prot.source
          ; type_name = [%e estring ~loc td.ptype_name.txt]
          ; dependencies_with_idents = [%e dependencies_with_idents]
          ; source_code_position = [%here]
          }]
      in
      value_binding
        ~loc
        ~pat:(ppat_var ~loc (Loc.make (with_prefix td.ptype_name.txt) ~loc))
        ~expr)
  in
  pstr_value ~loc Nonrecursive bindings
;;

let generate_dotnet_bin_prot_signature ~loc ~tds =
  List.map tds ~f:(fun td ->
    let descr =
      let name = Loc.make (with_prefix td.ptype_name.txt) ~loc in
      let type_ =
        let ident = Loc.make (Ldot (Lident "Dotnet_bin_prot", "t")) ~loc in
        ptyp_constr ~loc ident []
      in
      value_description ~loc ~name ~type_ ~prim:[]
    in
    psig_value ~loc descr)
;;
