open! Base
open! Ppxlib
open! Ast_builder.Default

let module_path_override_pattern : (_, expression option -> _, _) Ast_pattern.t =
  Ast_pattern.(
    map
      (pstr_value
         nonrecursive
         (value_binding ~pat:(ppat_var (string "unique_module_path")) ~expr:__ ^:: nil)
       ^:: nil)
      ~f:(fun f expr -> f (Some expr)))
;;

let dotnet_bin_prot_extension ~name ~parse =
  Extension.declare
    name
    Extension.Context.structure_item
    parse
    (fun ~loc ~path rec_flag tds module_path_override ->
       let loc = { loc with loc_ghost = true } in
       let ensure_payload_type_checks =
         let module_binding =
           let ignore_unused_type =
             let unused_type_declaration_payload =
               let expr = estring ~loc "-34" in
               PStr [ pstr_eval ~loc expr [] ]
             in
             pstr_attribute
               ~loc
               (attribute
                  ~loc
                  ~name:(Loc.make ~loc "ocaml.warning")
                  ~payload:unused_type_declaration_payload)
           in
           let expr =
             pmod_structure ~loc [ ignore_unused_type; pstr_type ~loc rec_flag tds ]
           in
           module_binding ~loc ~name:(Loc.make ~loc None) ~expr
         in
         pstr_module ~loc module_binding
       in
       let dotnet_value =
         Ppx_dotnet_bin_prot_expander.generate_dotnet_bin_prot_value
           ~loc
           ~path
           ~rec_flag
           ~tds
           ~module_path_override
       in
       let module_expression =
         pmod_structure ~loc [ ensure_payload_type_checks; dotnet_value ]
       in
       pstr_include ~loc (include_infos ~loc module_expression))
;;

let dotnet_bin_prot_alias =
  dotnet_bin_prot_extension
    ~name:"dotnet_bin_prot.alias"
    ~parse:Ast_pattern.(pstr (pstr_type __ __ ^:: map nil ~f:(fun f -> f None)))
;;

let dotnet_bin_prot_functor =
  dotnet_bin_prot_extension
    ~name:"dotnet_bin_prot.functor"
    ~parse:Ast_pattern.(pstr (pstr_type __ __ ^:: module_path_override_pattern))
;;

let () =
  Driver.register_transformation
    ~extensions:[ dotnet_bin_prot_alias; dotnet_bin_prot_functor ]
    "dotnet_bin_prot"
;;
