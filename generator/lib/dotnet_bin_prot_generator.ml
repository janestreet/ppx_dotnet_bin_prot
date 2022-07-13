open! Core
open! Async
open! Import
open! Ppxlib
open! Ast_builder.Default

module Source_code_position_and_dotnet_source = struct
  module T = struct
    type t =
      { source_code_position : Source_code_position.t
      ; dotnet_source : Dotnet_bin_prot.Source.t
      }
    [@@deriving compare, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)
end

let to_key (dotnet_bin_prot : Dotnet_bin_prot.t) =
  { Source_code_position_and_dotnet_source.source_code_position =
      dotnet_bin_prot.source_code_position
  ; dotnet_source = dotnet_bin_prot.source
  }
;;

module Override = struct
  type t =
    | Built_in of string
    | Source_file of
        { file_path : string
        ; module_name : string
        ; type_name : string
        }
end

module Fsproj_path : Identifiable.S = String

module Generation_state = struct
  type t =
    { source_code_position_to_fsharp_ident :
        (Fsproj_path.t * label) Source_code_position_and_dotnet_source.Map.t
    ; fsproj_dependencies : Fsproj_path.Set.t Fsproj_path.Map.t
    }

  let empty =
    { source_code_position_to_fsharp_ident =
        Source_code_position_and_dotnet_source.Map.empty
    ; fsproj_dependencies = Fsproj_path.Map.empty
    }
  ;;
end

type 'a t =
  { additional_nuget_config_path : string option
  (** Path to a nuget.config for the generated project to use if it is not built out of
      [lib/dotnet-libs]. [output_dir] must be a descendant relative to the directory
      containing [additional_nuget_config_path] for the dotnet to use the config file. *)
  ; types : 'a list
  ; project_name : string
  ; output_dir : string
  ; additional_jbuild_entry : generated_files:string list -> string option
  ; overrides : (Dotnet_bin_prot.t, Override.t) List.Assoc.t
  }
[@@deriving fields]

let create ?additional_nuget_config_path () = Fields.create ~additional_nuget_config_path

let loc =
  let location_can_be_arbitrary_as_ast_is_output_to_file_as_string = Location.none in
  location_can_be_arbitrary_as_ast_is_output_to_file_as_string
;;

let create_module_binding ~module_name ~module_contents =
  let module_binding =
    module_binding
      ~loc
      ~name:(Loc.make ~loc (Some module_name))
      ~expr:(pmod_structure ~loc module_contents)
  in
  pstr_module ~loc module_binding
;;

(* F# doesn't support anonymous records and polymorphic variants, so we create separate
   modules which contain a type that will represent the corresponding F# representation.

   Anonymous records are converted to a regular record. The serialization of anonymous
   records and regular records is the same, so nothing else has to be done.

   Polymorphic variants will be converted into regular variants. The serializers are a bit
   different between the two, so we need to generate the serializers first. Only then can
   we convert the polymorphic variant into a regular one.
*)

let generate_type_declaration
      ~additional_modules
      ~kind
      ~manifest
      ~params
      ~parent_type_name
  =
  let type_name = "t" in
  let type_declaration =
    type_declaration
      ~loc
      ~name:(Loc.make ~loc type_name)
      ~kind
      ~cstrs:[]
      ~params:(List.map params ~f:(Fn.flip Tuple2.create (NoVariance, NoInjectivity)))
      ~private_:Public
      ~manifest
  in
  let module_contents =
    let f_sharp_does_not_support_nonrec = Recursive in
    [ pstr_type ~loc f_sharp_does_not_support_nonrec [ type_declaration ] ]
  in
  (* There may be multiple types in the same namespace, so we tag the [Generated] modules
     with the parent type-name. Since [t] is the most common parent type-name, we omit the
     tagging in this case. *)
  let module_name =
    (match parent_type_name with
     | "t" -> "Generated_"
     | non_t -> String.capitalize non_t ^ "_generated_")
    ^ Int.to_string (Queue.length additional_modules)
  in
  Queue.enqueue additional_modules (create_module_binding ~module_name ~module_contents);
  Loc.make ~loc (Longident.parse (module_name ^ "." ^ type_name))
;;

let chop_ident ident ~namespace_parts =
  List.fold_until
    namespace_parts
    ~init:ident
    ~f:(fun ident part ->
      match String.chop_prefix ident ~prefix:(part ^ ".") with
      | None -> Stop ident
      | Some chopped_ident -> Continue chopped_ident)
    ~finish:Fn.id
;;

let%expect_test "chop_ident" =
  let ident = "A.B.C.D" in
  let parts = [ "A"; "B" ] in
  print_endline (chop_ident ident ~namespace_parts:parts);
  [%expect {| C.D |}];
  let parts_missing_middle = [ "A"; "C" ] in
  print_endline (chop_ident ident ~namespace_parts:parts_missing_middle);
  [%expect {| B.C.D |}];
  Deferred.unit
;;

let generate_ocaml_ast
      (source_code_position_to_f_sharp_ident :
         label Source_code_position_and_dotnet_source.Map.t)
      ~declaration_source_code
      ~dependencies_with_idents
      ~ast_file
      ~namespace
      ~module_path
      ~type_name
  =
  let additional_modules = Queue.create () in
  let%map ast =
    let%map () = Writer.save ast_file ~contents:declaration_source_code in
    let tool_name = "F# code generator" in
    Ocaml_common.Pparse.parse_implementation ~tool_name ast_file
    |> Ppxlib_ast.Selected_ast.Of_ocaml.copy_structure
  in
  (* F# doesn't support redefining a type with a type equality such as

     type t = some_other_type = | Redefinition

     Since we will be generating F# types for [bin_prot]able types we have to use the
     redefinition as there is no guarantee that [some_other_type] derives bin_io (and this
     case is very common in [Core] when types are redefined from [Base]).

     This means that we might sometimes lose type equality which isn't great. As of
     2021-06-29 this is not a problem in practice so we will revisit it only if it starts
     being problematic. *)
  let maybe_drop_type_equality =
    object
      inherit Ast_traverse.map

      method! type_declaration type_declaration =
        match type_declaration.ptype_manifest with
        | None -> type_declaration
        | Some (_ : core_type) ->
          (match type_declaration.ptype_kind with
           | Ptype_record (_ : label_declaration list)
           | Ptype_variant (_ : constructor_declaration list) ->
             { type_declaration with ptype_manifest = None }
           | Ptype_abstract | Ptype_open -> type_declaration)
    end
  in
  let ast = maybe_drop_type_equality#structure ast in
  let get_f_sharp_ident (ident_loc : Longident.t loc) =
    match
      List.Assoc.find
        dependencies_with_idents
        (Longident.name ident_loc.txt)
        ~equal:String.equal
    with
    | None ->
      (* We don't support mutual recursion as of right now. *)
      let recursive_occurence_of_type_is_not_a_dependency = ident_loc in
      recursive_occurence_of_type_is_not_a_dependency
    | Some (dotnet_bin_prot : Dotnet_bin_prot.t) ->
      let new_ident =
        Map.find_exn source_code_position_to_f_sharp_ident (to_key dotnet_bin_prot)
      in
      let chopped_ident =
        chop_ident new_ident ~namespace_parts:(namespace :: module_path)
      in
      { ident_loc with txt = Longident.parse chopped_ident }
  in
  let generate_type_parameters record_or_poly_variant =
    let var_folder =
      object
        inherit [string list] Ast_traverse.fold as super

        method! core_type_desc desc acc =
          match desc with
          | Ptyp_var v -> v :: acc
          | _ as desc -> super#core_type_desc desc acc
      end
    in
    let vars =
      match record_or_poly_variant with
      | `Record lds -> var_folder#list var_folder#label_declaration lds []
      | `Poly_var row_fields -> var_folder#list var_folder#row_field row_fields []
    in
    List.dedup_and_sort vars ~compare:String.compare |> List.map ~f:(ptyp_var ~loc)
  in
  let ident_mapper =
    object (self)
      inherit Ast_traverse.map as super

      method! constructor_arguments =
        function
        | Pcstr_tuple _ as tuple -> super#constructor_arguments tuple
        | Pcstr_record lds ->
          let lds = self#list self#label_declaration lds in
          let params = generate_type_parameters (`Record lds) in
          let created_module_ident =
            generate_type_declaration
              ~additional_modules
              ~kind:(Ptype_record lds)
              ~manifest:None
              ~params
              ~parent_type_name:type_name
          in
          Pcstr_tuple [ ptyp_constr ~loc created_module_ident params ]

      method! core_type_desc =
        function
        | Ptyp_constr (ident, core_types) ->
          Ptyp_constr (get_f_sharp_ident ident, self#list self#core_type core_types)
        | Ptyp_variant (row_fields, (_ : closed_flag), (_ : label list option)) ->
          let row_fields = self#list self#row_field row_fields in
          let params = generate_type_parameters (`Poly_var row_fields) in
          let created_module_ident =
            generate_type_declaration
              ~additional_modules
              ~kind:Ptype_abstract
              ~manifest:(Some (ptyp_variant ~loc row_fields Closed None))
              ~params
              ~parent_type_name:type_name
          in
          Ptyp_constr (created_module_ident, params)
        | _ as desc -> super#core_type_desc desc
    end
  in
  let mapped_ast = ident_mapper#structure ast in
  Queue.to_list additional_modules @ mapped_ast
;;

(* This is mostly copied out of ppx_bin_prot's [bin_tp_class_td], then specialized for the
   [bin_t] value. In [ppx_bin_prot] the function also supports [bin_shape]s but it's not
   worth it to generalize it here given that right now dotnet bin_prot doesn't have them.
   The generic code isn't worth the obfuscation. *)
let bin_class_t td ~rec_flag =
  let open Ast_builder.Default in
  let rec_flag = really_recursive rec_flag [ td ] in
  let loc = td.ptype_loc in
  let tparam_cnvs =
    List.map td.ptype_params ~f:(fun tp ->
      let name = get_type_param_name tp in
      "bin_" ^ name.txt)
  in
  let mk_pat id = pvar ~loc id in
  let tparam_patts = List.map tparam_cnvs ~f:mk_pat in
  let writer =
    let tparam_exprs =
      List.map td.ptype_params ~f:(fun tp ->
        let name = get_type_param_name tp in
        [%expr
          ([%e evar ~loc:name.loc @@ "bin_" ^ name.txt] : _ Bin_prot.Type_class.t)
          .writer])
    in
    eapply ~loc (evar ~loc @@ "bin_writer_" ^ td.ptype_name.txt) tparam_exprs
  in
  let reader =
    let tparam_exprs =
      List.map td.ptype_params ~f:(fun tp ->
        let name = get_type_param_name tp in
        [%expr
          ([%e evar ~loc:name.loc @@ "bin_" ^ name.txt] : _ Bin_prot.Type_class.t)
          .reader])
    in
    eapply ~loc (evar ~loc @@ "bin_reader_" ^ td.ptype_name.txt) tparam_exprs
  in
  let body =
    [%expr
      { Bin_prot.Type_class.writer = [%e writer]
      ; Bin_prot.Type_class.reader = [%e reader]
      }]
  in
  pstr_value
    ~loc
    rec_flag
    [ value_binding
        ~loc
        ~pat:(pvar ~loc @@ "bin_" ^ td.ptype_name.txt)
        ~expr:(eabstract ~loc tparam_patts body)
    ]
;;

let generate_f_sharp_ast
      (source_code_position_to_f_sharp_ident :
         label Source_code_position_and_dotnet_source.Map.t)
      ~declaration_source_code
      ~dependencies_with_idents
      ~ast_file
      ~filename
      ~namespace
      ~module_path
      ~type_name
  =
  let%map ocaml_ast =
    generate_ocaml_ast
      source_code_position_to_f_sharp_ident
      ~declaration_source_code
      ~dependencies_with_idents
      ~ast_file
      ~namespace
      ~module_path
      ~type_name
  in
  let ast_with_expanded_bin_prot =
    let expander =
      object (self)
        inherit Ast_traverse.map

        method! structure structure =
          let structure = self#list self#structure_item structure in
          List.concat_map structure ~f:(fun structure_item ->
            let additional_structure_items =
              match structure_item.pstr_desc with
              | Pstr_type (rec_flag, tds) ->
                List.concat_map
                  Ppx_bin_prot.For_f_sharp.[ bin_write; bin_read ]
                  ~f:(fun generator -> generator ~loc ~path:filename (rec_flag, tds))
                @ List.map tds ~f:(bin_class_t ~rec_flag)
              | _ -> []
            in
            structure_item :: additional_structure_items)
      end
    in
    expander#structure ocaml_ast
  in
  let ast_without_poly_variant_and_ppx_attributes =
    let remove_ocaml_specific_mapper =
      object (self)
        inherit Ast_traverse.map as super

        method! expression_desc =
          function
          | Pexp_variant (label, expression) ->
            let expression = self#option self#expression expression in
            Pexp_construct (Loc.make ~loc (Lident (String.capitalize label)), expression)
          | _ as desc -> super#expression_desc desc

        method! pattern_desc =
          function
          | Ppat_variant (label, pattern) ->
            let pattern =
              self#option self#pattern pattern |> Option.map ~f:(fun pat -> [], pat)
            in
            Ppat_construct (Loc.make ~loc (Lident (String.capitalize label)), pattern)
          | _ as desc -> super#pattern_desc desc

        method! attributes (_ : attribute list) = []

        method! type_declaration type_declaration =
          let { ptype_name
              ; ptype_params
              ; ptype_cstrs
              ; ptype_kind
              ; ptype_private
              ; ptype_manifest
              ; ptype_attributes
              ; ptype_loc
              }
            =
            super#type_declaration type_declaration
          in
          let ptype_kind, ptype_manifest =
            match ptype_kind, ptype_manifest with
            | Ptype_abstract, Some core_type ->
              (match core_type.ptyp_desc with
               | Ptyp_variant (row_fields, (_ : closed_flag), (_ : label list option)) ->
                 ( Ptype_variant
                     (List.map row_fields ~f:(fun row_field ->
                        match row_field.prf_desc with
                        | Rtag (label, empty, constructors) ->
                          let pcd_args =
                            match empty, constructors with
                            | true, [] -> Pcstr_tuple []
                            | false, [ core_type ] ->
                              (match core_type.ptyp_desc with
                               | Ptyp_tuple core_types -> Pcstr_tuple core_types
                               | _ -> Pcstr_tuple [ core_type ])
                            | _, _ ->
                              failwith
                                "AST in DSL shouldn't contain multiple different \
                                 constructors with the same name"
                          in
                          let pcd_name =
                            { label with txt = String.capitalize label.txt }
                          in
                          let pcd_res =
                            let only_used_in_gadt_syntax = None in
                            only_used_in_gadt_syntax
                          in
                          { pcd_name
                          ; pcd_vars = []
                          ; pcd_args
                          ; pcd_res
                          ; pcd_loc = loc
                          ; pcd_attributes = []
                          }
                        | Rinherit _ ->
                          failwith
                            "AST in DSL shouldn't contain polymorphic variants with \
                             inheritance"))
                 , None )
               | _ -> ptype_kind, ptype_manifest)
            | _, _ -> ptype_kind, ptype_manifest
          in
          { ptype_name
          ; ptype_params
          ; ptype_cstrs
          ; ptype_kind
          ; ptype_private
          ; ptype_manifest
          ; ptype_attributes
          ; ptype_loc
          }
      end
    in
    remove_ocaml_specific_mapper#structure ast_with_expanded_bin_prot
  in
  ast_without_poly_variant_and_ppx_attributes
;;

let formatter = Format.str_formatter
let default_margin = Format.pp_get_margin formatter ()

let write_to_file ?(margin = default_margin) ~write_line structure_item =
  Format.pp_set_margin formatter margin;
  Pprintast.structure_item formatter structure_item;
  let output = Format.flush_str_formatter () in
  write_line output
;;

let printer =
  object (self)
    inherit [string -> unit] Ast_traverse.map_with_context

    method! structure_item write_line structure_item =
      (match structure_item.pstr_desc with
       | Pstr_module
           { pmb_name = { txt = Some module_name; _ }
           ; pmb_expr = { pmod_desc = Pmod_structure structure; _ }
           ; _
           } ->
         write_line [%string "module %{module_name} = struct"];
         let write_line_with_additional_indents line =
           let indent = "  " in
           let new_line =
             String.substr_replace_all (indent ^ line) ~pattern:"\n" ~with_:("\n" ^ indent)
           in
           write_line new_line
         in
         ignore (self#structure write_line_with_additional_indents structure : structure);
         write_line [%string "end"]
       | Pstr_type _ ->
         (* Since F# is whitespace sensitive it was complaining about type definitions being
            incorrect when a variant constructor was split across multiple lines. *)
         let only_insert_newline_when_truly_necessary = 1_000_000 in
         write_to_file
           ~margin:only_insert_newline_when_truly_necessary
           ~write_line
           structure_item
       | _ -> write_to_file ~write_line structure_item);
      structure_item
  end
;;

let wrap_into_module_namespace structure module_name =
  let expr : module_expr =
    { pmod_desc = Pmod_structure structure; pmod_loc = loc; pmod_attributes = [] }
  in
  module_binding ~loc ~name:(Loc.make ~loc (Some module_name)) ~expr
;;

let hash_queue_to_alist hash_queue =
  let rec dequeue () =
    match Hash_queue.dequeue_front_with_key hash_queue with
    | Some (key, value) -> (key, value) :: dequeue ()
    | None -> []
  in
  dequeue ()
;;

(* This module exists to iteratively combine [Ast.structure] for the generated dotnet
   modules. Modules must be inserted in the order that they are defined in the source
   file. *)
module Nested_ast : sig
  type t

  val create : unit -> t
  val insert : t -> ast:Ast.structure -> module_path:label list -> unit
  val collapse : t -> Ast.structure
end = struct
  type t = [ `Ast of Ast.structure | `Submodule of string * t ] Queue.t

  let create () = Queue.create ()

  let find_child t ~module_name =
    match
      Queue.find t ~f:(function
        | `Ast (_ : Ast.structure) -> false
        | `Submodule (child_module_name, (_ : t)) ->
          String.equal module_name child_module_name)
    with
    | Some (`Ast (_ : Ast.structure)) -> raise_s [%message "Impossible"]
    | Some (`Submodule ((_ : string), child)) -> child
    | None ->
      let child = create () in
      Queue.enqueue t (`Submodule (module_name, child));
      child
  ;;

  let rec insert t ~ast ~module_path =
    match module_path with
    | [] -> Queue.enqueue t (`Ast ast)
    | module_name :: module_rest ->
      let child = find_child t ~module_name in
      insert child ~ast ~module_path:module_rest
  ;;

  let rec collapse t =
    Queue.to_list t
    |> List.concat_map ~f:(function
      | `Ast ast -> ast
      | `Submodule (module_name, child) ->
        let collapsed_child = collapse child in
        [ pstr_module ~loc (wrap_into_module_namespace collapsed_child module_name) ])
  ;;
end

module Dotnet_module = struct
  type t =
    | Override of string
    | Ast of Nested_ast.t
end

let remove_file_extension name =
  String.rsplit2 name ~on:'.' |> Option.value_map ~default:name ~f:fst
;;

let namespace_from_source_filename filename ~project_name =
  let namespace_without_project_name =
    filename
    |> remove_file_extension
    |> String.split ~on:'/'
    |> List.map ~f:String.capitalize
    |> String.concat ~sep:"." (* Dot-separate based on directory structure. *)
    |> String.tr ~target:'-' ~replacement:'_'
  in
  [%string "%{project_name}.%{namespace_without_project_name}"]
;;

let%expect_test "namespace_from_source_filename" =
  namespace_from_source_filename "lib/catalog/types/local_path.ml" ~project_name:"Test"
  |> print_endline;
  [%expect {| Test.Lib.Catalog.Types.Local_path |}];
  Deferred.unit
;;

let namespace (t : Dotnet_bin_prot.t) ~project_name =
  let namespace =
    namespace_from_source_filename
      ~project_name
      (Source_code_position.pos_fname t.source_code_position)
  in
  namespace
;;

let generate_single_file
      (t : Dotnet_bin_prot.t)
      (generation_state : Generation_state.t)
      ~declaration_source_code
      ~module_path
      ~project_name
      ~ast_file
      ~dotnet_module_by_namespace
  =
  let open Deferred.Or_error.Let_syntax in
  let namespace = namespace t ~project_name in
  let filename = namespace ^ ".fs" in
  let nested_ast =
    match Hash_queue.lookup dotnet_module_by_namespace namespace with
    | Some (Dotnet_module.Ast nested_ast) -> nested_ast
    | Some (Dotnet_module.Override (_ : string)) ->
      raise_s
        [%sexp
          "Outputted file exists as both override and generated", (filename : string)]
    | None ->
      let nested_ast = Nested_ast.create () in
      Hash_queue.enqueue_back_exn
        dotnet_module_by_namespace
        namespace
        (Dotnet_module.Ast nested_ast);
      nested_ast
  in
  (* Context: [nonrec] is not a thing in F# so any code such as a [Stable.V1.t] which is
     type-equal to [t] will not naively convert to compiling F# code. The straightforward
     way to get around this is to put all type declarations in their own submodule.

     To make the generated code more pleasant to use, we instead make some assumptions
     about the input code that will fail loudly if broken.

     We assume that if an OCaml type definition that generates binprot needs to use
     [nonrec], it is probably of the form [type nonrec t = (an expression involving an
     some other, top-level t)]. So, we only wrap a type in a submodule if it's a [t]
     that's not in a submodule. We also assume that its very unlikely for [nonrec] type
     definitions to involve a non-[t].

     If a case comes up such as an [A.B.t] which depends on an [A.t], we can try to: swap
     [A.B.t] and [A.t], pull [A] out to a separate module, or change [t] to another name.
     I think that this is worth not having to add an extra [T] submodule to every type. *)
  let full_module_path =
    match t.type_name with
    | "t" ->
      (match module_path with
       | [] -> [ "T" ]
       | nonempty_path -> nonempty_path)
    | (_ : string) -> module_path
  in
  let qualified_name =
    namespace
    ^ "."
    ^ (if List.is_empty full_module_path
       then ""
       else String.concat full_module_path ~sep:"." ^ ".")
    ^ t.type_name
  in
  let%map ast =
    generate_f_sharp_ast
      (Map.map generation_state.source_code_position_to_fsharp_ident ~f:snd)
      ~declaration_source_code
      ~dependencies_with_idents:t.dependencies_with_idents
      ~ast_file
      ~filename
      ~namespace
      ~module_path:full_module_path
      ~type_name:t.type_name
    |> Deferred.ok
  in
  Nested_ast.insert nested_ast ~ast ~module_path:full_module_path;
  qualified_name
;;

let namespace_for_override ~project_name ~module_name =
  let namespace = project_name ^ "." ^ module_name in
  namespace
;;

let generate_single_file_override
      ~project_name
      ~module_name
      ~file_path
      ~dotnet_module_by_namespace
      ~type_name
  =
  let namespace = namespace_for_override ~project_name ~module_name in
  let%map.Deferred.Or_error dotnet_module =
    Deferred.Or_error.try_with (fun () ->
      let%map.Deferred contents = Reader.file_contents file_path in
      let contents = [%string "namespace %{project_name}\n"] ^ contents in
      Dotnet_module.Override contents)
  in
  Hash_queue.enqueue_back_exn dotnet_module_by_namespace namespace dotnet_module;
  [%string "%{project_name}.%{module_name}.%{type_name}"]
;;

let rec generate_with_dependencies
          (dotnet_bin_prot : Dotnet_bin_prot.t)
          (generation_state : Generation_state.t)
          ~dotnet_module_by_namespace
          ~project_name
          ~ast_file
          ~overrides
          ~namespace_dependencies
          ~fsproj_path
  =
  let open Deferred.Or_error.Let_syntax in
  match
    Map.find
      generation_state.source_code_position_to_fsharp_ident
      (to_key dotnet_bin_prot)
  with
  | Some (dependency_fsproj_path, (_fsharp_ident : label)) ->
    if Fsproj_path.equal fsproj_path dependency_fsproj_path
    then return generation_state
    else (
      let fsproj_dependencies =
        Map.update generation_state.fsproj_dependencies fsproj_path ~f:(function
          | None -> Fsproj_path.Set.singleton dependency_fsproj_path
          | Some fsproj_dependencies ->
            Set.add fsproj_dependencies dependency_fsproj_path)
      in
      return { generation_state with fsproj_dependencies })
  | None ->
    let namespace_of_dotnet_bin_prot (dotnet_bin_prot : Dotnet_bin_prot.t) =
      match Map.find overrides dotnet_bin_prot.source_code_position with
      | Some (Override.Source_file { module_name; file_path = _; type_name = _ }) ->
        namespace_for_override ~project_name ~module_name
      | None | Some (Override.Built_in (_ : label)) ->
        namespace dotnet_bin_prot ~project_name
    in
    let self_namespace = namespace_of_dotnet_bin_prot dotnet_bin_prot in
    let%bind generation_state =
      List.map dotnet_bin_prot.dependencies_with_idents ~f:snd
      |> Deferred.Or_error.List.fold
           ~init:generation_state
           ~f:(fun generation_state dotnet_bin_prot ->
             let dependency_namespace = namespace_of_dotnet_bin_prot dotnet_bin_prot in
             Queue.enqueue
               namespace_dependencies
               { Topological_sort.Edge.from = dependency_namespace; to_ = self_namespace };
             generate_with_dependencies
               dotnet_bin_prot
               generation_state
               ~project_name
               ~ast_file
               ~dotnet_module_by_namespace
               ~overrides
               ~namespace_dependencies
               ~fsproj_path)
    in
    let%map f_sharp_ident =
      match
        Map.find overrides dotnet_bin_prot.source_code_position, dotnet_bin_prot.source
      with
      | Some override, Override ->
        (match override with
         | Override.Built_in ident -> return ident
         | Source_file { file_path; module_name; type_name } ->
           Queue.enqueue
             namespace_dependencies
             { Topological_sort.Edge.from =
                 namespace_for_override ~project_name ~module_name
             ; to_ = self_namespace
             };
           generate_single_file_override
             ~project_name
             ~module_name
             ~file_path
             ~dotnet_module_by_namespace
             ~type_name)
      | ( Some (_ : Override.t)
        , (Built_in | Derived { declaration_source_code = _; module_path = _ }) ) ->
        Deferred.Or_error.error_s
          [%message
            "Defined override for a type that wasn't declared as overridable"
              (dotnet_bin_prot.source_code_position : Source_code_position.t)]
      | None, Override ->
        Deferred.Or_error.error_s
          [%message
            "Type declared as overridable but no override present"
              (dotnet_bin_prot.source_code_position : Source_code_position.t)]
      | None, Built_in -> return dotnet_bin_prot.type_name
      | None, Derived { declaration_source_code; module_path } ->
        generate_single_file
          dotnet_bin_prot
          generation_state
          ~declaration_source_code
          ~module_path
          ~project_name
          ~ast_file
          ~dotnet_module_by_namespace
    in
    { generation_state with
      source_code_position_to_fsharp_ident =
        Map.set
          generation_state.source_code_position_to_fsharp_ident
          ~key:(to_key dotnet_bin_prot)
          ~data:(fsproj_path, f_sharp_ident)
    }
;;

let generate_fs_proj ~output_dir ~project_name ~generated_files ~fsproj_dependencies =
  let fsproj =
    sprintf
      {|
         <Project Sdk="Microsoft.NET.Sdk">
         <PropertyGroup>
         <TargetFramework>netstandard2.0</TargetFramework>
         <!-- 3370: Ignore deprecation info messages for dereferencing using [!] instead of [.Value] in the generated files since they are generated using OCaml and it will be backward compatible (FS-1111).

              1182: Warn on unused.

              3218: Check that argument names in signatures and implementations match.

              mlcompatibility: Turn on the ml compatibility flag, as this code is generated using ocaml -->
         <OtherFlags>--warnaserror+ --warnon:1182,3218 --nowarn:3370 --mlcompatibility</OtherFlags>
         <TreatWarningsAsErrors>true</TreatWarningsAsErrors>
         <EmbedAllSources>true</EmbedAllSources>
         <DebugType>embedded</DebugType>
         </PropertyGroup>
         <ItemGroup>
         %s
         </ItemGroup>

         <ItemGroup>
         %s
         </ItemGroup>

         <Target Name="CleanOutputDirs" AfterTargets="Clean">
         <RemoveDir Directories="$(BaseIntermediateOutputPath)" /><!-- obj -->
         <RemoveDir Directories="$(BaseOutputPath)" /><!-- bin -->
         </Target>
         </Project>
         |}
      (List.map generated_files ~f:(fun file ->
         [%string {|    <Compile Include="%{file}" />|}])
       |> String.concat ~sep:"\n")
      (List.map fsproj_dependencies ~f:(fun file ->
         [%string {|    <ProjectReference Include="%{file}" />|}])
       |> String.concat ~sep:"\n")
  in
  Writer.save (output_dir ^/ project_name ^ ".fsproj") ~contents:fsproj
;;

let bin_prot_dotnet_path = "/lib/dotnet-libs/bin_prot/src"

let generate_jbuild
      ?(additional_nuget_config_path = "")
      ~output_dir
      ~additional_jbuild_entry
      ()
  =
  let root = "%{root}" in
  let first_dep = "%{first_dep}" in
  let jbuild =
    List.concat
      [ Option.to_list additional_jbuild_entry
      ; [ [%string
          {|
         (alias (
         ;; We are only allowed to build dotnet on hydra in feature-subtree-build
         (name feature-subtree-build)
         (sandbox ignore_targets)
         (deps (
         %{root}/lib/dotnet/runner/bin/dotnet_runner.exe
         %{root}/lib/dotnet-libs/nuget.config
         %{additional_nuget_config_path}
         (glob_files *.{fsproj,fs,fsi,sln})
         (Files_recursively_in_including_buildable
          %{root}/lib/dotnet-libs
          (glob *.{fsproj,fs,fsi,sln}))))
         (action "%{first_dep} build .")))

         (enforce_style ((sexp_styler ((enabled false)))))
         |}]
        ]
      ]
    |> String.concat ~sep:"\n"
  in
  Writer.save (output_dir ^/ "jbuild") ~contents:jbuild
;;

let generated_file_list_filename = "generated_file_list"

let generate_generated_file_list ~output_dir ~generated_files =
  List.sort generated_files ~compare:[%compare: string]
  |> Writer.save_lines (output_dir ^/ generated_file_list_filename)
;;

let write_generated_files ~dotnet_module_by_namespace ~output_dir =
  Deferred.List.iter
    (hash_queue_to_alist dotnet_module_by_namespace)
    ~f:(fun (namespace, dotnet_module) ->
      let path = output_dir ^/ namespace ^ ".fs" in
      match dotnet_module with
      | Dotnet_module.Override contents -> Writer.save path ~contents
      | Ast nested_ast ->
        let structure = Nested_ast.collapse nested_ast in
        Writer.with_file path ~f:(fun writer ->
          Writer.write_line writer [%string "module %{namespace}"];
          Writer.write_line writer [%string "open Bin_prot.Write"];
          Writer.write_line writer [%string "open Bin_prot.Read"];
          Writer.write_line writer [%string "open Bin_prot.Size"];
          ignore (printer#structure (Writer.write_line writer) structure : structure);
          return ()))
;;

let generate_f_sharp_types_with_existing_dependencies
      (t : Dotnet_bin_prot.t t)
      (generation_state : Generation_state.t)
  =
  let open Deferred.Or_error.Let_syntax in
  (* The dependencies in F# have to be explicitly ordered. We can't easily use the values
     returned from [generate_with_dependencies] as they don't preserve the dependency
     graph so we construct a dependency graph explicitly using the namespaces as the
     nodes and [namespace_dependencies] as the edges and do a topological sort. *)
  let dotnet_module_by_namespace = String.Hash_queue.create () in
  let%bind overrides =
    List.map
      t.overrides
      ~f:
        (Tuple2.map_fst ~f:(fun (dotnet_bin_prot : Dotnet_bin_prot.t) ->
           dotnet_bin_prot.source_code_position))
    |> Source_code_position.Map.of_alist_or_error
    |> Deferred.return
  in
  let namespace_dependencies = Queue.create () in
  let fsproj_path = Fsproj_path.of_string (t.output_dir ^/ t.project_name ^ ".fsproj") in
  let%bind generation_state =
    (* AST can only be parsed from a file so we use a temporary file that will have the
       AST from DSL written to it. *)
    Tempfile.with_tempfile ~prefix:"dotnet_bin_prot_ast" ~suffix:"" (fun ast_file ->
      Deferred.Or_error.List.fold
        t.types
        ~init:generation_state
        ~f:(fun generation_state dotnet_bin_prot ->
          generate_with_dependencies
            dotnet_bin_prot
            generation_state
            ~project_name:t.project_name
            ~ast_file
            ~overrides
            ~dotnet_module_by_namespace
            ~namespace_dependencies
            ~fsproj_path))
  in
  let relative_path fsproj_path =
    let%map root = Jenga_rules_integration.blocking_root () |> Deferred.return in
    let chopped_fsproj_path = String.chop_prefix_if_exists fsproj_path ~prefix:root in
    match String.chop_prefix t.output_dir ~prefix:root with
    | None ->
      let root = Jenga_rules_integration.blocking_root () |> ok_exn in
      root ^/ chopped_fsproj_path
    | Some suffix ->
      let number_of_dirs_from_root = String.count suffix ~f:(Char.equal '/') in
      (List.init number_of_dirs_from_root ~f:(Fn.const "..") |> String.concat ~sep:"/")
      ^/ chopped_fsproj_path
  in
  let bin_prot_fsproj_path =
    Fsproj_path.of_string (bin_prot_dotnet_path ^/ "Bin_prot.fsproj")
  in
  let generated_files =
    let nodes = Hash_queue.keys dotnet_module_by_namespace in
    let edges =
      Queue.to_list namespace_dependencies
      |> List.filter ~f:(fun { from; to_ } -> not (String.equal from to_))
    in
    Topological_sort.sort (module String) ~what:Nodes ~nodes ~edges
    |> ok_exn
    |> List.map ~f:(fun namespace -> namespace ^ ".fs")
  in
  let%bind.Deferred () =
    write_generated_files ~dotnet_module_by_namespace ~output_dir:t.output_dir
  in
  let%bind () =
    generate_jbuild
      ?additional_nuget_config_path:t.additional_nuget_config_path
      ~output_dir:t.output_dir
      ~additional_jbuild_entry:(t.additional_jbuild_entry ~generated_files)
      ()
    |> Deferred.ok
  in
  let%bind fsproj_dependencies =
    bin_prot_fsproj_path
    ::
    (match Map.find generation_state.fsproj_dependencies fsproj_path with
     | None -> []
     | Some fsproj_dependencies -> Set.to_list fsproj_dependencies)
    |> Deferred.Or_error.List.map ~f:(fun fsproj_path ->
      Fsproj_path.to_string fsproj_path |> relative_path)
  in
  let%bind () =
    generate_fs_proj
      ~output_dir:t.output_dir
      ~project_name:t.project_name
      ~generated_files
      ~fsproj_dependencies
    |> Deferred.ok
  in
  let%map () =
    generate_generated_file_list ~output_dir:t.output_dir ~generated_files |> Deferred.ok
  in
  generation_state
;;

let generate_f_sharp_types (t : Dotnet_bin_prot.t t) =
  generate_f_sharp_types_with_existing_dependencies t Generation_state.empty
  |> Deferred.Or_error.ignore_m
;;

let generate_f_sharp_types_of_modules
      (t : (module Dotnet_bin_prot.Dotnet_bin_prot_able) t)
  =
  let (t : Dotnet_bin_prot.t t) =
    { t with
      types =
        List.map t.types ~f:(fun (module M : Dotnet_bin_prot.Dotnet_bin_prot_able) ->
          M.dotnet_bin_prot_t)
    }
  in
  generate_f_sharp_types t
;;

let generate_f_sharp_types_with_multiple_projects ts =
  Deferred.Or_error.List.fold
    ts
    ~init:Generation_state.empty
    ~f:(fun generation_state t ->
      generate_f_sharp_types_with_existing_dependencies t generation_state)
  |> Deferred.Or_error.ignore_m
;;

let create_self_generating_rule
      ~project_name
      ~generated_files
      ~executable
      ~subcommands
      ~overrides
  =
  let deps =
    executable
    :: List.filter_map overrides ~f:(function
      | Override.Built_in (_ : string) -> None
      | Source_file
          { file_path; module_name = (_ : string); type_name = (_ : string) } ->
        let root = Jenga_rules_integration.blocking_root () |> ok_exn in
        (match String.chop_prefix file_path ~prefix:root with
         | Some suffix -> Some ("%{root}" ^/ suffix)
         | None -> Some file_path))
  in
  [%string
    {|
         (rule (
         (sandbox ignore_targets)
         (targets (
         jbuild
         generated_file_list
         %{project_name}.fsproj
         %{String.concat generated_files ~sep:"\n    "}))
         (deps (
         %{String.concat deps ~sep:"\n    "}
         ))
         (action "%{executable} %{String.concat subcommands ~sep:" "}")))

         (alias
         ((name DEFAULT)
         (deps (
         %{generated_file_list_filename}
         (glob_files {*.fs,*.fsi})))
         (action "diff <(ls | grep '\.fsi\?$') generated_file_list")))
         |}]
;;

let%expect_test "self generating rule looks reasonable" =
  let root = Jenga_rules_integration.blocking_root () |> ok_exn in
  let overrides =
    List.map
      [ root ^/ "relative/to/jenga/root"; "/absolute/dep" ]
      ~f:(fun file_path ->
        Override.Source_file { module_name = ""; type_name = ""; file_path })
  in
  print_endline
    (create_self_generating_rule
       ~project_name:"project_name"
       ~generated_files:[ "file1.fs"; "another_file.fs" ]
       ~executable:"%{root}/path/to/executable"
       ~subcommands:[ "first-sucommand-command"; "-generate" ]
       ~overrides);
  [%expect
    {|
              (rule (
              (sandbox ignore_targets)
              (targets (
              jbuild
              generated_file_list
              project_name.fsproj
              file1.fs
         another_file.fs))
              (deps (
              %{root}/path/to/executable
         %{root}/relative/to/jenga/root
         /absolute/dep
              ))
              (action "%{root}/path/to/executable first-sucommand-command -generate")))

              (alias
              ((name DEFAULT)
              (deps (
              generated_file_list
              (glob_files {*.fs,*.fsi})))
              (action "diff <(ls | grep '\.fsi\?$') generated_file_list"))) |}];
  return ()
;;
