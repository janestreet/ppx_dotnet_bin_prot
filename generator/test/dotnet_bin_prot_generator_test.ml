open! Core
open! Async
open! Import

module Expect_test_config = struct
  include Expect_test_config

  let sanitize =
    let jenga_root = Jenga_rules_integration.blocking_root () |> ok_exn in
    fun x -> String.substr_replace_all x ~pattern:jenga_root ~with_:"<jenga_root>"
  ;;
end

let project_name = "Test_project_name"

module Overriden = struct
  type with_dependency = With_built_in_override.with_dependency [@@deriving bin_io]

  let dotnet_bin_prot_with_dependency =
    Dotnet_bin_prot.declare_override_exn
      ~type_name:"with_dependency"
      ~dependencies_with_idents:
        [ "dependency", With_built_in_override.Dependency.dotnet_bin_prot_t ]
      [%here]
  ;;

  let with_dependency_override ~tmp_dir =
    let copied_from_generated_with_different_module_name =
      [%string
        {|open Bin_prot.Write
open Bin_prot.Read
open Bin_prot.Size
open Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.With_built_in_override.Dependency
module Override =
  struct
    type with_dependency =
      { dependency: t }
    let bin_size_with_dependency =
      function
      | { dependency = v1 } ->
          let size = 0 in
          Bin_prot.Common.(+) size
            (bin_size_t v1)
    let bin_write_with_dependency buf pos =
      function
      | { dependency = v1 } ->
          bin_write_t
            buf pos v1
    let bin_writer_with_dependency =
      {
        Bin_prot.Type_class.size = bin_size_with_dependency;
        Bin_prot.Type_class.write = bin_write_with_dependency
      }
    let __bin_read_with_dependency__ _buf pos_ref _vint =
      Bin_prot.Common.raise_variant_wrong_type
        "Test_project_name.Lib.Dotnet_libs.Bin_prot.Ppx.Generator.Test.Dotnet_bin_prot_generator_test.Overriden.Override_with_dependency.fs.with_dependency"
        (!pos_ref)
    let bin_read_with_dependency buf pos_ref =
      let v_dependency =
        bin_read_t
          buf pos_ref in
      { dependency = v_dependency }
    let bin_reader_with_dependency =
      {
        Bin_prot.Type_class.read = bin_read_with_dependency;
        Bin_prot.Type_class.vtag_read = __bin_read_with_dependency__
      }
  end
|}]
    in
    let module_name = "Override" in
    let type_name = "with_dependency" in
    let overrides_dir = tmp_dir ^/ "overrides" in
    let%bind () = Unix.mkdir overrides_dir in
    let file_path = overrides_dir ^/ "random_file_name_that_is_different.fs" in
    let%map () =
      Writer.save file_path ~contents:copied_from_generated_with_different_module_name
    in
    Dotnet_bin_prot_generator.Override.Source_file { module_name; type_name; file_path }
  ;;
end

type 'a record =
  { dependency : ('a, 'a) Manifest_equality.t
  ; immediate : int
  ; built_in_overriden : With_built_in_override.built_in
  ; with_dependency_overriden : Overriden.with_dependency
  }
[@@deriving bin_io, dotnet_bin_prot]

type variant =
  | A_long_variant_name_to_force_newline of (unit, string) Manifest_equality.t
[@@deriving bin_io, dotnet_bin_prot]

let%expect_test "generated F# code looks reasonable" =
  Expect_test_helpers_async.with_temp_dir (fun tmp_dir ->
    let%bind overrides =
      Deferred.List.map
        [ ( With_built_in_override.dotnet_bin_prot_built_in
          , With_built_in_override.built_in_override )
        ; Overriden.dotnet_bin_prot_with_dependency, Overriden.with_dependency_override
        ]
        ~f:(fun (t, override) ->
          let%map override = override ~tmp_dir in
          t, override)
    in
    let%bind () =
      Dotnet_bin_prot_generator.create
        ()
        ~types:[ dotnet_bin_prot_record; dotnet_bin_prot_variant ]
        ~overrides
        ~output_dir:tmp_dir
        ~project_name
        ~additional_nuget_config_path:"additional-path/nuget.config"
        ~additional_jbuild_entry:(fun ~generated_files ->
          Some
            (Dotnet_bin_prot_generator.create_self_generating_rule
               ~project_name
               ~generated_files
               ~executable:"true"
               ~subcommands:[]
               ~overrides:(List.map overrides ~f:snd)))
      |> Dotnet_bin_prot_generator.generate_f_sharp_types
      |> Deferred.Or_error.ok_exn
    in
    let%bind () =
      let sanitize_tmp_dir =
        let tmp_dir_search_pattern = String.Search_pattern.create tmp_dir in
        fun s ->
          String.Search_pattern.replace_all
            tmp_dir_search_pattern
            ~in_:s
            ~with_:"<TMPDIR>"
      in
      Deferred.List.iter (Sys_extended.ls tmp_dir) ~f:(fun filename ->
        let file_path = tmp_dir ^/ filename in
        let print_separator () = print_endline "-------------------------------" in
        match Sys_extended.file_kind file_path with
        | S_REG ->
          print_endline filename;
          print_separator ();
          let%map contents = Async.Reader.file_contents file_path in
          print_endline (sanitize_tmp_dir contents)
        | _ ->
          print_separator ();
          printf "Ignoring non regular file: %s\n" filename;
          print_separator ();
          print_endline "";
          return ())
    in
    [%expect
      {|
      Test_project_name.Override.fs
      -------------------------------
      namespace Test_project_name
      open Bin_prot.Write
      open Bin_prot.Read
      open Bin_prot.Size
      open Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.With_built_in_override.Dependency
      module Override =
        struct
          type with_dependency =
            { dependency: t }
          let bin_size_with_dependency =
            function
            | { dependency = v1 } ->
                let size = 0 in
                Bin_prot.Common.(+) size
                  (bin_size_t v1)
          let bin_write_with_dependency buf pos =
            function
            | { dependency = v1 } ->
                bin_write_t
                  buf pos v1
          let bin_writer_with_dependency =
            {
              Bin_prot.Type_class.size = bin_size_with_dependency;
              Bin_prot.Type_class.write = bin_write_with_dependency
            }
          let __bin_read_with_dependency__ _buf pos_ref _vint =
            Bin_prot.Common.raise_variant_wrong_type
              "Test_project_name.Lib.Dotnet_libs.Bin_prot.Ppx.Generator.Test.Dotnet_bin_prot_generator_test.Overriden.Override_with_dependency.fs.with_dependency"
              (!pos_ref)
          let bin_read_with_dependency buf pos_ref =
            let v_dependency =
              bin_read_t
                buf pos_ref in
            { dependency = v_dependency }
          let bin_reader_with_dependency =
            {
              Bin_prot.Type_class.read = bin_read_with_dependency;
              Bin_prot.Type_class.vtag_read = __bin_read_with_dependency__
            }
        end

      Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Dotnet_bin_prot_generator_test.fs
      -------------------------------
      module Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Dotnet_bin_prot_generator_test
      open Bin_prot.Write
      open Bin_prot.Read
      open Bin_prot.Size
      type 'a record = {
        dependency: ('a, 'a) Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Manifest_equality.T.t ;
        immediate: int64 ;
        built_in_overriden: string ;
        with_dependency_overriden: Test_project_name.Override.with_dependency }
      let bin_size_record _size_of_a =
        function
        | { dependency = v1; immediate = v2; built_in_overriden = v3;
            with_dependency_overriden = v4 } ->
            let size = 0 in
            let size =
              Bin_prot.Common.(+) size
                (Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Manifest_equality.T.bin_size_t
                   _size_of_a _size_of_a v1) in
            let size = Bin_prot.Common.(+) size (bin_size_int64 v2) in
            let size = Bin_prot.Common.(+) size (bin_size_string v3) in
            Bin_prot.Common.(+) size
              (Test_project_name.Override.bin_size_with_dependency v4)
      let bin_write_record _write_a buf pos =
        function
        | { dependency = v1; immediate = v2; built_in_overriden = v3;
            with_dependency_overriden = v4 } ->
            let pos =
              Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Manifest_equality.T.bin_write_t
                _write_a _write_a buf pos v1 in
            let pos = bin_write_int64 buf pos v2 in
            let pos = bin_write_string buf pos v3 in
            Test_project_name.Override.bin_write_with_dependency buf pos v4
      let bin_writer_record bin_writer_a =
        {
          Bin_prot.Type_class.size =
            (fun v ->
               bin_size_record (bin_writer_a : _ Bin_prot.Type_class.writer).size v);
          Bin_prot.Type_class.write =
            (fun v ->
               bin_write_record (bin_writer_a : _ Bin_prot.Type_class.writer).write
                 v)
        }
      let __bin_read_record__ _of__a _buf pos_ref _vint =
        Bin_prot.Common.raise_variant_wrong_type
          "Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Dotnet_bin_prot_generator_test.fs.record"
          (!pos_ref)
      let bin_read_record _of__a buf pos_ref =
        let v_dependency =
          (Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Manifest_equality.T.bin_read_t
             _of__a _of__a) buf pos_ref in
        let v_immediate = bin_read_int64 buf pos_ref in
        let v_built_in_overriden = bin_read_string buf pos_ref in
        let v_with_dependency_overriden =
          Test_project_name.Override.bin_read_with_dependency buf pos_ref in
        {
          dependency = v_dependency;
          immediate = v_immediate;
          built_in_overriden = v_built_in_overriden;
          with_dependency_overriden = v_with_dependency_overriden
        }
      let bin_reader_record bin_reader_a =
        {
          Bin_prot.Type_class.read =
            (fun buf ->
               fun pos_ref ->
                 (bin_read_record
                    (bin_reader_a : _ Bin_prot.Type_class.reader).read) buf pos_ref);
          Bin_prot.Type_class.vtag_read =
            (fun buf ->
               fun pos_ref ->
                 fun vtag ->
                   (__bin_read_record__
                      (bin_reader_a : _ Bin_prot.Type_class.reader).read) buf
                     pos_ref vtag)
        }
      let bin_record bin_a =
        {
          Bin_prot.Type_class.writer =
            (bin_writer_record (bin_a : _ Bin_prot.Type_class.t).writer);
          Bin_prot.Type_class.reader =
            (bin_reader_record (bin_a : _ Bin_prot.Type_class.t).reader)
        }
      type variant =
        | A_long_variant_name_to_force_newline of (unit, string) Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Manifest_equality.T.t
      let bin_size_variant =
        function
        | A_long_variant_name_to_force_newline v1 ->
            let size = 1 in
            Bin_prot.Common.(+) size
              (Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Manifest_equality.T.bin_size_t
                 bin_size_unit bin_size_string v1)
      let bin_write_variant buf pos =
        function
        | A_long_variant_name_to_force_newline v1 ->
            let pos = Bin_prot.Write.bin_write_int_8bit buf pos 0 in
            Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Manifest_equality.T.bin_write_t
              bin_write_unit bin_write_string buf pos v1
      let bin_writer_variant =
        {
          Bin_prot.Type_class.size = bin_size_variant;
          Bin_prot.Type_class.write = bin_write_variant
        }
      let __bin_read_variant__ _buf pos_ref _vint =
        Bin_prot.Common.raise_variant_wrong_type
          "Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Dotnet_bin_prot_generator_test.fs.variant"
          (!pos_ref)
      let bin_read_variant buf pos_ref =
        match Bin_prot.Read.bin_read_int_8bit buf pos_ref with
        | 0 ->
            let arg_1 =
              (Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Manifest_equality.T.bin_read_t
                 bin_read_unit bin_read_string) buf pos_ref in
            A_long_variant_name_to_force_newline arg_1
        | _ ->
            Bin_prot.Common.raise_read_error
              (Bin_prot.Common.ReadError.Sum_tag
                 "Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Dotnet_bin_prot_generator_test.fs.variant")
              (!pos_ref)
      let bin_reader_variant =
        {
          Bin_prot.Type_class.read = bin_read_variant;
          Bin_prot.Type_class.vtag_read = __bin_read_variant__
        }
      let bin_variant =
        {
          Bin_prot.Type_class.writer = bin_writer_variant;
          Bin_prot.Type_class.reader = bin_reader_variant
        }

      Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Manifest_equality.fs
      -------------------------------
      module Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Manifest_equality
      open Bin_prot.Write
      open Bin_prot.Read
      open Bin_prot.Size
      module Unique_id = struct
        type t = int64
        let bin_size_t = bin_size_int64
        let bin_write_t = bin_write_int64
        let bin_writer_t =
          {
            Bin_prot.Type_class.size = bin_size_t;
            Bin_prot.Type_class.write = bin_write_t
          }
        let __bin_read_t__ = __bin_read_int64__
        let bin_read_t = bin_read_int64
        let bin_reader_t =
          {
            Bin_prot.Type_class.read = bin_read_t;
            Bin_prot.Type_class.vtag_read = __bin_read_t__
          }
        let bin_t =
          {
            Bin_prot.Type_class.writer = bin_writer_t;
            Bin_prot.Type_class.reader = bin_reader_t
          }
      end
      module Path_id = struct
        type t = Unique_id.t
        let bin_size_t = Unique_id.bin_size_t
        let bin_write_t = Unique_id.bin_write_t
        let bin_writer_t =
          {
            Bin_prot.Type_class.size = bin_size_t;
            Bin_prot.Type_class.write = bin_write_t
          }
        let __bin_read_t__ = Unique_id.__bin_read_t__
        let bin_read_t = Unique_id.bin_read_t
        let bin_reader_t =
          {
            Bin_prot.Type_class.read = bin_read_t;
            Bin_prot.Type_class.vtag_read = __bin_read_t__
          }
        let bin_t =
          {
            Bin_prot.Type_class.writer = bin_writer_t;
            Bin_prot.Type_class.reader = bin_reader_t
          }
      end
      module Priority = struct
        type t = int64
        let bin_size_t = bin_size_int64
        let bin_write_t = bin_write_int64
        let bin_writer_t =
          {
            Bin_prot.Type_class.size = bin_size_t;
            Bin_prot.Type_class.write = bin_write_t
          }
        let __bin_read_t__ = __bin_read_int64__
        let bin_read_t = bin_read_int64
        let bin_reader_t =
          {
            Bin_prot.Type_class.read = bin_read_t;
            Bin_prot.Type_class.vtag_read = __bin_read_t__
          }
        let bin_t =
          {
            Bin_prot.Type_class.writer = bin_writer_t;
            Bin_prot.Type_class.reader = bin_reader_t
          }
      end
      module Seq_id = struct
        type t = Unique_id.t
        let bin_size_t = Unique_id.bin_size_t
        let bin_write_t = Unique_id.bin_write_t
        let bin_writer_t =
          {
            Bin_prot.Type_class.size = bin_size_t;
            Bin_prot.Type_class.write = bin_write_t
          }
        let __bin_read_t__ = Unique_id.__bin_read_t__
        let bin_read_t = Unique_id.bin_read_t
        let bin_reader_t =
          {
            Bin_prot.Type_class.read = bin_read_t;
            Bin_prot.Type_class.vtag_read = __bin_read_t__
          }
        let bin_t =
          {
            Bin_prot.Type_class.writer = bin_writer_t;
            Bin_prot.Type_class.reader = bin_reader_t
          }
      end
      module Original_generated_0 = struct
        type t =
          | Nested
        let bin_size_t = function | _ -> 4
        let bin_write_t buf pos =
          function
          | Nested -> Bin_prot.Write.bin_write_variant_int buf pos (-283758857)
        let bin_writer_t =
          {
            Bin_prot.Type_class.size = bin_size_t;
            Bin_prot.Type_class.write = bin_write_t
          }
        let __bin_read_t__ _buf _ vint =
          match vint with
          | (-283758857) -> Nested
          | _ -> raise Bin_prot.Common.No_variant_match
        let bin_read_t buf pos_ref =
          let vint = Bin_prot.Read.bin_read_variant_int buf pos_ref in
          try __bin_read_t__ buf pos_ref vint
          with
          | Bin_prot.Common.No_variant_match ->
              let err =
                Bin_prot.Common.ReadError.Variant
                  "Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Manifest_equality.fs.t" in
              Bin_prot.Common.raise_read_error err (!pos_ref)
        let bin_reader_t =
          {
            Bin_prot.Type_class.read = bin_read_t;
            Bin_prot.Type_class.vtag_read = __bin_read_t__
          }
        let bin_t =
          {
            Bin_prot.Type_class.writer = bin_writer_t;
            Bin_prot.Type_class.reader = bin_reader_t
          }
      end
      module Original_generated_1 = struct
        type ('a, 'b) t = {
          field: unit ;
          poly: ('a * 'b) ;
          nested: Original_generated_0.t }
        let bin_size_t _size_of_a _size_of_b =
          function
          | { field = v1; poly = v2; nested = v3 } ->
              let size = 0 in
              let size = Bin_prot.Common.(+) size (bin_size_unit v1) in
              let size =
                Bin_prot.Common.(+) size
                  (match v2 with
                   | (v1, v2) ->
                       let size = 0 in
                       let size = Bin_prot.Common.(+) size (_size_of_a v1) in
                       Bin_prot.Common.(+) size (_size_of_b v2)) in
              Bin_prot.Common.(+) size (Original_generated_0.bin_size_t v3)
        let bin_write_t _write_a _write_b buf pos =
          function
          | { field = v1; poly = v2; nested = v3 } ->
              let pos = bin_write_unit buf pos v1 in
              let pos =
                match v2 with
                | (v1, v2) -> let pos = _write_a buf pos v1 in _write_b buf pos v2 in
              Original_generated_0.bin_write_t buf pos v3
        let bin_writer_t bin_writer_a bin_writer_b =
          {
            Bin_prot.Type_class.size =
              (fun v ->
                 bin_size_t (bin_writer_a : _ Bin_prot.Type_class.writer).size
                   (bin_writer_b : _ Bin_prot.Type_class.writer).size v);
            Bin_prot.Type_class.write =
              (fun v ->
                 bin_write_t (bin_writer_a : _ Bin_prot.Type_class.writer).write
                   (bin_writer_b : _ Bin_prot.Type_class.writer).write v)
          }
        let __bin_read_t__ _of__a _of__b _buf pos_ref _vint =
          Bin_prot.Common.raise_variant_wrong_type
            "Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Manifest_equality.fs.t"
            (!pos_ref)
        let bin_read_t _of__a _of__b buf pos_ref =
          let v_field = bin_read_unit buf pos_ref in
          let v_poly =
            let v1 = _of__a buf pos_ref in let v2 = _of__b buf pos_ref in (v1, v2) in
          let v_nested = Original_generated_0.bin_read_t buf pos_ref in
          { field = v_field; poly = v_poly; nested = v_nested }
        let bin_reader_t bin_reader_a bin_reader_b =
          {
            Bin_prot.Type_class.read =
              (fun buf ->
                 fun pos_ref ->
                   (bin_read_t (bin_reader_a : _ Bin_prot.Type_class.reader).read
                      (bin_reader_b : _ Bin_prot.Type_class.reader).read) buf pos_ref);
            Bin_prot.Type_class.vtag_read =
              (fun buf ->
                 fun pos_ref ->
                   fun vtag ->
                     (__bin_read_t__
                        (bin_reader_a : _ Bin_prot.Type_class.reader).read
                        (bin_reader_b : _ Bin_prot.Type_class.reader).read) buf
                       pos_ref vtag)
          }
        let bin_t bin_a bin_b =
          {
            Bin_prot.Type_class.writer =
              (bin_writer_t (bin_a : _ Bin_prot.Type_class.t).writer
                 (bin_b : _ Bin_prot.Type_class.t).writer);
            Bin_prot.Type_class.reader =
              (bin_reader_t (bin_a : _ Bin_prot.Type_class.t).reader
                 (bin_b : _ Bin_prot.Type_class.t).reader)
          }
      end
      module Original_generated_2 = struct
        type t =
          | Test
        let bin_size_t = function | _ -> 4
        let bin_write_t buf pos =
          function
          | Test -> Bin_prot.Write.bin_write_variant_int buf pos (-856045486)
        let bin_writer_t =
          {
            Bin_prot.Type_class.size = bin_size_t;
            Bin_prot.Type_class.write = bin_write_t
          }
        let __bin_read_t__ _buf _ vint =
          match vint with
          | (-856045486) -> Test
          | _ -> raise Bin_prot.Common.No_variant_match
        let bin_read_t buf pos_ref =
          let vint = Bin_prot.Read.bin_read_variant_int buf pos_ref in
          try __bin_read_t__ buf pos_ref vint
          with
          | Bin_prot.Common.No_variant_match ->
              let err =
                Bin_prot.Common.ReadError.Variant
                  "Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Manifest_equality.fs.t" in
              Bin_prot.Common.raise_read_error err (!pos_ref)
        let bin_reader_t =
          {
            Bin_prot.Type_class.read = bin_read_t;
            Bin_prot.Type_class.vtag_read = __bin_read_t__
          }
        let bin_t =
          {
            Bin_prot.Type_class.writer = bin_writer_t;
            Bin_prot.Type_class.reader = bin_reader_t
          }
      end
      type ('a, 'b) original =
        | First of 'a
        | Second of 'b
        | Neither of ('a, 'b) Original_generated_1.t
        | Poly of Original_generated_2.t
        | Recursive of ('a, 'b) original
        | Map of int64 Test_project_name.Ppx.Ppx_dotnet_bin_prot.Runtime_lib.Dotnet_bin_prot.Generated_map.t
        | Set of Test_project_name.Ppx.Ppx_dotnet_bin_prot.Runtime_lib.Dotnet_bin_prot.Generated_set.t
        | Seq_id of Seq_id.t
        | Path_id of Path_id.t
        | Module_created_with_include of Priority.t
      let rec bin_size_original _size_of_a _size_of_b =
        function
        | First v1 -> let size = 1 in Bin_prot.Common.(+) size (_size_of_a v1)
        | Second v1 -> let size = 1 in Bin_prot.Common.(+) size (_size_of_b v1)
        | Neither v1 ->
            let size = 1 in
            Bin_prot.Common.(+) size
              (Original_generated_1.bin_size_t _size_of_a _size_of_b v1)
        | Poly v1 ->
            let size = 1 in
            Bin_prot.Common.(+) size (Original_generated_2.bin_size_t v1)
        | Recursive v1 ->
            let size = 1 in
            Bin_prot.Common.(+) size (bin_size_original _size_of_a _size_of_b v1)
        | Map v1 ->
            let size = 1 in
            Bin_prot.Common.(+) size
              (Test_project_name.Ppx.Ppx_dotnet_bin_prot.Runtime_lib.Dotnet_bin_prot.Generated_map.bin_size_t
                 bin_size_int64 v1)
        | Set v1 ->
            let size = 1 in
            Bin_prot.Common.(+) size
              (Test_project_name.Ppx.Ppx_dotnet_bin_prot.Runtime_lib.Dotnet_bin_prot.Generated_set.bin_size_t
                 v1)
        | Seq_id v1 ->
            let size = 1 in Bin_prot.Common.(+) size (Seq_id.bin_size_t v1)
        | Path_id v1 ->
            let size = 1 in Bin_prot.Common.(+) size (Path_id.bin_size_t v1)
        | Module_created_with_include v1 ->
            let size = 1 in Bin_prot.Common.(+) size (Priority.bin_size_t v1)
      let rec bin_write_original _write_a _write_b buf pos =
        function
        | First v1 ->
            let pos = Bin_prot.Write.bin_write_int_8bit buf pos 0 in
            _write_a buf pos v1
        | Second v1 ->
            let pos = Bin_prot.Write.bin_write_int_8bit buf pos 1 in
            _write_b buf pos v1
        | Neither v1 ->
            let pos = Bin_prot.Write.bin_write_int_8bit buf pos 2 in
            Original_generated_1.bin_write_t _write_a _write_b buf pos v1
        | Poly v1 ->
            let pos = Bin_prot.Write.bin_write_int_8bit buf pos 3 in
            Original_generated_2.bin_write_t buf pos v1
        | Recursive v1 ->
            let pos = Bin_prot.Write.bin_write_int_8bit buf pos 4 in
            bin_write_original _write_a _write_b buf pos v1
        | Map v1 ->
            let pos = Bin_prot.Write.bin_write_int_8bit buf pos 5 in
            Test_project_name.Ppx.Ppx_dotnet_bin_prot.Runtime_lib.Dotnet_bin_prot.Generated_map.bin_write_t
              bin_write_int64 buf pos v1
        | Set v1 ->
            let pos = Bin_prot.Write.bin_write_int_8bit buf pos 6 in
            Test_project_name.Ppx.Ppx_dotnet_bin_prot.Runtime_lib.Dotnet_bin_prot.Generated_set.bin_write_t
              buf pos v1
        | Seq_id v1 ->
            let pos = Bin_prot.Write.bin_write_int_8bit buf pos 7 in
            Seq_id.bin_write_t buf pos v1
        | Path_id v1 ->
            let pos = Bin_prot.Write.bin_write_int_8bit buf pos 8 in
            Path_id.bin_write_t buf pos v1
        | Module_created_with_include v1 ->
            let pos = Bin_prot.Write.bin_write_int_8bit buf pos 9 in
            Priority.bin_write_t buf pos v1
      let bin_writer_original bin_writer_a bin_writer_b =
        {
          Bin_prot.Type_class.size =
            (fun v ->
               bin_size_original (bin_writer_a : _ Bin_prot.Type_class.writer).size
                 (bin_writer_b : _ Bin_prot.Type_class.writer).size v);
          Bin_prot.Type_class.write =
            (fun v ->
               bin_write_original
                 (bin_writer_a : _ Bin_prot.Type_class.writer).write
                 (bin_writer_b : _ Bin_prot.Type_class.writer).write v)
        }
      let rec __bin_read_original__ _of__a _of__b _buf pos_ref _vint =
        Bin_prot.Common.raise_variant_wrong_type
          "Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Manifest_equality.fs.original"
          (!pos_ref)
      and bin_read_original _of__a _of__b buf pos_ref =
        match Bin_prot.Read.bin_read_int_8bit buf pos_ref with
        | 0 -> let arg_1 = _of__a buf pos_ref in First arg_1
        | 1 -> let arg_1 = _of__b buf pos_ref in Second arg_1
        | 2 ->
            let arg_1 = (Original_generated_1.bin_read_t _of__a _of__b) buf pos_ref in
            Neither arg_1
        | 3 ->
            let arg_1 = Original_generated_2.bin_read_t buf pos_ref in Poly arg_1
        | 4 ->
            let arg_1 = (bin_read_original _of__a _of__b) buf pos_ref in
            Recursive arg_1
        | 5 ->
            let arg_1 =
              (Test_project_name.Ppx.Ppx_dotnet_bin_prot.Runtime_lib.Dotnet_bin_prot.Generated_map.bin_read_t
                 bin_read_int64) buf pos_ref in
            Map arg_1
        | 6 ->
            let arg_1 =
              Test_project_name.Ppx.Ppx_dotnet_bin_prot.Runtime_lib.Dotnet_bin_prot.Generated_set.bin_read_t
                buf pos_ref in
            Set arg_1
        | 7 -> let arg_1 = Seq_id.bin_read_t buf pos_ref in Seq_id arg_1
        | 8 -> let arg_1 = Path_id.bin_read_t buf pos_ref in Path_id arg_1
        | 9 ->
            let arg_1 = Priority.bin_read_t buf pos_ref in
            Module_created_with_include arg_1
        | _ ->
            Bin_prot.Common.raise_read_error
              (Bin_prot.Common.ReadError.Sum_tag
                 "Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Manifest_equality.fs.original")
              (!pos_ref)
      let bin_reader_original bin_reader_a bin_reader_b =
        {
          Bin_prot.Type_class.read =
            (fun buf ->
               fun pos_ref ->
                 (bin_read_original
                    (bin_reader_a : _ Bin_prot.Type_class.reader).read
                    (bin_reader_b : _ Bin_prot.Type_class.reader).read) buf pos_ref);
          Bin_prot.Type_class.vtag_read =
            (fun buf ->
               fun pos_ref ->
                 fun vtag ->
                   (__bin_read_original__
                      (bin_reader_a : _ Bin_prot.Type_class.reader).read
                      (bin_reader_b : _ Bin_prot.Type_class.reader).read) buf
                     pos_ref vtag)
        }
      let rec bin_original bin_a bin_b =
        {
          Bin_prot.Type_class.writer =
            (bin_writer_original (bin_a : _ Bin_prot.Type_class.t).writer
               (bin_b : _ Bin_prot.Type_class.t).writer);
          Bin_prot.Type_class.reader =
            (bin_reader_original (bin_a : _ Bin_prot.Type_class.t).reader
               (bin_b : _ Bin_prot.Type_class.t).reader)
        }
      module Redefinition_generated_0 = struct
        type t =
          | Nested
        let bin_size_t = function | _ -> 4
        let bin_write_t buf pos =
          function
          | Nested -> Bin_prot.Write.bin_write_variant_int buf pos (-283758857)
        let bin_writer_t =
          {
            Bin_prot.Type_class.size = bin_size_t;
            Bin_prot.Type_class.write = bin_write_t
          }
        let __bin_read_t__ _buf _ vint =
          match vint with
          | (-283758857) -> Nested
          | _ -> raise Bin_prot.Common.No_variant_match
        let bin_read_t buf pos_ref =
          let vint = Bin_prot.Read.bin_read_variant_int buf pos_ref in
          try __bin_read_t__ buf pos_ref vint
          with
          | Bin_prot.Common.No_variant_match ->
              let err =
                Bin_prot.Common.ReadError.Variant
                  "Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Manifest_equality.fs.t" in
              Bin_prot.Common.raise_read_error err (!pos_ref)
        let bin_reader_t =
          {
            Bin_prot.Type_class.read = bin_read_t;
            Bin_prot.Type_class.vtag_read = __bin_read_t__
          }
        let bin_t =
          {
            Bin_prot.Type_class.writer = bin_writer_t;
            Bin_prot.Type_class.reader = bin_reader_t
          }
      end
      module Redefinition_generated_1 = struct
        type ('a, 'b) t = {
          field: unit ;
          poly: ('a * 'b) ;
          nested: Redefinition_generated_0.t }
        let bin_size_t _size_of_a _size_of_b =
          function
          | { field = v1; poly = v2; nested = v3 } ->
              let size = 0 in
              let size = Bin_prot.Common.(+) size (bin_size_unit v1) in
              let size =
                Bin_prot.Common.(+) size
                  (match v2 with
                   | (v1, v2) ->
                       let size = 0 in
                       let size = Bin_prot.Common.(+) size (_size_of_a v1) in
                       Bin_prot.Common.(+) size (_size_of_b v2)) in
              Bin_prot.Common.(+) size (Redefinition_generated_0.bin_size_t v3)
        let bin_write_t _write_a _write_b buf pos =
          function
          | { field = v1; poly = v2; nested = v3 } ->
              let pos = bin_write_unit buf pos v1 in
              let pos =
                match v2 with
                | (v1, v2) -> let pos = _write_a buf pos v1 in _write_b buf pos v2 in
              Redefinition_generated_0.bin_write_t buf pos v3
        let bin_writer_t bin_writer_a bin_writer_b =
          {
            Bin_prot.Type_class.size =
              (fun v ->
                 bin_size_t (bin_writer_a : _ Bin_prot.Type_class.writer).size
                   (bin_writer_b : _ Bin_prot.Type_class.writer).size v);
            Bin_prot.Type_class.write =
              (fun v ->
                 bin_write_t (bin_writer_a : _ Bin_prot.Type_class.writer).write
                   (bin_writer_b : _ Bin_prot.Type_class.writer).write v)
          }
        let __bin_read_t__ _of__a _of__b _buf pos_ref _vint =
          Bin_prot.Common.raise_variant_wrong_type
            "Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Manifest_equality.fs.t"
            (!pos_ref)
        let bin_read_t _of__a _of__b buf pos_ref =
          let v_field = bin_read_unit buf pos_ref in
          let v_poly =
            let v1 = _of__a buf pos_ref in let v2 = _of__b buf pos_ref in (v1, v2) in
          let v_nested = Redefinition_generated_0.bin_read_t buf pos_ref in
          { field = v_field; poly = v_poly; nested = v_nested }
        let bin_reader_t bin_reader_a bin_reader_b =
          {
            Bin_prot.Type_class.read =
              (fun buf ->
                 fun pos_ref ->
                   (bin_read_t (bin_reader_a : _ Bin_prot.Type_class.reader).read
                      (bin_reader_b : _ Bin_prot.Type_class.reader).read) buf pos_ref);
            Bin_prot.Type_class.vtag_read =
              (fun buf ->
                 fun pos_ref ->
                   fun vtag ->
                     (__bin_read_t__
                        (bin_reader_a : _ Bin_prot.Type_class.reader).read
                        (bin_reader_b : _ Bin_prot.Type_class.reader).read) buf
                       pos_ref vtag)
          }
        let bin_t bin_a bin_b =
          {
            Bin_prot.Type_class.writer =
              (bin_writer_t (bin_a : _ Bin_prot.Type_class.t).writer
                 (bin_b : _ Bin_prot.Type_class.t).writer);
            Bin_prot.Type_class.reader =
              (bin_reader_t (bin_a : _ Bin_prot.Type_class.t).reader
                 (bin_b : _ Bin_prot.Type_class.t).reader)
          }
      end
      module Redefinition_generated_2 = struct
        type t =
          | Test
        let bin_size_t = function | _ -> 4
        let bin_write_t buf pos =
          function
          | Test -> Bin_prot.Write.bin_write_variant_int buf pos (-856045486)
        let bin_writer_t =
          {
            Bin_prot.Type_class.size = bin_size_t;
            Bin_prot.Type_class.write = bin_write_t
          }
        let __bin_read_t__ _buf _ vint =
          match vint with
          | (-856045486) -> Test
          | _ -> raise Bin_prot.Common.No_variant_match
        let bin_read_t buf pos_ref =
          let vint = Bin_prot.Read.bin_read_variant_int buf pos_ref in
          try __bin_read_t__ buf pos_ref vint
          with
          | Bin_prot.Common.No_variant_match ->
              let err =
                Bin_prot.Common.ReadError.Variant
                  "Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Manifest_equality.fs.t" in
              Bin_prot.Common.raise_read_error err (!pos_ref)
        let bin_reader_t =
          {
            Bin_prot.Type_class.read = bin_read_t;
            Bin_prot.Type_class.vtag_read = __bin_read_t__
          }
        let bin_t =
          {
            Bin_prot.Type_class.writer = bin_writer_t;
            Bin_prot.Type_class.reader = bin_reader_t
          }
      end
      type ('a, 'b) redefinition =
        | First of 'a
        | Second of 'b
        | Neither of ('a, 'b) Redefinition_generated_1.t
        | Poly of Redefinition_generated_2.t
        | Recursive of ('a, 'b) redefinition
        | Map of int64 Test_project_name.Ppx.Ppx_dotnet_bin_prot.Runtime_lib.Dotnet_bin_prot.Generated_map.t
        | Set of Test_project_name.Ppx.Ppx_dotnet_bin_prot.Runtime_lib.Dotnet_bin_prot.Generated_set.t
        | Seq_id of Seq_id.t
        | Path_id of Path_id.t
        | Module_created_with_include of Priority.t
      let rec bin_size_redefinition _size_of_a _size_of_b =
        function
        | First v1 -> let size = 1 in Bin_prot.Common.(+) size (_size_of_a v1)
        | Second v1 -> let size = 1 in Bin_prot.Common.(+) size (_size_of_b v1)
        | Neither v1 ->
            let size = 1 in
            Bin_prot.Common.(+) size
              (Redefinition_generated_1.bin_size_t _size_of_a _size_of_b v1)
        | Poly v1 ->
            let size = 1 in
            Bin_prot.Common.(+) size (Redefinition_generated_2.bin_size_t v1)
        | Recursive v1 ->
            let size = 1 in
            Bin_prot.Common.(+) size
              (bin_size_redefinition _size_of_a _size_of_b v1)
        | Map v1 ->
            let size = 1 in
            Bin_prot.Common.(+) size
              (Test_project_name.Ppx.Ppx_dotnet_bin_prot.Runtime_lib.Dotnet_bin_prot.Generated_map.bin_size_t
                 bin_size_int64 v1)
        | Set v1 ->
            let size = 1 in
            Bin_prot.Common.(+) size
              (Test_project_name.Ppx.Ppx_dotnet_bin_prot.Runtime_lib.Dotnet_bin_prot.Generated_set.bin_size_t
                 v1)
        | Seq_id v1 ->
            let size = 1 in Bin_prot.Common.(+) size (Seq_id.bin_size_t v1)
        | Path_id v1 ->
            let size = 1 in Bin_prot.Common.(+) size (Path_id.bin_size_t v1)
        | Module_created_with_include v1 ->
            let size = 1 in Bin_prot.Common.(+) size (Priority.bin_size_t v1)
      let rec bin_write_redefinition _write_a _write_b buf pos =
        function
        | First v1 ->
            let pos = Bin_prot.Write.bin_write_int_8bit buf pos 0 in
            _write_a buf pos v1
        | Second v1 ->
            let pos = Bin_prot.Write.bin_write_int_8bit buf pos 1 in
            _write_b buf pos v1
        | Neither v1 ->
            let pos = Bin_prot.Write.bin_write_int_8bit buf pos 2 in
            Redefinition_generated_1.bin_write_t _write_a _write_b buf pos v1
        | Poly v1 ->
            let pos = Bin_prot.Write.bin_write_int_8bit buf pos 3 in
            Redefinition_generated_2.bin_write_t buf pos v1
        | Recursive v1 ->
            let pos = Bin_prot.Write.bin_write_int_8bit buf pos 4 in
            bin_write_redefinition _write_a _write_b buf pos v1
        | Map v1 ->
            let pos = Bin_prot.Write.bin_write_int_8bit buf pos 5 in
            Test_project_name.Ppx.Ppx_dotnet_bin_prot.Runtime_lib.Dotnet_bin_prot.Generated_map.bin_write_t
              bin_write_int64 buf pos v1
        | Set v1 ->
            let pos = Bin_prot.Write.bin_write_int_8bit buf pos 6 in
            Test_project_name.Ppx.Ppx_dotnet_bin_prot.Runtime_lib.Dotnet_bin_prot.Generated_set.bin_write_t
              buf pos v1
        | Seq_id v1 ->
            let pos = Bin_prot.Write.bin_write_int_8bit buf pos 7 in
            Seq_id.bin_write_t buf pos v1
        | Path_id v1 ->
            let pos = Bin_prot.Write.bin_write_int_8bit buf pos 8 in
            Path_id.bin_write_t buf pos v1
        | Module_created_with_include v1 ->
            let pos = Bin_prot.Write.bin_write_int_8bit buf pos 9 in
            Priority.bin_write_t buf pos v1
      let bin_writer_redefinition bin_writer_a bin_writer_b =
        {
          Bin_prot.Type_class.size =
            (fun v ->
               bin_size_redefinition
                 (bin_writer_a : _ Bin_prot.Type_class.writer).size
                 (bin_writer_b : _ Bin_prot.Type_class.writer).size v);
          Bin_prot.Type_class.write =
            (fun v ->
               bin_write_redefinition
                 (bin_writer_a : _ Bin_prot.Type_class.writer).write
                 (bin_writer_b : _ Bin_prot.Type_class.writer).write v)
        }
      let rec __bin_read_redefinition__ _of__a _of__b _buf pos_ref _vint =
        Bin_prot.Common.raise_variant_wrong_type
          "Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Manifest_equality.fs.redefinition"
          (!pos_ref)
      and bin_read_redefinition _of__a _of__b buf pos_ref =
        match Bin_prot.Read.bin_read_int_8bit buf pos_ref with
        | 0 -> let arg_1 = _of__a buf pos_ref in First arg_1
        | 1 -> let arg_1 = _of__b buf pos_ref in Second arg_1
        | 2 ->
            let arg_1 =
              (Redefinition_generated_1.bin_read_t _of__a _of__b) buf pos_ref in
            Neither arg_1
        | 3 ->
            let arg_1 = Redefinition_generated_2.bin_read_t buf pos_ref in
            Poly arg_1
        | 4 ->
            let arg_1 = (bin_read_redefinition _of__a _of__b) buf pos_ref in
            Recursive arg_1
        | 5 ->
            let arg_1 =
              (Test_project_name.Ppx.Ppx_dotnet_bin_prot.Runtime_lib.Dotnet_bin_prot.Generated_map.bin_read_t
                 bin_read_int64) buf pos_ref in
            Map arg_1
        | 6 ->
            let arg_1 =
              Test_project_name.Ppx.Ppx_dotnet_bin_prot.Runtime_lib.Dotnet_bin_prot.Generated_set.bin_read_t
                buf pos_ref in
            Set arg_1
        | 7 -> let arg_1 = Seq_id.bin_read_t buf pos_ref in Seq_id arg_1
        | 8 -> let arg_1 = Path_id.bin_read_t buf pos_ref in Path_id arg_1
        | 9 ->
            let arg_1 = Priority.bin_read_t buf pos_ref in
            Module_created_with_include arg_1
        | _ ->
            Bin_prot.Common.raise_read_error
              (Bin_prot.Common.ReadError.Sum_tag
                 "Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Manifest_equality.fs.redefinition")
              (!pos_ref)
      let bin_reader_redefinition bin_reader_a bin_reader_b =
        {
          Bin_prot.Type_class.read =
            (fun buf ->
               fun pos_ref ->
                 (bin_read_redefinition
                    (bin_reader_a : _ Bin_prot.Type_class.reader).read
                    (bin_reader_b : _ Bin_prot.Type_class.reader).read) buf pos_ref);
          Bin_prot.Type_class.vtag_read =
            (fun buf ->
               fun pos_ref ->
                 fun vtag ->
                   (__bin_read_redefinition__
                      (bin_reader_a : _ Bin_prot.Type_class.reader).read
                      (bin_reader_b : _ Bin_prot.Type_class.reader).read) buf
                     pos_ref vtag)
        }
      let rec bin_redefinition bin_a bin_b =
        {
          Bin_prot.Type_class.writer =
            (bin_writer_redefinition (bin_a : _ Bin_prot.Type_class.t).writer
               (bin_b : _ Bin_prot.Type_class.t).writer);
          Bin_prot.Type_class.reader =
            (bin_reader_redefinition (bin_a : _ Bin_prot.Type_class.t).reader
               (bin_b : _ Bin_prot.Type_class.t).reader)
        }
      module T = struct
        type ('a, 'b) t = {
          original: ('a, 'b) original ;
          redefinition: ('a, 'b) redefinition }
        let bin_size_t _size_of_a _size_of_b =
          function
          | { original = v1; redefinition = v2 } ->
              let size = 0 in
              let size =
                Bin_prot.Common.(+) size (bin_size_original _size_of_a _size_of_b v1) in
              Bin_prot.Common.(+) size
                (bin_size_redefinition _size_of_a _size_of_b v2)
        let bin_write_t _write_a _write_b buf pos =
          function
          | { original = v1; redefinition = v2 } ->
              let pos = bin_write_original _write_a _write_b buf pos v1 in
              bin_write_redefinition _write_a _write_b buf pos v2
        let bin_writer_t bin_writer_a bin_writer_b =
          {
            Bin_prot.Type_class.size =
              (fun v ->
                 bin_size_t (bin_writer_a : _ Bin_prot.Type_class.writer).size
                   (bin_writer_b : _ Bin_prot.Type_class.writer).size v);
            Bin_prot.Type_class.write =
              (fun v ->
                 bin_write_t (bin_writer_a : _ Bin_prot.Type_class.writer).write
                   (bin_writer_b : _ Bin_prot.Type_class.writer).write v)
          }
        let __bin_read_t__ _of__a _of__b _buf pos_ref _vint =
          Bin_prot.Common.raise_variant_wrong_type
            "Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Manifest_equality.fs.t"
            (!pos_ref)
        let bin_read_t _of__a _of__b buf pos_ref =
          let v_original = (bin_read_original _of__a _of__b) buf pos_ref in
          let v_redefinition = (bin_read_redefinition _of__a _of__b) buf pos_ref in
          { original = v_original; redefinition = v_redefinition }
        let bin_reader_t bin_reader_a bin_reader_b =
          {
            Bin_prot.Type_class.read =
              (fun buf ->
                 fun pos_ref ->
                   (bin_read_t (bin_reader_a : _ Bin_prot.Type_class.reader).read
                      (bin_reader_b : _ Bin_prot.Type_class.reader).read) buf pos_ref);
            Bin_prot.Type_class.vtag_read =
              (fun buf ->
                 fun pos_ref ->
                   fun vtag ->
                     (__bin_read_t__
                        (bin_reader_a : _ Bin_prot.Type_class.reader).read
                        (bin_reader_b : _ Bin_prot.Type_class.reader).read) buf
                       pos_ref vtag)
          }
        let bin_t bin_a bin_b =
          {
            Bin_prot.Type_class.writer =
              (bin_writer_t (bin_a : _ Bin_prot.Type_class.t).writer
                 (bin_b : _ Bin_prot.Type_class.t).writer);
            Bin_prot.Type_class.reader =
              (bin_reader_t (bin_a : _ Bin_prot.Type_class.t).reader
                 (bin_b : _ Bin_prot.Type_class.t).reader)
          }
      end

      Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.With_built_in_override.fs
      -------------------------------
      module Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.With_built_in_override
      open Bin_prot.Write
      open Bin_prot.Read
      open Bin_prot.Size
      module Dependency = struct
        type t =
          | Simple_variant
        let bin_size_t = function | Simple_variant -> 1
        let bin_write_t buf pos =
          function | Simple_variant -> Bin_prot.Write.bin_write_int_8bit buf pos 0
        let bin_writer_t =
          {
            Bin_prot.Type_class.size = bin_size_t;
            Bin_prot.Type_class.write = bin_write_t
          }
        let __bin_read_t__ _buf pos_ref _vint =
          Bin_prot.Common.raise_variant_wrong_type
            "Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.With_built_in_override.fs.t"
            (!pos_ref)
        let bin_read_t buf pos_ref =
          match Bin_prot.Read.bin_read_int_8bit buf pos_ref with
          | 0 -> Simple_variant
          | _ ->
              Bin_prot.Common.raise_read_error
                (Bin_prot.Common.ReadError.Sum_tag
                   "Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.With_built_in_override.fs.t")
                (!pos_ref)
        let bin_reader_t =
          {
            Bin_prot.Type_class.read = bin_read_t;
            Bin_prot.Type_class.vtag_read = __bin_read_t__
          }
        let bin_t =
          {
            Bin_prot.Type_class.writer = bin_writer_t;
            Bin_prot.Type_class.reader = bin_reader_t
          }
      end

      Test_project_name.Ppx.Ppx_dotnet_bin_prot.Runtime_lib.Dotnet_bin_prot.fs
      -------------------------------
      module Test_project_name.Ppx.Ppx_dotnet_bin_prot.Runtime_lib.Dotnet_bin_prot
      open Bin_prot.Write
      open Bin_prot.Read
      open Bin_prot.Size
      module Generated_map = struct
        type 'a t = (string * 'a) list
        let bin_size_t _size_of_a v =
          bin_size_list
            (function
             | (v1, v2) ->
                 let size = 0 in
                 let size = Bin_prot.Common.(+) size (bin_size_string v1) in
                 Bin_prot.Common.(+) size (_size_of_a v2)) v
        let bin_write_t _write_a buf pos v =
          bin_write_list
            (fun buf ->
               fun pos ->
                 function
                 | (v1, v2) ->
                     let pos = bin_write_string buf pos v1 in _write_a buf pos v2)
            buf pos v
        let bin_writer_t bin_writer_a =
          {
            Bin_prot.Type_class.size =
              (fun v ->
                 bin_size_t (bin_writer_a : _ Bin_prot.Type_class.writer).size v);
            Bin_prot.Type_class.write =
              (fun v ->
                 bin_write_t (bin_writer_a : _ Bin_prot.Type_class.writer).write v)
          }
        let __bin_read_t__ _of__a buf pos_ref vint =
          (__bin_read_list__
             (fun buf ->
                fun pos_ref ->
                  let v1 = bin_read_string buf pos_ref in
                  let v2 = _of__a buf pos_ref in (v1, v2))) buf pos_ref vint
        let bin_read_t _of__a buf pos_ref =
          (bin_read_list
             (fun buf ->
                fun pos_ref ->
                  let v1 = bin_read_string buf pos_ref in
                  let v2 = _of__a buf pos_ref in (v1, v2))) buf pos_ref
        let bin_reader_t bin_reader_a =
          {
            Bin_prot.Type_class.read =
              (fun buf ->
                 fun pos_ref ->
                   (bin_read_t (bin_reader_a : _ Bin_prot.Type_class.reader).read)
                     buf pos_ref);
            Bin_prot.Type_class.vtag_read =
              (fun buf ->
                 fun pos_ref ->
                   fun vtag ->
                     (__bin_read_t__
                        (bin_reader_a : _ Bin_prot.Type_class.reader).read) buf
                       pos_ref vtag)
          }
        let bin_t bin_a =
          {
            Bin_prot.Type_class.writer =
              (bin_writer_t (bin_a : _ Bin_prot.Type_class.t).writer);
            Bin_prot.Type_class.reader =
              (bin_reader_t (bin_a : _ Bin_prot.Type_class.t).reader)
          }
      end
      module Generated_set = struct
        type t = string list
        let bin_size_t v = bin_size_list bin_size_string v
        let bin_write_t buf pos v = bin_write_list bin_write_string buf pos v
        let bin_writer_t =
          {
            Bin_prot.Type_class.size = bin_size_t;
            Bin_prot.Type_class.write = bin_write_t
          }
        let __bin_read_t__ buf pos_ref vint =
          (__bin_read_list__ bin_read_string) buf pos_ref vint
        let bin_read_t buf pos_ref = (bin_read_list bin_read_string) buf pos_ref
        let bin_reader_t =
          {
            Bin_prot.Type_class.read = bin_read_t;
            Bin_prot.Type_class.vtag_read = __bin_read_t__
          }
        let bin_t =
          {
            Bin_prot.Type_class.writer = bin_writer_t;
            Bin_prot.Type_class.reader = bin_reader_t
          }
      end

      Test_project_name.fsproj
      -------------------------------

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
                   <Compile Include="Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.With_built_in_override.fs" />
          <Compile Include="Test_project_name.Override.fs" />
          <Compile Include="Test_project_name.Ppx.Ppx_dotnet_bin_prot.Runtime_lib.Dotnet_bin_prot.fs" />
          <Compile Include="Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Manifest_equality.fs" />
          <Compile Include="Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Dotnet_bin_prot_generator_test.fs" />
               </ItemGroup>

               <ItemGroup>
                   <ProjectReference Include="<jenga_root>/lib/dotnet-libs/bin_prot/src/Bin_prot.fsproj" />
               </ItemGroup>

               <Target Name="CleanOutputDirs" AfterTargets="Clean">
               <RemoveDir Directories="$(BaseIntermediateOutputPath)" /><!-- obj -->
               <RemoveDir Directories="$(BaseOutputPath)" /><!-- bin -->
               </Target>
               </Project>

      generated_file_list
      -------------------------------
      Test_project_name.Override.fs
      Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Dotnet_bin_prot_generator_test.fs
      Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Manifest_equality.fs
      Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.With_built_in_override.fs
      Test_project_name.Ppx.Ppx_dotnet_bin_prot.Runtime_lib.Dotnet_bin_prot.fs

      jbuild
      -------------------------------

               (rule (
               (sandbox ignore_targets)
               (targets (
               jbuild
               generated_file_list
               Test_project_name.fsproj
               Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.With_built_in_override.fs
          Test_project_name.Override.fs
          Test_project_name.Ppx.Ppx_dotnet_bin_prot.Runtime_lib.Dotnet_bin_prot.fs
          Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Manifest_equality.fs
          Test_project_name.Ppx.Ppx_dotnet_bin_prot.Generator.Test.Dotnet_bin_prot_generator_test.fs))
               (deps (
               true
          <TMPDIR>/overrides/random_file_name_that_is_different.fs
               ))
               (action "true ")))

               (alias
               ((name DEFAULT)
               (deps (
               generated_file_list
               (glob_files {*.fs,*.fsi})))
               (action "diff <(ls | grep '\.fsi\?$') generated_file_list")))


               (alias (
               ;; We are only allowed to build dotnet on hydra in feature-subtree-build
               (name feature-subtree-build)
               (sandbox ignore_targets)
               (deps (
               %{root}/lib/dotnet/runner/bin/dotnet_runner.exe
               %{root}/lib/dotnet-libs/nuget.config
               additional-path/nuget.config
               (glob_files *.{fsproj,fs,fsi,sln})
               (Files_recursively_in_including_buildable
                %{root}/lib/dotnet-libs
                (glob *.{fsproj,fs,fsi,sln}))))
               (action "%{first_dep} build .")))

               (enforce_style ((sexp_styler ((enabled false)))))

      -------------------------------
      Ignoring non regular file: overrides
      ------------------------------- |}];
    Dotnet_runner.build ~should_pack:true V6_0 ~project_directory:tmp_dir
    |> Deferred.Or_error.ok_exn)
;;
