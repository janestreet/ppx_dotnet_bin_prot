open! Core
open! Import

module String = struct
  include String.Stable.V1

  module Map = struct
    include String.Stable.V1.Map

    let dotnet_bin_prot_t =
      Dotnet_bin_prot.make_iterable1
        ~t_for_key:dotnet_bin_prot_string
        ~iterable_name:"map"
    ;;
  end

  module Set = struct
    include String.Stable.V1.Set

    let dotnet_bin_prot_t =
      Dotnet_bin_prot.make_iterable ~t_for_key:dotnet_bin_prot_string ~iterable_name:"set"
    ;;
  end
end

module Unique_id (X : sig
    val source_code_position : Source_code_position.t
    val module_path : string list
  end)
    () =
struct
  type t = int [@@deriving bin_io, dotnet_bin_prot]

  let dotnet_bin_prot_t =
    Dotnet_bin_prot.create_alias
      dotnet_bin_prot_t
      X.source_code_position
      ~module_path:X.module_path
  ;;
end

module Priority = struct
  include Int

  [%%dotnet_bin_prot.alias type t = int]
end

module Seq_id =
  Unique_id
    (struct
      let source_code_position = [%here]
      let module_path = [ "Seq_id" ]
    end)
    ()

module Path_id =
  Unique_id
    (struct
      let source_code_position = [%here]
      let module_path = [ "Path_id" ]
    end)
    ()

type ('a, 'b) original =
  | First of 'a
  | Second of 'b
  | Neither of
      { field : unit
      ; poly : 'a * 'b
      ; nested : [ `nested ]
      }
  | Poly of [ `test ]
  | Recursive of ('a, 'b) original
  | Map of int String.Map.t
  | Set of String.Set.t
  | Seq_id of Seq_id.t
  | Path_id of Path_id.t
  | Module_created_with_include of Priority.t
[@@deriving bin_io, dotnet_bin_prot]

type ('a, 'b) redefinition = ('a, 'b) original =
  | First of 'a
  | Second of 'b
  | Neither of
      { field : unit
      ; poly : 'a * 'b
      ; nested : [ `nested ]
      }
  | Poly of [ `test ]
  | Recursive of ('a, 'b) redefinition
  | Map of int String.Map.t
  | Set of String.Set.t
  | Seq_id of Seq_id.t
  | Path_id of Path_id.t
  | Module_created_with_include of Priority.t
[@@deriving bin_io, dotnet_bin_prot]

type ('a, 'b) t =
  { original : ('a, 'b) original
  ; redefinition : ('a, 'b) redefinition
  }
[@@deriving bin_io, dotnet_bin_prot]
