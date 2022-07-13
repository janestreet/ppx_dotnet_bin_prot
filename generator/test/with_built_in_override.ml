open! Core
open! Async
open! Import

type built_in = string [@@deriving bin_io]

let dotnet_bin_prot_built_in =
  Dotnet_bin_prot.declare_override_exn
    ~type_name:"built_in"
    ~dependencies_with_idents:[]
    [%here]
;;

let built_in_override ~tmp_dir:(_ : string) =
  return (Dotnet_bin_prot_generator.Override.Built_in "string")
;;

module Dependency = struct
  type t = Simple_variant [@@deriving bin_io, dotnet_bin_prot]
end

type with_dependency = { dependency : Dependency.t } [@@deriving bin_io]
