open! Core
open! Async
open! Import

module Override : sig
  type t =
    | Built_in of string
    | Source_file of
        { file_path : string
        ; module_name : string
        ; type_name : string
        }
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

val create_self_generating_rule
  :  project_name:string
  -> generated_files:string list
  -> executable:string
  -> subcommands:string list
  -> overrides:Override.t list
  -> string

val create
  :  ?additional_nuget_config_path:string
  -> unit
  -> types:'a list
  -> project_name:string
  -> output_dir:string
  -> additional_jbuild_entry:(generated_files:string list -> string option)
  -> overrides:(Dotnet_bin_prot.t, Override.t) List.Assoc.t
  -> 'a t

val generate_f_sharp_types : Dotnet_bin_prot.t t -> unit Or_error.t Deferred.t

val generate_f_sharp_types_of_modules
  :  (module Dotnet_bin_prot.Dotnet_bin_prot_able) t
  -> unit Or_error.t Deferred.t


(** Generate multiple projects. If there are interdependencies the input list must be
    topologically sorted. *)
val generate_f_sharp_types_with_multiple_projects
  :  Dotnet_bin_prot.t t list
  -> unit Or_error.t Deferred.t
