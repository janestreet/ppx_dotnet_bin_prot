open! Base
open! Ppxlib

let str_type_decl =
  Deriving.Generator.make_noarg (fun ~loc ~path (rec_flag, tds) ->
    [ Ppx_dotnet_bin_prot_expander.generate_dotnet_bin_prot_value
        ~loc
        ~path
        ~rec_flag
        ~tds
        ~module_path_override:None
    ])
;;

let sig_type_decl =
  Deriving.Generator.make_noarg (fun ~loc ~path:(_ : string) ((_ : rec_flag), tds) ->
    Ppx_dotnet_bin_prot_expander.generate_dotnet_bin_prot_signature ~loc ~tds)
;;

let dotnet_bin_prot = Deriving.add "dotnet_bin_prot" ~str_type_decl ~sig_type_decl
