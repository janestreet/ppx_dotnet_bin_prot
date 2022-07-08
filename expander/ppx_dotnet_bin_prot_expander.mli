open! Base
open! Ppxlib

val generate_dotnet_bin_prot_value
  :  loc:Location.t
  -> path:string
  -> rec_flag:rec_flag
  -> tds:type_declaration list
  -> module_path_override:expression option
  -> structure_item

val generate_dotnet_bin_prot_signature
  :  loc:Location.t
  -> tds:type_declaration list
  -> signature
