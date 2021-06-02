open EzAPIServerUtils

module MakeRegisterer(S: module type of Services)(H:module type of Handlers) = struct

  let register s h dir =
    let h a _ b = h a b in
    register s h dir

  let register dir =
    dir
    |> register S.version H.version
    |> register S.package_entries H.package_entries
    |> register S.library_entries H.library_entries
    |> register S.meta_entries H.meta_entries
    |> register S.module_entries H.module_entries
    |> register S.source_entries H.source_entries
end

module R = MakeRegisterer(Services)(Handlers)

let services =
  empty |> R.register
