open EzAPIServerUtils

module MakeRegisterer(S: module type of Services)(H:module type of Handlers) = struct

  let register s h dir =
    let h a _ b = h a b in
    register s h dir

  let register dir =
    dir
    |> register S.entries H.entries
    |> register S.elements H.elements
    |> register S.exec_command H.exec_command
    |> register S.search H.search
end

module R = MakeRegisterer(Services)(Handlers)

let services =
  empty |> R.register
