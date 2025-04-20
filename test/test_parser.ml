open Po_menhir

let fixtures_path = "/home/dalpi/po_menhir/test/fixtures/"

let%test_unit "test parser" =
  Array.iter
    (fun file -> Driver.parse_file (fixtures_path ^ file) |> ignore)
    (Sys.readdir fixtures_path)
