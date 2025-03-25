open Po_menhir

let fixtures_path = "/home/dalpi/po_menhir/test/fixtures/"

let%expect_test "test_1" = Driver.parse_file (fixtures_path ^ "example.po") |> ignore;[%expect {||}]
