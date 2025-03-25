open Po_menhir

let test_directory = "/home/dalpi/po_menhir/test/"

let%expect_test "test_1" = Driver.parse_file (test_directory ^ "example.po") |> ignore;[%expect {||}]
