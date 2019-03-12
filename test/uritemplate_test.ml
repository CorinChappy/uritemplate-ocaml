open OUnit2

open Uritemplate

let assert_string_equal = assert_equal ~printer:(fun a -> a)


let test_list_of_examples ~variables ~cases =
  List.map
    (fun (template, expected) ->
       template >:: (fun _ -> assert_string_equal expected (template_uri ~template ~variables))
    ) cases


(* Test Fixture *)
let test_fixture = "UriTemplate" >::: [
    "README samples" >::: [
      test_case (fun _ ->
          assert_string_equal
            "https://example.com/a/b?b=b#e,f"
            (template_uri ~template:"https://example.com{/a,b}{?b}{#e,f}" ~variables:[("a", "a"); ("b", "b"); ("e", "e"); ("f", "f")])
        )
    ];

    "Example tests" >::: (
      let open Example_tests_t in
      List.map (fun (name, { variables; testcases; _ }) ->
          name >::: test_list_of_examples ~variables ~cases:testcases
        ) (Example_tests_j.file_of_string Samples.json)
    )
  ]


let _ = run_test_tt_main test_fixture
