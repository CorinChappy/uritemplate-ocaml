open OUnit2

open Uritemplate

let test_fixture = "UriTemplate.Template" >::: [
    "get_variable_names" >::: [
      test_case (fun _ ->
          let template = Template.empty
                         |> Template.add_literal "https://example.com"
                         |> Template.add_expression
                           Expansion_type.PathParameter
                           [Template.create_variable_expression "a";
                            Template.create_variable_expression "b";]
                         |> Template.add_expression
                           Expansion_type.Fragment
                           [Template.create_variable_expression "e";
                            Template.create_variable_expression "f";]

          in
          assert_equal
            ~cmp:(List.for_all2 (=))
            ~printer:(List.fold_left (fun s a -> s ^ " " ^ a) "")
            ["a"; "b"; "e"; "f"]
            (Template.get_variable_names template)
        )
    ];
  ]
