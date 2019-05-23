uritemplate-ocaml
===============

Work in progress implementation of URI templates for OCaml. (RFC6570 - http://tools.ietf.org/html/rfc6570)

**Currently compliant to level 4**

ODoc documentation avaliable [here](https://corinchappy.github.io/uritemplate-ocaml/).

## Install via opam
Easiest way to install is via [opam](https://opam.ocaml.org/packages/uritemplate/):
```bash
$ opam install uritemplate
```

## Usage

```ocaml
# #require "uritemplate";;

# Uritemplate.template_uri
    ~template:"https://example.com{/a,b}{?b}{#e,f}"
    ~variables:[("a", `String "a");
              ("b", `List ["b"; "c"]);
              ("e", `String "e");
              ("f", `String "f")];;
- : string = "https://example.com/a/b,c?b=b,c#e,f"
```
