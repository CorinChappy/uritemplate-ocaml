let json = {|
{
  "Level 1 Examples": {
    "level": 1,
    "variables": {
      "var": "value",
      "hello": "Hello World!"
    },
    "testcases": [
      [
        "{var}",
        "value"
      ],
      [
        "{hello}",
        "Hello%20World%21"
      ]
    ]
  },
  "Level 2 Examples": {
    "level": 2,
    "variables": {
      "var": "value",
      "hello": "Hello World!",
      "path": "/foo/bar"
    },
    "testcases": [
      [
        "{+var}",
        "value"
      ],
      [
        "{+hello}",
        "Hello%20World!"
      ],
      [
        "{+path}/here",
        "/foo/bar/here"
      ],
      [
        "here?ref={+path}",
        "here?ref=/foo/bar"
      ]
    ]
  },
  "Level 3 Examples" : {
      "level": 3,
      "variables": {
        "var"   : "value",
        "hello" : "Hello World!",
        "empty" : "",
        "path"  : "/foo/bar",
        "x"     : "1024",
        "y"     : "768"
      },
      "testcases" : [
          ["map?{x,y}", "map?1024,768"],
          ["{x,hello,y}", "1024,Hello%20World%21,768"],
          ["{+x,hello,y}", "1024,Hello%20World!,768"],
          ["{+path,x}/here", "/foo/bar,1024/here"],
          ["{#x,hello,y}", "#1024,Hello%20World!,768"],
          ["{#path,x}/here", "#/foo/bar,1024/here"],
          ["X{.var}", "X.value"],
          ["X{.x,y}", "X.1024.768"],
          ["{/var}", "/value"],
          ["{/var,x}/here", "/value/1024/here"],
          ["{;x,y}", ";x=1024;y=768"],
          ["{;x,y,empty}", ";x=1024;y=768;empty"],
          ["{?x,y}", "?x=1024&y=768"],
          ["{?x,y,empty}", "?x=1024&y=768&empty="],
          ["?fixed=yes{&x}", "?fixed=yes&x=1024"],
          ["{&x,y,empty}", "&x=1024&y=768&empty="]
      ]
    }
  }
|}
