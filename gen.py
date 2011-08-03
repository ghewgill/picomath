import cgi
import os
import re
import string
import sys

import pygments
import pygments.lexers
import pygments.formatters

import test

Languages = {
    "ada":          {"name": "Ada",                     "suffix": ".adb"},
    "cpp":          {"name": "C++",                     "suffix": ".cpp"},
    "csharp":       {"name": "C#",                      "suffix": ".cs"},
    "erlang":       {"name": "Erlang",                  "suffix": ".erl"},
    "go":           {"name": "Go",                      "suffix": ".go"},
    "haskell":      {"name": "Haskell",                 "suffix": ".hs"},
    "java":         {"name": "Java",                    "suffix": ".java"},
    "javascript":   {"name": "Javascript",              "suffix": ".js"},
    "lua":          {"name": "Lua",                     "suffix": ".lua"},
    "pascal":       {"name": "Pascal",                  "suffix": ".pas"},
    "perl":         {"name": "Perl",                    "suffix": ".pl"},
    "php":          {"name": "PHP",                     "suffix": ".php"},
    "python":       {"name": "Python (2.x and 3.x)",    "suffix": ".py"},
    "ruby":         {"name": "Ruby",                    "suffix": ".rb"},
    "scheme":       {"name": "Scheme",                  "suffix": ".scm"},
    "tcl":          {"name": "Tcl",                     "suffix": ".tcl"},
}

class UnescapedString:
    def __init__(self, s):
        self.s = s
    def __str__(self):
        return self.s

def template(file=None, text=None, vars=None):
    """Super quick and dirty template function"""
    if file is not None:
        with open(file) as f:
            text = f.read()
    escapedvars = {x: cgi.escape(y) if isinstance(y, str) else y for x, y in vars.items()}
    r = ""
    a = text.split("\n")
    i = 0
    while i < len(a):
        s = a[i]
        m = re.match(r"\s*@(\w+)\s+(.+?)@$", s)
        if m is not None:
            cmd = m.group(1)
            args = m.group(2)
            if cmd == "iter":
                it = args
                j = i + 1
                while j < len(a) and not re.match(r"\w*@end@$", a[j]):
                    j += 1
                for x in vars[it]:
                    if isinstance(x, dict):
                        d = {"_": x}
                        d.update(x)
                        r += template(text="\n".join(a[i+1:j]), vars=d)
                    else:
                        r += template(text="\n".join(a[i+1:j]), vars={"_": x})
                i = j + 1
            elif cmd == "include":
                i += 1
                try:
                    r += open(string.Template(args).substitute(escapedvars)).read()
                except Exception as e:
                    if not isinstance(e, IOError) or e.errno != 2:
                        print(e)
            else:
                print("Unknown directive: {0}".format(cmd), file=sys.stderr)
                sys.exit(1)
        else:
            t = string.Template(s)
            r += t.substitute(escapedvars) + "\n"
            i += 1
    return r

def main():
    languages = sorted(test.Languages.keys())
    with open("index.html", "w") as f:
        f.write(template(file="index.template", vars={"languages": [dict(Languages[x].items() | {"dir": x}.items()) for x in languages]}))
    for language in languages:
        files = [x for x in os.listdir(language) if x.endswith(Languages[language]["suffix"]) and not x.startswith("test")]
        with open(os.path.join(language, "index.html"), "w") as f:
            f.write(template(file="language.template", vars={
                "lang": language,
                "name": Languages[language]["name"],
                "functions": files,
            }))
        for fn in files:
            with open(os.path.join(language, fn + ".html"), "w") as f:
                code = open(os.path.join(language, fn)).read()
                highlighted = pygments.highlight(
                    code,
                    pygments.lexers.get_lexer_by_name(language),
                    pygments.formatters.HtmlFormatter()
                )
                f.write(template(file="function.template", vars={
                    "name": Languages[language]["name"],
                    "file": fn,
                    "extension": Languages[language]["suffix"][1:],
                    "code": UnescapedString(highlighted),
                    "style": pygments.formatters.HtmlFormatter().get_style_defs(".highlight"),
                }))

if __name__ == "__main__":
    main()
