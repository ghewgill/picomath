import cgi
import os
import re
import string

import test

Languages = {
    "cpp":          {"name": "C++",         "suffix": ".cpp"},
    "csharp":       {"name": "C#",          "suffix": ".cs"},
    "erlang":       {"name": "Erlang",      "suffix": ".erl"},
    "java":         {"name": "Java",        "suffix": ".java"},
    "javascript":   {"name": "Javascript",  "suffix": ".js"},
    "lua":          {"name": "Lua",         "suffix": ".lua"},
    "perl":         {"name": "Perl",        "suffix": ".pl"},
    "php":          {"name": "PHP",         "suffix": ".php"},
    "python":       {"name": "Python",      "suffix": ".py"},
    "ruby":         {"name": "Ruby",        "suffix": ".rb"},
    "tcl":          {"name": "Tcl",         "suffix": ".tcl"},
}

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
        m = re.match(r"\s*@iter\s+(\w+)@$", s)
        if m is not None:
            it = m.group(1)
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
                "name": Languages[language]["name"],
                "functions": files,
            }))
        for fn in files:
            with open(os.path.join(language, fn + ".html"), "w") as f:
                f.write(template(file="function.template", vars={
                    "name": Languages[language]["name"],
                    "file": fn,
                    "extension": Languages[language]["suffix"][1:],
                    "code": open(os.path.join(language, fn)).read(),
                }))

if __name__ == "__main__":
    main()
