import cgi
import os
import re
import string

import test

Suffix = {
    "cpp": ".cpp",
    "csharp": ".cs",
    "erlang": ".erl",
    "java": ".java",
    "javascript": ".js",
    "lua": ".lua",
    "perl": ".pl",
    "php": ".php",
    "python": ".py",
    "ruby": ".rb",
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
        f.write(template(file="index.template", vars={"languages": languages}))
    for language in languages:
        files = [x for x in os.listdir(language) if x.endswith(Suffix[language]) and not x.startswith("test")]
        with open(os.path.join(language, "index.html"), "w") as f:
            f.write(template(file="language.template", vars={
                "language": language,
                "functions": files,
            }))
        for fn in files:
            with open(os.path.join(language, fn + ".html"), "w") as f:
                f.write(template(file="function.template", vars={
                    "file": fn,
                    "code": open(os.path.join(language, fn)).read(),
                }))

if __name__ == "__main__":
    main()
