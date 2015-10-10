# path-string

Path string utility library based on nodejs' [path](https://nodejs.org/api/path.html).

The windows-compatible version has yet to be implemented.

## Examples

```lisp
(path-string:normalize "/foo/bar//baz/asdf/quux/..")

;; => "/foo/bar/baz/asdf"

(path-string:join "/foo" "bar" "baz/asdf" "quux" "..")

;; => "/foo/bar/baz/asdf"

(path-string:resolve "foo/bar" "/tmp/file/" ".." "a/../subfile")

;; => "/tmp/subfile"

(path-string:absolute-p "/tmp/file")

;; => #P"/tmp/file"

(path-string:relative "/data/orandea/test/aaa" "/data/orandea/impl/bbb")

;; => "../../impl/bbb"

(path-string:dirname "/foo/bar/baz/asdf/quux")

;; => "/foo/bar/baz/asdf"

(path-string:basename "/foo/bar/baz/asdf/quux.html")

;; => "quux.html"

(path-string:extname "index.html")

;; => ".html"

(path-string:parse "/home/user/dir/file.txt")

;; => "/"
;;    "/home/user/dir"
;;    "file.txt"
;;    ".txt"
::    "file"
```

## License

MIT
