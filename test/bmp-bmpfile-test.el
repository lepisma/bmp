(require 'bmp-bmpfile)

(describe "Version string check"
  (it "works on empty input"
    (expect (bmp-bmpfile-version-strs-consistent? '())))
  (it "works with regular cases"
    (expect (bmp-bmpfile-version-strs-consistent? '("12" "12" "12")))
    (expect (bmp-bmpfile-version-strs-consistent? '("0.0.0" "0.0.0" "0.0.0"))))
  (it "works for nulls"
    (expect (not (bmp-bmpfile-version-strs-consistent? '("hello" "hello" nil)))))
  (it "works for differences"
    (expect (not (bmp-bmpfile-version-strs-consistent? '("hello" "world" "world"))))
    (expect (not (bmp-bmpfile-version-strs-consistent? '("hello" "world" "world" nil))))))
