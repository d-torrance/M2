Macaulay2 in highlight.js
=========================

[highlight.js](https://highlightjs.org/) is a JavaScript syntax highlighter
with language auto-detection and zero dependencies.

Building
--------

You will need [node](https://nodejs.org/) and [git](https://git-scm.com/)
installed.  (And of course, Macaulay2!)

First, clone the highlight.js repository:

```
git clone https://github.com/highlightjs/highlight.js
cd highlight.js
npm install
```

Next, generate the Macaulay2 language file:

```
mkdir -p extra/highlightjs-macaulay2/src/languages
M2 --script <path-to-M2-source>/M2/Macaulay2/editors/make-M2-symbols.m2
cp highlightjs/macaulay2.js extra/highlightjs-macaulay2/src/languages
```

Finally, build:

```
node ./tools/build.js -t cdn
```

The generated files should now be in the `build` directory.  In particular,
you should take note of the paths to:

* `build/highlight.min.js`
* `build/languages/macaulay2.min.js`
* `build/styles/default.min.css` (or any of the other styles in `build/styles`)

Using
-----

In your html file, include the following, adjusting the paths to the css and
js files as necessary:

```html
<link rel="stylesheet" href="default.min.css">
<script src="highlight.min.js"></script>
<script src="macaulay2.min.js"></script>
<script type="text/javascript">
  hljs.highlightAll();
</script>
```

Then include your Macaulay2 code:

```html
<pre>
<code class="language-macaulay2">
-- Macaulay2 code goes here
</code>
</pre>
```
