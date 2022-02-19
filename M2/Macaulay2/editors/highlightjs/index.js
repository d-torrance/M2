import hljs from 'highlight.js/lib/core';
import macaulay2 from './macaulay2.js';
import 'highlight.js/styles/default.css';
hljs.registerLanguage('macaulay2', macaulay2);
hljs.highlightAll();
