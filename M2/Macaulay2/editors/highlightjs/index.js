import hljs from 'highlight.js/lib/core';
import macaulay2 from './macaulay2.js';
hljs.registerLanguage('macaulay2', macaulay2);
hljs.highlightAll();
