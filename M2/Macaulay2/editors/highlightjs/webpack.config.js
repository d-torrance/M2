const path = require('path');

module.exports = {
  entry: './index.js',
  output: {
    filename: 'highlight.js',
    path: path.resolve(__dirname, '')
  }
};
