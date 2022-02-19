const path = require('path');

module.exports = {
  mode: 'production',
  entry: './index.js',
  output: {
    filename: 'highlight.js',
    path: path.resolve(__dirname, '')
  }
};
