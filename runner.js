const path = require('path');

const moduleName = process.argv[2];

const { main } = require(path.resolve('output', moduleName, 'index.js'));

main();
