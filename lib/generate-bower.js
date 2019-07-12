/* Generates a bower.config file with dependencies copied from spago */
// In awk - spago list-packages -f direct | awk '{print "\"purescript-"$1"\": \"^"substr($2,2)"\""}'",
const fs = require('fs');
const {execSync} = require('child_process');

// Get spago package information
let spagoPackagesBuffer = execSync('spago list-packages -f direct');
let spagoPackagesString = `${spagoPackagesBuffer}`;

// Construct bower specific syntax
let bowerPackages =
    spagoPackagesString
      .replace(/([A-Za-z0-9_-]+)[ ]*v([0-9.]+).*/g, '"purescript-$1": "^$2",')
      // Remove trailing newline and comma
      .slice(0,-2);

// Read bower file, replace dependency section
fs.readFile('bower.json', 'utf8', function(err, contents) {
  if(err) {
      return console.error(err);
  }
  let rawContents = contents.replace(/("dependencies"[ ]*:[ ]*{)([\s\w"-:^]*)}/, "$1"+bowerPackages+"}");
  let newContents = JSON.stringify(JSON.parse(rawContents), null, 2);
  fs.writeFile('bower.json', newContents, function() {
    if(err) {
      return console.error(err);
    }
  });
});
