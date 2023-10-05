const fs = require('fs');

fs.readFile('tests/doubles.json', 'utf8', (err, data) => {
  if (err) {
    console.error(err);
    return;
  }

  var json = JSON.parse(data);
  var newd = JSON.stringify(json)
  console.log(data === newd);
});
