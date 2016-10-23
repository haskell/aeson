var fs = require('fs');
var path = process.argv[2];

try {
	var data = fs.readFileSync(path);
	var json = JSON.parse(data);
	//console.log(json)
} catch (e) {
	console.log("--", e.message)
	process.exit(1);
}

process.exit(0);
