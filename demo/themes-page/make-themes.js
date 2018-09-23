var fs = require("fs");
var path = require("path");
var templateFile = fs.readFileSync(path.resolve(__dirname + "/themes-template.html"));
var templateStr = templateFile.toString();


var elm = require("./elm-themes").Elm.Main.init();
elm.ports.themesList.subscribe(function(themeList){
	var themeListHtml = ""
	themeList.forEach(function(element, index, array) {
    	themeListHtml += "<li><details><summary>" + element.name + "</summary><pre class=\"elmsh\"><code>"+ element.content +"</code></pre></details></li>"
	});

	var themesHtml = templateStr.replace("${themeList}", themeListHtml)

	fs.writeFile("./themes.html", themesHtml, function(err) {
		if(err) { return console.log(err); }
	});
});
