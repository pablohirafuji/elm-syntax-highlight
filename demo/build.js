const { exec  } = require('node:child_process');
const fs = require('fs');

const build = () => {
  exec('npx elm make src/Main.elm --optimize --output build/index.js', (error, stdout, stderr) => {
    if (error) throw error;
    console.log(stdout);
    console.error(stderr);

    fs.copyFile('index.html', 'build/index.html', (error) => {
      if (error) throw error;
      exec('npx uglifyjs build/index.js --compress "pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe" | npx uglifyjs --mangle --output build/index.js', (error, stdout, stderr) => {
        if (error) throw error;
        console.log(stdout);
        console.error(stderr);
      });
    });
  });
}

build();