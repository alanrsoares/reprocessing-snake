#!/bin

const fs = require("fs");
const path = require("path");

const MODULES = ["unix", "dynlink", "bigarray"];

function replaceInFile(filePath, search, replace) {
  const file = fs.readFileSync(filePath, { encoding: "utf-8" });
  const newContent = file.replace(search, replace);

  fs.writeFileSync(filePath, newContent);

  return newContent;
}

MODULES.forEach((moduleName) => {
  console.log(`Patching module "${moduleName}"`);
  const filePath = path.join(
    __dirname,
    "..",
    "node_modules",
    moduleName,
    "bsconfig.json"
  );

  replaceInFile(filePath, "otherlibs", moduleName);
});
