let run = () => {
  let foo = Reprocessing.hotreload(~screen="src/index.res")
  Js.log(foo)
}
run()
