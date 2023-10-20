// Necessary because the corresponding purescript-web-cssom-view package cannot be 
// used since it relies on the implementation specific representaion of purescript 
// values in JS. :(
// See: https://github.com/purescript-web/purescript-web-cssom-view/issues/7
export const scrollIntoView = element => () => 
  element.scrollIntoView({ beaviour: "instant", block: "nearest", inline: "nearest" })

