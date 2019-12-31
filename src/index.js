import { Elm } from './Main.elm'

window.addEventListener('load', function () {
  const app =
    Elm.Main.init({ node: document.getElementById('elm') })
})
