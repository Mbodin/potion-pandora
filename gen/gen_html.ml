open Tyxml

let js =
  String.concat "\n" [
    "/* @license https://creativecommons.org/licenses/by/4.0/ CC-BY-4.0 */" ;
    "/* The source code of this compiled program is available at https://github.com/Mbodin/potion-pandora/ */" ;
    In_channel.(input_all stdin) ;
    "/* @license-end */" ;
    "//# sourceURL=main.js" ;
    ""
  ]


let%html html = {|
  <html>
    <head>
      <meta charset = "utf-8" />
      <title>Potion Pandora</title>
      <style>
body {
  background-color: #201127 ;
}

canvas {
  image-rendering: pixelated ;
  object-fit: contain ;
}
      </style>
    </head>
    <body>
      <noscript>
        Cette page nécessite JavaScript pour fonctionner.
        Son code source est disponible <a href = "https://github.com/Mbodin/potion-pandora/">ici</a>.
      </noscript>
      <script>|} (Html.Unsafe.data js) {|</script>
    </body>
  </html>
|}

let () =
  Format.printf "%a@." (Html.pp ~indent:false ()) html

