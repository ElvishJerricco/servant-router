servant-router
---

`servant-router` routes a URI given a Servant API and an appropriate
handler. In web applications, this is used to make single page
applications (SPAs) with front-end routing, letting you share portions
of your Servant APIs between the client and server.

`servant-router` does not depend on `reflex` or any GHCJS packages.
It's intended to be a general purpose URI router on any platform.
Combined with `reflex-dom`, `servant-reflex`, and
`reflex-dom-contrib`, this makes for a very satisfactory front-end
Haskell experience.

You can see examples of using `servant-router` on both the frontend
and the backend in the `examples` directory.
