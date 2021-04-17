# Inrupt-solid-client
A rollup.config for creating a js bundle for Inrupt's solid-client-js

In html document header include:

```html
<script defer language="javascript" src="path_to/solid-client.bundle.js"></script>
```
The API is exposed to the browser through `SolidClient`.  For example,

```js
SolidClient.createThing();
```
