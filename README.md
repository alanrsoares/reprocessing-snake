Reprocessing Snake
---

Snake Game written entirely in ReasonML using Reprocessing

Builds to both Web and OpenGL

![screenshot](/assets/screenshot.png)

## How to
```
git clone https://github.com/alanrsoares/reprocessing-snake.git
```

### Install
```
[npm | yarn] install
```

### Build
```
[npm | yarn] run build
```

### Start
```
[npm | yarn] start
```

To build to JS run `npm run build:web` and then run a static server, like `python -m SimpleHTTPServer` and go to `localhost:8000`. If you're using safari you can simply open the `index.html` and tick `Develop > Disable Cross-Origin Restrictions`.

To build to native run `npm run build:native` and run `npm run start:native`

The build system used is [bsb-native](https://github.com/bsansouci/bsb-native).
