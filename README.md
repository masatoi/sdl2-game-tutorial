# sdl2-game-tutorial

## Preparation

### Install SDL2 Library

```
$ sudo apt install libsdl2-dev
$ sudo apt install libsdl2-gfx-dev
$ sudo apt install libsdl2-image-dev
$ sudo apt install libsdl2-mixer-dev
$ sudo apt install libsdl2-ttf-dev
```

### Install SDL2 Wrapper for Common Lisp

```
$ ros install lispgames/cl-sdl2
$ ros install Zulu-Inuoe/cl-sdl2-gfx
```

### Install libffi

```
$ sudo apt install libffi-dev
```

### Install dependent files

```
$ ros install
```

## Usage

```
(ql:quickload :sdl2-game-tutorial)
```

### 01 ウィンドウ表示

```
(sdl2-game-tutorial:start :id 1)
```

![01](https://github.com/fireflower0/sdl2-game-tutorial/blob/master/img/01.png)

### 02 PNG画像表示

```
(sdl2-game-tutorial:start :id 2)
```

![02](https://github.com/fireflower0/sdl2-game-tutorial/blob/master/img/02.png)

### 03 テキスト表示

```
(sdl2-game-tutorial:start :id 3)
```

![03](https://github.com/fireflower0/sdl2-game-tutorial/blob/master/img/03.png)

### 04 2Dレンダリング

```
(sdl2-game-tutorial:start :id 4)
```

![04](https://github.com/fireflower0/sdl2-game-tutorial/blob/master/img/04.png)

### 05 キー入力

```
(sdl2-game-tutorial:start :id 5)
```

![05](https://github.com/fireflower0/sdl2-game-tutorial/blob/master/img/05.png)

### 06 画像移動

```
(sdl2-game-tutorial:start :id 6)
```

![06](https://github.com/fireflower0/sdl2-game-tutorial/blob/master/img/06.png)

### 07 システムウィンドウ表示

```
(sdl2-game-tutorial:start :id 7)
```

![07](https://github.com/fireflower0/sdl2-game-tutorial/blob/master/img/07.png)

### 08 FPSタイマー

```
(sdl2-game-tutorial:start :id 8)
```

![08](https://github.com/fireflower0/sdl2-game-tutorial/blob/master/img/08.png)

### 09 アニメーション

```
(sdl2-game-tutorial:start :id 9)
```

![09](https://github.com/fireflower0/sdl2-game-tutorial/blob/master/img/09.png)

### 10 メッセージウィンドウ

```
(sdl2-game-tutorial:start :id 10)
```

![10](https://github.com/fireflower0/sdl2-game-tutorial/blob/master/img/10.png)

### 11 キャラクター操作

```
(sdl2-game-tutorial:start :id 11)
```

![11](https://github.com/fireflower0/sdl2-game-tutorial/blob/master/img/11.png)

### 12 選択肢ウィンドウ

```
(sdl2-game-tutorial:start :id 12)
```

![12](https://github.com/fireflower0/sdl2-game-tutorial/blob/master/img/12.png)

### 13 通行制限

```
(sdl2-game-tutorial:start :id 13)
```

![13](https://github.com/fireflower0/sdl2-game-tutorial/blob/master/img/13.png)
