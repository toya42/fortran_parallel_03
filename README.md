# fortran_parallel_03

1/15 に行う並列計算勉強会の資料

1 次元配列の要素ごとの操作を行うテストを`one_dimensional`ディレクトリに
2 次元配列を用いるテストを`two_dimensional`ディレクトリに

## コンパイル&実行方法

`one_dimensional`あるいは`two_dimensional`ディレクトリで

```bash
$bash execution.sh
```

とすると実行されます

## 実行時間計測

`omp_get_wtime`を用います

## コンパイラオプション

s

## one_dimensional

src/one_dimensional
一次元配列の要素ごとの和・積・商などを求めています
配列の宣言位置や，変数の型による計算時間の違いを確認します
src/two_dimensional
d

## two_dimensional

src/two_dimensional
二次元配列で隣接データを参照するような（配列構文では書きにくい？）問題
とりあえず，次の二つを実装しました

### 4 点ステンシルの jacobi 法

### Roe の近似リーマン解法
