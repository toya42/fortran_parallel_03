---
marp: true
theme: gaia
size: 16:9
backgroundColor: #fff
math: katex
---

<!-- _class: lead -->

# 第三回並列計算勉強会

## ~ do concurrent によるスレッド並列化 2 ~

## gfortran を使用する場合の実用性など

### toya42, 2022.1.15

---

<!-- paginate: true -->
<!-- footer: 第三回並列計算勉強会 (2022.1.15)  -->

# この勉強会の目的

- モダン Fortran の機能を活かした並列計算について学ぶ
  - `do concurrent`, `co-array` を中心に
  - 将来的にはコンパイラ，ライブラリ依存が少ない（はず）
  - ソースコードの寿命が長い（はず）
- 数値計算プログラムの高速化技法についての知見を共有する
- モダン Fortran プログラムのチューニングの手引きを作成する
  - コーディングスタイルガイドへ

---

# do concurrent, co-array への期待

- Fortran の長所：**ほどほど**のコーディングで**ある程度**の性能が出る
- ハイブリッド並列化は，もはや必須
  - OpenMP はともかく，OpenACC や MPI はそれなりに面倒（個人の感想）

$\rightarrow$ `do concurrent`, `co-array` は，負担を軽減してくれるのか?

---

# 今回の目的

- `do concurrent` について学ぶ
  - 前回の勉強会の結果の共有
  - 調査報告
  - テストプログラムを各環境で実行
    - Jacobi 法（18-point stencil）よりもシンプルな問題
  - 議論・考察
- 次回の勉強会の内容決め

---

# 本日の流れ

1. 参加者の簡単な自己紹介 (-13:15)
2. 基礎知識の共有のための発表 (-14:00)
   - do concurrent について
   - テストプログラムについて
3. 各自作業 (-15:00)
4. 結果報告・議論・考察 (-16:00)
5. 次回の調整など (-17:00)

---

# do concurrent について

- 文法
  - 基本
  - 禁止事項
  - 対応していない機能
    - 対応予定
- 各コンパイラについて
  - gfortran, ifort, nvfortran, flang, lfortran

---

## Modern Fortran explained: Incorporating Fortran 2018

- Fortran2008 で定義された構文
- 各繰り返し間に相互依存性がない事を明示する
- スレッド並列化の指示ではない
  - 並列化の手法はコンパイラに依存する

![bg right:25% fit](figs/book.png)

---

### 構文

```Fortran
! 構文
  do concurrent ([type-spec ::] index-spec-list[,scalar-mask-expr])
    ...
  end do
! iの昇順に実行される保証はない
  do concurrent (i=1:n)
    a(i) = b(i)*c(i)
  end do
! i=1からnまで逐次実行される
  do i=1,n
    a(i) = b(i)*c(i)
  end do
! 配列構文
  a(1:n) =b(1:n)*c(1:n)
```

---

### 構文：OpenMP

```Fortran
! OpenMP(1) : do 構文
  !$omp parallel do
  do i=1,n
    a(i) = b(i)*c(i)
  end do
  !$omp end parallel do
! OpenMP(2) : 配列構文
  !$omp workshare
  a(:)=b(:)*c(:)
  !$omp end workshare
```

- 配列要素の一括操作については`workshare`の方が簡潔かも

---

### 構文：禁止事項

- do concurrent 構文内で許されない事
  - 構文の終了（`return`, `stop`, `exit`），構文外の反復への`cycle`
  - image control statement (co-array で他の image を参照する操作)
  - pure でない手続きの参照
  - `ieee_get_flag`, `ieee_set_halting_mode`の手続きの参照
  - advance 指定子付きの入出力

---

### 今後，対応予定の機能

- locality の指定 : Fortran2018
  - OpenMP の`shared`,`private`．ifort が対応？
- reduction : Fortran202X
  - OpenMP の`reduction`指示節．nvfortran では 21.9 から対応．

```Fortran
do while (error>TOL)
  error = 0.0d0
  do concurrent(j=2,ny-1,i=2,nx-1) reduce(max:error)
    b(i,j) = 0.25d0*(a(i,j-1)+a(i-1,j)+a(i+1,j)+a(i,j+1))
    error = max(error,abs(b(i,j)-a(i,j)))
  end do
  ...
```

---

## 前回の勉強会のまとめ

- 環境依存が大きいが，性能が向上しないケースが多かった
  - 4 スレッドで 2 倍以上の高速化を達成したのは，1/8
  - 半数は do concurrent を用いると遅くなる．半数は高々 2 倍弱
- 問題依存性(18 点ステンシルの jacobi 法)
  - reduction 節を使いたい処理があった．（対応が様々）
  - もう少し簡単な問題で検討したい
- 多重ループの記述の仕方に任意性があり，計算速度にも影響した
- gfortran の場合，できるだけ新しいバージョンを使用すべき

---

## gfortran

- 前回の勉強会で教えていただいた論文 "Can Fortran’s ‘do concurrent’ replace directives for accelerated computing?" (2021,arXiv)によれば，
  - v9 以上で`-ftree-parallelize-loops=<N>`オプションを付けてコンパイルすれば良い．(N はスレッド数)
  - do concurrent 構文の GPU オフローディングには対応していない

---

## ifort

- コンパイル時に，OpenMP のオプションをつけると並列化される
- 2018 で規定される locality の記述が可能

- （調べきれませんでした）

---

## nvfortran

- reduction 節にも対応 (v21.9)
- シンプルな問題（9 点ステンシルの jacobi 法）で OpenACC と同程度の性能を発揮（A100 プロセッサを使用）

 <div style="text-align:center">
      <img src="./figs/nvfortran.png" width="450">
  </div>

---

## flang

- "DO CONCURRENT isn’t necessarily concurrent" なるドキュメントを公開している

  - 現状の定義は安全な並列演算実行のための十分条件でない
  - 2018 で導入予定の locality specifier だけで現状の問題が全て解決されるわけではない

- コンパイラ自体の対応状況は調べきれませんでした

---

## lfortran

- 2020 年夏ごろに対応をアナウンス（Twitter より引用）
  > LFortran can now compile the following code with `do concurrent` via the C++ backend (Fortran source code -> AST -> ASR -> C++ source code -> binary) and it runs in parallel via Kokkos (on CPU or GPU). ASR is reused for both LLVM and C++ backends.
  <div style="text-align:center">
      <img src="./figs/lfortran_architecture.png" width="400">
  </div>

---

## lfortran

- 現状 (v0.14.0)，ループインデックス？が 2 つ以上の問題には未対応
  - 下に示す，二次元配列の要素ごとの積を求めるコードはコンパイルできない．(AST までは変換できる) `Semantic error: Do concurrent: exactly one control statement is required for now `

```Fortran
...
  do concurrent(j=1:100,i=1:100)
    mc(i,j) = ma(i,j)+mb(i,j)
  end do
...
```

---

# テストプログラム

- https://github.com/toya42/fortran_parallel_03
- 一次元の問題 (src/one_dimensional)
  - 配列要素ごとの和・積・商を求める
- 二次元の問題 (src/two_dimensional)
  - 四点ステンシルの Jacobi 法
  - 近似リーマン解法（Roe の風上流束）

---

## 一次元の問題

- 二つの一次元配列の要素ごとの和などを，三つ目の一次元配列に代入する
  - 4 つの数値型(int4,real4,real8,complex8)・一つの論理型(logical)について，それぞれ`tests_{type}.f90`ファイル
  - 個人的な興味で，テストする配列は 3 種類用意
    - main で宣言：static に確保されることを期待
    - module 内の subroutine で宣言：stack
    - module 内の subroutine で allocate：heap

---

## 二次元の問題

- 四点ステンシルの Jacobi 法
  - jacobi.f90
  - 残差は計算していない
- 近似リーマン解法
  - roe.f90
  - 圧縮性流体の数値計算でスタンダードな解法の一つである Roe scheme を実装

---

## パラメータ

| 変数   | 定義しているファイル | 説明                                 |
| ------ | -------------------- | ------------------------------------ |
| nsize  | main.f90             | 配列サイズ                           |
| nloop  | execusion.sh         | 繰り返し回数                         |
| ntrial | execusion.sh         | 試行回数                             |
| napara | execusion.sh         | 自動並列化のスレッド数（0 なら off） |
| nomp   | execusion.sh         | OpenMP のスレッド数                  |

---

## パラメータ

| 変数          | 定義しているファイル | 説明                                                  |
| ------------- | -------------------- | ----------------------------------------------------- |
| compiler_type | execusion.sh         | 利用するコンパイラ（gfortran など）                   |
| build_type    | execusion.sh         | debug:最適化オプションなし．fast:最適化オプションあり |

- コンパイルオプションは CMakelists.txt で変更可能

---

## 時間計測

- `omp_get_wtime`を使用
  - https://qiita.com/implicit_none/items/dd067e9bf5f7b49ce84b

---

# コンパイルと実行

`one_dimensional`あるいは`two_dimensional`ディレクトリで

```bash
$bash execution.sh
```

とするとコンパイルと実行されます．

- デフォルトでは，(npara,nomp)=(0,1), (1,1), (2,1), (4,1), (0,2), (0,4) の 6 ケースが連続で実行されます．(execution.sh で変更可能)

---

# 各自作業 (-15:00)

- 質問や結果報告は slack の #並列計算勉強会 チャンネルへ

  - output ディレクトリの csv ファイルをアップロード

- 余裕がある方向け
  - シングルスレッド演算の高速化技法の適用
  - valgrind などのツールで分析 etc...

---

# 結果・知見の共有 (-16:00)

## do concurrent に関する事

...

---

# 参考，画像引用したサイトのリンク

- do concurrent の文法

  - https://www.intel.com/content/www/us/en/develop/documentation/fortran-compiler-oneapi-dev-guide-and-reference/top/language-reference/a-to-z-reference/c-to-d/do-concurrent.html

- gfortran
  - https://arxiv.org/pdf/2110.10151.pdf

---

- ifort

  - https://www.intel.com/content/www/us/en/developer/articles/release-notes/intel-fortran-compiler-191-for-windows-release-notes-for-intel-parallel-studio-xe-2020.html#f18

- nvfortran

  - https://www.youtube.com/watch?v=pK-gllheNXE

- flang
  - https://releases.llvm.org/12.0.1/tools/flang/docs/DoConcurrent.html

---

- lfortran
  - https://www.youtube.com/watch?v=tW9tUxVMnzc
  - https://twitter.com/lfortranorg/status/1293416170138120193

---

# 次回の詳細決め等 (-17:00)

- 日時
- テーマ
- 発表者

---

<!-- _class: lead -->

# ご参加ありがとうございました
