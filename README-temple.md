# LLVM for Temple architecture
Templeアーキテクチャ向けLLVMバックエンド実装(TempleE/EXには非対応)

## ビルド方法
Ubuntuでのビルド方法です

- 必要なソフトウェアのインストール
  - `sudo apt install cmake ninja-build clang lld`
- ソースコードの取得
  - `git clone https://github.com/pepepper/llvm-temple`
- ブランチのチェックアウト
  - `cd llvm-temple && git checkout temple`
- ビルドフォルダの作成
  - `mkdir build && cd build`
- ビルドの設定
  - `cmake -G Ninja 
    -DLLVM_ENABLE_PROJECTS="clang;lld" 
    -DCMAKE_BUILD_TYPE="Release" 
    -DLLVM_BUILD_TESTS=True 
    -DCMAKE_C_COMPILER=clang 
    -DCMAKE_CXX_COMPILER=clang++ 
    -DLLVM_USE_LINKER=lld 
    -DLLVM_TARGETS_TO_BUILD="" 
    -DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD="Temple" 
    ../llvm`
- ビルド
  - `ninja`

## 使用方法
- C言語からTempleアセンブリを生成する
  - buildディレクトリ内にいることを確認
  - `bin/clang -target temple -S -emit-llvm (C言語ソースファイル)`


## 現状不可能なこと
- C言語における8bitの型(char等)の使用
  - 疑似命令を追加する必要あり
  - int型の数値の上位/下位8bitに対してビット演算を用いてアクセスすることで疑似的にchar型の再現は可能?
- バイナリ出力
  - 分岐先アドレスの最終決定にリンカの実装が必要
- TempleE/EX向けの出力
  - JL命令のオペランドの順番がTempleと異なるため