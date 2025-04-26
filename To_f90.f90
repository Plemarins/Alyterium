PROGRAM AlysteriumCompiler
  IMPLICIT NONE

  ! 定数
  INTEGER, PARAMETER :: MAX_TOKENS = 100
  INTEGER, PARAMETER :: MAX_TOKEN_LEN = 20
  INTEGER, PARAMETER :: MAX_ENV = 10
  INTEGER, PARAMETER :: MAX_CODE = 1000

  ! 環境用の構造体
  TYPE :: EnvEntry
    CHARACTER(LEN=MAX_TOKEN_LEN) :: name
    INTEGER :: value
    CHARACTER(LEN=MAX_TOKEN_LEN) :: fn_name
    CHARACTER(LEN=MAX_TOKEN_LEN) :: fn_param
    CHARACTER(LEN=MAX_TOKEN_LEN) :: fn_body
  END TYPE EnvEntry

  ! グローバル変数
  TYPE(EnvEntry), DIMENSION(MAX_ENV) :: env
  INTEGER :: env_size = 0
  CHARACTER(LEN=MAX_TOKEN_LEN), DIMENSION(MAX_TOKENS) :: tokens
  INTEGER :: token_count
  CHARACTER(LEN=MAX_CODE) :: output_code
  INTEGER :: output_pos

  ! 音韻論データ
  CHARACTER(LEN=1), DIMENSION(4) :: phoneme_fold = ['f', 'o', 'l', 'd']
  CHARACTER(LEN=1), DIMENSION(4) :: phoneme_flow = ['f', 'l', 'o', 'w']

  ! メイン処理
  CALL RunCompiler()

CONTAINS

  ! コンパイラのメインルーチン
  SUBROUTINE RunCompiler()
    CHARACTER(LEN=200) :: input
    INTEGER :: i

    ! サンプル入力（ファイル入力の代わり）
    WRITE(*, '(A)') 'Alysteriumコードを入力（終了は空行）:'
    input = 'x := 5; fn add(a) => a + 1; flow add(x); fold add(x); inverse add(6)'
    WRITE(*, '(A,A)') '入力例: ', TRIM(input)

    ! トークナイズ
    CALL Tokenize(input)

    ! 初期化
    output_code = ''
    output_pos = 1
    CALL AppendOutput('PROGRAM AlysteriumOutput')
    CALL AppendOutput('  IMPLICIT NONE')
    CALL AppendOutput('  INTEGER :: x')

    ! 解析とコード生成
    CALL ParseAndGenerate()

    ! プログラム終了
    CALL AppendOutput('END PROGRAM AlysteriumOutput')

    ! 出力ファイルに書き込み
    OPEN(UNIT=10, FILE='output.f90', STATUS='REPLACE')
    WRITE(10, '(A)') TRIM(output_code)
    CLOSE(10)
    WRITE(*, '(A)') 'コンパイル完了: output.f90 に出力しました'

  END SUBROUTINE RunCompiler

  ! トークナイザ
  SUBROUTINE Tokenize(input)
    CHARACTER(LEN=*), INTENT(IN) :: input
    CHARACTER(LEN=200) :: temp
    INTEGER :: i, pos
    LOGICAL :: in_token

    token_count = 0
    temp = TRIM(input) // ' '
    in_token = .FALSE.
    pos = 1

    DO i = 1, LEN_TRIM(temp)
      IF (temp(i:i) == ' ' .OR. temp(i:i) == ';') THEN
        IF (in_token) THEN
          token_count = token_count + 1
          tokens(token_count) = temp(pos:i-1)
          in_token = .FALSE.
        END IF
        IF (temp(i:i) == ';') THEN
          token_count = token_count + 1
          tokens(token_count) = ';'
        END IF
      ELSE IF (temp(i:i) == ':' .AND. temp(i+1:i+1) == '=') THEN
        IF (in_token) THEN
          token_count = token_count + 1
          tokens(token_count) = temp(pos:i-1)
          in_token = .FALSE.
        END IF
        token_count = token_count + 1
        tokens(token_count) = ':='
        i = i + 1
      ELSE IF (.NOT. in_token) THEN
        in_token = .TRUE.
        pos = i
      END IF
    END DO
  END SUBROUTINE Tokenize

  ! 音韻論チェック
  LOGICAL FUNCTION IsPhonotacticallyValid(token)
    CHARACTER(LEN=*), INTENT(IN) :: token
    INTEGER :: i

    IF (token == 'fold' .OR. token == 'flow') THEN
      IsPhonotacticallyValid = .TRUE.
      RETURN
    END IF
    IsPhonotacticallyValid = .TRUE.
    DO i = 1, LEN_TRIM(token)
      IF (.NOT. (token(i:i) >= 'a' .AND. token(i:i) <= 'z')) THEN
        IsPhonotacticallyValid = .FALSE.
        RETURN
      END IF
    END DO
  END FUNCTION IsPhonotacticallyValid

  ! 形態論変換
  SUBROUTINE ApplyMorphology(token, result)
    CHARACTER(LEN=*), INTENT(IN) :: token
    CHARACTER(LEN=MAX_TOKEN_LEN), INTENT(OUT) :: result
    IF (token == 'flow') THEN
      result = 'flows'
    ELSE IF (token == 'fold') THEN
      result = 'folds'
    ELSE
      result = token
    END IF
  END SUBROUTINE ApplyMorphology

  ! 解析とコード生成
  SUBROUTINE ParseAndGenerate()
    INTEGER :: i
    CHARACTER(LEN=MAX_TOKEN_LEN) :: morph_result

    i = 1
    DO WHILE (i <= token_count)
      IF (.NOT. IsPhonotacticallyValid(tokens(i))) THEN
        WRITE(*, '(A,A)') 'エラー: 無効なトークン: ', TRIM(tokens(i))
        i = i + 1
        CYCLE
      END IF

      CALL ApplyMorphology(tokens(i), morph_result)

      IF (tokens(i) == 'fn') THEN
        CALL GenerateFunction(i)
      ELSE IF (i + 1 <= token_count .AND. tokens(i+1) == ':=') THEN
        CALL GenerateAssign(i)
      ELSE IF (tokens(i) == 'flow') THEN
        CALL GenerateFlow(i)
      ELSE IF (tokens(i) == 'fold') THEN
        CALL GenerateFold(i)
      ELSE IF (tokens(i) == 'inverse') THEN
        CALL GenerateInverse(i)
      END IF
      i = i + 1
    END DO

    ! サブルーチンを出力
    IF (env_size > 0) THEN
      CALL AppendOutput('CONTAINS')
      CALL GenerateSubroutines()
    END IF
  END SUBROUTINE ParseAndGenerate

  ! 代入のコード生成
  SUBROUTINE GenerateAssign(pos)
    INTEGER, INTENT(INOUT) :: pos
    CHARACTER(LEN=MAX_TOKEN_LEN) :: name, value_str
    INTEGER :: value

    name = tokens(pos)
    value_str = tokens(pos + 2)
    READ(value_str, *) value
    CALL AddToEnv(name, value, '', '', '')
    CALL AppendOutput('  ' // TRIM(name) // ' = ' // TRIM(value_str))
    pos = pos + 3
  END SUBROUTINE GenerateAssign

  ! 関数のコード生成
  SUBROUTINE GenerateFunction(pos)
    INTEGER, INTENT(INOUT) :: pos
    CHARACTER(LEN=MAX_TOKEN_LEN) :: name, param, body

    name = tokens(pos + 1)
    param = tokens(pos + 2)
    body = tokens(pos + 4)
    CALL AddToEnv('', 0, name, param, body)
    pos = pos + 6
  END SUBROUTINE GenerateFunction

  ! flowのコード生成
  SUBROUTINE GenerateFlow(pos)
    INTEGER, INTENT(INOUT) :: pos
    CHARACTER(LEN=MAX_TOKEN_LEN) :: expr, fn_name, arg

    expr = tokens(pos + 1)
    CALL SplitExpr(expr, fn_name, arg)
    CALL AppendOutput('  CALL Flow_' // TRIM(fn_name) // '(' // TRIM(arg) // ')')
    pos = pos + 2
  END SUBROUTINE GenerateFlow

  ! foldのコード生成
  SUBROUTINE GenerateFold(pos)
    INTEGER, INTENT(INOUT) :: pos
    CHARACTER(LEN=MAX_TOKEN_LEN) :: expr, fn_name, arg

    expr = tokens(pos + 1)
    CALL SplitExpr(expr, fn_name, arg)
    CALL AppendOutput('  CALL Fold_' // TRIM(fn_name) // '(' // TRIM(arg) // ')')
    pos = pos + 2
  END SUBROUTINE GenerateFold

  ! inverseのコード生成
  SUBROUTINE GenerateInverse(pos)
    INTEGER, INTENT(INOUT) :: pos
    CHARACTER(LEN=MAX_TOKEN_LEN) :: expr, fn_name, arg

    expr = tokens(pos + 1)
    CALL SplitExpr(expr, fn_name, arg)
    CALL AppendOutput('  CALL Inverse_' // TRIM(fn_name) // '(' // TRIM(arg) // ')')
    pos = pos + 2
  END SUBROUTINE GenerateInverse

  ! サブルーチンのコード生成
  SUBROUTINE GenerateSubroutines()
    INTEGER :: i
    CHARACTER(LEN=MAX_TOKEN_LEN) :: fn_name, param, body
    CHARACTER(LEN=100) :: line

    DO i = 1, env_size
      IF (env(i)%fn_name /= '') THEN
        fn_name = env(i)%fn_name
        param = env(i)%fn_param
        body = env(i)%fn_body

        ! Flowサブルーチン
        CALL AppendOutput('  SUBROUTINE Flow_' // TRIM(fn_name) // '(a)')
        CALL AppendOutput('    INTEGER, INTENT(IN) :: a')
        IF (INDEX(body, '+') > 0) THEN
          line = '    WRITE(*, ''(A,I0)'') ''flow ' // TRIM(fn_name) // ' = '', a + ' // body(5:)
        ELSE
          line = '    WRITE(*, ''(A,I0)'') ''flow ' // TRIM(fn_name) // ' = '', a'
        END IF
        CALL AppendOutput(TRIM(line))
        CALL AppendOutput('  END SUBROUTINE Flow_' // TRIM(fn_name))

        ! Foldサブルーチン
        CALL AppendOutput('  SUBROUTINE Fold_' // TRIM(fn_name) // '(a)')
        CALL AppendOutput('    INTEGER, INTENT(IN) :: a')
        IF (INDEX(body, '+') > 0) THEN
          line = '    WRITE(*, ''(A,I0)'') ''fold ' // TRIM(fn_name) // ' = '', a - ' // body(5:)
        ELSE
          line = '    WRITE(*, ''(A,I0)'') ''fold ' // TRIM(fn_name) // ' = '', a'
        END IF
        CALL AppendOutput(TRIM(line))
        CALL AppendOutput('  END SUBROUTINE Fold_' // TRIM(fn_name))

        ! Inverseサブルーチン
        CALL AppendOutput('  SUBROUTINE Inverse_' // TRIM(fn_name) // '(a)')
        CALL AppendOutput('    INTEGER, INTENT(IN) :: a')
        IF (INDEX(body, '+') > 0) THEN
          line = '    WRITE(*, ''(A,I0)'') ''inverse ' // TRIM(fn_name) // ' = '', a - ' // body(5:)
        ELSE
          line = '    WRITE(*, ''(A,I0)'') ''inverse ' // TRIM(fn_name) // ' = '', a'
        END IF
        CALL AppendOutput(TRIM(line))
        CALL AppendOutput('  END SUBROUTINE Inverse_' // TRIM(fn_name))
      END IF
    END DO
  END SUBROUTINE GenerateSubroutines

  ! 環境への追加
  SUBROUTINE AddToEnv(name, value, fn_name, fn_param, fn_body)
    CHARACTER(LEN=*), INTENT(IN) :: name, fn_name, fn_param, fn_body
    INTEGER, INTENT(IN) :: value
    env_size = env_size + 1
    env(env_size)%name = name
    env(env_size)%value = value
    env(env_size)%fn_name = fn_name
    env(env_size)%fn_param = fn_param
    env(env_size)%fn_body = fn_body
  END SUBROUTINE AddToEnv

  ! 式の分割
  SUBROUTINE SplitExpr(expr, fn_name, arg)
    CHARACTER(LEN=*), INTENT(IN) :: expr
    CHARACTER(LEN=MAX_TOKEN_LEN), INTENT(OUT) :: fn_name, arg
    INTEGER :: i

    fn_name = ''
    arg = ''
    DO i = 1, LEN_TRIM(expr)
      IF (expr(i:i) == '(') THEN
        fn_name = expr(1:i-1)
        arg = expr(i+1:LEN_TRIM(expr)-1)
        EXIT
      END IF
    END DO
  END SUBROUTINE SplitExpr

  ! 出力コードの追加
  SUBROUTINE AppendOutput(line)
    CHARACTER(LEN=*), INTENT(IN) :: line
    INTEGER :: len

    len = LEN_TRIM(line)
    IF (output_pos + len > MAX_CODE) THEN
      WRITE(*, '(A)') 'エラー: 出力バッファ超過'
      STOP
    END IF
    output_code(output_pos:output_pos+len-1) = TRIM(line)
    output_pos = output_pos + len
    output_code(output_pos:output_pos) = CHAR(10) ! 改行
    output_pos = output_pos + 1
  END SUBROUTINE AppendOutput

END PROGRAM AlysteriumCompiler
