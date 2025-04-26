import java.io.*;
import java.util.*;

public class AlysteriumCompiler {
    // 音韻論データ
    private static final Map<String, List<String>> phonemes = new HashMap<>();
    static {
        phonemes.put("fold", Arrays.asList("f", "o", "l", "d"));
        phonemes.put("flow", Arrays.asList("f", "l", "o", "w"));
    }

    // 形態論データ
    private static final Map<String, String> morphVariants = new HashMap<>();
    static {
        morphVariants.put("flow", "flows");
        morphVariants.put("fold", "folds");
    }

    // 環境（変数と関数）
    private static class EnvEntry {
        String name;
        int value;
        String fnName;
        String fnParam;
        String fnBody;

        EnvEntry(String name, int value) {
            this.name = name;
            this.value = value;
        }

        EnvEntry(String fnName, String fnParam, String fnBody) {
            this.fnName = fnName;
            this.fnParam = fnParam;
            this.fnBody = fnBody;
        }
    }

    private List<EnvEntry> env = new ArrayList<>();
    private List<String> tokens = new ArrayList<>();
    private StringBuilder outputCode = new StringBuilder();
    private int tokenIndex = 0;

    public static void main(String[] args) {
        AlysteriumCompiler compiler = new AlysteriumCompiler();
        String input = "x := 5; fn add(a) => a + 1; flow add(x); fold add(x); inverse add(6);";
        System.out.println("入力例: " + input);
        compiler.compile(input);
    }

    public void compile(String input) {
        try {
            // トークナイズ
            tokenize(input);

            // コード生成の初期化
            outputCode.append("public class AlysteriumOutput {\n");
            outputCode.append("    public static int x;\n");
            outputCode.append("    public static void main(String[] args) {\n");

            // 解析とコード生成
            parseAndGenerate();

            // メソッドとクラスを閉じる
            outputCode.append("    }\n");
            outputCode.append("}\n");

            // 出力ファイルに書き込み
            try (PrintWriter out = new PrintWriter("AlysteriumOutput.java")) {
                out.println(outputCode.toString());
            }
            System.out.println("コンパイル完了: AlysteriumOutput.java に出力しました");

        } catch (Exception e) {
            System.err.println("エラー: " + e.getMessage());
        }
    }

    // トークナイザ
    private void tokenize(String input) {
        String[] parts = input.replace(";", " ; ").replace(":=", " := ").split("\\s+");
        for (String part : parts) {
            if (!part.isEmpty()) {
                tokens.add(part);
            }
        }
    }

    // 音韻論チェック
    private boolean isPhonotacticallyValid(String token) {
        if (phonemes.containsKey(token)) {
            return true;
        }
        return token.matches("[a-zA-Z]+");
    }

    // 形態論変換
    private String applyMorphology(String token) {
        return morphVariants.getOrDefault(token, token);
    }

    // 解析とコード生成
    private void parseAndGenerate() {
        tokenIndex = 0;
        while (tokenIndex < tokens.size()) {
            String token = tokens.get(tokenIndex);
            if (!isPhonotacticallyValid(token)) {
                throw new RuntimeException("無効なトークン: " + token);
            }

            // 形態論適用（ログのみ）
            String morphResult = applyMorphology(token);
            // System.out.println("形態論: " + morphResult);

            if (token.equals("fn")) {
                generateFunction();
            } else if (tokenIndex + 1 < tokens.size() && tokens.get(tokenIndex + 1).equals(":=")) {
                generateAssign();
            } else if (token.equals("flow")) {
                generateFlow();
            } else if (token.equals("fold")) {
                generateFold();
            } else if (token.equals("inverse")) {
                generateInverse();
            } else {
                tokenIndex++;
            }
        }

        // メソッドを生成
        generateMethods();
    }

    // 代入のコード生成
    private void generateAssign() {
        String name = tokens.get(tokenIndex);
        String value = tokens.get(tokenIndex + 2);
        env.add(new EnvEntry(name, Integer.parseInt(value)));
        outputCode.append("        ").append(name).append(" = ").append(value).append(";\n");
        tokenIndex += 4; // name := value ;
    }

    // 関数のコード生成
    private void generateFunction() {
        String fnName = tokens.get(tokenIndex + 1);
        String fnParam = tokens.get(tokenIndex + 2).replaceAll("[()]", "");
        String fnBody = tokens.get(tokenIndex + 4);
        env.add(new EnvEntry(fnName, fnParam, fnBody));
        tokenIndex += 6; // fn name (param) => body ;
    }

    // flowのコード生成
    private void generateFlow() {
        String expr = tokens.get(tokenIndex + 1);
        String[] parts = expr.split("[()]");
        String fnName = parts[0];
        String arg = parts[1];
        outputCode.append("        flow_").append(fnName).append("(").append(arg).append(");\n");
        tokenIndex += 3; // flow expr ;
    }

    // foldのコード生成
    private void generateFold() {
        String expr = tokens.get(tokenIndex + 1);
        String[] parts = expr.split("[()]");
        String fnName = parts[0];
        String arg = parts[1];
        outputCode.append("        fold_").append(fnName).append("(").append(arg).append(");\n");
        tokenIndex += 3; // fold expr ;
    }

    // inverseのコード生成
    private void generateInverse() {
        String expr = tokens.get(tokenIndex + 1);
        String[] parts = expr.split("[()]");
        String fnName = parts[0];
        String arg = parts[1];
        outputCode.append("        inverse_").append(fnName).append("(").append(arg).append(");\n");
        tokenIndex += 3; // inverse expr ;
    }

    // メソッドのコード生成
    private void generateMethods() {
        for (EnvEntry entry : env) {
            if (entry.fnName != null) {
                String fnName = entry.fnName;
                String fnParam = entry.fnParam;
                String fnBody = entry.fnBody;

                // flowメソッド
                outputCode.append("    public static void flow_").append(fnName).append("(int ").append(fnParam).append(") {\n");
                if (fnBody.contains("+")) {
                    String[] parts = fnBody.split("\\+");
                    outputCode.append("        System.out.println(\"flow ").append(fnName).append(" = \" + (").append(fnParam).append(" + ").append(parts[1].trim()).append("));\n");
                } else {
                    outputCode.append("        System.out.println(\"flow ").append(fnName).append(" = \" + ").append(fnParam).append(");\n");
                }
                outputCode.append("    }\n");

                // foldメソッド
                outputCode.append("    public static void fold_").append(fnName).append("(int ").append(fnParam).append(") {\n");
                if (fnBody.contains("+")) {
                    String[] parts = fnBody.split("\\+");
                    outputCode.append("        System.out.println(\"fold ").append(fnName).append(" = \" + (").append(fnParam).append(" - ").append(parts[1].trim()).append("));\n");
                } else {
                    outputCode.append("        System.out.println(\"fold ").append(fnName).append(" = \" + ").append(fnParam).append(");\n");
                }
                outputCode.append("    }\n");

                // inverseメソッド
                outputCode.append("    public static void inverse_").append(fnName).append("(int ").append(fnParam).append(") {\n");
                if (fnBody.contains("+")) {
                    String[] parts = fnBody.split("\\+");
                    outputCode.append("        System.out.println(\"inverse ").append(fnName).append(" = \" + (").append(fnParam).append(" - ").append(parts[1].trim()).append("));\n");
                } else {
                    outputCode.append("        System.out.println(\"inverse ").append(fnName).append(" = \" + ").append(fnParam).append(");\n");
                }
                outputCode.append("    }\n");
            }
        }
    }
}
