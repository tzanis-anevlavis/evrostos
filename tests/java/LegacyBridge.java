// SPDX-License-Identifier: GPL-3.0-or-later
// Test-only access to the unmodified Java parser and translation visitors.
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.StringReader;
import java.nio.charset.StandardCharsets;
import java.util.Base64;
import org.mpi_sws.rltl.expressions.*;
import org.mpi_sws.rltl.parser.LTLParser;
import org.mpi_sws.rltl.parser.ParseException;
import org.mpi_sws.rltl.parser.TokenMgrError;
import org.mpi_sws.rltl.visitors.OptimizedRLTL2LTLVisitor;
import org.mpi_sws.rltl.visitors.RLTL2LTLVisitor;

public final class LegacyBridge {
    private static String quoted(String text) {
        StringBuilder result = new StringBuilder("\"");
        for (int i = 0; i < text.length(); ++i) {
            char c = text.charAt(i);
            if (c == '"' || c == '\\') {
                result.append('\\').append(c);
            } else if (c < 32) {
                result.append(String.format("\\u%04x", (int) c));
            } else {
                result.append(c);
            }
        }
        return result.append('"').toString();
    }

    private static String unary(String op, Expression child, boolean robust) {
        return "[" + quoted(op) + "," + ast(child, robust) + "]";
    }

    private static String binary(String op, Expression left, Expression right, boolean robust) {
        return "[" + quoted(op) + "," + ast(left, robust) + "," + ast(right, robust) + "]";
    }

    // Serialize existing nodes only; no translation or semantic rules live here.
    private static String ast(Expression expression, boolean robust) {
        if (expression instanceof Atom) {
            return quoted(((Atom) expression).identifier);
        } else if (expression instanceof Negation) {
            return unary("!", ((Negation) expression).subExpr, robust);
        } else if (expression instanceof Next) {
            return unary(robust ? "rX" : "X", ((Next) expression).subExpr, robust);
        } else if (expression instanceof Finally) {
            return unary(robust ? "rF" : "F", ((Finally) expression).subExpr, robust);
        } else if (expression instanceof Globally) {
            return unary(robust ? "rG" : "G", ((Globally) expression).subExpr, robust);
        } else if (expression instanceof Conjunction) {
            Conjunction node = (Conjunction) expression;
            return binary("&", node.subExpr1, node.subExpr2, robust);
        } else if (expression instanceof Disjunction) {
            Disjunction node = (Disjunction) expression;
            return binary("|", node.subExpr1, node.subExpr2, robust);
        } else if (expression instanceof Implication) {
            Implication node = (Implication) expression;
            return binary(robust ? "=>" : "->", node.subExpr1, node.subExpr2, robust);
        } else if (expression instanceof Until) {
            Until node = (Until) expression;
            return binary(robust ? "rU" : "U", node.subExpr1, node.subExpr2, robust);
        } else if (expression instanceof Release) {
            Release node = (Release) expression;
            return binary(robust ? "rR" : "R", node.subExpr1, node.subExpr2, robust);
        }
        throw new IllegalArgumentException("Unrecognized expression: " + expression.getClass());
    }

    private static String roots(Expression[] expressions) {
        if (expressions.length != 4) {
            throw new IllegalStateException("Expected exactly four LTL roots");
        }
        return "[" + ast(expressions[0], false) + "," + ast(expressions[1], false)
            + "," + ast(expressions[2], false) + "," + ast(expressions[3], false) + "]";
    }

    public static void main(String[] args) throws Exception {
        BufferedReader input = new BufferedReader(new InputStreamReader(System.in, StandardCharsets.UTF_8));
        // The generated JavaCC parser is static; ReInit resets each request.
        LTLParser parser = new LTLParser(new StringReader(""));
        String line;
        while ((line = input.readLine()) != null) {
            String[] request = line.split("\t", -1);
            if (request.length != 2 || !(request[0].equals("parse") || request[0].equals("translate"))) {
                throw new IllegalArgumentException("Expected mode and base64 UTF-8 source");
            }
            String source = new String(Base64.getDecoder().decode(request[1]), StandardCharsets.UTF_8);
            Expression parsed;
            try {
                parser.ReInit(new StringReader(source));
                parsed = parser.expression();
            } catch (ParseException | TokenMgrError error) {
                System.out.println("{\"ok\":false,\"error\":\"syntax_error\"}");
                continue;
            }
            String response = "{\"ok\":true,\"ast\":" + ast(parsed, true);
            if (request[0].equals("translate")) {
                response += ",\"default\":" + roots(RLTL2LTLVisitor.convert(parsed));
                try {
                    response += ",\"optimized\":" + roots(OptimizedRLTL2LTLVisitor.convert(parsed));
                } catch (UnsupportedOperationException error) {
                    response += ",\"optimized_error\":\"unsupported_operator\"";
                }
            }
            System.out.println(response + "}");
        }
    }
}
