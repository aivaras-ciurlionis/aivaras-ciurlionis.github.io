(function () {
'use strict';

/**
 * Main class that defines all control funtions for a chart.
 *
 * ### Example
 * <pre>
 * import { MatchChart } from 'math-chart'
 * let chart = new MatchChart()
 * chart.setChartContainer('elementId')
 * </pre>
 */
class ExpressionCreator {
    constructor(expressionFactory, monomialResolver, minOperationFinder) {
        this.expressionFactory = expressionFactory;
        this.monomialResolver = monomialResolver;
        this.minOperationFinder = minOperationFinder;
    }
    /**
     * Recursive function that parses a list of tokens into an expression
     * @param tokens List of tokens
     */
    ToExpression(tokens) {
        if (tokens.length < 1) {
            return null;
        }
        if (tokens.length === 1) {
            return this.monomialResolver.GetMonomial(tokens[0]);
        }
        let index = this.minOperationFinder.FindMinOperationIndex(tokens);
        const nextOperation = tokens[index];
        const leftTokens = tokens.slice(0, index);
        const rightTokens = tokens.slice(index + 1);
        switch (nextOperation.Arity) {
            case 1:
                return this.expressionFactory.GetExpressionByOperands(nextOperation.Value, [this.ToExpression(rightTokens)]);
            case 2:
                return this.expressionFactory.GetExpressionByOperands(nextOperation.Value, [this.ToExpression(leftTokens), this.ToExpression(rightTokens)]);
            default:
                return this.expressionFactory.GetExpressionByKey(nextOperation.Value);
        }
    }
    /**
     * Returns an expression from a list of parsed tokens
     * @param tokens List of tokens
     * @returns A parsed expression prepared for calculations
     */
    CreateExpression(tokens) {
        const validTokens = tokens.filter(t => t.Value !== ')');
        return this.ToExpression(validTokens);
    }
}

var TokenType;
(function (TokenType) {
    TokenType[TokenType["Number"] = 0] = "Number";
    TokenType[TokenType["Operation"] = 1] = "Operation";
    TokenType[TokenType["Variable"] = 2] = "Variable";
    TokenType[TokenType["Other"] = 3] = "Other";
})(TokenType || (TokenType = {}));
class Token {
    static MultiplyToken() {
        var token = Object.assign(new Token(), {
            Value: '*',
            Arity: 2,
            Order: 2,
            TypeOfToken: TokenType.Operation
        });
        return token;
    }
}

/**
 * Base expression that represents any expression with operands
 */
class BaseExpression {
    /**
     * Replaces variables with given values
     * @param replacementDictionary Variable values to replace: {x: 2, y: 4}
     * @returns Expression with numerical values
     */
    ReplaceVariables(replacementDictionary) {
        this.Operands.forEach((operand, i) => {
            this.Operands[i] = operand.ReplaceVariables(replacementDictionary);
        });
        return this;
    }
}

/**
 * Expression that represents a sum of two numbers 2+4
 */
class SumExpression extends BaseExpression {
    constructor(left, right) {
        super();
        this.Arity = 2;
        this.Order = 3;
        this.Operands = [left, right];
    }
    /**
     * @returns A sum of operands
     */
    Execute() {
        return this.Operands[0].Execute() + this.Operands[1].Execute();
    }
    Clone() {
        return new SumExpression(this.Operands[0].Clone(), this.Operands[1].Clone());
    }
}

/**
 * Expression that represents a subtraction of two numbers 2-4
 */
class SubtractExpression extends BaseExpression {
    constructor(left, right) {
        super();
        this.Arity = 2;
        this.Order = 3;
        this.Operands = [left, right];
    }
    /**
     * @returns A difference of operands
     */
    Execute() {
        return this.Operands[0].Execute() - this.Operands[1].Execute();
    }
    Clone() {
        return new SubtractExpression(this.Operands[0].Clone(), this.Operands[1].Clone());
    }
}

/**
 * Expression that negates a given operand
 */
class NegationExpression extends BaseExpression {
    constructor(operand) {
        super();
        this.Arity = 1;
        this.Order = 2;
        this.Operands = [operand];
    }
    /**
     * @returns Negated operand result
     */
    Execute() {
        return -this.Operands[0].Execute();
    }
    Clone() {
        return new NegationExpression(this.Operands[0].Clone());
    }
}

/**
 * Expression that represents a product of two numbers 2*4
 */
class MultiplyExpression extends BaseExpression {
    constructor(left, right) {
        super();
        this.Arity = 2;
        this.Order = 2;
        this.Operands = [left, right];
    }
    /**
     * @returns A difference of operands
     */
    Execute() {
        return this.Operands[0].Execute() * this.Operands[1].Execute();
    }
    Clone() {
        return new MultiplyExpression(this.Operands[0].Clone(), this.Operands[1].Clone());
    }
}

/**
 * Expression that represents a division or a fraction of two numbers 2/4
 */
class DivisionExpression extends BaseExpression {
    constructor(left, right) {
        super();
        this.Arity = 2;
        this.Order = 2;
        this.Operands = [left, right];
    }
    /**
     * @returns A division of operands
     */
    Execute() {
        return this.Operands[0].Execute() / this.Operands[1].Execute();
    }
    Clone() {
        return new DivisionExpression(this.Operands[0].Clone(), this.Operands[1].Clone());
    }
}

/**
 * Expression that represents a power of two numbers 2^4
 */
class ExponentExpression extends BaseExpression {
    constructor(left, right) {
        super();
        this.Arity = 2;
        this.Order = 1;
        this.Operands = [left, right];
    }
    /**
     * @returns A first operand with a power of second
     */
    Execute() {
        return Math.pow(this.Operands[0].Execute(), this.Operands[1].Execute());
    }
    Clone() {
        return new ExponentExpression(this.Operands[0].Clone(), this.Operands[1].Clone());
    }
}

/**
 * Expression that represents an operand inside parenthesis
 */
class ParenthesisExpression extends BaseExpression {
    constructor(inside) {
        super();
        this.Arity = 1;
        this.Order = 5;
        this.Operands = [inside];
    }
    /**
     * @returns Operand result inside parenthesis
     */
    Execute() {
        return this.Operands[0].Execute();
    }
    Clone() {
        return new ParenthesisExpression(this.Operands[0].Clone());
    }
}

/**
 * Expression that represents a square root of two numbers sqrt(4)
 */
class SqrRootExpression extends BaseExpression {
    constructor(operand) {
        super();
        this.Arity = 1;
        this.Order = 1;
        this.Operands = [operand];
    }
    /**
     * @returns A square root of an operand
     */
    Execute() {
        return Math.sqrt(this.Operands[0].Execute());
    }
    Clone() {
        return new SqrRootExpression(this.Operands[0].Clone());
    }
}

/**
 * Expression that represents a square root of two numbers sqrt(4)
 */
class SinExpression extends BaseExpression {
    constructor(operand) {
        super();
        this.Arity = 1;
        this.Order = 1;
        this.Operands = [operand];
    }
    /**
     * @returns A square root of an operand
     */
    Execute() {
        return Math.sin(this.Operands[0].Execute());
    }
    Clone() {
        return new SinExpression(this.Operands[0].Clone());
    }
}

/**
 * Expression that represents a square root of two numbers sqrt(4)
 */
class CosExpression extends BaseExpression {
    constructor(operand) {
        super();
        this.Arity = 1;
        this.Order = 1;
        this.Operands = [operand];
    }
    /**
     * @returns A square root of an operand
     */
    Execute() {
        return Math.cos(this.Operands[0].Execute());
    }
    Clone() {
        return new CosExpression(this.Operands[0].Clone());
    }
}

/**
 * Expression that represents a square root of two numbers sqrt(4)
 */
class TanExpression extends BaseExpression {
    constructor(operand) {
        super();
        this.Arity = 1;
        this.Order = 1;
        this.Operands = [operand];
    }
    /**
     * @returns A square root of an operand
     */
    Execute() {
        return Math.tan(this.Operands[0].Execute());
    }
    Clone() {
        return new TanExpression(this.Operands[0].Clone());
    }
}

/**
 * Expression that represents a square root of two numbers sqrt(4)
 */
class LnExpression extends BaseExpression {
    constructor(operand) {
        super();
        this.Arity = 1;
        this.Order = 1;
        this.Operands = [operand];
    }
    /**
     * @returns A square root of an operand
     */
    Execute() {
        return Math.log(this.Operands[0].Execute());
    }
    Clone() {
        return new LnExpression(this.Operands[0].Clone());
    }
}

class ExpressionFactory {
    /**
     * Returns a matching expression from a key
     * @param key Expression key (+/- etc..)
     */
    GetExpressionByKey(key) {
        const operands = [null, null];
        return this.GetExpressionByOperands(key, operands);
    }
    /**
     * Returns a matching expression from expression operands
     * @param key Expression key (+/- etc..)
     * @param operands A list of expression operandss
     */
    GetExpressionByOperands(key, operands) {
        return this.GetExpression(key, operands, operands.length === 1 ? TokenType.Other : TokenType.Number);
    }
    /**
    * Returns a matching expression from a last token
    * @param key Expression key (+/- etc..)
    * @param lastToken Last added token
    */
    GetExpressionByLastToken(key, lastToken) {
        const operands = [null, null];
        return this.GetExpression(key, operands, lastToken ? lastToken.TypeOfToken : TokenType.Other);
    }
    GetExpression(key, operands, lastTokenType) {
        switch (key) {
            case '+': return new SumExpression(operands[0], operands[1]);
            case '-': return this.GetSubractExpression(operands[0], operands.length > 1 ? operands[1] : null, lastTokenType);
            case '*': return new MultiplyExpression(operands[0], operands[1]);
            case '/': return new DivisionExpression(operands[0], operands[1]);
            case '^': return new ExponentExpression(operands[0], operands[1]);
            case '(': return new ParenthesisExpression(operands[0]);
            case 'sqrt': return new SqrRootExpression(operands[0]);
            case 'sin': return new SinExpression(operands[0]);
            case 'cos': return new CosExpression(operands[0]);
            case 'tan': return new TanExpression(operands[0]);
            case 'ln': return new LnExpression(operands[0]);
            case 'lg': return new LnExpression(operands[0]);
            default: return null;
        }
    }
    /**
     * Determines a type of subtraction expression: subraction or negation
     * @param op1
     * @param op2
     * @param lastTokenType
     */
    GetSubractExpression(op1, op2, lastTokenType) {
        if (lastTokenType === TokenType.Number || lastTokenType === TokenType.Variable) {
            return new SubtractExpression(op1, op2);
        }
        return new NegationExpression(op1);
    }
}

class Variable {
    constructor(name, exponent) {
        this.Exponent = exponent;
        this.Name = name;
    }
    Evaluate(value) {
        return Math.pow(value, this.Exponent);
    }
    ToString() {
        return Math.abs(this.Exponent - 1) < 0.001 ? `${this.Name}` : `${this.Name}^${this.Exponent}`;
    }
}

class Monomial {
    constructor(coefficient, variableName = '') {
        this.Variables = [];
        this.Coefficient = coefficient;
        if (variableName) {
            this.Variables = [new Variable(variableName, coefficient)];
        }
    }
    /**
     * Sets given variables to monomial
     * @param variables A list os of variables
     */
    SetVariables(variables) {
        this.Variables = variables;
    }
    /**
     * Evaluates current expression and returns its numerical result.
     * All variables should be replaced with numerical values before execution.
     */
    Execute() {
        return this.Coefficient;
    }
    /**
     * Replaces variables with numerical values
     * @param replacementDictionary key value didctionary of variables to be replace
     */
    ReplaceVariables(replacementDictionary) {
        let variablesProduct = 1;
        if (this.Variables.length > 0) {
            this.Variables.forEach(variable => {
                if (replacementDictionary[variable.Name] !== undefined) {
                    variablesProduct *= variable.Evaluate(replacementDictionary[variable.Name]);
                }
            });
        }
        return new Monomial(variablesProduct * this.Coefficient);
    }
    CloneVarialbes() {
        const variables = [];
        this.Variables.forEach(v => {
            variables.push(new Variable(v.Name, v.Exponent));
        });
        return variables;
    }
    Clone() {
        var monomial = new Monomial(this.Coefficient);
        monomial.SetVariables(this.CloneVarialbes());
        return monomial;
    }
}

class MonomialResolver {
    /**
     * Parses a monomial from a given token
     * @param token Token
     */
    GetMonomial(token) {
        if (!token) {
            throw "Empty token!";
        }
        if (token.TypeOfToken !== TokenType.Number && token.TypeOfToken !== TokenType.Variable) {
            throw `Invalid token type: ${token.TypeOfToken}; value: ${token.Value}`;
        }
        if (token.TypeOfToken === TokenType.Number) {
            return new Monomial(parseFloat(token.Value));
        }
        const variables = [];
        let i = token.Value.length;
        while (i--) {
            const char = token.Value.charAt(i);
            const variable = new Variable(char, 1);
            variables.push(variable);
        }
        const monomial = new Monomial(1);
        monomial.SetVariables(variables);
        return monomial;
    }
}

class MinOperationFinder {
    IsTokenSmaller(t1, t2) {
        if (t1.Level < t2.Level) {
            return true;
        }
        if (t1.Level === t2.Level &&
            t1.Order > t2.Order) {
            return true;
        }
        return t1.Level === t2.Level &&
            t1.Order === t2.Order &&
            t1.Index > t2.Index;
    }
    /**
     * Finds a token with the lowest execution index
     * @param tokens A list of tokens
     */
    FindMinOperationIndex(tokens) {
        let minIndex = 0;
        let i = 0;
        let minToken = new Token();
        minToken.Level = 999;
        minToken.Order = -1;
        tokens.forEach(token => {
            if (token.TypeOfToken === TokenType.Operation &&
                this.IsTokenSmaller(token, minToken)) {
                minIndex = i;
                minToken = token;
            }
            i++;
        });
        return minIndex;
    }
}

var SymbolType;
(function (SymbolType) {
    SymbolType[SymbolType["Numeric"] = 0] = "Numeric";
    SymbolType[SymbolType["Parenthesis"] = 1] = "Parenthesis";
    SymbolType[SymbolType["Symbol"] = 2] = "Symbol";
    SymbolType[SymbolType["Other"] = 3] = "Other";
    SymbolType[SymbolType["Equality"] = 4] = "Equality";
})(SymbolType || (SymbolType = {}));

class TokenParser {
    constructor(symbolTypeChecker, tokenCreator, tokenFixer) {
        this.symbolTypeChecker = symbolTypeChecker;
        this.tokenCreator = tokenCreator;
        this.tokenFixer = tokenFixer;
    }
    AddToken(token) {
        let lastToken = this.GetLastToken();
        if (!token)
            return;
        let fixedToken = this.tokenFixer.GetAdditionalToken(lastToken, token);
        if (fixedToken) {
            fixedToken.Index = this.tokens.length;
            fixedToken.Level = lastToken ? lastToken.Level : token.Level;
            this.tokens.push(fixedToken);
        }
        token.Index = this.tokens.length;
        this.tokens.push(token);
    }
    GetLastToken() {
        return this.tokens[this.tokens.length - 1] || null;
    }
    /**
     * Parses given string equation to list of tokens
     * @param equation Mathematical equation
     * @returns A list of parsed tokens
     */
    ParseTokens(equation) {
        this.tokens = [];
        let currentLevel = 0;
        let currentToken = '';
        let lastSymbolType = SymbolType.Other;
        let currentSymbolType = lastSymbolType;
        let i = 0;
        while (i < equation.length) {
            const character = equation.charAt(i);
            if (!character || character.length === 0)
                continue;
            currentSymbolType = this.symbolTypeChecker.GetSymbolType(character);
            if (currentToken.length > 0 && lastSymbolType != currentSymbolType && i > 0) {
                this.AddToken(this.tokenCreator.GetTokenByLast(lastSymbolType, currentToken, currentLevel, this.GetLastToken()));
                currentToken = '';
            }
            if (currentSymbolType === SymbolType.Parenthesis) {
                currentLevel += character === '(' ? 1 : -1;
                if (character === '(') {
                    this.AddToken(this.tokenCreator.GetTokenByLast(SymbolType.Parenthesis, '(', currentLevel, this.GetLastToken()));
                }
            }
            else {
                currentToken += character;
            }
            i++;
            lastSymbolType = currentSymbolType;
        }
        if (currentToken.length > 0) {
            this.AddToken(this.tokenCreator.GetTokenByLast(lastSymbolType, currentToken, currentLevel, this.GetLastToken()));
        }
        if (currentLevel !== 0) {
            throw 'Unbalanced parenthesis';
        }
        return this.tokens;
    }
}

class SymbolTypeChecker {
    /**
     * Returs a symbol type of a single character
     * @param char Single character
     */
    GetSymbolType(char) {
        if ('()'.includes(char)) {
            return SymbolType.Parenthesis;
        }
        if ('+-*/^@$&!:'.includes(char)) {
            return SymbolType.Symbol;
        }
        if ('=<>'.includes(char)) {
            return SymbolType.Equality;
        }
        return "1234567890.,".includes(char) ? SymbolType.Numeric : SymbolType.Other;
    }
}

class TokenCreator {
    constructor(expressionFactory) {
        this.expressionFactory = expressionFactory;
    }
    /**
     * Returns a token from a given symbol and its parameters
     * @param type A type of a symbol
     * @param value String value of a symbol
     * @param level A level inside parenthesis
     * @param lastToken Previous token
     */
    GetTokenByLast(type, value, level, lastToken) {
        let token = new Token();
        token.Level = level;
        token.Value = value;
        if (type === SymbolType.Numeric) {
            token.Order = 0;
            token.Arity = 0;
            token.TypeOfToken = TokenType.Number;
            return token;
        }
        const expression = this.expressionFactory.GetExpressionByLastToken(value, lastToken);
        if (expression) {
            token.Order = expression.Order;
            token.Arity = expression.Arity;
            token.TypeOfToken = TokenType.Operation;
        }
        else {
            token.Order = 0;
            token.Arity = 0;
            token.TypeOfToken = TokenType.Variable;
        }
        return token;
    }
    /**
    * Returns a token from a given symbol and its parameters
    * @param type A type of a symbol
    * @param value String value of a symbol
    * @param level A level inside parenthesis
    */
    GetToken(type, value, level) {
        return this.GetTokenByLast(type, value, level, null);
    }
}

class TokenFixer {
    /**
     * Determines and returns implicit token that should intercept
     * between the current and the last token
     * @param lastToken Last processed token
     * @param currentToken Token that is being currently proccessed
     * @returns Required additional token or null
     */
    GetAdditionalToken(lastToken, currentToken) {
        if (!lastToken || !currentToken) {
            return null;
        }
        if ((lastToken.TypeOfToken === TokenType.Number ||
            lastToken.TypeOfToken === TokenType.Variable) &&
            currentToken.Value === '(') {
            return Token.MultiplyToken();
        }
        if (lastToken.Value === ')' && currentToken.Value == '(') {
            return Token.MultiplyToken();
        }
        if (currentToken.TypeOfToken == TokenType.Variable &&
            lastToken.TypeOfToken === 0 &&
            currentToken.Value !== '0') {
            return Token.MultiplyToken();
        }
        return null;
    }
}

class Parser {
    constructor() {
        this.expressionCreator = new ExpressionCreator(new ExpressionFactory(), new MonomialResolver(), new MinOperationFinder());
        this.tokenParser = new TokenParser(new SymbolTypeChecker(), new TokenCreator(new ExpressionFactory()), new TokenFixer());
    }
    /**
     * Parses an equation to computable expression
     * @param equation Given string equation
     */
    Parse(equation) {
        const tokens = this.tokenParser.ParseTokens(equation);
        return this.expressionCreator.CreateExpression(tokens);
    }
}

class Interpreter {
    constructor() {
        this.parser = new Parser();
    }
    /**
     * Parses given string expression to computable expression
     * @param expression A string expression
     * @returns Computable expression
     */
    CreateExpression(expression) {
        return this.parser.Parse(expression);
    }
    /**
     * Executes numerical expression (without variables)
     * @param expression A string expression
     * @returns Numerical result
     */
    ExecuteExpression(expression) {
        const parsedExpression = this.CreateExpression(expression);
        return this.ExecuteParsedExpression(parsedExpression);
    }
    /**
     * Executes parsed numerical expression (without variables)
     * @param expression A parsed expression
     * @returns Numerical result
     */
    ExecuteParsedExpression(expression) {
        return expression.Execute();
    }
    /**
     * Evaluates string expression result using given variable replacement values
     * @param expression A string expression
     * @param replacementDictionary Variable replacement values: {x: 2, y : 4}
     */
    EvaluateExpression(expression, replacementDictionary) {
        const parsedExpression = this.CreateExpression(expression);
        return this.EvaluateParsedExpression(parsedExpression, replacementDictionary);
    }
    /**
     * Evaluates parsed expression result using given variable replacement values
     * @param expression A parsed expression
     * @param replacementDictionary Variable replacement values: {x: 2, y : 4}
     */
    EvaluateParsedExpression(expression, replacementDictionary) {
        expression = expression.ReplaceVariables(replacementDictionary);
        return expression.Execute();
    }
}

/**
 * Representation of a function in a graph
 */
class GraphFunction {
    constructor(fx, fxString) {
        this.Function = fx;
        this.IsVisible = true;
        this.FunctionString = fxString;
    }
}

/**
 * Result of a single function evaluation
 */
class FunctionEvaluationResult {
    constructor(results, fx, fxString) {
        this.Results = results;
        this.Function = fx;
        this.FunctionString = fxString;
    }
}

/**
 * Result of a single function evaluation at value x
 */
class FunctionStepEvaluationResult {
    constructor(x, y, defined) {
        this.x = x;
        this.y = y;
        this.isDefined = defined;
    }
}

/**
 * Class that manages function values in a graph
 */
class MathFuncions {
    constructor() {
        /**
         * Original parsed expressions
         */
        this.Functions = [];
        this.interpreter = new Interpreter();
    }
    /**
     * Adds a new function to a graph
     * @param functionExpression String function expression
     */
    AddFunction(functionExpression) {
        const parsedExpression = this.interpreter.CreateExpression(functionExpression);
        if (!parsedExpression) {
            throw `Unable to parse a function: ${functionExpression}`;
        }
        this.Functions.push(new GraphFunction(parsedExpression, functionExpression));
    }
    /**
     * Removes a function from a graph by index
     * @param index Function index to remove
     */
    RemoveFunction(index) {
        this.Functions.splice(index, 0);
    }
    /**
     * Sets evaluation boundaries for all functions
     * @param start Function evaluation start
     * @param step Function evaluation step
     * @param count Evaluation step count
     */
    SetEvaluationBoundaries(start, step, count) {
        this.EvaluationStart = start;
        this.EvaluationStep = step;
        this.EvaluationCount = count;
    }
    /**
     * Evaluates given function value at a given result
     * @param fx Function expression
     * @param x Evaluation point
     */
    EvaluateFunctionAt(fx, x) {
        const expression = fx.Clone();
        try {
            let result = this.interpreter.EvaluateParsedExpression(expression, { x });
            return new FunctionStepEvaluationResult(x, result, true);
        }
        catch (e) {
            return new FunctionStepEvaluationResult(0, 0, false);
        }
    }
    /**
     * Evaluates a function in a current evaluation boundary
     * @param fx Function expression
     * @returns Function evaluation result
     */
    EvaluateFunction(fx, fxString) {
        let xValue = this.EvaluationStart;
        const results = [];
        for (var i = 0; i < this.EvaluationCount; i++) {
            const stepEvaluation = this.EvaluateFunctionAt(fx, xValue);
            xValue += this.EvaluationStep;
            results.push(stepEvaluation);
        }
        return new FunctionEvaluationResult(results, fx, fxString);
    }
    /**
     * Evaluates all current functions if they are visible
     * @returns Array of function evaluation results
     */
    EvaluateFunctions() {
        const results = [];
        this.Functions.forEach(fx => {
            if (fx.IsVisible) {
                const result = this.EvaluateFunction(fx.Function, fx.FunctionString);
                results.push(result);
            }
        });
        return results;
    }
}

class Viewport {
    constructor(x, y, scale) {
        this.StartX = x;
        this.StartY = y;
        this.Scale = scale;
    }
}

class GridWidth {
    constructor(unit, width, dash = false) {
        this.unit = unit;
        this.width = width;
        this.dash = dash;
    }
}
class GridWidthSetting {
    constructor(scale, widths) {
        this.Scale = scale;
        this.GridWidths = widths;
    }
}
class LabelSetting {
    constructor(scale, value) {
        this.Value = value;
        this.Scale = scale;
    }
}
class GraphSettings {
    constructor(params) {
        this.drawGrid = params.drawGrid === undefined ? true : params.drawGrid;
        this.drawFunctions = params.drawFunctions === undefined ? true : params.drawFunctions;
        this.gridColor = params.gridColor || '#75ea88';
        this.labelColor = params.labelColor || '#000';
        this.drawGridLabels = params.drawGridLabels === undefined ? true : params.drawGridLabels;
        this.labelFont = params.labelFont || '14px serif';
        this.canResize = params.canResize === undefined ? true : params.canResize;
        this.canMove = params.canMove === undefined ? true : params.canMove;
        this.functionColors = params.functionColors || [
            '#4286f4',
            '#f44141',
            '#772f93',
            '#e8a23a'
        ];
        this.gridWidths = [
            new GridWidthSetting(2, [new GridWidth(1, 1), new GridWidth(0.25, 0.5, true)]),
            new GridWidthSetting(0.2, [new GridWidth(5, 1), new GridWidth(1, 1, true)]),
            new GridWidthSetting(0.1, [new GridWidth(20, 2), new GridWidth(5, 1)]),
            new GridWidthSetting(Number.EPSILON, [new GridWidth(50, 2), new GridWidth(10, 1)])
        ];
        this.labelDisplay = [
            new LabelSetting(2, 1),
            new LabelSetting(0.2, 1),
            new LabelSetting(0.1, 5),
            new LabelSetting(Number.EPSILON, 10)
        ];
    }
}

/**
 * Holds variables required for drawing a graph
 */
class GraphDrawParameters {
    constructor(startingPointX, startingOffsetX, adjustedPixelsPerValue, startingPointY, startingOffsetY, baseX, baseY) {
        this.StartingPointX = startingPointX;
        this.StartingOffsetX = startingOffsetX;
        this.AdjustedPixelsPerValue = adjustedPixelsPerValue;
        this.StartingPointY = startingPointY;
        this.StartingOffsetY = startingOffsetY;
        this.BaseX = baseX;
        this.BaseY = baseY;
    }
}
const minStep$1 = 0.25;
/**
 * Computes graph drawing parameters
 */
class GraphDrawParametersComputor {
    constructor(viewport, canvasContext) {
        this.viewport = viewport;
        this.canvasContext = canvasContext;
    }
    /**
     * Returns nearest min step to given value
     * @param value Numerical value
     * @returns Nearest min step value
     */
    GetNearestValidPoint(value) {
        const scale = value / minStep$1;
        return Math.ceil(scale) * minStep$1;
    }
    /**
     * Computes and returns graph drawing parameters
     * @param pixelsPerValue How many pixels a single graph value take
     * @returns Graph drawing parameters
     */
    GetParameters(pixelsPerValue) {
        const startingPointX = this.GetNearestValidPoint(this.viewport.StartX);
        const startingOffsetX = (startingPointX - this.viewport.StartX) * pixelsPerValue;
        const adjustedPixelsPerValue = pixelsPerValue * minStep$1;
        const startingPointY = this.GetNearestValidPoint(this.viewport.StartY);
        const startingOffsetY = this.canvasContext.Height - (startingPointY - this.viewport.StartY) * pixelsPerValue;
        const baseY = this.canvasContext.Height + this.viewport.StartY * pixelsPerValue;
        const baseX = -this.viewport.StartX * pixelsPerValue;
        return new GraphDrawParameters(startingPointX, startingOffsetX, adjustedPixelsPerValue, startingPointY, startingOffsetY, baseX, baseY);
    }
}

const minStep = 0.25;
class GridDrawer {
    constructor(viewport, settings, canvasContext) {
        this.viewport = viewport;
        this.settings = settings;
        this.canvasContext = canvasContext;
    }
    DrawGrid(pixelsPerValue) {
        const paramsComputor = new GraphDrawParametersComputor(this.viewport, this.canvasContext);
        const graphParameters = paramsComputor.GetParameters(pixelsPerValue);
        let ctx = this.canvasContext.Context;
        ctx.strokeStyle = this.settings.gridColor;
        let currentPixelsX = graphParameters.StartingOffsetX;
        let currentX = graphParameters.StartingPointX;
        const scaleSettings = this.settings.gridWidths.find(s => s.Scale <= this.viewport.Scale);
        // Draw vertical grid
        while (currentPixelsX < this.canvasContext.Width) {
            this.TryDrawVertical(ctx, scaleSettings, currentPixelsX, currentX, this.canvasContext.Height);
            currentPixelsX += graphParameters.AdjustedPixelsPerValue;
            currentX += minStep;
        }
        // Draw horizontal grid
        let currentPixelsY = graphParameters.StartingOffsetY;
        let currentY = graphParameters.StartingPointY;
        while (currentPixelsY > 0) {
            this.TryDrawHorizontal(ctx, scaleSettings, currentPixelsY, currentY, this.canvasContext.Width);
            currentPixelsY -= graphParameters.AdjustedPixelsPerValue;
            currentY += minStep;
        }
    }
    TryDrawVertical(ctx, settings, pixelsX, x, height) {
        let gridValue = settings.GridWidths.find((s) => Math.abs(x % s.unit) < 0.0001);
        if (gridValue) {
            ctx.beginPath();
            if (gridValue.dash) {
                ctx.setLineDash([5]);
            }
            else {
                ctx.setLineDash([]);
            }
            ctx.lineWidth = Math.abs(x) < 0.001 ? 2 : gridValue.width;
            ctx.moveTo(pixelsX, 0);
            ctx.lineTo(pixelsX, height);
            ctx.stroke();
        }
    }
    TryDrawHorizontal(ctx, settings, pixelsY, y, width) {
        let gridValue = settings.GridWidths.find(s => Math.abs(y % s.unit) < 0.0001);
        if (gridValue) {
            ctx.beginPath();
            if (gridValue.dash) {
                ctx.setLineDash([5]);
            }
            else {
                ctx.setLineDash([]);
            }
            ctx.lineWidth = Math.abs(y) < 0.001 ? 2 : gridValue.width;
            ctx.moveTo(0, pixelsY);
            ctx.lineTo(width, pixelsY);
            ctx.stroke();
        }
    }
}

const minStep$2 = 0.25;
/**
 * Manages graph grid labels
 */
class LabelDrawer {
    constructor(viewport, settings, canvasContext) {
        this.viewport = viewport;
        this.settings = settings;
        this.canvasContext = canvasContext;
    }
    /**
     * Draws graph labels
     * @param pixelsPerValue Pixels per single graph value
     */
    Draw(pixelsPerValue) {
        const paramsComputor = new GraphDrawParametersComputor(this.viewport, this.canvasContext);
        const graphParameters = paramsComputor.GetParameters(pixelsPerValue);
        const ctx = this.canvasContext.Context;
        ctx.strokeStyle = this.settings.labelColor;
        let currentPixelsX = graphParameters.StartingOffsetX;
        let currentX = graphParameters.StartingPointX;
        let currentPixelsY = graphParameters.StartingOffsetY;
        let currentY = graphParameters.StartingPointY;
        const labelSettings = this.settings.labelDisplay.find(s => s.Scale <= this.viewport.Scale);
        // Draw X labels
        while (currentPixelsX < this.canvasContext.Width) {
            this.TryDrawXLabel(ctx, labelSettings, currentPixelsX, currentX, graphParameters.BaseY);
            currentPixelsX += graphParameters.AdjustedPixelsPerValue;
            currentX += minStep$2;
        }
        // Draw Y labels
        while (currentPixelsY > 0) {
            this.TryDrawYLabel(ctx, labelSettings, currentPixelsY, currentY, graphParameters.BaseX);
            currentPixelsY -= graphParameters.AdjustedPixelsPerValue;
            currentY += minStep$2;
        }
    }
    /**
     * Draws label on X axis
     * @param ctx Drawing context
     * @param setting Label setting
     * @param pixelsX X coordinate
     * @param x Current x value
     * @param baseY Base Y coordinate
     */
    TryDrawXLabel(ctx, setting, pixelsX, x, baseY) {
        if (Math.abs(x % setting.Value) < 0.001) {
            ctx.font = this.settings.labelFont;
            ctx.fillText(x.toString(), pixelsX, baseY + 5);
        }
    }
    /**
     * Draws label on Y axis
     * @param ctx Drawing context
     * @param setting Label setting
     * @param pixelsY Y coordinate
     * @param y Current Y value
     * @param baseX Base X coordinate
     */
    TryDrawYLabel(ctx, setting, pixelsY, y, baseX) {
        if (Math.abs(y % setting.Value) < 0.001 && Math.abs(y) > 0.01) {
            ctx.font = this.settings.labelFont;
            ctx.fillText(y.toString(), baseX + 5, pixelsY);
        }
    }
}

/**
 * Manages drawing of functions in a graph
 */
class FunctionsDrawer {
    constructor(viewport, canvasContext, graphSettings, pixelsPerValue) {
        this.viewport = viewport;
        this.canvasContext = canvasContext;
        this.graphSettings = graphSettings;
        this.pixelsPerValue = pixelsPerValue;
        const paramsComputor = new GraphDrawParametersComputor(this.viewport, this.canvasContext);
        this.GraphParameters = paramsComputor.GetParameters(pixelsPerValue);
    }
    /**
     * Draws given functions in a graph
     * @param functions Functions to draws
     */
    DrawFunctions(functions) {
        functions.forEach((f, i) => this.DrawFunction(f, i));
    }
    /**
     * Draws a single function in a graph
     * @param evaluatedFunction Function
     * @param index Index of a function
     */
    DrawFunction(evaluatedFunction, index) {
        const ctx = this.canvasContext.Context;
        ctx.strokeStyle = this.graphSettings.functionColors[index];
        ctx.setLineDash([]);
        const results = evaluatedFunction.Results;
        for (var i = 0; i < results.length - 1; i++) {
            if (results[i].isDefined && results[i + 1].isDefined) {
                var y0 = this.GraphParameters.BaseY - results[i].y * this.pixelsPerValue;
                var y1 = this.GraphParameters.BaseY - results[i + 1].y * this.pixelsPerValue;
                if (Math.abs(y0 - y1) > 1000) {
                    continue;
                }
                ctx.beginPath();
                ctx.moveTo(this.GraphParameters.BaseX + results[i].x * this.pixelsPerValue, y0);
                ctx.lineTo(this.GraphParameters.BaseX + results[i + 1].x * this.pixelsPerValue, y1);
                ctx.closePath();
                ctx.stroke();
            }
        }
    }
}

/**
 * Class that performs functions drawing in graph
 */
class GraphDrawer {
    constructor() {
        this.Settings = new GraphSettings({});
    }
    /**
     * Updates current drawer settings
     * @param settings Graph settings replacement
     */
    UpdateSettings(settings) {
        this.Settings = new GraphSettings(settings);
    }
    /**
     * Draw funtion evaluation results in a graph
     * @param canvasContext Graph container context
     * @param functions Functions evaluation result
     * @param viewport Graph viewport
     * @param pixelsPerValueBase How many pixels per single value for a scale of 1
     */
    Draw(context, functions, viewport, pixelsPerValueBase) {
        context.Context.clearRect(0, 0, context.Width, context.Height);
        const scaledPixelsPerValue = pixelsPerValueBase * viewport.Scale;
        const gridDrawer = new GridDrawer(viewport, this.Settings, context);
        const labelDrawer = new LabelDrawer(viewport, this.Settings, context);
        const functionsDrawer = new FunctionsDrawer(viewport, context, this.Settings, scaledPixelsPerValue);
        if (this.Settings.drawGrid) {
            gridDrawer.DrawGrid(scaledPixelsPerValue);
        }
        if (this.Settings.drawFunctions) {
            functionsDrawer.DrawFunctions(functions);
        }
        if (this.Settings.drawGridLabels) {
            labelDrawer.Draw(scaledPixelsPerValue);
        }
        functions = [];
    }
}

class CanvasContext {
}

const prepareCanvas = (container) => {
    let canvas = document.getElementById(`${container}`);
    let context = canvas.getContext('2d');
    context.moveTo(0, 0);
    const canvasContext = new CanvasContext();
    let rect = canvas.getBoundingClientRect();
    canvasContext.Context = context;
    canvasContext.Height = canvas.height;
    canvasContext.Width = canvas.width;
    canvasContext.X = rect.left;
    canvasContext.Y = rect.top;
    return canvasContext;
};
/**
 * Class that represents a graph with functions
 */
class FunctionsGraph {
    /**
     * Initialize graph
     * @param settings Initial graph settings
     */
    constructor(settings) {
        this.PixelsPerValueBase = 50;
        this.Functions = new MathFuncions();
        this.GraphDrawer = new GraphDrawer();
        this.Settings = new GraphSettings(settings || {});
        this.GraphDrawer.UpdateSettings(settings || {});
    }
    /**
     * Initializes resize event listener on canvas
     * @param container Canvas id
     */
    InitResize(container) {
        if (!this.Settings.canResize) {
            return;
        }
        const canvas = document.getElementById(container);
        const wheelEvt = 'onwheel' in document.createElement('div') ? 'wheel' :
            document.onmousewheel !== undefined ? 'mousewheel' : 'DOMMouseScroll';
        canvas.addEventListener(wheelEvt, this.ProcessResize.bind(this));
    }
    /**
     * Initializes dragging
     * @param e Mouse event
     */
    ProcessMouseDown(e) {
        if (!this.Settings.canMove) {
            return;
        }
        this.dragging = true;
        this.startX = e.x;
        this.startY = e.y;
    }
    /**
     * Stops dragging
     */
    ProcessMouseUp() {
        if (!this.Settings.canMove) {
            return;
        }
        this.dragging = false;
    }
    /**
     * Processes graph dragging
     * @param e Mouse event
     */
    ProcessMouseMove(e) {
        if (!this.Settings.canMove) {
            return;
        }
        if (this.dragging) {
            const dx = e.x - this.startX;
            const dy = e.y - this.startY;
            this.SetViewport(this.Viewport.StartX -= dx / (this.PixelsPerValueBase * this.Viewport.Scale), this.Viewport.StartY += dy / (this.PixelsPerValueBase * this.Viewport.Scale), this.Viewport.Scale);
            this.startX = e.x;
            this.startY = e.y;
            this.Draw();
        }
    }
    InitMove(container) {
        if (!this.Settings.canMove) {
            return;
        }
        const canvas = document.getElementById(container);
        canvas.addEventListener('mousedown', this.ProcessMouseDown.bind(this));
        canvas.addEventListener('mousemove', this.ProcessMouseMove.bind(this));
        canvas.addEventListener('mouseup', this.ProcessMouseUp.bind(this));
    }
    /**
     * Processes graph resize event by increasing/decreasing scale
     * @param event Mouse wheel event
     */
    ProcessResize(event) {
        event.preventDefault();
        if (!this.Settings.canResize) {
            return;
        }
        let scale = 1;
        if (event.deltaY < 0) {
            scale = 1.05;
        }
        else {
            scale = 0.95;
        }
        const newScale = this.Viewport.Scale * scale;
        let adb = this.PixelsPerValueBase * this.Viewport.Scale;
        let paramsComputor = new GraphDrawParametersComputor(this.Viewport, this.Context);
        let graphParameters = paramsComputor.GetParameters(adb);
        const fixedEventX = event.x - this.Context.X + window.scrollX;
        const fixedEventY = event.y - this.Context.Y + window.scrollY;
        const currentX = (fixedEventX - graphParameters.BaseX) / adb;
        const currentY = -(fixedEventY - graphParameters.BaseY) / adb;
        const newViewport = new Viewport(this.Viewport.StartX, this.Viewport.StartY, this.Viewport.Scale * scale);
        paramsComputor = new GraphDrawParametersComputor(newViewport, this.Context);
        const newadb = this.PixelsPerValueBase * newScale;
        graphParameters = paramsComputor.GetParameters(newadb);
        const newXC = currentX * newadb + graphParameters.BaseX;
        const newYC = -currentY * newadb + graphParameters.BaseY;
        const dx = (fixedEventX - newXC) / newadb;
        const dy = (fixedEventY - newYC) / newadb;
        this.SetViewport(this.Viewport.StartX - dx, this.Viewport.StartY + dy, this.Viewport.Scale * scale);
        this.Draw();
    }
    /**
     * Sets a container for a graph
     * @param container Id of html container
     */
    SetContainer(container) {
        this.Container = container;
        this.Context = prepareCanvas(this.Container);
        this.InitResize(container);
        this.InitMove(container);
    }
    /**
     * Adds a new function to a graph
     * @param functionExpression String function expression
     */
    AddFunction(functionExpression) {
        this.Functions.AddFunction(functionExpression);
        this.Draw();
    }
    /**
     * Removes a function from a graph by index
     * @param index Function index to remove
     */
    RemoveFunction(index) {
        this.Functions.RemoveFunction(index);
        this.Draw();
    }
    /**
     * Sets evaluation boundaries for all functions
     * @param start Function evaluation start
     * @param step Function evaluation step
     * @param count Evaluation step count
     */
    SetEvaluationBoundaries(start, step, count) {
        this.Functions.SetEvaluationBoundaries(start, step, count);
    }
    /**
     * Sets current viewport
     * @param startX Start x
     * @param startY Start y
     * @param scale Scale
     */
    SetViewport(startX, startY, scale = 1) {
        this.Viewport = new Viewport(startX, startY, scale);
        this.SetEvaluationBoundaries(startX, 0.1 / scale, (this.Context.Width / (this.PixelsPerValueBase * scale)) * 10 * scale);
    }
    /**
     * Redraws graph with all visible functions
     */
    Draw() {
        const evaluation = this.Functions.EvaluateFunctions();
        this.GraphDrawer.Draw(this.Context, evaluation, this.Viewport, this.PixelsPerValueBase);
    }
    /**
     * Updates graph settings
     * @param newSettings Settings replacement object
     */
    UpdateSettings(newSettings) {
        this.GraphDrawer.UpdateSettings(newSettings);
        this.Settings = new GraphSettings(newSettings);
    }
}

var interpreter = new Interpreter();
console.log(interpreter.ExecuteExpression('2+2'));
console.log(interpreter.ExecuteExpression('sqrt(0)'));
document.addEventListener("DOMContentLoaded", function (event) {
    var graph = new FunctionsGraph({});
    graph.SetContainer('testCanvas');
    graph.SetViewport(-8, -2, 1);
    graph.AddFunction('x^2');
    graph.AddFunction('lg(-x)');
    graph.AddFunction('ln(x+3)');
    graph.Draw();
    var graph2 = new FunctionsGraph({});
    graph2.SetContainer('graph2');
    graph2.SetViewport(-4, -3, 1);
    graph2.AddFunction('x^3');
    graph2.AddFunction('sqrt(x+2)');
    graph2.AddFunction('2/x');
    graph2.Draw();
    var graph3 = new FunctionsGraph({ canResize: false, canMove: false });
    graph3.SetContainer('graph3');
    graph3.SetViewport(-1, -2, 1);
    graph3.AddFunction('x-3');
    graph3.AddFunction('-x+4');
    graph3.Draw();
    var graph4 = new FunctionsGraph({ drawGrid: false, drawGridLabels: false });
    graph4.SetContainer('graph4');
    graph4.SetViewport(-1, -2, 1);
    graph4.AddFunction('x^(x-3)');
    graph4.AddFunction('x^4+2x^3-6x^2');
    graph4.Draw();
    var functionColors = [
        '#ad8708',
        '#edbd1e',
        '#ffd64f'
    ];
    var graph5 = new FunctionsGraph({
        functionColors: functionColors,
        labelFont: '12px monospace',
        gridColor: '#c8daf7'
    });
    graph5.SetContainer('graph5');
    graph5.SetViewport(-6, -4, 0.7);
    graph5.AddFunction('x^3-3x^2+1');
    graph5.AddFunction('2^x');
    graph5.AddFunction('sin(x)+x');
    graph5.Draw();
});

}());
//# sourceMappingURL=test.js.map
