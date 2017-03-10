using System;
using System.Collections.Generic;
using System.Composition;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace IMuravevSoft.AtlasRefactor
{
    [ExportCodeRefactoringProvider(LanguageNames.CSharp, Name = nameof(AtlasCodeRefactoringProvider)), Shared]
    // ReSharper disable once InconsistentNaming
    internal class AtlasCodeRefactoringProvider : CodeRefactoringProvider
    {
        public sealed override async Task ComputeRefactoringsAsync(CodeRefactoringContext context)
        {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
            var node = root.FindNode(context.Span);
            var classDecl = node as ClassDeclarationSyntax;
            if (classDecl != null)
            {
                var action = CodeAction.Create("Create wrappers", c => CreateSettingsWrappers(context.Document, classDecl, c));
                context.RegisterRefactoring(action);
            }

            var methodDecl = node as MethodDeclarationSyntax;
            if (methodDecl != null)
            {
                var isWriter = false;
                var isReader = false;
                var identifier = String.Empty;
                foreach (var parameter in methodDecl.ParameterList.Parameters)
                {
                    if (parameter.Type.ToFullString().Contains("TSUFFWriter"))
                    {
                        isWriter = true;
                        identifier = parameter.Identifier.ToString();
                        break;
                    }
                    if (parameter.Type.ToFullString().Contains("TSUFFReader"))
                    {
                        isReader = true;
                        identifier = parameter.Identifier.ToString();
                        break;
                    }
                }

                if (isWriter || isReader)
                {
                    var action = CodeAction.Create("Create TSUFF template",
                        c => CreateTsuffTemplate(context.Document, methodDecl, isReader, identifier, c));
                    context.RegisterRefactoring(action);
                }
            }
        }

        private static Dictionary<string, string> GetAttributes(INamedTypeSymbol namedTypeSymbol, CancellationToken token)
        {
            var dictionarySolution = new Dictionary<string, string>();
            if (namedTypeSymbol != null && namedTypeSymbol.DeclaredAccessibility != Accessibility.Private)
            {
                var attrDictionary = new Dictionary<string, ITypeSymbol>();
                var defaultPair = default(KeyValuePair<string, ITypeSymbol>);
                foreach (var attrData in namedTypeSymbol.GetAttributes())
                {
                    var type = attrData.AttributeClass;
                    if (type.Name.Contains("RequiredSettings"))
                    {
                        var settingsName = attrData.ConstructorArguments[0].Value;
                        var settingsType = attrData.ConstructorArguments[1].Value as INamedTypeSymbol;
                        var baseType = settingsType?.BaseType;
                        while (baseType != null)
                        {
                            if (baseType.Name.Contains("SettingsForStruct")) break;
                            if (baseType.Name.Contains("SettingsBase")) break;
                            baseType = baseType.BaseType;
                        }
                        var typeArg = baseType?.TypeArguments[0];
                        if (typeArg != null)
                            attrDictionary.Add(settingsName.ToString(), typeArg);
                    }
                }

                foreach (var symbol in namedTypeSymbol.GetMembers().Where(x => x.Kind == SymbolKind.Method))
                {
                    if (attrDictionary.Count == 0) break;
                    var member = (IMethodSymbol)symbol;
                    if (member.MethodKind == MethodKind.Constructor) continue;
                    var returnType = member.ReturnType;
                    var syntax = member.DeclaringSyntaxReferences[0].GetSyntax(token);
                    var text = syntax.ToString().Replace(" ", "").Replace(@"\\", @"\");
                    var removeAttr = defaultPair;
                    foreach (var attribute in attrDictionary)
                    {
                        var findText = @"return[\s\S]*" + Regex.Escape(String.Format("{1}[\"{0}\"].Value", attribute.Key, "Settings"));
                        if (Regex.IsMatch(text, findText) && Equals(attribute.Value, returnType))
                        {
                            removeAttr = attribute;
                            break;
                        }
                    }
                    if (!(removeAttr.Key == defaultPair.Key && Equals(removeAttr.Value, defaultPair.Value)))
                        attrDictionary.Remove(removeAttr.Key);

                }
                if (attrDictionary.Count > 0)
                {
                    foreach (var symbol in attrDictionary)
                        dictionarySolution.Add(symbol.Key, symbol.Value.ToDisplayString());
                }
            }
            return dictionarySolution;
        }

        private async Task<Document> CreateSettingsWrappers(Document document, ClassDeclarationSyntax classDecl, CancellationToken cancellationToken)
        {
            var semanticModel = await document.GetSemanticModelAsync(cancellationToken);
            var typeSymbol = semanticModel.GetDeclaredSymbol(classDecl, cancellationToken);
            var properties = GetAttributes(typeSymbol, cancellationToken);

            var list = new SyntaxList<MemberDeclarationSyntax>();
            foreach (var property in properties)
            {
                var name = property.Key.Replace(@"\", "");
                var type = SyntaxFactory.ParseTypeName(property.Value);
                var p = SyntaxFactory.PropertyDeclaration(type, name).AddModifiers(SyntaxFactory.Token(SyntaxKind.PublicKeyword));
                var acessSyntax = SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression,
                    SyntaxFactory.ElementAccessExpression(
                        SyntaxFactory.IdentifierName("Settings"),
                        SyntaxFactory.BracketedArgumentList(
                            SyntaxFactory.Token(SyntaxKind.OpenBracketToken),
                            SyntaxFactory.SeparatedList(
                                new[]
                                {
                                    SyntaxFactory.Argument(
                                        SyntaxFactory.LiteralExpression(SyntaxKind.StringLiteralExpression,
                                            SyntaxFactory.Literal(property.Key))),
                                }
                                ),
                            SyntaxFactory.Token(SyntaxKind.CloseBracketToken)))
                    , SyntaxFactory.IdentifierName("Value"));


                var getExp = SyntaxFactory.ReturnStatement(SyntaxFactory.CastExpression(type, acessSyntax));
                var setExp = SyntaxFactory.AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, acessSyntax,
                    SyntaxFactory.IdentifierName("value"));

                p = p.AddAccessorListAccessors(
                    SyntaxFactory.AccessorDeclaration(SyntaxKind.GetAccessorDeclaration,
                    SyntaxFactory.Block(SyntaxFactory.List(new[] { getExp }))));

                p = p.AddAccessorListAccessors(
                    SyntaxFactory.AccessorDeclaration(SyntaxKind.SetAccessorDeclaration,
                    SyntaxFactory.Block(SyntaxFactory.List(new[] { SyntaxFactory.ExpressionStatement(setExp) }))));

                list = list.Add(p);
            }

            var newClassDecl = classDecl.WithMembers(classDecl.Members.InsertRange(0, list));
            var root = await document.GetSyntaxRootAsync(cancellationToken);
            return document.WithSyntaxRoot(root.ReplaceNode(classDecl, newClassDecl));

        }

        private MethodDeclarationSyntax GetWriterSyntax(SemanticModel model,ClassDeclarationSyntax classDecl, MethodDeclarationSyntax methodDecl, string id)
        {
            var list = new SyntaxList<StatementSyntax>();
            foreach (var member in classDecl.Members)
            {
                if (member.IsKind(SyntaxKind.FieldDeclaration) || member.IsKind(SyntaxKind.PropertyDeclaration))
                {
                    
                    var memberSyntax = member as FieldDeclarationSyntax;
                    var propertySyntax = member as PropertyDeclarationSyntax;

                    var type = memberSyntax != null ? memberSyntax.Declaration.Type : propertySyntax.Type;
                    var name = memberSyntax != null ? memberSyntax.Declaration.Variables[0].ToString() : propertySyntax.Identifier.ToString();

                    var symbol = model.GetSymbolInfo(type).Symbol as ITypeSymbol;

                    var isDic = false;
                    var isList = false;
                    if (symbol.Name != "String")
                    {
                        foreach (var i in symbol.AllInterfaces)
                        {
                            if (i.Name.Contains("IDictionary"))
                                isDic = true;
                            if (i.Name.Contains("IEnumerable"))
                                isList = true;
                        }
                    }

                    var identifierMethod = isDic ? "WriteDictionary" : isList ? "WriteEnumerable" : "Write";
                   
                    var exp =
                    SyntaxFactory.InvocationExpression(
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            SyntaxFactory.IdentifierName(id),
                            SyntaxFactory.IdentifierName(identifierMethod)
                        ),
                        SyntaxFactory.ArgumentList(
                            SyntaxFactory.SeparatedList(
                                new[]
                                {
                                    SyntaxFactory.Argument(
                                        SyntaxFactory.LiteralExpression(
                                            SyntaxKind.StringLiteralExpression,
                                            SyntaxFactory.Literal(name)
                                        )
                                    ),

                                    SyntaxFactory.Argument(
                                        SyntaxFactory.IdentifierName(name)
                                    )
                                }
                            )
                        )
                    );
                    list = list.Add(SyntaxFactory.ExpressionStatement(exp));
                }
            }

            return methodDecl.WithBody(methodDecl.Body.WithStatements(list));
        }

        private MethodDeclarationSyntax GetReadSyntax(SemanticModel model, ClassDeclarationSyntax classDecl, MethodDeclarationSyntax methodDecl, string id)
        {
            var list = new SyntaxList<StatementSyntax>();
            foreach (var member in classDecl.Members)
            {
                if (member.IsKind(SyntaxKind.FieldDeclaration) || member.IsKind(SyntaxKind.PropertyDeclaration))
                {
                    var memberSyntax = member as FieldDeclarationSyntax;
                    var propertySyntax = member as PropertyDeclarationSyntax;

                    var type = memberSyntax != null ? memberSyntax.Declaration.Type : propertySyntax.Type;
                    var name = memberSyntax != null ? memberSyntax.Declaration.Variables[0].ToString() : propertySyntax.Identifier.ToString();

                    var symbol = model.GetSymbolInfo(type).Symbol as ITypeSymbol;

                    var isDic = false;
                    var isList = false;
                    if (symbol.Name != "String")
                    {
                        foreach (var i in symbol.AllInterfaces)
                        {
                            if (i.Name.Contains("IDictionary"))
                                isDic = true;
                            if (i.Name.Contains("IEnumerable"))
                                isList = true;
                        }
                    }

                    var identifierMethod = isDic ? "ReadDictionary" : isList ? "ReadEnumerable" : "Read";


                    ExpressionSyntax expression;
                    if (isDic || isList)
                        expression = SyntaxFactory.ObjectCreationExpression(type).WithArgumentList(SyntaxFactory.ArgumentList());
                    else
                        expression = SyntaxFactory.DefaultExpression(type);

                    var exp =
                    SyntaxFactory.InvocationExpression(
                        SyntaxFactory.MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            SyntaxFactory.IdentifierName(id),
                            SyntaxFactory.IdentifierName(identifierMethod)
                        ),
                        SyntaxFactory.ArgumentList(
                            SyntaxFactory.SeparatedList(
                                new[]
                                {
                                    SyntaxFactory.Argument(
                                        SyntaxFactory.LiteralExpression(
                                            SyntaxKind.StringLiteralExpression,
                                            SyntaxFactory.Literal(name)
                                        )
                                    ),

                                    SyntaxFactory.Argument(expression)
                                }
                            )
                        )
                    );

                    var toList = SyntaxFactory.AssignmentExpression(
                        SyntaxKind.SimpleAssignmentExpression,
                        SyntaxFactory.IdentifierName(name),
                        exp
                   );
                    list = list.Add(SyntaxFactory.ExpressionStatement(toList));
                }
            }
            return methodDecl.WithBody(methodDecl.Body.WithStatements(list));
        }

        private async Task<Document> CreateTsuffTemplate(Document document, MethodDeclarationSyntax methodDecl, bool isReader, string id,
            CancellationToken cancellationToken)
        {
            var classDecl = methodDecl.Parent as ClassDeclarationSyntax;
            if (classDecl == null) return document;

            var semanticModel = await document.GetSemanticModelAsync(cancellationToken);

            var newMethodDeclaration = isReader ? GetReadSyntax(semanticModel,classDecl, methodDecl, id) : GetWriterSyntax(semanticModel,classDecl, methodDecl, id);

            var root = await document.GetSyntaxRootAsync(cancellationToken);
            return document.WithSyntaxRoot(root.ReplaceNode(methodDecl, newMethodDeclaration));



        }
    }
}