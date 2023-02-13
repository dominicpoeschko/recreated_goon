#include "clang/AST/ASTConsumer.h"
#include "clang/AST/QualTypeNames.h"
#include "clang/AST/RecursiveASTVisitor.h"
#include "clang/Driver/Options.h"
#include "clang/Frontend/ASTConsumers.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Rewrite/Frontend/FrontendActions.h"
#include "clang/StaticAnalyzer/Frontend/FrontendActions.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Syntax/BuildTree.h"
#include "clang/Tooling/Syntax/Tokens.h"
#include "clang/Tooling/Syntax/Tree.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Option/OptTable.h"
#include "llvm/Support/Path.h"
#include "llvm/Support/Signals.h"

template<typename T>
bool hasAnnotation(T const& declaration, llvm::StringRef value) {
    auto const& annotaions = declaration.template specific_attrs<clang::AnnotateAttr>();
    return std::any_of(
      annotaions.begin(),
      annotaions.end(),
      [&](clang::AnnotateAttr const* const annotation) {
          return annotation->getAnnotation() == value;
      });
}

static std::string
makeOutputFilename(std::string_view mainFileName, std::string_view outputDirectory) {
    llvm::SmallVector<char> outputFilenameBuffer;
    llvm::sys::path::append(
      outputFilenameBuffer,
      std::string{outputDirectory.begin(), outputDirectory.end()},
      "TypeDescriptor_" + llvm::sys::path::filename(mainFileName));
    return std::string{outputFilenameBuffer.begin(), outputFilenameBuffer.end()};
}

namespace RecreatedGoon {
struct Class {
    std::string name;
    std::string qualifiedName;

    std::vector<std::string> templateDeclList;
    std::vector<std::string> templateParamList;

    std::vector<std::string> bases;
    std::vector<std::string> members;

    template<typename Stream, typename List>
    void addList(Stream& os, List const& list) const {
        bool first = true;
        for(auto const& v : list) {
            if(!first) {
                os << ',';
            }
            first = false;
            os << v;
        }
    }

    template<typename Stream>
    void printTypeDescriptor(Stream& os) const {
        bool const isTemplate = !templateDeclList.empty() || !templateParamList.empty();
        os << "namespace aglio";
        os << '{';
        os << "template";
        os << '<';
        if(isTemplate) {
            addList(os, templateDeclList);
        }
        os << '>';
        os << "struct TypeDescriptorGen";
        os << '<';
        os << qualifiedName;
        if(isTemplate) {
            os << '<';
            addList(os, templateParamList);
            os << '>';
        }
        os << '>';
        if(!bases.empty()) {
            os << ':';
            os << "BaseClassList";
            os << '<';
            addList(os, bases);
            os << '>';
        }
        if(!members.empty()) {
            if(!bases.empty()) {
                os << ',';
            } else {
                os << ':';
            }
            os << "MemberList";
            os << '<';
            {
                bool first = true;
                for(auto const& member : members) {
                    if(!first) {
                        os << ',';
                    }
                    first = false;
                    os << "MemberDescriptor";
                    os << '<';
                    os << '&';
                    os << qualifiedName;
                    if(isTemplate) {
                        os << '<';
                        addList(os, templateParamList);
                        os << '>';
                    }
                    os << "::";
                    os << member;
                    os << ",\"";
                    os << member;
                    os << '\"';
                    os << '>';
                }
            }
            os << '>';
        }
        os << '{';

        os << "static constexpr std::string_view Name{\"";
        os << name;
        os << "\"};";

        os << "static constexpr std::string_view QualifiedName{\"";
        os << qualifiedName;
        os << "\"};";

        os << "};";
        os << "}\n";
    }
};

static llvm::Expected<std::string> getTemplateArgumentAsString(
  clang::TemplateArgument const&            templateArg,
  clang::ASTContext const&                  ctx,
  clang::PrintingPolicy const&              pp,
  clang::TemplateParameterList const* const templateParamList) {
    if(templateArg.isDependent()) {
        if(templateArg.getKind() == clang::TemplateArgument::ArgKind::Type) {
            clang::QualType const qualType = templateArg.getAsType();
            if(clang::TemplateTypeParmType const* const templateTypeParam
               = qualType->getAs<clang::TemplateTypeParmType>();
               templateTypeParam != nullptr)
            {
                if(clang::NamedDecl const* const nameDecl
                   = templateParamList->getParam(templateTypeParam->getIndex());
                   nameDecl != nullptr)
                {
                    return nameDecl->getDeclName().getAsString();
                } else {
                    return llvm::createStringError(
                      std::make_error_code(std::errc::invalid_argument),
                      "1");
                }
            } else if(clang::TemplateSpecializationType const* const templateSpecializationType
                      = qualType->getAs<clang::TemplateSpecializationType>();
                      templateSpecializationType != nullptr)
            {
                std::string              ret;
                llvm::raw_string_ostream ss{ret};
                {
                    clang::TemplateName const templateName
                      = templateSpecializationType->getTemplateName();

                    clang::TemplateName::NameKind templateKind = templateName.getKind();
                    if(templateKind == clang::TemplateName::NameKind::Template) {
                        std::string                qualifiedName;
                        llvm::raw_string_ostream   qss{qualifiedName};
                        clang::TemplateDecl const* templateDecl = templateName.getAsTemplateDecl();
                        if(templateDecl) {
                            templateDecl->printQualifiedName(qss, pp);
                        } else {
                            return llvm::createStringError(
                              std::make_error_code(std::errc::invalid_argument),
                              "2");
                        }
                        qss.flush();
                        ss << qualifiedName;
                    } else {
                        return llvm::createStringError(
                          std::make_error_code(std::errc::invalid_argument),
                          "3");
                    }
                }

                ss << '<';
                auto args = templateSpecializationType->template_arguments();
                {
                    bool first = true;
                    for(auto const& arg : args) {
                        if(!first) {
                            ss << ',';
                        }
                        first   = false;
                        auto es = getTemplateArgumentAsString(arg, ctx, pp, templateParamList);
                        if(!es) {
                            return es;
                        }
                        ss << *es;
                    }
                }

                ss << '>';
                return ret;
            } else if(clang::PackExpansionType const* const packExpansionType
                      = qualType->getAs<clang::PackExpansionType>();
                      packExpansionType != nullptr)
            {
                packExpansionType->dump();
                templateArg.dump(llvm::errs());
                return llvm::createStringError(
                  std::make_error_code(std::errc::invalid_argument),
                  "4");
            } else {
                qualType.dump();
                templateArg.dump(llvm::errs());
                return llvm::createStringError(
                  std::make_error_code(std::errc::invalid_argument),
                  "5");
            }
        } else if(templateArg.getKind() == clang::TemplateArgument::ArgKind::Template) {
            templateArg.dump(llvm::errs());
            return llvm::createStringError(std::make_error_code(std::errc::invalid_argument), "5");
        } else if(templateArg.getKind() == clang::TemplateArgument::ArgKind::Pack) {
            templateArg.dump(llvm::errs());
            return llvm::createStringError(std::make_error_code(std::errc::invalid_argument), "5");
            /*llvm::raw_string_ostream ss{ret};
            //            templateArg.dump(ss);
            //            templateArg.print(pp,ss,"foogar");
            llvm::outs() << templateArg.pack_size() << '\n';
            for(auto const& arg : templateArg.pack_elements()) {
                return getTemplateArgumentAsString(arg, ctx, pp, templateParamList);
            }
            return ret;*/
            /*   return llvm::createStringError(
              std::make_error_code(std::errc::invalid_argument),
              "6");*/
        } else {
            templateArg.dump(llvm::errs());
            return llvm::createStringError(std::make_error_code(std::errc::invalid_argument), "7");
        }
    } else {
        if(templateArg.getKind() == clang::TemplateArgument::ArgKind::Type) {
            return clang::TypeName::getFullyQualifiedName(templateArg.getAsType(), ctx, pp, false);
        } else {
            templateArg.dump(llvm::errs());
            return llvm::createStringError(std::make_error_code(std::errc::invalid_argument), "8");
        }
    }

  //  return llvm::createStringError(std::make_error_code(std::errc::invalid_argument), "9");
}

struct ClassASTInfo {
    clang::CXXRecordDecl const*          self;
    std::vector<clang::CXXBaseSpecifier> bases;
    std::vector<clang::FieldDecl const*> members;

    explicit ClassASTInfo(clang::CXXRecordDecl const* self_) : self{self_} {}

    Class genClass(
      clang::SourceManager const& sm,
      clang::LangOptions const&   lo,
      clang::ASTContext const&    ctx) const {
        clang::PrintingPolicy const pp = [&]() {
            clang::PrintingPolicy pp_{lo};
            pp_.FullyQualifiedName = true;
            return pp_;
        }();

        Class ret;
        ret.name          = self->getNameAsString();
        ret.qualifiedName = self->getQualifiedNameAsString();

        clang::TemplateSpecializationKind const templateSpecializationKind
          = self->getTemplateSpecializationKind();

        llvm::ExitOnError exit;
        auto              error = [&](auto const& v) {
            std::string              s;
            llvm::raw_string_ostream ss{s};
            self->dump(llvm::errs());
            ss << "Error: what is this: " << v << "\n";
            self->print(ss);
            ss << "\n";
            exit.setBanner(s);
            exit(llvm::createStringError(std::make_error_code(std::errc::invalid_argument), ""));
        };

        if(self->isTemplated()) {
            if(clang::ClassTemplateDecl const* templateDecl = self->getDescribedClassTemplate();
               templateDecl != nullptr)
            {
                clang::TemplateParameterList const* templateParamList
                  = templateDecl->getTemplateParameters();
                if(templateParamList != nullptr) {
                    for(clang::NamedDecl const* namedDecl : *templateParamList) {
                        auto&                    s = ret.templateDeclList.emplace_back();
                        llvm::raw_string_ostream ss{s};
                        namedDecl->print(ss, pp, 0, false);
                        s.erase(
                          std::remove_if(
                            s.begin(),
                            s.end(),
                            [](auto c) { return c == '(' || c == ')'; }),
                          s.end());
                        ret.templateParamList.push_back(
                          (namedDecl->getName()
                           + (namedDecl->isTemplateParameterPack() ? "..." : ""))
                            .str());
                    }
                } else {
                    error(1);
                }
            } else if(
              templateSpecializationKind
              == clang::TemplateSpecializationKind::TSK_ExplicitSpecialization)
            {
                if(clang::ClassTemplatePartialSpecializationDecl const*
                     templatePartialSpecialization
                   = llvm::dyn_cast_or_null<clang::ClassTemplatePartialSpecializationDecl>(self);

                   templatePartialSpecialization != nullptr)
                {
                    if(clang::TemplateParameterList const* templateParamList
                       = templatePartialSpecialization->getTemplateParameters();

                       templateParamList != nullptr)
                    {
                        for(clang::NamedDecl const* namedDecl : *templateParamList) {
                            auto&                    s = ret.templateDeclList.emplace_back();
                            llvm::raw_string_ostream ss{s};
                            namedDecl->print(ss, pp, 0, false);
                        }
                        for(clang::TemplateArgument const& templateArg :
                            templatePartialSpecialization->getTemplateArgs().asArray())
                        {
                            exit.setBanner("getTemplateArgumentAsString failed: ");
                            ret.templateParamList.push_back(exit(getTemplateArgumentAsString(
                              templateArg,
                              ctx,
                              pp,
                              templateParamList)));
                        }
                    } else {
                        error(2);
                    }
                } else {
                    error(3);
                }
            }
        } else if(
          templateSpecializationKind
          == clang::TemplateSpecializationKind::TSK_ExplicitSpecialization)
        {
            auto const* templateSpecialization
              = llvm::dyn_cast_or_null<clang::ClassTemplateSpecializationDecl>(self);
            if(templateSpecialization != nullptr) {
                for(clang::TemplateArgument const& templateArg :
                    templateSpecialization->getTemplateArgs().asArray())
                {
                    if(templateArg.getKind() == clang::TemplateArgument::ArgKind::Type) {
                        ret.templateParamList.push_back(clang::TypeName::getFullyQualifiedName(
                          templateArg.getAsType(),
                          ctx,
                          pp,
                          false));
                    } else {
                        error(4);
                    }
                }
            } else {
                error(5);
            }
        }

        if(self->hasPrivateFields()) {
            bool found = false;
            for(clang::FriendDecl const* friendDecl : self->friends()) {
                std::string              s;
                llvm::raw_string_ostream ss{s};
                friendDecl->dump(ss);
                if(s.find("aglio::TypeDescriptorGen") != std::string::npos) {
                    found = true;
                }
            }
            if(!found) {
                exit.setBanner(
                  "private member in \"" + self->getSourceRange().printToString(sm)
                  + "\" but TypeDescriptorGen is not a friend: ");
                exit(llvm::errorCodeToError(std::make_error_code(std::errc::invalid_argument)));
            }
        }

        for(clang::CXXBaseSpecifier const& base : bases) {
            ret.bases.push_back(
              clang::TypeName::getFullyQualifiedName(base.getType(), ctx, pp, false));
        }

        for(clang::FieldDecl const* member : members) {
            ret.members.push_back(member->getNameAsString());
        }
        return ret;
    }
};

class ClassVisitor : public clang::RecursiveASTVisitor<ClassVisitor> {
public:
    bool VisitCXXRecordDecl(clang::CXXRecordDecl const* declaration) {
        if(
          hasAnnotation(*declaration, "recreated_goon_ignore")
          || !declaration->isThisDeclarationADefinition() || declaration->isLocalClass() != nullptr)
        {
            return true;
        }
        ClassASTInfo& current_class = classes.emplace_back(declaration);
        for(clang::CXXBaseSpecifier base : declaration->bases()) {
            current_class.bases.push_back(base);
        }
        return true;
    }
    bool VisitFieldDecl(clang::FieldDecl const* declaration) {
        if(auto class_it = std::find_if(
             classes.begin(),
             classes.end(),
             [&](ClassASTInfo const& c) { return c.self == declaration->getParent(); });
           class_it != classes.end())
        {
            if(!hasAnnotation(*declaration, "recreated_goon_ignore")) {
                class_it->members.push_back(declaration);
            }
        }

        return true;
    }

    template<typename OS>
    void generate(
      OS&                         os,
      clang::SourceManager const& sm,
      clang::LangOptions const&   lo,
      clang::ASTContext const&    ctx) const {
        for(auto const& c : classes) {
            c.genClass(sm, lo, ctx).printTypeDescriptor(os);
        }
    }

private:
    std::vector<ClassASTInfo> classes;
};

class ClassConsumer : public clang::ASTConsumer {
public:
    explicit ClassConsumer(std::string outputDirectory_, std::string mainFileName_)
      : outputDirectory{std::move(outputDirectory_)}
      , mainFileName{std::move(mainFileName_)} {}

    void HandleTranslationUnit(clang::ASTContext& context) override {
        clang::SourceManager const& sm = context.getSourceManager();
        clang::LangOptions const&   lo = context.getLangOpts();
        ClassVisitor                visitor{};
        auto                        decls = context.getTranslationUnitDecl()->decls();

        for(clang::Decl* decl : decls) {
            clang::SourceLocation const location = decl->getLocation();
            if(sm.getFileID(location) != sm.getMainFileID()) {
                continue;
            }
            visitor.TraverseDecl(decl);
        }

        llvm::ExitOnError exit;
        std::string const outputFilename = makeOutputFilename(mainFileName, outputDirectory);

        std::error_code ec;

        auto os = llvm::raw_fd_ostream(outputFilename, ec, llvm::sys::fs::FA_Write);

        exit.setBanner("Error creating file " + outputFilename + ": ");
        exit(llvm::errorCodeToError(ec));

        visitor.generate(os, sm, lo, context);
    }

private:
    std::string outputDirectory;
    std::string mainFileName;
};

class ClassAction : public clang::ASTFrontendAction {
public:
    explicit ClassAction(std::string outputDirectory_)
      : outputDirectory{std::move(outputDirectory_)} {}
    std::unique_ptr<clang::ASTConsumer>
    CreateASTConsumer(clang::CompilerInstance&, llvm::StringRef fileName) override {
        return std::make_unique<ClassConsumer>(outputDirectory, fileName.str());
    }
    std::string outputDirectory;
};

static std::unique_ptr<clang::tooling::FrontendActionFactory>
newClassActionFactory(std::string outputDirectory) {
    class SimpleFrontendActionFactory : public clang::tooling::FrontendActionFactory {
    public:
        explicit SimpleFrontendActionFactory(std::string outputDirectory_)
          : outputDirectory{std::move(outputDirectory_)} {}
        std::unique_ptr<clang::FrontendAction> create() override {
            return std::make_unique<ClassAction>(outputDirectory);
        }

    private:
        std::string outputDirectory;
    };

    return std::unique_ptr<clang::tooling::FrontendActionFactory>(
      new SimpleFrontendActionFactory{std::move(outputDirectory)});
}

}   // namespace RecreatedGoon

static void clearAndCreateOutputFiles(
  llvm::StringRef                 outputDirectory,
  std::vector<std::string> const& inputfiles) {
    llvm::ExitOnError exit;
    exit.setBanner("Error creating directory " + std::string{outputDirectory} + ": ");
    exit(llvm::errorCodeToError(llvm::sys::fs::create_directories(outputDirectory)));

    for(auto const& file : inputfiles) {
        auto const outputFilename = makeOutputFilename(file, outputDirectory);
        llvm::sys::fs::remove(outputFilename);

        std::error_code ec;

        auto os = llvm::raw_fd_ostream(outputFilename, ec, llvm::sys::fs::FA_Write);

        exit.setBanner("Error creating file " + outputFilename + ": ");
        exit(llvm::errorCodeToError(ec));
    }
}

int main(int argc, char const** argv) {
    if(argc != 0) {
        llvm::sys::PrintStackTraceOnErrorSignal(*argv);
    }

    llvm::cl::OptionCategory   category("recreated_goon code generator options");
    llvm::cl::opt<std::string> outputDirectory(
      "output-directory",
      llvm::cl::desc("the directory to save the generated files"),
      llvm::cl::cat(category));

    llvm::cl::opt<bool> ignoreErrors(
      "ignore-errors",
      llvm::cl::desc("ignore errors while parsing"),
      llvm::cl::cat(category));

    llvm::cl::opt<bool> printCompilerArgs(
      "print-compiler-args",
      llvm::cl::desc("print the compiler args get from compilation database"),
      llvm::cl::cat(category));

    llvm::ExitOnError exit;
    exit.setBanner("Error parsing commandline: ");

    clang::tooling::CommonOptionsParser optionsParser
      = exit(clang::tooling::CommonOptionsParser::create(
        argc,
        argv,
        category,
        llvm::cl::OneOrMore,
        nullptr));

    exit.setBanner("Error creating directory " + std::string{outputDirectory.getValue()} + ": ");
    exit(llvm::errorCodeToError(llvm::sys::fs::create_directories(outputDirectory.getValue())));

    clearAndCreateOutputFiles(outputDirectory.getValue(), optionsParser.getSourcePathList());

    clang::tooling::ClangTool tool(
      optionsParser.getCompilations(),
      optionsParser.getSourcePathList());

    tool.appendArgumentsAdjuster(
      [](const clang::tooling::CommandLineArguments& args, llvm::StringRef)
        -> clang::tooling::CommandLineArguments {
          auto args2 = args;
          args2.erase(
            std::remove_if(
              args2.begin(),
              args2.end(),
              [](auto const& arg) { return arg.find("-march=") != std::string::npos; }),
            args2.end());
          return args2;
      });

    if(printCompilerArgs.getNumOccurrences() > 0) {
        tool.appendArgumentsAdjuster(
          [](const clang::tooling::CommandLineArguments& args, llvm::StringRef Filename)
            -> clang::tooling::CommandLineArguments {
              llvm::outs() << Filename << '\n';
              for(auto arg : args) {
                  llvm::outs() << arg << '\n';
              }
              llvm::outs() << "\n\n";
              return args;
          });
    }

    auto ignoringDiagConsumer = clang::IgnoringDiagConsumer{};
    if(ignoreErrors.getNumOccurrences() > 0) {
        tool.setDiagnosticConsumer(&ignoringDiagConsumer);
        tool.setPrintErrorMessage(false);
    }

    return tool.run(RecreatedGoon::newClassActionFactory(outputDirectory.getValue()).get());
}
