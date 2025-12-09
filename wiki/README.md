# MotorHandPro Wiki

This directory contains the complete wiki documentation for MotorHandPro.

## 📚 About This Wiki

The MotorHandPro wiki provides comprehensive documentation for:
- Getting started with the framework
- Understanding Primal Logic control theory
- Deploying to various environments
- Integrating with external systems
- API reference and examples

## 🚀 Using This Wiki

### On GitHub

This wiki can be used with GitHub's wiki feature:

1. **Enable GitHub Wiki** for your repository
2. **Clone the wiki repository**:
   ```bash
   git clone https://github.com/STLNFTART/MotorHandPro.wiki.git
   ```
3. **Copy wiki files**:
   ```bash
   cp wiki/*.md MotorHandPro.wiki/
   cd MotorHandPro.wiki
   git add .
   git commit -m "Add comprehensive wiki documentation"
   git push
   ```

### As Local Documentation

You can also browse the wiki files locally:

```bash
cd wiki

# View with any markdown viewer
# Examples:
# - grip (renders GitHub-flavored markdown)
# - pandoc (convert to HTML/PDF)
# - VS Code markdown preview
# - Any markdown-capable editor
```

## 📖 Wiki Structure

```
wiki/
├── Home.md                      # Main landing page
├── _Sidebar.md                  # Navigation sidebar
├── README.md                    # This file
│
├── Getting Started
│   ├── Getting-Started.md
│   ├── Quick-Start-Guide.md
│   └── Installation.md
│
├── Core Concepts
│   ├── Architecture.md
│   ├── Primal-Logic-Framework.md
│   ├── Control-Theory.md
│   └── Temporal-Displacement.md
│
├── User Guides
│   ├── User-Guide.md
│   ├── Hardware-Setup.md
│   ├── LAM-System-Guide.md
│   └── Web-Control-Panel.md
│
├── Development
│   ├── API-Reference.md
│   ├── Development-Setup.md
│   ├── Contributing.md
│   └── Code-Examples.md
│
├── Deployment
│   ├── Deployment-Guide.md
│   ├── Docker-Setup.md
│   ├── Kubernetes-Deployment.md
│   └── Edge-Deployment.md
│
├── Integrations
│   ├── Integration-Examples.md
│   ├── Node-RED-Integration.md
│   ├── Mobile-App.md
│   └── MQTT-Integration.md
│
├── Advanced Topics
│   ├── Drug-Safety-Modeling.md
│   ├── Biomedical-Simulation.md
│   ├── Performance-Tuning.md
│   └── Benchmarking.md
│
└── Reference
    ├── Glossary.md
    ├── FAQ.md
    ├── Troubleshooting.md
    └── Release-Notes.md
```

## 📝 Wiki Page Status

### ✅ Complete Pages

- [x] Home.md
- [x] _Sidebar.md
- [x] Getting-Started.md
- [x] Quick-Start-Guide.md
- [x] Architecture.md
- [x] API-Reference.md
- [x] Deployment-Guide.md
- [x] LAM-System-Guide.md
- [x] FAQ.md
- [x] Glossary.md

### 🚧 Placeholder Pages (Link to Existing Docs)

The following pages should link to existing documentation in the repository:

- [ ] Primal-Logic-Framework.md → `/PRIMAL_LOGIC_FRAMEWORK.md`
- [ ] Temporal-Displacement.md → `/lam/TEMPORAL_DISPLACEMENT.md`
- [ ] User-Guide.md → `/docs/guides/USER_GUIDE.md`
- [ ] Hardware-Setup.md → `/docs/guides/USER_GUIDE.md#hardware`
- [ ] Development-Setup.md → `/CONTRIBUTING.md`
- [ ] Contributing.md → `/CONTRIBUTING.md`
- [ ] Docker-Setup.md → `/docker-compose.yml` + comments
- [ ] Kubernetes-Deployment.md → `/k8s/README.md`

## 🔗 Creating Links Between Pages

Wiki pages use GitHub wiki-style links:

```markdown
<!-- Link to another wiki page -->
[Getting Started](Getting-Started)

<!-- Link to section within page -->
[Installation](#installation)

<!-- Link to external docs -->
[Primal Logic](../PRIMAL_LOGIC_FRAMEWORK.md)
```

## 📄 Converting to Other Formats

### Generate HTML

```bash
# Using pandoc
for file in wiki/*.md; do
    pandoc "$file" -o "${file%.md}.html"
done
```

### Generate PDF

```bash
# Using pandoc with LaTeX
pandoc wiki/Home.md -o MotorHandPro-Wiki.pdf
```

### Create Single Document

```bash
# Combine all wiki pages
cat wiki/*.md > FULL_DOCUMENTATION.md
```

## 🛠️ Maintaining the Wiki

### Adding New Pages

1. Create new `.md` file in `wiki/` directory
2. Use kebab-case for filenames (e.g., `New-Feature.md`)
3. Add link to `_Sidebar.md` for navigation
4. Link from relevant existing pages

### Updating Existing Pages

1. Edit the `.md` file
2. Maintain consistent formatting
3. Update last-modified date if tracking
4. Test all internal links

### Style Guidelines

- Use GitHub-flavored Markdown
- Include table of contents for long pages
- Use code blocks with language specifiers
- Include examples where appropriate
- Link to related pages at the bottom

## 📊 Wiki Statistics

- **Total Pages**: 10+ comprehensive guides
- **Coverage**: All major features documented
- **Examples**: Extensive code examples throughout
- **Navigation**: Sidebar + cross-references

## 🤝 Contributing to Wiki

See [Contributing](Contributing) guide for:
- Documentation standards
- Pull request process
- Style guidelines

## 📞 Support

For wiki-related issues:
- **GitHub Issues**: Report documentation gaps or errors
- **Pull Requests**: Submit improvements directly

## 📜 License

Wiki documentation follows the same license as MotorHandPro:
- Research evaluation only
- Patent pending (U.S. Provisional Patent Application No. 63/842,846)
- © 2025 Donte Lightfoot

---

**Quick Start**: Begin with [Home.md](Home.md) for the main wiki landing page.
