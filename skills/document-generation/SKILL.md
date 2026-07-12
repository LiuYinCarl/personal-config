---
name: document-generation
description: 项目文档生成工作流。生成 README、架构文档、API 文档、编译指南、CHANGELOG、贡献指南等。支持 ASCII 框图自动修复。触发词：写文档、生成文档、README、架构文档、编译指南、API 文档、CHANGELOG、贡献指南、项目文档。
---

# 文档生成工作流 (Document Generation Workflow)

本 Skill 规范了为项目生成高质量技术文档的标准流程。融合了 Write the Docs、Google Tech Writing、Diátaxis 框架等业界最佳实践，覆盖从代码阅读到最终交付的完整文档生产链路。

## 前置步骤：项目探测

在开始撰写文档前，先探测项目特征以适配文档策略：

- 识别项目语言和框架（Python/JS/TS/Go/Rust/...）
- 识别项目类型（CLI 工具 / Web 服务 / 库&SDK / 桌面应用 / 嵌入式 / 数据管道）
- 识别包管理器和构建系统
- 识别现有文档情况（哪些文档已存在、哪些需要补充或更新）
- 识别目标读者（开发者 / 最终用户 / 运维人员 / 贡献者）
- 通过 `git log` / `git tag` 确定文档应覆盖的版本范围

## 文档编写原则

### 核心理念

1. **文档是给人看的**：优先考虑读者的知识背景和使用场景。技术准确性固然重要，但可读性和可发现性同样关键。如果读者找不到答案，再准确的文档也是无效的。
2. **金字塔原则（先总后分）**：每个文档章节从概述开始，再逐步深入细节。读者应能在 30 秒内判断这个章节是否包含他们需要的信息。
3. **单一真相来源（SSOT）**：同一信息只在一处定义。API 参数说明应引用源码类型定义，版本号应引用 `package.json`/`Cargo.toml`/`pyproject.toml`。避免多处维护导致不一致。
4. **渐进式披露**：核心信息前置，细节按需展开。README 应该一屏内让读者知道「这是什么、怎么安装、怎么用」。详细配置用链接引导到专门文档。
5. **文档即代码（Docs as Code）**：文档与代码同仓库管理，跟随版本同步更新。文档变更走同样的 PR/Review 流程。
6. **示例优先**：「一个可运行的完整示例胜过一千字描述」。每个 API 至少有一个可复制粘贴即运行的代码片段。

### 文档质量金字塔（分配精力的优先级）

```
         ┌──────────┐
         │  美观    │ ← 格式统一、排版整洁（交给工具）
         ├──────────┤
         │  完整    │ ← 覆盖所有模块/API/配置项
         ├──────────┤
         │  准确    │ ← 与代码行为一致、版本号正确
         ├──────────┤
         │  可用    │ ← 读者能快速找到答案、示例能跑通
         ├──────────┤
         │  清晰    │ ← 逻辑通顺、术语统一、图表正确
         └──────────┘
```

越底层越重要，应分配越多精力。美观交给格式化工具，人的精力集中在清晰性和可用性上。

## 可用工具

### fix_ascii_diagram.py

修复 Markdown/文本中 ASCII Box-Drawing 图表的对齐问题。

文件位置：`skills/document-generation/fix_ascii_diagram.py`

**用法**：

```bash
# 检查对齐问题（安全，不修改文件）
python3 fix_ascii_diagram.py <文件> --check

# 原地修复
python3 fix_ascii_diagram.py <文件> --inplace

# 输出到新文件（对比用）
python3 fix_ascii_diagram.py <文件> --output <输出文件>
```

**适用场景**：
- 编辑框线图后竖线（│├┤┼）在行间对不齐
- CJK 全角字符（中文/日文/韩文）导致框线错半格
- 水平线（─）长度与框体宽度不匹配

**安全策略**：
- 仅修复矩形盒子图（非树形/流程图）
- 仅修正 box-drawing 字符和空格，不修改文本内容
- 偏差超过 2 列的边保持原样（需人工判断）
- 建议先 `--check` 再 `--inplace`

## 文档类型与标准结构

### 文档类型决策树

根据项目特征选择需要生成的文档：

| 项目类型 | 必需文档 | 推荐文档 | 可选文档 |
|---------|---------|---------|---------|
| 库/SDK | README + API 文档 | CHANGELOG + CONTRIBUTING | ARCHITECTURE |
| CLI 工具 | README + BUILD | CHANGELOG | ARCHITECTURE |
| Web 服务 | README + BUILD + DEPLOYMENT | API 文档 + ARCHITECTURE | CONTRIBUTING |
| 桌面应用 | README + BUILD | CHANGELOG + 用户指南 | ARCHITECTURE |
| 框架 | README + API 文档 + 快速入门 | ARCHITECTURE + CHANGELOG | CONTRIBUTING + 迁移指南 |

---

### 1. README.md — 项目门面

> 目标：30 秒内让读者理解项目是什么、怎么用、为什么选它。

**标准结构**：

```markdown
# 项目名称

[![License](https://img.shields.io/...)](LICENSE)
[![CI Status](https://img.shields.io/...)](...)
[![Version](https://img.shields.io/...)](...)

> 一句话概述：用一句话说清项目做什么、解决什么问题、核心优势是什么。

## ✨ 特性 (Features)

- 用动词开头：支持... / 提供... / 自动...
- 3-7 条为宜，过多说明职责不清晰
- 每个特性尽量一句话

## 📦 快速开始 (Quick Start)

### 环境要求
- 语言版本（Python 3.10+ / Node.js 18+ / Go 1.21+）
- 系统依赖（如有）

### 安装

\`\`\`bash
# 可复制粘贴运行的安装命令
pip install xxx
# 或
git clone ... && cd ... && make install
\`\`\`

### 5 分钟示例

\`\`\`python
# 一个完整的最小可用示例
from xxx import YYY
result = YYY.do_something()
print(result)
\`\`\`

## 📖 使用指南

简要说明使用入口，引导读者到更详细的文档。

## 🤝 贡献

指向 CONTRIBUTING.md 的链接。

## 📄 License

[LICENSE 文件链接]
```

---

### 2. BUILD.md — 编译/安装指南

> 目标：一个新克隆仓库的开发者能按照文档一次性成功编译。

**标准结构**：

```markdown
# 编译指南

## 环境要求

| 依赖 | 最低版本 | 推荐版本 | 说明 |
|------|---------|---------|------|
| OS | ... | ... | ... |
| 编译器 | GCC 11+ / Clang 14+ / MSVC 2022+ | ... | 必须支持 C++20 |
| CMake | 3.20+ | 3.28+ | 构建系统 |
| ... | ... | ... | ... |

## 前置准备

\`\`\`bash
# 系统依赖安装（按平台分组）
# macOS
brew install cmake ninja

# Ubuntu/Debian
sudo apt install build-essential cmake ninja-build

# 初始化 submodule（如有）
git submodule update --init --recursive
\`\`\`

## 编译步骤

\`\`\`bash
# 配置（说明各选项含义）
cmake -B build -DCMAKE_BUILD_TYPE=Release -DENABLE_FEATURE_X=ON

# 编译
cmake --build build -j$(nproc)

# 可选：运行测试
cmake --build build --target test
\`\`\`

## 构建输出

| 产物 | 路径 | 说明 |
|------|------|------|
| 可执行文件 | `build/bin/xxx` | 主程序 |
| 动态库 | `build/lib/libxxx.so` | 共享库 |

## 编译选项一览

| 选项 | 默认值 | 说明 |
|------|-------|------|
| `ENABLE_FEATURE_X` | OFF | 启用实验性功能 X |
| `BUILD_TESTS` | ON | 是否编译测试 |

## 常见问题

### Q: 编译报错 'xxx.h' not found
A: 确保已安装对应 dev 包：`sudo apt install libxxx-dev`
```

---

### 3. ARCHITECTURE.md — 架构设计文档

> 目标：新贡献者阅读后能理解系统的整体结构和关键设计决策。

**标准结构**：

```markdown
# 架构设计文档

## 概述

项目定位、设计哲学、核心设计原则。

## 整体架构

\`\`\`
┌──────────────────────────────────────────────┐
│                  入口层                      │
│  ┌─────────┐  ┌──────────┐  ┌──────────┐   │
│  │ CLI     │  │ HTTP API │  │ gRPC     │   │
│  └────┬────┘  └────┬─────┘  └────┬─────┘   │
│       └────────────┼─────────────┘          │
├────────────────────┼────────────────────────┤
│              核心业务层                       │
│       ┌───────────┴───────────┐             │
│       │      Service          │             │
│       └───────────┬───────────┘             │
├───────────────────┼─────────────────────────┤
│              基础设施层                       │
│   ┌──────┐  ┌─────┴────┐  ┌────────┐       │
│   │ DB   │  │  Cache   │  │ Queue  │       │
│   └──────┘  └──────────┘  └────────┘       │
└──────────────────────────────────────────────┘
\`\`\`

## 模块详解

对每个核心模块，按以下格式描述：

### 模块名（`src/module_name/`）

| 属性 | 值 |
|------|-----|
| 职责 | 一句话描述模块做什么 |
| 入口 | `entry_point.py:main()` |
| 依赖 | module_a, module_b |
| 被依赖 | module_c |

**设计要点**：
- 为什么选择了 A 方案而非 B 方案
- 关键的不变量和数据流
- 已知的限制和未来改进方向

**核心接口**：
- `function_a(param)` — 功能描述
- `function_b(param)` — 功能描述

## 数据流

描述关键业务流程的数据流，可配合 Mermaid 时序图。

## 跨模块策略

- 错误处理策略（哪里捕获、哪里记录、如何传递）
- 日志策略（日志级别约定、结构化字段规范）
- 配置管理（配置文件层次、环境变量优先级）

## 扩展点与定制

- 插件系统设计
- 钩子/中间件机制
- 第三方集成接口
```

---

### 4. API 文档

> 目标：开发者不需要阅读源码就能正确使用公开 API。

**标准结构**：

```markdown
# API 参考文档

## 模块概览

| 模块 | 说明 | 主要导出 |
|------|------|---------|
| `core` | 核心功能 | `Engine`, `Config` |
| `utils` | 工具函数 | `parse`, `format` |

## Core 模块

### `Engine(config: Config)`

核心引擎类，管理整个系统的生命周期。

**参数**：
| 参数 | 类型 | 必填 | 默认值 | 说明 |
|------|------|------|-------|------|
| `config` | `Config` | 是 | — | 引擎配置对象 |

**返回**：无（构造函数）

**异常**：
| 异常类型 | 触发条件 |
|---------|---------|
| `ConfigError` | 配置格式无效 |
| `ConnectionError` | 无法连接到后端服务 |

**使用示例**：

\`\`\`python
from mylib import Engine, Config

config = Config(host="localhost", port=8080)
engine = Engine(config)
engine.start()
\`\`\`

### `Engine.process(data: bytes) -> Result`

处理输入数据并返回结果。

**参数**：
| 参数 | 类型 | 必填 | 说明 |
|------|------|------|------|
| `data` | `bytes` | 是 | 待处理的原始数据，最大 10MB |

**返回**：`Result` 对象，包含处理结果和元数据。

**异常**：`ValueError` — 数据格式无效；`TimeoutError` — 处理超时。

**使用示例**：

\`\`\`python
result = engine.process(b"hello world")
print(result.output)  # => "processed: hello world"
print(result.duration_ms)  # => 42
\`\`\`
```

**API 文档撰写规则**：
- 每个公开函数/类/方法必须有独立条目
- 参数表包含：参数名、类型、是否必填、默认值、说明
- 异常表包含：异常类型、触发条件
- 示例代码可直接运行，展示输入和输出
- 当类型本身较复杂时，链接到类型定义

---

### 5. CHANGELOG.md — 变更日志

> 目标：用户和开发者能快速了解每个版本的变化，判断是否需要升级。

遵循 [Keep a Changelog](https://keepachangelog.com/) 规范：

```markdown
# Changelog

## [1.2.0] - 2026-07-12

### Added
- 新功能：支持 WebSocket 实时推送 (#234)
- CLI 新增 `--verbose` 选项

### Changed
- **Breaking**: `Engine.start()` 现在返回 `Promise<void>` 而非 `void`
- 默认缓存时间从 300s 调整为 600s

### Deprecated
- `legacy_api()` 将在 v2.0 移除，请迁移到 `new_api()`

### Fixed
- 修复高并发下连接池泄漏问题 (#456)
- 修复 Windows 下路径分隔符错误

### Security
- 修复 CVE-2026-XXXX：用户输入未校验导致 XSS
```

---

### 6. CONTRIBUTING.md — 贡献指南

> 目标：降低新贡献者的入门门槛，确保贡献流程一致。

```markdown
# 贡献指南

## 行为准则
（链接到 CODE_OF_CONDUCT.md 或简述）

## 如何贡献

### 报告 Bug
- 使用 Bug Report 模板
- 提供最小复现步骤
- 提供环境信息（OS、版本等）

### 提交代码

1. Fork 仓库并创建分支：`git checkout -b feat/my-feature`
2. 编写代码 + 测试
3. 运行 lint 和测试：`make lint && make test`
4. 提交：使用 [Conventional Commits](https://www.conventionalcommits.org/) 格式
   - `feat: 添加 xxx 功能`
   - `fix: 修复 xxx 问题`
   - `docs: 更新 xxx 文档`
5. 推送并创建 PR

### 代码风格
- 遵循项目已有的代码风格
- Python: PEP 8 + ruff
- TypeScript: ESLint + Prettier

### 测试要求
- 新功能必须有测试覆盖
- Bug 修复必须有回归测试
- 保持测试通过率 100%

## 开发环境搭建
（链接到 BUILD.md）

## PR Review 流程
- 所有 PR 需要至少 1 位维护者 Approve
- CI 必须全部通过
```

---

### 7. DEPLOYMENT.md — 部署文档

> 目标：运维人员能按照文档独立完成部署和故障排查。

```markdown
# 部署指南

## 部署架构
（整体部署拓扑图）

## 环境要求
| 组件 | 版本 | 规格 |
|------|------|------|
| Docker | 24+ | — |
| K8s | 1.28+ | — |
| PostgreSQL | 16+ | 4C8G, 100GB SSD |

## 快速部署

### Docker Compose（开发/测试环境）

\`\`\`bash
docker compose up -d
\`\`\`

### Kubernetes（生产环境）

\`\`\`bash
kubectl apply -f k8s/
\`\`\`

## 配置说明

| 环境变量 | 必填 | 默认值 | 说明 |
|---------|------|-------|------|
| `DATABASE_URL` | 是 | — | 数据库连接串 |
| `LOG_LEVEL` | 否 | `info` | 日志级别 |

## 健康检查

\`\`\`bash
curl http://localhost:8080/health
# 预期输出: {"status": "ok"}
\`\`\`

## 监控与告警
- Metrics 端点: `/metrics` (Prometheus 格式)
- 关键指标: QPS、延迟 P99、错误率
- 告警阈值建议

## 故障排查

### 常见问题
| 症状 | 可能原因 | 解决方案 |
|------|---------|---------|
| 服务无法启动 | 端口被占用 | `lsof -i :8080` 检查占用进程 |
| 数据库连接超时 | 网络不通/防火墙 | 检查安全组规则和连接串 |

## 备份与恢复
（备份策略、恢复步骤）
```

## 工作流程

### Step 0：项目探测（5% 时间）

- 用 `tree -L 2` 了解目录结构
- 用 `git log --oneline -20` 了解项目历史
- 识别项目类型、语言、框架、构建系统
- 检查现有文档：`ls docs/ README* CHANGELOG* CONTRIBUTING* BUILD* ARCHITECTURE*`
- 确定本次需要生成/更新的文档清单
- 识别目标读者及其技术水平

### Step 1：代码深度阅读与项目结构模块分析（30% 时间）

> ⚠️ **这是最关键的一步**。花足够时间深入理解代码结构和模块设计，文档质量由这一步决定。宁可多花时间读代码，也不要仓促动笔。

#### 1.1 目录结构扫描与理解

首先完整了解项目的物理布局：

```bash
# 生成完整目录树（排除无关目录和生成文件）
tree -L 3 -I '__pycache__|*.pyc|node_modules|.git|target|build|dist|vendor|.venv|venv'
# 或
eza -T -L 3 --ignore-glob='__pycache__|node_modules|.git|target|build|dist'
```

对每个一级/二级目录回答三个问题：
- **这个目录做什么**？（一句话职责描述）
- **它对外暴露什么**？（`__init__.py`、`index.ts`、`mod.rs` 等公开接口）
- **它依赖谁、被谁依赖**？（import/reference 方向）

#### 1.2 模块划分与职责分析

将目录结构映射为逻辑模块图。对每个模块做以下分析：

**模块分析模板**：

| 维度 | 分析内容 |
|------|---------|
| 模块名 | `src/auth/` |
| 一句话职责 | 用户认证与授权管理 |
| 公开接口 | `login()`, `logout()`, `checkPermission()`, `AuthMiddleware` |
| 内部实现 | `token.py`（JWT 生成/验证）、`oauth.py`（OAuth2 流程）、`session.py`（会话管理） |
| 依赖（本项目的其他模块） | `models.User`, `config.Settings`, `utils.crypto` |
| 被依赖方 | `api.routes`（所有需要认证的路由）、`admin` 模块 |
| 外部依赖 | `pyjwt`, `bcrypt`, `httpx`（OAuth 回调） |
| 关键设计决策 | 为什么选 JWT 而非 Session？Token 刷新策略是什么？ |
| 已知限制 | 不支持多租户、Token 吊销是软删除 |

**模块关系图**（必须产出）：

用 ASCII 框线图或 Mermaid 绘制模块依赖关系图。矩形盒子图优先（工具可自动修复对齐）。

```
┌─────────────────────────────────────────────────────────┐
│                      入口层                             │
│  ┌──────────┐  ┌──────────────┐  ┌──────────────┐     │
│  │ main.py  │  │  cli/main.py │  │  api/main.py │     │
│  └────┬─────┘  └──────┬───────┘  └──────┬───────┘     │
│       └───────────────┼─────────────────┘              │
├───────────────────────┼────────────────────────────────┤
│                  核心业务层                              │
│       ┌───────────────┴───────────────┐                │
│       │         services/             │                │
│       │  ┌────────┐  ┌────────────┐   │                │
│       │  │ auth   │  │  pipeline  │   │                │
│       │  └───┬────┘  └─────┬──────┘   │                │
│       │      └──────┬──────┘          │                │
│       └─────────────┼─────────────────┘                │
├─────────────────────┼──────────────────────────────────┤
│                 基础设施层                               │
│  ┌────────┐  ┌──────┴─────┐  ┌──────────┐            │
│  │  db/   │  │  cache/    │  │  utils/  │            │
│  └────────┘  └────────────┘  └──────────┘            │
└─────────────────────────────────────────────────────────┘
```

#### 1.3 设计模式与架构风格识别

主动识别项目使用的设计模式和架构风格，这是 ARCHITECTURE.md 的核心素材：

**架构风格识别**：
- [ ] 分层架构（Layered）→ 标注各层职责和层间调用规则
- [ ] 微服务（Microservices）→ 标注服务边界和通信方式（REST/gRPC/消息队列）
- [ ] 事件驱动（Event-Driven）→ 标注事件类型、生产者、消费者
- [ ] 插件架构（Plugin）→ 标注扩展点和注册机制
- [ ] 管道-过滤器（Pipeline）→ 标注各阶段输入输出和数据流
- [ ] 六边形架构（Hexagonal/Ports & Adapters）→ 标注端口和适配器
- [ ] CQRS/Event Sourcing → 标注命令、查询、事件的分离

**设计模式识别**（在代码中搜索典型信号）：
- [ ] 工厂模式：`*Factory*`、`*Builder*`、`create_*` 函数簇
- [ ] 单例模式：模块级全局变量、`__new__` 控制、`once sync.Once`
- [ ] 策略模式：接口/抽象类 + 多个实现，运行时切换
- [ ] 观察者模式：`EventEmitter`/`Signal`/`addEventListener`/`subscribe`
- [ ] 装饰器模式：Python decorator、ES decorator、middleware 链
- [ ] 仓储模式（Repository）：`*Repository` 类、DAO 层
- [ ] 适配器模式：第三方 API 封装、`*Adapter` 类
- [ ] 责任链模式：middleware 链、interceptor 链、handler chain

**编码约定识别**：
- 命名约定：`snake_case` vs `camelCase` vs `PascalCase` 的使用场景
- 错误处理约定：异常 vs Result 类型 vs error code
- 日志约定：结构化日志 vs 文本日志、日志级别使用场景
- 异步约定：`async/await` vs 回调 vs channel vs actor model

#### 1.4 核心数据流追踪

追踪 2-5 个关键业务流程的完整链路，这是 ARCHITECTURE.md 中数据流章节的素材：

**数据流追踪模板**（对每个关键流程）：

```
流程：用户登录请求处理

1. [入口] api/routes/auth.py:login()       ← HTTP POST /auth/login
2. [验证] services/auth.py:authenticate()  ← 验证用户名密码
3. [查询] db/repo/user.py:find_by_email()  ← 数据库查询用户
4. [校验] utils/crypto.py:verify_hash()    ← bcrypt 密码校验
5. [生成] services/auth.py:create_token()  ← 生成 JWT
6. [缓存] cache/session.py:store()         ← Redis 缓存会话
7. [返回] api/routes/auth.py → 200 OK     ← 返回 token + 用户信息

涉及模块：api, services/auth, db/repo, utils/crypto, cache
关键数据：email, password_hash, jwt_token, session_id
错误路径：用户不存在(404)、密码错误(401)、Redis 不可用(503)
```

#### 1.5 配置与常量提取

系统性地提取所有需要文档化的配置和常量：

```bash
# 搜索配置文件
find . -name '*.toml' -o -name '*.yaml' -o -name '*.yml' -o -name '*.json' -o -name '.env*' -o -name '*.ini' -o -name '*.conf' | grep -v node_modules | grep -v '.git'

# 搜索环境变量引用
grep -rn 'os.getenv\|os.environ\|process.env\|env::var\|std::env' --include='*.py' --include='*.ts' --include='*.js' --include='*.rs' | grep -v node_modules | grep -v '.git'

# 搜索常量定义
grep -rn '^[A-Z_]+\s*=' --include='*.py' | head -30  # Python
grep -rn 'const [A-Z_]+' --include='*.ts' --include='*.js' | head -30  # TS/JS
```

#### 1.6 输出：项目结构分析报告

这一步结束后，**必须先输出一份结构分析报告**与用户确认，再进入 Step 2 结构规划：

```
## 项目结构分析报告

### 目录树（简化）
```
project/
├── src/
│   ├── api/          ← HTTP API 层（路由、中间件、请求/响应模型）
│   ├── services/     ← 业务逻辑层（auth、pipeline、notification）
│   ├── db/           ← 数据访问层（ORM 模型、迁移脚本、查询）
│   ├── cache/        ← 缓存层（Redis 封装）
│   └── utils/        ← 通用工具（加密、日志、配置读取）
├── tests/            ← 测试代码
├── docs/             ← 现有文档
└── scripts/          ← 运维脚本
```

### 模块清单（共 N 个模块）

| 模块 | 路径 | 职责 | 公开接口数 | 依赖模块 | 被依赖模块 |
|------|------|------|-----------|---------|-----------|
| api | `src/api/` | HTTP 路由和请求处理 | 15 端点 | services, utils | —（顶层） |
| services/auth | `src/services/auth.py` | 认证授权 | 5 函数 | db, utils, cache | api |
| services/pipeline | `src/services/pipeline.py` | 数据处理管道 | 3 类 | db, utils | api |
| db | `src/db/` | 数据库访问 | 8 模型 | — | services |
| cache | `src/cache/` | 缓存操作 | 4 函数 | — | services |
| utils | `src/utils/` | 通用工具 | 12 函数 | — | 所有模块 |

### 模块依赖关系图
（ASCII 框线图）

### 识别的架构风格
- 分层架构：入口层 → 业务层 → 基础设施层
- 策略模式：auth 模块支持多种认证策略（JWT、OAuth2、API Key）

### 识别的设计模式
- 工厂模式：`create_app()` / `make_celery()`
- 装饰器模式：`@login_required` / `@rate_limit`
- 仓储模式：`UserRepository` / `ProjectRepository`

### 关键业务流程（共 3 条）
1. 用户登录 → 5 个模块参与，最长路径 7 步
2. 数据处理 → 3 个模块参与，异步队列解耦
3. ...

### 配置项汇总（共 15 项）
| 配置项 | 来源 | 必填 | 默认值 | 说明 |
|--------|------|------|-------|------|
| DATABASE_URL | 环境变量 | 是 | — | 数据库连接串 |
| SECRET_KEY | 环境变量 | 是 | — | JWT 签名密钥 |
| LOG_LEVEL | 环境变量 | 否 | INFO | 日志级别 |
| ... | ... | ... | ... | ... |
```

> **⛔ 阻断规则**：在用户确认结构分析报告之前，不得进入 Step 2 结构规划。一旦用户确认，这份报告将作为后续所有文档的「单一真相来源」。

### Step 1 检查清单

- [ ] 完整目录树已生成并理解
- [ ] 所有一级/二级目录的职责已标注
- [ ] 模块清单表格已填写（覆盖所有核心模块）
- [ ] 模块依赖关系图已绘制（ASCII 或 Mermaid）
- [ ] 架构风格已识别（至少 1 种）
- [ ] 设计模式已识别（至少 3 种典型用法）
- [ ] 关键业务流程已追踪（至少 2 条）
- [ ] 配置项和常量已提取汇总
- [ ] 结构分析报告已输出并获用户确认 ✅

### Step 2：结构规划（10% 时间）

- 为每份文档规划大纲（参考上方的标准结构）
- 画整体架构草图（ASCII Box-Drawing 或 Mermaid）
- 列出所有需要覆盖的模块、API、配置项
- 确定文档间的交叉引用关系（README → BUILD, ARCHITECTURE → API 等）
- **与用户确认大纲**后再开始撰写（避免方向性返工）

### Step 3：内容撰写（30% 时间）

遵循以下撰写规范：

**结构规范**：
- 每个文档从概述（Overview）开始，30 秒可读完
- 使用一致性标题层级（# 文档标题 → ## 一级章节 → ### 二级章节）
- 相关章节之间添加「参见 xxx」的交叉引用

**内容规范**：
- 使用表格汇总职责、接口、配置、版本要求（而非散文式列表）
- 代码块必须有语言标记（```python 而非 ```）
- 命令行示例包含预期输出（或至少说明预期行为）
- 框图保持简洁、对齐（完成后用 fix_ascii_diagram.py 检查）
- 每个模块说明包含：职责 + 关键设计决策 + 使用场景
- 避免「TODO」「待补充」等占位符——要么写清楚，要么不写

**语言规范**：
- 中文文档使用中文（技术术语保留英文）
- 英文文档使用英文
- 术语在全文保持一致（不要一会被叫「引擎」一会叫「Engine」）
- 句子简短、主动语态、避免双重否定

### Step 4：图表对齐与验证（5% 时间）

- 如有 ASCII 框图，运行 `fix_ascii_diagram.py --check`
- 发现问题用 `--inplace` 修复或手动调整
- 最终确认 `--check` 零问题
- Mermaid 图表检查语法正确性

### Step 5：强制二次审查（20% 时间）

> ⛔ **硬性要求**：文档撰写完成后，必须执行两轮独立审查，不得跳过或合并。每一轮审查有独立的检查清单和关注点。两轮审查通过之前，文档不得交付。

#### 第一轮审查：技术准确性（Technical Accuracy Review）

**审查目标**：确保文档与代码行为 100% 一致。不允许出现「文档说 A、代码做 B」的情况。

**审查方法**：逐条对照源代码验证文档中的每一个技术声明。

**审查清单**：

##### 1. API 签名一致性

对文档中列出的每个公开 API，打开对应源码文件逐一核对：

| 检查项 | 检查方法 |
|--------|---------|
| 函数名/类名拼写 | 直接匹配源码中的定义名 |
| 参数名和顺序 | 匹配函数签名中的参数列表 |
| 参数类型 | 匹配类型注解（Python type hints / TS types / Go types / Rust types） |
| 是否必填 | 检查是否有默认值 |
| 返回值类型 | 匹配返回类型注解 |
| 抛出的异常/错误 | 在源码中搜索 `raise` / `throw` / `return Err` |
| 异步/同步标记 | 检查是否有 `async` / `func` / `Promise` 等标记 |

**典型错误示例**：
- 文档写 `login(user, pass)`，源码是 `login(email, password)` → 参数名错误
- 文档写返回 `User`，源码返回 `User | None` → 遗漏 nullable
- 文档写抛 `AuthError`，源码抛 `UnauthorizedError` → 异常类型名错误
- 文档写 `async def process()`，源码是 `def process()` → 同步/异步混淆

##### 2. 配置项一致性

| 检查项 | 检查方法 |
|--------|---------|
| 配置项名称 | 与 `os.getenv()`/`process.env`/`env::var()` 中的字符串完全一致 |
| 默认值 | 与源码中的第二个参数一致（`os.getenv('KEY', 'DEFAULT')`） |
| 必填/可选 | 检查是否有默认值或 fallback 逻辑 |
| 类型 | 环境变量始终是字符串，但需说明期望格式（`'true'`/`'1'`/`'yes'` 等布尔真值的写法） |

##### 3. 版本号一致性

逐一核对文档中出现的所有版本号：

```bash
# 从项目配置文件中提取版本号
cat pyproject.toml | grep version   # Python
cat Cargo.toml | grep version       # Rust
cat package.json | grep version     # TS/JS
cat go.mod | head -1                # Go module 版本
git tag --sort=-version:refname | head -5  # Git 标签
```

##### 4. 命令可执行性

**逐条在终端执行文档中的每个命令**（或至少模拟验证语法正确）：

| 检查项 | 检查方法 |
|--------|---------|
| 安装命令 | `pip install` / `npm install` / `cargo build` 的参数是否正确 |
| 编译命令 | Makefile/CMake/构建脚本中的 target 名是否匹配 |
| 运行命令 | 入口文件路径和参数是否正确 |
| 测试命令 | `pytest` / `npm test` / `cargo test` 的路径和参数 |
| Docker 命令 | image 名、tag、端口、volume 映射是否正确 |

##### 5. 架构描述一致性

| 检查项 | 检查方法 |
|--------|---------|
| 模块路径 | ARCHITECTURE.md 中的路径是否在源码中存在 |
| 依赖方向 | 图中箭头方向与 import/require 方向一致 |
| 数据流步骤 | 流程描述的每一步在源码中有对应的函数调用 |
| 模块职责 | 表述与模块内的实际功能一致 |

##### 6. 代码示例可运行性

对文档中的每个代码示例：

- [ ] 检查 import/require 路径在项目中存在
- [ ] 检查类名/函数名在对应模块中存在
- [ ] 检查参数名和参数顺序正确
- [ ] 检查示例中引用的常量/枚举值存在
- [ ] 检查异常处理语法正确
- [ ] **（推荐）在本地环境实际运行示例代码**

**第一轮审查通过标准**：
- 零个 API 签名错误
- 零个配置项遗漏或错误
- 零个版本号不一致
- 零个无法执行的命令
- 零个架构描述与代码不符
- 零个不可运行的代码示例

> ⛔ **如果第一轮审查发现任何错误，必须修复后重新执行本轮审查**，直到通过标准达成。

---

#### 第二轮审查：可读性与可用性（Readability & Usability Review）

**审查目标**：假设自己是目标读者，能否在不阅读源码的情况下，仅凭文档完成任务。

**审查方法**：以「读者」身份逐文档模拟使用场景，记录所有遇到障碍的地方。

**审查清单**：

##### 1. 快速开始体验（模拟新用户）

从零开始，仅按 README 的「快速开始」章节操作：

- [ ] 能在 2 分钟内完成环境准备
- [ ] 安装命令能一次成功
- [ ] 示例代码复制粘贴后直接可运行
- [ ] 示例的输出与文档描述一致
- [ ] 如果第一步失败，文档提供了排查指引

**方法**：脑内模拟每一步操作，识别所有隐含假设（「读者已经装了 Python 3.10」—— 如果读者不知道自己的 Python 版本呢？）。

##### 2. 信息可发现性（模拟查找信息）

给定 5 个典型任务，在文档中查找答案：

| 典型任务 | 应在哪里找到 | 是否 30 秒内找到 |
|---------|-------------|-----------------|
| 如何安装 | README > 快速开始 | [ ] |
| 如何配置数据库 | BUILD.md 或 配置文档 | [ ] |
| 如何调用某个 API | API 文档 > 对应模块 | [ ] |
| 如何部署 | DEPLOYMENT.md | [ ] |
| 如何贡献代码 | CONTRIBUTING.md | [ ] |

- [ ] 每个答案在 30 秒内可找到
- [ ] 相关文档间有清晰的交叉引用链接
- [ ] 目录/索引覆盖了核心章节

##### 3. 概念完整性

- [ ] 每个核心概念在首次出现时有定义或解释
- [ ] 术语在全文中使用一致（不会混用「模块/组件/服务」指同一事物）
- [ ] 缩写首次出现时给出了全称
- [ ] 外来概念有背景说明（如「本项目使用 JWT（JSON Web Token）进行...」）
- [ ] 没有「显而易见」「当然」「只需」等跳过解释的词

##### 4. 渐进可读性

- [ ] README 第一屏能回答「这是什么、怎么用」
- [ ] 每个章节的前 3 句话概括了本章节内容
- [ ] 复杂概念从简单用例开始，再逐步深入
- [ ] 代码示例按从简单到复杂的顺序排列
- [ ] 高级功能有明确标记，初学者可先跳过

##### 5. 边界与错误覆盖

- [ ] 文档不仅覆盖 happy path，也覆盖了常见错误场景
- [ ] API 文档包含异常/错误说明
- [ ] 编译指南包含常见编译错误和解决方法
- [ ] 部署文档包含故障排查章节
- [ ] 如果某功能有已知限制，文档明确说明

##### 6. 排版与导航

- [ ] 长文档（>3 屏）有目录
- [ ] 代码块有正确的语言标记
- [ ] 表格在窄屏幕上不会横向溢出（列数 ≤ 5）
- [ ] 链接有描述性文本（非「点击这里」「here」等无意义链接文本）
- [ ] 图片/图表有替代文本或标题
- [ ] ASCII 框图通过 `fix_ascii_diagram.py --check`
- [ ] 标题层级连续不跳跃（无 h1 → h3）

##### 7. 角色适配

- [ ] 开发者文档不包含最终用户才需要的信息（反之亦然）
- [ ] 运维文档不假设读者了解代码内部细节
- [ ] 贡献者文档的前提依赖已说明（链接到 BUILD.md）
- [ ] 每份文档的开头指出了目标读者

**第二轮审查通过标准**：
- 所有「快速开始」操作脑内模拟成功（零阻塞）
- 5 个典型任务全部 30 秒内找到答案
- 零个未定义的术语或概念
- 零个「显而易见」式的信息跳过
- 至少 1 个错误场景被覆盖
- 格式检查全部通过

> ⛔ **如果第二轮审查发现任何阻塞问题（读者无法继续操作），必须修复后重新执行本轮审查**。

---

#### 审查总结

两轮审查通过后，输出审查总结：

```
## 二次审查报告

### 第一轮：技术准确性审查
- 审查时间：[开始] → [结束]
- API 签名核对数：N 个
- 配置项核对数：N 个
- 命令核对数：N 个
- 代码示例核对数：N 个
- 发现并修复问题：N 个
- 审查轮次：[1 轮通过 / 第 N 轮通过]

### 第二轮：可读性与可用性审查
- 审查时间：[开始] → [结束]
- 快速开始模拟：[✅ 通过 / ❌ 阻塞 → 修复后重审通过]
- 信息可发现性：5/5 任务在 30 秒内找到答案
- 术语一致性：[✅ 通过 / 发现 N 处不一致 → 已修复]
- 错误场景覆盖：[✅ 通过 / ⚠️ 缺少 xxx 场景]
- 格式检查：[✅ 通过 / 发现 N 处格式问题 → 已修复]
- 审查轮次：[1 轮通过 / 第 N 轮通过]

### 审查结论
- [ ] ✅ 两轮审查全部通过，文档可交付
- [ ] ⚠️ 第二轮存在非阻塞建议，已记录在下方，不影响交付

### 遗留建议（非阻塞）
- （如有）
```

## 文档审查清单

每次文档交付前逐项确认：

### 结构完整性
- [ ] README 包含：概述、特性、快速开始、License
- [ ] 快速开始章节能在 5 分钟内跑通
- [ ] 所有公开 API 都有文档覆盖（无遗漏）
- [ ] 所有配置项都有说明（含默认值和可选值）
- [ ] 必需的构建/运行依赖已列出（含版本要求）

### 准确性
- [ ] 代码示例与当前 API 一致（可直接运行）
- [ ] 版本号与 `pyproject.toml`/`Cargo.toml`/`package.json` 一致
- [ ] 命令示例与 Makefile/脚本一致
- [ ] API 签名与实际源码一致（参数名、类型、返回值）
- [ ] 异常/错误码列表与源码一致
- [ ] 架构图反映当前代码结构（不是过时的设计）

### 可用性
- [ ] 读者能在 30 秒内从 README 了解项目是什么
- [ ] 读者能在 5 分钟内完成安装和运行
- [ ] 读者能通过 API 文档独立完成集成开发
- [ ] 所有文档有清晰的导航和交叉引用
- [ ] 常见问题（FAQ/故障排查）覆盖了已知痛点

### 格式一致性
- [ ] 代码块都有正确的语言标记
- [ ] 表格对齐良好
- [ ] ASCII 框图通过 `fix_ascii_diagram.py --check`
- [ ] 标题层级一致（无跳跃）
- [ ] 链接格式正确且有链接文本（非裸 URL）

### 语言质量
- [ ] 无拼写错误
- [ ] 术语使用一致
- [ ] 无歧义或模糊表述
- [ ] 中文/英文混排规范（术语首次出现标注原文）

## 各语言/框架文档要点

### Python 项目
- **必须使用 `pyproject.toml` 中的版本号和依赖信息**作为文档依据
- 安装命令推荐使用 `pip install` 或 `uv add`
- API 文档引用 Python 类型注解（`typing` 模块）
- 注意区分 `async` 和同步 API
- 如果使用了 `pydantic`，应说明数据模型结构
- 虚拟环境管理说明（`venv` / `virtualenv` / `conda`）

### TypeScript / JavaScript 项目
- 安装命令区分 `npm`/`yarn`/`pnpm`/`bun`
- API 文档引用 TypeScript 类型定义
- React 组件文档包含 Props 表格
- 注意区分 ESM 和 CJS 导入方式
- 浏览器/Node.js 兼容性说明

### Go 项目
- 安装命令 `go install` / `go get`
- 编译说明包含 `CGO_ENABLED` 等环境变量
- API 文档关注公开函数和接口
- 注意 goroutine 安全性和 context 传递

### Rust 项目
- 安装命令 `cargo install` / `cargo build --release`
- API 文档可直接引用 `docs.rs` 或 `cargo doc` 输出
- 注意 `unsafe` 代码需要特别说明
- feature flag 一览

### CLI 工具（通用）
- 必须包含 `--help` 等效输出或命令参考
- 每个子命令独立说明参数和选项
- 提供完整的常见使用场景示例
- 退出码说明

### Web 服务（通用）
- 必须包含 API 端点列表（方法、路径、请求/响应格式）
- 环境变量/配置文件完整清单
- 健康检查端点说明
- 认证/授权方式说明

## 常见问题速查

| 问题 | 修复 |
|------|------|
| 文档内容与代码不一致 | 回查源码确认当前实际行为，优先修正文档 |
| 代码示例不能运行 | 在本地按示例执行一遍确认 |
| 架构图过于复杂 | 拆分为多个层次图（整体 → 模块 → 细节） |
| 术语不一致 | 选定一个术语并全局替换，首次出现标注英文原文 |
| 版本号过时 | 从 `pyproject.toml`/`Cargo.toml`/`package.json` 提取当前版本 |
| 链接失效 | 检查文件路径和锚点拼写，确保文件名大小写一致 |
| 代码块无语言标记 | 补全语言标记，命令行用 `bash`，输出用 `text` |
| 文档过长难以导航 | 添加目录（TOC），README 超过 2 屏内容移到专门文档 |
| 配置项遗漏 | 扫描所有 `os.getenv`/`process.env`/`config.*` 调用 |
| ASCII 框图对不齐 | 运行 `fix_ascii_diagram.py --inplace` |
| 跨平台说明缺失 | 分别为 Linux/macOS/Windows 写命令和说明 |

## 文档交付

文档生成完毕后，输出结构化的交付报告：

```
## 文档生成报告

### 概览
- 项目名称：xxx
- 项目类型：[库/SDK | CLI 工具 | Web 服务 | ...]
- 目标读者：[开发者 | 最终用户 | 运维人员 | ...]

### 生成的文档清单
| 文档 | 路径 | 状态 | 说明 |
|------|------|------|------|
| README | `README.md` | ✅ 新建 | 项目概述和快速开始 |
| 编译指南 | `BUILD.md` | ✅ 更新 | 新增 Docker 编译说明 |
| 架构文档 | `docs/ARCHITECTURE.md` | ✅ 新建 | 含整体架构图和模块详解 |
| API 文档 | `docs/API.md` | ✅ 新建 | 覆盖全部公开 API |
| 变更日志 | `CHANGELOG.md` | ✅ 新建 | 从 git log 生成 |

### 审查结果
- 第一轮（技术准确性审查）：[轮次数] 轮 → ✅ 通过
- 第二轮（可读性与可用性审查）：[轮次数] 轮 → ✅ 通过
- ASCII 框图格式：✅ 零问题（`fix_ascii_diagram.py --check` 通过）

### 建议
- （如有）后续可补充的内容
- （如有）发现的不一致需要在代码侧修复

> ⛔ 两轮审查未全部通过前，文档不得交付。
```
