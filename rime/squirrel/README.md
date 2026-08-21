# Squirrel（鼠须管）自定义配置说明

本目录存放 `~/Library/Rime` 下两个 `.custom.yaml` 的备份，配合雾凇拼音（rime-ice）使用。
修改后复制到 `~/Library/Rime`，然后在菜单栏 Squirrel 图标 → **重新部署** 生效。

## 安装 / 更新雾凇拼音

菜单第 5 项：浅克隆 [iDvel/rime-ice](https://github.com/iDvel/rime-ice) 到临时目录，
用 rsync 合并复制到 `~/Library/Rime`（不删除已有文件），最后自动触发重新部署。

- `*.custom.yaml`、用户词库、`custom_phrase.txt`（自定义短语）都会保留
- `default.yaml`、`squirrel.yaml` 等基础文件会被覆盖为新版本（个性化应始终写在 `.custom.yaml` 里）
- 重复运行 = 更新到 rime-ice 最新版

## 文件说明

### default.custom.yaml（全局）

```yaml
patch:
  "menu/page_size": 7        # 候选个数，最多 9
  schema_list:               # 注意：这会替换整个方案列表，没列出的方案从 F4 菜单消失
    - {schema: rime_ice}
```

### squirrel.custom.yaml（界面）

- `show_notifications_when: never` — 切换中英文不弹通知；取值 `always | never | appropriate`，顶层键（不在 style 下）
- `style/color_scheme` / `style/color_scheme_dark` — 明/暗主题分别指定配色；rime-ice 默认自带 `color_scheme_dark: purity_of_form_custom`，想固定一个皮肤必须两个都设
- `style/candidate_list_layout` — 候选框方向：`linear` 横向、`stacked` 竖向
- `style/inline_preedit: true` — 拼音内嵌在输入位置
- `style/font_point` / `style/label_font_point` — 正文字号 / 序号字号
- `style/corner_radius` / `style/hilited_corner_radius` — 外框 / 高亮框圆角
- `style/border_width` — 外边框宽（可负）
- `style/line_spacing` — 行间距（仅 stacked 布局生效）
- `style/translucency: true` — 毛玻璃背景，需 `back_color` 半透明才可见

## 配置脚本 squirrel-config.sh

Squirrel 没有运行时设置界面，常用修改都收进了这个脚本，统一「选编号 → 改配置 → 自动重新部署」：

```bash
./squirrel-config.sh
```

主菜单九个功能：

1. **切换配色主题** — 列出全部可用皮肤（含显示名，解析自 `~/Library/Rime/build/squirrel.yaml`），
   依次选浅色、深色编号；深色直接回车 = 与浅色相同（固定皮肤），选不同编号 = 跟随系统明暗切换。
   对应 `squirrel.custom.yaml` 的 `style/color_scheme` / `style/color_scheme_dark`。
2. **修改字体大小** — 输入正文字号（8-72），序号字号回车则自动取「正文 - 5」。
   对应 `style/font_point` / `style/label_font_point`。
3. **修改候选词数量** — 1-9 个候选。对应 `default.custom.yaml` 的 `menu/page_size`。
4. **候选框横竖排切换** — `linear` 横向 / `stacked` 竖向。
   对应 `style/candidate_list_layout`。
5. **中英文切换通知开关** — `never` / `appropriate`（默认）/ `always` 三选一。
   对应 `show_notifications_when`。
6. **管理输入法方案** — 列出 `~/Library/Rime` 下全部方案（当前启用的标 `*`），
   编号多选后重写 `default.custom.yaml` 的 `schema_list`。
   注意：脚本只能决定 **F4 菜单里有哪些方案**；部署后在方案之间切换仍需按
   `F4`（或 `` Control+` ``），Squirrel 没有切换当前方案的命令行接口。
7. **安装/更新雾凇拼音** — 详见上文「安装 / 更新雾凇拼音」。
8. **备份当前配置** — 把 `~/Library/Rime` 下的 `*.yaml`、`*.txt` 和 `*.userdb`
   （用户词库/词频）复制到 `~/rime-backup/<时间戳>/`。`build/`、`cn_dicts`、`lua`
   等不在备份范围——它们都可用第 7 项重装 rime-ice 恢复。
9. **同步用户数据** — 发送 `SquirrelSyncNotification` 触发同步，
   同步目标目录在 `installation.yaml` 的 `sync_dir` 中配置。

### 关于自动重新部署

脚本通过发送 `SquirrelReloadNotification` 分布式通知触发部署。Squirrel ≤ 1.1.2
自带的 `Squirrel --reload` 发通知时未带 `deliverImmediately`，输入法在后台时
通知会被 AppKit 挂起、部署不触发（master 已修复），所以脚本改用 JXA（osascript）
发送即时通知。

手动修改配置时，也可编辑 `~/Library/Rime/squirrel.custom.yaml` 后从菜单栏
Squirrel 图标 → **重新部署**。配色相关的两行：

   ```yaml
   "style/color_scheme": liquid_glass_dark        # 系统浅色外观时使用的配色
   "style/color_scheme_dark": liquid_glass_dark   # 系统深色外观时使用的配色
   ```

值为 `preset_color_schemes` 下的方案名，rime-ice 自带 `purity_of_form_custom` 等，
本目录 yaml 中自定义了 `liquid_glass_light` / `liquid_glass_dark`。

## 自定义配色（preset_color_schemes）

**色值格式 `0xAABBGGRR`（BGR 序！）**：A 透明度（可省，省则不透明）、B 蓝、G 绿、R 红。
例如高亮蓝 `(61,123,224)` 写作 `0xE6E07B3D`。

可用键：

| 键 | 作用 |
|---|---|
| `back_color` / `border_color` | 候选框底色 / 描边 |
| `candidate_text_color` / `candidate_back_color` | 候选词字色 / 底色 |
| `label_color` | 序号字色 |
| `comment_text_color` | 注疏字色 |
| `text_color` / `hilited_text_color` / `hilited_back_color` | 键入码字色 / 高亮字色 / 高亮底色 |
| `preedit_back_color` | 键入码区底色 |
| `hilited_candidate_text_color` / `hilited_candidate_back_color` / `hilited_candidate_label_color` | 选中候选词字色 / 底色 / 序号色 |
| `hilited_comment_text_color` | 选中注疏字色 |

## 常见坑：小狼毫（Weasel）键名在 Squirrel 无效

以下写法 Squirrel **不支持**，会被静默忽略：

- `show_notifications: false` → 用 `show_notifications_when: never`
- `style/horizontal` → 已移除，用 `candidate_list_layout: linear/stacked`
- `style/layout/*` 整段 → Squirrel 没有 layout 子配置，键直接平铺在 `style/` 下
  - `round_corner` → `corner_radius`
  - `hilite_corner_radius` → `hilited_corner_radius`
  - `candidate_spacing` → `line_spacing`
  - `hilite_padding`、`hilite_spacing`、`margin_x/y`、`min_width` → 无对应项
- 配色里：`hilited_label_color` → `hilited_candidate_label_color`；`shadow_color` 不存在（阴影由 `style/shadow_size` 控制）

## 参考

- 官方界面配置指南：<https://github.com/LEOYoon-Tsaw/Rime_collections/blob/master/鼠鬚管介面配置指南.md>
- 完整默认配置：`/Library/Input Methods/Squirrel.app/Contents/SharedSupport/squirrel.yaml`
