# Weasel（小狼毫）自定义配置说明

本目录存放 Windows 小狼毫 `%APPDATA%\Rime`（即 `C:\Users\<用户名>\AppData\Roaming\Rime`）
下两个 `.custom.yaml` 的备份，配合雾凇拼音（rime-ice）使用。
修改后复制到该目录，然后在托盘图标右键菜单 → **重新部署** 生效。

macOS 鼠须管版本见 `../squirrel/`，两端色值可直接互通（都是 `0xAABBGGRR`）。

## 文件说明

### default.custom.yaml（全局）

```yaml
patch:
  "menu/page_size": 7        # 候选个数，最多 9
  schema_list:               # 注意：这会替换整个方案列表，没列出的方案从 F4 菜单消失
    - {schema: rime_ice}
```

### weasel.custom.yaml（界面）

- `show_notifications: false` — 切换中英文不弹提示，顶层键（不在 style 下）
- `style/color_scheme` — 配色方案名，于 `preset_color_schemes` 下设定；小狼毫没有明暗双主题，只能指定一个
- `style/horizontal: true` — 横向候选（`false` 竖排）
- `style/inline_preedit: true` — 拼音内嵌在输入位置
- `style/font_point` / `style/label_font_point` — 正文字号 / 序号字号
- 布局键都在 `style/layout/` 子配置下（这是小狼毫特有结构）：
  - `corner_radius` — 外框圆角
  - `round_corner` — 高亮框圆角（别名 `hilited_corner_radius`；**注意没有 `hilite_corner_radius` 这个键**）
  - `border_width`、`min_width`、`margin_x/y`、`candidate_spacing`、`hilite_padding`、`hilite_spacing`
  - `shadow_radius`、`shadow_offset_x/y` — 阴影几何；配色里的 `shadow_color` 需配合 `shadow_radius > 0` 才可见

## 自定义配色（preset_color_schemes）

**色值格式 `0xAABBGGRR`（BGR 序！）**：A 透明度（可省）、B 蓝、G 绿、R 红。
与鼠须管格式相同，同一份色值两端通用。

可用键（均可在配色内覆写 style 属性）：

| 键 | 作用 |
|---|---|
| `back_color` / `border_color` / `shadow_color` | 候选框底色 / 描边 / 阴影 |
| `candidate_text_color` / `candidate_back_color` / `candidate_shadow_color` | 候选词字色 / 底色 / 底色块阴影 |
| `label_color` / `hilited_label_color` | 序号字色 / 选中序号色 |
| `comment_text_color` / `hilited_comment_text_color` | 注疏字色 / 选中注疏色 |
| `text_color` / `hilited_text_color` / `hilited_back_color` / `hilited_shadow_color` | 键入码相关 |
| `hilited_candidate_text_color` / `hilited_candidate_back_color` / `hilited_candidate_shadow_color` / `hilited_candidate_label_color` | 选中候选词相关 |

## 切换配色样式

小狼毫没有运行时换肤菜单：编辑 `weasel.custom.yaml` 的 `"style/color_scheme": <方案名>`，
托盘图标右键 → **重新部署**。自带配色（aqua、azure、google 等）见默认 weasel.yaml。

## 与鼠须管（Squirrel）键名对照

两端键名**不一样**，配置文件不能直接互拷：

| 功能 | Weasel（小狼毫） | Squirrel（鼠须管） |
|---|---|---|
| 开关通知 | `show_notifications: false` | `show_notifications_when: never` |
| 横排候选 | `style/horizontal: true` | `style/candidate_list_layout: linear` |
| 布局键位置 | `style/layout/*` 子配置 | 平铺在 `style/` 下 |
| 外框圆角 | `layout/corner_radius` | `corner_radius` |
| 高亮框圆角 | `layout/round_corner` | `hilited_corner_radius` |
| 候选间距 | `layout/candidate_spacing` | `line_spacing`（仅 stacked）|
| 边距 / 高亮内边距 | `layout/margin_x/y`、`hilite_padding` 等 | 无对应项 |
| 阴影 | `shadow_color` + `layout/shadow_radius` | `style/shadow_size` |
| 明暗双主题 | 不支持 | `color_scheme` + `color_scheme_dark` |

## 参考

- 小狼毫默认配置（全部可用键）：<https://raw.githubusercontent.com/rime/weasel/master/output/data/weasel.yaml>
- 鼠须管官方界面配置指南：<https://github.com/LEOYoon-Tsaw/Rime_collections/blob/master/鼠鬚管介面配置指南.md>
