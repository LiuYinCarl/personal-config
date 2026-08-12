#!/bin/bash
# Squirrel（鼠须管）配置工具
#
# 交互式菜单：
#   1) 切换配色主题
#   2) 修改字体大小
#   3) 中英文切换通知开关
#   4) 管理输入法方案（F4 方案选单）
#   5) 安装/更新雾凇拼音（rime-ice）
#
# 每个功能都会修改 ~/Library/Rime 下对应的 .custom.yaml 并自动重新部署。

set -euo pipefail

RIME_DIR="$HOME/Library/Rime"
SQUIRREL_CUSTOM="$RIME_DIR/squirrel.custom.yaml"
DEFAULT_CUSTOM="$RIME_DIR/default.custom.yaml"
BUILD_SQUIRREL="$RIME_DIR/build/squirrel.yaml"

# 触发重新部署。
# Squirrel ≤ 1.1.2 自带的 `Squirrel --reload` 发分布式通知时未带
# deliverImmediately，输入法处于后台时通知会被 AppKit 挂起、部署不触发
#（master 已修复）。这里用 JXA 直接发送即时通知。
deploy() {
  # $() 是 JXA 的 Objective-C 桥语法，不是 shell 命令替换，单引号是有意的
  # shellcheck disable=SC2016
  osascript -l JavaScript -e \
    'ObjC.import("Foundation"); $.NSDistributedNotificationCenter.defaultCenter.postNotificationNameObjectUserInfoDeliverImmediately("SquirrelReloadNotification", $(), $(), true)'
}

# 读取并校验一个 1-max 之间的编号，结果存入 PICKED；EOF 时返回失败
# 用法：if ! pick_number "提示语: " 24; then return 1; fi; num="$PICKED"
PICKED=""
pick_number() {
  local prompt="$1"
  local max="$2"
  local num
  PICKED=""
  while true; do
    if ! read -r -p "$prompt" num; then
      return 1
    fi
    if [[ "$num" =~ ^[0-9]+$ ]] && [ "$num" -ge 1 ] && [ "$num" -le "$max" ]; then
      PICKED="$num"
      return 0
    fi
    echo "请输入 1-$max 之间的编号" >&2
  done
}

# ---------- 1. 切换配色主题 ----------

switch_theme() {
  if [ ! -f "$BUILD_SQUIRREL" ]; then
    echo "找不到 $BUILD_SQUIRREL，请先部署一次 Squirrel" >&2
    return 1
  fi

  # 解析全部配色方案（方案名 + 显示名）
  local schemes=() displays=() key val
  while IFS=$'\t' read -r key val; do
    schemes+=("$key")
    displays+=("$val")
  done < <(awk '
    /^preset_color_schemes:/ { inblock=1; next }
    inblock && /^#/ { next }
    inblock && /^[^ ]/ { exit }
    inblock && /^  [A-Za-z0-9_]+:$/ { key=$1; sub(/:$/,"",key); keys[++n]=key; next }
    inblock && /^    name:/ { val=$0; sub(/^    name: */,"",val); gsub(/"/,"",val); names[key]=val }
    END { for(i=1;i<=n;i++) print keys[i] "\t" names[keys[i]] }
  ' "$BUILD_SQUIRREL")

  local current_light current_dark
  current_light=$(sed -n 's|^  "style/color_scheme": *\([A-Za-z0-9_]*\).*|\1|p' "$SQUIRREL_CUSTOM")
  current_dark=$(sed -n 's|^  "style/color_scheme_dark": *\([A-Za-z0-9_]*\).*|\1|p' "$SQUIRREL_CUSTOM")
  echo "当前配色：浅色=$current_light 深色=$current_dark"
  echo
  echo "可用配色："
  local i
  for i in "${!schemes[@]}"; do
    printf "  %2d) %-22s %s\n" $((i+1)) "${schemes[$i]}" "${displays[$i]}"
  done

  local num light dark
  if ! pick_number "浅色配色编号: " "${#schemes[@]}"; then
    return 1
  fi
  light="${schemes[$((PICKED-1))]}"
  if ! read -r -p "深色配色编号（回车 = 与浅色相同）: " num; then
    return 1
  fi
  if [ -z "$num" ]; then
    dark="$light"
  elif [[ "$num" =~ ^[0-9]+$ ]] && [ "$num" -ge 1 ] && [ "$num" -le "${#schemes[@]}" ]; then
    dark="${schemes[$((num-1))]}"
  else
    echo "无效编号" >&2
    return 1
  fi

  sed -i '' \
    -e "s|^  \"style/color_scheme\":.*|  \"style/color_scheme\": $light|" \
    -e "s|^  \"style/color_scheme_dark\":.*|  \"style/color_scheme_dark\": $dark|" \
    "$SQUIRREL_CUSTOM"
  deploy
  echo "已切换并重新部署：浅色=$light 深色=$dark"
}

# ---------- 2. 修改字体大小 ----------

change_font_size() {
  local font label num
  font=$(sed -n 's|^  "style/font_point": *\([0-9]*\).*|\1|p' "$SQUIRREL_CUSTOM")
  label=$(sed -n 's|^  "style/label_font_point": *\([0-9]*\).*|\1|p' "$SQUIRREL_CUSTOM")
  echo "当前字号：正文=${font:-默认} 序号=${label:-默认}"
  echo

  while true; do
    if ! read -r -p "正文字号（8-72）: " num; then
      return 1
    fi
    if [[ "$num" =~ ^[0-9]+$ ]] && [ "$num" -ge 8 ] && [ "$num" -le 72 ]; then
      break
    fi
    echo "请输入 8-72 之间的数字" >&2
  done
  font="$num"

  if ! read -r -p "序号字号（回车 = 正文字号 - 5）: " num; then
    return 1
  fi
  if [ -z "$num" ]; then
    label=$((font - 5))
    if [ "$label" -lt 6 ]; then
      label=6
    fi
  elif [[ "$num" =~ ^[0-9]+$ ]] && [ "$num" -ge 6 ] && [ "$num" -le 72 ]; then
    label="$num"
  else
    echo "无效字号" >&2
    return 1
  fi

  sed -i '' \
    -e "s|^  \"style/font_point\":.*|  \"style/font_point\": $font|" \
    -e "s|^  \"style/label_font_point\":.*|  \"style/label_font_point\": $label|" \
    "$SQUIRREL_CUSTOM"
  deploy
  echo "已设置并重新部署：正文字号=$font 序号字号=$label"
}

# ---------- 3. 中英文切换通知开关 ----------

toggle_notifications() {
  local options=("never" "appropriate" "always")
  local descs=("从不弹通知" "仅在没有输入内容时弹（默认）" "总是弹通知")

  local current
  current=$(sed -n 's|^  show_notifications_when: *\([a-z]*\).*|\1|p' "$SQUIRREL_CUSTOM")
  echo "当前设置：${current:-未设置}"
  echo
  local i mark
  for i in "${!options[@]}"; do
    mark=" "
    if [ "${options[$i]}" = "$current" ]; then
      mark="*"
    fi
    printf "  %d%s %-12s %s\n" $((i+1)) "$mark" "${options[$i]}" "${descs[$i]}"
  done

  if ! pick_number "选择编号: " 3; then
    return 1
  fi
  local value="${options[$((PICKED-1))]}"

  sed -i '' "s|^  show_notifications_when:.*|  show_notifications_when: $value # 切换中英文时不弹出提示|" "$SQUIRREL_CUSTOM"
  deploy
  echo "已设置 show_notifications_when: $value 并重新部署"
}

# ---------- 4. 管理输入法方案 ----------

manage_schemas() {
  # 扫描全部方案（文件名即 schema_id），附带显示名
  local ids=() names=() f id name
  for f in "$RIME_DIR"/*.schema.yaml; do
    id=$(basename "$f" .schema.yaml)
    name=$(awk '/^schema:/{found=1; next} found && /^  name:/{sub(/^  name: */,""); gsub(/"/,""); print; exit}' "$f")
    ids+=("$id")
    names+=("$name")
  done

  # 当前启用的方案
  local current
  current=$(sed -n '/^  schema_list:/,$p' "$DEFAULT_CUSTOM" | sed -n 's/.*{schema: *\([A-Za-z0-9_]*\)}.*/\1/p')

  echo "当前启用：$(echo "$current" | tr '\n' ' ')"
  echo
  echo "可用方案（* = 已启用）："
  local i mark
  for i in "${!ids[@]}"; do
    mark=" "
    if echo "$current" | grep -qx "${ids[$i]}"; then
      mark="*"
    fi
    printf "  %2d%s %-22s %s\n" $((i+1)) "$mark" "${ids[$i]}" "${names[$i]}"
  done

  local picks=()
  if ! read -r -p "输入要启用的编号（空格分隔，顺序即 F4 菜单顺序）: " -a picks; then
    return 1
  fi
  if [ ${#picks[@]} -eq 0 ]; then
    echo "未选择任何方案，已取消" >&2
    return 1
  fi

  local selected=() num
  for num in "${picks[@]}"; do
    if [[ "$num" =~ ^[0-9]+$ ]] && [ "$num" -ge 1 ] && [ "$num" -le "${#ids[@]}" ]; then
      selected+=("${ids[$((num-1))]}")
    else
      echo "无效编号：$num" >&2
      return 1
    fi
  done

  # 重写 schema_list 段（保留它之前的所有内容；该段必须是文件最后一个配置块）
  local tmp
  tmp=$(mktemp)
  awk '/^  schema_list:/{exit} {print}' "$DEFAULT_CUSTOM" > "$tmp"
  echo "  schema_list:" >> "$tmp"
  for id in "${selected[@]}"; do
    echo "    - {schema: $id}" >> "$tmp"
  done
  chmod 644 "$tmp" # mktemp 默认 600，替换前恢复普通权限
  mv "$tmp" "$DEFAULT_CUSTOM"

  deploy
  echo "已启用 ${#selected[@]} 个方案并重新部署：${selected[*]}"
  echo "部署完成后按 F4 即可在方案间切换"
}

# ---------- 5. 安装 / 更新雾凇拼音 ----------

install_rime_ice() {
  # 覆盖基础文件前提醒；*.custom.yaml 和 custom_phrase.txt 不受影响
  local confirm
  if ! read -r -p "将用 rime-ice 最新版覆盖 default.yaml、squirrel.yaml 等基础文件，继续？[y/N] " confirm; then
    return 1
  fi
  if [ "$confirm" != "y" ] && [ "$confirm" != "Y" ]; then
    echo "已取消"
    return 0
  fi

  local tmp
  tmp=$(mktemp -d)
  echo "正在下载 rime-ice ..."
  # 菜单分发处的 || true 会关闭函数内的 set -e，失败必须显式处理
  if ! git clone --depth 1 "https://github.com/iDvel/rime-ice" "$tmp/rime-ice"; then
    echo "下载失败，请检查网络" >&2
    rm -rf "$tmp"
    return 1
  fi

  # 排除：版本控制文件、仓库自带的 build/（部署时会重新生成）、
  # 以及用户已有的 custom_phrase.txt（自定义短语，不能被覆盖）
  local excludes=(--exclude=.git --exclude=.github --exclude=.gitignore --exclude=build)
  if [ -f "$RIME_DIR/custom_phrase.txt" ]; then
    excludes+=(--exclude=custom_phrase.txt)
  fi

  if ! rsync -a "${excludes[@]}" "$tmp/rime-ice/" "$RIME_DIR/"; then
    echo "复制失败" >&2
    rm -rf "$tmp"
    return 1
  fi
  rm -rf "$tmp"
  echo "文件已复制到 $RIME_DIR"

  deploy
  echo "已触发重新部署，稍等几秒即可使用"
}

# ---------- 主菜单 ----------

for f in "$SQUIRREL_CUSTOM" "$DEFAULT_CUSTOM"; do
  if [ ! -f "$f" ]; then
    echo "找不到 $f，请先把配置复制到 ~/Library/Rime" >&2
    exit 1
  fi
done

echo "配置目录：$RIME_DIR"

while true; do
  echo
  echo "Squirrel 配置工具"
  echo "  1) 切换配色主题"
  echo "  2) 修改字体大小"
  echo "  3) 中英文切换通知开关"
  echo "  4) 管理输入法方案（F4 方案选单）"
  echo "  5) 安装/更新雾凇拼音（rime-ice）"
  echo "  q) 退出"
  if ! read -r -p "选择功能: " choice; then
    exit 0
  fi
  echo
  case "$choice" in
    # 功能函数出错（如输入无效）时打印错误后回到菜单，不退出整个工具
    1) switch_theme || true ;;
    2) change_font_size || true ;;
    3) toggle_notifications || true ;;
    4) manage_schemas || true ;;
    5) install_rime_ice || true ;;
    q) exit 0 ;;
    *) echo "无效选择" ;;
  esac
done
