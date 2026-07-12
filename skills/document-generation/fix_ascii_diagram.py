#!/usr/bin/env python3
"""
修复 Markdown/文本中 ASCII Box-Drawing 图表的对齐问题。

算法（智能重建）：
    1. 追踪竖边：匹配相邻行之间的竖线，为每条竖边确定统一的目标列
    2. 对每行重建：
       - 将该行的竖线吸附到目标列
       - 竖线之间的「横线段」填充 ─
       - 竖线之间的「文本段」保留原文，以空格填充到目标宽度

用法：
    python3 fix_ascii_diagram.py <文件> --check       # 只检查
    python3 fix_ascii_diagram.py <文件> --inplace     # 原地修复
    python3 fix_ascii_diagram.py <文件> --output OUT  # 输出到新文件
"""

import argparse
import sys
import unicodedata
from collections import Counter, defaultdict
from dataclasses import dataclass
from typing import Dict, List, Tuple


# ═══════════════════════════════════════════════════════════════
# Box-Drawing 字符集
# ═══════════════════════════════════════════════════════════════

CONNECTS_DOWN  = set('│┌┐├┤┬┼')
CONNECTS_UP    = set('│└┘├┤┴┼')
HAS_VERT       = CONNECTS_DOWN | CONNECTS_UP
CONNECTS_RIGHT = set('─┌└├┼┬┴')
CONNECTS_LEFT  = set('─┐┘┤┼┬┴')
HAS_HORIZ      = CONNECTS_RIGHT | CONNECTS_LEFT
ALL_BOX        = HAS_VERT | HAS_HORIZ

# 判断是否为「横线内容行」——框中主要是 ─
PURE_HORIZ_CHARS = set('─┼┬┴├┤┌┐└┘')


def box_char(up: bool, down: bool, left: bool, right: bool) -> str:
    pattern = (up, down, left, right)
    table = {
        (0,0,0,0): '─', (0,0,0,1): '─', (0,0,1,0): '─', (0,0,1,1): '─',
        (0,1,0,0): '│', (0,1,0,1): '┌', (0,1,1,0): '┐', (0,1,1,1): '┬',
        (1,0,0,0): '│', (1,0,0,1): '└', (1,0,1,0): '┘', (1,0,1,1): '┴',
        (1,1,0,0): '│', (1,1,0,1): '├', (1,1,1,0): '┤', (1,1,1,1): '┼',
    }
    return table.get(pattern, '│')


# ═══════════════════════════════════════════════════════════════
# CJK 宽度
# ═══════════════════════════════════════════════════════════════

def char_width(ch: str) -> int:
    if ch in ('\t', '\n', '\r'):
        return 0
    w = unicodedata.east_asian_width(ch)
    return 2 if w in ('F', 'W') else 1


def vislen(text: str) -> int:
    return sum(char_width(c) for c in text)


# ═══════════════════════════════════════════════════════════════
# 行解析
# ═══════════════════════════════════════════════════════════════

@dataclass
class VertPos:
    vis_col: int
    byte_idx: int
    ch: str


def parse_line(line: str) -> Tuple[List[Tuple[int, int, str]], List[VertPos]]:
    """
    解析一行，返回 (所有字符列表, 竖线字符列表)。
    每个字符: (视觉列号, 字符索引, 字符)。
    """
    scanned = []
    verts = []
    vis = 0
    for i, ch in enumerate(line):
        scanned.append((vis, i, ch))
        if ch in HAS_VERT:
            verts.append(VertPos(vis_col=vis, byte_idx=i, ch=ch))
        vis += char_width(ch)
    return scanned, verts


def extract_indent(line: str) -> Tuple[str, int]:
    """提取行首缩进字符串及其视觉宽度。"""
    s = line[:len(line) - len(line.lstrip())]
    return s, vislen(s)


# ═══════════════════════════════════════════════════════════════
# 框图块检测
# ═══════════════════════════════════════════════════════════════

def find_blocks(lines: List[str]) -> List[Tuple[int, int]]:
    blocks = []
    in_block = False
    start = 0
    for i, line in enumerate(lines):
        has_box = any(ch in ALL_BOX for ch in line)
        if has_box and not in_block:
            start = i
            in_block = True
        elif not has_box and in_block:
            blocks.append((start, i))
            in_block = False
    if in_block:
        blocks.append((start, len(lines)))
    return blocks


# ═══════════════════════════════════════════════════════════════
# 竖边追踪（Union-Find）
# ═══════════════════════════════════════════════════════════════

def trace_edges(lines: List[str]) -> Tuple[
    List[List[VertPos]],                        # per_row_verts
    Dict[Tuple[int, int], int],                 # (row, vert_idx) → edge_id
    List[List[int]],                            # edge_id → [col, ...]
]:
    per_row = [parse_line(line)[1] for line in lines]
    n_rows = len(lines)

    parent: List[int] = []
    vertex_id: Dict[Tuple[int, int], int] = {}

    def make_node() -> int:
        nid = len(parent)
        parent.append(nid)
        return nid

    def find(x: int) -> int:
        while parent[x] != x:
            parent[x] = parent[parent[x]]
            x = parent[x]
        return x

    def union(a: int, b: int):
        ra, rb = find(a), find(b)
        if ra != rb:
            parent[ra] = rb

    for r, verts in enumerate(per_row):
        for vi in range(len(verts)):
            vertex_id[(r, vi)] = make_node()

    for r in range(n_rows - 1):
        upper = per_row[r]
        lower = per_row[r + 1]
        if not upper or not lower:
            continue

        up_sorted = sorted(range(len(upper)), key=lambda i: upper[i].vis_col)
        lo_sorted = sorted(range(len(lower)), key=lambda i: lower[i].vis_col)

        used_u, used_l = set(), set()
        pairs = []
        for ui in up_sorted:
            for li in lo_sorted:
                d = abs(upper[ui].vis_col - lower[li].vis_col)
                if d <= 3:
                    pairs.append((d, ui, li))
        pairs.sort()

        for d, ui, li in pairs:
            if ui in used_u or li in used_l:
                continue
            union(vertex_id[(r, ui)], vertex_id[(r + 1, li)])
            used_u.add(ui)
            used_l.add(li)

    groups: Dict[int, List[Tuple[int, int, int]]] = defaultdict(list)
    for (r, vi), nid in vertex_id.items():
        root = find(nid)
        groups[root].append((r, vi, per_row[r][vi].vis_col))

    vertex_to_edge = {}
    edges = []
    for members in groups.values():
        eid = len(edges)
        cols = []
        for r, vi, col in sorted(members):
            vertex_to_edge[(r, vi)] = eid
            cols.append(col)
        edges.append(cols)

    return per_row, vertex_to_edge, edges


# ═══════════════════════════════════════════════════════════════
# 智能重建
# ═══════════════════════════════════════════════════════════════

def is_box_diagram(lines: List[str]) -> bool:
    """
    判断是否为矩形盒子图（而非树形图/流程图）。
    
    特征：最左竖线列一致，且每行竖线数量相同。
    """
    if len(lines) < 2:
        return False
    
    left_cols = []
    vert_counts = []
    
    for line in lines:
        _, verts = parse_line(line)
        if verts:
            left_cols.append(verts[0].vis_col)
            vert_counts.append(len(verts))
    
    if not left_cols:
        return False
    
    # 最左列一致（容差 1）
    left_min, left_max = min(left_cols), max(left_cols)
    if left_max - left_min > 1:
        return False
    
    # 竖线数量一致（容差：多数行相同）
    if vert_counts:
        mode_count = Counter(vert_counts).most_common(1)[0][0]
        same = sum(1 for v in vert_counts if v == mode_count)
        if same < len(vert_counts) * 0.7:
            return False
    
    return True


def smart_rebuild(lines: List[str]) -> List[str]:
    """对框图块执行智能重建。非矩形盒子图保持不变。"""
    if not is_box_diagram(lines):
        return lines[:]

    per_row, vertex_to_edge, edge_cols = trace_edges(lines)

    # 计算每条边的目标列（直接用众数）
    edge_target: Dict[int, int] = {}
    for eid, cols in enumerate(edge_cols):
        edge_target[eid] = Counter(cols).most_common(1)[0][0]

    # 重建每行
    result = []
    for r, line in enumerate(lines):
        verts = per_row[r]
        if not verts:
            result.append(line)
            continue

        # 构建该行的目标列序列
        target_cols = []
        for vi, vp in enumerate(verts):
            eid = vertex_to_edge.get((r, vi))
            tgt = edge_target[eid] if eid is not None else vp.vis_col
            target_cols.append((vi, vp, tgt))

        # 将原始行划分为段：[段0, 竖线0, 段1, 竖线1, ...]
        # 段 i 是竖线 i-1 和竖线 i 之间的字符
        segments = []
        prev_bi = 0
        for vp in verts:
            seg_chars = line[prev_bi:vp.byte_idx]
            segments.append(seg_chars)
            segments.append(vp.ch)  # 竖线本身
            prev_bi = vp.byte_idx + 1
        segments.append(line[prev_bi:])  # 尾巴

        # segments 长度 = 2 * len(verts) + 1
        # segments[0] = before first vert
        # segments[1] = vert 0
        # segments[2] = between vert 0 and vert 1
        # segments[3] = vert 1
        # ...

        # 重建
        indent_str, indent_w = extract_indent(line)
        parts = []
        current_vis = indent_w

        for seg_idx in range(len(segments)):
            if seg_idx % 2 == 0:
                # 偶数索引 = 段（内容）
                vi = seg_idx // 2  # 这是哪个竖线之前的段
                seg_text = segments[seg_idx]

                if seg_idx == 0:
                    # 第一个竖线之前的内容
                    target_col = target_cols[0][2] if len(target_cols) > 0 else current_vis
                elif seg_idx == len(segments) - 1:
                    # 最后一个竖线之后的内容
                    parts.append(seg_text)
                    continue
                else:
                    # 两个竖线之间的段
                    target_col = target_cols[vi][2]

                gap = target_col - current_vis
                if gap < 0:
                    gap = 0

                # 判断这是横线段还是文本段
                has_horiz = any(ch in ('─', '═') for ch in seg_text)
                if has_horiz and all(ch in PURE_HORIZ_CHARS or ch == ' ' for ch in seg_text):
                    # 横线段：填充 ─
                    parts.append('─' * gap)
                    current_vis = target_col
                else:
                    # 文本段：保留原文，空格填充到 gap
                    seg_vis = vislen(seg_text)
                    if seg_vis < gap:
                        parts.append(seg_text + ' ' * (gap - seg_vis))
                        current_vis = target_col
                    else:
                        # 文本比 gap 宽：保留全文，推进实际位置
                        parts.append(seg_text)
                        current_vis += seg_vis

            else:
                # 奇数索引 = 竖线字符
                vi = seg_idx // 2
                vp = verts[vi]
                target_col = target_cols[vi][2]

                # 跳到目标列
                gap = target_col - current_vis
                if gap > 0:
                    parts.append(' ' * gap)
                # gap <= 0: 文本已超出目标列，竖线紧随文本放置

                # 根据实际（修正后）的连接性确定正确的 box 字符
                up, down = False, False
                left, right = False, False

                if r > 0:
                    for pvi, pc in enumerate(per_row[r - 1]):
                        peid = vertex_to_edge.get((r - 1, pvi))
                        if peid is not None and edge_target.get(peid) == target_col:
                            up = pc.ch in CONNECTS_DOWN
                            break
                    if not up:
                        up = vp.ch in CONNECTS_UP  # 保留原始上连接

                if r < len(lines) - 1:
                    for nvi, nc in enumerate(per_row[r + 1]):
                        neid = vertex_to_edge.get((r + 1, nvi))
                        if neid is not None and edge_target.get(neid) == target_col:
                            down = nc.ch in CONNECTS_UP
                            break
                    if not down:
                        down = vp.ch in CONNECTS_DOWN  # 保留原始下连接

                # 左右连接：基于相邻竖线的段类型
                # 注意：段必须包含至少一个 ─ 才视为水平连接，纯空格不算
                if vi > 0:
                    prev_target = target_cols[vi - 1][2]
                    seg_between = segments[seg_idx - 1]
                    has_horiz = any(ch in ('─', '═') for ch in seg_between)
                    left = has_horiz and prev_target < target_col
                else:
                    left = vp.ch in CONNECTS_LEFT

                if vi < len(verts) - 1:
                    next_target = target_cols[vi + 1][2]
                    seg_between = segments[seg_idx + 1]
                    has_horiz = any(ch in ('─', '═') for ch in seg_between)
                    right = has_horiz and next_target > target_col
                else:
                    right = vp.ch in CONNECTS_RIGHT

                new_ch = box_char(up, down, left, right)
                parts.append(new_ch)
                current_vis = target_col + 1

        result.append(indent_str + ''.join(parts))

    return result


# ═══════════════════════════════════════════════════════════════
# 检查模式
# ═══════════════════════════════════════════════════════════════

def check_block(lines: List[str], line_offset: int = 0) -> List[str]:
    issues = []
    if len(lines) < 2:
        return issues

    per_row, vertex_to_edge, edge_cols = trace_edges(lines)

    for eid, cols in enumerate(edge_cols):
        if len(set(cols)) <= 1:
            continue
        spread = max(cols) - min(cols)
        mode_col = Counter(cols).most_common(1)[0][0]
        severity = "轻微" if spread <= 2 else "严重"

        for r, verts in enumerate(per_row):
            for vi, vp in enumerate(verts):
                if vertex_to_edge.get((r, vi)) == eid and vp.vis_col != mode_col:
                    abs_line = line_offset + r + 1
                    issues.append(
                        f"[{severity}] 行 {abs_line} 列 {vp.vis_col}: "
                        f"竖边偏离目标列 {mode_col}（差 {mode_col - vp.vis_col:+d}, "
                        f"边跨 {spread} 列）"
                    )
                    break

    return issues


def check_all(text: str) -> List[str]:
    lines = text.split('\n')
    blocks = find_blocks(lines)
    all_issues = []
    for start, end in blocks:
        block = lines[start:end]
        all_issues.extend(check_block(block, start))
    return all_issues


def fix_all(text: str) -> str:
    lines = text.split('\n')
    blocks = find_blocks(lines)
    for start, end in reversed(blocks):
        block = lines[start:end]
        fixed = smart_rebuild(block)
        lines[start:end] = fixed
    return '\n'.join(lines)


# ═══════════════════════════════════════════════════════════════
# CLI
# ═══════════════════════════════════════════════════════════════

def main():
    parser = argparse.ArgumentParser(
        description='修复 ASCII Box-Drawing 图表的对齐问题'
    )
    parser.add_argument('input', help='输入文件')
    parser.add_argument('--inplace', '-i', action='store_true', help='原地修改')
    parser.add_argument('--output', '-o', help='输出文件')
    parser.add_argument('--check', '-c', action='store_true', help='只检查不修改')
    args = parser.parse_args()

    with open(args.input, 'r', encoding='utf-8') as f:
        text = f.read()

    if args.check:
        issues = check_all(text)
        if issues:
            print(f"发现 {len(issues)} 处对齐问题：")
            for issue in issues:
                print(f"  {issue}")
        else:
            print("所有框图对齐良好。")
        return

    result = fix_all(text)

    if args.inplace:
        with open(args.input, 'w', encoding='utf-8') as f:
            f.write(result)
        print(f"已修复: {args.input}")
    elif args.output:
        with open(args.output, 'w', encoding='utf-8') as f:
            f.write(result)
        print(f"已写入: {args.output}")
    else:
        sys.stdout.write(result)


if __name__ == '__main__':
    main()
