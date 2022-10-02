# 电子书管理脚本
# 每本书籍有一个 ID，
#
#
# 数据数据格式
# 程序启动时先检查 ebook_info,json 比较 ebook_info.json 和 实际的目录，检查是否有电子书被删除或者添加了
# 

import hashlib
import json
import os
import sys
import time


# 日志文件
EBOOK_LOG_FILE = "ebook.log"
EBOOK_DIR = "ebook"
# 保存电子书资料简要信息的文件
EBOOK_INFO_FILE = "ebook_info.json"
# 关注的电子书的格式
EBOOK_TYPE = [".pdf"]

def show_caller():
    """ a decorator, used to append caller's infomation when
        call log function.
    """
    def show(func):
        def wrapper(*args, **kwargs):
            frame = sys._getframe(1)
            caller_info = "{}:{}:{}".format(frame.f_code.co_filename,
                                            frame.f_lineno,
                                            frame.f_code.co_name,)
            func(caller_info, *args, **kwargs)
        return wrapper
    return show

@show_caller()
def ebook_log(caller_info, fmt, *args):
    t:str = time.strftime("%Y-%m-%d %H:%M:%S", time.localtime())
    log = "{} {} {}".format(t, caller_info, fmt % args)

    print(log)
    with open(EBOOK_LOG_FILE, "a", encoding="utf8") as f:
        f.write(log + "\n",)


class BaseEbook(object):
    """ebook 基础信息."""
    def __init__(self):
        self.path = ""
        self.name = ""
        self.size = 0
        self.md5 = 0

class Ebook(BaseEbook):
    def __init__(self):
        super().__init__()
        self.tags = []
        self.id = 0

    def load_from_dict(self, ebook:dict):
        self.path = ebook.get("path")
        self.name = ebook.get("name")
        self.size = ebook.get("size")
        self.md5 = ebook.get("md5")
        self.tags = ebook.get("tags", [])
        self.id = ebook.get("id", 0)

    def dump_to_dict(self) -> dict:
        ebook = {
            "path": self.path,
            "name": self.name,
            "size": self.size,
            "md5": self.md5,
            "tags": self.tags,
            "id": self.id,
        }
        return ebook

class DiskBook(BaseEbook):
    """磁盘上的电子书的简要信息."""
    def __init__(self) -> None:
        super().__init__()

    def load(self, path:str) -> bool:
        # Basic Check
        if not path:
            ebook_log("path error, path: {}".format(path))
            return False
        ext = os.path.splitext(path)[-1]
        if not ext.lower() in EBOOK_TYPE:
            ebook_log("file extension error, path: {}".format(path))
            return False
        if not os.path.exists(path):
            ebook_log("load ebook failed, path: {}".format(path))
            return False

        file_stat = os.stat(path)
        self.path = path
        self.name = os.path.split(path)[-1]
        self.size = round(file_stat.st_size / (1024 * 1024), 2)
        with open(path, "rb") as f:
            self.md5 = hashlib.md5(f.read()).hexdigest()
        return True



class EbookMgr(object):
    def __init__(self):
        self.ebook_info = {} # k:v = path: Ebook
        self.disk_books = {} # k:v = path: DiskBook
        self.modify = False
        self.max_id = 0
        self.all_tags = set()

        self.load_ebook_info()
        self.load_disk_ebooks()
        self.check_ebook_info()
        self.analysis_tags()
        self.print_all_tags()
        self.dump_ebook_info()

    def get_new_id(self) -> int:
        id = self.max_id
        self.max_id += 1
        return id

    def append_disk_ebook(self, disk_ebook:DiskBook):
        if disk_ebook.path in self.ebook_info:
            ebook_log("ebook already in ebook_info, path: {}".format(disk_ebook.path))
            return
        ebook = Ebook()
        ebook.path = disk_ebook.path
        ebook.name = disk_ebook.name
        ebook.size = disk_ebook.size
        ebook.md5 = disk_ebook.md5
        ebook.id = self.get_new_id()
        self.ebook_info[disk_ebook.path] = ebook

    def load_ebook_info(self) -> bool:
        """加载保存了配置信息的文件."""
        if not os.path.exists(EBOOK_INFO_FILE):
            return True
        try:
            with open(EBOOK_INFO_FILE, "r", encoding="utf8") as f:
                ebook_info = json.load(f)
                self.max_id = ebook_info.get("max_id", 1000) # id 从 1000 开始
                json_ebook = ebook_info.get("ebook_info", {})
                for path, ebook in json_ebook.items():
                    self.ebook_info[path] = Ebook()
                    self.ebook_info[path].load_from_dict(ebook)
        except Exception as e:
            ebook_log("load book info failed, error: {}".format(repr(e)))
            self.ebook_info = {}
            return False
        return True

    def dump_ebook_info(self) -> bool:
        for disk_book in self.disk_books.values():
            if disk_book.path not in self.ebook_info:
                self.append_disk_ebook(disk_book)

        json_ebook = {}
        for path, ebook in self.ebook_info.items():
            json_ebook[path] = ebook.dump_to_dict()

        ebook_info = {}
        ebook_info["max_id"] = self.max_id
        ebook_info["ebook_info"] = json_ebook
        try:
            with open(EBOOK_INFO_FILE, "w", encoding="utf8") as f:
                json.dump(ebook_info, f, sort_keys=True, indent=2, ensure_ascii=False)
        except Exception as e:
            ebook_log("dump book info failed, error: {}".format(repr(e)))
            return False
        return True

    def analysis_tags(self):
        for ebook in self.ebook_info.values():
            for tag in ebook.tags:
                self.all_tags.add(tag)

    def print_all_tags(self):
        print("-------------------------- all tags --------------------------")
        for tag in self.all_tags:
            print(tag, end=" ")
        print("\n--------------------------------------------------------------")

    def filter_ebook_names(self, filenames) -> list:
        """过滤出所有符合标准的文件名."""
        files = []
        for file in filenames:
            ext = os.path.splitext(file)[-1]
            if ext.lower() in EBOOK_TYPE:
                files.append(file)
        return files

    def load_disk_ebooks(self) -> bool:
        """检查硬盘上的实际电子书资料."""
        for root, dirs, files in os.walk(EBOOK_DIR):
            # print(files)
            for name in self.filter_ebook_names(files):
                path = os.path.join(root, name)
                disk_book:DiskBook =  DiskBook()
                disk_book.load(path)
                self.disk_books[path] = disk_book
        return True

    def check_ebook_info(self) -> bool:
        """检查简要信息文件和磁盘文件的差异."""
        delete_ebooks = [] # 删除的 ebooks
        add_ebooks    = [] # 新增的 ebooks
        modify_ebooks = [] # 修改过的 eooks

        for disk_ebook in self.disk_books.values():
            path = disk_ebook.path
            ebook = self.ebook_info.get(path)
            if not ebook:
                add_ebooks.append(disk_ebook)
            else:
                if ebook.size != disk_ebook.size:
                    modify_ebooks.append(ebook)
                elif (ebook.md5 and disk_ebook.md5
                      and ebook.md5 != disk_ebook.md5):
                    modify_ebooks.append(ebook)
        for ebook in self.ebook_info.values():
            if ebook.path not in self.disk_books:
                delete_ebooks.append(ebook)

        if delete_ebooks:
            print("--------------- delete ebooks ---------------")
            for basic_ebook in delete_ebooks:
                print(basic_ebook.path)

        if add_ebooks:
            print("--------------- add ebooks ---------------")
            for basic_ebook in add_ebooks:
                print(basic_ebook.path)

        if modify_ebooks:
            print("--------------- modify ebooks ---------------")
            for basic_ebook in modify_ebooks:
                print(basic_ebook.path)
        return True


if __name__ == "__main__":
    ebook_mgr = EbookMgr()
