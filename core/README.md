mbr.asm
    是内核的组成部分，—— 初始化代码。
    从BIOS接管计算机的控制权，初始化执行环境，大致包括这些部分：安装GDT、使CPU进入32位保护工作模式位内存布局规划、加载内核的剩余部分

core.asm
    是内核的主要部分，包括：公共代码段、内核数据段、内核代码段，负责分配内存，加载和重定位用户程序









# 内存布局
