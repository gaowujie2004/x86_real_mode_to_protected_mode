# 保护模式下的中断和抢占式任务切换

本章有两个大的任务：1）保护模式下的中断和实模式下的中断不一样，在保护模式下使用中断；2）使用时钟中断来实现抢占式任务切换


# 代码说明
- mbr.asm   复用第17章，协同式任务切换，没有任何修改
- core.asm  复用第17章，协同式任务切换，有修改
- app.asm   复用第17章，协同式任务切换，有修改