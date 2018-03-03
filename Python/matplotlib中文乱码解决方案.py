# -*- coding: utf-8 -*-
"""
-------------------------------------------------
   File Name：     EnvTest
   Description :
   Author :       elan
   date：          2/24/2018
-------------------------------------------------
   Change Activity:
                   2/24/2018:
-------------------------------------------------
"""

#! /usr/bin/env python
# -*- coding: utf-8 -*-

''' 获取python lib path 
import os
print(os.path.dirname(os.__file__))
'''

''' 获取python lib/site-packages path 
from distutils.sysconfig import get_python_lib
print(get_python_lib())
'''

# 建议从网上下载一个Microsoft YaHei
# 复制到路径: C:\Software\Anaconda3\Lib\site-packages\matplotlib\mpl-data\fonts\ttf
# 获取python font list
#  检查是否有Microsoft YaHei字体，如果没有那么考虑重新换一个
'''
from matplotlib.font_manager import FontManager
fm = FontManager()
print( set(f.name for f in fm.ttflist))
'''


''' 修改matplotlib-matplotlibrc文件 将中文font添加进去
1 获取matplotlibrc文件路径 / 根据lib文件进行拼接
import matplotlib
print(matplotlib.matplotlib_fname())

2  修改内容
font.family         : Microsoft YaHei
font.sans-serif     : Microsoft YaHei, ....

'''

# 在做中文测试之前 需要先清空一下cache
#   windows: C:\Users\[username]\.matplotlib\*
#   centos: /[username]/.cache/matplotlib
'''  中文测试
import matplotlib
import matplotlib.pyplot as plt
plt.figure()
plt.title(u'中文测试')
plt.show()
'''
