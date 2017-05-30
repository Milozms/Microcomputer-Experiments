# Microcomputer-Experiments
微机实验 综合实验报告

基于双机串行通信的语音对讲系统

Milo


摘要

本实验项目通过数模、模数转换和双机串行通信等微机模块和DOS文件操作系统调用，实现了单机录音、双机实时对讲、双机录音对讲三个主要功能。
目前单机录音、双机实时对讲、双机录音对讲三个模块均已可以正常工作。

关键词：录音机 双机串行通信 数模模数转换


目标要求

本项目包括三个功能模块：

一、	单机录音：通过键盘操作，按R键开始录音，按E键结束，结束后按P键开始播放。

二、	双机实时对讲：两台机器连接好线路后，同时运行程序，在机器A的键盘按下R键后立即开始录音，录音时实时将数据传到机器B并播放，实现对讲功能。

三、	双机录音对讲：两台机器连接好线路后，同时运行程序，在机器A的键盘按下R键后立即开始录音并存入文件，按E键结束后随即将数据发送到机器B，机器B将收到的数据存入文件，按下P键可播放。


设计和实施方案

设计方案简介

一、	单机录音：通过数模、模数转换实现录音功能。录音采样频率可通过调整参数自由调整，默认为10KHz（最大2MHz）。录音数据存入文件，大小不受限制。模数转换后将数据存入内存中大小为1024字节的缓冲区，缓冲区满后写入文件。

二、	双机实时对讲：两台机器连接好线路后，同时运行程序，在机器A的键盘按下R键后立即开始录音，录音时实时将数据传到机器B，保持录音采样频率和串行通信的数据发送频率一致，均为1960Hz（波特率15.625kbps，16倍时钟，8251的时钟频率为250kHz）；通过中断方式接受数据，在中断处理程序中，将收到的音频信息立刻输出到数模转换芯片，即可实现实时对讲功能。（注：为保证结束后没有声音，发送的最后一个数据必须人为设为0）

三、	双机录音对讲：两台机器连接好线路后，同时运行程序，在机器A的键盘按下R键后立即开始录音并存入文件（同样使用缓冲区）；按E键后先发送录音时写入文件的次数N（文件的大小为1024*N字节），再读取文件，每次读取1024字节并存入缓冲区，再将缓冲区中数据通过串行通信芯片发送；保持录音采样频率和串行通信的数据发送频率一致，均为1960Hz（波特率15.625kbps，16倍时钟，8251的时钟频率为250kHz）；机器B将收到的数据存入内存中大小为1024字节的缓冲区，缓冲区满后写入文件，按下P键可播放（注：播放前要将文件的头两个字符复制到存储N值的临时变量，用于控制播放时长）。

程序框图

录音子程序：见附录图1；

播放录音子程序：见附录图2；

双机录音对讲模块与中断处理程序：见附录图3、4；

硬件连线方案

 
实施条件

本实验用到的设备有：

带DOS操作系统的微型计算机一台；

连接到上述微型计算机的实验箱一个；

串行通信接口芯片（8251A）；

数模转换芯片（DAC0832）；

模数转换芯片（ADC0809）；

并行I/O接口芯片（8255A）；

可编程定时/计数器芯片（8253）；

麦克风和扬声器各一个。


功能测试及结果分析

连接麦克风和扬声器后，可直接测试录音和对讲的效果。

另外也可通过实验使用的微机自带的LLL51程序测试双机连接是否正常。


讨论

本实验进行过程中主要遇到了两个问题：

一、由于8251A芯片异步通信模式下波特率最高为19.2Kbps，限制了本实验第二、三个功能的录音采样频率不能超过2400Hz，明显降低了音质。但录制人声时，在这个频率下声音尚可分辨。

二、实现“双机录音对讲”模块时，最初采用了在中断处理程序中判断是否为头、写入缓冲区的做法，虽然没有违规操作，但由于接收数据的频率太高，仍然造成了丢失大量数据的错误。经过重写后，采用了在中断处理程序中只做简单的三个操作、而将其他的操作转移到中断处理程序外的做法，解决了这个问题，详情见附录的程序框图。经核查没有丢失数据。（为了简便，将最后的不足1024字节的部分省去，不到半秒钟对录音不会有明显影响。）

附录（程序框图）
