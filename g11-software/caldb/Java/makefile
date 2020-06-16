# using the javac compiler for Caldb
# Invoke with: make

JVC = javac
JVCFLAGS = -classpath .:/group/clas/mysql/mm.mysql-2.0.4-bin.jar:/group/clas/mysql/jas.jar
.SUFFIXES : .class .java
.java.class :
	$(JVC) $(JVCFLAGS) $<

all:  \
	JDBCDataSource.class \
	RSystem.class \
	RSubSystem.class \
	RItem.class \
	RConstantSet.class \
	RChannelData.java \
	RSystemsCaldb.class \
	ArrayDataSource.class \
	HistoView.class \
	Caldbjview.class \
	DefaultComboBoxModelHack.class \
	JCaldbview.class

JDBCDataSource.class: JDBCDataSource.java
RSystem.class: RSystem.java
RSubSystem.class: RSubSystem.java
RItem.class: RItem.java
RConstantSet.class: RConstantSet.java
RChannelData.class: RChannelData.java
RSystemsCaldb.class: RSystemsCaldb.java
HistoView.class: HistoView.java
ArrayDataSource.class: ArrayDataSource.java
Caldbjview.class: Caldbjview.java
DefaultComboBoxModelHack.class:	DefaultComboBoxModelHack.java 
JCaldbview.class:	JCaldbview.java
