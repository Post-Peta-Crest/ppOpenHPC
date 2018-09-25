MPIDIR            = 
MPIBINDIR         = 
MPILIBDIR         = 
MPIINCDIR         = 
MPILIBS           = 

PREFIX            = $(HOME)/FrontISTR
BINDIR            = $(PREFIX)/bin
LIBDIR            = $(PREFIX)/lib
INCLUDEDIR        = $(PREFIX)/include

HECMWBINDIR       = ../../bin
HECMWLIBDIR       = ../../lib
HECMWINCDIR       = ../../include
HECMWLIBS         = -lfhecmw2 -lhecmw2 -lstdc++

REFINERDIR        = $(HOME)/REVOCAP_Refiner
REFINERINCDIR     = $(REFINERDIR)/Refiner
REFINERLIBDIR     = $(REFINERDIR)/lib
REFINERLIBS       = -lRcapRefiner

CC                = mpifccpx
CFLAGS            = -Kfast,openmp
BASE_CFLAGS       = -I.
MPI_CFLAGS        = 
HECMW_CFLAGS      = -I$(HECMWINCDIR)
ALL_CFLAGS        = $(BASE_CFLAGS) $(HECMW_CFLAGS) $(MPI_CFLAGS) $(CFLAGS)
LDFLAGS           = -lm
MPI_LDFLAGS       = 
HECMW_LDFLAGS     = -L$(HECMWLIBDIR) $(HECMWLIBS)
ALL_LDFLAGS       = $(HECMW_LDFLAGS) $(MPI_LDFLAGS) $(LDFLAGS)
OPTFLAGS          =  -DNDEBUG -DHAVE_MPI

CPP               = mpiFCCpx
CPPFLAGS          = -Kfast,openmp -DMPICH_IGNORE_CXX_SEEK
BASE_CPPFLAGS     = -I.
HECMW_CPPFLAGS    = -I$(HECMWINCDIR)
ALL_CPPFLAGS      = $(BASE_CPPFLAGS) $(HECMW_CPPFLAGS) $(MPI_CFLAGS) $(CPPFLAGS)
CPPLDFLAGS        = 
HECMW_CPPLDFLAGS  = -L$(HECMWLIBDIR) $(HECMWLIBS)
ALL_CPPLDFLAGS    = $(HECMW_CPPLDFLAGS) $(MPI_LDFLAGS) $(CPPLDFLAGS)
CPPOPTFLAGS       =  -DHAVE_MPI

F90               = mpifrtpx
F90FLAGS          = -Kfast,openmp
BASE_F90FLAGS     = -I.
MPI_F90FLAGS      = 
HECMW_F90FLAGS    = -I$(HECMWINCDIR)
ALL_F90FLAGS      = $(BASE_F90FLAGS) $(HECMW_F90FLAGS) $(MPI_F90FLAGS) $(F90FLAGS)
F90LDFLAGS        = -SCALAPACK -SSL2BLAMP
MPI_F90LDFLAGS    = 
HECMW_F90LDFLAGS  = -L$(HECMWLIBDIR) $(HECMWLIBS)
ALL_F90LDFLAGS    = $(HECMW_F90LDFLAGS) $(MPI_F90LDFLAGS) $(F90LDFLAGS)
F90OPTFLAGS       = 

AR                = ar ruv
CP                = cp -f
RM                = rm -f
RANLIB            = ranlib
MKDIR             = mkdir -p

#-----------------------------------------------------------------------------#
TARGET            = libhecmw2.a
F90TARGET         = libfhecmw2.a

F90MODULEPOSTFIX  = mod
COBJFILEPOSTFIX   = o
CPPOBJFILEPOSTFIX = o
F90OBJFILEPOSTFIX = o

#-----------------------------------------------------------------------------#
.SUFFIXES:
.SUFFIXES: .o .o .o .c .cpp .f90

.c.o:
	$(CC) -c $< $(ALL_CFLAGS) $(OPTFLAGS)

.cpp.o:
	$(CPP) -c $< $(ALL_CPPFLAGS) $(CPPOPTFLAGS)

.f90.o:
	$(F90) -c $< $(ALL_F90FLAGS) $(F90OPTFLAGS)

#-----------------------------------------------------------------------------#
OBJS =

OBJSP = \
	AggregateElement.o \
	AggregateNode.o \
	API_Fortran.o \
	AssyMatrix.o \
	AssyModel.o \
	AssyVector.o \
	Beam.o \
	Beam2.o \
	BeamBase.o \
	BndVertex.o \
	BNodeMeshGrp.o \
	BoundaryEdge.o \
	BoundaryEdgeMesh.o \
	BoundaryFace.o \
	BoundaryFaceMesh.o \
	BoundaryHexa.o \
	BoundaryMesh.o \
	BoundaryNode.o \
	BoundaryNodeMesh.o \
	BoundaryParts.o \
	BoundaryPrism.o \
	BoundarySBNode.o \
	BoundaryTetra.o \
	BoundaryVolume.o \
	BoundaryVolumeMesh.o \
	BoundingBox.o \
	Calc.o \
	CommBeam.o \
	CommElement.o \
	CommFace.o \
	CommHexa.o \
	CommMesh2.o \
	CommNode.o \
	CommPrism.o \
	CommQuad.o \
	CommTetra.o \
	CommTriangle.o \
	CommunicationMesh.o \
	ContactMatrix.o \
	ContactMesh.o \
	ContactNode.o \
	DiscreteVolume.o \
	Edge2ISTR.o \
	EdgeFaceTree.o \
	EdgeTree.o \
	Element.o \
	ElementGroup.o \
	FaceProgFaceTree.o \
	FaceTree.o \
	FileIO.o \
	FileIOManage.o \
	FileReader.o \
	FileReaderAlgebra.o \
	FileReaderAssyModel.o \
	FileReaderBinCheck.o \
	FileReaderBoundaryEdge.o \
	FileReaderBoundaryEdgeMesh.o \
	FileReaderBoundaryFace.o \
	FileReaderBoundaryFaceMesh.o \
	FileReaderBoundaryNode.o \
	FileReaderBoundaryNodeMesh.o \
	FileReaderBoundaryVolume.o \
	FileReaderBoundaryVolumeMesh.o \
	FileReaderChunk.o \
	FileReaderCnt.o \
	FileReaderCommElement.o \
	FileReaderCommFace.o \
	FileReaderCommMesh.o \
	FileReaderCommMesh2.o \
	FileReaderCommNode.o \
	FileReaderCommNode_CM2.o \
	FileReaderContactMesh.o \
	FileReaderElement.o \
	FileReaderElementGroup.o \
	FileReaderElementGroupEntity.o \
	FileReaderMaterial.o \
	FileReaderNode.o \
	FileReaderRefine.o \
	FileReaderRes.o \
	FileReaderVersion.o \
	FileWriter.o \
	FileWriterAVS.o \
	FileWriterBoundaryEdge.o \
	FileWriterBoundaryFace.o \
	FileWriterBoundaryNode.o \
	FileWriterBoundaryVolume.o \
	FileWriterChunk.o \
	FileWriterCommMesh2.o \
	FileWriterContactMesh.o \
	FileWriterElement.o \
	FileWriterElementGroup.o \
	FileWriterNode.o \
	FileWriterRes.o \
	FileWriterUNS.o \
	FileWriterVTK.o \
	Film.o \
	GMGModel.o \
	HEC_MPI.o \
	HEC_MW3.o \
	Hexa.o \
	Hexa2.o \
	IndexBucket.o \
	IndexBucketMesh.o \
	ISTR2Edge.o \
	Jacobian.o \
	Logger.o \
	MasterFace.o \
	Material.o \
	Matrix.o \
	MatrixBCRS.o \
	Mesh.o \
	MeshFactory.o \
	MPCMatrix.o \
	Node.o \
	NodeConnectNodeTree.o \
	OctreeKnot.o \
	Poland.o \
	Prism.o \
	Prism2.o \
	ProgElementTree.o \
	Quad.o \
	Quad2.o \
	ScalarNode.o \
	ScalarVectorNode.o \
	ShapeFunctionBase.o \
	ShapeFunctionCatalog.o \
	ShapeHexa.o \
	ShapeHexaNic.o \
	ShapeLine.o \
	ShapePrism.o \
	ShapeQuad.o \
	ShapeTetra.o \
	ShapeTriangle.o \
	ShellBase.o \
	SkinFace.o \
	SolidBase.o \
	Solver.o \
	SolverBiCGSTAB.o \
	SolverCG.o \
	SolverGMRES.o \
	SolverGPBiCG.o \
	SolverMG.o \
	Tetra.o \
	Tetra2.o \
	Triangle.o \
	Triangle2.o \
	Vector.o \
	VectorNode.o \
	Vertex.o

OBJSF =

HEADERS = \
	AggregateElement.h \
	AggregateNode.h \
	API_Fortran.h \
	API_Fortran.hxx \
	AssyMatrix.h \
	AssyModel.h \
	AssyVector.h \
	Beam.h \
	Beam2.h \
	BeamBase.h \
	BndVertex.h \
	BNodeMeshGrp.h \
	BoundaryEdge.h \
	BoundaryEdgeMesh.h \
	BoundaryFace.h \
	BoundaryFaceMesh.h \
	BoundaryGroup.h \
	BoundaryHexa.h \
	BoundaryMesh.h \
	BoundaryNode.h \
	BoundaryNodeMesh.h \
	BoundaryParts.h \
	BoundaryPrism.h \
	BoundarySBNode.h \
	BoundaryTetra.h \
	BoundaryType.h \
	BoundaryVolume.h \
	BoundaryVolumeMesh.h \
	BoundingBox.h \
	Calc.h \
	CodeType.h \
	CommBeam.h \
	CommElement.h \
	CommFace.h \
	CommHexa.h \
	CommMesh2.h \
	CommNode.h \
	CommonFile.h \
	CommonStd.h \
	CommPrism.h \
	CommQuad.h \
	CommTetra.h \
	CommTriangle.h \
	CommunicationMesh.h \
	ContactMatrix.h \
	ContactMesh.h \
	ContactNode.h \
	DeleteObject.h \
	DiscreteVolume.h \
	Edge2ISTR.h \
	EdgeFaceTree.h \
	EdgeTree.h \
	Element.h \
	Element4Factory.h \
	ElementGroup.h \
	ElementProperty.h \
	ElementType.h \
	EqnTerm.h \
	Equation.h \
	FaceProgFaceTree.h \
	FaceTree.h \
	FileBlockName.h \
	FileBlockNameMW2.h \
	FileIO.h \
	FileIOManage.h \
	FileReader.h \
	FileReaderAlgebra.h \
	FileReaderAssyModel.h \
	FileReaderBinCheck.h \
	FileReaderBoundaryEdge.h \
	FileReaderBoundaryEdgeMesh.h \
	FileReaderBoundaryFace.h \
	FileReaderBoundaryFaceMesh.h \
	FileReaderBoundaryNode.h \
	FileReaderBoundaryNodeMesh.h \
	FileReaderBoundaryVolume.h \
	FileReaderBoundaryVolumeMesh.h \
	FileReaderChunk.h \
	FileReaderCnt.h \
	FileReaderCommElement.h \
	FileReaderCommFace.h \
	FileReaderCommMesh.h \
	FileReaderCommMesh2.h \
	FileReaderCommNode.h \
	FileReaderCommNode_CM2.h \
	FileReaderContactMesh.h \
	FileReaderElement.h \
	FileReaderElementGroup.h \
	FileReaderElementGroupEntity.h \
	FileReaderMaterial.h \
	FileReaderNode.h \
	FileReaderRefine.h \
	FileReaderRes.h \
	FileReaderVersion.h \
	FileWriter.h \
	FileWriterAVS.h \
	FileWriterBoundaryEdge.h \
	FileWriterBoundaryFace.h \
	FileWriterBoundaryNode.h \
	FileWriterBoundaryVolume.h \
	FileWriterChunk.h \
	FileWriterCommMesh2.h \
	FileWriterContactMesh.h \
	FileWriterElement.h \
	FileWriterElementGroup.h \
	FileWriterNode.h \
	FileWriterRes.h \
	FileWriterUNS.h \
	FileWriterVTK.h \
	Film.h \
	GMGModel.h \
	HEC_MPI.h \
	HEC_MW3.h \
	HEC_MW3.hxx \
	HEC_MW3_For.h \
	Hexa.h \
	Hexa2.h \
	IndexBucket.h \
	IndexBucketMesh.h \
	IndexValuePair.h \
	Info.h \
	ISTR2Edge.h \
	Jacobian.h \
	Logger.h \
	LoggerType.h \
	MasterFace.h \
	Material.h \
	MatrialPropType.h \
	Matrix.h \
	MatrixBCRS.h \
	Mesh.h \
	MeshFactory.h \
	MPCMatrix.h \
	MPCValueType.h \
	Node.h \
	NodeConnectNodeTree.h \
	NodeType.h \
	OctreeKnot.h \
	Poland.h \
	Prism.h \
	Prism2.h \
	ProgElementTree.h \
	Quad.h \
	Quad2.h \
	QuickSort.h \
	ScalarNode.h \
	ScalarVectorNode.h \
	ShapeFunctionBase.h \
	ShapeFunctionCatalog.h \
	ShapeHexa.h \
	ShapeHexaNic.h \
	ShapeLine.h \
	ShapePrism.h \
	ShapeQuad.h \
	ShapeTetra.h \
	ShapeTriangle.h \
	ShellBase.h \
	SkinFace.h \
	SolidBase.h \
	SolutionType.h \
	Solver.h \
	SolverBiCGSTAB.h \
	SolverCG.h \
	SolverGMRES.h \
	SolverGPBiCG.h \
	SolverMG.h \
	Tetra.h \
	Tetra2.h \
	Triangle.h \
	Triangle2.h \
	TypeDef.h \
	Vector.h \
	VectorNode.h \
	Vertex.h

#-----------------------------------------------------------------------------#
all: build-default

build-default: $(TARGET) $(F90TARGET) install-header

build-serial: $(TARGET) $(F90TARGET) install-header

build-without-f: $(TARGET) install-header

$(TARGET): $(OBJS) $(OBJSP)
	$(AR) $(HECMWLIBDIR)/$@ $(OBJS)
	$(AR) $(HECMWLIBDIR)/$@ $(OBJSP)

$(F90TARGET): $(OBJSF)
	$(AR) $(HECMWLIBDIR)/$@ $(OBJSF)

$(OBJS): $(HEADERS)

$(OBJSP): $(HEADERS)

$(OBJSF):

install-header: $(HEADERS)
	$(CP) $(HEADERS) $(HECMWINCDIR)/.

install-module:
	$(CP) *.$(F90MODULEPOSTFIX) $(HECMWINCDIR)/.

install:

clean:
	$(RM) *.$(COBJFILEPOSTFIX) *.$(CPPOBJFILEPOSTFIX) *.$(F90OBJFILEPOSTFIX) *.$(F90MODULEPOSTFIX)

distclean: clean
