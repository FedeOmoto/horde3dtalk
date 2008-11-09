| package |
package := Package name: 'Horde3DTalk'.
package paxVersion: 1;
	basicComment: ''.


package classNames
	add: #Horde3DAnimationResource;
	add: #Horde3DCameraNode;
	add: #Horde3DCodeResource;
	add: #Horde3DEffectResource;
	add: #Horde3DEmitterNode;
	add: #Horde3DEngine;
	add: #Horde3DError;
	add: #Horde3DFontMaterialResource;
	add: #Horde3DGeometryResource;
	add: #Horde3DGroupNode;
	add: #Horde3DJointNode;
	add: #Horde3DLibrary;
	add: #Horde3DLightNode;
	add: #Horde3DMaterialResource;
	add: #Horde3DMeshNode;
	add: #Horde3DModelNode;
	add: #Horde3DNode;
	add: #Horde3DNotification;
	add: #Horde3DOptions;
	add: #Horde3DPipelineResource;
	add: #Horde3DPresenter;
	add: #Horde3DResource;
	add: #Horde3DResourceManager;
	add: #Horde3DScene;
	add: #Horde3DSceneGraphResource;
	add: #Horde3DShaderResource;
	add: #Horde3DShell;
	add: #Horde3DTexture2DResource;
	add: #Horde3DTextureCubeResource;
	add: #Horde3DUtilsLibrary;
	add: #Horde3DView;
	add: #Knight;
	yourself.

package methodNames
	add: #Integer -> #value;
	add: 'FLOATArray class' -> #with:with:with:with:;
	yourself.

package globalNames
	add: #Horde3DConstants;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: (IdentitySet new
	add: '..\Object Arts\Dolphin\Base\Dolphin';
	add: '..\Object Arts\Dolphin\MVP\Base\Dolphin MVP Base';
	yourself).

package!

"Class Definitions"!

Object subclass: #Horde3DEngine
	instanceVariableNames: 'horde3DLibrary horde3DUtilsLibrary options scene resourceManager hasOverlays cameras mainLoopProcess pause fps'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Object subclass: #Horde3DOptions
	instanceVariableNames: 'horde3DLibrary'
	classVariableNames: ''
	poolDictionaries: 'Horde3DConstants'
	classInstanceVariableNames: ''!
Object subclass: #Horde3DResourceManager
	instanceVariableNames: 'horde3DLibrary horde3DUtilsLibrary'
	classVariableNames: ''
	poolDictionaries: 'Horde3DConstants'
	classInstanceVariableNames: ''!
Object subclass: #Horde3DScene
	instanceVariableNames: 'horde3DLibrary'
	classVariableNames: ''
	poolDictionaries: 'Horde3DConstants'
	classInstanceVariableNames: ''!
Error subclass: #Horde3DError
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Notification subclass: #Horde3DNotification
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalLibrary subclass: #Horde3DLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
ExternalLibrary subclass: #Horde3DUtilsLibrary
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
SDWORD subclass: #Horde3DNode
	instanceVariableNames: 'horde3DLibrary scene'
	classVariableNames: ''
	poolDictionaries: 'Horde3DConstants'
	classInstanceVariableNames: ''!
SDWORD subclass: #Horde3DResource
	instanceVariableNames: 'horde3DLibrary resourceManager'
	classVariableNames: ''
	poolDictionaries: 'Horde3DConstants'
	classInstanceVariableNames: ''!
Horde3DNode subclass: #Horde3DCameraNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Horde3DNode subclass: #Horde3DEmitterNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Horde3DNode subclass: #Horde3DGroupNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Horde3DNode subclass: #Horde3DJointNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Horde3DNode subclass: #Horde3DLightNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: 'Horde3DConstants'
	classInstanceVariableNames: ''!
Horde3DNode subclass: #Horde3DMeshNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Horde3DNode subclass: #Horde3DModelNode
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Horde3DResource subclass: #Horde3DAnimationResource
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Horde3DResource subclass: #Horde3DCodeResource
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Horde3DResource subclass: #Horde3DEffectResource
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Horde3DResource subclass: #Horde3DGeometryResource
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Horde3DResource subclass: #Horde3DMaterialResource
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Horde3DResource subclass: #Horde3DPipelineResource
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Horde3DResource subclass: #Horde3DSceneGraphResource
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Horde3DResource subclass: #Horde3DShaderResource
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Horde3DResource subclass: #Horde3DTexture2DResource
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Horde3DResource subclass: #Horde3DTextureCubeResource
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Horde3DMaterialResource subclass: #Horde3DFontMaterialResource
	instanceVariableNames: 'horde3DUtilsLibrary'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Presenter subclass: #Horde3DPresenter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Shell subclass: #Horde3DShell
	instanceVariableNames: 'horde3DPresenter engine'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Horde3DShell subclass: #Knight
	instanceVariableNames: 'hdrPipeline forwardPipeline font logo knight particleSystem cameraPosition cameraRotation animationTime blendWeight'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
View subclass: #Horde3DView
	instanceVariableNames: 'hDC'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"Global Aliases"!


"Loose Methods"!

!FLOATArray class methodsFor!

with: element1 with: element2 with: element3 with: element4 
	"Answer an instance of the receiver containing the <Float>
	arguments as its elements."

	^(self new: 4)
		at: 1 put: element1;
		at: 2 put: element2;
		at: 3 put: element3;
		at: 4 put: element4;
		yourself! !
!FLOATArray class categoriesFor: #with:with:with:with:!public! !

!Integer methodsFor!

value
	"Answer the value of the receiver."

	^self! !
!Integer categoriesFor: #value!public! !

"End of package definition"!

"Source Globals"!

Smalltalk at: #Horde3DConstants put: (PoolConstantsDictionary named: #Horde3DConstants)!
Horde3DConstants at: 'Animation' put: 16r3!
Horde3DConstants at: 'AnisotropyFactor' put: 16r4!
Horde3DConstants at: 'AttachmentString' put: 16r2!
Horde3DConstants at: 'BottomPlane' put: 16r25D!
Horde3DConstants at: 'Camera' put: 16r6!
Horde3DConstants at: 'Code' put: 16r5!
Horde3DConstants at: 'Col_B' put: 16r1F9!
Horde3DConstants at: 'Col_G' put: 16r1F8!
Horde3DConstants at: 'Col_R' put: 16r1F7!
Horde3DConstants at: 'DebugViewMode' put: 16rB!
Horde3DConstants at: 'Effect' put: 16r9!
Horde3DConstants at: 'Emitter' put: 16r7!
Horde3DConstants at: 'FarPlane' put: 16r260!
Horde3DConstants at: 'FastAnimation' put: 16r7!
Horde3DConstants at: 'FontMaterial' put: 16rB!
Horde3DConstants at: 'FOV' put: 16r1F6!
Horde3DConstants at: 'Geometry' put: 16r2!
Horde3DConstants at: 'Group' put: 16r1!
Horde3DConstants at: 'Joint' put: 16r4!
Horde3DConstants at: 'LeftPlane' put: 16r25B!
Horde3DConstants at: 'Light' put: 16r5!
Horde3DConstants at: 'LoadTextures' put: 16r6!
Horde3DConstants at: 'Material' put: 16r4!
Horde3DConstants at: 'MaterialRes' put: 16r1F4!
Horde3DConstants at: 'MaxLogLevel' put: 16r1!
Horde3DConstants at: 'MaxNumMessages' put: 16r2!
Horde3DConstants at: 'Mesh' put: 16r3!
Horde3DConstants at: 'Model' put: 16r2!
Horde3DConstants at: 'Name' put: 16r1!
Horde3DConstants at: 'NearPlane' put: 16r25F!
Horde3DConstants at: 'OcclusionCulling' put: 16r262!
Horde3DConstants at: 'Orthographic' put: 16r261!
Horde3DConstants at: 'OutputBufferIndex' put: 16r25A!
Horde3DConstants at: 'OutputTex' put: 16r259!
Horde3DConstants at: 'Pipeline' put: 16rA!
Horde3DConstants at: 'PipelineRes' put: 16r258!
Horde3DConstants at: 'Radius' put: 16r1F5!
Horde3DConstants at: 'RightPlane' put: 16r25C!
Horde3DConstants at: 'RootNode' put: 16r1!
Horde3DConstants at: 'SampleCount' put: 16r9!
Horde3DConstants at: 'SceneGraph' put: 16r1!
Horde3DConstants at: 'Shader' put: 16r6!
Horde3DConstants at: 'ShadowMapBias' put: 16r1FC!
Horde3DConstants at: 'ShadowMapCount' put: 16r1FA!
Horde3DConstants at: 'ShadowMapSize' put: 16r8!
Horde3DConstants at: 'ShadowSplitLambda' put: 16r1FB!
Horde3DConstants at: 'TexCompression' put: 16r5!
Horde3DConstants at: 'Texture2D' put: 16r7!
Horde3DConstants at: 'TextureCube' put: 16r8!
Horde3DConstants at: 'TopPlane' put: 16r25E!
Horde3DConstants at: 'TrilinearFiltering' put: 16r3!
Horde3DConstants at: 'Undefined' put: 16r0!
Horde3DConstants at: 'WireframeMode' put: 16rA!
Horde3DConstants shrink!

"Classes"!

Horde3DEngine guid: (GUID fromString: '{F98111A8-F042-4B7B-81E9-FCD719A221B1}')!
Horde3DEngine comment: ''!
!Horde3DEngine categoriesForClass!Kernel-Objects! !
!Horde3DEngine methodsFor!

addCamera: aHorde3DCameraNode 
	"Private - Add aHorde3DCameraNode to the receiver's cameras instance variable."

	cameras at: aHorde3DCameraNode name put: aHorde3DCameraNode!

cameras
	"Answer the receiver's cameras instance variable."

	^cameras!

clearOverlays
	"Private - Remove all overlays that were added to the reveiver."

	hasOverlays ifFalse: [^self].
	horde3DLibrary clearOverlays.
	hasOverlays := false!

currentCamera
	"Answer the camera currently used by the #render method."

	^cameras at: #currentCamera ifAbsent: []!

currentCamera: aHorde3DCameraNode 
	"Set the camera to be used by the #render method."

	self currentCamera 
		ifNotNil: 
			[:currentCamera | 
			currentCamera == aHorde3DCameraNode ifTrue: [^currentCamera].
			self addCamera: currentCamera].
	^cameras at: #currentCamera put: aHorde3DCameraNode!

fps
	"Answer the receiver's frames per seconds."

	^fps!

hasOverlays: aBoolean 
	"Private - Set the receiver's hasOverlays instance variable to aBoolean."

	hasOverlays := aBoolean!

initialize
	"Private - Initialize the receiver."

	horde3DLibrary := Horde3DLibrary default.
	horde3DUtilsLibrary := Horde3DUtilsLibrary default.
	options := Horde3DOptions new.
	scene := Horde3DScene new.
	resourceManager := Horde3DResourceManager new.
	cameras := LookupTable new.
	hasOverlays := false.
	pause := false.
	self registerEventHandlers!

isPaused
	"Answer whether the receiver is paused or not."

	^pause!

onCameraCreated: aHorde3DCameraNode 
	"Private - Handler for the camera created event."

	self currentCamera ifNil: [^self currentCamera: aHorde3DCameraNode].
	self addCamera: aHorde3DCameraNode!

options
	"Answer the receiver's options instance variable."

	^options!

pause: aBoolean 
	"Set the receiver's pause instance variable to aBoolean."

	pause := aBoolean!

registerEventHandlers
	"Private - Register with scene and resource manager events."

	scene 
		when: #cameraCreated:
		send: #onCameraCreated:
		to: self.
	resourceManager 
		when: #showOverlay
		send: #hasOverlays:
		to: self
		with: true!

render
	"Render the receiver's scene from the currentCamera perspective."

	| currentCamera |
	currentCamera := self currentCamera.
	(horde3DLibrary render: currentCamera value) not 
		ifTrue: [Horde3DError signal: 'Error trying to render the scene'].
	self clearOverlays!

resourceManager
	"Answer the receiver's resourceManager instance variable."

	^resourceManager!

runMainLoopOn: anObject 
	"Run the receiver's main loop on anObject (anObject>>mainLoop).

	TODO: refactor this method.
	There should be an #fps method on the receiver, allowing to call
	Horde3DFontMaterialResource>>showFrameStats: whithout its argument (in order to do this,
	instances of Horde3DResourceManager should know the engine it belongs to. Also make
	instances of Horde3DScene know the engine it belongs to).

	NOTE: replace definitions of #scene, #scene:, #resourceManager and #resourceManager: by
	#owner and #owner: for better clarity?"

	| frames seconds |
	frames := 0.
	fps := 30.0.
	seconds := Time microsecondClockValue / 1000000.0.
	mainLoopProcess := 
			[
			[| currentSeconds |
			frames := frames + 1.
			frames >= 3 
				ifTrue: 
					[currentSeconds := Time microsecondClockValue / 1000000.0.
					fps := frames / (currentSeconds - seconds).
					frames := 0.
					seconds := currentSeconds].
			pause ifFalse: [anObject mainLoop] ifTrue: [(Delay forMilliseconds: 1) wait].
			self render.
			self swapBuffers] 
					repeat] 
					fork!

scene
	"Answer the receiver's scene instance variable."

	^scene!

stopMainLoop
	"Stop running the receiver's main loop."

	mainLoopProcess ifNotNil: [mainLoopProcess terminate]!

swapBuffers
	"Private - Display the receiver's rendered image."

	horde3DUtilsLibrary swapBuffers! !
!Horde3DEngine categoriesFor: #addCamera:!accessing!private! !
!Horde3DEngine categoriesFor: #cameras!accessing!public! !
!Horde3DEngine categoriesFor: #clearOverlays!private! !
!Horde3DEngine categoriesFor: #currentCamera!accessing!public! !
!Horde3DEngine categoriesFor: #currentCamera:!accessing!public! !
!Horde3DEngine categoriesFor: #fps!accessing!public! !
!Horde3DEngine categoriesFor: #hasOverlays:!accessing!private! !
!Horde3DEngine categoriesFor: #initialize!initializing!private! !
!Horde3DEngine categoriesFor: #isPaused!public! !
!Horde3DEngine categoriesFor: #onCameraCreated:!private! !
!Horde3DEngine categoriesFor: #options!accessing!public! !
!Horde3DEngine categoriesFor: #pause:!accessing!public! !
!Horde3DEngine categoriesFor: #registerEventHandlers!event handlers!private! !
!Horde3DEngine categoriesFor: #render!public! !
!Horde3DEngine categoriesFor: #resourceManager!accessing!public! !
!Horde3DEngine categoriesFor: #runMainLoopOn:!public! !
!Horde3DEngine categoriesFor: #scene!accessing!public! !
!Horde3DEngine categoriesFor: #stopMainLoop!public! !
!Horde3DEngine categoriesFor: #swapBuffers!private! !

!Horde3DEngine class methodsFor!

icon
	"Answer the receiver's icon."

	^Icon fromId: 'ClassHierarchyDiagram.ico'!

new
	"Answer a new initialized instance of the receiver."

	^super new initialize! !
!Horde3DEngine class categoriesFor: #icon!constants!public! !
!Horde3DEngine class categoriesFor: #new!public! !

Horde3DOptions guid: (GUID fromString: '{862F38E1-EB47-4F22-89FF-1E713A054CA4}')!
Horde3DOptions comment: 'Horde3D engine option parameters.'!
!Horde3DOptions categoriesForClass!Kernel-Objects! !
!Horde3DOptions methodsFor!

anisotropyFactor
	"Answer the anisotropic filtering quality."

	^(horde3DLibrary getOption: AnisotropyFactor) asInteger!

anisotropyFactor: anInteger 
	"Set the quality for anisotropic filtering; only affects textures that are loaded after setting
	the option (Values: 1, 2, 4, 8; Default: 1)."

	^horde3DLibrary setOption: AnisotropyFactor value: anInteger!

debugViewMode
	"Answer the debug view mode option parameter value."

	^(horde3DLibrary getOption: DebugViewMode) asInteger asBoolean!

debugViewMode: aBoolean 
	"Enable or disable debug view where geometry is rendered in wireframe without shaders and 
	lights are visualized using their screen space bounding box (Default: false)."

	^horde3DLibrary setOption: DebugViewMode value: aBoolean asParameter!

fastAnimation
	"Answer the animations inter-frame interpolation option parameter value."

	^(horde3DLibrary getOption: FastAnimation) asInteger asBoolean!

fastAnimation: aBoolean 
	"Disable or enable inter-frame interpolation for animations (Default: true)."

	^horde3DLibrary setOption: FastAnimation value: aBoolean asParameter!

initialize
	"Private - Initialize the receiver."

	horde3DLibrary := Horde3DLibrary default!

loadTextures
	"Answer the load textures option parameter value."

	^(horde3DLibrary getOption: LoadTextures) asInteger asBoolean!

loadTextures: aBoolean 
	"Enable or disable loading of texture images; option can be used to minimize loading times
	for testing (Default: true)."

	^horde3DLibrary setOption: LoadTextures value: aBoolean asParameter!

logLevel
	"Answer the log level option parameter value."

	^(horde3DLibrary getOption: MaxLogLevel) asInteger!

logLevel: anInteger 
	"Set the maximum log level (Default: 4)."

	^horde3DLibrary setOption: MaxLogLevel value: anInteger!

sampleCount
	"Answer the maximum number of samples used for multisampled render targets."

	^(horde3DLibrary getOption: SampleCount) asInteger!

sampleCount: anInteger 
	"Set the maximum number of samples used for multisampled render targets; only affects
	pipelines that are loaded after setting the option (Values: 0, 2, 4, 8, 16; Default: 0)."

	^horde3DLibrary setOption: SampleCount value: anInteger!

shadowMapSize
	"Answer the size of the shadow map buffer."

	^(horde3DLibrary getOption: ShadowMapSize) asInteger!

shadowMapSize: anInteger 
	"Set the size of the shadow map buffer (Values: 128, 256, 512, 1024, 2048; Default: 1024)."

	^horde3DLibrary setOption: ShadowMapSize value: anInteger!

textureCompression
	"Answer the texture compression option parameter value."

	^(horde3DLibrary getOption: TexCompression) asInteger asBoolean!

textureCompression: aBoolean 
	"Enable or disable texture compression; only affects textures that are loaded after setting
	the option (Default: false)."

	^horde3DLibrary setOption: TexCompression value: aBoolean asParameter!

trilinearFiltering
	"Answer the trilinear filtering option parameter value."

	^(horde3DLibrary getOption: TrilinearFiltering) asInteger asBoolean!

trilinearFiltering: aBoolean 
	"Enable or disable trilinear filtering for textures; only affects textures that are loaded after
	setting the option. (Default: true)."

	^horde3DLibrary setOption: TrilinearFiltering value: aBoolean asParameter!

wireframeMode
	"Answer the wireframe rendering option parameter value."

	^(horde3DLibrary getOption: WireframeMode) asInteger asBoolean!

wireframeMode: aBoolean 
	"Enable or disable wireframe rendering."

	^horde3DLibrary setOption: WireframeMode value: aBoolean asParameter! !
!Horde3DOptions categoriesFor: #anisotropyFactor!public! !
!Horde3DOptions categoriesFor: #anisotropyFactor:!public! !
!Horde3DOptions categoriesFor: #debugViewMode!public! !
!Horde3DOptions categoriesFor: #debugViewMode:!public! !
!Horde3DOptions categoriesFor: #fastAnimation!public! !
!Horde3DOptions categoriesFor: #fastAnimation:!public! !
!Horde3DOptions categoriesFor: #initialize!initializing!private! !
!Horde3DOptions categoriesFor: #loadTextures!public! !
!Horde3DOptions categoriesFor: #loadTextures:!public! !
!Horde3DOptions categoriesFor: #logLevel!public! !
!Horde3DOptions categoriesFor: #logLevel:!public! !
!Horde3DOptions categoriesFor: #sampleCount!public! !
!Horde3DOptions categoriesFor: #sampleCount:!public! !
!Horde3DOptions categoriesFor: #shadowMapSize!public! !
!Horde3DOptions categoriesFor: #shadowMapSize:!public! !
!Horde3DOptions categoriesFor: #textureCompression!public! !
!Horde3DOptions categoriesFor: #textureCompression:!public! !
!Horde3DOptions categoriesFor: #trilinearFiltering!public! !
!Horde3DOptions categoriesFor: #trilinearFiltering:!public! !
!Horde3DOptions categoriesFor: #wireframeMode!public! !
!Horde3DOptions categoriesFor: #wireframeMode:!public! !

!Horde3DOptions class methodsFor!

new
	"Answer a new initialized instance of the receiver."

	^super new initialize! !
!Horde3DOptions class categoriesFor: #new!instance creation!public! !

Horde3DResourceManager guid: (GUID fromString: '{D02680ED-26F8-4553-B5C1-C14027F33CCC}')!
Horde3DResourceManager comment: ''!
!Horde3DResourceManager categoriesForClass!Kernel-Objects! !
!Horde3DResourceManager methodsFor!

addAnimationFromFile: aFilename 
	"Add an animation resource to the resource manager from the file named aFilename."

	^self addAnimationFromFile: aFilename flags: 0!

addAnimationFromFile: aFilename flags: anInteger 
	"Add an animation resource to the resource manager from the file named aFilename with
	resource creation flags anInteger."

	^self 
		addResourceFromFile: aFilename
		resourceType: Animation
		flags: anInteger!

addCodeFromFile: aFilename 
	"Add a code resource to the resource manager from the file named aFilename."

	^self addCodeFromFile: aFilename flags: 0!

addCodeFromFile: aFilename flags: anInteger 
	"Add a code resource to the resource manager from the file named aFilename with
	resource creation flags anInteger."

	^self 
		addResourceFromFile: aFilename
		resourceType: Code
		flags: anInteger!

addEffectFromFile: aFilename 
	"Add a particle configuration resource to the resource manager from the file named aFilename."

	^self addEffectFromFile: aFilename flags: 0!

addEffectFromFile: aFilename flags: anInteger 
	"Add a particle configuration resource to the resource manager from the file named aFilename
	with resource creation flags anInteger."

	^self 
		addResourceFromFile: aFilename
		resourceType: Effect
		flags: anInteger!

addFontMaterialFromFile: aFilename 
	"Add a font material resource to the resource manager from the file named aFilename."

	^self addFontMaterialFromFile: aFilename flags: 0!

addFontMaterialFromFile: aFilename flags: anInteger 
	"Add a font material resource to the resource manager from the file named aFilename with
	resource creation flags anInteger."

	^self 
		addResourceFromFile: aFilename
		resourceType: FontMaterial
		flags: anInteger!

addGeometryFromFile: aFilename 
	"Add a geometry resource to the resource manager from the file named aFilename."

	^self addGeometryFromFile: aFilename flags: 0!

addGeometryFromFile: aFilename flags: anInteger 
	"Add a geometry resource to the resource manager from the file named aFilename with
	resource creation flags anInteger."

	^self 
		addResourceFromFile: aFilename
		resourceType: Geometry
		flags: anInteger!

addMaterialFromFile: aFilename 
	"Add a material resource to the resource manager from the file named aFilename."

	^self addMaterialFromFile: aFilename flags: 0!

addMaterialFromFile: aFilename flags: anInteger 
	"Add a material resource to the resource manager from the file named aFilename with
	resource creation flags anInteger."

	^self 
		addResourceFromFile: aFilename
		resourceType: Material
		flags: anInteger!

addPipelineFromFile: aFilename 
	"Add a rendering pipeline resource to the resource manager from the file named aFilename."

	^self addPipelineFromFile: aFilename flags: 0!

addPipelineFromFile: aFilename flags: anInteger 
	"Add a rendering pipeline resource to the resource manager from the file named aFilename
	with resource creation flags anInteger."

	^self 
		addResourceFromFile: aFilename
		resourceType: Pipeline
		flags: anInteger!

addResourceFromFile: aFilename resourceType: aResourceType flags: anInteger 
	"Private - Add a resource of type aResourceType to the resource manager from the file
	named aFilename with resource creation flags anInteger."

	| resource |
	resource := ((self resourceClassFor: aResourceType) fromFile: aFilename flags: anInteger) 
				resourceManager: self.
	resource value isZero 
		ifTrue: 
			[Horde3DNotification signal: 'Can''t add resource ''' , aFilename , ''' to the resource manager'].
	^resource!

addSceneGraphFromFile: aFilename 
	"Add a scene graph resource to the resource manager from the file named aFilename."

	^self addSceneGraphFromFile: aFilename flags: 0!

addSceneGraphFromFile: aFilename flags: anInteger 
	"Add a scene graph resource to the resource manager from the file named aFilename with
	resource creation flags anInteger."

	^self 
		addResourceFromFile: aFilename
		resourceType: SceneGraph
		flags: anInteger!

addShaderFromFile: aFilename 
	"Add a shader resource to the resource manager from the file named aFilename."

	^self addShaderFromFile: aFilename flags: 0!

addShaderFromFile: aFilename flags: anInteger 
	"Add a shader resource to the resource manager from the file named aFilename with
	resource creation flags anInteger."

	^self 
		addResourceFromFile: aFilename
		resourceType: Shader
		flags: anInteger!

addTexture2DFromFile: aFilename 
	"Add a two-dimensional texture map resource to the resource manager from the file named
	aFilename."

	^self addTexture2DFromFile: aFilename flags: 0!

addTexture2DFromFile: aFilename flags: anInteger 
	"Add a two-dimensional texture map resource to the resource manager from the file named
	aFilename with resource creation flags anInteger."

	^self 
		addResourceFromFile: aFilename
		resourceType: Texture2D
		flags: anInteger!

addTextureCubeFromFile: aFilename 
	"Add a cube map texture resource to the resource manager from the file named aFilename."

	^self addTextureCubeFromFile: aFilename flags: 0!

addTextureCubeFromFile: aFilename flags: anInteger 
	"Add a cube map texture resource to the resource manager from the file named aFilename
	with resource creation flags anInteger."

	^self 
		addResourceFromFile: aFilename
		resourceType: TextureCube
		flags: anInteger!

animationPath
	"Get the search path for animation resources."

	^horde3DUtilsLibrary getResourcePath: Animation!

animationPath: aString 
	"Set the search path for animation resources."

	horde3DUtilsLibrary setResourcePath: Animation path: aString!

animationResourceNamed: aString 
	"Answer the receiver's animation resource named aString."

	^self resourceNamed: aString ofType: Animation!

codePath
	"Get the search path for code resources."

	^horde3DUtilsLibrary getResourcePath: Code!

codePath: aString 
	"Set the search path for code resources."

	horde3DUtilsLibrary setResourcePath: Code path: aString!

codeResourceNamed: aString 
	"Answer the receiver's code resource named aString."

	^self resourceNamed: aString ofType: Code!

effectPath
	"Get the search path for particle configuration resources."

	^horde3DUtilsLibrary getResourcePath: Effect!

effectPath: aString 
	"Set the search path for particle configuration resources."

	horde3DUtilsLibrary setResourcePath: Effect path: aString!

effectResourceNamed: aString 
	"Answer the receiver's effect resource named aString."

	^self resourceNamed: aString ofType: Effect!

fontMaterialResourceNamed: aString 
	"Answer the receiver's font material resource named aString."

	^self resourceNamed: aString ofType: FontMaterial!

geometryPath
	"Get the search path for geometry resources."

	^horde3DUtilsLibrary getResourcePath: Geometry!

geometryPath: aString 
	"Set the search path for geometry resources."

	horde3DUtilsLibrary setResourcePath: Geometry path: aString!

geometryResourceNamed: aString 
	"Answer the receiver's geometry resource named aString."

	^self resourceNamed: aString ofType: Geometry!

initialize
	"Private - Initialize the receiver."

	horde3DLibrary := Horde3DLibrary default.
	horde3DUtilsLibrary := Horde3DUtilsLibrary default!

loadResourcesFrom: aDirectoryString 
	"Load previously added and still unloaded resources from the directories specified in
	aDirectoryString."

	(horde3DUtilsLibrary loadResourcesFromDisk: aDirectoryString) 
		ifFalse: [Horde3DNotification signal: 'At least one resource could not be loaded']!

materialPath
	"Get the search path for material resources."

	^horde3DUtilsLibrary getResourcePath: Material!

materialPath: aString 
	"Set the search path for material resources."

	horde3DUtilsLibrary setResourcePath: Material path: aString!

materialResourceNamed: aString 
	"Answer the receiver's material resource named aString."

	^self resourceNamed: aString ofType: Material!

pipelinePath
	"Get the search path for pipeline resources."

	^horde3DUtilsLibrary getResourcePath: Pipeline!

pipelinePath: aString 
	"Set the search path for pipeline resources."

	horde3DUtilsLibrary setResourcePath: Pipeline path: aString!

pipelineResourceNamed: aString 
	"Answer the receiver's pipeline resource named aString."

	^self resourceNamed: aString ofType: Pipeline!

resourceClassFor: aResourceType 
	"Private - Answer the resource class for the resource type aResourceType."

	aResourceType = SceneGraph ifTrue: [^Horde3DSceneGraphResource].
	aResourceType = Geometry ifTrue: [^Horde3DGeometryResource].
	aResourceType = Animation ifTrue: [^Horde3DAnimationResource].
	aResourceType = Material ifTrue: [^Horde3DMaterialResource].
	aResourceType = Code ifTrue: [^Horde3DCodeResource].
	aResourceType = Shader ifTrue: [^Horde3DShaderResource].
	aResourceType = Texture2D ifTrue: [^Horde3DTexture2DResource].
	aResourceType = TextureCube ifTrue: [^Horde3DTextureCubeResource].
	aResourceType = Effect ifTrue: [^Horde3DEffectResource].
	aResourceType = Pipeline ifTrue: [^Horde3DPipelineResource].
	aResourceType = FontMaterial ifTrue: [^Horde3DFontMaterialResource]!

resourceNamed: aString ofType: aResourceType 
	"Private - Answer the receiver's resource named aString of type aResourceType."

	| resourceHandle resourceClass |
	resourceHandle := horde3DLibrary findResource: aResourceType name: aString.
	resourceClass := self resourceClassFor: aResourceType.
	resourceHandle isZero 
		ifTrue: 
			[Horde3DNotification signal: 'Can''t find a ' , resourceClass printString , ' named ''' , aString 
						, ''' in the resource manager'].
	^(resourceClass new)
		value: resourceHandle;
		resourceManager: self!

resourceTypeFor: aHorde3DResource 
	"Answer the type of the resource aHorde3DResource."

	^aHorde3DResource class resourceType
	"^Horde3DLibrary default getResourceType: aHorde3DResource value"!

sceneGraphPath
	"Get the search path for scene graph resources."

	^horde3DUtilsLibrary getResourcePath: SceneGraph!

sceneGraphPath: aString 
	"Set the search path for scene graph resources."

	horde3DUtilsLibrary setResourcePath: SceneGraph path: aString!

sceneGraphResourceNamed: aString 
	"Answer the receiver's scene graph resource named aString."

	^self resourceNamed: aString ofType: SceneGraph!

shaderPath
	"Get the search path for shader resources."

	^horde3DUtilsLibrary getResourcePath: Shader!

shaderPath: aString 
	"Set the search path for shader resources."

	horde3DUtilsLibrary setResourcePath: Shader path: aString!

shaderResourceNamed: aString 
	"Answer the receiver's shader resource named aString."

	^self resourceNamed: aString ofType: Shader!

texture2DPath
	"Get the search path for two-dimensional texture map resources."

	^horde3DUtilsLibrary getResourcePath: Texture2D!

texture2DPath: aString 
	"Set the search path for two-dimensional texture map resources."

	horde3DUtilsLibrary setResourcePath: Texture2D path: aString!

texture2DResourceNamed: aString 
	"Answer the receiver's texture 2D resource named aString."

	^self resourceNamed: aString ofType: Texture2D!

textureCubePath
	"Get the search path for cube map texture resources."

	^horde3DUtilsLibrary getResourcePath: TextureCube!

textureCubePath: aString 
	"Set the search path for cube map texture resources."

	horde3DUtilsLibrary setResourcePath: TextureCube path: aString!

textureCubeResourceNamed: aString 
	"Answer the receiver's texture cube resource named aString."

	^self resourceNamed: aString ofType: TextureCube! !
!Horde3DResourceManager categoriesFor: #addAnimationFromFile:!public! !
!Horde3DResourceManager categoriesFor: #addAnimationFromFile:flags:!public! !
!Horde3DResourceManager categoriesFor: #addCodeFromFile:!public! !
!Horde3DResourceManager categoriesFor: #addCodeFromFile:flags:!public! !
!Horde3DResourceManager categoriesFor: #addEffectFromFile:!public! !
!Horde3DResourceManager categoriesFor: #addEffectFromFile:flags:!public! !
!Horde3DResourceManager categoriesFor: #addFontMaterialFromFile:!public! !
!Horde3DResourceManager categoriesFor: #addFontMaterialFromFile:flags:!public! !
!Horde3DResourceManager categoriesFor: #addGeometryFromFile:!public! !
!Horde3DResourceManager categoriesFor: #addGeometryFromFile:flags:!public! !
!Horde3DResourceManager categoriesFor: #addMaterialFromFile:!public! !
!Horde3DResourceManager categoriesFor: #addMaterialFromFile:flags:!public! !
!Horde3DResourceManager categoriesFor: #addPipelineFromFile:!public! !
!Horde3DResourceManager categoriesFor: #addPipelineFromFile:flags:!public! !
!Horde3DResourceManager categoriesFor: #addResourceFromFile:resourceType:flags:!private! !
!Horde3DResourceManager categoriesFor: #addSceneGraphFromFile:!public! !
!Horde3DResourceManager categoriesFor: #addSceneGraphFromFile:flags:!public! !
!Horde3DResourceManager categoriesFor: #addShaderFromFile:!public! !
!Horde3DResourceManager categoriesFor: #addShaderFromFile:flags:!public! !
!Horde3DResourceManager categoriesFor: #addTexture2DFromFile:!public! !
!Horde3DResourceManager categoriesFor: #addTexture2DFromFile:flags:!public! !
!Horde3DResourceManager categoriesFor: #addTextureCubeFromFile:!public! !
!Horde3DResourceManager categoriesFor: #addTextureCubeFromFile:flags:!public! !
!Horde3DResourceManager categoriesFor: #animationPath!public! !
!Horde3DResourceManager categoriesFor: #animationPath:!public! !
!Horde3DResourceManager categoriesFor: #animationResourceNamed:!public! !
!Horde3DResourceManager categoriesFor: #codePath!public! !
!Horde3DResourceManager categoriesFor: #codePath:!public! !
!Horde3DResourceManager categoriesFor: #codeResourceNamed:!public! !
!Horde3DResourceManager categoriesFor: #effectPath!public! !
!Horde3DResourceManager categoriesFor: #effectPath:!public! !
!Horde3DResourceManager categoriesFor: #effectResourceNamed:!public! !
!Horde3DResourceManager categoriesFor: #fontMaterialResourceNamed:!public! !
!Horde3DResourceManager categoriesFor: #geometryPath!public! !
!Horde3DResourceManager categoriesFor: #geometryPath:!public! !
!Horde3DResourceManager categoriesFor: #geometryResourceNamed:!public! !
!Horde3DResourceManager categoriesFor: #initialize!initializing!private! !
!Horde3DResourceManager categoriesFor: #loadResourcesFrom:!public! !
!Horde3DResourceManager categoriesFor: #materialPath!public! !
!Horde3DResourceManager categoriesFor: #materialPath:!public! !
!Horde3DResourceManager categoriesFor: #materialResourceNamed:!public! !
!Horde3DResourceManager categoriesFor: #pipelinePath!public! !
!Horde3DResourceManager categoriesFor: #pipelinePath:!public! !
!Horde3DResourceManager categoriesFor: #pipelineResourceNamed:!public! !
!Horde3DResourceManager categoriesFor: #resourceClassFor:!private! !
!Horde3DResourceManager categoriesFor: #resourceNamed:ofType:!private! !
!Horde3DResourceManager categoriesFor: #resourceTypeFor:!public! !
!Horde3DResourceManager categoriesFor: #sceneGraphPath!public! !
!Horde3DResourceManager categoriesFor: #sceneGraphPath:!public! !
!Horde3DResourceManager categoriesFor: #sceneGraphResourceNamed:!public! !
!Horde3DResourceManager categoriesFor: #shaderPath!public! !
!Horde3DResourceManager categoriesFor: #shaderPath:!public! !
!Horde3DResourceManager categoriesFor: #shaderResourceNamed:!public! !
!Horde3DResourceManager categoriesFor: #texture2DPath!public! !
!Horde3DResourceManager categoriesFor: #texture2DPath:!public! !
!Horde3DResourceManager categoriesFor: #texture2DResourceNamed:!public! !
!Horde3DResourceManager categoriesFor: #textureCubePath!public! !
!Horde3DResourceManager categoriesFor: #textureCubePath:!public! !
!Horde3DResourceManager categoriesFor: #textureCubeResourceNamed:!public! !

!Horde3DResourceManager class methodsFor!

new
	"Answer a new initialized instance of the receiver."

	^super new initialize! !
!Horde3DResourceManager class categoriesFor: #new!instance creation!public! !

Horde3DScene guid: (GUID fromString: '{31B30F77-184A-411E-9462-FFA19E44D5F2}')!
Horde3DScene comment: ''!
!Horde3DScene categoriesForClass!Kernel-Objects! !
!Horde3DScene methodsFor!

addCamera: aCameraName at: aParentNode withPipeline: aHorde3DPipelineResource 
	"Add a camera node named aCameraName to the scene, attached to the parent node
	aParentNode, with the pipeline resource aHorde3DPipelineResource used for rendering."

	| camera |
	camera := self nodeFromHandle: (horde3DLibrary 
						addCameraNode: aParentNode value
						name: aCameraName
						pipelineRes: aHorde3DPipelineResource value).
	self trigger: #cameraCreated: with: camera.
	^camera!

addCamera: aCameraName withPipeline: aHorde3DPipelineResource 
	"Add a camera node named aCameraName to the scene, attached to the RootNode, with the
	pipeline resource aHorde3DPipelineResource used for rendering."

	^self 
		addCamera: aCameraName
		at: RootNode
		withPipeline: aHorde3DPipelineResource!

addLight: aLightName at: aParentNode usingLightingShaderContext: aLightingContextString usingShadowShaderContext: aShadowContextString 
	"Add a light node named aLightName to the scene, attached to the parent node aParentNode,
	using the shader context names for rendering shadow maps and doing light calculations
	aShadowContextString and aLightingContextString respectively."

	^self 
		addLight: aLightName
		at: aParentNode
		withMaterial: 0
		usingLightingShaderContext: aLightingContextString
		usingShadowShaderContext: aShadowContextString!

addLight: aLightName at: aParentNode withMaterial: aHorde3DMaterialResource usingLightingShaderContext: aLightingContextString usingShadowShaderContext: aShadowContextString 
	"Add a light node named aLightName to the scene, attached to the parent node aParentNode,
	with the material resource aHorde3DMaterialResource, using the shader context names for
	rendering shadow maps and doing light calculations aShadowContextString and
	aLightingContextString respectively."

	^self nodeFromHandle: (horde3DLibrary 
				addLightNode: aParentNode value
				name: aLightName
				materialRes: aHorde3DMaterialResource value
				lightingContext: aLightingContextString
				shadowContext: aShadowContextString)!

addLight: aLightName usingLightingShaderContext: aLightingContextString usingShadowShaderContext: aShadowContextString 
	"Add a light node named aLightName to the scene, attached to the RootNode, using the shader
	context names for rendering shadow maps and doing light calculations aShadowContextString
	and aLightingContextString respectively."

	^self 
		addLight: aLightName
		at: RootNode
		usingLightingShaderContext: aLightingContextString
		usingShadowShaderContext: aShadowContextString!

addLight: aLightName withMaterial: aHorde3DMaterialResource usingLightingShaderContext: aLightingContextString usingShadowShaderContext: aShadowContextString 
	"Add a light node named aLightName to the scene, attached to the RootNode, with the material
	resource aHorde3DMaterialResource, using the shader context names for rendering shadow
	maps and doing light calculations aShadowContextString and aLightingContextString respectively."

	^self 
		addLight: aLightName
		at: RootNode
		withMaterial: aHorde3DMaterialResource
		usingLightingShaderContext: aLightingContextString
		usingShadowShaderContext: aShadowContextString!

addNodesFrom: aHorde3DSceneGraphResource 
	"Add new nodes from the scene graph resouce aHorde3DSceneGraphResource to the scene,
	attached to the RootNode."

	^self addNodesFrom: aHorde3DSceneGraphResource at: RootNode!

addNodesFrom: aHorde3DSceneGraphResource at: aParentNode 
	"Add new nodes from the scene graph resouce aHorde3DSceneGraphResource to the scene,
	attached to the parent node aParentNode."

	| nodeHandle |
	nodeHandle := horde3DLibrary addNodes: aParentNode value
				sceneGraphRes: aHorde3DSceneGraphResource value.
	^self nodeFromHandle: nodeHandle!

cameraNodes
	"Answer an array with all the camera scene nodes."

	^self cameraNodesAt: RootNode!

cameraNodesAt: aHorde3DNode 
	"Answer an array of camera scene nodes starting the search at aHorde3DNode."

	^self cameraNodesNamed: '' at: aHorde3DNode!

cameraNodesNamed: aString 
	"Answer an array of camera scene nodes with name aString."

	^self cameraNodesNamed: aString at: RootNode!

cameraNodesNamed: aString at: aHorde3DNode 
	"Answer an array of camera scene nodes with name aString, starting the search at aHorde3DNode."

	^self 
		nodesNamed: aString
		at: aHorde3DNode
		ofType: Camera!

emitterNodes
	"Answer an array with all the emitter scene nodes."

	^self emitterNodesAt: RootNode!

emitterNodesAt: aHorde3DNode 
	"Answer an array of emitter scene nodes starting the search at aHorde3DNode."

	^self emitterNodesNamed: '' at: aHorde3DNode!

emitterNodesNamed: aString 
	"Answer an array of emitter scene nodes with name aString."

	^self emitterNodesNamed: aString at: RootNode!

emitterNodesNamed: aString at: aHorde3DNode 
	"Answer an array of emitter scene nodes with name aString, starting the search at aHorde3DNode."

	^self 
		nodesNamed: aString
		at: aHorde3DNode
		ofType: Emitter!

groupNodes
	"Answer an array with all the group scene nodes."

	^self groupNodesAt: RootNode!

groupNodesAt: aHorde3DNode 
	"Answer an array of group scene nodes starting the search at aHorde3DNode."

	^self groupNodesNamed: '' at: aHorde3DNode!

groupNodesNamed: aString 
	"Answer an array of group scene nodes with name aString."

	^self groupNodesNamed: aString at: RootNode!

groupNodesNamed: aString at: aHorde3DNode 
	"Answer an array of group scene nodes with name aString, starting the search at aHorde3DNode."

	^self 
		nodesNamed: aString
		at: aHorde3DNode
		ofType: Group!

initialize
	"Private - Initialize the receiver."

	horde3DLibrary := Horde3DLibrary default!

jointNodes
	"Answer an array with all the joint scene nodes."

	^self jointNodesAt: RootNode!

jointNodesAt: aHorde3DNode 
	"Answer an array of joint scene nodes starting the search at aHorde3DNode."

	^self jointNodesNamed: '' at: aHorde3DNode!

jointNodesNamed: aString 
	"Answer an array of joint scene nodes with name aString."

	^self jointNodesNamed: aString at: RootNode!

jointNodesNamed: aString at: aHorde3DNode 
	"Answer an array of joint scene nodes with name aString, starting the search at aHorde3DNode."

	^self 
		nodesNamed: aString
		at: aHorde3DNode
		ofType: Joint!

lightNodes
	"Answer an array with all the light scene nodes."

	^self lightNodesAt: RootNode!

lightNodesAt: aHorde3DNode 
	"Answer an array of light scene nodes starting the search at aHorde3DNode."

	^self lightNodesNamed: '' at: aHorde3DNode!

lightNodesNamed: aString 
	"Answer an array of light scene nodes with name aString."

	^self lightNodesNamed: aString at: RootNode!

lightNodesNamed: aString at: aHorde3DNode 
	"Answer an array of light scene nodes with name aString, starting the search at aHorde3DNode."

	^self 
		nodesNamed: aString
		at: aHorde3DNode
		ofType: Light!

meshNodes
	"Answer an array with all the mesh scene nodes."

	^self meshNodesAt: RootNode!

meshNodesAt: aHorde3DNode 
	"Answer an array of mesh scene nodes starting the search at aHorde3DNode."

	^self meshNodesNamed: '' at: aHorde3DNode!

meshNodesNamed: aString 
	"Answer an array of mesh scene nodes with name aString."

	^self meshNodesNamed: aString at: RootNode!

meshNodesNamed: aString at: aHorde3DNode 
	"Answer an array of mesh scene nodes with name aString, starting the search at aHorde3DNode."

	^self 
		nodesNamed: aString
		at: aHorde3DNode
		ofType: Mesh!

modelNodes
	"Answer an array with all the model scene nodes."

	^self modelNodesAt: RootNode!

modelNodesAt: aHorde3DNode 
	"Answer an array of model scene nodes starting the search at aHorde3DNode."

	^self modelNodesNamed: '' at: aHorde3DNode!

modelNodesNamed: aString 
	"Answer an array of model scene nodes with name aString."

	^self modelNodesNamed: aString at: RootNode!

modelNodesNamed: aString at: aHorde3DNode 
	"Answer an array of model scene nodes with name aString, starting the search at aHorde3DNode."

	^self 
		nodesNamed: aString
		at: aHorde3DNode
		ofType: Model!

nodeClassFor: aNodeType 
	"Private - Answer the node class for the node type aNodeType."

	aNodeType = Group ifTrue: [^Horde3DGroupNode].
	aNodeType = Model ifTrue: [^Horde3DModelNode].
	aNodeType = Mesh ifTrue: [^Horde3DMeshNode].
	aNodeType = Joint ifTrue: [^Horde3DJointNode].
	aNodeType = Light ifTrue: [^Horde3DLightNode].
	aNodeType = Camera ifTrue: [^Horde3DCameraNode].
	aNodeType = Emitter ifTrue: [^Horde3DEmitterNode]!

nodeFromHandle: aNodeHandle 
	"Private - Answer a Horde3DNode subclass instance from the node handle aNodeHandle."

	| nodeType |
	nodeType := self nodeTypeFor: aNodeHandle.
	^((self nodeClassFor: nodeType) new)
		value: aNodeHandle;
		scene: self!

nodes
	"Answer an array with all the scene nodes."

	^self nodesNamed: ''!

nodesNamed: aString 
	"Answer an array of scene nodes with name aString."

	^self nodesNamed: aString at: RootNode!

nodesNamed: aString at: aHorde3DNode 
	"Answer an array of scene nodes with name aString, starting the search at aHorde3DNode."

	^self 
		nodesNamed: aString
		at: aHorde3DNode
		ofType: Undefined!

nodesNamed: aString at: aHorde3DNode ofType: aNodeType 
	"Private - Answer an array of scene nodes with name aString, of type aNodeType, starting the
	search at aHorde3DNode."

	| nodes nodeCount |
	nodeCount := horde3DLibrary 
				findNodes: aHorde3DNode value
				name: aString
				type: aNodeType.
	nodes := Array new: nodeCount.
	1 to: nodeCount
		do: 
			[:index | 
			| nodeHandle |
			nodeHandle := horde3DLibrary getNodeFindResult: index - 1.
			nodes at: index put: (self nodeFromHandle: nodeHandle)].
	^nodes!

nodeTypeFor: aHorde3DNode 
	"Answer the type of the scene node aHorde3DNode."

	^horde3DLibrary getNodeType: aHorde3DNode value!

rootNode
	"Answer the scene root node handle."

	^RootNode! !
!Horde3DScene categoriesFor: #addCamera:at:withPipeline:!public! !
!Horde3DScene categoriesFor: #addCamera:withPipeline:!public! !
!Horde3DScene categoriesFor: #addLight:at:usingLightingShaderContext:usingShadowShaderContext:!public! !
!Horde3DScene categoriesFor: #addLight:at:withMaterial:usingLightingShaderContext:usingShadowShaderContext:!public! !
!Horde3DScene categoriesFor: #addLight:usingLightingShaderContext:usingShadowShaderContext:!public! !
!Horde3DScene categoriesFor: #addLight:withMaterial:usingLightingShaderContext:usingShadowShaderContext:!public! !
!Horde3DScene categoriesFor: #addNodesFrom:!public! !
!Horde3DScene categoriesFor: #addNodesFrom:at:!public! !
!Horde3DScene categoriesFor: #cameraNodes!public! !
!Horde3DScene categoriesFor: #cameraNodesAt:!public! !
!Horde3DScene categoriesFor: #cameraNodesNamed:!public! !
!Horde3DScene categoriesFor: #cameraNodesNamed:at:!public! !
!Horde3DScene categoriesFor: #emitterNodes!public! !
!Horde3DScene categoriesFor: #emitterNodesAt:!public! !
!Horde3DScene categoriesFor: #emitterNodesNamed:!public! !
!Horde3DScene categoriesFor: #emitterNodesNamed:at:!public! !
!Horde3DScene categoriesFor: #groupNodes!public! !
!Horde3DScene categoriesFor: #groupNodesAt:!public! !
!Horde3DScene categoriesFor: #groupNodesNamed:!public! !
!Horde3DScene categoriesFor: #groupNodesNamed:at:!public! !
!Horde3DScene categoriesFor: #initialize!initializing!private! !
!Horde3DScene categoriesFor: #jointNodes!public! !
!Horde3DScene categoriesFor: #jointNodesAt:!public! !
!Horde3DScene categoriesFor: #jointNodesNamed:!public! !
!Horde3DScene categoriesFor: #jointNodesNamed:at:!public! !
!Horde3DScene categoriesFor: #lightNodes!public! !
!Horde3DScene categoriesFor: #lightNodesAt:!public! !
!Horde3DScene categoriesFor: #lightNodesNamed:!public! !
!Horde3DScene categoriesFor: #lightNodesNamed:at:!public! !
!Horde3DScene categoriesFor: #meshNodes!public! !
!Horde3DScene categoriesFor: #meshNodesAt:!public! !
!Horde3DScene categoriesFor: #meshNodesNamed:!public! !
!Horde3DScene categoriesFor: #meshNodesNamed:at:!public! !
!Horde3DScene categoriesFor: #modelNodes!public! !
!Horde3DScene categoriesFor: #modelNodesAt:!public! !
!Horde3DScene categoriesFor: #modelNodesNamed:!public! !
!Horde3DScene categoriesFor: #modelNodesNamed:at:!public! !
!Horde3DScene categoriesFor: #nodeClassFor:!private! !
!Horde3DScene categoriesFor: #nodeFromHandle:!private! !
!Horde3DScene categoriesFor: #nodes!public! !
!Horde3DScene categoriesFor: #nodesNamed:!public! !
!Horde3DScene categoriesFor: #nodesNamed:at:!public! !
!Horde3DScene categoriesFor: #nodesNamed:at:ofType:!private! !
!Horde3DScene categoriesFor: #nodeTypeFor:!public! !
!Horde3DScene categoriesFor: #rootNode!public! !

!Horde3DScene class methodsFor!

icon
	"Answer the receiver's icon."

	^Icon fromId: 'Canvas.ico'!

new
	"Answer a new initialized instance of the receiver."

	^super new initialize! !
!Horde3DScene class categoriesFor: #icon!constants!public! !
!Horde3DScene class categoriesFor: #new!instance creation!public! !

Horde3DError guid: (GUID fromString: '{FA768A5D-CA7F-491F-8796-E243F67130D9}')!
Horde3DError comment: ''!
!Horde3DError categoriesForClass!Kernel-Exception Handling! !
Horde3DNotification guid: (GUID fromString: '{15F2B8D0-2757-457B-A53B-D5EE5E1536B1}')!
Horde3DNotification comment: ''!
!Horde3DNotification categoriesForClass!Kernel-Exception Handling! !
Horde3DLibrary guid: (GUID fromString: '{F83BC555-F192-4A5A-A1FD-9758887BECF0}')!
Horde3DLibrary comment: ''!
!Horde3DLibrary categoriesForClass!External-Libraries! !
!Horde3DLibrary methodsFor!

addCameraNode: parent name: name pipelineRes: pipelineRes 
	"Adds a Camera node to the scene.

		NodeHandle addCameraNode(
			NodeHandle parent,
			const char *name,
			ResHandle pipelineRes
		);"

	<cdecl: sdword addCameraNode sdword char* sdword>
	^self invalidCall!

addLightNode: parent name: name materialRes: materialRes lightingContext: lightingContext shadowContext: shadowContext 
	"Adds a Light node to the scene.

		NodeHandle addLightNode(
			NodeHandle parent,
			const char *name,
			ResHandle materialRes,
			const char *lightingContext,
			const char *shadowContext
		);"

	<cdecl: sdword addLightNode sdword char* sdword char* char*>
	^self invalidCall!

addNodes: parent sceneGraphRes: sceneGraphRes 
	"Adds nodes from a SceneGraph resource to the scene.

		NodeHandle addNodes(
			NodeHandle parent,
			ResHandle sceneGraphRes
		);"

	<cdecl: sdword addNodes sdword sdword>
	^self invalidCall!

addResource: type name: name flags: flags 
	"Adds a resource.

		ResHandle addResource(
			ResourceTypes::List type,
			const char *name,
			int flags
		);"

	<cdecl: sdword addResource sdword char* sdword>
	^self invalidCall!

advanceEmitterTime: emitterNode timeDelta: timeDelta 
	"Advances the time value of an Emitter node.

		bool advanceEmitterTime(
			NodeHandle emitterNode,
			float timeDelta
		);"

	<cdecl: bool advanceEmitterTime sdword float>
	^self invalidCall!

clearOverlays
	"Removes all overlays.

		void clearOverlays();"

	<cdecl: void clearOverlays>
	^self invalidCall!

findNodes: startNode name: name type: type 
	"Finds scene nodes with the specified properties.

		int findNodes(
			NodeHandle startNode,
			const char *name,
			int type
		);"

	<cdecl: sdword findNodes sdword char* sdword>
	^self invalidCall!

findResource: type name: name 
	"Finds a resource and returns its handle.

		ResHandle findResource(
			ResourceTypes::List type,
			const char *name
		);"

	<cdecl: sdword findResource sdword char*>
	^self invalidCall!

getNodeFindResult: index 
	"Gets a result from the findNodes query.

		NodeHandle getNodeFindResult(
			int index
		);"

	<cdecl: sdword getNodeFindResult sdword>
	^self invalidCall!

getNodeParamf: node param: param 
	"Gets a property of a scene node.

		float getNodeParamf(
			NodeHandle node,
			int param
		);"

	<cdecl: float getNodeParamf sdword sdword>
	^self invalidCall!

getNodeParami: node param: param 
	"Gets a property of a scene node.

		int getNodeParami(
			NodeHandle node,
			int param
		);"

	<cdecl: sdword getNodeParami sdword sdword>
	^self invalidCall!

getNodeParamstr: node param: param 
	"Gets a property of a scene node.

		const char *getNodeParamstr(
			NodeHandle node,
			int param
		);"

	<cdecl: char* getNodeParamstr sdword sdword>
	^self invalidCall!

getNodeType: node 
	"Returns the type of a scene node.

		int getNodeType(
			NodeHandle node
		);"

	<cdecl: sdword getNodeType sdword>
	^self invalidCall!

getOption: param 
	"Gets an option parameter of the engine.

		float getOption(
			EngineOptions::List param
		);"

	<cdecl: float getOption sdword>
	^self invalidCall!

getResourceType: res 
	"Returns the type of a resource.

		ResourceTypes::List getResourceType(
			ResHandle res
		);"

	<cdecl: sdword getResourceType sdword>
	^self invalidCall!

init
	"Initializes the engine.

		bool init();"

	<cdecl: bool init>
	^self invalidCall!

release
	"Releases the engine.

		void release();"

	<cdecl: bool release>
	^self invalidCall!

render: cameraNode 
	"Main rendering function.

		bool render(
			NodeHandle cameraNode
		);"

	<cdecl: bool render sdword>
	^self invalidCall!

resize: x y: y width: width height: height 
	"Resizes the viewport.

		void resize(
			int x,
			int y,
			int width,
			int height
		);"

	<cdecl: void resize sdword sdword sdword sdword>
	^self invalidCall!

setMaterialUniform: materialRes name: name a: a b: b c: c d: d 
	"Sets a shader uniform of a Material resource.

		bool setMaterialUniform(
			ResHandle materialRes,
			const char *name,
			float a,
			float b,
			float c,
			float d
		);"

	<cdecl: bool setMaterialUniform sdword char* float float float float>
	^self invalidCall!

setModelAnimParams: modelNode stage: stage time: time weight: weight 
	"Sets the parameters of an animation stage in a Model node.

		bool setModelAnimParams(
			NodeHandle modelNode,
			int stage,
			float time,
			float weight
		);"

	<cdecl: bool setModelAnimParams sdword sdword float float>
	^self invalidCall!

setNodeParamf: node param: param value: value 
	"Sets a property of a scene node.

		bool setNodeParamf(
			NodeHandle node,
			int param,
			float value
		);"

	<cdecl: bool setNodeParamf sdword sdword float>
	^self invalidCall!

setNodeParami: node param: param value: value 
	"Sets a property of a scene node.

		bool setNodeParamf(
			NodeHandle node,
			int param,
			int value
		);"

	<cdecl: bool setNodeParami sdword sdword sdword>
	^self invalidCall!

setNodeParamstr: node param: param value: value 
	"Sets a property of a scene node.

		bool setNodeParamstr(
			NodeHandle node,
			int param,
			const char *value
		);"

	<cdecl: bool setNodeParamstr sdword sdword char*>
	^self invalidCall!

setNodeTransform: node tx: tx ty: ty tz: tz rx: rx ry: ry rz: rz sx: sx sy: sy sz: sz 
	"Sets the relative transformation of a node.

		bool setNodeTransform(
			NodeHandle node,
			float 	tx,
			float 	ty,
			float 	tz,
			float 	rx,
			float 	ry,
			float 	rz,
			float 	sx,
			float 	sy,
			float 	sz
		);"

	<cdecl: bool setNodeTransform sdword float float float float float float float float float>
	^self invalidCall!

setOption: param value: value 
	"Sets an option parameter for the engine.

		bool setOption(
			EngineOptions::List param,
			float value
		);"

	<cdecl: bool setOption sdword float>
	^self invalidCall!

setupCameraView: cameraNode fov: fov aspect: aspect nearDist: nearDist farDist: farDist 
	"Sets the planes of a camera viewing frustum.

		bool setupCameraView(
			NodeHandle cameraNode,
			float fov,
			float aspect,
			float nearDist,
			float farDist
		);"

	<cdecl: bool setupCameraView sdword float float float float>
	^self invalidCall!

setupModelAnimStage: modelNode stage: stage animationRes: animationRes startNode: startNode additive: additive 
	"Configures an animation stage of a Model node.

		bool setupModelAnimStage(
			NodeHandle modelNode,
			int stage,
			ResHandle animationRes,
			const char *startNode,
			bool additive
		);"

	<cdecl: bool setupModelAnimStage sdword sdword sdword char* bool>
	^self invalidCall!

showOverlay: xll yll: yll ull: ull vll: vll xlr: xlr ylr: ylr ulr: ulr vlr: vlr xur: xur yur: yur uur: uur vur: vur xul: xul yul: yul uul: uul vul: vul layer: layer materialRes: materialRes 
	"Shows an overlay on the screen.

		void showOverlay(
			float x_ll,
			float y_ll,
			float u_ll,
			float v_ll,
			float x_lr,
			float y_lr,
			float u_lr,
			float v_lr,
			float x_ur,
			float y_ur,
			float u_ur,
			float v_ur,
			float x_ul,
			float y_ul,
			float u_ul,
			float v_ul,
			int layer,
			ResHandle materialRes
		);"

	<cdecl: void showOverlay float float float float float float float float float float float float float float float float sdword sdword>
	^self invalidCall! !
!Horde3DLibrary categoriesFor: #addCameraNode:name:pipelineRes:!public! !
!Horde3DLibrary categoriesFor: #addLightNode:name:materialRes:lightingContext:shadowContext:!public! !
!Horde3DLibrary categoriesFor: #addNodes:sceneGraphRes:!public! !
!Horde3DLibrary categoriesFor: #addResource:name:flags:!public! !
!Horde3DLibrary categoriesFor: #advanceEmitterTime:timeDelta:!public! !
!Horde3DLibrary categoriesFor: #clearOverlays!public! !
!Horde3DLibrary categoriesFor: #findNodes:name:type:!public! !
!Horde3DLibrary categoriesFor: #findResource:name:!public! !
!Horde3DLibrary categoriesFor: #getNodeFindResult:!public! !
!Horde3DLibrary categoriesFor: #getNodeParamf:param:!public! !
!Horde3DLibrary categoriesFor: #getNodeParami:param:!public! !
!Horde3DLibrary categoriesFor: #getNodeParamstr:param:!public! !
!Horde3DLibrary categoriesFor: #getNodeType:!public! !
!Horde3DLibrary categoriesFor: #getOption:!public! !
!Horde3DLibrary categoriesFor: #getResourceType:!public! !
!Horde3DLibrary categoriesFor: #init!public! !
!Horde3DLibrary categoriesFor: #release!public! !
!Horde3DLibrary categoriesFor: #render:!public! !
!Horde3DLibrary categoriesFor: #resize:y:width:height:!public! !
!Horde3DLibrary categoriesFor: #setMaterialUniform:name:a:b:c:d:!public! !
!Horde3DLibrary categoriesFor: #setModelAnimParams:stage:time:weight:!public! !
!Horde3DLibrary categoriesFor: #setNodeParamf:param:value:!public! !
!Horde3DLibrary categoriesFor: #setNodeParami:param:value:!public! !
!Horde3DLibrary categoriesFor: #setNodeParamstr:param:value:!public! !
!Horde3DLibrary categoriesFor: #setNodeTransform:tx:ty:tz:rx:ry:rz:sx:sy:sz:!public! !
!Horde3DLibrary categoriesFor: #setOption:value:!public! !
!Horde3DLibrary categoriesFor: #setupCameraView:fov:aspect:nearDist:farDist:!public! !
!Horde3DLibrary categoriesFor: #setupModelAnimStage:stage:animationRes:startNode:additive:!public! !
!Horde3DLibrary categoriesFor: #showOverlay:yll:ull:vll:xlr:ylr:ulr:vlr:xur:yur:uur:vur:xul:yul:uul:vul:layer:materialRes:!public! !

!Horde3DLibrary class methodsFor!

fileName
	^'Horde3D.dll'! !
!Horde3DLibrary class categoriesFor: #fileName!public! !

Horde3DUtilsLibrary guid: (GUID fromString: '{F53C3C25-8EFA-4612-B4BD-ED3BE7E85100}')!
Horde3DUtilsLibrary comment: ''!
!Horde3DUtilsLibrary categoriesForClass!External-Libraries! !
!Horde3DUtilsLibrary methodsFor!

dumpMessages
	"Writes all messages in the queue to a log file.

		bool dumpMessages();"

	<cdecl: bool dumpMessages>
	^self invalidCall!

getResourcePath: type 
	"Returns the search path of a resource type.

		const char *getResourcePath(
			int type
		);"

	<cdecl: char* getResourcePath sdword>
	^self invalidCall!

initOpenGL: hDC 
	"Initializes OpenGL.

		bool initOpenGL(
			int hDC
		);"

	<cdecl: bool initOpenGL sdword>
	^self invalidCall!

loadResourcesFromDisk: contentDir 
	"Loads previously added resources from a data drive.

		bool loadResourcesFromDisk(
			const char *contentDir
		);"

	<cdecl: bool loadResourcesFromDisk char*>
	^self invalidCall!

releaseOpenGL
	"Releases OpenGL.

		void releaseOpenGL();"

	<cdecl: void releaseOpenGL>
	^self invalidCall!

setResourcePath: type path: path 
	"Sets the search path for a resource type.

		void setResourcePath(
			int type,
			const char *path
		);"

	<cdecl: void setResourcePath sdword char*>
	^self invalidCall!

showFrameStats: fontMaterialRes curFPS: curFPS 
	"Shows frame statistics on the screen.

		void showFrameStats(
			ResHandle fontMaterialRes,
			float curFPS
		);"

	<cdecl: void showFrameStats sdword float>
	^self invalidCall!

showText: text x: x y: y size: size layer: layer fontMaterialRes: fontMaterialRes 
	"Shows text on the screen using a font texture.

		void showText(
			const char *text,
			float x,
			float y,
			float size,
			int layer,
			ResHandle fontMaterialRes
		);"

	<cdecl: void showText char* float float float sdword sdword>
	^self invalidCall!

swapBuffers
	"Displays the rendered image on the screen.

		void swapBuffers();"

	<cdecl: void swapBuffers>
	^self invalidCall! !
!Horde3DUtilsLibrary categoriesFor: #dumpMessages!public! !
!Horde3DUtilsLibrary categoriesFor: #getResourcePath:!public! !
!Horde3DUtilsLibrary categoriesFor: #initOpenGL:!public! !
!Horde3DUtilsLibrary categoriesFor: #loadResourcesFromDisk:!public! !
!Horde3DUtilsLibrary categoriesFor: #releaseOpenGL!public! !
!Horde3DUtilsLibrary categoriesFor: #setResourcePath:path:!public! !
!Horde3DUtilsLibrary categoriesFor: #showFrameStats:curFPS:!public! !
!Horde3DUtilsLibrary categoriesFor: #showText:x:y:size:layer:fontMaterialRes:!public! !
!Horde3DUtilsLibrary categoriesFor: #swapBuffers!public! !

!Horde3DUtilsLibrary class methodsFor!

fileName
	^'Horde3DUtils.dll'! !
!Horde3DUtilsLibrary class categoriesFor: #fileName!public! !

Horde3DNode guid: (GUID fromString: '{C65B27FD-695D-47E2-B537-C3C6FD1005F4}')!
Horde3DNode comment: ''!
!Horde3DNode categoriesForClass!External-Data-Structured! !
!Horde3DNode methodsFor!

initialize
	"Private - Initialize the receiver."

	horde3DLibrary := Horde3DLibrary default!

name
	"Answer the receiver's name."

	^horde3DLibrary getNodeParamstr: self value param: Name!

name: aString 
	"Set the receiver's name to aString."

	^horde3DLibrary 
		setNodeParamstr: self value
		param: Name
		value: aString!

scene
	"Answer the Horde3DScene object this node belongs to."

	^scene!

scene: aHorde3DScene 
	"Private - Set the receiver's scene instance variable to the Horde3DScene object this node
	belongs to."

	scene := aHorde3DScene!

translation: aTranslationPoint3D rotation: aRotationPoint3D scale: aScalePoint3D 
	"Set the translation, rotation (in Euler angles) and scale of the receiver.

	NOTE1: it would be cool if I can split this method in three: #translation:, #rotation: and #scale:
	but with the current API I'm afraid this is not possible, please see my request at:
	http://www.horde3d.org/forums/viewtopic.php?f=3&t=511

	NOTE2: see if we've to pass the translation and rotation values negated for intuitiveness."

	^horde3DLibrary 
		setNodeTransform: self value
		tx: aTranslationPoint3D x
		ty: aTranslationPoint3D y
		tz: aTranslationPoint3D z
		rx: aRotationPoint3D x
		ry: aRotationPoint3D y
		rz: aRotationPoint3D z
		sx: aScalePoint3D x
		sy: aScalePoint3D y
		sz: aScalePoint3D z! !
!Horde3DNode categoriesFor: #initialize!initializing!private! !
!Horde3DNode categoriesFor: #name!public! !
!Horde3DNode categoriesFor: #name:!public! !
!Horde3DNode categoriesFor: #scene!accessing!public! !
!Horde3DNode categoriesFor: #scene:!accessing!private! !
!Horde3DNode categoriesFor: #translation:rotation:scale:!public! !

Horde3DResource guid: (GUID fromString: '{3C6CC397-A851-490D-B79F-49326735A294}')!
Horde3DResource comment: ''!
!Horde3DResource categoriesForClass!Kernel-Objects! !
!Horde3DResource methodsFor!

initialize
	"Private - Initialize the receiver."

	horde3DLibrary := Horde3DLibrary default!

resourceManager
	"Answer the Horde3DResourceManager object this resource belongs to."

	^resourceManager!

resourceManager: aHorde3DResourceManager 
	"Private - Set the receiver's resourceManager instance variable to the Horde3DResourceManager
	object this resource belongs to."

	resourceManager := aHorde3DResourceManager! !
!Horde3DResource categoriesFor: #initialize!initializing!private! !
!Horde3DResource categoriesFor: #resourceManager!accessing!public! !
!Horde3DResource categoriesFor: #resourceManager:!accessing!private! !

!Horde3DResource class methodsFor!

fromFile: aFilename 
	"Answer a new instance of the receiver from the file named aFilename."

	^self fromFile: aFilename flags: 0!

fromFile: aFilename flags: anInteger 
	"Answer a new instance of the receiver from the file named aFilename with creation flags anInteger."

	^self new value: (Horde3DLibrary default 
				addResource: self resourceType
				name: aFilename
				flags: anInteger)!

icon
	"Answer the receiver's icon."

	^Icon fromId: 'Resource.ico'!

resourceType
	"Private - Answer the appropriate resource type for the class."

	^self subclassResponsibility! !
!Horde3DResource class categoriesFor: #fromFile:!instance creation!public! !
!Horde3DResource class categoriesFor: #fromFile:flags:!instance creation!public! !
!Horde3DResource class categoriesFor: #icon!constants!public! !
!Horde3DResource class categoriesFor: #resourceType!private! !

Horde3DCameraNode guid: (GUID fromString: '{EB255F49-917B-4265-B2A8-46388651685D}')!
Horde3DCameraNode comment: ''!
!Horde3DCameraNode categoriesForClass!External-Data-Structured! !
!Horde3DCameraNode methodsFor!

bottomPlaneCoordinate
	"Answer the receiver's coordinate of bottom plane relative to near plane center."

	^horde3DLibrary getNodeParamf: self value param: BottomPlane!

bottomPlaneCoordinate: aFloat 
	"Set the receiver's coordinate of bottom plane relative to near plane center (Default: -0.041421354)."

	^horde3DLibrary 
		setNodeParamf: self value
		param: BottomPlane
		value: aFloat!

farPlaneDistance
	"Answer the receiver's distance of the far clipping plane."

	^horde3DLibrary getNodeParamf: self value param: FarPlane!

farPlaneDistance: aFloat 
	"Set the receiver's distance of the far clipping plane to aFloat (Default: 1000)."

	^horde3DLibrary 
		setNodeParamf: self value
		param: FarPlane
		value: aFloat!

fov: aFOVFloat aspectRatio: anExtentPoint 
	"Set the receiver's field of view angle to aFOVFloat and the aspect ratio to anExtentPoint."

	^self 
		fov: aFOVFloat
		aspectRatio: anExtentPoint
		nearPlaneDistance: self nearPlaneDistance
		farPlaneDistance: self farPlaneDistance!

fov: aFOVFloat aspectRatio: anExtentPoint nearPlaneDistance: aNearPlaneDistanceFloat farPlaneDistance: aFarPlaneDistanceFloat 
	"Set the receiver's field of view angle to aFOVFloat, the aspect ratio to anExtentPoint,
	the distance of the near clipping plane to aNearPlaneDistanceFloat and the distance of the
	far clipping plane to aFarPlaneDistanceFloat."

	^horde3DLibrary 
		setupCameraView: self value
		fov: aFOVFloat
		aspect: (anExtentPoint x / anExtentPoint y) asFloat
		nearDist: aNearPlaneDistanceFloat
		farDist: aFarPlaneDistanceFloat!

leftPlaneCoordinate
	"Answer the receiver's coordinate of left plane relative to near plane center."

	^horde3DLibrary getNodeParamf: self value param: LeftPlane!

leftPlaneCoordinate: aFloat 
	"Set the receiver's coordinate of left plane relative to near plane center (Default: -0.055228457)."

	^horde3DLibrary 
		setNodeParamf: self value
		param: LeftPlane
		value: aFloat!

nearPlaneDistance
	"Answer the receiver's distance of the near clipping plane."

	^horde3DLibrary getNodeParamf: self value param: NearPlane!

nearPlaneDistance: aFloat 
	"Set the receiver's distance of the near clipping plane to aFloat (Default: 0.1)."

	^horde3DLibrary 
		setNodeParamf: self value
		param: NearPlane
		value: aFloat!

occlusionCulling
	"Answer whether occlusion culling is enabled or disabled."

	^(horde3DLibrary getNodeParami: self value param: OcclusionCulling) asBoolean!

occlusionCulling: aBoolean 
	"Enable or disable occlusion culling (Default: false)."

	^horde3DLibrary 
		setNodeParami: self value
		param: OcclusionCulling
		value: aBoolean asParameter!

orthographic
	"Answer whether the receiver is using an orthographic frustum or not."

	^(horde3DLibrary getNodeParami: self value param: Orthographic) asBoolean!

orthographic: aBoolean 
	"Enable or disable an orthographic frustum instead of a perspective one (Default: false)."

	^horde3DLibrary 
		setNodeParami: self value
		param: Orthographic
		value: aBoolean asParameter!

outputBufferIndex
	"Answer the receiver's index of the output buffer for stereo rendering."

	^horde3DLibrary getNodeParami: self value param: OutputBufferIndex!

outputBufferIndex: anInteger 
	"Set the receiver's index of the output buffer for stereo rendering (Values: 0 for left eye, 1 for
	right eye; Default: 0)."

	^horde3DLibrary 
		setNodeParami: self value
		param: OutputBufferIndex
		value: anInteger!

outputTexture2D
	"Answer the receiver's Texture2D resource used as output buffer."

	^(Horde3DTexture2DResource new)
		value: (horde3DLibrary getNodeParami: self value param: OutputTex);
		scene: self scene!

outputTexture2D: aHorde3DTexture2DResource 
	"Set the receiver's Texture2D resource used as output buffer to aHorde3DTexture2DResource
	(Default: 0 to use main framebuffer)."

	^horde3DLibrary 
		setNodeParami: self value
		param: OutputTex
		value: aHorde3DTexture2DResource value!

pipelineResource
	"Answer the receiver's pipeline resource used for rendering."

	^(Horde3DPipelineResource new)
		value: (horde3DLibrary getNodeParami: self value param: PipelineRes);
		scene: self scene!

pipelineResource: aHorde3DPipelineResource 
	"Set the receiver's pipeline resource used for rendering to aHorde3DPipelineResource."

	^horde3DLibrary 
		setNodeParami: self value
		param: PipelineRes
		value: aHorde3DPipelineResource value!

render
	"Render the scene from the receiver's point of view."

	^horde3DLibrary render: self value!

rightPlaneCoordinate
	"Answer the receiver's coordinate of right plane relative to near plane center."

	^horde3DLibrary getNodeParamf: self value param: RightPlane!

rightPlaneCoordinate: aFloat 
	"Set the receiver's coordinate of right plane relative to near plane center (Default: 0.055228457)."

	^horde3DLibrary 
		setNodeParamf: self value
		param: RightPlane
		value: aFloat!

topPlaneCoordinate
	"Set the receiver's coordinate of top plane relative to near plane center (Default: 0.041421354)."

	^horde3DLibrary getNodeParamf: self value param: TopPlane!

topPlaneCoordinate: aFloat 
	"Set the receiver's coordinate of top plane relative to near plane center (Default: 0.041421354)."

	^horde3DLibrary 
		setNodeParamf: self value
		param: TopPlane
		value: aFloat! !
!Horde3DCameraNode categoriesFor: #bottomPlaneCoordinate!public! !
!Horde3DCameraNode categoriesFor: #bottomPlaneCoordinate:!public! !
!Horde3DCameraNode categoriesFor: #farPlaneDistance!public! !
!Horde3DCameraNode categoriesFor: #farPlaneDistance:!public! !
!Horde3DCameraNode categoriesFor: #fov:aspectRatio:!public! !
!Horde3DCameraNode categoriesFor: #fov:aspectRatio:nearPlaneDistance:farPlaneDistance:!public! !
!Horde3DCameraNode categoriesFor: #leftPlaneCoordinate!public! !
!Horde3DCameraNode categoriesFor: #leftPlaneCoordinate:!public! !
!Horde3DCameraNode categoriesFor: #nearPlaneDistance!public! !
!Horde3DCameraNode categoriesFor: #nearPlaneDistance:!public! !
!Horde3DCameraNode categoriesFor: #occlusionCulling!public! !
!Horde3DCameraNode categoriesFor: #occlusionCulling:!public! !
!Horde3DCameraNode categoriesFor: #orthographic!public! !
!Horde3DCameraNode categoriesFor: #orthographic:!public! !
!Horde3DCameraNode categoriesFor: #outputBufferIndex!public! !
!Horde3DCameraNode categoriesFor: #outputBufferIndex:!public! !
!Horde3DCameraNode categoriesFor: #outputTexture2D!public! !
!Horde3DCameraNode categoriesFor: #outputTexture2D:!public! !
!Horde3DCameraNode categoriesFor: #pipelineResource!public! !
!Horde3DCameraNode categoriesFor: #pipelineResource:!public! !
!Horde3DCameraNode categoriesFor: #render!public! !
!Horde3DCameraNode categoriesFor: #rightPlaneCoordinate!public! !
!Horde3DCameraNode categoriesFor: #rightPlaneCoordinate:!public! !
!Horde3DCameraNode categoriesFor: #topPlaneCoordinate!public! !
!Horde3DCameraNode categoriesFor: #topPlaneCoordinate:!public! !

!Horde3DCameraNode class methodsFor!

icon
	"Answer the receiver's icon."

	^Icon fromId: 'Snapshot.ico'!

publishedEventsOfInstances
	"Answer a Set of Symbols that describe the published events triggered by instances of the
	receiver."

	^(super publishedEventsOfInstances)
		add: #cameraCreated:;
		yourself! !
!Horde3DCameraNode class categoriesFor: #icon!constants!public! !
!Horde3DCameraNode class categoriesFor: #publishedEventsOfInstances!development!events!public! !

Horde3DEmitterNode guid: (GUID fromString: '{E610B185-0A3E-4C49-814B-60D69A01AEAA}')!
Horde3DEmitterNode comment: ''!
!Horde3DEmitterNode categoriesForClass!External-Data-Structured! !
!Horde3DEmitterNode methodsFor!

advanceTime: timeDeltaFloat 
	"Advance the receiver's simulation time by timeDeltaFloat seconds."

	^horde3DLibrary advanceEmitterTime: self value timeDelta: timeDeltaFloat! !
!Horde3DEmitterNode categoriesFor: #advanceTime:!public! !

Horde3DGroupNode guid: (GUID fromString: '{A2A498D0-3162-40FA-A378-00284CBF64A8}')!
Horde3DGroupNode comment: ''!
!Horde3DGroupNode categoriesForClass!External-Data-Structured! !
Horde3DJointNode guid: (GUID fromString: '{FCC4F7B9-ADC1-4321-9F40-89E880612DBE}')!
Horde3DJointNode comment: ''!
!Horde3DJointNode categoriesForClass!External-Data-Structured! !
Horde3DLightNode guid: (GUID fromString: '{350F8412-51D5-4F3B-ADF5-166C000B6FD0}')!
Horde3DLightNode comment: ''!
!Horde3DLightNode categoriesForClass!External-Data-Structured! !
!Horde3DLightNode methodsFor!

blue
	"Answer the receiver's blue component of light diffuse color."

	^((horde3DLibrary getNodeParamf: self value param: Col_B) * 255) asInteger!

blue: anInteger 
	"Set the receiver's blue component of light diffuse color (Default: 255)."

	^horde3DLibrary 
		setNodeParamf: self value
		param: Col_B
		value: ((anInteger / 255.0 min: 1.0) max: 0.0)!

color
	"Answer the receiver's color."

	^RGB 
		red: self red
		green: self green
		blue: self blue!

color: aRGB 
	"Set the receiver's color to aRGB."

	| redStatus greenStatus blueStatus |
	redStatus := self red: aRGB red.
	greenStatus := self green: aRGB green.
	blueStatus := self blue: aRGB blue.
	^redStatus & greenStatus & blueStatus!

fov
	"Answer the receiver's field of view angle."

	^horde3DLibrary getNodeParamf: self value param: FOV!

fov: aFloat 
	"Set the receiver's field of view angle (Default: 90.0)."

	^horde3DLibrary 
		setNodeParamf: self value
		param: FOV
		value: aFloat!

green
	"Answer the receiver's green component of light diffuse color."

	^((horde3DLibrary getNodeParamf: self value param: Col_G) * 255) asInteger!

green: anInteger 
	"Set the receiver's green component of light diffuse color (Default: 255)."

	^horde3DLibrary 
		setNodeParamf: self value
		param: Col_G
		value: ((anInteger / 255.0 min: 1.0) max: 0.0)!

materialResource
	"Answer the receiver's material resource used for the light."

	^(Horde3DMaterialResource new)
		value: (horde3DLibrary getNodeParami: self value param: MaterialRes);
		scene: self scene!

materialResource: aHorde3DMaterialResource 
	"Set the receiver's material resource used for the light."

	^horde3DLibrary 
		setNodeParami: self value
		param: MaterialRes
		value: aHorde3DMaterialResource value!

radius
	"Answer the receiver's radius of influence."

	^horde3DLibrary getNodeParamf: self value param: Radius!

radius: aFloat 
	"Set the receiver's radius of influence (Default: 100.0)."

	^horde3DLibrary 
		setNodeParamf: self value
		param: Radius
		value: aFloat!

red
	"Answer the receiver's red component of light diffuse color."

	^((horde3DLibrary getNodeParamf: self value param: Col_R) * 255) asInteger!

red: anInteger 
	"Set the receiver's red component of light diffuse color (Default: 255)."

	^horde3DLibrary 
		setNodeParamf: self value
		param: Col_R
		value: ((anInteger / 255.0 min: 1.0) max: 0.0)!

shadowMapBias
	"Answer the bias value for shadow mapping to reduce shadow acne."

	^horde3DLibrary getNodeParamf: self value param: ShadowMapBias!

shadowMapBias: aFloat 
	"Set the bias value for shadow mapping to reduce shadow acne (Default: 0.005)."

	^horde3DLibrary 
		setNodeParamf: self value
		param: ShadowMapBias
		value: aFloat!

shadowMapCount
	"Answer the number of shadow maps used for light source."

	^horde3DLibrary getNodeParami: self value param: ShadowMapCount!

shadowMapCount: anInteger 
	"Set the number of shadow maps used for light source (Values: 0, 1, 2, 3, 4; Default: 0)."

	^horde3DLibrary 
		setNodeParami: self value
		param: ShadowMapCount
		value: anInteger!

shadowSplitLambda
	"Answer the constant determining segmentation of view frustum for Parallel Split Shadow Maps."

	^horde3DLibrary getNodeParamf: self value param: ShadowSplitLambda!

shadowSplitLambda: aFloat 
	"Set the constant determining segmentation of view frustum for Parallel Split Shadow Maps
	(Default: 0.5)."

	^horde3DLibrary 
		setNodeParamf: self value
		param: ShadowSplitLambda
		value: aFloat! !
!Horde3DLightNode categoriesFor: #blue!public! !
!Horde3DLightNode categoriesFor: #blue:!public! !
!Horde3DLightNode categoriesFor: #color!public! !
!Horde3DLightNode categoriesFor: #color:!public! !
!Horde3DLightNode categoriesFor: #fov!public! !
!Horde3DLightNode categoriesFor: #fov:!public! !
!Horde3DLightNode categoriesFor: #green!public! !
!Horde3DLightNode categoriesFor: #green:!public! !
!Horde3DLightNode categoriesFor: #materialResource!public! !
!Horde3DLightNode categoriesFor: #materialResource:!public! !
!Horde3DLightNode categoriesFor: #radius!public! !
!Horde3DLightNode categoriesFor: #radius:!public! !
!Horde3DLightNode categoriesFor: #red!public! !
!Horde3DLightNode categoriesFor: #red:!public! !
!Horde3DLightNode categoriesFor: #shadowMapBias!public! !
!Horde3DLightNode categoriesFor: #shadowMapBias:!public! !
!Horde3DLightNode categoriesFor: #shadowMapCount!public! !
!Horde3DLightNode categoriesFor: #shadowMapCount:!public! !
!Horde3DLightNode categoriesFor: #shadowSplitLambda!public! !
!Horde3DLightNode categoriesFor: #shadowSplitLambda:!public! !

!Horde3DLightNode class methodsFor!

icon
	"Answer the receiver's icon."

	^Icon fromId: 'IdeaSpaceShell.ico'! !
!Horde3DLightNode class categoriesFor: #icon!constants!public! !

Horde3DMeshNode guid: (GUID fromString: '{60C95703-5DAC-48E8-9A57-E84A2CBD841D}')!
Horde3DMeshNode comment: ''!
!Horde3DMeshNode categoriesForClass!External-Data-Structured! !
Horde3DModelNode guid: (GUID fromString: '{24B310DE-B563-488E-930C-E64A169C96C3}')!
Horde3DModelNode comment: ''!
!Horde3DModelNode categoriesForClass!External-Data-Structured! !
!Horde3DModelNode methodsFor!

animationTime: aFloat1 weight: aFloat2 forStage: anInteger 
	"Set the receiver's animation time and weight for the specified stage."

	^horde3DLibrary 
		setModelAnimParams: self value
		stage: anInteger - 1
		time: aFloat1
		weight: aFloat2!

setupAnimationStage: anInteger withResource: aHorde3DAnimationResource 
	"Configure an animation stage for the receiver."

	^self 
		setupAnimationStage: anInteger
		withResource: aHorde3DAnimationResource
		at: ''!

setupAnimationStage: anInteger withResource: aHorde3DAnimationResource at: nodeName 
	"Configure an animation stage for the receiver."

	^self 
		setupAnimationStage: anInteger
		withResource: aHorde3DAnimationResource
		at: nodeName
		isAdditive: false!

setupAnimationStage: anInteger withResource: aHorde3DAnimationResource at: nodeName isAdditive: aBoolean 
	"Configure an animation stage for the receiver.

	TODO: try to find a better selector name..."

	^horde3DLibrary 
		setupModelAnimStage: self value
		stage: anInteger - 1
		animationRes: aHorde3DAnimationResource value
		startNode: nodeName
		additive: aBoolean! !
!Horde3DModelNode categoriesFor: #animationTime:weight:forStage:!public! !
!Horde3DModelNode categoriesFor: #setupAnimationStage:withResource:!public! !
!Horde3DModelNode categoriesFor: #setupAnimationStage:withResource:at:!public! !
!Horde3DModelNode categoriesFor: #setupAnimationStage:withResource:at:isAdditive:!public! !

Horde3DAnimationResource guid: (GUID fromString: '{F89E9189-60F3-41FA-B77C-DC7F55F40982}')!
Horde3DAnimationResource comment: ''!
!Horde3DAnimationResource categoriesForClass!Kernel-Objects! !
!Horde3DAnimationResource class methodsFor!

resourceType
	"Private - Answer the appropriate resource type for the class."

	^Animation! !
!Horde3DAnimationResource class categoriesFor: #resourceType!private! !

Horde3DCodeResource guid: (GUID fromString: '{087875F3-5DBD-427C-90CA-BC441DE21014}')!
Horde3DCodeResource comment: ''!
!Horde3DCodeResource categoriesForClass!Kernel-Objects! !
!Horde3DCodeResource class methodsFor!

resourceType
	"Private - Answer the appropriate resource type for the class."

	^Code! !
!Horde3DCodeResource class categoriesFor: #resourceType!private! !

Horde3DEffectResource guid: (GUID fromString: '{7212C1E7-7D71-4DF6-8EFE-0DBDF578B13E}')!
Horde3DEffectResource comment: ''!
!Horde3DEffectResource categoriesForClass!Kernel-Objects! !
!Horde3DEffectResource class methodsFor!

resourceType
	"Private - Answer the appropriate resource type for the class."

	^Effect! !
!Horde3DEffectResource class categoriesFor: #resourceType!private! !

Horde3DGeometryResource guid: (GUID fromString: '{F95BCECF-10CA-4E4D-8A51-51D63DA38C08}')!
Horde3DGeometryResource comment: ''!
!Horde3DGeometryResource categoriesForClass!Kernel-Objects! !
!Horde3DGeometryResource class methodsFor!

resourceType
	"Private - Answer the appropriate resource type for the class."

	^Geometry! !
!Horde3DGeometryResource class categoriesFor: #resourceType!private! !

Horde3DMaterialResource guid: (GUID fromString: '{1F69F551-2E1A-4203-BA38-B9E47E2D0EBB}')!
Horde3DMaterialResource comment: ''!
!Horde3DMaterialResource categoriesForClass!Kernel-Objects! !
!Horde3DMaterialResource methodsFor!

onShowOverlay
	"Private - Inform the receiver's resourceManager that we're showing overlays."

	resourceManager trigger: #showOverlay!

setUniform: aString values: aFLOATArray 
	"Set the receiver's shader uniform named aString to the four float values contained in aFLOATArray."

	^horde3DLibrary 
		setMaterialUniform: self value
		name: aString
		a: (aFLOATArray at: 1)
		b: (aFLOATArray at: 2)
		c: (aFLOATArray at: 3)
		d: (aFLOATArray at: 4)!

showOverlayAt: aPositionRectange usingLayer: anInteger 
	"Show an overlay with the receiver's material on the screen, with dimensions and position as
	specified by aPositionRectange, using the layer index anInteger.
	Note that the coordinate system used has its origin at 0 @ 0 and its corner at 1 @ 1."

	self 
		showOverlayAt: aPositionRectange
		withTextureRectangle: (Rectangle origin: 0 @ 0 corner: 1 @ 1)
		usingLayer: anInteger!

showOverlayAt: aPositionRectange withTextureRectangle: aTextureRectangle usingLayer: anInteger 
	"Show an overlay with the receiver's material on the screen, with dimensions and position as
	specified by aPositionRectange, with texture coordinates as specified by aTextureRectangle,
	using the layer index anInteger.
	Note that the coordinate system used has its origin at 0 @ 0 and its corner at 1 @ 1."

	horde3DLibrary 
		showOverlay: aPositionRectange left
		yll: 1 - aPositionRectange bottom
		ull: aTextureRectangle left
		vll: 1 - aTextureRectangle bottom
		xlr: aPositionRectange right
		ylr: 1 - aPositionRectange bottom
		ulr: aTextureRectangle right
		vlr: 1 - aTextureRectangle bottom
		xur: aPositionRectange right
		yur: 1 - aPositionRectange top
		uur: aTextureRectangle right
		vur: 1 - aTextureRectangle top
		xul: aPositionRectange left
		yul: 1 - aPositionRectange top
		uul: aTextureRectangle left
		vul: 1 - aTextureRectangle top
		layer: anInteger - 1
		materialRes: self value.
	self onShowOverlay! !
!Horde3DMaterialResource categoriesFor: #onShowOverlay!event handling!private! !
!Horde3DMaterialResource categoriesFor: #setUniform:values:!public! !
!Horde3DMaterialResource categoriesFor: #showOverlayAt:usingLayer:!public! !
!Horde3DMaterialResource categoriesFor: #showOverlayAt:withTextureRectangle:usingLayer:!public! !

!Horde3DMaterialResource class methodsFor!

publishedEventsOfInstances
	"Answer a Set of Symbols that describe the published events triggered by instances of the
	receiver."

	^(super publishedEventsOfInstances)
		add: #showOverlay;
		yourself!

resourceType
	"Private - Answer the appropriate resource type for the class."

	^Material! !
!Horde3DMaterialResource class categoriesFor: #publishedEventsOfInstances!development!events!public! !
!Horde3DMaterialResource class categoriesFor: #resourceType!private! !

Horde3DPipelineResource guid: (GUID fromString: '{09D2F1D8-33B7-4E36-B7B1-D43691A9C9BD}')!
Horde3DPipelineResource comment: ''!
!Horde3DPipelineResource categoriesForClass!Kernel-Objects! !
!Horde3DPipelineResource class methodsFor!

resourceType
	"Private - Answer the appropriate resource type for the class."

	^Pipeline! !
!Horde3DPipelineResource class categoriesFor: #resourceType!private! !

Horde3DSceneGraphResource guid: (GUID fromString: '{6806A6AF-8866-4DC7-B094-75E1AA1CF425}')!
Horde3DSceneGraphResource comment: ''!
!Horde3DSceneGraphResource categoriesForClass!Kernel-Objects! !
!Horde3DSceneGraphResource class methodsFor!

resourceType
	"Private - Answer the appropriate resource type for the class."

	^SceneGraph! !
!Horde3DSceneGraphResource class categoriesFor: #resourceType!private! !

Horde3DShaderResource guid: (GUID fromString: '{F933A827-7388-4304-A3F8-F7108E944DC1}')!
Horde3DShaderResource comment: ''!
!Horde3DShaderResource categoriesForClass!Kernel-Objects! !
!Horde3DShaderResource class methodsFor!

resourceType
	"Private - Answer the appropriate resource type for the class."

	^Shader! !
!Horde3DShaderResource class categoriesFor: #resourceType!private! !

Horde3DTexture2DResource guid: (GUID fromString: '{E67B5159-C106-4F61-803A-05261B904DA6}')!
Horde3DTexture2DResource comment: ''!
!Horde3DTexture2DResource categoriesForClass!Kernel-Objects! !
!Horde3DTexture2DResource class methodsFor!

resourceType
	"Private - Answer the appropriate resource type for the class."

	^Texture2D! !
!Horde3DTexture2DResource class categoriesFor: #resourceType!private! !

Horde3DTextureCubeResource guid: (GUID fromString: '{8097E6DD-F70F-45E7-95AB-DA9A4F40035B}')!
Horde3DTextureCubeResource comment: ''!
!Horde3DTextureCubeResource categoriesForClass!Kernel-Objects! !
!Horde3DTextureCubeResource class methodsFor!

resourceType
	"Private - Answer the appropriate resource type for the class."

	^TextureCube! !
!Horde3DTextureCubeResource class categoriesFor: #resourceType!private! !

Horde3DFontMaterialResource guid: (GUID fromString: '{29CACEC9-54AF-4CB6-AE94-14DF276F6495}')!
Horde3DFontMaterialResource comment: ''!
!Horde3DFontMaterialResource categoriesForClass!Kernel-Objects! !
!Horde3DFontMaterialResource methodsFor!

initialize
	"Private - Initialize the receiver."

	super initialize.
	horde3DUtilsLibrary := Horde3DUtilsLibrary default!

show: aString at: aPoint usingLayer: anInteger 
	"Show the text aString at aPoint position on the screen, using the layer index anInteger."

	self 
		show: aString
		at: aPoint
		withScale: 1
		usingLayer: anInteger!

show: aString at: aPoint withScale: aFloat usingLayer: anInteger 
	"Show the text aString at aPoint position on the screen, with aFloat size (scale) factor of the font,
	using the layer index anInteger.
	Note that the coordinate system used has its origin at 0 @ 0 and its corner at 1 @ 1."

	horde3DUtilsLibrary 
		showText: aString
		x: aPoint x
		y: 1 - aPoint y
		size: aFloat
		layer: anInteger - 1
		fontMaterialRes: self value.
	self onShowOverlay!

showFrameStats: currentFPS 
	"Show statistics for the current frame in the upper left corner of the screen.
	You've to tell this method the frames per second with which the application is currently running
	as specified by the currentFPS parameter."

	horde3DUtilsLibrary showFrameStats: self value curFPS: currentFPS.
	self onShowOverlay! !
!Horde3DFontMaterialResource categoriesFor: #initialize!initializing!private! !
!Horde3DFontMaterialResource categoriesFor: #show:at:usingLayer:!public! !
!Horde3DFontMaterialResource categoriesFor: #show:at:withScale:usingLayer:!public! !
!Horde3DFontMaterialResource categoriesFor: #showFrameStats:!public! !

!Horde3DFontMaterialResource class methodsFor!

fromFile: aFilename flags: anInteger 
	"Answer a new instance of the receiver from the file named aFilename with creation flags anInteger."

	^self new value: (Horde3DLibrary default 
				addResource: super resourceType
				name: aFilename
				flags: anInteger)!

resourceType
	"Private - Answer the appropriate resource type for the class."

	^FontMaterial! !
!Horde3DFontMaterialResource class categoriesFor: #fromFile:flags:!public! !
!Horde3DFontMaterialResource class categoriesFor: #resourceType!private! !

Horde3DPresenter guid: (GUID fromString: '{853E2408-47E8-41B3-AC42-5A4F81FD1E7B}')!
Horde3DPresenter comment: ''!
!Horde3DPresenter categoriesForClass!MVP-Presenters! !
!Horde3DPresenter methodsFor!

createSchematicWiring
	"Create the trigger wiring for the receiver"

	super createSchematicWiring.
	self 
		when: #cameraViewChanged:
		send: #onCameraViewChanged:
		to: self!

onCameraViewChanged: anExtentPoint 
	"Sent by the receiver's view when we must set new virtual camera parameters."

	! !
!Horde3DPresenter categoriesFor: #createSchematicWiring!public! !
!Horde3DPresenter categoriesFor: #onCameraViewChanged:!public! !

!Horde3DPresenter class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.Horde3DView)  98 13 0 0 98 2 8 1140850688 1 416 0 0 0 7 0 0 0 416 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 328198 ##(Smalltalk.Point)  2559 21 626 701 501 416 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 93 6 0 0 4 1 0 0] 98 0 626 193 193 0 27 )! !
!Horde3DPresenter class categoriesFor: #resource_Default_view!public!resources-views! !

Horde3DShell guid: (GUID fromString: '{23DDD150-871C-48E3-B538-A36770D2687D}')!
Horde3DShell comment: ''!
!Horde3DShell categoriesForClass!MVP-Presenters! !
!Horde3DShell methodsFor!

createComponents
	"Create the presenters contained by the receiver."

	super createComponents.
	horde3DPresenter := self add: Horde3DPresenter new name: 'horde'!

createSchematicWiring
	"Create the trigger wiring for the receiver"

	super createSchematicWiring.
	horde3DPresenter 
		when: #cameraViewChanged:
		send: #onCameraViewChanged:
		to: self!

initialize
	"Private - Initialize the receiver."

	super initialize.
	engine := Horde3DEngine new!

mainLoop
	"This is the application main loop, it'll be called once per frame to update your application's logic."

	^self subclassResponsibility!

onCameraViewChanged: anExtentPoint 
	"Default handler for a camera view changed event. Might be overriden in subclasses."

	engine currentCamera fov: 45 aspectRatio: anExtentPoint!

onViewClosed
	engine stopMainLoop.
	super onViewClosed!

onViewOpened
	super onViewOpened.
	self setup.
	self onCameraViewChanged: horde3DPresenter view extent.
	engine runMainLoopOn: self!

setup
	"This method will be called before #mainLoop to let you do any setup the application needs."

	^self subclassResponsibility! !
!Horde3DShell categoriesFor: #createComponents!public! !
!Horde3DShell categoriesFor: #createSchematicWiring!public! !
!Horde3DShell categoriesFor: #initialize!initializing!private! !
!Horde3DShell categoriesFor: #mainLoop!public! !
!Horde3DShell categoriesFor: #onCameraViewChanged:!public! !
!Horde3DShell categoriesFor: #onViewClosed!public! !
!Horde3DShell categoriesFor: #onViewOpened!public! !
!Horde3DShell categoriesFor: #setup!public! !

!Horde3DShell class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1617 1255 679 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 32 234 256 98 2 410 8 ##(Smalltalk.Horde3DView)  98 13 0 416 98 2 8 1140850688 1 656 0 0 0 7 0 0 0 656 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1 1 530 1601 1201 656 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 32 3 0 0 88 2 0 0] 98 0 530 193 193 0 27 8 'horde' 0 0 0 0 0 1 0 0 0 0 1 0 0 738 202 208 98 2 802 832 98 2 530 2559 21 530 1617 1255 416 802 8 #updateMenuBar 608 416 898 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 39 8 0 0 125 2 0 0] 98 1 656 960 0 27 )! !
!Horde3DShell class categoriesFor: #resource_Default_view!public!resources-views! !

Knight guid: (GUID fromString: '{115E1C51-CB10-4F49-9124-2EA7A4A234EE}')!
Knight comment: ''!
!Knight categoriesForClass!MVP-Presenters! !
!Knight methodsFor!

addCamera
	"Private - Add a camera node to the scene."

	engine scene addCamera: 'Camera' withPipeline: hdrPipeline!

addEnvironment
	"Private - Add the environment nodes to the scene."

	| environment |
	environment := engine scene 
				addNodesFrom: (engine resourceManager sceneGraphResourceNamed: self sphereResourceName).
	environment 
		translation: 0 @ -20 @ 0
		rotation: 0 @ 0 @ 0
		scale: 20 @ 20 @ 20!

addKnight
	"Private - Add the knight nodes to the scene."

	knight := engine scene 
				addNodesFrom: (engine resourceManager sceneGraphResourceNamed: self knightResourceName).
	knight 
		translation: 0 @ 0 @ 0
		rotation: 0 @ 180 @ 0
		scale: 0.1 @ 0.1 @ 0.1.
	knight setupAnimationStage: 1
		withResource: (engine resourceManager animationResourceNamed: self knightOrderResourceName).
	knight setupAnimationStage: 2
		withResource: (engine resourceManager animationResourceNamed: self knightAttackResourceName)!

addLight
	"Private - Add light source."

	| light |
	light := engine scene 
				addLight: 'Light1'
				usingLightingShaderContext: 'LIGHTING'
				usingShadowShaderContext: 'SHADOWMAP'.
	light 
		translation: 0 @ 15 @ 10
		rotation: -60 @ 0 @ 0
		scale: 1 @ 1 @ 1.
	light
		radius: 30;
		fov: 90;
		shadowMapCount: 1;
		shadowMapBias: 0.01;
		color: (RGB 
					red: 255
					green: 204
					blue: 178)!

addParticleSystem
	"Private - Attach a particle system to the knight's hand joint."

	| hand |
	hand := (engine scene jointNodesNamed: 'Bip01_R_Hand' at: knight) first.
	particleSystem := engine scene 
				addNodesFrom: (engine resourceManager sceneGraphResourceNamed: self particleSystemResourceName)
				at: hand.
	particleSystem 
		translation: 0 @ 40 @ 0
		rotation: 90 @ 0 @ 0
		scale: 1 @ 1 @ 1!

addResources
	"Private - Add the resources we're going to use."

	hdrPipeline := engine resourceManager addPipelineFromFile: 'hdr.pipeline.xml'.
	forwardPipeline := engine resourceManager addPipelineFromFile: 'forward.pipeline.xml'.
	font := engine resourceManager addFontMaterialFromFile: 'font.material.xml'.
	logo := engine resourceManager addMaterialFromFile: 'logo.material.xml'.
	engine resourceManager addSceneGraphFromFile: self sphereResourceName.
	engine resourceManager addSceneGraphFromFile: self knightResourceName.
	engine resourceManager addAnimationFromFile: self knightOrderResourceName.
	engine resourceManager addAnimationFromFile: self knightAttackResourceName.
	engine resourceManager addSceneGraphFromFile: self particleSystemResourceName!

addSceneNodes
	"Private - Add scene nodes."

	self addCamera.
	self addEnvironment.
	self addKnight.
	self addParticleSystem.
	self addLight!

animateKnight
	"Private - Animate the knight."

	animationTime := animationTime + (1 / engine fps).
	knight 
		animationTime: animationTime * 24
		weight: blendWeight
		forStage: 1.
	knight 
		animationTime: animationTime * 24
		weight: 1 - blendWeight
		forStage: 2!

animateParticleSystem
	"Private - Animate the particle system."

	| emitterNodes |
	emitterNodes := engine scene emitterNodesAt: particleSystem.
	emitterNodes do: [:each | each advanceTime: 1 / engine fps]!

initialize
	"Private - Initialize the receiver."

	super initialize.
	cameraPosition := 5 @ 3 @ 19.
	cameraRotation := 7 @ 15 @ 0.
	animationTime := 0.
	blendWeight := 1.0!

knightAttackResourceName
	"Private - Answer the knight attack animation resource name."

	^'knight_attack.anim'!

knightOrderResourceName
	"Private - Answer the knight order animation resource name."

	^'knight_order.anim'!

knightResourceName
	"Private - Answer the knight scene graph resource name."

	^'knight.scene.xml'!

loadResources
	"Private - Load resources from disk."

	self setResourcesPaths.
	self addResources.
	engine resourceManager loadResourcesFrom: self resourcesPath!

mainLoop
	"The receiver's main loop."

	self animateKnight.
	self animateParticleSystem.
	self updateCamera.
	self showStats.
	self showLogo.
	Horde3DUtilsLibrary default dumpMessages!

onKeyPressed: aKeyEvent 
	"Private - Handler for the various key events we're interested in."

	| keyCode |
	keyCode := aKeyEvent code.
	keyCode == VK_SPACE ifTrue: [engine pause: engine isPaused not].
	keyCode == VK_ESCAPE ifTrue: [self view close].
	^super onKeyPressed: aKeyEvent!

particleSystemResourceName
	"Private - Answer the particle system scene graph resource name."

	^'particleSys1.scene.xml'!

postProcessingEffects
	"Private - Customize post processing effects."

	| materialResource |
	materialResource := engine resourceManager materialResourceNamed: 'postHDR.material.xml'.
	materialResource setUniform: 'hdrParams'
		values: (FLOATArray 
				with: 2.5
				with: 0.5
				with: 0.08
				with: 0)!

resourcesPath
	"Private - Answer the resources dictectory pathname."

	^'C:/Documents and Settings/Administrator/Desktop/Horde3D_SDK_1.0.0_Beta2/Horde3D/Binaries/Content'!

setEngineOptions
	"Private - Set the engine options we want to use."

	engine options loadTextures: true.
	engine options textureCompression: false.
	engine options fastAnimation: false.
	engine options anisotropyFactor: 8.
	engine options shadowMapSize: 2048!

setResourcesPaths
	"Private - Set paths for resources."

	engine resourceManager sceneGraphPath: 'models'.
	engine resourceManager geometryPath: 'models'.
	engine resourceManager animationPath: 'models'.
	engine resourceManager materialPath: 'materials'.
	engine resourceManager codePath: 'shaders'.
	engine resourceManager shaderPath: 'shaders'.
	engine resourceManager texture2DPath: 'textures'.
	engine resourceManager textureCubePath: 'textures'.
	engine resourceManager effectPath: 'effects'.
	engine resourceManager pipelinePath: 'pipelines'!

setup
	"Setup the receiver before #mainLoop is called."

	self setEngineOptions.
	self loadResources.
	self addSceneNodes.
	self postProcessingEffects!

showLogo
	"Private - Show the engine logo."

	logo showOverlayAt: (Rectangle origin: 0.75 @ 0.8 corner: 1 @ 1) usingLayer: 8!

showStats
	"Private - Show frame and weight statistics."

	| stream |
	font showFrameStats: engine fps.
	stream := String writeStream.
	blendWeight printOn: stream decimalPlaces: 2.
	font 
		show: 'Weight: ' , stream contents
		at: 0 @ 0.22
		withScale: 0.03
		usingLayer: 1!

sphereResourceName
	"Private - Answer the sphere scene graph resource name."

	^'sphere.scene.xml'!

updateCamera
	"Private - Update the camera view."

	engine currentCamera 
		translation: cameraPosition
		rotation: cameraRotation
		scale: 1 @ 1 @ 1! !
!Knight categoriesFor: #addCamera!private! !
!Knight categoriesFor: #addEnvironment!private! !
!Knight categoriesFor: #addKnight!private! !
!Knight categoriesFor: #addLight!private! !
!Knight categoriesFor: #addParticleSystem!private! !
!Knight categoriesFor: #addResources!private! !
!Knight categoriesFor: #addSceneNodes!private! !
!Knight categoriesFor: #animateKnight!private! !
!Knight categoriesFor: #animateParticleSystem!private! !
!Knight categoriesFor: #initialize!private! !
!Knight categoriesFor: #knightAttackResourceName!private! !
!Knight categoriesFor: #knightOrderResourceName!private! !
!Knight categoriesFor: #knightResourceName!private! !
!Knight categoriesFor: #loadResources!private! !
!Knight categoriesFor: #mainLoop!public! !
!Knight categoriesFor: #onKeyPressed:!private! !
!Knight categoriesFor: #particleSystemResourceName!private! !
!Knight categoriesFor: #postProcessingEffects!private! !
!Knight categoriesFor: #resourcesPath!private! !
!Knight categoriesFor: #setEngineOptions!private! !
!Knight categoriesFor: #setResourcesPaths!private! !
!Knight categoriesFor: #setup!public! !
!Knight categoriesFor: #showLogo!private! !
!Knight categoriesFor: #showStats!private! !
!Knight categoriesFor: #sphereResourceName!private! !
!Knight categoriesFor: #updateCamera!private! !

!Knight class methodsFor!

resource_Default_view
	"Answer the literal data from which the 'Default view' resource can be reconstituted.
	DO NOT EDIT OR RECATEGORIZE THIS METHOD.

	If you wish to modify this resource evaluate:
	ViewComposer openOn: (ResourceIdentifier class: self selector: #resource_Default_view)
	"

	^#(#'!!STL' 3 788558 10 ##(Smalltalk.STBViewProxy)  8 ##(Smalltalk.ShellView)  98 27 0 0 98 2 27131905 131073 416 0 524550 ##(Smalltalk.ColorRef)  8 4278190080 328198 ##(Smalltalk.Point)  1617 1255 679 0 0 0 416 1180166 ##(Smalltalk.ProportionalLayout)  234 240 98 0 32 234 256 98 2 410 8 ##(Smalltalk.Horde3DView)  98 13 0 416 98 2 8 1140850688 1 656 0 0 0 7 0 0 0 656 0 983302 ##(Smalltalk.MessageSequence)  202 208 98 1 721670 ##(Smalltalk.MessageSend)  8 #createAt:extent: 98 2 530 1 1 530 1601 1201 656 983302 ##(Smalltalk.WINDOWPLACEMENT)  8 #[44 0 0 0 0 0 0 0 1 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 0 0 0 0 0 0 0 0 32 3 0 0 88 2 0 0] 98 0 530 193 193 0 27 8 'horde' 0 0 0 0 0 1 0 0 0 0 1 0 0 738 202 208 98 3 802 832 98 2 530 2559 21 530 1617 1255 416 802 8 #text: 98 1 8 'Knight - Horde3DTalk Sample' 416 802 8 #updateMenuBar 608 416 898 8 #[44 0 0 0 0 0 0 0 0 0 0 0 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 255 4 0 0 10 0 0 0 39 8 0 0 125 2 0 0] 98 1 656 960 0 27 )! !
!Knight class categoriesFor: #resource_Default_view!public!resources-views! !

Horde3DView guid: (GUID fromString: '{BEE60EB7-7071-4D61-9450-6DB29956238B}')!
Horde3DView comment: ''!
!Horde3DView categoriesForClass!MVP-Resources-Misc! !
!Horde3DView methodsFor!

createRC
	"Private - Create an OpenGL rendering context for the receiver."

	hDC := self getDC.
	Horde3DUtilsLibrary default initOpenGL: hDC!

destroyRC
	"Private - Destroy the receiver's OpenGL rendering context."

	Horde3DUtilsLibrary default releaseOpenGL.
	self releaseDC: hDC!

onCameraViewChanged
	"Handler for camera view change event."

	self presenter trigger: #cameraViewChanged: with: self extent!

onPositionChanged: aPositionEvent 
	"On a resize event we must resize the OpenGL rendering viewport."

	aPositionEvent isResize 
		ifTrue: 
			[self resize.
			self onCameraViewChanged].
	super onPositionChanged: aPositionEvent!

onViewClosed
	"Release Horde3D and destroy the receiver's rendering context."

	Horde3DLibrary default release.
	self destroyRC.
	super onViewClosed!

onViewOpened
	"Create a rendering context for the receiver and initialize Horde3D."

	self createRC.
	Horde3DLibrary default init ifFalse: [Horde3DError signal: 'Error trying to initialize Horde3D'].
	self resize.
	super onViewOpened!

resize
	"Resize the rendering viewport with the new reciver's dimensions."

	Horde3DLibrary default 
		resize: 0
		y: 0
		width: self width
		height: self height! !
!Horde3DView categoriesFor: #createRC!private! !
!Horde3DView categoriesFor: #destroyRC!private! !
!Horde3DView categoriesFor: #onCameraViewChanged!public! !
!Horde3DView categoriesFor: #onPositionChanged:!event handling!public! !
!Horde3DView categoriesFor: #onViewClosed!event handling!public! !
!Horde3DView categoriesFor: #onViewOpened!public! !
!Horde3DView categoriesFor: #resize!public! !

!Horde3DView class methodsFor!

publishedEventsOfInstances
	"Answer a Set of Symbols that describe the published events triggered
    	by instances of the receiver."

	^(super publishedEventsOfInstances)
		add: #cameraViewChanged:;
		yourself! !
!Horde3DView class categoriesFor: #publishedEventsOfInstances!public! !

"Binary Globals"!

