pragma Ada_2012;

with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Extensions;
with Interfaces.C.Strings;

package raylib_h is

   RAYLIB_VERSION : aliased constant String :=
     "4.0" & ASCII.NUL;  --  .\deps\raylib\src\raylib.h:83

   PI : constant :=
     3.141_592_653_589_793_238_46;  --  .\deps\raylib\src\raylib.h:103
   --  unsupported macro: DEG2RAD (PI/180.0f)
   --  unsupported macro: RAD2DEG (180.0f/PI)
   --  arg-macro: procedure RL_MALLOC (sz)
   --    malloc(sz)
   --  arg-macro: procedure RL_CALLOC (n, sz)
   --    calloc(n,sz)
   --  arg-macro: procedure RL_REALLOC (ptr, sz)
   --    realloc(ptr,sz)
   --  arg-macro: procedure RL_FREE (ptr)
   --    free(ptr)
   --  arg-macro: function CLITERAL (type)
   --    return type;
   --  unsupported macro: LIGHTGRAY CLITERAL(Color){ 200, 200, 200, 255 }
   --  unsupported macro: GRAY CLITERAL(Color){ 130, 130, 130, 255 }
   --  unsupported macro: DARKGRAY CLITERAL(Color){ 80, 80, 80, 255 }
   --  unsupported macro: YELLOW CLITERAL(Color){ 253, 249, 0, 255 }
   --  unsupported macro: GOLD CLITERAL(Color){ 255, 203, 0, 255 }
   --  unsupported macro: ORANGE CLITERAL(Color){ 255, 161, 0, 255 }
   --  unsupported macro: PINK CLITERAL(Color){ 255, 109, 194, 255 }
   --  unsupported macro: RED CLITERAL(Color){ 230, 41, 55, 255 }
   --  unsupported macro: MAROON CLITERAL(Color){ 190, 33, 55, 255 }
   --  unsupported macro: GREEN CLITERAL(Color){ 0, 228, 48, 255 }
   --  unsupported macro: LIME CLITERAL(Color){ 0, 158, 47, 255 }
   --  unsupported macro: DARKGREEN CLITERAL(Color){ 0, 117, 44, 255 }
   --  unsupported macro: SKYBLUE CLITERAL(Color){ 102, 191, 255, 255 }
   --  unsupported macro: BLUE CLITERAL(Color){ 0, 121, 241, 255 }
   --  unsupported macro: DARKBLUE CLITERAL(Color){ 0, 82, 172, 255 }
   --  unsupported macro: PURPLE CLITERAL(Color){ 200, 122, 255, 255 }
   --  unsupported macro: VIOLET CLITERAL(Color){ 135, 60, 190, 255 }
   --  unsupported macro: DARKPURPLE CLITERAL(Color){ 112, 31, 126, 255 }
   --  unsupported macro: BEIGE CLITERAL(Color){ 211, 176, 131, 255 }
   --  unsupported macro: BROWN CLITERAL(Color){ 127, 106, 79, 255 }
   --  unsupported macro: DARKBROWN CLITERAL(Color){ 76, 63, 47, 255 }
   --  unsupported macro: WHITE CLITERAL(Color){ 255, 255, 255, 255 }
   --  unsupported macro: BLACK CLITERAL(Color){ 0, 0, 0, 255 }
   --  unsupported macro: BLANK CLITERAL(Color){ 0, 0, 0, 0 }
   --  unsupported macro: MAGENTA CLITERAL(Color){ 255, 0, 255, 255 }
   --  unsupported macro: RAYWHITE CLITERAL(Color){ 245, 245, 245, 255 }
   --  unsupported macro: MOUSE_LEFT_BUTTON MOUSE_BUTTON_LEFT
   --  unsupported macro: MOUSE_RIGHT_BUTTON MOUSE_BUTTON_RIGHT
   --  unsupported macro: MOUSE_MIDDLE_BUTTON MOUSE_BUTTON_MIDDLE
   --  unsupported macro: MATERIAL_MAP_DIFFUSE MATERIAL_MAP_ALBEDO
   --  unsupported macro: MATERIAL_MAP_SPECULAR MATERIAL_MAP_METALNESS
   --  unsupported macro: SHADER_LOC_MAP_DIFFUSE SHADER_LOC_MAP_ALBEDO
   --  unsupported macro: SHADER_LOC_MAP_SPECULAR SHADER_LOC_MAP_METALNESS

   --*********************************************************************************************
   --*
   --*   raylib v4.0 - A simple and easy-to-use library to enjoy videogames programming (www.raylib.com)
   --*
   --*   FEATURES:
   --*       - NO external dependencies, all required libraries included with raylib
--*       - Multiplatform: Windows, Linux, FreeBSD, OpenBSD, NetBSD, DragonFly,
   --*                        MacOS, Haiku, Android, Raspberry Pi, DRM native, HTML5.
   --*       - Written in plain C code (C99) in PascalCase/camelCase notation
   --*       - Hardware accelerated with OpenGL (1.1, 2.1, 3.3, 4.3 or ES2 - choose at compile)
   --*       - Unique OpenGL abstraction layer (usable as standalone module): [rlgl]
--*       - Multiple Fonts formats supported (TTF, XNA fonts, AngelCode fonts)
   --*       - Outstanding texture formats support, including compressed formats (DXT, ETC, ASTC)
   --*       - Full 3d support for 3d Shapes, Models, Billboards, Heightmaps and more!
   --*       - Flexible Materials system, supporting classic maps and PBR maps
   --*       - Animated 3D models supported (skeletal bones animation) (IQM)
--*       - Shaders support, including Model shaders and Postprocessing shaders
   --*       - Powerful math module for Vector, Matrix and Quaternion operations: [raymath]
   --*       - Audio loading and playing with streaming support (WAV, OGG, MP3, FLAC, XM, MOD)
   --*       - VR stereo rendering with configurable HMD device parameters
   --*       - Bindings to multiple programming languages available!
   --*
   --*   NOTES:
   --*       - One default Font is loaded on InitWindow()->LoadFontDefault() [core, text]
   --*       - One default Texture2D is loaded on rlglInit(), 1x1 white pixel R8G8B8A8 [rlgl] (OpenGL 3.3 or ES2)
   --*       - One default Shader is loaded on rlglInit()->rlLoadShaderDefault() [rlgl] (OpenGL 3.3 or ES2)
   --*       - One default RenderBatch is loaded on rlglInit()->rlLoadRenderBatch() [rlgl] (OpenGL 3.3 or ES2)
   --*
   --*   DEPENDENCIES (included):
   --*       [rcore] rglfw (Camilla Löwy - github.com/glfw/glfw) for window/context management and input (PLATFORM_DESKTOP)
   --*       [rlgl] glad (David Herberth - github.com/Dav1dde/glad) for OpenGL 3.3 extensions loading (PLATFORM_DESKTOP)
   --*       [raudio] miniaudio (David Reid - github.com/mackron/miniaudio) for audio device/context management
   --*
   --*   OPTIONAL DEPENDENCIES (included):
   --*       [rcore] msf_gif (Miles Fogle) for GIF recording
   --*       [rcore] sinfl (Micha Mettke) for DEFLATE decompression algorythm
   --*       [rcore] sdefl (Micha Mettke) for DEFLATE compression algorythm
   --*       [rtextures] stb_image (Sean Barret) for images loading (BMP, TGA, PNG, JPEG, HDR...)
   --*       [rtextures] stb_image_write (Sean Barret) for image writing (BMP, TGA, PNG, JPG)
   --*       [rtextures] stb_image_resize (Sean Barret) for image resizing algorithms
   --*       [rtext] stb_truetype (Sean Barret) for ttf fonts loading
   --*       [rtext] stb_rect_pack (Sean Barret) for rectangles packing
   --*       [rmodels] par_shapes (Philip Rideout) for parametric 3d shapes generation
   --*       [rmodels] tinyobj_loader_c (Syoyo Fujita) for models loading (OBJ, MTL)
   --*       [rmodels] cgltf (Johannes Kuhlmann) for models loading (glTF)
   --*       [raudio] dr_wav (David Reid) for WAV audio file loading
   --*       [raudio] dr_flac (David Reid) for FLAC audio file loading
   --*       [raudio] dr_mp3 (David Reid) for MP3 audio file loading
   --*       [raudio] stb_vorbis (Sean Barret) for OGG audio loading
   --*       [raudio] jar_xm (Joshua Reisenauer) for XM audio module loading
   --*       [raudio] jar_mod (Joshua Reisenauer) for MOD audio module loading
   --*
   --*
   --*   LICENSE: zlib/libpng
   --*
   --*   raylib is licensed under an unmodified zlib/libpng license, which is an OSI-certified,
--*   BSD-like license that allows static linking with closed source software:
   --*
   --*   Copyright (c) 2013-2021 Ramon Santamaria (@raysan5)
   --*
   --*   This software is provided "as-is", without any express or implied warranty. In no event
   --*   will the authors be held liable for any damages arising from the use of this software.
   --*
   --*   Permission is granted to anyone to use this software for any purpose, including commercial
   --*   applications, and to alter it and redistribute it freely, subject to the following restrictions:
   --*
   --*     1. The origin of this software must not be misrepresented; you must not claim that you
   --*     wrote the original software. If you use this software in a product, an acknowledgment
--*     in the product documentation would be appreciated but is not required.
   --*
   --*     2. Altered source versions must be plainly marked as such, and must not be misrepresented
   --*     as being the original software.
   --*
   --*     3. This notice may not be removed or altered from any source distribution.
   --*
   --*********************************************************************************************

   -- Required for: va_list - Only used by TraceLogCallback
   -- Function specifiers in case library is build/used as a shared library (Windows)
   -- NOTE: Microsoft specifiers to tell compiler that symbols are imported/exported from a .dll
   ------------------------------------------------------------------------------------
   -- Some basic Defines
   ------------------------------------------------------------------------------------
   -- Allow custom memory allocators
   -- NOTE: MSVC C++ compiler does not support compound literals (C99 feature)
   -- Plain structures in C++ (without constructors) can be initialized with { }
   -- NOTE: We set some defines with some data types declared by raylib
   -- Other modules (raymath, rlgl) also require some of those types, so,
   -- to be able to use those other modules as standalone (not depending on raylib)
   -- this defines are very useful for internal check and avoid type (re)definitions
   -- Some Basic Colors
   -- NOTE: Custom raylib color palette for amazing visuals on WHITE background
   ------------------------------------------------------------------------------------
   -- Structures Definition
   ------------------------------------------------------------------------------------
   -- Boolean type
   -- Vector2, 2 components
   -- Vector x component
   type Vector2 is record
      x : aliased Float;  -- .\deps\raylib\src\raylib.h:189
      y : aliased Float;  -- .\deps\raylib\src\raylib.h:190
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:188

      -- Vector y component
      -- Vector3, 3 components
      -- Vector x component
   type Vector3 is record
      x : aliased Float;  -- .\deps\raylib\src\raylib.h:195
      y : aliased Float;  -- .\deps\raylib\src\raylib.h:196
      z : aliased Float;  -- .\deps\raylib\src\raylib.h:197
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:194

      -- Vector y component
      -- Vector z component
      -- Vector4, 4 components
      -- Vector x component
   type Vector4 is record
      x : aliased Float;  -- .\deps\raylib\src\raylib.h:202
      y : aliased Float;  -- .\deps\raylib\src\raylib.h:203
      z : aliased Float;  -- .\deps\raylib\src\raylib.h:204
      w : aliased Float;  -- .\deps\raylib\src\raylib.h:205
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:201

      -- Vector y component
      -- Vector z component
      -- Vector w component
      -- Quaternion, 4 components (Vector4 alias)
   subtype Quaternion is Vector4;  -- .\deps\raylib\src\raylib.h:209

   -- Matrix, 4x4 components, column major, OpenGL style, right handed
   -- Matrix first row (4 components)
   type Matrix is record
      m0  : aliased Float;  -- .\deps\raylib\src\raylib.h:213
      m4  : aliased Float;  -- .\deps\raylib\src\raylib.h:213
      m8  : aliased Float;  -- .\deps\raylib\src\raylib.h:213
      m12 : aliased Float;  -- .\deps\raylib\src\raylib.h:213
      m1  : aliased Float;  -- .\deps\raylib\src\raylib.h:214
      m5  : aliased Float;  -- .\deps\raylib\src\raylib.h:214
      m9  : aliased Float;  -- .\deps\raylib\src\raylib.h:214
      m13 : aliased Float;  -- .\deps\raylib\src\raylib.h:214
      m2  : aliased Float;  -- .\deps\raylib\src\raylib.h:215
      m6  : aliased Float;  -- .\deps\raylib\src\raylib.h:215
      m10 : aliased Float;  -- .\deps\raylib\src\raylib.h:215
      m14 : aliased Float;  -- .\deps\raylib\src\raylib.h:215
      m3  : aliased Float;  -- .\deps\raylib\src\raylib.h:216
      m7  : aliased Float;  -- .\deps\raylib\src\raylib.h:216
      m11 : aliased Float;  -- .\deps\raylib\src\raylib.h:216
      m15 : aliased Float;  -- .\deps\raylib\src\raylib.h:216
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:212

      -- Matrix second row (4 components)
      -- Matrix third row (4 components)
      -- Matrix fourth row (4 components)
      -- Color, 4 components, R8G8B8A8 (32bit)
      -- Color red value
   type Color is record
      r : aliased unsigned_char;  -- .\deps\raylib\src\raylib.h:221
      g : aliased unsigned_char;  -- .\deps\raylib\src\raylib.h:222
      b : aliased unsigned_char;  -- .\deps\raylib\src\raylib.h:223
      a : aliased unsigned_char;  -- .\deps\raylib\src\raylib.h:224
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:220

      -- Color green value
      -- Color blue value
      -- Color alpha value
      -- Rectangle, 4 components
      -- Rectangle top-left corner position x
   type Rectangle is record
      x      : aliased Float;  -- .\deps\raylib\src\raylib.h:229
      y      : aliased Float;  -- .\deps\raylib\src\raylib.h:230
      width  : aliased Float;  -- .\deps\raylib\src\raylib.h:231
      height : aliased Float;  -- .\deps\raylib\src\raylib.h:232
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:228

      -- Rectangle top-left corner position y
      -- Rectangle width
      -- Rectangle height
      -- Image, pixel data stored in CPU memory (RAM)
      -- Image raw data
   type Image is record
      data    : System.Address;  -- .\deps\raylib\src\raylib.h:237
      width   : aliased int;  -- .\deps\raylib\src\raylib.h:238
      height  : aliased int;  -- .\deps\raylib\src\raylib.h:239
      mipmaps : aliased int;  -- .\deps\raylib\src\raylib.h:240
      format  : aliased int;  -- .\deps\raylib\src\raylib.h:241
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:236

      -- Image base width
      -- Image base height
      -- Mipmap levels, 1 by default
      -- Data format (PixelFormat type)
      -- Texture, tex data stored in GPU memory (VRAM)
      -- OpenGL texture id
   type Texture is record
      id      : aliased unsigned;  -- .\deps\raylib\src\raylib.h:246
      width   : aliased int;  -- .\deps\raylib\src\raylib.h:247
      height  : aliased int;  -- .\deps\raylib\src\raylib.h:248
      mipmaps : aliased int;  -- .\deps\raylib\src\raylib.h:249
      format  : aliased int;  -- .\deps\raylib\src\raylib.h:250
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:245

      -- Texture base width
      -- Texture base height
      -- Mipmap levels, 1 by default
      -- Data format (PixelFormat type)
      -- Texture2D, same as Texture
   subtype Texture2D is Texture;  -- .\deps\raylib\src\raylib.h:254

   -- TextureCubemap, same as Texture
   subtype TextureCubemap is Texture;  -- .\deps\raylib\src\raylib.h:257

   -- RenderTexture, fbo for texture rendering
   -- OpenGL framebuffer object id
   type RenderTexture is record
      id          : aliased unsigned;  -- .\deps\raylib\src\raylib.h:261
      the_texture : aliased Texture;  -- .\deps\raylib\src\raylib.h:262
      depth       : aliased Texture;  -- .\deps\raylib\src\raylib.h:263
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:260

      -- Color buffer attachment texture
      -- Depth buffer attachment texture
      -- RenderTexture2D, same as RenderTexture
   subtype RenderTexture2D is RenderTexture;  -- .\deps\raylib\src\raylib.h:267

   -- NPatchInfo, n-patch layout info
   -- Texture source rectangle
   type NPatchInfo is record
      source : aliased Rectangle;  -- .\deps\raylib\src\raylib.h:271
      left   : aliased int;  -- .\deps\raylib\src\raylib.h:272
      top    : aliased int;  -- .\deps\raylib\src\raylib.h:273
      right  : aliased int;  -- .\deps\raylib\src\raylib.h:274
      bottom : aliased int;  -- .\deps\raylib\src\raylib.h:275
      layout : aliased int;  -- .\deps\raylib\src\raylib.h:276
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:270

      -- Left border offset
      -- Top border offset
      -- Right border offset
      -- Bottom border offset
      -- Layout of the n-patch: 3x3, 1x3 or 3x1
      -- GlyphInfo, font characters glyphs info
      -- Character value (Unicode)
   type GlyphInfo is record
      value     : aliased int;  -- .\deps\raylib\src\raylib.h:281
      offsetX   : aliased int;  -- .\deps\raylib\src\raylib.h:282
      offsetY   : aliased int;  -- .\deps\raylib\src\raylib.h:283
      advanceX  : aliased int;  -- .\deps\raylib\src\raylib.h:284
      the_image : aliased Image;  -- .\deps\raylib\src\raylib.h:285
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:280

      -- Character offset X when drawing
      -- Character offset Y when drawing
      -- Character advance position X
      -- Character image data
      -- Font, font texture and GlyphInfo array data
      -- Base size (default chars height)
   type Font is record
      baseSize     : aliased int;  -- .\deps\raylib\src\raylib.h:290
      glyphCount   : aliased int;  -- .\deps\raylib\src\raylib.h:291
      glyphPadding : aliased int;  -- .\deps\raylib\src\raylib.h:292
      texture      : aliased Texture2D;  -- .\deps\raylib\src\raylib.h:293
      recs         : access Rectangle;  -- .\deps\raylib\src\raylib.h:294
      glyphs       : access GlyphInfo;  -- .\deps\raylib\src\raylib.h:295
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:289

      -- Number of glyph characters
      -- Padding around the glyph characters
      -- Texture atlas containing the glyphs
      -- Rectangles in texture for the glyphs
      -- Glyphs info data
      -- Camera, defines position/orientation in 3d space
      -- Camera position
   type Camera3D is record
      position   : aliased Vector3;  -- .\deps\raylib\src\raylib.h:300
      target     : aliased Vector3;  -- .\deps\raylib\src\raylib.h:301
      up         : aliased Vector3;  -- .\deps\raylib\src\raylib.h:302
      fovy       : aliased Float;  -- .\deps\raylib\src\raylib.h:303
      projection : aliased int;  -- .\deps\raylib\src\raylib.h:304
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:299

      -- Camera target it looks-at
      -- Camera up vector (rotation over its axis)
      -- Camera field-of-view apperture in Y (degrees) in perspective, used as near plane width in orthographic
      -- Camera projection: CAMERA_PERSPECTIVE or CAMERA_ORTHOGRAPHIC
      -- Camera type fallback, defaults to Camera3D
   subtype Camera is Camera3D;  -- .\deps\raylib\src\raylib.h:307

   -- Camera2D, defines position/orientation in 2d space
   -- Camera offset (displacement from target)
   type Camera2D is record
      offset   : aliased Vector2;  -- .\deps\raylib\src\raylib.h:311
      target   : aliased Vector2;  -- .\deps\raylib\src\raylib.h:312
      rotation : aliased Float;  -- .\deps\raylib\src\raylib.h:313
      zoom     : aliased Float;  -- .\deps\raylib\src\raylib.h:314
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:310

      -- Camera target (rotation and zoom origin)
      -- Camera rotation in degrees
      -- Camera zoom (scaling), should be 1.0f by default
      -- Mesh, vertex data and vao/vbo
      -- Number of vertices stored in arrays
   type Mesh is record
      vertexCount   : aliased int;  -- .\deps\raylib\src\raylib.h:319
      triangleCount : aliased int;  -- .\deps\raylib\src\raylib.h:320
      vertices      : access Float;  -- .\deps\raylib\src\raylib.h:323
      texcoords     : access Float;  -- .\deps\raylib\src\raylib.h:324
      texcoords2    : access Float;  -- .\deps\raylib\src\raylib.h:325
      normals       : access Float;  -- .\deps\raylib\src\raylib.h:326
      tangents      : access Float;  -- .\deps\raylib\src\raylib.h:327
      colors        : access unsigned_char;  -- .\deps\raylib\src\raylib.h:328
      indices       : access unsigned_short;  -- .\deps\raylib\src\raylib.h:329
      animVertices  : access Float;  -- .\deps\raylib\src\raylib.h:332
      animNormals   : access Float;  -- .\deps\raylib\src\raylib.h:333
      boneIds       : access unsigned_char;  -- .\deps\raylib\src\raylib.h:334
      boneWeights   : access Float;  -- .\deps\raylib\src\raylib.h:335
      vaoId         : aliased unsigned;  -- .\deps\raylib\src\raylib.h:338
      vboId         : access unsigned;  -- .\deps\raylib\src\raylib.h:339
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:318

      -- Number of triangles stored (indexed or not)
      -- Vertex attributes data
      -- Vertex position (XYZ - 3 components per vertex) (shader-location = 0)
      -- Vertex texture coordinates (UV - 2 components per vertex) (shader-location = 1)
      -- Vertex second texture coordinates (useful for lightmaps) (shader-location = 5)
      -- Vertex normals (XYZ - 3 components per vertex) (shader-location = 2)
      -- Vertex tangents (XYZW - 4 components per vertex) (shader-location = 4)
      -- Vertex colors (RGBA - 4 components per vertex) (shader-location = 3)
      -- Vertex indices (in case vertex data comes indexed)
      -- Animation vertex data
      -- Animated vertex positions (after bones transformations)
      -- Animated normals (after bones transformations)
      -- Vertex bone ids, max 255 bone ids, up to 4 bones influence by vertex (skinning)
      -- Vertex bone weight, up to 4 bones influence by vertex (skinning)
      -- OpenGL identifiers
      -- OpenGL Vertex Array Object id
      -- OpenGL Vertex Buffer Objects id (default vertex data)
      -- Shader
      -- Shader program id
   type Shader is record
      id   : aliased unsigned;  -- .\deps\raylib\src\raylib.h:344
      locs : access int;  -- .\deps\raylib\src\raylib.h:345
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:343

      -- Shader locations array (RL_MAX_SHADER_LOCATIONS)
      -- MaterialMap
      -- Material map texture
   type MaterialMap is record
      texture   : aliased Texture2D;  -- .\deps\raylib\src\raylib.h:350
      the_color : aliased Color;  -- .\deps\raylib\src\raylib.h:351
      value     : aliased Float;  -- .\deps\raylib\src\raylib.h:352
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:349

      -- Material map color
      -- Material map value
      -- Material, includes shader and maps
      -- Material shader
   type anon895_array899 is array (0 .. 3) of aliased Float;
   type Material is record
      the_shader : aliased Shader;  -- .\deps\raylib\src\raylib.h:357
      maps       : access MaterialMap;  -- .\deps\raylib\src\raylib.h:358
      params     : aliased anon895_array899;  -- .\deps\raylib\src\raylib.h:359
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:356

      -- Material maps array (MAX_MATERIAL_MAPS)
      -- Material generic parameters (if required)
      -- Transform, vectex transformation data
      -- Translation
   type Transform is record
      translation : aliased Vector3;  -- .\deps\raylib\src\raylib.h:364
      rotation    : aliased Quaternion;  -- .\deps\raylib\src\raylib.h:365
      scale       : aliased Vector3;  -- .\deps\raylib\src\raylib.h:366
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:363

      -- Rotation
      -- Scale
      -- Bone, skeletal animation bone
      -- Bone name
   subtype anon903_array905 is Interfaces.C.char_array (0 .. 31);
   type BoneInfo is record
      name   : aliased anon903_array905;  -- .\deps\raylib\src\raylib.h:371
      parent : aliased int;  -- .\deps\raylib\src\raylib.h:372
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:370

      -- Bone parent
      -- Model, meshes, materials and animation data
      -- Local transform matrix
   type Model is record
      transform     : aliased Matrix;  -- .\deps\raylib\src\raylib.h:377
      meshCount     : aliased int;  -- .\deps\raylib\src\raylib.h:379
      materialCount : aliased int;  -- .\deps\raylib\src\raylib.h:380
      meshes        : access Mesh;  -- .\deps\raylib\src\raylib.h:381
      materials     : access Material;  -- .\deps\raylib\src\raylib.h:382
      meshMaterial  : access int;  -- .\deps\raylib\src\raylib.h:383
      boneCount     : aliased int;  -- .\deps\raylib\src\raylib.h:386
      bones         : access BoneInfo;  -- .\deps\raylib\src\raylib.h:387
      bindPose : access raylib_h.Transform;  -- .\deps\raylib\src\raylib.h:388
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:376

      -- Number of meshes
      -- Number of materials
      -- Meshes array
      -- Materials array
      -- Mesh material number
      -- Animation data
      -- Number of bones
      -- Bones information (skeleton)
      -- Bones base transformation (pose)
      -- ModelAnimation
      -- Number of bones
   type ModelAnimation is record
      boneCount  : aliased int;  -- .\deps\raylib\src\raylib.h:393
      frameCount : aliased int;  -- .\deps\raylib\src\raylib.h:394
      bones      : access BoneInfo;  -- .\deps\raylib\src\raylib.h:395
      framePoses : System.Address;  -- .\deps\raylib\src\raylib.h:396
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:392

      -- Number of animation frames
      -- Bones information (skeleton)
      -- Poses array by frame
      -- Ray, ray for raycasting
      -- Ray position (origin)
   type Ray is record
      position  : aliased Vector3;  -- .\deps\raylib\src\raylib.h:401
      direction : aliased Vector3;  -- .\deps\raylib\src\raylib.h:402
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:400

      -- Ray direction
      -- RayCollision, ray hit information
      -- Did the ray hit something?
   type RayCollision is record
      hit      : aliased Extensions.bool;  -- .\deps\raylib\src\raylib.h:407
      distance : aliased Float;  -- .\deps\raylib\src\raylib.h:408
      point    : aliased Vector3;  -- .\deps\raylib\src\raylib.h:409
      normal   : aliased Vector3;  -- .\deps\raylib\src\raylib.h:410
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:406

      -- Distance to nearest hit
      -- Point of nearest hit
      -- Surface normal of hit
      -- BoundingBox
      -- Minimum vertex box-corner
   type BoundingBox is record
      min : aliased Vector3;  -- .\deps\raylib\src\raylib.h:415
      max : aliased Vector3;  -- .\deps\raylib\src\raylib.h:416
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:414

      -- Maximum vertex box-corner
      -- Wave, audio wave data
      -- Total number of frames (considering channels)
   type Wave is record
      frameCount : aliased unsigned;  -- .\deps\raylib\src\raylib.h:421
      sampleRate : aliased unsigned;  -- .\deps\raylib\src\raylib.h:422
      sampleSize : aliased unsigned;  -- .\deps\raylib\src\raylib.h:423
      channels   : aliased unsigned;  -- .\deps\raylib\src\raylib.h:424
      data       : System.Address;  -- .\deps\raylib\src\raylib.h:425
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:420

      -- Frequency (samples per second)
      -- Bit depth (bits per sample): 8, 16, 32 (24 not supported)
      -- Number of channels (1-mono, 2-stereo, ...)
      -- Buffer data pointer
   type rAudioBuffer is null record;   -- incomplete struct

   -- AudioStream, custom audio stream
   -- Pointer to internal data used by the audio system
   type AudioStream is record
      buffer     : access rAudioBuffer;  -- .\deps\raylib\src\raylib.h:432
      sampleRate : aliased unsigned;  -- .\deps\raylib\src\raylib.h:434
      sampleSize : aliased unsigned;  -- .\deps\raylib\src\raylib.h:435
      channels   : aliased unsigned;  -- .\deps\raylib\src\raylib.h:436
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:431

      -- Frequency (samples per second)
      -- Bit depth (bits per sample): 8, 16, 32 (24 not supported)
      -- Number of channels (1-mono, 2-stereo, ...)
      -- Sound
      -- Audio stream
   type Sound is record
      stream     : aliased AudioStream;  -- .\deps\raylib\src\raylib.h:441
      frameCount : aliased unsigned;  -- .\deps\raylib\src\raylib.h:442
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:440

   -- Total number of frames (considering channels)
   -- Music, audio stream, anything longer than ~10 seconds should be streamed
   -- Audio stream
   type Music is record
      stream     : aliased AudioStream;  -- .\deps\raylib\src\raylib.h:447
      frameCount : aliased unsigned;  -- .\deps\raylib\src\raylib.h:448
      looping    : aliased Extensions.bool;  -- .\deps\raylib\src\raylib.h:449
      ctxType    : aliased int;  -- .\deps\raylib\src\raylib.h:451
      ctxData    : System.Address;  -- .\deps\raylib\src\raylib.h:452
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:446

      -- Total number of frames (considering channels)
      -- Music looping enable
      -- Type of music context (audio filetype)
      -- Audio context data, depends on type
      -- VrDeviceInfo, Head-Mounted-Display device parameters
      -- Horizontal resolution in pixels
   type anon939_array899 is array (0 .. 3) of aliased Float;
   type VrDeviceInfo is record
      hResolution            : aliased int;  -- .\deps\raylib\src\raylib.h:457
      vResolution            : aliased int;  -- .\deps\raylib\src\raylib.h:458
      hScreenSize : aliased Float;  -- .\deps\raylib\src\raylib.h:459
      vScreenSize : aliased Float;  -- .\deps\raylib\src\raylib.h:460
      vScreenCenter : aliased Float;  -- .\deps\raylib\src\raylib.h:461
      eyeToScreenDistance : aliased Float;  -- .\deps\raylib\src\raylib.h:462
      lensSeparationDistance : aliased Float;  -- .\deps\raylib\src\raylib.h:463
      interpupillaryDistance : aliased Float;  -- .\deps\raylib\src\raylib.h:464
      lensDistortionValues : aliased anon939_array899;  -- .\deps\raylib\src\raylib.h:465
      chromaAbCorrection : aliased anon939_array899;  -- .\deps\raylib\src\raylib.h:466
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:456

      -- Vertical resolution in pixels
      -- Horizontal size in meters
      -- Vertical size in meters
      -- Screen center in meters
      -- Distance between eye and display in meters
      -- Lens separation distance in meters
      -- IPD (distance between pupils) in meters
      -- Lens distortion constant parameters
      -- Chromatic aberration correction parameters
      -- VrStereoConfig, VR stereo rendering configuration for simulator
      -- VR projection matrices (per eye)
   type anon941_array943 is array (0 .. 1) of aliased Matrix;
   type anon941_array945 is array (0 .. 1) of aliased Float;
   type VrStereoConfig is record
      projection : aliased anon941_array943;  -- .\deps\raylib\src\raylib.h:471
      viewOffset : aliased anon941_array943;  -- .\deps\raylib\src\raylib.h:472
      leftLensCenter : aliased anon941_array945;  -- .\deps\raylib\src\raylib.h:473
      rightLensCenter : aliased anon941_array945;  -- .\deps\raylib\src\raylib.h:474
      leftScreenCenter : aliased anon941_array945;  -- .\deps\raylib\src\raylib.h:475
      rightScreenCenter : aliased anon941_array945;  -- .\deps\raylib\src\raylib.h:476
      scale : aliased anon941_array945;  -- .\deps\raylib\src\raylib.h:477
      scaleIn : aliased anon941_array945;  -- .\deps\raylib\src\raylib.h:478
   end record with
      Convention => C_Pass_By_Copy;  -- .\deps\raylib\src\raylib.h:470

      -- VR view offset matrices (per eye)
      -- VR left lens center
      -- VR right lens center
      -- VR left screen center
      -- VR right screen center
      -- VR distortion scale
      -- VR distortion scale in
      ------------------------------------------------------------------------------------
      -- Enumerators Definition
      ------------------------------------------------------------------------------------
      -- System/Window config flags
      -- NOTE: Every bit registers one state (use it with bit masks)
      -- By default all flags are set to 0
      -- Set to try enabling V-Sync on GPU
      -- Set to run program in fullscreen
      -- Set to allow resizable window
      -- Set to disable window decoration (frame and buttons)
      -- Set to hide window
      -- Set to minimize window (iconify)
      -- Set to maximize window (expanded to monitor)
      -- Set to window non focused
      -- Set to window always on top
      -- Set to allow windows running while minimized
      -- Set to allow transparent framebuffer
      -- Set to support HighDPI
      -- Set to try enabling MSAA 4X
      -- Set to try enabling interlaced video format (for V3D)
   subtype ConfigFlags is unsigned;
   FLAG_VSYNC_HINT         : constant ConfigFlags := 64;
   FLAG_FULLSCREEN_MODE    : constant ConfigFlags := 2;
   FLAG_WINDOW_RESIZABLE   : constant ConfigFlags := 4;
   FLAG_WINDOW_UNDECORATED : constant ConfigFlags := 8;
   FLAG_WINDOW_HIDDEN      : constant ConfigFlags := 128;
   FLAG_WINDOW_MINIMIZED   : constant ConfigFlags := 512;
   FLAG_WINDOW_MAXIMIZED   : constant ConfigFlags := 1_024;
   FLAG_WINDOW_UNFOCUSED   : constant ConfigFlags := 2_048;
   FLAG_WINDOW_TOPMOST     : constant ConfigFlags := 4_096;
   FLAG_WINDOW_ALWAYS_RUN  : constant ConfigFlags := 256;
   FLAG_WINDOW_TRANSPARENT : constant ConfigFlags := 16;
   FLAG_WINDOW_HIGHDPI     : constant ConfigFlags := 8_192;
   FLAG_MSAA_4X_HINT       : constant ConfigFlags := 32;
   FLAG_INTERLACED_HINT    : constant ConfigFlags :=
     65_536;  -- .\deps\raylib\src\raylib.h:502

   -- Trace log level
   -- NOTE: Organized by priority level
   -- Display all logs
   -- Trace logging, intended for internal use only
   -- Debug logging, used for internal debugging, it should be disabled on release builds
   -- Info logging, used for program execution info
   -- Warning logging, used on recoverable failures
   -- Error logging, used on unrecoverable failures
   -- Fatal logging, used to abort program: exit(EXIT_FAILURE)
   -- Disable logging
   type TraceLogLevel is
     (LOG_ALL, LOG_TRACE, LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR,
      LOG_FATAL, LOG_NONE) with
      Convention => C;  -- .\deps\raylib\src\raylib.h:515

      -- Keyboard keys (US keyboard layout)
      -- NOTE: Use GetKeyPressed() to allow redefining
      -- required keys for alternative layouts
      -- Key: NULL, used for no key pressed
      -- Alphanumeric keys
      -- Key: '
      -- Key: ,
      -- Key: -
      -- Key: .
      -- Key: /
      -- Key: 0
      -- Key: 1
      -- Key: 2
      -- Key: 3
      -- Key: 4
      -- Key: 5
      -- Key: 6
      -- Key: 7
      -- Key: 8
      -- Key: 9
      -- Key: ;
      -- Key: =
      -- Key: A | a
      -- Key: B | b
      -- Key: C | c
      -- Key: D | d
      -- Key: E | e
      -- Key: F | f
      -- Key: G | g
      -- Key: H | h
      -- Key: I | i
      -- Key: J | j
      -- Key: K | k
      -- Key: L | l
      -- Key: M | m
      -- Key: N | n
      -- Key: O | o
      -- Key: P | p
      -- Key: Q | q
      -- Key: R | r
      -- Key: S | s
      -- Key: T | t
      -- Key: U | u
      -- Key: V | v
      -- Key: W | w
      -- Key: X | x
      -- Key: Y | y
      -- Key: Z | z
      -- Key: [
      -- Key: '\'
      -- Key: ]
      -- Key: `
      -- Function keys
      -- Key: Space
      -- Key: Esc
      -- Key: Enter
      -- Key: Tab
      -- Key: Backspace
      -- Key: Ins
      -- Key: Del
      -- Key: Cursor right
      -- Key: Cursor left
      -- Key: Cursor down
      -- Key: Cursor up
      -- Key: Page up
      -- Key: Page down
      -- Key: Home
      -- Key: End
      -- Key: Caps lock
      -- Key: Scroll down
      -- Key: Num lock
      -- Key: Print screen
      -- Key: Pause
      -- Key: F1
      -- Key: F2
      -- Key: F3
      -- Key: F4
      -- Key: F5
      -- Key: F6
      -- Key: F7
      -- Key: F8
      -- Key: F9
      -- Key: F10
      -- Key: F11
      -- Key: F12
      -- Key: Shift left
      -- Key: Control left
      -- Key: Alt left
      -- Key: Super left
      -- Key: Shift right
      -- Key: Control right
      -- Key: Alt right
      -- Key: Super right
      -- Key: KB menu
      -- Keypad keys
      -- Key: Keypad 0
      -- Key: Keypad 1
      -- Key: Keypad 2
      -- Key: Keypad 3
      -- Key: Keypad 4
      -- Key: Keypad 5
      -- Key: Keypad 6
      -- Key: Keypad 7
      -- Key: Keypad 8
      -- Key: Keypad 9
      -- Key: Keypad .
      -- Key: Keypad /
      -- Key: Keypad *
      -- Key: Keypad -
      -- Key: Keypad +
      -- Key: Keypad Enter
      -- Key: Keypad =
      -- Android key buttons
      -- Key: Android back button
      -- Key: Android menu button
      -- Key: Android volume up button
      -- Key: Android volume down button
   subtype KeyboardKey is unsigned;
   KEY_NULL          : constant KeyboardKey := 0;
   KEY_APOSTROPHE    : constant KeyboardKey := 39;
   KEY_COMMA         : constant KeyboardKey := 44;
   KEY_MINUS         : constant KeyboardKey := 45;
   KEY_PERIOD        : constant KeyboardKey := 46;
   KEY_SLASH         : constant KeyboardKey := 47;
   KEY_ZERO          : constant KeyboardKey := 48;
   KEY_ONE           : constant KeyboardKey := 49;
   KEY_TWO           : constant KeyboardKey := 50;
   KEY_THREE         : constant KeyboardKey := 51;
   KEY_FOUR          : constant KeyboardKey := 52;
   KEY_FIVE          : constant KeyboardKey := 53;
   KEY_SIX           : constant KeyboardKey := 54;
   KEY_SEVEN         : constant KeyboardKey := 55;
   KEY_EIGHT         : constant KeyboardKey := 56;
   KEY_NINE          : constant KeyboardKey := 57;
   KEY_SEMICOLON     : constant KeyboardKey := 59;
   KEY_EQUAL         : constant KeyboardKey := 61;
   KEY_A             : constant KeyboardKey := 65;
   KEY_B             : constant KeyboardKey := 66;
   KEY_C             : constant KeyboardKey := 67;
   KEY_D             : constant KeyboardKey := 68;
   KEY_E             : constant KeyboardKey := 69;
   KEY_F             : constant KeyboardKey := 70;
   KEY_G             : constant KeyboardKey := 71;
   KEY_H             : constant KeyboardKey := 72;
   KEY_I             : constant KeyboardKey := 73;
   KEY_J             : constant KeyboardKey := 74;
   KEY_K             : constant KeyboardKey := 75;
   KEY_L             : constant KeyboardKey := 76;
   KEY_M             : constant KeyboardKey := 77;
   KEY_N             : constant KeyboardKey := 78;
   KEY_O             : constant KeyboardKey := 79;
   KEY_P             : constant KeyboardKey := 80;
   KEY_Q             : constant KeyboardKey := 81;
   KEY_R             : constant KeyboardKey := 82;
   KEY_S             : constant KeyboardKey := 83;
   KEY_T             : constant KeyboardKey := 84;
   KEY_U             : constant KeyboardKey := 85;
   KEY_V             : constant KeyboardKey := 86;
   KEY_W             : constant KeyboardKey := 87;
   KEY_X             : constant KeyboardKey := 88;
   KEY_Y             : constant KeyboardKey := 89;
   KEY_Z             : constant KeyboardKey := 90;
   KEY_LEFT_BRACKET  : constant KeyboardKey := 91;
   KEY_BACKSLASH     : constant KeyboardKey := 92;
   KEY_RIGHT_BRACKET : constant KeyboardKey := 93;
   KEY_GRAVE         : constant KeyboardKey := 96;
   KEY_SPACE         : constant KeyboardKey := 32;
   KEY_ESCAPE        : constant KeyboardKey := 256;
   KEY_ENTER         : constant KeyboardKey := 257;
   KEY_TAB           : constant KeyboardKey := 258;
   KEY_BACKSPACE     : constant KeyboardKey := 259;
   KEY_INSERT        : constant KeyboardKey := 260;
   KEY_DELETE        : constant KeyboardKey := 261;
   KEY_RIGHT         : constant KeyboardKey := 262;
   KEY_LEFT          : constant KeyboardKey := 263;
   KEY_DOWN          : constant KeyboardKey := 264;
   KEY_UP            : constant KeyboardKey := 265;
   KEY_PAGE_UP       : constant KeyboardKey := 266;
   KEY_PAGE_DOWN     : constant KeyboardKey := 267;
   KEY_HOME          : constant KeyboardKey := 268;
   KEY_END           : constant KeyboardKey := 269;
   KEY_CAPS_LOCK     : constant KeyboardKey := 280;
   KEY_SCROLL_LOCK   : constant KeyboardKey := 281;
   KEY_NUM_LOCK      : constant KeyboardKey := 282;
   KEY_PRINT_SCREEN  : constant KeyboardKey := 283;
   KEY_PAUSE         : constant KeyboardKey := 284;
   KEY_F1            : constant KeyboardKey := 290;
   KEY_F2            : constant KeyboardKey := 291;
   KEY_F3            : constant KeyboardKey := 292;
   KEY_F4            : constant KeyboardKey := 293;
   KEY_F5            : constant KeyboardKey := 294;
   KEY_F6            : constant KeyboardKey := 295;
   KEY_F7            : constant KeyboardKey := 296;
   KEY_F8            : constant KeyboardKey := 297;
   KEY_F9            : constant KeyboardKey := 298;
   KEY_F10           : constant KeyboardKey := 299;
   KEY_F11           : constant KeyboardKey := 300;
   KEY_F12           : constant KeyboardKey := 301;
   KEY_LEFT_SHIFT    : constant KeyboardKey := 340;
   KEY_LEFT_CONTROL  : constant KeyboardKey := 341;
   KEY_LEFT_ALT      : constant KeyboardKey := 342;
   KEY_LEFT_SUPER    : constant KeyboardKey := 343;
   KEY_RIGHT_SHIFT   : constant KeyboardKey := 344;
   KEY_RIGHT_CONTROL : constant KeyboardKey := 345;
   KEY_RIGHT_ALT     : constant KeyboardKey := 346;
   KEY_RIGHT_SUPER   : constant KeyboardKey := 347;
   KEY_KB_MENU       : constant KeyboardKey := 348;
   KEY_KP_0          : constant KeyboardKey := 320;
   KEY_KP_1          : constant KeyboardKey := 321;
   KEY_KP_2          : constant KeyboardKey := 322;
   KEY_KP_3          : constant KeyboardKey := 323;
   KEY_KP_4          : constant KeyboardKey := 324;
   KEY_KP_5          : constant KeyboardKey := 325;
   KEY_KP_6          : constant KeyboardKey := 326;
   KEY_KP_7          : constant KeyboardKey := 327;
   KEY_KP_8          : constant KeyboardKey := 328;
   KEY_KP_9          : constant KeyboardKey := 329;
   KEY_KP_DECIMAL    : constant KeyboardKey := 330;
   KEY_KP_DIVIDE     : constant KeyboardKey := 331;
   KEY_KP_MULTIPLY   : constant KeyboardKey := 332;
   KEY_KP_SUBTRACT   : constant KeyboardKey := 333;
   KEY_KP_ADD        : constant KeyboardKey := 334;
   KEY_KP_ENTER      : constant KeyboardKey := 335;
   KEY_KP_EQUAL      : constant KeyboardKey := 336;
   KEY_BACK          : constant KeyboardKey := 4;
   KEY_MENU          : constant KeyboardKey := 82;
   KEY_VOLUME_UP     : constant KeyboardKey := 24;
   KEY_VOLUME_DOWN   : constant KeyboardKey :=
     25;  -- .\deps\raylib\src\raylib.h:635

   -- Add backwards compatibility support for deprecated names
   -- Mouse buttons
   -- Mouse button left
   -- Mouse button right
   -- Mouse button middle (pressed wheel)
   -- Mouse button side (advanced mouse device)
   -- Mouse button extra (advanced mouse device)
   -- Mouse button fordward (advanced mouse device)
   -- Mouse button back (advanced mouse device)
   type MouseButton is
     (MOUSE_BUTTON_LEFT, MOUSE_BUTTON_RIGHT, MOUSE_BUTTON_MIDDLE,
      MOUSE_BUTTON_SIDE, MOUSE_BUTTON_EXTRA, MOUSE_BUTTON_FORWARD,
      MOUSE_BUTTON_BACK) with
      Convention => C;  -- .\deps\raylib\src\raylib.h:651

      -- Mouse cursor
      -- Default pointer shape
      -- Arrow shape
      -- Text writing cursor shape
      -- Cross shape
      -- Pointing hand cursor
      -- Horizontal resize/move arrow shape
      -- Vertical resize/move arrow shape
      -- Top-left to bottom-right diagonal resize/move arrow shape
      -- The top-right to bottom-left diagonal resize/move arrow shape
      -- The omni-directional resize/move cursor shape
      -- The operation-not-allowed shape
   type MouseCursor is
     (MOUSE_CURSOR_DEFAULT, MOUSE_CURSOR_ARROW, MOUSE_CURSOR_IBEAM,
      MOUSE_CURSOR_CROSSHAIR, MOUSE_CURSOR_POINTING_HAND,
      MOUSE_CURSOR_RESIZE_EW, MOUSE_CURSOR_RESIZE_NS, MOUSE_CURSOR_RESIZE_NWSE,
      MOUSE_CURSOR_RESIZE_NESW, MOUSE_CURSOR_RESIZE_ALL,
      MOUSE_CURSOR_NOT_ALLOWED) with
      Convention => C;  -- .\deps\raylib\src\raylib.h:666

      -- Gamepad buttons
      -- Unknown button, just for error checking
      -- Gamepad left DPAD up button
      -- Gamepad left DPAD right button
      -- Gamepad left DPAD down button
      -- Gamepad left DPAD left button
      -- Gamepad right button up (i.e. PS3: Triangle, Xbox: Y)
      -- Gamepad right button right (i.e. PS3: Square, Xbox: X)
      -- Gamepad right button down (i.e. PS3: Cross, Xbox: A)
      -- Gamepad right button left (i.e. PS3: Circle, Xbox: B)
      -- Gamepad top/back trigger left (first), it could be a trailing button
      -- Gamepad top/back trigger left (second), it could be a trailing button
      -- Gamepad top/back trigger right (one), it could be a trailing button
      -- Gamepad top/back trigger right (second), it could be a trailing button
      -- Gamepad center buttons, left one (i.e. PS3: Select)
      -- Gamepad center buttons, middle one (i.e. PS3: PS, Xbox: XBOX)
      -- Gamepad center buttons, right one (i.e. PS3: Start)
      -- Gamepad joystick pressed button left
      -- Gamepad joystick pressed button right
   type GamepadButton is
     (GAMEPAD_BUTTON_UNKNOWN, GAMEPAD_BUTTON_LEFT_FACE_UP,
      GAMEPAD_BUTTON_LEFT_FACE_RIGHT, GAMEPAD_BUTTON_LEFT_FACE_DOWN,
      GAMEPAD_BUTTON_LEFT_FACE_LEFT, GAMEPAD_BUTTON_RIGHT_FACE_UP,
      GAMEPAD_BUTTON_RIGHT_FACE_RIGHT, GAMEPAD_BUTTON_RIGHT_FACE_DOWN,
      GAMEPAD_BUTTON_RIGHT_FACE_LEFT, GAMEPAD_BUTTON_LEFT_TRIGGER_1,
      GAMEPAD_BUTTON_LEFT_TRIGGER_2, GAMEPAD_BUTTON_RIGHT_TRIGGER_1,
      GAMEPAD_BUTTON_RIGHT_TRIGGER_2, GAMEPAD_BUTTON_MIDDLE_LEFT,
      GAMEPAD_BUTTON_MIDDLE, GAMEPAD_BUTTON_MIDDLE_RIGHT,
      GAMEPAD_BUTTON_LEFT_THUMB, GAMEPAD_BUTTON_RIGHT_THUMB) with
      Convention => C;  -- .\deps\raylib\src\raylib.h:688

      -- Gamepad axis
      -- Gamepad left stick X axis
      -- Gamepad left stick Y axis
      -- Gamepad right stick X axis
      -- Gamepad right stick Y axis
      -- Gamepad back trigger left, pressure level: [1..-1]
      -- Gamepad back trigger right, pressure level: [1..-1]
   type GamepadAxis is
     (GAMEPAD_AXIS_LEFT_X, GAMEPAD_AXIS_LEFT_Y, GAMEPAD_AXIS_RIGHT_X,
      GAMEPAD_AXIS_RIGHT_Y, GAMEPAD_AXIS_LEFT_TRIGGER,
      GAMEPAD_AXIS_RIGHT_TRIGGER) with
      Convention => C;  -- .\deps\raylib\src\raylib.h:698

      -- Material map index
      -- Albedo material (same as: MATERIAL_MAP_DIFFUSE)
      -- Metalness material (same as: MATERIAL_MAP_SPECULAR)
      -- Normal material
      -- Roughness material
      -- Ambient occlusion material
      -- Emission material
      -- Heightmap material
      -- Cubemap material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
      -- Irradiance material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
      -- Prefilter material (NOTE: Uses GL_TEXTURE_CUBE_MAP)
      -- Brdf material
   type MaterialMapIndex is
     (MATERIAL_MAP_ALBEDO, MATERIAL_MAP_METALNESS, MATERIAL_MAP_NORMAL,
      MATERIAL_MAP_ROUGHNESS, MATERIAL_MAP_OCCLUSION, MATERIAL_MAP_EMISSION,
      MATERIAL_MAP_HEIGHT, MATERIAL_MAP_CUBEMAP, MATERIAL_MAP_IRRADIANCE,
      MATERIAL_MAP_PREFILTER, MATERIAL_MAP_BRDF) with
      Convention => C;  -- .\deps\raylib\src\raylib.h:713

      -- Shader location index
      -- Shader location: vertex attribute: position
      -- Shader location: vertex attribute: texcoord01
      -- Shader location: vertex attribute: texcoord02
      -- Shader location: vertex attribute: normal
      -- Shader location: vertex attribute: tangent
      -- Shader location: vertex attribute: color
      -- Shader location: matrix uniform: model-view-projection
      -- Shader location: matrix uniform: view (camera transform)
      -- Shader location: matrix uniform: projection
      -- Shader location: matrix uniform: model (transform)
      -- Shader location: matrix uniform: normal
      -- Shader location: vector uniform: view
      -- Shader location: vector uniform: diffuse color
      -- Shader location: vector uniform: specular color
      -- Shader location: vector uniform: ambient color
      -- Shader location: sampler2d texture: albedo (same as: SHADER_LOC_MAP_DIFFUSE)
      -- Shader location: sampler2d texture: metalness (same as: SHADER_LOC_MAP_SPECULAR)
      -- Shader location: sampler2d texture: normal
      -- Shader location: sampler2d texture: roughness
      -- Shader location: sampler2d texture: occlusion
      -- Shader location: sampler2d texture: emission
      -- Shader location: sampler2d texture: height
      -- Shader location: samplerCube texture: cubemap
      -- Shader location: samplerCube texture: irradiance
      -- Shader location: samplerCube texture: prefilter
      -- Shader location: sampler2d texture: brdf
   type ShaderLocationIndex is
     (SHADER_LOC_VERTEX_POSITION, SHADER_LOC_VERTEX_TEXCOORD01,
      SHADER_LOC_VERTEX_TEXCOORD02, SHADER_LOC_VERTEX_NORMAL,
      SHADER_LOC_VERTEX_TANGENT, SHADER_LOC_VERTEX_COLOR,
      SHADER_LOC_MATRIX_MVP, SHADER_LOC_MATRIX_VIEW,
      SHADER_LOC_MATRIX_PROJECTION, SHADER_LOC_MATRIX_MODEL,
      SHADER_LOC_MATRIX_NORMAL, SHADER_LOC_VECTOR_VIEW,
      SHADER_LOC_COLOR_DIFFUSE, SHADER_LOC_COLOR_SPECULAR,
      SHADER_LOC_COLOR_AMBIENT, SHADER_LOC_MAP_ALBEDO,
      SHADER_LOC_MAP_METALNESS, SHADER_LOC_MAP_NORMAL,
      SHADER_LOC_MAP_ROUGHNESS, SHADER_LOC_MAP_OCCLUSION,
      SHADER_LOC_MAP_EMISSION, SHADER_LOC_MAP_HEIGHT, SHADER_LOC_MAP_CUBEMAP,
      SHADER_LOC_MAP_IRRADIANCE, SHADER_LOC_MAP_PREFILTER,
      SHADER_LOC_MAP_BRDF) with
      Convention => C;  -- .\deps\raylib\src\raylib.h:746

      -- Shader uniform data type
      -- Shader uniform type: float
      -- Shader uniform type: vec2 (2 float)
      -- Shader uniform type: vec3 (3 float)
      -- Shader uniform type: vec4 (4 float)
      -- Shader uniform type: int
      -- Shader uniform type: ivec2 (2 int)
      -- Shader uniform type: ivec3 (3 int)
      -- Shader uniform type: ivec4 (4 int)
      -- Shader uniform type: sampler2d
   type ShaderUniformDataType is
     (SHADER_UNIFORM_FLOAT, SHADER_UNIFORM_VEC2, SHADER_UNIFORM_VEC3,
      SHADER_UNIFORM_VEC4, SHADER_UNIFORM_INT, SHADER_UNIFORM_IVEC2,
      SHADER_UNIFORM_IVEC3, SHADER_UNIFORM_IVEC4,
      SHADER_UNIFORM_SAMPLER2D) with
      Convention => C;  -- .\deps\raylib\src\raylib.h:762

      -- Shader attribute data types
      -- Shader attribute type: float
      -- Shader attribute type: vec2 (2 float)
      -- Shader attribute type: vec3 (3 float)
      -- Shader attribute type: vec4 (4 float)
   type ShaderAttributeDataType is
     (SHADER_ATTRIB_FLOAT, SHADER_ATTRIB_VEC2, SHADER_ATTRIB_VEC3,
      SHADER_ATTRIB_VEC4) with
      Convention => C;  -- .\deps\raylib\src\raylib.h:770

      -- Pixel formats
      -- NOTE: Support depends on OpenGL version and platform
      -- 8 bit per pixel (no alpha)
      -- 8*2 bpp (2 channels)
      -- 16 bpp
      -- 24 bpp
      -- 16 bpp (1 bit alpha)
      -- 16 bpp (4 bit alpha)
      -- 32 bpp
      -- 32 bpp (1 channel - float)
      -- 32*3 bpp (3 channels - float)
      -- 32*4 bpp (4 channels - float)
      -- 4 bpp (no alpha)
      -- 4 bpp (1 bit alpha)
      -- 8 bpp
      -- 8 bpp
      -- 4 bpp
      -- 4 bpp
      -- 8 bpp
      -- 4 bpp
      -- 4 bpp
      -- 8 bpp
      -- 2 bpp
   subtype PixelFormat is unsigned;
   PIXELFORMAT_UNCOMPRESSED_GRAYSCALE    : constant PixelFormat := 1;
   PIXELFORMAT_UNCOMPRESSED_GRAY_ALPHA   : constant PixelFormat := 2;
   PIXELFORMAT_UNCOMPRESSED_R5G6B5       : constant PixelFormat := 3;
   PIXELFORMAT_UNCOMPRESSED_R8G8B8       : constant PixelFormat := 4;
   PIXELFORMAT_UNCOMPRESSED_R5G5B5A1     : constant PixelFormat := 5;
   PIXELFORMAT_UNCOMPRESSED_R4G4B4A4     : constant PixelFormat := 6;
   PIXELFORMAT_UNCOMPRESSED_R8G8B8A8     : constant PixelFormat := 7;
   PIXELFORMAT_UNCOMPRESSED_R32          : constant PixelFormat := 8;
   PIXELFORMAT_UNCOMPRESSED_R32G32B32    : constant PixelFormat := 9;
   PIXELFORMAT_UNCOMPRESSED_R32G32B32A32 : constant PixelFormat := 10;
   PIXELFORMAT_COMPRESSED_DXT1_RGB       : constant PixelFormat := 11;
   PIXELFORMAT_COMPRESSED_DXT1_RGBA      : constant PixelFormat := 12;
   PIXELFORMAT_COMPRESSED_DXT3_RGBA      : constant PixelFormat := 13;
   PIXELFORMAT_COMPRESSED_DXT5_RGBA      : constant PixelFormat := 14;
   PIXELFORMAT_COMPRESSED_ETC1_RGB       : constant PixelFormat := 15;
   PIXELFORMAT_COMPRESSED_ETC2_RGB       : constant PixelFormat := 16;
   PIXELFORMAT_COMPRESSED_ETC2_EAC_RGBA  : constant PixelFormat := 17;
   PIXELFORMAT_COMPRESSED_PVRT_RGB       : constant PixelFormat := 18;
   PIXELFORMAT_COMPRESSED_PVRT_RGBA      : constant PixelFormat := 19;
   PIXELFORMAT_COMPRESSED_ASTC_4x4_RGBA  : constant PixelFormat := 20;
   PIXELFORMAT_COMPRESSED_ASTC_8x8_RGBA  : constant PixelFormat :=
     21;  -- .\deps\raylib\src\raylib.h:796

   -- Texture parameters: filter mode
   -- NOTE 1: Filtering considers mipmaps if available in the texture
   -- NOTE 2: Filter is accordingly set for minification and magnification
   -- No filter, just pixel aproximation
   -- Linear filtering
   -- Trilinear filtering (linear with mipmaps)
   -- Anisotropic filtering 4x
   -- Anisotropic filtering 8x
   -- Anisotropic filtering 16x
   type TextureFilter is
     (TEXTURE_FILTER_POINT, TEXTURE_FILTER_BILINEAR, TEXTURE_FILTER_TRILINEAR,
      TEXTURE_FILTER_ANISOTROPIC_4X, TEXTURE_FILTER_ANISOTROPIC_8X,
      TEXTURE_FILTER_ANISOTROPIC_16X) with
      Convention => C;  -- .\deps\raylib\src\raylib.h:808

      -- Texture parameters: wrap mode
      -- Repeats texture in tiled mode
      -- Clamps texture to edge pixel in tiled mode
      -- Mirrors and repeats the texture in tiled mode
      -- Mirrors and clamps to border the texture in tiled mode
   type TextureWrap is
     (TEXTURE_WRAP_REPEAT, TEXTURE_WRAP_CLAMP, TEXTURE_WRAP_MIRROR_REPEAT,
      TEXTURE_WRAP_MIRROR_CLAMP) with
      Convention => C;  -- .\deps\raylib\src\raylib.h:816

      -- Cubemap layouts
      -- Automatically detect layout type
      -- Layout is defined by a vertical line with faces
      -- Layout is defined by an horizontal line with faces
      -- Layout is defined by a 3x4 cross with cubemap faces
      -- Layout is defined by a 4x3 cross with cubemap faces
      -- Layout is defined by a panorama image (equirectangular map)
   type CubemapLayout is
     (CUBEMAP_LAYOUT_AUTO_DETECT, CUBEMAP_LAYOUT_LINE_VERTICAL,
      CUBEMAP_LAYOUT_LINE_HORIZONTAL, CUBEMAP_LAYOUT_CROSS_THREE_BY_FOUR,
      CUBEMAP_LAYOUT_CROSS_FOUR_BY_THREE, CUBEMAP_LAYOUT_PANORAMA) with
      Convention => C;  -- .\deps\raylib\src\raylib.h:826

      -- Font type, defines generation method
      -- Default font generation, anti-aliased
      -- Bitmap font generation, no anti-aliasing
      -- SDF font generation, requires external shader
   type FontType is (FONT_DEFAULT, FONT_BITMAP, FONT_SDF) with
      Convention => C;  -- .\deps\raylib\src\raylib.h:833

      -- Color blending modes (pre-defined)
      -- Blend textures considering alpha (default)
      -- Blend textures adding colors
      -- Blend textures multiplying colors
      -- Blend textures adding colors (alternative)
      -- Blend textures subtracting colors (alternative)
      -- Belnd textures using custom src/dst factors (use rlSetBlendMode())
   type BlendMode is
     (BLEND_ALPHA, BLEND_ADDITIVE, BLEND_MULTIPLIED, BLEND_ADD_COLORS,
      BLEND_SUBTRACT_COLORS, BLEND_CUSTOM) with
      Convention => C;  -- .\deps\raylib\src\raylib.h:843

      -- Gesture
      -- NOTE: It could be used as flags to enable only some gestures
      -- No gesture
      -- Tap gesture
      -- Double tap gesture
      -- Hold gesture
      -- Drag gesture
      -- Swipe right gesture
      -- Swipe left gesture
      -- Swipe up gesture
      -- Swipe down gesture
      -- Pinch in gesture
      -- Pinch out gesture
   subtype Gesture is unsigned;
   GESTURE_NONE        : constant Gesture := 0;
   GESTURE_TAP         : constant Gesture := 1;
   GESTURE_DOUBLETAP   : constant Gesture := 2;
   GESTURE_HOLD        : constant Gesture := 4;
   GESTURE_DRAG        : constant Gesture := 8;
   GESTURE_SWIPE_RIGHT : constant Gesture := 16;
   GESTURE_SWIPE_LEFT  : constant Gesture := 32;
   GESTURE_SWIPE_UP    : constant Gesture := 64;
   GESTURE_SWIPE_DOWN  : constant Gesture := 128;
   GESTURE_PINCH_IN    : constant Gesture := 256;
   GESTURE_PINCH_OUT   : constant Gesture :=
     512;  -- .\deps\raylib\src\raylib.h:859

   -- Camera system modes
   -- Custom camera
   -- Free camera
   -- Orbital camera
   -- First person camera
   -- Third person camera
   type CameraMode is
     (CAMERA_CUSTOM, CAMERA_FREE, CAMERA_ORBITAL, CAMERA_FIRST_PERSON,
      CAMERA_THIRD_PERSON) with
      Convention => C;  -- .\deps\raylib\src\raylib.h:868

      -- Camera projection
      -- Perspective projection
      -- Orthographic projection
   type CameraProjection is (CAMERA_PERSPECTIVE, CAMERA_ORTHOGRAPHIC) with
      Convention => C;  -- .\deps\raylib\src\raylib.h:874

      -- N-patch layout
      -- Npatch layout: 3x3 tiles
      -- Npatch layout: 1x3 tiles
      -- Npatch layout: 3x1 tiles
   type NPatchLayout is
     (NPATCH_NINE_PATCH, NPATCH_THREE_PATCH_VERTICAL,
      NPATCH_THREE_PATCH_HORIZONTAL) with
      Convention => C;  -- .\deps\raylib\src\raylib.h:881

      -- Callbacks to hook some internal functions
      -- WARNING: This callbacks are intended for advance users
      -- Logging: Redirect trace log messages
   type TraceLogCallback is access procedure
     (arg1 : int; arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : System.Address) with
      Convention => C;  -- .\deps\raylib\src\raylib.h:885

      -- FileIO: Load binary data
   type LoadFileDataCallback is access function
     (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : access unsigned)
      return access unsigned_char with
      Convention => C;  -- .\deps\raylib\src\raylib.h:886

      -- FileIO: Save binary data
   type SaveFileDataCallback is access function
     (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : System.Address;
      arg3 : unsigned) return Extensions.bool with
      Convention => C;  -- .\deps\raylib\src\raylib.h:887

      -- FileIO: Load text data
   type LoadFileTextCallback is access function
     (arg1 : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings.chars_ptr with
      Convention => C;  -- .\deps\raylib\src\raylib.h:888

      -- FileIO: Save text data
   type SaveFileTextCallback is access function
     (arg1 : Interfaces.C.Strings.chars_ptr;
      arg2 : Interfaces.C.Strings.chars_ptr) return Extensions.bool with
      Convention => C;  -- .\deps\raylib\src\raylib.h:889

      --------------------------------------------------------------------------------------
      -- Global Variables Definition
      --------------------------------------------------------------------------------------
      -- It's lonely here...
      --------------------------------------------------------------------------------------
      -- Window and Graphics Device Functions (Module: core)
      --------------------------------------------------------------------------------------
      -- Prevents name mangling of functions
      -- Window-related functions
      -- Initialize window and OpenGL context
   procedure InitWindow
     (width : int; height : int; title : Interfaces.C.Strings
        .chars_ptr)  -- .\deps\raylib\src\raylib.h:905
   with
      Import        => True,
      Convention    => C,
      External_Name => "InitWindow";

      -- Check if KEY_ESCAPE pressed or Close icon pressed
   function WindowShouldClose return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:906
   with
      Import        => True,
      Convention    => C,
      External_Name => "WindowShouldClose";

      -- Close window and unload OpenGL context
   procedure CloseWindow  -- .\deps\raylib\src\raylib.h:907
   with
      Import        => True,
      Convention    => C,
      External_Name => "CloseWindow";

      -- Check if window has been initialized successfully
   function IsWindowReady return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:908
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsWindowReady";

      -- Check if window is currently fullscreen
   function IsWindowFullscreen return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:909
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsWindowFullscreen";

      -- Check if window is currently hidden (only PLATFORM_DESKTOP)
   function IsWindowHidden return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:910
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsWindowHidden";

      -- Check if window is currently minimized (only PLATFORM_DESKTOP)
   function IsWindowMinimized return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:911
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsWindowMinimized";

      -- Check if window is currently maximized (only PLATFORM_DESKTOP)
   function IsWindowMaximized return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:912
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsWindowMaximized";

      -- Check if window is currently focused (only PLATFORM_DESKTOP)
   function IsWindowFocused return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:913
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsWindowFocused";

      -- Check if window has been resized last frame
   function IsWindowResized return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:914
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsWindowResized";

      -- Check if one specific window flag is enabled
   function IsWindowState
     (flag : unsigned) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:915
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsWindowState";

      -- Set window configuration state using flags
   procedure SetWindowState
     (flags : unsigned)  -- .\deps\raylib\src\raylib.h:916
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetWindowState";

      -- Clear window configuration state flags
   procedure ClearWindowState
     (flags : unsigned)  -- .\deps\raylib\src\raylib.h:917
   with
      Import        => True,
      Convention    => C,
      External_Name => "ClearWindowState";

      -- Toggle window state: fullscreen/windowed (only PLATFORM_DESKTOP)
   procedure ToggleFullscreen  -- .\deps\raylib\src\raylib.h:918
   with
      Import        => True,
      Convention    => C,
      External_Name => "ToggleFullscreen";

      -- Set window state: maximized, if resizable (only PLATFORM_DESKTOP)
   procedure MaximizeWindow  -- .\deps\raylib\src\raylib.h:919
   with
      Import        => True,
      Convention    => C,
      External_Name => "MaximizeWindow";

      -- Set window state: minimized, if resizable (only PLATFORM_DESKTOP)
   procedure MinimizeWindow  -- .\deps\raylib\src\raylib.h:920
   with
      Import        => True,
      Convention    => C,
      External_Name => "MinimizeWindow";

      -- Set window state: not minimized/maximized (only PLATFORM_DESKTOP)
   procedure RestoreWindow  -- .\deps\raylib\src\raylib.h:921
   with
      Import        => True,
      Convention    => C,
      External_Name => "RestoreWindow";

      -- Set icon for window (only PLATFORM_DESKTOP)
   procedure SetWindowIcon
     (the_image : Image)  -- .\deps\raylib\src\raylib.h:922
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetWindowIcon";

      -- Set title for window (only PLATFORM_DESKTOP)
   procedure SetWindowTitle
     (title : Interfaces.C.Strings
        .chars_ptr)  -- .\deps\raylib\src\raylib.h:923
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetWindowTitle";

      -- Set window position on screen (only PLATFORM_DESKTOP)
   procedure SetWindowPosition
     (x : int;
      y : int)  -- .\deps\raylib\src\raylib.h:924
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetWindowPosition";

      -- Set monitor for the current window (fullscreen mode)
   procedure SetWindowMonitor
     (monitor : int)  -- .\deps\raylib\src\raylib.h:925
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetWindowMonitor";

      -- Set window minimum dimensions (for FLAG_WINDOW_RESIZABLE)
   procedure SetWindowMinSize
     (width  : int;
      height : int)  -- .\deps\raylib\src\raylib.h:926
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetWindowMinSize";

      -- Set window dimensions
   procedure SetWindowSize
     (width  : int;
      height : int)  -- .\deps\raylib\src\raylib.h:927
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetWindowSize";

      -- Get native window handle
   function GetWindowHandle return System
     .Address  -- .\deps\raylib\src\raylib.h:928
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetWindowHandle";

      -- Get current screen width
   function GetScreenWidth
      return int  -- .\deps\raylib\src\raylib.h:929
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetScreenWidth";

      -- Get current screen height
   function GetScreenHeight
      return int  -- .\deps\raylib\src\raylib.h:930
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetScreenHeight";

      -- Get number of connected monitors
   function GetMonitorCount
      return int  -- .\deps\raylib\src\raylib.h:931
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetMonitorCount";

      -- Get current connected monitor
   function GetCurrentMonitor
      return int  -- .\deps\raylib\src\raylib.h:932
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetCurrentMonitor";

      -- Get specified monitor position
   function GetMonitorPosition
     (monitor : int)
      return Vector2  -- .\deps\raylib\src\raylib.h:933
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetMonitorPosition";

      -- Get specified monitor width (max available by monitor)
   function GetMonitorWidth
     (monitor : int)
      return int  -- .\deps\raylib\src\raylib.h:934
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetMonitorWidth";

      -- Get specified monitor height (max available by monitor)
   function GetMonitorHeight
     (monitor : int)
      return int  -- .\deps\raylib\src\raylib.h:935
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetMonitorHeight";

      -- Get specified monitor physical width in millimetres
   function GetMonitorPhysicalWidth
     (monitor : int)
      return int  -- .\deps\raylib\src\raylib.h:936
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetMonitorPhysicalWidth";

      -- Get specified monitor physical height in millimetres
   function GetMonitorPhysicalHeight
     (monitor : int)
      return int  -- .\deps\raylib\src\raylib.h:937
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetMonitorPhysicalHeight";

      -- Get specified monitor refresh rate
   function GetMonitorRefreshRate
     (monitor : int)
      return int  -- .\deps\raylib\src\raylib.h:938
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetMonitorRefreshRate";

      -- Get window position XY on monitor
   function GetWindowPosition
      return Vector2  -- .\deps\raylib\src\raylib.h:939
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetWindowPosition";

      -- Get window scale DPI factor
   function GetWindowScaleDPI
      return Vector2  -- .\deps\raylib\src\raylib.h:940
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetWindowScaleDPI";

      -- Get the human-readable, UTF-8 encoded name of the primary monitor
   function GetMonitorName
     (monitor : int) return Interfaces.C.Strings
     .chars_ptr  -- .\deps\raylib\src\raylib.h:941
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetMonitorName";

      -- Set clipboard text content
   procedure SetClipboardText
     (text : Interfaces.C.Strings
        .chars_ptr)  -- .\deps\raylib\src\raylib.h:942
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetClipboardText";

      -- Get clipboard text content
   function GetClipboardText return Interfaces.C.Strings
     .chars_ptr  -- .\deps\raylib\src\raylib.h:943
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetClipboardText";

      -- Custom frame control functions
      -- NOTE: Those functions are intended for advance users that want full control over the frame processing
      -- By default EndDrawing() does this job: draws everything + SwapScreenBuffer() + manage frame timming + PollInputEvents()
      -- To avoid that behaviour and control frame processes manually, enable in config.h: SUPPORT_CUSTOM_FRAME_CONTROL
      -- Swap back buffer with front buffer (screen drawing)
   procedure SwapScreenBuffer  -- .\deps\raylib\src\raylib.h:949
   with
      Import        => True,
      Convention    => C,
      External_Name => "SwapScreenBuffer";

      -- Register all input events
   procedure PollInputEvents  -- .\deps\raylib\src\raylib.h:950
   with
      Import        => True,
      Convention    => C,
      External_Name => "PollInputEvents";

      -- Wait for some milliseconds (halt program execution)
   procedure WaitTime
     (ms : Float)  -- .\deps\raylib\src\raylib.h:951
   with
      Import        => True,
      Convention    => C,
      External_Name => "WaitTime";

      -- Cursor-related functions
      -- Shows cursor
   procedure ShowCursor  -- .\deps\raylib\src\raylib.h:954
   with
      Import        => True,
      Convention    => C,
      External_Name => "ShowCursor";

      -- Hides cursor
   procedure HideCursor  -- .\deps\raylib\src\raylib.h:955
   with
      Import        => True,
      Convention    => C,
      External_Name => "HideCursor";

      -- Check if cursor is not visible
   function IsCursorHidden return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:956
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsCursorHidden";

      -- Enables cursor (unlock cursor)
   procedure EnableCursor  -- .\deps\raylib\src\raylib.h:957
   with
      Import        => True,
      Convention    => C,
      External_Name => "EnableCursor";

      -- Disables cursor (lock cursor)
   procedure DisableCursor  -- .\deps\raylib\src\raylib.h:958
   with
      Import        => True,
      Convention    => C,
      External_Name => "DisableCursor";

      -- Check if cursor is on the screen
   function IsCursorOnScreen return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:959
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsCursorOnScreen";

      -- Drawing-related functions
      -- Set background color (framebuffer clear color)
   procedure ClearBackground
     (the_color : Color)  -- .\deps\raylib\src\raylib.h:962
   with
      Import        => True,
      Convention    => C,
      External_Name => "ClearBackground";

      -- Setup canvas (framebuffer) to start drawing
   procedure BeginDrawing  -- .\deps\raylib\src\raylib.h:963
   with
      Import        => True,
      Convention    => C,
      External_Name => "BeginDrawing";

      -- End canvas drawing and swap buffers (double buffering)
   procedure EndDrawing  -- .\deps\raylib\src\raylib.h:964
   with
      Import        => True,
      Convention    => C,
      External_Name => "EndDrawing";

      -- Begin 2D mode with custom camera (2D)
   procedure BeginMode2D
     (camera : Camera2D)  -- .\deps\raylib\src\raylib.h:965
   with
      Import        => True,
      Convention    => C,
      External_Name => "BeginMode2D";

      -- Ends 2D mode with custom camera
   procedure EndMode2D  -- .\deps\raylib\src\raylib.h:966
   with
      Import        => True,
      Convention    => C,
      External_Name => "EndMode2D";

      -- Begin 3D mode with custom camera (3D)
   procedure BeginMode3D
     (camera : Camera3D)  -- .\deps\raylib\src\raylib.h:967
   with
      Import        => True,
      Convention    => C,
      External_Name => "BeginMode3D";

      -- Ends 3D mode and returns to default 2D orthographic mode
   procedure EndMode3D  -- .\deps\raylib\src\raylib.h:968
   with
      Import        => True,
      Convention    => C,
      External_Name => "EndMode3D";

      -- Begin drawing to render texture
   procedure BeginTextureMode
     (target : RenderTexture2D)  -- .\deps\raylib\src\raylib.h:969
   with
      Import        => True,
      Convention    => C,
      External_Name => "BeginTextureMode";

      -- Ends drawing to render texture
   procedure EndTextureMode  -- .\deps\raylib\src\raylib.h:970
   with
      Import        => True,
      Convention    => C,
      External_Name => "EndTextureMode";

      -- Begin custom shader drawing
   procedure BeginShaderMode
     (the_shader : Shader)  -- .\deps\raylib\src\raylib.h:971
   with
      Import        => True,
      Convention    => C,
      External_Name => "BeginShaderMode";

      -- End custom shader drawing (use default shader)
   procedure EndShaderMode  -- .\deps\raylib\src\raylib.h:972
   with
      Import        => True,
      Convention    => C,
      External_Name => "EndShaderMode";

      -- Begin blending mode (alpha, additive, multiplied, subtract, custom)
   procedure BeginBlendMode
     (mode : int)  -- .\deps\raylib\src\raylib.h:973
   with
      Import        => True,
      Convention    => C,
      External_Name => "BeginBlendMode";

      -- End blending mode (reset to default: alpha blending)
   procedure EndBlendMode  -- .\deps\raylib\src\raylib.h:974
   with
      Import        => True,
      Convention    => C,
      External_Name => "EndBlendMode";

      -- Begin scissor mode (define screen area for following drawing)
   procedure BeginScissorMode
     (x      : int;
      y      : int;
      width  : int;
      height : int)  -- .\deps\raylib\src\raylib.h:975
   with
      Import        => True,
      Convention    => C,
      External_Name => "BeginScissorMode";

      -- End scissor mode
   procedure EndScissorMode  -- .\deps\raylib\src\raylib.h:976
   with
      Import        => True,
      Convention    => C,
      External_Name => "EndScissorMode";

      -- Begin stereo rendering (requires VR simulator)
   procedure BeginVrStereoMode
     (config : VrStereoConfig)  -- .\deps\raylib\src\raylib.h:977
   with
      Import        => True,
      Convention    => C,
      External_Name => "BeginVrStereoMode";

      -- End stereo rendering (requires VR simulator)
   procedure EndVrStereoMode  -- .\deps\raylib\src\raylib.h:978
   with
      Import        => True,
      Convention    => C,
      External_Name => "EndVrStereoMode";

      -- VR stereo config functions for VR simulator
      -- Load VR stereo config for VR simulator device parameters
   function LoadVrStereoConfig
     (device : VrDeviceInfo)
      return VrStereoConfig  -- .\deps\raylib\src\raylib.h:981
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadVrStereoConfig";

      -- Unload VR stereo config
   procedure UnloadVrStereoConfig
     (config : VrStereoConfig)  -- .\deps\raylib\src\raylib.h:982
   with
      Import        => True,
      Convention    => C,
      External_Name => "UnloadVrStereoConfig";

      -- Shader management functions
      -- NOTE: Shader functionality is not available on OpenGL 1.1
      -- Load shader from files and bind default locations
   function LoadShader
     (vsFileName : Interfaces.C.Strings.chars_ptr;
      fsFileName : Interfaces.C.Strings.chars_ptr)
      return Shader  -- .\deps\raylib\src\raylib.h:986
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadShader";

      -- Load shader from code strings and bind default locations
   function LoadShaderFromMemory
     (vsCode : Interfaces.C.Strings.chars_ptr;
      fsCode : Interfaces.C.Strings.chars_ptr)
      return Shader  -- .\deps\raylib\src\raylib.h:987
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadShaderFromMemory";

      -- Get shader uniform location
   function GetShaderLocation
     (the_shader : Shader; uniformName : Interfaces.C.Strings.chars_ptr)
      return int  -- .\deps\raylib\src\raylib.h:988
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetShaderLocation";

      -- Get shader attribute location
   function GetShaderLocationAttrib
     (the_shader : Shader; attribName : Interfaces.C.Strings.chars_ptr)
      return int  -- .\deps\raylib\src\raylib.h:989
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetShaderLocationAttrib";

      -- Set shader uniform value
   procedure SetShaderValue
     (the_shader  : Shader; locIndex : int; value : System.Address;
      uniformType : int)  -- .\deps\raylib\src\raylib.h:990
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetShaderValue";

      -- Set shader uniform value vector
   procedure SetShaderValueV
     (the_shader  : Shader; locIndex : int; value : System.Address;
      uniformType : int;
      count       : int)  -- .\deps\raylib\src\raylib.h:991
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetShaderValueV";

      -- Set shader uniform value (matrix 4x4)
   procedure SetShaderValueMatrix
     (the_shader : Shader;
      locIndex   : int;
      mat        : Matrix)  -- .\deps\raylib\src\raylib.h:992
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetShaderValueMatrix";

      -- Set shader uniform value for texture (sampler2d)
   procedure SetShaderValueTexture
     (the_shader : Shader;
      locIndex   : int;
      texture    : Texture2D)  -- .\deps\raylib\src\raylib.h:993
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetShaderValueTexture";

      -- Unload shader from GPU memory (VRAM)
   procedure UnloadShader
     (the_shader : Shader)  -- .\deps\raylib\src\raylib.h:994
   with
      Import        => True,
      Convention    => C,
      External_Name => "UnloadShader";

      -- Screen-space-related functions
      -- Get a ray trace from mouse position
   function GetMouseRay
     (mousePosition : Vector2;
      the_camera    : Camera)
      return Ray  -- .\deps\raylib\src\raylib.h:997
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetMouseRay";

      -- Get camera transform matrix (view matrix)
   function GetCameraMatrix
     (the_camera : Camera)
      return Matrix  -- .\deps\raylib\src\raylib.h:998
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetCameraMatrix";

      -- Get camera 2d transform matrix
   function GetCameraMatrix2D
     (camera : Camera2D)
      return Matrix  -- .\deps\raylib\src\raylib.h:999
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetCameraMatrix2D";

      -- Get the screen space position for a 3d world space position
   function GetWorldToScreen
     (position   : Vector3;
      the_camera : Camera)
      return Vector2  -- .\deps\raylib\src\raylib.h:1000
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetWorldToScreen";

      -- Get size position for a 3d world space position
   function GetWorldToScreenEx
     (position : Vector3; the_camera : Camera; width : int; height : int)
      return Vector2  -- .\deps\raylib\src\raylib.h:1001
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetWorldToScreenEx";

      -- Get the screen space position for a 2d camera world space position
   function GetWorldToScreen2D
     (position : Vector2;
      camera   : Camera2D)
      return Vector2  -- .\deps\raylib\src\raylib.h:1002
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetWorldToScreen2D";

      -- Get the world space position for a 2d camera screen space position
   function GetScreenToWorld2D
     (position : Vector2;
      camera   : Camera2D)
      return Vector2  -- .\deps\raylib\src\raylib.h:1003
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetScreenToWorld2D";

      -- Timing-related functions
      -- Set target FPS (maximum)
   procedure SetTargetFPS
     (fps : int)  -- .\deps\raylib\src\raylib.h:1006
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetTargetFPS";

      -- Get current FPS
   function GetFPS
      return int  -- .\deps\raylib\src\raylib.h:1007
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetFPS";

      -- Get time in seconds for last frame drawn (delta time)
   function GetFrameTime
      return Float  -- .\deps\raylib\src\raylib.h:1008
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetFrameTime";

      -- Get elapsed time in seconds since InitWindow()
   function GetTime
      return double  -- .\deps\raylib\src\raylib.h:1009
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetTime";

      -- Misc. functions
      -- Get a random value between min and max (both included)
   function GetRandomValue
     (min : int;
      max : int)
      return int  -- .\deps\raylib\src\raylib.h:1012
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetRandomValue";

      -- Set the seed for the random number generator
   procedure SetRandomSeed
     (seed : unsigned)  -- .\deps\raylib\src\raylib.h:1013
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetRandomSeed";

   -- Takes a screenshot of current screen (filename extension defines format)
   procedure TakeScreenshot
     (fileName : Interfaces.C.Strings
        .chars_ptr)  -- .\deps\raylib\src\raylib.h:1014
   with
      Import        => True,
      Convention    => C,
      External_Name => "TakeScreenshot";

      -- Setup init configuration flags (view FLAGS)
   procedure SetConfigFlags
     (flags : unsigned)  -- .\deps\raylib\src\raylib.h:1015
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetConfigFlags";

   -- Show trace log messages (LOG_DEBUG, LOG_INFO, LOG_WARNING, LOG_ERROR...)
   procedure TraceLog
     (logLevel : int; text : Interfaces.C.Strings
        .chars_ptr  -- , ...
   )  -- .\deps\raylib\src\raylib.h:1017
   with
      Import        => True,
      Convention    => C,
      External_Name => "TraceLog";

      -- Set the current threshold (minimum) log level
   procedure SetTraceLogLevel
     (logLevel : int)  -- .\deps\raylib\src\raylib.h:1018
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetTraceLogLevel";

      -- Internal memory allocator
   function MemAlloc
     (size : int) return System
     .Address  -- .\deps\raylib\src\raylib.h:1019
   with
      Import        => True,
      Convention    => C,
      External_Name => "MemAlloc";

      -- Internal memory reallocator
   function MemRealloc
     (ptr : System.Address; size : int) return System
     .Address  -- .\deps\raylib\src\raylib.h:1020
   with
      Import        => True,
      Convention    => C,
      External_Name => "MemRealloc";

      -- Internal memory free
   procedure MemFree
     (ptr : System
        .Address)  -- .\deps\raylib\src\raylib.h:1021
   with
      Import        => True,
      Convention    => C,
      External_Name => "MemFree";

      -- Set custom callbacks
      -- WARNING: Callbacks setup is intended for advance users
      -- Set custom trace log
   procedure SetTraceLogCallback
     (callback : TraceLogCallback)  -- .\deps\raylib\src\raylib.h:1025
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetTraceLogCallback";

      -- Set custom file binary data loader
   procedure SetLoadFileDataCallback
     (callback : LoadFileDataCallback)  -- .\deps\raylib\src\raylib.h:1026
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetLoadFileDataCallback";

      -- Set custom file binary data saver
   procedure SetSaveFileDataCallback
     (callback : SaveFileDataCallback)  -- .\deps\raylib\src\raylib.h:1027
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetSaveFileDataCallback";

      -- Set custom file text data loader
   procedure SetLoadFileTextCallback
     (callback : LoadFileTextCallback)  -- .\deps\raylib\src\raylib.h:1028
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetLoadFileTextCallback";

      -- Set custom file text data saver
   procedure SetSaveFileTextCallback
     (callback : SaveFileTextCallback)  -- .\deps\raylib\src\raylib.h:1029
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetSaveFileTextCallback";

      -- Files management functions
      -- Load file data as byte array (read)
   function LoadFileData
     (fileName : Interfaces.C.Strings.chars_ptr; bytesRead : access unsigned)
      return access unsigned_char  -- .\deps\raylib\src\raylib.h:1032
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadFileData";

      -- Unload file data allocated by LoadFileData()
   procedure UnloadFileData
     (data : access unsigned_char)  -- .\deps\raylib\src\raylib.h:1033
   with
      Import        => True,
      Convention    => C,
      External_Name => "UnloadFileData";

      -- Save data to file from byte array (write), returns true on success
   function SaveFileData
     (fileName     : Interfaces.C.Strings.chars_ptr; data : System.Address;
      bytesToWrite : unsigned) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1034
   with
      Import        => True,
      Convention    => C,
      External_Name => "SaveFileData";

      -- Load text data from file (read), returns a '\0' terminated string
   function LoadFileText
     (fileName : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings
     .chars_ptr  -- .\deps\raylib\src\raylib.h:1035
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadFileText";

      -- Unload file text data allocated by LoadFileText()
   procedure UnloadFileText
     (text : Interfaces.C.Strings
        .chars_ptr)  -- .\deps\raylib\src\raylib.h:1036
   with
      Import        => True,
      Convention    => C,
      External_Name => "UnloadFileText";

      -- Save text data to file (write), string must be '\0' terminated, returns true on success
   function SaveFileText
     (fileName : Interfaces.C.Strings.chars_ptr;
      text     : Interfaces.C.Strings.chars_ptr) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1037
   with
      Import        => True,
      Convention    => C,
      External_Name => "SaveFileText";

      -- Check if file exists
   function FileExists
     (fileName : Interfaces.C.Strings.chars_ptr) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1038
   with
      Import        => True,
      Convention    => C,
      External_Name => "FileExists";

      -- Check if a directory path exists
   function DirectoryExists
     (dirPath : Interfaces.C.Strings.chars_ptr) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1039
   with
      Import        => True,
      Convention    => C,
      External_Name => "DirectoryExists";

      -- Check file extension (including point: .png, .wav)
   function IsFileExtension
     (fileName : Interfaces.C.Strings.chars_ptr;
      ext      : Interfaces.C.Strings.chars_ptr) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1040
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsFileExtension";

      -- Get pointer to extension for a filename string (includes dot: '.png')
   function GetFileExtension
     (fileName : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings
     .chars_ptr  -- .\deps\raylib\src\raylib.h:1041
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetFileExtension";

      -- Get pointer to filename for a path string
   function GetFileName
     (filePath : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings
     .chars_ptr  -- .\deps\raylib\src\raylib.h:1042
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetFileName";

      -- Get filename string without extension (uses static string)
   function GetFileNameWithoutExt
     (filePath : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings
     .chars_ptr  -- .\deps\raylib\src\raylib.h:1043
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetFileNameWithoutExt";

      -- Get full path for a given fileName with path (uses static string)
   function GetDirectoryPath
     (filePath : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings
     .chars_ptr  -- .\deps\raylib\src\raylib.h:1044
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetDirectoryPath";

      -- Get previous directory path for a given path (uses static string)
   function GetPrevDirectoryPath
     (dirPath : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings
     .chars_ptr  -- .\deps\raylib\src\raylib.h:1045
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetPrevDirectoryPath";

      -- Get current working directory (uses static string)
   function GetWorkingDirectory return Interfaces.C.Strings
     .chars_ptr  -- .\deps\raylib\src\raylib.h:1046
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetWorkingDirectory";

      -- Get filenames in a directory path (memory should be freed)
   function GetDirectoryFiles
     (dirPath : Interfaces.C.Strings.chars_ptr; count : access int)
      return System
     .Address  -- .\deps\raylib\src\raylib.h:1047
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetDirectoryFiles";

      -- Clear directory files paths buffers (free memory)
   procedure ClearDirectoryFiles  -- .\deps\raylib\src\raylib.h:1048
   with
      Import        => True,
      Convention    => C,
      External_Name => "ClearDirectoryFiles";

      -- Change working directory, return true on success
   function ChangeDirectory
     (dir : Interfaces.C.Strings.chars_ptr) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1049
   with
      Import        => True,
      Convention    => C,
      External_Name => "ChangeDirectory";

      -- Check if a file has been dropped into window
   function IsFileDropped return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1050
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsFileDropped";

      -- Get dropped files names (memory should be freed)
   function GetDroppedFiles
     (count : access int) return System
     .Address  -- .\deps\raylib\src\raylib.h:1051
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetDroppedFiles";

      -- Clear dropped files paths buffer (free memory)
   procedure ClearDroppedFiles  -- .\deps\raylib\src\raylib.h:1052
   with
      Import        => True,
      Convention    => C,
      External_Name => "ClearDroppedFiles";

      -- Get file modification time (last write time)
   function GetFileModTime
     (fileName : Interfaces.C.Strings.chars_ptr)
      return long  -- .\deps\raylib\src\raylib.h:1053
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetFileModTime";

      -- Compression/Encoding functionality
      -- Compress data (DEFLATE algorithm)
   function CompressData
     (data           : access unsigned_char; dataLength : int;
      compDataLength : access int)
      return access unsigned_char  -- .\deps\raylib\src\raylib.h:1056
   with
      Import        => True,
      Convention    => C,
      External_Name => "CompressData";

      -- Decompress data (DEFLATE algorithm)
   function DecompressData
     (compData   : access unsigned_char; compDataLength : int;
      dataLength : access int)
      return access unsigned_char  -- .\deps\raylib\src\raylib.h:1057
   with
      Import        => True,
      Convention    => C,
      External_Name => "DecompressData";

      -- Encode data to Base64 string
   function EncodeDataBase64
     (data : access unsigned_char; dataLength : int; outputLength : access int)
      return Interfaces.C.Strings
     .chars_ptr  -- .\deps\raylib\src\raylib.h:1058
   with
      Import        => True,
      Convention    => C,
      External_Name => "EncodeDataBase64";

      -- Decode Base64 string data
   function DecodeDataBase64
     (data : access unsigned_char; outputLength : access int)
      return access unsigned_char  -- .\deps\raylib\src\raylib.h:1059
   with
      Import        => True,
      Convention    => C,
      External_Name => "DecodeDataBase64";

      -- Persistent storage management
      -- Save integer value to storage file (to defined position), returns true on success
   function SaveStorageValue
     (position : unsigned; value : int) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1062
   with
      Import        => True,
      Convention    => C,
      External_Name => "SaveStorageValue";

      -- Load integer value from storage file (from defined position)
   function LoadStorageValue
     (position : unsigned)
      return int  -- .\deps\raylib\src\raylib.h:1063
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadStorageValue";

      -- Open URL with default system browser (if available)
   procedure OpenURL
     (url : Interfaces.C.Strings
        .chars_ptr)  -- .\deps\raylib\src\raylib.h:1065
   with
      Import        => True,
      Convention    => C,
      External_Name => "OpenURL";

      --------------------------------------------------------------------------------------
      -- Input Handling Functions (Module: core)
      --------------------------------------------------------------------------------------
      -- Input-related functions: keyboard
      -- Check if a key has been pressed once
   function IsKeyPressed
     (key : int) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1072
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsKeyPressed";

      -- Check if a key is being pressed
   function IsKeyDown
     (key : int) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1073
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsKeyDown";

      -- Check if a key has been released once
   function IsKeyReleased
     (key : int) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1074
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsKeyReleased";

      -- Check if a key is NOT being pressed
   function IsKeyUp
     (key : int) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1075
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsKeyUp";

      -- Set a custom key to exit program (default is ESC)
   procedure SetExitKey
     (key : int)  -- .\deps\raylib\src\raylib.h:1076
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetExitKey";

      -- Get key pressed (keycode), call it multiple times for keys queued, returns 0 when the queue is empty
   function GetKeyPressed
      return int  -- .\deps\raylib\src\raylib.h:1077
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetKeyPressed";

      -- Get char pressed (unicode), call it multiple times for chars queued, returns 0 when the queue is empty
   function GetCharPressed
      return int  -- .\deps\raylib\src\raylib.h:1078
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetCharPressed";

      -- Input-related functions: gamepads
      -- Check if a gamepad is available
   function IsGamepadAvailable
     (gamepad : int) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1081
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsGamepadAvailable";

      -- Get gamepad internal name id
   function GetGamepadName
     (gamepad : int) return Interfaces.C.Strings
     .chars_ptr  -- .\deps\raylib\src\raylib.h:1082
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetGamepadName";

      -- Check if a gamepad button has been pressed once
   function IsGamepadButtonPressed
     (gamepad : int; button : int) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1083
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsGamepadButtonPressed";

      -- Check if a gamepad button is being pressed
   function IsGamepadButtonDown
     (gamepad : int; button : int) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1084
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsGamepadButtonDown";

      -- Check if a gamepad button has been released once
   function IsGamepadButtonReleased
     (gamepad : int; button : int) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1085
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsGamepadButtonReleased";

      -- Check if a gamepad button is NOT being pressed
   function IsGamepadButtonUp
     (gamepad : int; button : int) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1086
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsGamepadButtonUp";

      -- Get the last gamepad button pressed
   function GetGamepadButtonPressed
      return int  -- .\deps\raylib\src\raylib.h:1087
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetGamepadButtonPressed";

      -- Get gamepad axis count for a gamepad
   function GetGamepadAxisCount
     (gamepad : int)
      return int  -- .\deps\raylib\src\raylib.h:1088
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetGamepadAxisCount";

      -- Get axis movement value for a gamepad axis
   function GetGamepadAxisMovement
     (gamepad : int;
      axis    : int)
      return Float  -- .\deps\raylib\src\raylib.h:1089
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetGamepadAxisMovement";

      -- Set internal gamepad mappings (SDL_GameControllerDB)
   function SetGamepadMappings
     (mappings : Interfaces.C.Strings.chars_ptr)
      return int  -- .\deps\raylib\src\raylib.h:1090
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetGamepadMappings";

      -- Input-related functions: mouse
      -- Check if a mouse button has been pressed once
   function IsMouseButtonPressed
     (button : int) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1093
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsMouseButtonPressed";

      -- Check if a mouse button is being pressed
   function IsMouseButtonDown
     (button : int) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1094
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsMouseButtonDown";

      -- Check if a mouse button has been released once
   function IsMouseButtonReleased
     (button : int) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1095
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsMouseButtonReleased";

      -- Check if a mouse button is NOT being pressed
   function IsMouseButtonUp
     (button : int) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1096
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsMouseButtonUp";

      -- Get mouse position X
   function GetMouseX
      return int  -- .\deps\raylib\src\raylib.h:1097
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetMouseX";

      -- Get mouse position Y
   function GetMouseY
      return int  -- .\deps\raylib\src\raylib.h:1098
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetMouseY";

      -- Get mouse position XY
   function GetMousePosition
      return Vector2  -- .\deps\raylib\src\raylib.h:1099
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetMousePosition";

      -- Get mouse delta between frames
   function GetMouseDelta
      return Vector2  -- .\deps\raylib\src\raylib.h:1100
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetMouseDelta";

      -- Set mouse position XY
   procedure SetMousePosition
     (x : int;
      y : int)  -- .\deps\raylib\src\raylib.h:1101
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetMousePosition";

      -- Set mouse offset
   procedure SetMouseOffset
     (offsetX : int;
      offsetY : int)  -- .\deps\raylib\src\raylib.h:1102
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetMouseOffset";

      -- Set mouse scaling
   procedure SetMouseScale
     (scaleX : Float;
      scaleY : Float)  -- .\deps\raylib\src\raylib.h:1103
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetMouseScale";

      -- Get mouse wheel movement Y
   function GetMouseWheelMove
      return Float  -- .\deps\raylib\src\raylib.h:1104
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetMouseWheelMove";

      -- Set mouse cursor
   procedure SetMouseCursor
     (cursor : int)  -- .\deps\raylib\src\raylib.h:1105
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetMouseCursor";

      -- Input-related functions: touch
      -- Get touch position X for touch point 0 (relative to screen size)
   function GetTouchX
      return int  -- .\deps\raylib\src\raylib.h:1108
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetTouchX";

      -- Get touch position Y for touch point 0 (relative to screen size)
   function GetTouchY
      return int  -- .\deps\raylib\src\raylib.h:1109
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetTouchY";

   -- Get touch position XY for a touch point index (relative to screen size)
   function GetTouchPosition
     (index : int)
      return Vector2  -- .\deps\raylib\src\raylib.h:1110
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetTouchPosition";

      -- Get touch point identifier for given index
   function GetTouchPointId
     (index : int)
      return int  -- .\deps\raylib\src\raylib.h:1111
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetTouchPointId";

      -- Get number of touch points
   function GetTouchPointCount
      return int  -- .\deps\raylib\src\raylib.h:1112
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetTouchPointCount";

      --------------------------------------------------------------------------------------
      -- Gestures and Touch Handling Functions (Module: rgestures)
      --------------------------------------------------------------------------------------
      -- Enable a set of gestures using flags
   procedure SetGesturesEnabled
     (flags : unsigned)  -- .\deps\raylib\src\raylib.h:1117
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetGesturesEnabled";

      -- Check if a gesture have been detected
   function IsGestureDetected
     (gesture : int) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1118
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsGestureDetected";

      -- Get latest detected gesture
   function GetGestureDetected
      return int  -- .\deps\raylib\src\raylib.h:1119
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetGestureDetected";

      -- Get gesture hold time in milliseconds
   function GetGestureHoldDuration
      return Float  -- .\deps\raylib\src\raylib.h:1120
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetGestureHoldDuration";

      -- Get gesture drag vector
   function GetGestureDragVector
      return Vector2  -- .\deps\raylib\src\raylib.h:1121
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetGestureDragVector";

      -- Get gesture drag angle
   function GetGestureDragAngle
      return Float  -- .\deps\raylib\src\raylib.h:1122
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetGestureDragAngle";

      -- Get gesture pinch delta
   function GetGesturePinchVector
      return Vector2  -- .\deps\raylib\src\raylib.h:1123
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetGesturePinchVector";

      -- Get gesture pinch angle
   function GetGesturePinchAngle
      return Float  -- .\deps\raylib\src\raylib.h:1124
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetGesturePinchAngle";

      --------------------------------------------------------------------------------------
      -- Camera System Functions (Module: rcamera)
      --------------------------------------------------------------------------------------
      -- Set camera mode (multiple camera modes available)
   procedure SetCameraMode
     (the_camera : Camera;
      mode       : int)  -- .\deps\raylib\src\raylib.h:1129
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetCameraMode";

      -- Update camera position for selected mode
   procedure UpdateCamera
     (the_camera : access Camera)  -- .\deps\raylib\src\raylib.h:1130
   with
      Import        => True,
      Convention    => C,
      External_Name => "UpdateCamera";

      -- Set camera pan key to combine with mouse movement (free camera)
   procedure SetCameraPanControl
     (keyPan : int)  -- .\deps\raylib\src\raylib.h:1132
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetCameraPanControl";

      -- Set camera alt key to combine with mouse movement (free camera)
   procedure SetCameraAltControl
     (keyAlt : int)  -- .\deps\raylib\src\raylib.h:1133
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetCameraAltControl";

      -- Set camera smooth zoom key to combine with mouse (free camera)
   procedure SetCameraSmoothZoomControl
     (keySmoothZoom : int)  -- .\deps\raylib\src\raylib.h:1134
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetCameraSmoothZoomControl";

      -- Set camera move controls (1st person and 3rd person cameras)
   procedure SetCameraMoveControls
     (keyFront : int; keyBack : int; keyRight : int; keyLeft : int;
      keyUp    : int;
      keyDown  : int)  -- .\deps\raylib\src\raylib.h:1135
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetCameraMoveControls";

      --------------------------------------------------------------------------------------
      -- Basic Shapes Drawing Functions (Module: shapes)
      --------------------------------------------------------------------------------------
      -- Set texture and rectangle to be used on shapes drawing
      -- NOTE: It can be useful when using basic shapes and one single font,
      -- defining a font char white rectangle would allow drawing everything in a single draw call
      -- Set texture and rectangle to be used on shapes drawing
   procedure SetShapesTexture
     (texture : Texture2D;
      source  : Rectangle)  -- .\deps\raylib\src\raylib.h:1143
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetShapesTexture";

      -- Basic shapes drawing functions
      -- Draw a pixel
   procedure DrawPixel
     (posX      : int;
      posY      : int;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1146
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawPixel";

      -- Draw a pixel (Vector version)
   procedure DrawPixelV
     (position  : Vector2;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1147
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawPixelV";

      -- Draw a line
   procedure DrawLine
     (startPosX : int; startPosY : int; endPosX : int; endPosY : int;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1148
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawLine";

      -- Draw a line (Vector version)
   procedure DrawLineV
     (startPos  : Vector2;
      endPos    : Vector2;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1149
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawLineV";

      -- Draw a line defining thickness
   procedure DrawLineEx
     (startPos  : Vector2; endPos : Vector2; thick : Float;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1150
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawLineEx";

      -- Draw a line using cubic-bezier curves in-out
   procedure DrawLineBezier
     (startPos  : Vector2; endPos : Vector2; thick : Float;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1151
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawLineBezier";

      -- Draw line using quadratic bezier curves with a control point
   procedure DrawLineBezierQuad
     (startPos  : Vector2; endPos : Vector2; controlPos : Vector2;
      thick     : Float;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1152
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawLineBezierQuad";

      -- Draw line using cubic bezier curves with 2 control points
   procedure DrawLineBezierCubic
     (startPos      : Vector2; endPos : Vector2; startControlPos : Vector2;
      endControlPos : Vector2;
      thick         : Float;
      the_color     : Color)  -- .\deps\raylib\src\raylib.h:1153
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawLineBezierCubic";

      -- Draw lines sequence
   procedure DrawLineStrip
     (points     : access Vector2;
      pointCount : int;
      the_color  : Color)  -- .\deps\raylib\src\raylib.h:1154
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawLineStrip";

      -- Draw a color-filled circle
   procedure DrawCircle
     (centerX   : int;
      centerY   : int;
      radius    : Float;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1155
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawCircle";

      -- Draw a piece of a circle
   procedure DrawCircleSector
     (center : Vector2; radius : Float; startAngle : Float; endAngle : Float;
      segments  : int;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1156
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawCircleSector";

      -- Draw circle sector outline
   procedure DrawCircleSectorLines
     (center : Vector2; radius : Float; startAngle : Float; endAngle : Float;
      segments  : int;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1157
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawCircleSectorLines";

      -- Draw a gradient-filled circle
   procedure DrawCircleGradient
     (centerX : int; centerY : int; radius : Float; color1 : Color;
      color2  : Color)  -- .\deps\raylib\src\raylib.h:1158
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawCircleGradient";

      -- Draw a color-filled circle (Vector version)
   procedure DrawCircleV
     (center    : Vector2;
      radius    : Float;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1159
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawCircleV";

      -- Draw circle outline
   procedure DrawCircleLines
     (centerX   : int;
      centerY   : int;
      radius    : Float;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1160
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawCircleLines";

      -- Draw ellipse
   procedure DrawEllipse
     (centerX   : int; centerY : int; radiusH : Float; radiusV : Float;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1161
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawEllipse";

      -- Draw ellipse outline
   procedure DrawEllipseLines
     (centerX   : int; centerY : int; radiusH : Float; radiusV : Float;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1162
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawEllipseLines";

      -- Draw ring
   procedure DrawRing
     (center     : Vector2; innerRadius : Float; outerRadius : Float;
      startAngle : Float; endAngle : Float; segments : int;
      the_color  : Color)  -- .\deps\raylib\src\raylib.h:1163
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawRing";

      -- Draw ring outline
   procedure DrawRingLines
     (center     : Vector2; innerRadius : Float; outerRadius : Float;
      startAngle : Float; endAngle : Float; segments : int;
      the_color  : Color)  -- .\deps\raylib\src\raylib.h:1164
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawRingLines";

      -- Draw a color-filled rectangle
   procedure DrawRectangle
     (posX      : int; posY : int; width : int; height : int;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1165
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawRectangle";

      -- Draw a color-filled rectangle (Vector version)
   procedure DrawRectangleV
     (position  : Vector2;
      size      : Vector2;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1166
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawRectangleV";

      -- Draw a color-filled rectangle
   procedure DrawRectangleRec
     (rec       : Rectangle;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1167
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawRectangleRec";

      -- Draw a color-filled rectangle with pro parameters
   procedure DrawRectanglePro
     (rec       : Rectangle; origin : Vector2; rotation : Float;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1168
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawRectanglePro";

      -- Draw a vertical-gradient-filled rectangle
   procedure DrawRectangleGradientV
     (posX   : int; posY : int; width : int; height : int; color1 : Color;
      color2 : Color)  -- .\deps\raylib\src\raylib.h:1169
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawRectangleGradientV";

      -- Draw a horizontal-gradient-filled rectangle
   procedure DrawRectangleGradientH
     (posX   : int; posY : int; width : int; height : int; color1 : Color;
      color2 : Color)  -- .\deps\raylib\src\raylib.h:1170
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawRectangleGradientH";

      -- Draw a gradient-filled rectangle with custom vertex colors
   procedure DrawRectangleGradientEx
     (rec  : Rectangle; col1 : Color; col2 : Color; col3 : Color;
      col4 : Color)  -- .\deps\raylib\src\raylib.h:1171
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawRectangleGradientEx";

      -- Draw rectangle outline
   procedure DrawRectangleLines
     (posX      : int; posY : int; width : int; height : int;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1172
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawRectangleLines";

      -- Draw rectangle outline with extended parameters
   procedure DrawRectangleLinesEx
     (rec       : Rectangle;
      lineThick : Float;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1173
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawRectangleLinesEx";

      -- Draw rectangle with rounded edges
   procedure DrawRectangleRounded
     (rec       : Rectangle; roundness : Float; segments : int;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1174
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawRectangleRounded";

      -- Draw rectangle with rounded edges outline
   procedure DrawRectangleRoundedLines
     (rec : Rectangle; roundness : Float; segments : int; lineThick : Float;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1175
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawRectangleRoundedLines";

      -- Draw a color-filled triangle (vertex in counter-clockwise order!)
   procedure DrawTriangle
     (v1        : Vector2;
      v2        : Vector2;
      v3        : Vector2;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1176
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawTriangle";

      -- Draw triangle outline (vertex in counter-clockwise order!)
   procedure DrawTriangleLines
     (v1        : Vector2;
      v2        : Vector2;
      v3        : Vector2;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1177
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawTriangleLines";

      -- Draw a triangle fan defined by points (first vertex is the center)
   procedure DrawTriangleFan
     (points     : access Vector2;
      pointCount : int;
      the_color  : Color)  -- .\deps\raylib\src\raylib.h:1178
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawTriangleFan";

      -- Draw a triangle strip defined by points
   procedure DrawTriangleStrip
     (points     : access Vector2;
      pointCount : int;
      the_color  : Color)  -- .\deps\raylib\src\raylib.h:1179
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawTriangleStrip";

      -- Draw a regular polygon (Vector version)
   procedure DrawPoly
     (center    : Vector2; sides : int; radius : Float; rotation : Float;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1180
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawPoly";

      -- Draw a polygon outline of n sides
   procedure DrawPolyLines
     (center    : Vector2; sides : int; radius : Float; rotation : Float;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1181
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawPolyLines";

      -- Draw a polygon outline of n sides with extended parameters
   procedure DrawPolyLinesEx
     (center    : Vector2; sides : int; radius : Float; rotation : Float;
      lineThick : Float;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1182
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawPolyLinesEx";

      -- Basic shapes collision detection functions
      -- Check collision between two rectangles
   function CheckCollisionRecs
     (rec1 : Rectangle; rec2 : Rectangle) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1185
   with
      Import        => True,
      Convention    => C,
      External_Name => "CheckCollisionRecs";

      -- Check collision between two circles
   function CheckCollisionCircles
     (center1 : Vector2; radius1 : Float; center2 : Vector2; radius2 : Float)
      return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1186
   with
      Import        => True,
      Convention    => C,
      External_Name => "CheckCollisionCircles";

      -- Check collision between circle and rectangle
   function CheckCollisionCircleRec
     (center : Vector2; radius : Float; rec : Rectangle)
      return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1187
   with
      Import        => True,
      Convention    => C,
      External_Name => "CheckCollisionCircleRec";

      -- Check if point is inside rectangle
   function CheckCollisionPointRec
     (point : Vector2; rec : Rectangle) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1188
   with
      Import        => True,
      Convention    => C,
      External_Name => "CheckCollisionPointRec";

      -- Check if point is inside circle
   function CheckCollisionPointCircle
     (point : Vector2; center : Vector2; radius : Float)
      return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1189
   with
      Import        => True,
      Convention    => C,
      External_Name => "CheckCollisionPointCircle";

      -- Check if point is inside a triangle
   function CheckCollisionPointTriangle
     (point : Vector2; p1 : Vector2; p2 : Vector2; p3 : Vector2)
      return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1190
   with
      Import        => True,
      Convention    => C,
      External_Name => "CheckCollisionPointTriangle";

      -- Check the collision between two lines defined by two points each, returns collision point by reference
   function CheckCollisionLines
     (startPos1 : Vector2; endPos1 : Vector2; startPos2 : Vector2;
      endPos2   : Vector2; collisionPoint : access Vector2)
      return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1191
   with
      Import        => True,
      Convention    => C,
      External_Name => "CheckCollisionLines";

      -- Check if point belongs to line created between two points [p1] and [p2] with defined margin in pixels [threshold]
   function CheckCollisionPointLine
     (point : Vector2; p1 : Vector2; p2 : Vector2; threshold : int)
      return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1192
   with
      Import        => True,
      Convention    => C,
      External_Name => "CheckCollisionPointLine";

      -- Get collision rectangle for two rectangles collision
   function GetCollisionRec
     (rec1 : Rectangle;
      rec2 : Rectangle)
      return Rectangle  -- .\deps\raylib\src\raylib.h:1193
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetCollisionRec";

      --------------------------------------------------------------------------------------
      -- Texture Loading and Drawing Functions (Module: textures)
      --------------------------------------------------------------------------------------
      -- Image loading functions
      -- NOTE: This functions do not require GPU access
      -- Load image from file into CPU memory (RAM)
   function LoadImage
     (fileName : Interfaces.C.Strings.chars_ptr)
      return Image  -- .\deps\raylib\src\raylib.h:1201
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadImage";

      -- Load image from RAW file data
   function LoadImageRaw
     (fileName   : Interfaces.C.Strings.chars_ptr; width : int; height : int;
      format     : int;
      headerSize : int)
      return Image  -- .\deps\raylib\src\raylib.h:1202
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadImageRaw";

      -- Load image sequence from file (frames appended to image.data)
   function LoadImageAnim
     (fileName : Interfaces.C.Strings.chars_ptr; frames : access int)
      return Image  -- .\deps\raylib\src\raylib.h:1203
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadImageAnim";

   -- Load image from memory buffer, fileType refers to extension: i.e. '.png'
   function LoadImageFromMemory
     (fileType : Interfaces.C.Strings.chars_ptr;
      fileData : access unsigned_char;
      dataSize : int)
      return Image  -- .\deps\raylib\src\raylib.h:1204
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadImageFromMemory";

      -- Load image from GPU texture data
   function LoadImageFromTexture
     (texture : Texture2D)
      return Image  -- .\deps\raylib\src\raylib.h:1205
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadImageFromTexture";

      -- Load image from screen buffer and (screenshot)
   function LoadImageFromScreen
      return Image  -- .\deps\raylib\src\raylib.h:1206
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadImageFromScreen";

      -- Unload image from CPU memory (RAM)
   procedure UnloadImage
     (the_image : Image)  -- .\deps\raylib\src\raylib.h:1207
   with
      Import        => True,
      Convention    => C,
      External_Name => "UnloadImage";

      -- Export image data to file, returns true on success
   function ExportImage
     (the_image : Image; fileName : Interfaces.C.Strings.chars_ptr)
      return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1208
   with
      Import        => True,
      Convention    => C,
      External_Name => "ExportImage";

      -- Export image as code file defining an array of bytes, returns true on success
   function ExportImageAsCode
     (the_image : Image; fileName : Interfaces.C.Strings.chars_ptr)
      return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1209
   with
      Import        => True,
      Convention    => C,
      External_Name => "ExportImageAsCode";

      -- Image generation functions
      -- Generate image: plain color
   function GenImageColor
     (width     : int;
      height    : int;
      the_color : Color)
      return Image  -- .\deps\raylib\src\raylib.h:1212
   with
      Import        => True,
      Convention    => C,
      External_Name => "GenImageColor";

      -- Generate image: vertical gradient
   function GenImageGradientV
     (width  : int;
      height : int;
      top    : Color;
      bottom : Color)
      return Image  -- .\deps\raylib\src\raylib.h:1213
   with
      Import        => True,
      Convention    => C,
      External_Name => "GenImageGradientV";

      -- Generate image: horizontal gradient
   function GenImageGradientH
     (width  : int;
      height : int;
      left   : Color;
      right  : Color)
      return Image  -- .\deps\raylib\src\raylib.h:1214
   with
      Import        => True,
      Convention    => C,
      External_Name => "GenImageGradientH";

      -- Generate image: radial gradient
   function GenImageGradientRadial
     (width : int; height : int; density : Float; inner : Color; outer : Color)
      return Image  -- .\deps\raylib\src\raylib.h:1215
   with
      Import        => True,
      Convention    => C,
      External_Name => "GenImageGradientRadial";

      -- Generate image: checked
   function GenImageChecked
     (width : int; height : int; checksX : int; checksY : int; col1 : Color;
      col2  : Color)
      return Image  -- .\deps\raylib\src\raylib.h:1216
   with
      Import        => True,
      Convention    => C,
      External_Name => "GenImageChecked";

      -- Generate image: white noise
   function GenImageWhiteNoise
     (width  : int;
      height : int;
      factor : Float)
      return Image  -- .\deps\raylib\src\raylib.h:1217
   with
      Import        => True,
      Convention    => C,
      External_Name => "GenImageWhiteNoise";

      -- Generate image: cellular algorithm, bigger tileSize means bigger cells
   function GenImageCellular
     (width    : int;
      height   : int;
      tileSize : int)
      return Image  -- .\deps\raylib\src\raylib.h:1218
   with
      Import        => True,
      Convention    => C,
      External_Name => "GenImageCellular";

      -- Image manipulation functions
      -- Create an image duplicate (useful for transformations)
   function ImageCopy
     (the_image : Image)
      return Image  -- .\deps\raylib\src\raylib.h:1221
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageCopy";

      -- Create an image from another image piece
   function ImageFromImage
     (the_image : Image;
      rec       : Rectangle)
      return Image  -- .\deps\raylib\src\raylib.h:1222
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageFromImage";

      -- Create an image from text (default font)
   function ImageText
     (text : Interfaces.C.Strings.chars_ptr; fontSize : int; the_color : Color)
      return Image  -- .\deps\raylib\src\raylib.h:1223
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageText";

      -- Create an image from text (custom sprite font)
   function ImageTextEx
     (the_font : Font; text : Interfaces.C.Strings.chars_ptr; fontSize : Float;
      spacing  : Float;
      tint     : Color)
      return Image  -- .\deps\raylib\src\raylib.h:1224
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageTextEx";

      -- Convert image data to desired format
   procedure ImageFormat
     (the_image : access Image;
      newFormat : int)  -- .\deps\raylib\src\raylib.h:1225
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageFormat";

      -- Convert image to POT (power-of-two)
   procedure ImageToPOT
     (the_image : access Image;
      fill      : Color)  -- .\deps\raylib\src\raylib.h:1226
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageToPOT";

      -- Crop an image to a defined rectangle
   procedure ImageCrop
     (the_image : access Image;
      crop      : Rectangle)  -- .\deps\raylib\src\raylib.h:1227
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageCrop";

      -- Crop image depending on alpha value
   procedure ImageAlphaCrop
     (the_image : access Image;
      threshold : Float)  -- .\deps\raylib\src\raylib.h:1228
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageAlphaCrop";

      -- Clear alpha channel to desired color
   procedure ImageAlphaClear
     (the_image : access Image;
      the_color : Color;
      threshold : Float)  -- .\deps\raylib\src\raylib.h:1229
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageAlphaClear";

      -- Apply alpha mask to image
   procedure ImageAlphaMask
     (the_image : access Image;
      alphaMask : Image)  -- .\deps\raylib\src\raylib.h:1230
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageAlphaMask";

      -- Premultiply alpha channel
   procedure ImageAlphaPremultiply
     (the_image : access Image)  -- .\deps\raylib\src\raylib.h:1231
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageAlphaPremultiply";

      -- Resize image (Bicubic scaling algorithm)
   procedure ImageResize
     (the_image : access Image;
      newWidth  : int;
      newHeight : int)  -- .\deps\raylib\src\raylib.h:1232
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageResize";

      -- Resize image (Nearest-Neighbor scaling algorithm)
   procedure ImageResizeNN
     (the_image : access Image;
      newWidth  : int;
      newHeight : int)  -- .\deps\raylib\src\raylib.h:1233
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageResizeNN";

      -- Resize canvas and fill with color
   procedure ImageResizeCanvas
     (the_image : access Image; newWidth : int; newHeight : int; offsetX : int;
      offsetY   : int;
      fill      : Color)  -- .\deps\raylib\src\raylib.h:1234
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageResizeCanvas";

      -- Compute all mipmap levels for a provided image
   procedure ImageMipmaps
     (the_image : access Image)  -- .\deps\raylib\src\raylib.h:1235
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageMipmaps";

      -- Dither image data to 16bpp or lower (Floyd-Steinberg dithering)
   procedure ImageDither
     (the_image : access Image; rBpp : int; gBpp : int; bBpp : int;
      aBpp      : int)  -- .\deps\raylib\src\raylib.h:1236
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageDither";

      -- Flip image vertically
   procedure ImageFlipVertical
     (the_image : access Image)  -- .\deps\raylib\src\raylib.h:1237
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageFlipVertical";

      -- Flip image horizontally
   procedure ImageFlipHorizontal
     (the_image : access Image)  -- .\deps\raylib\src\raylib.h:1238
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageFlipHorizontal";

      -- Rotate image clockwise 90deg
   procedure ImageRotateCW
     (the_image : access Image)  -- .\deps\raylib\src\raylib.h:1239
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageRotateCW";

      -- Rotate image counter-clockwise 90deg
   procedure ImageRotateCCW
     (the_image : access Image)  -- .\deps\raylib\src\raylib.h:1240
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageRotateCCW";

      -- Modify image color: tint
   procedure ImageColorTint
     (the_image : access Image;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1241
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageColorTint";

      -- Modify image color: invert
   procedure ImageColorInvert
     (the_image : access Image)  -- .\deps\raylib\src\raylib.h:1242
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageColorInvert";

      -- Modify image color: grayscale
   procedure ImageColorGrayscale
     (the_image : access Image)  -- .\deps\raylib\src\raylib.h:1243
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageColorGrayscale";

      -- Modify image color: contrast (-100 to 100)
   procedure ImageColorContrast
     (the_image : access Image;
      contrast  : Float)  -- .\deps\raylib\src\raylib.h:1244
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageColorContrast";

      -- Modify image color: brightness (-255 to 255)
   procedure ImageColorBrightness
     (the_image  : access Image;
      brightness : int)  -- .\deps\raylib\src\raylib.h:1245
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageColorBrightness";

      -- Modify image color: replace color
   procedure ImageColorReplace
     (the_image : access Image;
      the_color : Color;
      replace   : Color)  -- .\deps\raylib\src\raylib.h:1246
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageColorReplace";

      -- Load color data from image as a Color array (RGBA - 32bit)
   function LoadImageColors
     (the_image : Image)
      return access Color  -- .\deps\raylib\src\raylib.h:1247
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadImageColors";

      -- Load colors palette from image as a Color array (RGBA - 32bit)
   function LoadImagePalette
     (the_image : Image; maxPaletteSize : int; colorCount : access int)
      return access Color  -- .\deps\raylib\src\raylib.h:1248
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadImagePalette";

      -- Unload color data loaded with LoadImageColors()
   procedure UnloadImageColors
     (colors : access Color)  -- .\deps\raylib\src\raylib.h:1249
   with
      Import        => True,
      Convention    => C,
      External_Name => "UnloadImageColors";

      -- Unload colors palette loaded with LoadImagePalette()
   procedure UnloadImagePalette
     (colors : access Color)  -- .\deps\raylib\src\raylib.h:1250
   with
      Import        => True,
      Convention    => C,
      External_Name => "UnloadImagePalette";

      -- Get image alpha border rectangle
   function GetImageAlphaBorder
     (the_image : Image;
      threshold : Float)
      return Rectangle  -- .\deps\raylib\src\raylib.h:1251
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetImageAlphaBorder";

      -- Get image pixel color at (x, y) position
   function GetImageColor
     (the_image : Image;
      x         : int;
      y         : int)
      return Color  -- .\deps\raylib\src\raylib.h:1252
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetImageColor";

      -- Image drawing functions
      -- NOTE: Image software-rendering functions (CPU)
      -- Clear image background with given color
   procedure ImageClearBackground
     (dst       : access Image;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1256
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageClearBackground";

      -- Draw pixel within an image
   procedure ImageDrawPixel
     (dst       : access Image;
      posX      : int;
      posY      : int;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1257
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageDrawPixel";

      -- Draw pixel within an image (Vector version)
   procedure ImageDrawPixelV
     (dst       : access Image;
      position  : Vector2;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1258
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageDrawPixelV";

      -- Draw line within an image
   procedure ImageDrawLine
     (dst : access Image; startPosX : int; startPosY : int; endPosX : int;
      endPosY   : int;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1259
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageDrawLine";

      -- Draw line within an image (Vector version)
   procedure ImageDrawLineV
     (dst       : access Image; start : Vector2; c_end : Vector2;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1260
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageDrawLineV";

      -- Draw circle within an image
   procedure ImageDrawCircle
     (dst       : access Image; centerX : int; centerY : int; radius : int;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1261
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageDrawCircle";

      -- Draw circle within an image (Vector version)
   procedure ImageDrawCircleV
     (dst       : access Image; center : Vector2; radius : int;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1262
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageDrawCircleV";

      -- Draw rectangle within an image
   procedure ImageDrawRectangle
     (dst : access Image; posX : int; posY : int; width : int; height : int;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1263
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageDrawRectangle";

      -- Draw rectangle within an image (Vector version)
   procedure ImageDrawRectangleV
     (dst       : access Image; position : Vector2; size : Vector2;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1264
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageDrawRectangleV";

      -- Draw rectangle within an image
   procedure ImageDrawRectangleRec
     (dst       : access Image;
      rec       : Rectangle;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1265
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageDrawRectangleRec";

      -- Draw rectangle lines within an image
   procedure ImageDrawRectangleLines
     (dst       : access Image;
      rec       : Rectangle;
      thick     : int;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1266
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageDrawRectangleLines";

   -- Draw a source image within a destination image (tint applied to source)
   procedure ImageDraw
     (dst  : access Image; src : Image; srcRec : Rectangle; dstRec : Rectangle;
      tint : Color)  -- .\deps\raylib\src\raylib.h:1267
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageDraw";

      -- Draw text (using default font) within an image (destination)
   procedure ImageDrawText
     (dst : access Image; text : Interfaces.C.Strings.chars_ptr; posX : int;
      posY      : int;
      fontSize  : int;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1268
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageDrawText";

      -- Draw text (custom sprite font) within an image (destination)
   procedure ImageDrawTextEx
     (dst      : access Image; the_font : Font;
      text     : Interfaces.C.Strings.chars_ptr; position : Vector2;
      fontSize : Float;
      spacing  : Float;
      tint     : Color)  -- .\deps\raylib\src\raylib.h:1269
   with
      Import        => True,
      Convention    => C,
      External_Name => "ImageDrawTextEx";

      -- Texture loading functions
      -- NOTE: These functions require GPU access
      -- Load texture from file into GPU memory (VRAM)
   function LoadTexture
     (fileName : Interfaces.C.Strings.chars_ptr)
      return Texture2D  -- .\deps\raylib\src\raylib.h:1273
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadTexture";

      -- Load texture from image data
   function LoadTextureFromImage
     (the_image : Image)
      return Texture2D  -- .\deps\raylib\src\raylib.h:1274
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadTextureFromImage";

      -- Load cubemap from image, multiple image cubemap layouts supported
   function LoadTextureCubemap
     (the_image : Image;
      layout    : int)
      return TextureCubemap  -- .\deps\raylib\src\raylib.h:1275
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadTextureCubemap";

      -- Load texture for rendering (framebuffer)
   function LoadRenderTexture
     (width  : int;
      height : int)
      return RenderTexture2D  -- .\deps\raylib\src\raylib.h:1276
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadRenderTexture";

      -- Unload texture from GPU memory (VRAM)
   procedure UnloadTexture
     (texture : Texture2D)  -- .\deps\raylib\src\raylib.h:1277
   with
      Import        => True,
      Convention    => C,
      External_Name => "UnloadTexture";

      -- Unload render texture from GPU memory (VRAM)
   procedure UnloadRenderTexture
     (target : RenderTexture2D)  -- .\deps\raylib\src\raylib.h:1278
   with
      Import        => True,
      Convention    => C,
      External_Name => "UnloadRenderTexture";

      -- Update GPU texture with new data
   procedure UpdateTexture
     (texture : Texture2D; pixels : System
        .Address)  -- .\deps\raylib\src\raylib.h:1279
   with
      Import        => True,
      Convention    => C,
      External_Name => "UpdateTexture";

      -- Update GPU texture rectangle with new data
   procedure UpdateTextureRec
     (texture : Texture2D; rec : Rectangle; pixels : System
        .Address)  -- .\deps\raylib\src\raylib.h:1280
   with
      Import        => True,
      Convention    => C,
      External_Name => "UpdateTextureRec";

      -- Texture configuration functions
      -- Generate GPU mipmaps for a texture
   procedure GenTextureMipmaps
     (texture : access Texture2D)  -- .\deps\raylib\src\raylib.h:1283
   with
      Import        => True,
      Convention    => C,
      External_Name => "GenTextureMipmaps";

      -- Set texture scaling filter mode
   procedure SetTextureFilter
     (texture : Texture2D;
      filter  : int)  -- .\deps\raylib\src\raylib.h:1284
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetTextureFilter";

      -- Set texture wrapping mode
   procedure SetTextureWrap
     (texture : Texture2D;
      wrap    : int)  -- .\deps\raylib\src\raylib.h:1285
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetTextureWrap";

      -- Texture drawing functions
      -- Draw a Texture2D
   procedure DrawTexture
     (texture : Texture2D;
      posX    : int;
      posY    : int;
      tint    : Color)  -- .\deps\raylib\src\raylib.h:1288
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawTexture";

      -- Draw a Texture2D with position defined as Vector2
   procedure DrawTextureV
     (texture  : Texture2D;
      position : Vector2;
      tint     : Color)  -- .\deps\raylib\src\raylib.h:1289
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawTextureV";

      -- Draw a Texture2D with extended parameters
   procedure DrawTextureEx
     (texture : Texture2D; position : Vector2; rotation : Float; scale : Float;
      tint    : Color)  -- .\deps\raylib\src\raylib.h:1290
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawTextureEx";

      -- Draw a part of a texture defined by a rectangle
   procedure DrawTextureRec
     (texture : Texture2D; source : Rectangle; position : Vector2;
      tint    : Color)  -- .\deps\raylib\src\raylib.h:1291
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawTextureRec";

      -- Draw texture quad with tiling and offset parameters
   procedure DrawTextureQuad
     (texture : Texture2D; tiling : Vector2; offset : Vector2;
      quad    : Rectangle;
      tint    : Color)  -- .\deps\raylib\src\raylib.h:1292
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawTextureQuad";

      -- Draw part of a texture (defined by a rectangle) with rotation and scale tiled into dest.
   procedure DrawTextureTiled
     (texture  : Texture2D; source : Rectangle; dest : Rectangle;
      origin   : Vector2;
      rotation : Float;
      scale    : Float;
      tint     : Color)  -- .\deps\raylib\src\raylib.h:1293
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawTextureTiled";

      -- Draw a part of a texture defined by a rectangle with 'pro' parameters
   procedure DrawTexturePro
     (texture  : Texture2D; source : Rectangle; dest : Rectangle;
      origin   : Vector2;
      rotation : Float;
      tint     : Color)  -- .\deps\raylib\src\raylib.h:1294
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawTexturePro";

      -- Draws a texture (or part of it) that stretches or shrinks nicely
   procedure DrawTextureNPatch
     (texture  : Texture2D; the_nPatchInfo : NPatchInfo; dest : Rectangle;
      origin   : Vector2;
      rotation : Float;
      tint     : Color)  -- .\deps\raylib\src\raylib.h:1295
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawTextureNPatch";

      -- Draw a textured polygon
   procedure DrawTexturePoly
     (texture    : Texture2D; center : Vector2; points : access Vector2;
      texcoords  : access Vector2;
      pointCount : int;
      tint       : Color)  -- .\deps\raylib\src\raylib.h:1296
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawTexturePoly";

      -- Color/pixel related functions
      -- Get color with alpha applied, alpha goes from 0.0f to 1.0f
   function Fade
     (the_color : Color;
      alpha     : Float)
      return Color  -- .\deps\raylib\src\raylib.h:1299
   with
      Import        => True,
      Convention    => C,
      External_Name => "Fade";

      -- Get hexadecimal value for a Color
   function ColorToInt
     (the_color : Color)
      return int  -- .\deps\raylib\src\raylib.h:1300
   with
      Import        => True,
      Convention    => C,
      External_Name => "ColorToInt";

      -- Get Color normalized as float [0..1]
   function ColorNormalize
     (the_color : Color)
      return Vector4  -- .\deps\raylib\src\raylib.h:1301
   with
      Import        => True,
      Convention    => C,
      External_Name => "ColorNormalize";

      -- Get Color from normalized values [0..1]
   function ColorFromNormalized
     (normalized : Vector4)
      return Color  -- .\deps\raylib\src\raylib.h:1302
   with
      Import        => True,
      Convention    => C,
      External_Name => "ColorFromNormalized";

      -- Get HSV values for a Color, hue [0..360], saturation/value [0..1]
   function ColorToHSV
     (the_color : Color)
      return Vector3  -- .\deps\raylib\src\raylib.h:1303
   with
      Import        => True,
      Convention    => C,
      External_Name => "ColorToHSV";

      -- Get a Color from HSV values, hue [0..360], saturation/value [0..1]
   function ColorFromHSV
     (hue        : Float;
      saturation : Float;
      value      : Float)
      return Color  -- .\deps\raylib\src\raylib.h:1304
   with
      Import        => True,
      Convention    => C,
      External_Name => "ColorFromHSV";

      -- Get color with alpha applied, alpha goes from 0.0f to 1.0f
   function ColorAlpha
     (the_color : Color;
      alpha     : Float)
      return Color  -- .\deps\raylib\src\raylib.h:1305
   with
      Import        => True,
      Convention    => C,
      External_Name => "ColorAlpha";

      -- Get src alpha-blended into dst color with tint
   function ColorAlphaBlend
     (dst  : Color;
      src  : Color;
      tint : Color)
      return Color  -- .\deps\raylib\src\raylib.h:1306
   with
      Import        => True,
      Convention    => C,
      External_Name => "ColorAlphaBlend";

      -- Get Color structure from hexadecimal value
   function GetColor
     (hexValue : unsigned)
      return Color  -- .\deps\raylib\src\raylib.h:1307
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetColor";

      -- Get Color from a source pixel pointer of certain format
   function GetPixelColor
     (srcPtr : System.Address;
      format : int)
      return Color  -- .\deps\raylib\src\raylib.h:1308
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetPixelColor";

      -- Set color formatted into destination pixel pointer
   procedure SetPixelColor
     (dstPtr    : System.Address;
      the_color : Color;
      format    : int)  -- .\deps\raylib\src\raylib.h:1309
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetPixelColor";

      -- Get pixel data size in bytes for certain format
   function GetPixelDataSize
     (width  : int;
      height : int;
      format : int)
      return int  -- .\deps\raylib\src\raylib.h:1310
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetPixelDataSize";

      --------------------------------------------------------------------------------------
      -- Font Loading and Text Drawing Functions (Module: text)
      --------------------------------------------------------------------------------------
      -- Font loading/unloading functions
      -- Get the default Font
   function GetFontDefault
      return Font  -- .\deps\raylib\src\raylib.h:1317
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetFontDefault";

      -- Load font from file into GPU memory (VRAM)
   function LoadFont
     (fileName : Interfaces.C.Strings.chars_ptr)
      return Font  -- .\deps\raylib\src\raylib.h:1318
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadFont";

      -- Load font from file with extended parameters
   function LoadFontEx
     (fileName   : Interfaces.C.Strings.chars_ptr; fontSize : int;
      fontChars  : access int;
      glyphCount : int)
      return Font  -- .\deps\raylib\src\raylib.h:1319
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadFontEx";

      -- Load font from Image (XNA style)
   function LoadFontFromImage
     (the_image : Image;
      key       : Color;
      firstChar : int)
      return Font  -- .\deps\raylib\src\raylib.h:1320
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadFontFromImage";

   -- Load font from memory buffer, fileType refers to extension: i.e. '.ttf'
   function LoadFontFromMemory
     (fileType   : Interfaces.C.Strings.chars_ptr;
      fileData   : access unsigned_char; dataSize : int; fontSize : int;
      fontChars  : access int;
      glyphCount : int)
      return Font  -- .\deps\raylib\src\raylib.h:1321
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadFontFromMemory";

      -- Load font data for further use
   function LoadFontData
     (fileData  : access unsigned_char; dataSize : int; fontSize : int;
      fontChars : access int; glyphCount : int; c_type : int)
      return access GlyphInfo  -- .\deps\raylib\src\raylib.h:1322
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadFontData";

      -- Generate image font atlas using chars info
   function GenImageFontAtlas
     (chars      : access constant GlyphInfo; recs : System.Address;
      glyphCount : int; fontSize : int; padding : int; packMethod : int)
      return Image  -- .\deps\raylib\src\raylib.h:1323
   with
      Import        => True,
      Convention    => C,
      External_Name => "GenImageFontAtlas";

      -- Unload font chars info data (RAM)
   procedure UnloadFontData
     (chars      : access GlyphInfo;
      glyphCount : int)  -- .\deps\raylib\src\raylib.h:1324
   with
      Import        => True,
      Convention    => C,
      External_Name => "UnloadFontData";

      -- Unload Font from GPU memory (VRAM)
   procedure UnloadFont
     (the_font : Font)  -- .\deps\raylib\src\raylib.h:1325
   with
      Import        => True,
      Convention    => C,
      External_Name => "UnloadFont";

      -- Text drawing functions
      -- Draw current FPS
   procedure DrawFPS
     (posX : int;
      posY : int)  -- .\deps\raylib\src\raylib.h:1328
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawFPS";

      -- Draw text (using default font)
   procedure DrawText
     (text      : Interfaces.C.Strings.chars_ptr; posX : int; posY : int;
      fontSize  : int;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1329
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawText";

      -- Draw text using font and additional parameters
   procedure DrawTextEx
     (the_font : Font; text : Interfaces.C.Strings.chars_ptr;
      position : Vector2;
      fontSize : Float;
      spacing  : Float;
      tint     : Color)  -- .\deps\raylib\src\raylib.h:1330
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawTextEx";

      -- Draw text using Font and pro parameters (rotation)
   procedure DrawTextPro
     (the_font : Font; text : Interfaces.C.Strings.chars_ptr;
      position : Vector2; origin : Vector2; rotation : Float; fontSize : Float;
      spacing  : Float;
      tint     : Color)  -- .\deps\raylib\src\raylib.h:1331
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawTextPro";

      -- Draw one character (codepoint)
   procedure DrawTextCodepoint
     (the_font : Font; codepoint : int; position : Vector2; fontSize : Float;
      tint     : Color)  -- .\deps\raylib\src\raylib.h:1332
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawTextCodepoint";

      -- Text font info functions
      -- Measure string width for default font
   function MeasureText
     (text     : Interfaces.C.Strings.chars_ptr;
      fontSize : int)
      return int  -- .\deps\raylib\src\raylib.h:1335
   with
      Import        => True,
      Convention    => C,
      External_Name => "MeasureText";

      -- Measure string size for Font
   function MeasureTextEx
     (the_font : Font; text : Interfaces.C.Strings.chars_ptr; fontSize : Float;
      spacing  : Float)
      return Vector2  -- .\deps\raylib\src\raylib.h:1336
   with
      Import        => True,
      Convention    => C,
      External_Name => "MeasureTextEx";

      -- Get glyph index position in font for a codepoint (unicode character), fallback to '?' if not found
   function GetGlyphIndex
     (the_font  : Font;
      codepoint : int)
      return int  -- .\deps\raylib\src\raylib.h:1337
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetGlyphIndex";

      -- Get glyph font info data for a codepoint (unicode character), fallback to '?' if not found
   function GetGlyphInfo
     (the_font  : Font;
      codepoint : int)
      return GlyphInfo  -- .\deps\raylib\src\raylib.h:1338
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetGlyphInfo";

      -- Get glyph rectangle in font atlas for a codepoint (unicode character), fallback to '?' if not found
   function GetGlyphAtlasRec
     (the_font  : Font;
      codepoint : int)
      return Rectangle  -- .\deps\raylib\src\raylib.h:1339
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetGlyphAtlasRec";

      -- Text codepoints management functions (unicode characters)
      -- Load all codepoints from a UTF-8 text string, codepoints count returned by parameter
   function LoadCodepoints
     (text : Interfaces.C.Strings.chars_ptr; count : access int)
      return access int  -- .\deps\raylib\src\raylib.h:1342
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadCodepoints";

      -- Unload codepoints data from memory
   procedure UnloadCodepoints
     (codepoints : access int)  -- .\deps\raylib\src\raylib.h:1343
   with
      Import        => True,
      Convention    => C,
      External_Name => "UnloadCodepoints";

      -- Get total number of codepoints in a UTF-8 encoded string
   function GetCodepointCount
     (text : Interfaces.C.Strings.chars_ptr)
      return int  -- .\deps\raylib\src\raylib.h:1344
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetCodepointCount";

      -- Get next codepoint in a UTF-8 encoded string, 0x3f('?') is returned on failure
   function GetCodepoint
     (text : Interfaces.C.Strings.chars_ptr; bytesProcessed : access int)
      return int  -- .\deps\raylib\src\raylib.h:1345
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetCodepoint";

      -- Encode one codepoint into UTF-8 byte array (array length returned as parameter)
   function CodepointToUTF8
     (codepoint : int; byteSize : access int)
      return Interfaces.C.Strings
     .chars_ptr  -- .\deps\raylib\src\raylib.h:1346
   with
      Import        => True,
      Convention    => C,
      External_Name => "CodepointToUTF8";

      -- Encode text as codepoints array into UTF-8 text string (WARNING: memory must be freed!)
   function TextCodepointsToUTF8
     (codepoints : access int; length : int)
      return Interfaces.C.Strings
     .chars_ptr  -- .\deps\raylib\src\raylib.h:1347
   with
      Import        => True,
      Convention    => C,
      External_Name => "TextCodepointsToUTF8";

      -- Text strings management functions (no UTF-8 strings, only byte chars)
      -- NOTE: Some strings allocate memory internally for returned strings, just be careful!
      -- Copy one string to another, returns bytes copied
   function TextCopy
     (dst : Interfaces.C.Strings.chars_ptr;
      src : Interfaces.C.Strings.chars_ptr)
      return int  -- .\deps\raylib\src\raylib.h:1351
   with
      Import        => True,
      Convention    => C,
      External_Name => "TextCopy";

      -- Check if two text string are equal
   function TextIsEqual
     (text1 : Interfaces.C.Strings.chars_ptr;
      text2 : Interfaces.C.Strings.chars_ptr) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1352
   with
      Import        => True,
      Convention    => C,
      External_Name => "TextIsEqual";

      -- Get text length, checks for '\0' ending
   function TextLength
     (text : Interfaces.C.Strings.chars_ptr)
      return unsigned  -- .\deps\raylib\src\raylib.h:1353
   with
      Import        => True,
      Convention    => C,
      External_Name => "TextLength";

      -- Text formatting with variables (sprintf() style)
   function TextFormat
     (text : Interfaces.C.Strings
        .chars_ptr  -- , ...
   )
      return Interfaces.C.Strings
     .chars_ptr  -- .\deps\raylib\src\raylib.h:1354
   with
      Import        => True,
      Convention    => C,
      External_Name => "TextFormat";

      -- Get a piece of a text string
   function TextSubtext
     (text : Interfaces.C.Strings.chars_ptr; position : int; length : int)
      return Interfaces.C.Strings
     .chars_ptr  -- .\deps\raylib\src\raylib.h:1355
   with
      Import        => True,
      Convention    => C,
      External_Name => "TextSubtext";

      -- Replace text string (WARNING: memory must be freed!)
   function TextReplace
     (text    : Interfaces.C.Strings.chars_ptr;
      replace : Interfaces.C.Strings.chars_ptr;
      by      : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings
     .chars_ptr  -- .\deps\raylib\src\raylib.h:1356
   with
      Import        => True,
      Convention    => C,
      External_Name => "TextReplace";

      -- Insert text in a position (WARNING: memory must be freed!)
   function TextInsert
     (text   : Interfaces.C.Strings.chars_ptr;
      insert : Interfaces.C.Strings.chars_ptr; position : int)
      return Interfaces.C.Strings
     .chars_ptr  -- .\deps\raylib\src\raylib.h:1357
   with
      Import        => True,
      Convention    => C,
      External_Name => "TextInsert";

      -- Join text strings with delimiter
   function TextJoin
     (textList  : System.Address; count : int;
      delimiter : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings
     .chars_ptr  -- .\deps\raylib\src\raylib.h:1358
   with
      Import        => True,
      Convention    => C,
      External_Name => "TextJoin";

      -- Split text into multiple strings
   function TextSplit
     (text  : Interfaces.C.Strings.chars_ptr; delimiter : char;
      count : access int) return System
     .Address  -- .\deps\raylib\src\raylib.h:1359
   with
      Import        => True,
      Convention    => C,
      External_Name => "TextSplit";

      -- Append text at specific position and move cursor!
   procedure TextAppend
     (text     : Interfaces.C.Strings.chars_ptr;
      append   : Interfaces.C.Strings.chars_ptr;
      position : access int)  -- .\deps\raylib\src\raylib.h:1360
   with
      Import        => True,
      Convention    => C,
      External_Name => "TextAppend";

      -- Find first text occurrence within a string
   function TextFindIndex
     (text : Interfaces.C.Strings.chars_ptr;
      find : Interfaces.C.Strings.chars_ptr)
      return int  -- .\deps\raylib\src\raylib.h:1361
   with
      Import        => True,
      Convention    => C,
      External_Name => "TextFindIndex";

      -- Get upper case version of provided string
   function TextToUpper
     (text : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings
     .chars_ptr  -- .\deps\raylib\src\raylib.h:1362
   with
      Import        => True,
      Convention    => C,
      External_Name => "TextToUpper";

      -- Get lower case version of provided string
   function TextToLower
     (text : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings
     .chars_ptr  -- .\deps\raylib\src\raylib.h:1363
   with
      Import        => True,
      Convention    => C,
      External_Name => "TextToLower";

      -- Get Pascal case notation version of provided string
   function TextToPascal
     (text : Interfaces.C.Strings.chars_ptr)
      return Interfaces.C.Strings
     .chars_ptr  -- .\deps\raylib\src\raylib.h:1364
   with
      Import        => True,
      Convention    => C,
      External_Name => "TextToPascal";

      -- Get integer value from text (negative values not supported)
   function TextToInteger
     (text : Interfaces.C.Strings.chars_ptr)
      return int  -- .\deps\raylib\src\raylib.h:1365
   with
      Import        => True,
      Convention    => C,
      External_Name => "TextToInteger";

      --------------------------------------------------------------------------------------
      -- Basic 3d Shapes Drawing Functions (Module: models)
      --------------------------------------------------------------------------------------
      -- Basic geometric 3D shapes drawing functions
      -- Draw a line in 3D world space
   procedure DrawLine3D
     (startPos  : Vector3;
      endPos    : Vector3;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1372
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawLine3D";

      -- Draw a point in 3D space, actually a small line
   procedure DrawPoint3D
     (position  : Vector3;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1373
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawPoint3D";

      -- Draw a circle in 3D world space
   procedure DrawCircle3D
     (center        : Vector3; radius : Float; rotationAxis : Vector3;
      rotationAngle : Float;
      the_color     : Color)  -- .\deps\raylib\src\raylib.h:1374
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawCircle3D";

      -- Draw a color-filled triangle (vertex in counter-clockwise order!)
   procedure DrawTriangle3D
     (v1        : Vector3;
      v2        : Vector3;
      v3        : Vector3;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1375
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawTriangle3D";

      -- Draw a triangle strip defined by points
   procedure DrawTriangleStrip3D
     (points     : access Vector3;
      pointCount : int;
      the_color  : Color)  -- .\deps\raylib\src\raylib.h:1376
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawTriangleStrip3D";

      -- Draw cube
   procedure DrawCube
     (position  : Vector3; width : Float; height : Float; length : Float;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1377
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawCube";

      -- Draw cube (Vector version)
   procedure DrawCubeV
     (position  : Vector3;
      size      : Vector3;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1378
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawCubeV";

      -- Draw cube wires
   procedure DrawCubeWires
     (position  : Vector3; width : Float; height : Float; length : Float;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1379
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawCubeWires";

      -- Draw cube wires (Vector version)
   procedure DrawCubeWiresV
     (position  : Vector3;
      size      : Vector3;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1380
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawCubeWiresV";

      -- Draw cube textured
   procedure DrawCubeTexture
     (texture   : Texture2D; position : Vector3; width : Float; height : Float;
      length    : Float;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1381
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawCubeTexture";

      -- Draw cube with a region of a texture
   procedure DrawCubeTextureRec
     (texture   : Texture2D; source : Rectangle; position : Vector3;
      width     : Float;
      height    : Float;
      length    : Float;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1382
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawCubeTextureRec";

      -- Draw sphere
   procedure DrawSphere
     (centerPos : Vector3;
      radius    : Float;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1383
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawSphere";

      -- Draw sphere with extended parameters
   procedure DrawSphereEx
     (centerPos : Vector3; radius : Float; rings : int; slices : int;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1384
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawSphereEx";

      -- Draw sphere wires
   procedure DrawSphereWires
     (centerPos : Vector3; radius : Float; rings : int; slices : int;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1385
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawSphereWires";

      -- Draw a cylinder/cone
   procedure DrawCylinder
     (position  : Vector3; radiusTop : Float; radiusBottom : Float;
      height    : Float;
      slices    : int;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1386
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawCylinder";

      -- Draw a cylinder with base at startPos and top at endPos
   procedure DrawCylinderEx
     (startPos  : Vector3; endPos : Vector3; startRadius : Float;
      endRadius : Float;
      sides     : int;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1387
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawCylinderEx";

      -- Draw a cylinder/cone wires
   procedure DrawCylinderWires
     (position  : Vector3; radiusTop : Float; radiusBottom : Float;
      height    : Float;
      slices    : int;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1388
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawCylinderWires";

      -- Draw a cylinder wires with base at startPos and top at endPos
   procedure DrawCylinderWiresEx
     (startPos  : Vector3; endPos : Vector3; startRadius : Float;
      endRadius : Float;
      sides     : int;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1389
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawCylinderWiresEx";

      -- Draw a plane XZ
   procedure DrawPlane
     (centerPos : Vector3;
      size      : Vector2;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1390
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawPlane";

      -- Draw a ray line
   procedure DrawRay
     (the_ray   : Ray;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1391
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawRay";

      -- Draw a grid (centered at (0, 0, 0))
   procedure DrawGrid
     (slices  : int;
      spacing : Float)  -- .\deps\raylib\src\raylib.h:1392
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawGrid";

      --------------------------------------------------------------------------------------
      -- Model 3d Loading and Drawing Functions (Module: models)
      --------------------------------------------------------------------------------------
      -- Model management functions
      -- Load model from files (meshes and materials)
   function LoadModel
     (fileName : Interfaces.C.Strings.chars_ptr)
      return Model  -- .\deps\raylib\src\raylib.h:1399
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadModel";

      -- Load model from generated mesh (default material)
   function LoadModelFromMesh
     (the_mesh : Mesh)
      return Model  -- .\deps\raylib\src\raylib.h:1400
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadModelFromMesh";

      -- Unload model (including meshes) from memory (RAM and/or VRAM)
   procedure UnloadModel
     (the_model : Model)  -- .\deps\raylib\src\raylib.h:1401
   with
      Import        => True,
      Convention    => C,
      External_Name => "UnloadModel";

      -- Unload model (but not meshes) from memory (RAM and/or VRAM)
   procedure UnloadModelKeepMeshes
     (the_model : Model)  -- .\deps\raylib\src\raylib.h:1402
   with
      Import        => True,
      Convention    => C,
      External_Name => "UnloadModelKeepMeshes";

      -- Compute model bounding box limits (considers all meshes)
   function GetModelBoundingBox
     (the_model : Model)
      return BoundingBox  -- .\deps\raylib\src\raylib.h:1403
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetModelBoundingBox";

      -- Model drawing functions
      -- Draw a model (with texture if set)
   procedure DrawModel
     (the_model : Model;
      position  : Vector3;
      scale     : Float;
      tint      : Color)  -- .\deps\raylib\src\raylib.h:1406
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawModel";

      -- Draw a model with extended parameters
   procedure DrawModelEx
     (the_model     : Model; position : Vector3; rotationAxis : Vector3;
      rotationAngle : Float;
      scale         : Vector3;
      tint          : Color)  -- .\deps\raylib\src\raylib.h:1407
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawModelEx";

      -- Draw a model wires (with texture if set)
   procedure DrawModelWires
     (the_model : Model;
      position  : Vector3;
      scale     : Float;
      tint      : Color)  -- .\deps\raylib\src\raylib.h:1408
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawModelWires";

      -- Draw a model wires (with texture if set) with extended parameters
   procedure DrawModelWiresEx
     (the_model     : Model; position : Vector3; rotationAxis : Vector3;
      rotationAngle : Float;
      scale         : Vector3;
      tint          : Color)  -- .\deps\raylib\src\raylib.h:1409
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawModelWiresEx";

      -- Draw bounding box (wires)
   procedure DrawBoundingBox
     (box       : BoundingBox;
      the_color : Color)  -- .\deps\raylib\src\raylib.h:1410
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawBoundingBox";

      -- Draw a billboard texture
   procedure DrawBillboard
     (the_camera : Camera; texture : Texture2D; position : Vector3;
      size       : Float;
      tint       : Color)  -- .\deps\raylib\src\raylib.h:1411
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawBillboard";

      -- Draw a billboard texture defined by source
   procedure DrawBillboardRec
     (the_camera : Camera; texture : Texture2D; source : Rectangle;
      position   : Vector3;
      size       : Vector2;
      tint       : Color)  -- .\deps\raylib\src\raylib.h:1412
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawBillboardRec";

      -- Draw a billboard texture defined by source and rotation
   procedure DrawBillboardPro
     (the_camera : Camera; texture : Texture2D; source : Rectangle;
      position   : Vector3; up : Vector3; size : Vector2; origin : Vector2;
      rotation   : Float;
      tint       : Color)  -- .\deps\raylib\src\raylib.h:1413
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawBillboardPro";

      -- Mesh management functions
      -- Upload mesh vertex data in GPU and provide VAO/VBO ids
   procedure UploadMesh
     (the_mesh : access Mesh; dynamic : Extensions
        .bool)  -- .\deps\raylib\src\raylib.h:1416
   with
      Import        => True,
      Convention    => C,
      External_Name => "UploadMesh";

      -- Update mesh vertex data in GPU for a specific buffer index
   procedure UpdateMeshBuffer
     (the_mesh : Mesh; index : int; data : System.Address; dataSize : int;
      offset   : int)  -- .\deps\raylib\src\raylib.h:1417
   with
      Import        => True,
      Convention    => C,
      External_Name => "UpdateMeshBuffer";

      -- Unload mesh data from CPU and GPU
   procedure UnloadMesh
     (the_mesh : Mesh)  -- .\deps\raylib\src\raylib.h:1418
   with
      Import        => True,
      Convention    => C,
      External_Name => "UnloadMesh";

      -- Draw a 3d mesh with material and transform
   procedure DrawMesh
     (the_mesh     : Mesh;
      the_material : Material;
      transform    : Matrix)  -- .\deps\raylib\src\raylib.h:1419
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawMesh";

      -- Draw multiple mesh instances with material and different transforms
   procedure DrawMeshInstanced
     (the_mesh  : Mesh; the_material : Material; transforms : access Matrix;
      instances : int)  -- .\deps\raylib\src\raylib.h:1420
   with
      Import        => True,
      Convention    => C,
      External_Name => "DrawMeshInstanced";

      -- Export mesh data to file, returns true on success
   function ExportMesh
     (the_mesh : Mesh; fileName : Interfaces.C.Strings.chars_ptr)
      return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1421
   with
      Import        => True,
      Convention    => C,
      External_Name => "ExportMesh";

      -- Compute mesh bounding box limits
   function GetMeshBoundingBox
     (the_mesh : Mesh)
      return BoundingBox  -- .\deps\raylib\src\raylib.h:1422
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetMeshBoundingBox";

      -- Compute mesh tangents
   procedure GenMeshTangents
     (the_mesh : access Mesh)  -- .\deps\raylib\src\raylib.h:1423
   with
      Import        => True,
      Convention    => C,
      External_Name => "GenMeshTangents";

      -- Compute mesh binormals
   procedure GenMeshBinormals
     (the_mesh : access Mesh)  -- .\deps\raylib\src\raylib.h:1424
   with
      Import        => True,
      Convention    => C,
      External_Name => "GenMeshBinormals";

      -- Mesh generation functions
      -- Generate polygonal mesh
   function GenMeshPoly
     (sides  : int;
      radius : Float)
      return Mesh  -- .\deps\raylib\src\raylib.h:1427
   with
      Import        => True,
      Convention    => C,
      External_Name => "GenMeshPoly";

      -- Generate plane mesh (with subdivisions)
   function GenMeshPlane
     (width  : Float;
      length : Float;
      resX   : int;
      resZ   : int)
      return Mesh  -- .\deps\raylib\src\raylib.h:1428
   with
      Import        => True,
      Convention    => C,
      External_Name => "GenMeshPlane";

      -- Generate cuboid mesh
   function GenMeshCube
     (width  : Float;
      height : Float;
      length : Float)
      return Mesh  -- .\deps\raylib\src\raylib.h:1429
   with
      Import        => True,
      Convention    => C,
      External_Name => "GenMeshCube";

      -- Generate sphere mesh (standard sphere)
   function GenMeshSphere
     (radius : Float;
      rings  : int;
      slices : int)
      return Mesh  -- .\deps\raylib\src\raylib.h:1430
   with
      Import        => True,
      Convention    => C,
      External_Name => "GenMeshSphere";

      -- Generate half-sphere mesh (no bottom cap)
   function GenMeshHemiSphere
     (radius : Float;
      rings  : int;
      slices : int)
      return Mesh  -- .\deps\raylib\src\raylib.h:1431
   with
      Import        => True,
      Convention    => C,
      External_Name => "GenMeshHemiSphere";

      -- Generate cylinder mesh
   function GenMeshCylinder
     (radius : Float;
      height : Float;
      slices : int)
      return Mesh  -- .\deps\raylib\src\raylib.h:1432
   with
      Import        => True,
      Convention    => C,
      External_Name => "GenMeshCylinder";

      -- Generate cone/pyramid mesh
   function GenMeshCone
     (radius : Float;
      height : Float;
      slices : int)
      return Mesh  -- .\deps\raylib\src\raylib.h:1433
   with
      Import        => True,
      Convention    => C,
      External_Name => "GenMeshCone";

      -- Generate torus mesh
   function GenMeshTorus
     (radius : Float;
      size   : Float;
      radSeg : int;
      sides  : int)
      return Mesh  -- .\deps\raylib\src\raylib.h:1434
   with
      Import        => True,
      Convention    => C,
      External_Name => "GenMeshTorus";

      -- Generate trefoil knot mesh
   function GenMeshKnot
     (radius : Float;
      size   : Float;
      radSeg : int;
      sides  : int)
      return Mesh  -- .\deps\raylib\src\raylib.h:1435
   with
      Import        => True,
      Convention    => C,
      External_Name => "GenMeshKnot";

      -- Generate heightmap mesh from image data
   function GenMeshHeightmap
     (heightmap : Image;
      size      : Vector3)
      return Mesh  -- .\deps\raylib\src\raylib.h:1436
   with
      Import        => True,
      Convention    => C,
      External_Name => "GenMeshHeightmap";

      -- Generate cubes-based map mesh from image data
   function GenMeshCubicmap
     (cubicmap : Image;
      cubeSize : Vector3)
      return Mesh  -- .\deps\raylib\src\raylib.h:1437
   with
      Import        => True,
      Convention    => C,
      External_Name => "GenMeshCubicmap";

      -- Material loading/unloading functions
      -- Load materials from model file
   function LoadMaterials
     (fileName : Interfaces.C.Strings.chars_ptr; materialCount : access int)
      return access Material  -- .\deps\raylib\src\raylib.h:1440
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadMaterials";

      -- Load default material (Supports: DIFFUSE, SPECULAR, NORMAL maps)
   function LoadMaterialDefault
      return Material  -- .\deps\raylib\src\raylib.h:1441
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadMaterialDefault";

      -- Unload material from GPU memory (VRAM)
   procedure UnloadMaterial
     (the_material : Material)  -- .\deps\raylib\src\raylib.h:1442
   with
      Import        => True,
      Convention    => C,
      External_Name => "UnloadMaterial";

      -- Set texture for a material map type (MATERIAL_MAP_DIFFUSE, MATERIAL_MAP_SPECULAR...)
   procedure SetMaterialTexture
     (the_material : access Material;
      mapType      : int;
      texture      : Texture2D)  -- .\deps\raylib\src\raylib.h:1443
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetMaterialTexture";

      -- Set material for a mesh
   procedure SetModelMeshMaterial
     (the_model  : access Model;
      meshId     : int;
      materialId : int)  -- .\deps\raylib\src\raylib.h:1444
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetModelMeshMaterial";

      -- Model animations loading/unloading functions
      -- Load model animations from file
   function LoadModelAnimations
     (fileName : Interfaces.C.Strings.chars_ptr; animCount : access unsigned)
      return access ModelAnimation  -- .\deps\raylib\src\raylib.h:1447
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadModelAnimations";

      -- Update model animation pose
   procedure UpdateModelAnimation
     (the_model : Model;
      anim      : ModelAnimation;
      frame     : int)  -- .\deps\raylib\src\raylib.h:1448
   with
      Import        => True,
      Convention    => C,
      External_Name => "UpdateModelAnimation";

      -- Unload animation data
   procedure UnloadModelAnimation
     (anim : ModelAnimation)  -- .\deps\raylib\src\raylib.h:1449
   with
      Import        => True,
      Convention    => C,
      External_Name => "UnloadModelAnimation";

      -- Unload animation array data
   procedure UnloadModelAnimations
     (animations : access ModelAnimation;
      count      : unsigned)  -- .\deps\raylib\src\raylib.h:1450
   with
      Import        => True,
      Convention    => C,
      External_Name => "UnloadModelAnimations";

      -- Check model animation skeleton match
   function IsModelAnimationValid
     (the_model : Model; anim : ModelAnimation) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1451
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsModelAnimationValid";

      -- Collision detection functions
      -- Check collision between two spheres
   function CheckCollisionSpheres
     (center1 : Vector3; radius1 : Float; center2 : Vector3; radius2 : Float)
      return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1454
   with
      Import        => True,
      Convention    => C,
      External_Name => "CheckCollisionSpheres";

      -- Check collision between two bounding boxes
   function CheckCollisionBoxes
     (box1 : BoundingBox; box2 : BoundingBox) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1455
   with
      Import        => True,
      Convention    => C,
      External_Name => "CheckCollisionBoxes";

      -- Check collision between box and sphere
   function CheckCollisionBoxSphere
     (box : BoundingBox; center : Vector3; radius : Float)
      return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1456
   with
      Import        => True,
      Convention    => C,
      External_Name => "CheckCollisionBoxSphere";

      -- Get collision info between ray and sphere
   function GetRayCollisionSphere
     (the_ray : Ray;
      center  : Vector3;
      radius  : Float)
      return RayCollision  -- .\deps\raylib\src\raylib.h:1457
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetRayCollisionSphere";

      -- Get collision info between ray and box
   function GetRayCollisionBox
     (the_ray : Ray;
      box     : BoundingBox)
      return RayCollision  -- .\deps\raylib\src\raylib.h:1458
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetRayCollisionBox";

      -- Get collision info between ray and model
   function GetRayCollisionModel
     (the_ray   : Ray;
      the_model : Model)
      return RayCollision  -- .\deps\raylib\src\raylib.h:1459
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetRayCollisionModel";

      -- Get collision info between ray and mesh
   function GetRayCollisionMesh
     (the_ray : Ray; the_mesh : Mesh; transform : Matrix)
      return RayCollision  -- .\deps\raylib\src\raylib.h:1460
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetRayCollisionMesh";

      -- Get collision info between ray and triangle
   function GetRayCollisionTriangle
     (the_ray : Ray; p1 : Vector3; p2 : Vector3; p3 : Vector3)
      return RayCollision  -- .\deps\raylib\src\raylib.h:1461
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetRayCollisionTriangle";

      -- Get collision info between ray and quad
   function GetRayCollisionQuad
     (the_ray : Ray; p1 : Vector3; p2 : Vector3; p3 : Vector3; p4 : Vector3)
      return RayCollision  -- .\deps\raylib\src\raylib.h:1462
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetRayCollisionQuad";

      --------------------------------------------------------------------------------------
      -- Audio Loading and Playing Functions (Module: audio)
      --------------------------------------------------------------------------------------
      -- Audio device management functions
      -- Initialize audio device and context
   procedure InitAudioDevice  -- .\deps\raylib\src\raylib.h:1469
   with
      Import        => True,
      Convention    => C,
      External_Name => "InitAudioDevice";

      -- Close the audio device and context
   procedure CloseAudioDevice  -- .\deps\raylib\src\raylib.h:1470
   with
      Import        => True,
      Convention    => C,
      External_Name => "CloseAudioDevice";

      -- Check if audio device has been initialized successfully
   function IsAudioDeviceReady return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1471
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsAudioDeviceReady";

      -- Set master volume (listener)
   procedure SetMasterVolume
     (volume : Float)  -- .\deps\raylib\src\raylib.h:1472
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetMasterVolume";

      -- Wave/Sound loading/unloading functions
      -- Load wave data from file
   function LoadWave
     (fileName : Interfaces.C.Strings.chars_ptr)
      return Wave  -- .\deps\raylib\src\raylib.h:1475
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadWave";

   -- Load wave from memory buffer, fileType refers to extension: i.e. '.wav'
   function LoadWaveFromMemory
     (fileType : Interfaces.C.Strings.chars_ptr;
      fileData : access unsigned_char;
      dataSize : int)
      return Wave  -- .\deps\raylib\src\raylib.h:1476
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadWaveFromMemory";

      -- Load sound from file
   function LoadSound
     (fileName : Interfaces.C.Strings.chars_ptr)
      return Sound  -- .\deps\raylib\src\raylib.h:1477
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadSound";

      -- Load sound from wave data
   function LoadSoundFromWave
     (the_wave : Wave)
      return Sound  -- .\deps\raylib\src\raylib.h:1478
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadSoundFromWave";

      -- Update sound buffer with new data
   procedure UpdateSound
     (the_sound   : Sound;
      data        : System.Address;
      sampleCount : int)  -- .\deps\raylib\src\raylib.h:1479
   with
      Import        => True,
      Convention    => C,
      External_Name => "UpdateSound";

      -- Unload wave data
   procedure UnloadWave
     (the_wave : Wave)  -- .\deps\raylib\src\raylib.h:1480
   with
      Import        => True,
      Convention    => C,
      External_Name => "UnloadWave";

      -- Unload sound
   procedure UnloadSound
     (the_sound : Sound)  -- .\deps\raylib\src\raylib.h:1481
   with
      Import        => True,
      Convention    => C,
      External_Name => "UnloadSound";

      -- Export wave data to file, returns true on success
   function ExportWave
     (the_wave : Wave; fileName : Interfaces.C.Strings.chars_ptr)
      return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1482
   with
      Import        => True,
      Convention    => C,
      External_Name => "ExportWave";

      -- Export wave sample data to code (.h), returns true on success
   function ExportWaveAsCode
     (the_wave : Wave; fileName : Interfaces.C.Strings.chars_ptr)
      return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1483
   with
      Import        => True,
      Convention    => C,
      External_Name => "ExportWaveAsCode";

      -- Wave/Sound management functions
      -- Play a sound
   procedure PlaySound
     (the_sound : Sound)  -- .\deps\raylib\src\raylib.h:1486
   with
      Import        => True,
      Convention    => C,
      External_Name => "PlaySound";

      -- Stop playing a sound
   procedure StopSound
     (the_sound : Sound)  -- .\deps\raylib\src\raylib.h:1487
   with
      Import        => True,
      Convention    => C,
      External_Name => "StopSound";

      -- Pause a sound
   procedure PauseSound
     (the_sound : Sound)  -- .\deps\raylib\src\raylib.h:1488
   with
      Import        => True,
      Convention    => C,
      External_Name => "PauseSound";

      -- Resume a paused sound
   procedure ResumeSound
     (the_sound : Sound)  -- .\deps\raylib\src\raylib.h:1489
   with
      Import        => True,
      Convention    => C,
      External_Name => "ResumeSound";

      -- Play a sound (using multichannel buffer pool)
   procedure PlaySoundMulti
     (the_sound : Sound)  -- .\deps\raylib\src\raylib.h:1490
   with
      Import        => True,
      Convention    => C,
      External_Name => "PlaySoundMulti";

      -- Stop any sound playing (using multichannel buffer pool)
   procedure StopSoundMulti  -- .\deps\raylib\src\raylib.h:1491
   with
      Import        => True,
      Convention    => C,
      External_Name => "StopSoundMulti";

      -- Get number of sounds playing in the multichannel
   function GetSoundsPlaying
      return int  -- .\deps\raylib\src\raylib.h:1492
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetSoundsPlaying";

      -- Check if a sound is currently playing
   function IsSoundPlaying
     (the_sound : Sound) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1493
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsSoundPlaying";

      -- Set volume for a sound (1.0 is max level)
   procedure SetSoundVolume
     (the_sound : Sound;
      volume    : Float)  -- .\deps\raylib\src\raylib.h:1494
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetSoundVolume";

      -- Set pitch for a sound (1.0 is base level)
   procedure SetSoundPitch
     (the_sound : Sound;
      pitch     : Float)  -- .\deps\raylib\src\raylib.h:1495
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetSoundPitch";

      -- Convert wave data to desired format
   procedure WaveFormat
     (the_wave : access Wave; sampleRate : int; sampleSize : int;
      channels : int)  -- .\deps\raylib\src\raylib.h:1496
   with
      Import        => True,
      Convention    => C,
      External_Name => "WaveFormat";

      -- Copy a wave to a new wave
   function WaveCopy
     (the_wave : Wave)
      return Wave  -- .\deps\raylib\src\raylib.h:1497
   with
      Import        => True,
      Convention    => C,
      External_Name => "WaveCopy";

      -- Crop a wave to defined samples range
   procedure WaveCrop
     (the_wave    : access Wave;
      initSample  : int;
      finalSample : int)  -- .\deps\raylib\src\raylib.h:1498
   with
      Import        => True,
      Convention    => C,
      External_Name => "WaveCrop";

      -- Load samples data from wave as a floats array
   function LoadWaveSamples
     (the_wave : Wave)
      return access Float  -- .\deps\raylib\src\raylib.h:1499
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadWaveSamples";

      -- Unload samples data loaded with LoadWaveSamples()
   procedure UnloadWaveSamples
     (samples : access Float)  -- .\deps\raylib\src\raylib.h:1500
   with
      Import        => True,
      Convention    => C,
      External_Name => "UnloadWaveSamples";

      -- Music management functions
      -- Load music stream from file
   function LoadMusicStream
     (fileName : Interfaces.C.Strings.chars_ptr)
      return Music  -- .\deps\raylib\src\raylib.h:1503
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadMusicStream";

      -- Load music stream from data
   function LoadMusicStreamFromMemory
     (fileType : Interfaces.C.Strings.chars_ptr; data : access unsigned_char;
      dataSize : int)
      return Music  -- .\deps\raylib\src\raylib.h:1504
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadMusicStreamFromMemory";

      -- Unload music stream
   procedure UnloadMusicStream
     (the_music : Music)  -- .\deps\raylib\src\raylib.h:1505
   with
      Import        => True,
      Convention    => C,
      External_Name => "UnloadMusicStream";

      -- Start music playing
   procedure PlayMusicStream
     (the_music : Music)  -- .\deps\raylib\src\raylib.h:1506
   with
      Import        => True,
      Convention    => C,
      External_Name => "PlayMusicStream";

      -- Check if music is playing
   function IsMusicStreamPlaying
     (the_music : Music) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1507
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsMusicStreamPlaying";

      -- Updates buffers for music streaming
   procedure UpdateMusicStream
     (the_music : Music)  -- .\deps\raylib\src\raylib.h:1508
   with
      Import        => True,
      Convention    => C,
      External_Name => "UpdateMusicStream";

      -- Stop music playing
   procedure StopMusicStream
     (the_music : Music)  -- .\deps\raylib\src\raylib.h:1509
   with
      Import        => True,
      Convention    => C,
      External_Name => "StopMusicStream";

      -- Pause music playing
   procedure PauseMusicStream
     (the_music : Music)  -- .\deps\raylib\src\raylib.h:1510
   with
      Import        => True,
      Convention    => C,
      External_Name => "PauseMusicStream";

      -- Resume playing paused music
   procedure ResumeMusicStream
     (the_music : Music)  -- .\deps\raylib\src\raylib.h:1511
   with
      Import        => True,
      Convention    => C,
      External_Name => "ResumeMusicStream";

      -- Seek music to a position (in seconds)
   procedure SeekMusicStream
     (the_music : Music;
      position  : Float)  -- .\deps\raylib\src\raylib.h:1512
   with
      Import        => True,
      Convention    => C,
      External_Name => "SeekMusicStream";

      -- Set volume for music (1.0 is max level)
   procedure SetMusicVolume
     (the_music : Music;
      volume    : Float)  -- .\deps\raylib\src\raylib.h:1513
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetMusicVolume";

      -- Set pitch for a music (1.0 is base level)
   procedure SetMusicPitch
     (the_music : Music;
      pitch     : Float)  -- .\deps\raylib\src\raylib.h:1514
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetMusicPitch";

      -- Get music time length (in seconds)
   function GetMusicTimeLength
     (the_music : Music)
      return Float  -- .\deps\raylib\src\raylib.h:1515
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetMusicTimeLength";

      -- Get current music time played (in seconds)
   function GetMusicTimePlayed
     (the_music : Music)
      return Float  -- .\deps\raylib\src\raylib.h:1516
   with
      Import        => True,
      Convention    => C,
      External_Name => "GetMusicTimePlayed";

      -- AudioStream management functions
      -- Load audio stream (to stream raw audio pcm data)
   function LoadAudioStream
     (sampleRate : unsigned; sampleSize : unsigned; channels : unsigned)
      return AudioStream  -- .\deps\raylib\src\raylib.h:1519
   with
      Import        => True,
      Convention    => C,
      External_Name => "LoadAudioStream";

      -- Unload audio stream and free memory
   procedure UnloadAudioStream
     (stream : AudioStream)  -- .\deps\raylib\src\raylib.h:1520
   with
      Import        => True,
      Convention    => C,
      External_Name => "UnloadAudioStream";

      -- Update audio stream buffers with data
   procedure UpdateAudioStream
     (stream     : AudioStream;
      data       : System.Address;
      frameCount : int)  -- .\deps\raylib\src\raylib.h:1521
   with
      Import        => True,
      Convention    => C,
      External_Name => "UpdateAudioStream";

      -- Check if any audio stream buffers requires refill
   function IsAudioStreamProcessed
     (stream : AudioStream) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1522
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsAudioStreamProcessed";

      -- Play audio stream
   procedure PlayAudioStream
     (stream : AudioStream)  -- .\deps\raylib\src\raylib.h:1523
   with
      Import        => True,
      Convention    => C,
      External_Name => "PlayAudioStream";

      -- Pause audio stream
   procedure PauseAudioStream
     (stream : AudioStream)  -- .\deps\raylib\src\raylib.h:1524
   with
      Import        => True,
      Convention    => C,
      External_Name => "PauseAudioStream";

      -- Resume audio stream
   procedure ResumeAudioStream
     (stream : AudioStream)  -- .\deps\raylib\src\raylib.h:1525
   with
      Import        => True,
      Convention    => C,
      External_Name => "ResumeAudioStream";

      -- Check if audio stream is playing
   function IsAudioStreamPlaying
     (stream : AudioStream) return Extensions
     .bool  -- .\deps\raylib\src\raylib.h:1526
   with
      Import        => True,
      Convention    => C,
      External_Name => "IsAudioStreamPlaying";

      -- Stop audio stream
   procedure StopAudioStream
     (stream : AudioStream)  -- .\deps\raylib\src\raylib.h:1527
   with
      Import        => True,
      Convention    => C,
      External_Name => "StopAudioStream";

      -- Set volume for audio stream (1.0 is max level)
   procedure SetAudioStreamVolume
     (stream : AudioStream;
      volume : Float)  -- .\deps\raylib\src\raylib.h:1528
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetAudioStreamVolume";

      -- Set pitch for audio stream (1.0 is base level)
   procedure SetAudioStreamPitch
     (stream : AudioStream;
      pitch  : Float)  -- .\deps\raylib\src\raylib.h:1529
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetAudioStreamPitch";

      -- Default size for new audio streams
   procedure SetAudioStreamBufferSizeDefault
     (size : int)  -- .\deps\raylib\src\raylib.h:1530
   with
      Import        => True,
      Convention    => C,
      External_Name => "SetAudioStreamBufferSizeDefault";

end raylib_h;
