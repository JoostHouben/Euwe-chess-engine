#include "EvalParams.h"

#include "Piece.h"
#include "RangePatches.h"

#include <format>
#include <ranges>
#include <sstream>

#include <cstring>

namespace {

constexpr EvalParamArray kDefaultParams = {
        0.209377f,    1.000000f,    0.762991f,    1.679964f,    6.320265f,    0.000000f,
        0.209377f,    1.076343f,    0.337535f,    0.180702f,    0.335263f,    1.000000f,
        -0.013482f,   -0.003418f,   -0.009432f,   -0.000560f,   64.000435f,   -26.351181f,
        156.565796f,  340.719513f,  353.118835f,  320.997894f,  410.556458f,  464.268372f,
        675.974976f,  1224.790771f, 1105.098999f, 0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    -52.539951f,  156.096634f,  -53.452187f,
        154.767105f,  -49.717445f,  160.140411f,  -65.971672f,  159.911392f,  -47.723312f,
        178.570053f,  -11.968603f,  160.740524f,  -5.154614f,   154.229828f,  -42.270832f,
        146.641235f,  -43.390354f,  152.114502f,  -43.003387f,  149.330414f,  -37.329178f,
        152.994232f,  -45.799900f,  162.807800f,  -37.250408f,  159.786774f,  -19.545551f,
        156.006592f,  -14.389118f,  149.385803f,  -27.468624f,  145.834946f,  -26.385317f,
        160.959305f,  -17.506132f,  160.170441f,  -20.490709f,  163.154144f,  -12.601834f,
        156.041901f,  -12.060139f,  155.346329f,  -10.452168f,  160.511627f,  -14.936560f,
        161.201157f,  -27.409262f,  156.786713f,  -11.593290f,  178.171204f,  -5.057729f,
        168.132599f,  -7.664227f,   161.709122f,  -3.198724f,   156.380219f,  11.653509f,
        162.379822f,  16.579561f,   153.036148f,  -0.153399f,   166.115585f,  -10.211335f,
        162.491104f,  5.378837f,    185.003494f,  -10.601591f,  184.084656f,  20.883957f,
        183.432709f,  -2.821534f,   159.667664f,  6.543560f,    175.046860f,  75.817436f,
        179.154404f,  84.529587f,   180.801788f,  38.137192f,   179.073608f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    276.880737f,  312.359131f,  301.825775f,  317.550446f,  309.051849f,
        349.887299f,  303.591949f,  355.379761f,  306.679321f,  336.516418f,  321.829071f,
        326.656769f,  293.445984f,  318.549286f,  260.437775f,  286.230103f,  298.713867f,
        340.299561f,  325.410095f,  335.075073f,  316.427582f,  340.341125f,  333.736633f,
        347.682281f,  328.811707f,  348.011871f,  337.975067f,  340.702545f,  311.684265f,
        333.824280f,  304.721283f,  311.363678f,  307.938812f,  335.022766f,  329.864532f,
        345.194702f,  339.093536f,  350.340363f,  350.362030f,  362.750336f,  364.255859f,
        355.489258f,  341.665100f,  339.550110f,  347.071777f,  323.712372f,  315.674774f,
        328.751221f,  339.955841f,  347.399902f,  371.934967f,  352.193207f,  373.052460f,
        365.595917f,  365.982544f,  362.459045f,  371.728729f,  372.921112f,  356.240875f,
        368.359314f,  357.322174f,  358.123871f,  334.528534f,  347.267456f,  345.510010f,
        364.538544f,  359.502472f,  367.760071f,  387.240631f,  370.067169f,  395.627136f,
        372.812805f,  374.689087f,  370.900391f,  383.845520f,  362.309723f,  364.920135f,
        359.267853f,  364.004059f,  348.515625f,  334.228851f,  366.992157f,  353.792969f,
        378.499176f,  416.586487f,  354.378204f,  409.144226f,  356.176819f,  465.152710f,
        341.994385f,  429.411957f,  348.315277f,  414.081604f,  339.728058f,  333.053589f,
        349.813477f,  319.208282f,  374.392181f,  339.678558f,  359.967133f,  376.747009f,
        356.194183f,  379.415161f,  373.835114f,  389.623779f,  351.801453f,  397.999084f,
        349.193909f,  354.828186f,  356.112061f,  345.618591f,  357.353790f,  153.898193f,
        388.036041f,  312.385559f,  385.139862f,  310.740601f,  391.129425f,  326.764008f,
        394.838776f,  311.335663f,  382.054810f,  380.559082f,  383.242981f,  321.306427f,
        360.722809f,  208.529037f,  258.703674f,  289.836517f,  379.031830f,  314.792755f,
        390.744995f,  298.209900f,  398.929901f,  292.806091f,  395.352020f,  293.626282f,
        398.854309f,  296.287628f,  401.934845f,  294.616272f,  403.198395f,  314.678467f,
        347.240051f,  321.145721f,  387.372253f,  310.097229f,  395.153015f,  325.267700f,
        405.966217f,  305.841888f,  412.452606f,  318.536926f,  403.626373f,  324.350677f,
        402.490112f,  335.521881f,  393.956451f,  320.868347f,  367.382324f,  311.423950f,
        394.887665f,  331.362366f,  406.868317f,  326.476379f,  413.936096f,  326.838226f,
        413.678986f,  328.809143f,  418.715027f,  331.436951f,  411.384399f,  336.544647f,
        390.659851f,  326.087708f,  395.239441f,  309.407379f,  415.268463f,  327.894287f,
        411.892273f,  326.977081f,  420.420380f,  341.912262f,  424.184326f,  342.745911f,
        420.361938f,  327.037903f,  419.982300f,  344.981384f,  404.439331f,  332.350708f,
        395.729492f,  309.368591f,  423.133148f,  325.549225f,  420.614716f,  349.890594f,
        424.890961f,  366.308472f,  412.914398f,  358.766510f,  416.470490f,  360.645844f,
        421.345551f,  328.999603f,  422.056366f,  313.961060f,  415.556854f,  329.244415f,
        408.668030f,  322.452148f,  432.874451f,  342.327179f,  421.372742f,  363.749054f,
        416.513580f,  356.181061f,  424.847351f,  404.017456f,  410.372681f,  382.061798f,
        418.608795f,  363.778137f,  399.385529f,  299.819794f,  417.049011f,  318.127747f,
        425.870789f,  343.916656f,  423.063080f,  320.242493f,  426.988892f,  372.675720f,
        409.990692f,  341.423523f,  419.215332f,  363.044128f,  408.536621f,  300.148987f,
        388.953461f,  318.885498f,  420.162018f,  276.536499f,  443.121002f,  333.072418f,
        419.531952f,  309.739136f,  424.541382f,  325.093445f,  418.297058f,  307.139496f,
        415.053345f,  349.646973f,  378.399963f,  411.825348f,  382.679718f,  474.965302f,
        651.564819f,  466.176758f,  654.881714f,  465.265778f,  658.151123f,  463.712463f,
        656.276001f,  465.614136f,  653.527832f,  458.627411f,  663.426025f,  445.019501f,
        667.062073f,  450.010681f,  640.341614f,  455.850311f,  658.261353f,  457.013184f,
        656.475952f,  452.177368f,  666.524292f,  450.314941f,  669.652039f,  453.764221f,
        669.868774f,  456.908844f,  664.893494f,  448.391449f,  651.088135f,  434.758179f,
        665.111450f,  446.796356f,  671.987854f,  452.751953f,  670.892517f,  438.108551f,
        682.836670f,  445.058197f,  673.050781f,  443.037842f,  674.070862f,  434.201691f,
        683.209229f,  435.058472f,  676.385010f,  445.134399f,  665.957153f,  452.188995f,
        691.069275f,  438.960266f,  696.535034f,  442.173889f,  694.157349f,  447.540039f,
        688.761536f,  438.574921f,  694.546143f,  433.262146f,  699.308472f,  434.453247f,
        705.616882f,  432.023010f,  692.421387f,  456.076050f,  702.294495f,  456.161774f,
        701.293518f,  474.457855f,  688.167053f,  474.047516f,  691.530823f,  471.150208f,
        693.024780f,  474.767792f,  695.740967f,  457.441345f,  701.344238f,  460.123627f,
        700.256165f,  467.910187f,  701.164917f,  481.997620f,  698.642944f,  500.686584f,
        688.087830f,  493.789917f,  688.615662f,  505.449707f,  690.404297f,  513.256104f,
        687.490723f,  518.950989f,  679.943481f,  497.118164f,  704.274719f,  473.740997f,
        706.489563f,  475.670105f,  711.546997f,  511.560211f,  696.664551f,  512.616943f,
        692.836487f,  510.341370f,  700.363220f,  529.740662f,  687.876160f,  495.624939f,
        695.681763f,  478.474670f,  702.549438f,  495.091949f,  688.794006f,  475.897919f,
        701.136719f,  492.907990f,  694.266907f,  462.456207f,  703.017456f,  452.583618f,
        711.148438f,  465.471100f,  708.529297f,  446.254120f,  722.009949f,  521.615906f,
        691.251282f,  1231.885498f, 1024.171875f, 1227.536377f, 1040.379150f, 1223.584229f,
        1041.591675f, 1223.505005f, 1040.275391f, 1228.368774f, 1038.200195f, 1191.128906f,
        1067.275391f, 1181.666382f, 1012.109131f, 1204.472534f, 989.568909f,  1239.977783f,
        1036.183838f, 1244.650757f, 1027.640625f, 1236.338379f, 1061.500977f, 1231.765381f,
        1060.920166f, 1228.016357f, 1071.844116f, 1233.125977f, 1012.849548f, 1242.464233f,
        974.963013f,  1212.020630f, 1048.437988f, 1224.728882f, 1088.571899f, 1228.553955f,
        1090.736572f, 1222.951904f, 1091.693970f, 1222.471680f, 1096.608032f, 1226.899414f,
        1088.119507f, 1221.679321f, 1105.350952f, 1236.373291f, 1060.222778f, 1222.476318f,
        1080.347290f, 1222.619629f, 1102.151001f, 1223.312744f, 1114.278687f, 1213.677124f,
        1134.694458f, 1214.763794f, 1146.605591f, 1216.757080f, 1145.439453f, 1211.476929f,
        1160.220093f, 1217.759888f, 1142.310425f, 1211.757812f, 1162.597656f, 1231.072388f,
        1108.342163f, 1204.026489f, 1172.095459f, 1203.823120f, 1194.365601f, 1196.431030f,
        1187.634644f, 1193.611328f, 1204.831177f, 1206.086060f, 1195.291626f, 1218.626587f,
        1170.560303f, 1216.462036f, 1190.611938f, 1230.841187f, 1139.987549f, 1242.504150f,
        1099.122192f, 1197.997192f, 1201.636597f, 1183.817993f, 1215.283813f, 1186.905884f,
        1252.290527f, 1250.233398f, 1161.899536f, 1265.477051f, 1125.220825f, 1205.748169f,
        1204.892090f, 1212.778442f, 1130.919556f, 1191.187988f, 1153.783447f, 1235.069458f,
        1144.923462f, 1182.807861f, 1225.764648f, 1198.263062f, 1220.780884f, 1211.983643f,
        1165.165894f, 1202.857544f, 1191.500000f, 1270.171997f, 1119.898315f, 1220.942993f,
        1122.667969f, 1263.183350f, 1084.540283f, 1198.786743f, 1190.514282f, 1186.258423f,
        1195.251465f, 1191.782959f, 1210.421753f, 1257.340576f, 1159.972290f, 1201.277588f,
        1178.144897f, 1214.819702f, 1181.248291f, 61.369740f,   -41.028259f,  24.161716f,
        -12.872226f,  -38.333687f,  1.693691f,    -58.589024f,  -9.782020f,   -35.471127f,
        -19.462009f,  -54.211418f,  8.531075f,    23.903738f,   -0.302051f,   55.561367f,
        -23.587637f,  28.168798f,   -28.645651f,  -22.195492f,  -8.960397f,   -78.471313f,
        2.972276f,    -118.756805f, 12.158041f,   -98.890991f,  4.800930f,    -63.129345f,
        1.710313f,    -4.338565f,   -9.500677f,   22.734045f,   -22.070171f,  -28.379225f,
        -26.407478f,  -37.404549f,  -8.804842f,   -113.573921f, 7.083610f,    -117.971237f,
        7.367099f,    -118.288895f, 7.316282f,    -114.856583f, 10.883091f,   -67.540489f,
        -8.719220f,   -43.722328f,  -29.324329f,  23.931715f,   -35.732937f,  -7.776310f,
        -19.755215f,  -56.510113f,  -3.362521f,   -118.855652f, 8.504073f,    -97.462318f,
        3.712321f,    -110.534866f, 8.196481f,    -65.235245f,  -13.622937f,  -105.784920f,
        -26.771509f,  45.167782f,   -23.721918f,  38.232727f,   -14.423880f,  -7.039620f,
        3.426570f,    -10.636659f,  -4.580351f,   -69.622101f,  10.815468f,   23.431847f,
        -13.522975f,  14.915241f,   -25.481951f,  -87.420769f,  -11.282286f,  78.884933f,
        -1.008186f,   77.433655f,   -3.390567f,   4.071245f,    13.091332f,   42.190506f,
        -3.196981f,   141.583862f,  -30.923988f,  119.705933f,  7.668843f,    125.503563f,
        -16.170204f,  6.259509f,    1.928330f,    73.627327f,   -7.935602f,   185.890930f,
        -36.230740f,  128.475845f,  -18.227808f,  144.163010f,  -36.791149f,  -30.729237f,
        -3.269900f,   195.281342f,  -50.121185f,  64.087631f,   26.644068f,   70.511162f,
        -47.466038f,  116.005089f,  -53.048725f,  190.122925f,  -53.777527f,  137.479767f,
        -17.991459f,  63.099606f,   -64.078941f,  96.497093f,   -83.223824f,  246.342148f,
        -81.149796f,  -69.496414f,  38.748428f,   314.830566f,  -131.488144f, 0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.618398f,    79.008385f,   16.725714f,
        82.031700f,   33.034149f,   63.023903f,   25.312441f,   162.172516f,  46.949123f,
        108.918022f,  98.596657f,   54.187878f,   105.033501f,  62.843559f,   36.016827f,
        74.365051f,   18.198515f,   100.728569f,  1.695963f,    93.398521f,   16.411110f,
        106.893509f,  39.818638f,   96.532410f,   15.600206f,   120.221771f,  78.342728f,
        83.165184f,   80.064644f,   71.556870f,   48.064564f,   73.240082f,   54.797302f,
        138.905136f,  31.940023f,   144.023041f,  44.104630f,   137.401993f,  61.855125f,
        109.946144f,  93.112495f,   109.464409f,  72.078323f,   124.338425f,  48.941662f,
        124.908974f,  79.910507f,   108.113556f,  52.011665f,   239.793716f,  65.754921f,
        248.223343f,  80.254364f,   195.936798f,  97.962288f,   203.138992f,  110.866295f,
        165.046219f,  123.061028f,  130.303558f,  104.174400f,  132.544830f,  129.315002f,
        169.149750f,  37.011551f,   362.880371f,  35.519768f,   383.491333f,  69.375603f,
        301.830780f,  96.178856f,   272.926239f,  196.597275f,  181.953384f,  159.287537f,
        161.385361f,  177.925491f,  140.623535f,  160.189255f,  179.893600f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    -47.169628f,
        100.192329f,  -44.801735f,  91.618515f,   -28.191143f,  67.835594f,   -3.506720f,
        64.124649f,   19.443945f,   67.428574f,   19.836529f,   58.789120f,   27.025572f,
        42.685734f,   -52.921574f,  74.096184f,   -25.740509f,  109.056358f,  3.383283f,
        79.598221f,   -2.313760f,   87.374222f,   -40.453865f,  88.557350f,   -12.100625f,
        79.249733f,   -6.197131f,   79.387604f,   25.553057f,   71.318817f,   3.136025f,
        66.234886f,   38.045677f,   132.639038f,  60.244015f,   117.783974f,  21.195208f,
        107.500000f,  47.309162f,   90.528954f,   27.899488f,   102.657677f,  50.380199f,
        108.135071f,  26.279148f,   138.792145f,  32.247978f,   110.117355f,  119.395691f,
        202.477768f,  121.076088f,  187.825348f,  103.480804f,  167.815491f,  103.308060f,
        132.744736f,  131.529373f,  121.996803f,  123.713692f,  134.323074f,  53.257641f,
        176.644775f,  72.661324f,   159.749084f,  206.492920f,  275.873962f,  238.845932f,
        264.308533f,  242.101135f,  221.338730f,  218.962601f,  195.090042f,  289.853943f,
        149.308823f,  298.996887f,  151.184708f,  233.627014f,  193.832336f,  164.545959f,
        206.224243f,  442.125824f,  263.174286f,  421.674164f,  300.343048f,  362.397675f,
        313.370575f,  408.240509f,  222.062317f,  357.270874f,  209.292831f,  300.731842f,
        226.205978f,  181.800018f,  252.994003f,  201.585159f,  256.164948f,  0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,    0.000000f,
        0.000000f,    0.000000f,    0.000000f,    11.341336f,   7.354125f,    26.039732f,
        59.608742f,   30.411585f,   7.234879f,    5.780099f,    17.909994f,   -0.383619f,
        -0.330520f,   -0.270907f,   -0.166038f,   -0.150920f,   0.071461f,    31.475607f,
        6.097769f,    38.005722f,   -4.293477f,   25.547703f,   -0.724081f,   15.822425f,
        -1.696028f,   0.000000f,    0.000000f,    -9.838127f,   -1.011879f,   -22.191319f,
        -3.340419f,   -43.652393f,  59.841537f,   -21.138748f,  -8.990250f,   27.896252f,
        -6.140539f,   35.336353f,   -19.139317f,  18.277031f,   -5.585634f,   13.132952f,
        -2.244741f,   0.000000f,    0.000000f,    -8.720009f,   2.395998f,    -16.334986f,
        2.379356f,    -36.081722f,  38.319130f,   -38.592144f,  -25.293524f,  29.484932f,
        60.952076f,   -2.513120f,   -6.410553f,   -29.409376f,  -15.030966f,  18.635595f,
        2.607402f,    34.338158f,   1.939508f,    20.283131f,   15.290606f,   34.156879f,
        -29.781080f,  7.248896f,    -61.120445f,  0.878369f,    23.225595f,   -95.430016f,
        45.955219f,   -125.612625f, 33.202293f,   7.148687f,    -25.781603f,  0.617864f,
        -7.531545f,   0.000000f,    0.000000f,    -1.321614f,   13.276786f,   -5.725983f,
        40.666378f,   -9.797519f,   64.291275f,   -16.004480f,  109.610634f,  -247.924774f,
        94.140060f,   -151.718414f, 39.982800f,   21.110403f,   -34.571602f,  26.933529f,
        -31.369663f,  0.000000f,    0.000000f,    -8.883981f,   28.856205f,   -10.035532f,
        54.604160f,   -42.055351f,  104.455444f,  -75.470543f,  125.048172f,  -78.901299f,
        19.673117f,   140.636780f,  -204.369354f, -108.456131f, 25.640568f,   -90.762184f,
        38.077030f,   0.000000f,    0.000000f,    77.347809f,   -33.810596f,  152.066574f,
        -99.191650f,  287.177460f,  -209.020065f, 429.013916f,  -329.941315f, -0.202252f,
        2.020256f,    4.948848f,    1.210987f,    3.654148f,    2.022504f,    1.939197f,
        0.022958f,    -6.327742f,   -40.580315f,  -5.061973f,   -13.839694f,  -3.135507f,
        -4.265285f,   -2.275661f,   -1.244766f,   0.000000f,    0.000000f,    4.069341f,
        -0.893586f,   8.116845f,    -0.600223f,   -4.624782f,   1.216673f,    -62.915615f,
        5.042915f,    3.751762f,    -39.417694f,  5.540482f,    -13.184116f,  5.588696f,
        -8.631001f,   2.253985f,    -1.471676f,   0.000000f,    0.000000f,    -6.024684f,
        4.135754f,    -9.926563f,   2.529159f,    -3.981733f,   1.030060f,    -5.174264f,
        -1.667462f,   10.729550f,   -35.695412f,  9.244300f,    -12.300451f,  6.767049f,
        -7.832577f,   4.620152f,    -3.688972f,   0.000000f,    0.000000f,    -1.106728f,
        0.496495f,    -3.172386f,   2.075857f,    -3.109593f,   -0.386625f,   -0.069261f,
        -4.983466f,   -11.485346f,  -119.359360f, 0.239686f,    -66.555504f,  1.804124f,
        -38.796436f,  1.751823f,    -17.905106f,  0.000000f,    0.000000f,    -1.017570f,
        6.639958f,    -4.954022f,   16.946598f,   -5.900111f,   21.178160f,   -5.774418f,
        28.261063f,   6.517031f,    -1.617736f,   1.484958f,    1.741585f,    1.384400f,
        -1.090894f,   2.617177f,    -0.771704f,   1.853417f,    -2.893428f,   5.820073f,
        -1.725828f,   5.075033f,    0.452145f,    5.865297f,    11.116379f,   -4.447809f,
        8.946952f,    -2.002396f,   -3.838429f,   0.582087f,    0.301839f,    -2.953493f,
        -0.763491f,   4.802984f,    -5.554667f,   4.266141f,    -7.824973f,   0.948019f,
        -3.697573f,   -1.047686f,   -5.021234f,   -7.006489f,   -5.613669f,   0.986543f,
        -7.478756f,   20.000000f,   10.000000f,   -1.263783f,   -6.455718f,   -7.371679f,
        -40.687706f,  8.493212f,    -17.676943f,  7.136734f,    3.694832f,    -38.707706f,
        -30.005154f,  -1.447165f,   -12.222742f,  3.902585f,    11.771508f,   -41.222431f,
        -32.157482f,  -13.402482f,  -5.415693f,   5.271189f,    9.490350f,    -50.336853f,
        -6.992504f,   -4.035035f,   -2.467246f,   1.984116f,    32.706188f,   -61.081047f,
        -46.204056f,  -44.390762f,  -0.608304f,   0.000000f,    0.000000f,    -11.801809f,
        -2.940617f,   -17.125109f,  0.889785f,    -11.448219f,  1.936625f,    3.207811f,
        -3.361319f,   13.781226f,   -6.020318f,   62.118942f,   -16.137531f,  135.184723f,
        -27.052151f,  227.059052f,  -52.561111f,  1.182282f,    0.392114f,    41.559841f,
        -1.316391f,   24.897152f,   0.135663f,    26.865280f,   -3.941488f,   26.419203f,
        -5.123530f,   0.531429f,    5.872722f,    0.752264f,    3.381307f,    0.853467f,
        2.360752f,    1.000000f,    1.000000f,    1.393496f,    1.803454f,    1.419142f,
        0.102034f,    0.331390f,    -7.030445f,   -11.473533f,  0.856797f,    -22.955612f,
        -49.882847f,  -26.441448f,  -41.373375f,  -55.604622f,  -22.871319f,  -20.257124f,
        -26.043509f,  -6.744742f,   8.229078f,    -6.898763f,   3.773464f,    -15.237773f,
        27.861610f,   0.000000f,    0.000000f,    -0.801665f,   -3.364029f,   -7.294553f,
        -11.736492f,  -24.884117f,  -8.695228f,   -32.868462f,  -9.081800f,   -65.938828f,
        28.310844f,   -108.638237f, 71.106567f,   0.000000f,    0.000000f,    -18.656349f,
        -7.908713f,   -55.484814f,  -6.297475f,   -94.799698f,  3.359365f,    -148.931641f,
        64.782349f,   -297.080780f, 222.516998f,  -213.412094f, 119.440002f};

std::string evalCalcTToString(const EvalCalcT term) {
    return std::format("{:>6.1f}", term);
}

std::string taperedTermToString(const TaperedTerm& term) {
    return std::format("{{{:>6.1f}, {:>6.1f}}}", term.early, term.late);
}

std::string getPstPieceTypeName(int idx) {
    if (idx == EvalParams::kCandidatePassedPawnPstIdx) {
        return "Candidate";
    } else if (idx == EvalParams::kPassedPawnPstIdx) {
        return "Passed pawn";
    } else {
        return pieceToString((Piece)idx);
    }
}

std::string getTropismPieceTypeName(int idx) {
    if (idx == EvalParams::kDoubledPawnTropismIdx) {
        return "Doubled pawn";
    } else if (idx == EvalParams::kIsolatedPawnTropismIdx) {
        return "Isolated pawn";
    } else if (idx == EvalParams::kPassedPawnTropismIdx) {
        return "Passed pawn";
    } else if (idx == EvalParams::kCandidatePassedPawnTropismIdx) {
        return "Candidate";
    } else {
        return pieceToString((Piece)idx);
    }
}

void writeSquareTable(const EvalParams::SquareTable& squareTable, std::ostream& out) {
    for (int rank = kRanks - 1; rank >= 0; --rank) {
        out << "\t\t";
        for (int file = 0; file < kFiles; ++file) {
            const auto& value = squareTable[rank * kFiles + file];
            out << std::format("{}, ", taperedTermToString(value));
        }
        out << "\n";
    }
}

void writePieceSquareTables(
        const EvalParams::PieceSquareTables& pieceSquareTables, std::ostream& out) {
    for (int pieceIdx = 0; pieceIdx < (int)pieceSquareTables.size(); ++pieceIdx) {
        out << "\t" << getPstPieceTypeName(pieceIdx) << ": {\n";
        writeSquareTable(pieceSquareTables[pieceIdx], out);
        out << "\t}\n";
    }
}

void writeTropismTable(
        const std::array<TaperedTerm, EvalParams::kNumTropismPieceTypes>& tropism,
        std::ostream& out) {
    for (int pieceIdx = 0; pieceIdx < (int)EvalParams::kNumTropismPieceTypes; ++pieceIdx) {
        out << std::format(
                "\t{}: {}\n",
                getTropismPieceTypeName(pieceIdx),
                taperedTermToString(tropism[pieceIdx]));
    }
}

template <std::size_t N>
std::string arrayToString(const std::array<TaperedTerm, N>& valueArray) {
    std::string inner =
            valueArray | std::ranges::views::transform(taperedTermToString) | joinToString(", ");
    return "{" + inner + "}";
}

template <std::size_t N>
std::string arrayToString(const std::array<EvalCalcT, N>& valueArray) {
    std::string inner =
            valueArray | std::ranges::views::transform(evalCalcTToString) | joinToString(", ");
    return "{" + inner + "}";
}

void writeTaperedTermPerPiece(
        const std::array<TaperedTerm, kNumPieceTypes>& terms, std::ostream& out) {
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes; ++pieceIdx) {
        out << std::format(
                "\t{}: {}\n", pieceToString((Piece)pieceIdx), taperedTermToString(terms[pieceIdx]));
    }
}

}  // namespace

EvalParams EvalParams::getEmptyParams() {
    return EvalParams{};
}

EvalParams EvalParams::getDefaultParams() {
    return evalParamsFromArray(kDefaultParams);
}

EvalParamArray evalParamsToArray(const EvalParams& params) {
    EvalParamArray array;
    std::memcpy(array.data(), &params, sizeof(EvalParams));
    return array;
}

EvalParams evalParamsFromArray(const EvalParamArray& array) {
    EvalParams params = EvalParams::getEmptyParams();
    std::memcpy(&params, array.data(), sizeof(EvalParams));
    return params;
}

std::string evalParamsToString(const EvalParams& params) {
    std::ostringstream oss;

    oss << "phaseMaterialValues: {\n";
    for (int pieceIdx = 0; pieceIdx < (int)EvalParams::kNumPstPieceTypes; ++pieceIdx) {
        oss << std::format(
                "\t{}: {:>6.1f}\n",
                getPstPieceTypeName(pieceIdx),
                params.phaseMaterialValues[pieceIdx]);
    }
    oss << "}\n";

    oss << std::format(
            "\noppositeColoredBishopFactor:\n\t{}",
            arrayToString(params.oppositeColoredBishopFactor));

    oss << std::format("\nsingleMinorFactor:\n\t{}", evalCalcTToString(params.singleMinorFactor));
    oss << std::format("\ntwoKnightsFactor:\n\t{}", evalCalcTToString(params.twoKnightsFactor));
    oss << std::format("\nrookVsMinorFactor:\n\t{}", evalCalcTToString(params.rookVsMinorFactor));
    oss << std::format(
            "\nrookAndMinorVsRookFactor:\n\t{}",
            evalCalcTToString(params.rookAndMinorVsRookFactor));

    oss << std::format("\nhasUnstoppablePawn:\n\t{}", evalCalcTToString(params.hasUnstoppablePawn));

    oss << "\npieceValues: {\n";
    writeTaperedTermPerPiece(params.pieceValues, oss);
    oss << "}\n";

    oss << "\npieceSquareTables: {\n";
    writePieceSquareTables(params.pieceSquareTables, oss);
    oss << "}\n";

    oss << std::format(
            "\nprotectedPawnBonus:\n\t{}", taperedTermToString(params.protectedPawnBonus));

    oss << std::format(
            "\nconnectedPassedPawnBonus:\n\t{}",
            taperedTermToString(params.connectedPassedPawnBonus));

    oss << std::format(
            "\ndoubledPawnPenalty:\n\t{}", taperedTermToString(params.doubledPawnPenalty));

    oss << std::format(
            "\nisolatedPawnPenalty:\n\t{}", taperedTermToString(params.isolatedPawnPenalty));

    oss << std::format(
            "\npasserMechanicalObstructionFactor:\n\t{}",
            taperedTermToString(params.passerMechanicalObstructionFactor));

    oss << std::format(
            "\npasserDynamicObstructionFactor:\n\t{}",
            taperedTermToString(params.passerDynamicObstructionFactor));

    oss << std::format(
            "\npasserSacrificialObstructionFactor:\n\t{}",
            taperedTermToString(params.passerSacrificialObstructionFactor));

    oss << std::format(
            "\nbishopPawnSameColorAdjustment:\n\t{}",
            arrayToString(params.bishopPawnSameColorAdjustment));

    oss << std::format(
            "\nbishopEnemyPawnSameColorAdjustment:\n\t{}",
            arrayToString(params.bishopEnemyPawnSameColorAdjustment));

    oss << std::format("\nbishopPairBonus:\n\t{}", taperedTermToString(params.bishopPairBonus));

    oss << std::format("\nknightPairBonus:\n\t{}", taperedTermToString(params.knightPairBonus));

    oss << std::format("\nrookPairBonus:\n\t{}", taperedTermToString(params.rookPairBonus));

    oss << std::format(
            "\nrookSemiOpenFileBonus:\n\t{}", taperedTermToString(params.rookSemiOpenFileBonus));

    oss << std::format("\nrookOpenFileBonus:\n\t{}", taperedTermToString(params.rookOpenFileBonus));

    oss << std::format(
            "\nownRookBehindPassedPawn:\n\t{}",
            taperedTermToString(params.ownRookBehindPassedPawn));

    oss << std::format(
            "\nownRookInFrontOfPassedPawn:\n\t{}",
            taperedTermToString(params.ownRookInFrontOfPassedPawn));

    oss << std::format(
            "\nenemyRookBehindPassedPawn:\n\t{}",
            taperedTermToString(params.enemyRookBehindPassedPawn));

    oss << std::format(
            "\nenemyRookInFrontOfPassedPawn:\n\t{}",
            taperedTermToString(params.enemyRookInFrontOfPassedPawn));

    oss << std::format("\nknightPawnAdjustment:\n\t{}", arrayToString(params.knightPawnAdjustment));

    oss << std::format("\nrookPawnAdjustment:\n\t{}", arrayToString(params.rookPawnAdjustment));

    oss << std::format("\nqueenPawnAdjustment:\n\t{}", arrayToString(params.queenPawnAdjustment));

    oss << "\n\nmobilityBonus: {\n";
    for (int pieceIdx = 1; pieceIdx < kNumPieceTypes - 1; ++pieceIdx) {
        oss << std::format(
                "\t{}: {}\n",
                pieceToString((Piece)pieceIdx),
                taperedTermToString(params.mobilityBonus[pieceIdx - 1]));
    }
    oss << "}\n";

    oss << "\n\nsafeMobilityAdjustment: {\n";
    for (int pieceIdx = 1; pieceIdx < kNumPieceTypes - 1; ++pieceIdx) {
        oss << std::format(
                "\t{}: {}\n",
                pieceToString((Piece)pieceIdx),
                arrayToString(params.safeMobilityAdjustment[pieceIdx - 1]));
    }
    oss << "}\n";

    oss << "\nownKingTropism: {\n";
    writeTropismTable(params.ownKingTropism, oss);
    oss << "}\n";

    oss << "\nenemyKingTropism: {\n";
    writeTropismTable(params.enemyKingTropism, oss);
    oss << "}\n";

    oss << std::format("\ntempoBonus:\n\t{}", taperedTermToString(params.tempoBonus));

    oss << "\n\nattackDefendAdjustment: {\n";
    for (int pieceIdx = 0; pieceIdx < (int)params.attackDefendAdjustment.size(); ++pieceIdx) {
        oss << std::format(
                "\t{}: {}\n",
                pieceToString((Piece)pieceIdx),
                arrayToString(params.attackDefendAdjustment[pieceIdx]));
    }
    oss << "}\n";

    oss << std::format("\ncontrolNearEnemyKing:\n\t{}", arrayToString(params.controlNearEnemyKing));

    oss << "\n\nkingAttackWeight: {\n";
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes - 1; ++pieceIdx) {
        oss << std::format(
                "\t{}: {}\n",
                pieceToString((Piece)pieceIdx),
                taperedTermToString(params.kingAttackWeight[pieceIdx]));
    }
    oss << "}\n";

    oss << std::format(
            "\nattackersMinusDefendersFactor:\n\t{}",
            arrayToString(params.attackersMinusDefendersFactor));

    oss << "\n\npiecePinnedAdjustment: {\n";
    for (int pieceIdx = 0; pieceIdx < kNumPieceTypes - 1; ++pieceIdx) {
        oss << std::format(
                "\t{}: {}\n",
                pieceToString((Piece)pieceIdx),
                taperedTermToString(params.piecePinnedAdjustment[pieceIdx]));
    }
    oss << "}\n";

    oss << std::format(
            "\nkingOpenFileAdjustment:\n\t{}", taperedTermToString(params.kingOpenFileAdjustment));
    oss << std::format(
            "\nkingFlankOpenFileAdjustment:\n\t{}",
            taperedTermToString(params.kingFlankOpenFileAdjustment));
    oss << std::format(
            "\npassedPawnOutsideKingSquare:\n\t{}",
            taperedTermToString(params.passedPawnOutsideKingSquare));

    oss << std::format(
            "\ndefendedChecksAdjustment:\n\t{}", arrayToString(params.defendedChecksAdjustment));

    oss << std::format(
            "\nundefendedChecksAdjustment:\n\t{}",
            arrayToString(params.undefendedChecksAdjustment));

    return oss.str();
}
