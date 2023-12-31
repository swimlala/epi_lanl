CDF       
      	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       	DATE_TIME         N_PROF        N_PARAM       N_LEVELS  !   N_CALIB       	N_HISTORY            	   title         Argo float vertical profile    institution       BODC   source        
Argo float     history       14-Jun-2023 08:20:22Zcreation      
references        (http://www.argodatamgt.org/Documentation   comment       bThis netCDF file is generated using BODC's argoReader and netCDF writer software (argo@bodc.ac.uk)     user_manual_version       3.4    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    <H   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    <X   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    <\   REFERENCE_DATE_TIME                	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    <`   DATE_CREATION                  	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    <p   DATE_UPDATE                	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    <�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    <�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  <�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  <�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  =   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        =H   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    =L   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    =P   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     =T   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    =t   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    =x   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     =|   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     =�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     =�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    =�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
_FillValue        A.�~       
resolution        >�E�vQ�        =�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    =�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
_FillValue        A.�~       
resolution        >�E�vQ�        =�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            =�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            =�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    >   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    >   VERTICAL_SAMPLING_SCHEME                   	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    >   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ?   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ?   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ?   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ?   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                    	valid_max         @�p        axis      Z      
_FillValue        G�O�   C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�������     �  ?    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @          	valid_max         @D�        
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  G�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �         	valid_max         @D         
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  P(   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  X�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  Z�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 $  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                    	valid_max         @�p        conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�������     �  _   PSAL_ADJUSTED            
      
   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @          	valid_max         @D�        conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  g�   TEMP_ADJUSTED            
      
   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �         	valid_max         @D         conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  p    PRES_ADJUSTED_QC         
         	long_name         quality flag   standard_name         PRES_ADJUSTED_QC   conventions       Argo reference table 2     
_FillValue                 $  x�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   standard_name         PSAL_ADJUSTED_QC   conventions       Argo reference table 2     
_FillValue                 $  z�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   standard_name         TEMP_ADJUSTED_QC   conventions       Argo reference table 2     
_FillValue                 $  |�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     standard_name         PRES_ADJUSTED_ERROR    units         decibar    conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %7.1f      FORTRAN_format        F7.1   
resolution        ?�������     �     PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     standard_name         PSAL_ADJUSTED_ERROR    units         psu    conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     standard_name         TEMP_ADJUSTED_ERROR    units         degree_Celsius     conventions       Argo reference table 2     
_FillValue        G�O�   C_format      %9.3f      FORTRAN_format        F9.3   
resolution        ?PbM���     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    source_name       	PARAMETER      conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	             	long_name         'Calibration equation for this parameter    source_name       SCIENTIFIC_CALIB_EQUATION      
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	             	long_name         *Calibration coefficients for this equation     source_name       SCIENTIFIC_CALIB_COEFFICIENT   
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	             	long_name         .Comment applying to this parameter calibration     source_name       SCIENTIFIC_CALIB_COMMENT   
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	            	long_name         Date of calibration    source_name       SCIENTIFIC_CALIB_DATE      conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     source_name       HISTORY_INSTITUTION    conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    source_name       HISTORY_STEP   conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    source_name       HISTORY_SOFTWARE   conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     source_name       HISTORY_SOFTWARE_RELEASE   conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      source_name       HISTORY_REFERENCE      conventions       Institution dependent      
_FillValue                  �  �(   HISTORY_DATE                     	long_name         #Date the history record was created    source_name       HISTORY_DATE   conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_ACTION                       	long_name         Action performed on data   source_name       HISTORY_ACTION     conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   source_name       HISTORY_PARAMETER      conventions       Argo reference table 3     
_FillValue                  0  �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   source_name       HISTORY_START_PRES     units         decibar    
_FillValue        G�O�        �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    source_name       HISTORY_STOP_PRES      units         decibar    
_FillValue        G�O�        �\   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    source_name       HISTORY_PREVIOUS_VALUE     
_FillValue        G�O�        �h   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   source_name       HISTORY_QCTEST     conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                  0  �tArgo profile    3.1 1.2 19500101000000  20230614082029  20230614082029  1901924 Argo UK                                                         Jon Turton                                                      PSAL            TEMP            PRES               cA   BO  165788                          2B  A   APEX                            8981                            2.13.1.1                        846 @�2�Vx��1   @�2�Vx���C֍���@D�}�H�1   GPS     Primary sampling: mixed                                                                                                                                                                                                                                            :A   A   A   @�
=@���A z�A z�A@  A`  A�Q�A�{A��A�A�  A�=qA��A�B 
=B��B�B�
B�HB'�
B/�
B7�RB?��BG�RBO�BX{B_��Bh
=Bp�Bx{B�B�
=B��B�{B�B���B��B���B�B�  B�\B�
=B�B�
=B�B�
=B���B���B��B��B�
=B�{B�  B���B�\B�\B�  B��B�B�  B�  B�\C C�C
=C  C��C	�RC�qC�qC�3C��C�3C�RC  C�qC�qC�C 
=C"�C$
=C&  C'�RC)��C+��C-��C0C2�C4  C6  C8C:
=C<�C=��C?��CB  CD�CF�CH
=CJ�CK��CNCP\CR�CT�CU��CW��CY��C\C^
=C`�Cb�Cc�RCe��Ch  Cj  ClCnCo��Cr�Ct�Cu�qCxCz  C|  C~�C��C�HC���C�HC��C��C�C�  C��C��C�HC��C��C�  C��C��C�HC�HC��C�HC�  C���C��)C���C��qC�HC�HC��C�fC���C���C��)C���C�  C���C���C�  C�HC�HC��C���C��)C��qC���C��C�HC���C�HC���C��)C�HC�C��C�HC�C�C�  C���C���C��C�HC��qC��)C��qC�HC���C��qC��C��C�  C���C�  C�  C��qC��qC���C��C��C��C�HC��C�C��C��C��C���C��)C��qC�HC�fC��C��C�  C���C���C�HC�  C�HC�  C��C�HC��)C��qC���C���C��C�C��C��C�HC�HC�  C��C���C��)C�HC���C��)C��qC��qC��)C���C���C���C��C��C�  C�HD �D \D �D��D�D� DHD��DHD~�D�\D\D��D� D�D��D  D~�D	HD	� D	��D
�HD �D~�D�\D� D �D��D  D~�D�\D� D  D~D��D~�D�\D\D  D\D��D~�D�\D~�D��D~D  D��D �D�HDHD~�D�qD\D�D��D �D\D �D�3DHD��DHD�HD �D �HD �\D!\D" �D"� D"�\D#� D$�D$��D%�D%~D%�qD&}qD&��D'� D(  D(}qD(��D)}qD)�D*� D+�D+��D,�D,��D-  D-\D-�\D.}qD.�D/� D0 �D0��D1HD1��D2  D2~D3 �D3��D4 �D4�HD5HD5��D6HD6\D7 �D7�HD8 �D8��D9  D9� D:�D:��D; �D;��D<�D<\D<��D=��D> �D>� D>�\D?\D?�\D@� D@��DA~�DB  DB� DB�DC|�DC�DD~DD�DE� DF  DF}qDF�DG~�DG�\DH\DI�DI� DJ  DJ��DKHDK�HDL �DL\DM  DM��DNHDN� DOHDO� DO�\DP��DQHDQ\DR  DR��DSHDS�HDS��DT|�DT�qDU~�DVHDV��DW �DW�HDX  DX\DX�\DY~�DY�\DZ�HDZ��D[~�D[�\D\~�D\��D]~�D^  D^� D_HD_�HD`HD`��D`�\Da\Da�\Db��Dc�Dc��Dc�\Dd\De  De�HDe�\Df}qDf�qDg�HDhHDh��DiHDi��Dj �Dj��DkHDk~Dk�\Dl��DmHDm��Dn�Dn\Dn�\Do�HDo�\Dp� Dp��Dq\Dr  Dr��DsHDs� Ds�\Dt\Du  Du��DvHDv\Dv��Dw\Dx �Dx� Dx��D{��D~1HD�=D��fD���D�qD�Y�D��fD��qD�fD�VD��qD���D��D�x D��RD��D�G\D��\D��=D�J=D�ǮD�@�D��=D�O
D��)D�L)D��RD�@�D�˅D�G\D��HD�<)D��D�K3D��3D�NfD��qD�?
DǊ�D��D�IHDڇ�D��3D��D�<�D�t{D��3B�B��B��B�B��B�$B�RB�B��B�B��B�B�@B�:B��B�&B�&B�B��B��B�B�2B� B�,B�vB�nB��BؓBԯB��BɆB�dB�4B��B�xB�8B�	B�WB�B��B�0B�CB�[B�=B͹B�jB̳B�B�_B��B�B��B�8B��B��B�wB��B�/B�[B�jB� ByrB� B��B��B�4B��B��B��B�`B�CB��B~Bt�Bv�Bt�B�7B� B��B�1B�mB��B��B�vB�XB��B�'B}�BwfBshBo5Be�B[�BN�BA;B8lB4B3B3B3�B5%B;JB>�BEmBC�BA�B>BB<B5ZB2B/�B-wB)�B BB'mB%,B�BmBB�B%�B�BB�B�.B�xB�B�UB�B�2B�3B�8B��B��B��B�'B��B�fB�(B��B�B�<B��B�zB��BǔB�[BāBðB�-B��BňB�aB�lB��B�RB��B��B��B�4B�HB��B��B�B~]B|PB{0BzDBw�BqBoBj�Bh>Bg8BgBg8Be�Bb4Ba-B^BW?BQhBJ=BG�BG�BH�BK�BMBN�BOvBP�BP�BO(BK�BHBE9BCGB=�B;�B;B:*B6�B,�B(
B'B&2B"B 'B�B�B�B�B�B�BVB�BjB�BxBB�B
�VB
��B
�B
�B
�AB
�CB
�8B
�B
�|B
��B
ܒB
��B
��B
�=B
��B
�[B
�<B
��B
�B
�B
ƨB
��B
�<B
��B
��B
�[B
�bB
��B
��B
��B
��B
��B
�B
��B
�B
��B
��B
�aB
�[B
�B
}B
|jB
z�B
y>B
x8B
v�B
t�B
pUB
l=B
i�B
g�B
e�B
d�B
d&B
c B
`vB
\�B
Y�B
W?B
UB
Q�B
N�B
N"B
L�B
IRB
H1B
GEB
FB
D�B
A�B
=�B
:�B
7�B
5B
3B
/�B
+�B
%�B
�B
=B
�B
�B
�B
B
2B
�B
�B
PB
�B
	RB
zB
YB
mB
B
�B
 �B	��B	�LB	��B	�-B	��B	�AB	��B	�;B	�OB	��B	�B	��B	�*B	�XB	�B	�LB	� B	��B	�jB	ݘB	��B	�B	��B	�[B	�PB	��B	�xB	�B	��B	�YB	��B	�'B	�BB	�<B	�JB	�	B	��B	�?B	��B	��B	�IB	��B	�zB	�@B	��B	�HB	�;B	��B	��B	��B	�yB	�sB	�mB	�B	�[B	� B	�hB	��B	��B	�vB	��B	��B	�"B	�6B	��B	��B	��B	�B	�EB	��B	��B	��B	y$B	vB	t�B	sB	p�B	pB	l=B	k�B	l"B	lWB	l�B	m�B	p�B	q�B	s�B	y�B	|jB	}�B	cB	��B	�'B	�'B	�'B	�uB	��B	��B	�uB	�[B	�{B	�MB	�3B	�B	�B	��B	�GB	��B	��B	.B	|�B	}�B	y�B	y>B	y>B	y$B	z�B	z�B	z�B	z*B	{�B	{�B	{B	{B	{JB	{�B	}"B	~BB	~�B	�B	�UB	��B	��B	��B	�B	�B	��B	�AB	~�B	|jB	}qB	~�B	B	�B	�iB	�;B	��B	��B	��B	�B	��B	�fB	�XB	��B	��B	�\B	�}B	�oB	��B	��B	��B	�)B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�KB	��B	�B	�QB	�"B	��B	�}B	�oB	�zB	�B	�AB	�tB	��B	�~B	�pB	��B	�aB	�B	��B	�B	�B	��B	�5B	��B	�B	��B	�6B
�B
PB
$ZB
4�B
=�B
EmB
J�B
J�B
MB
Y1B
f�B
s�B
zxB
� B
��B
�aB
�1B
�BB
��B
��B
��B
�"B
یB
�B
�[B
�B�Ap�HAp�HAp�0Ap�tAp��Ap�IAp}�ApcAp~(Ap~�Apr�ApY�ApW�Ap<�ApMjApU2ApH�Aph
Apy>Ap��Ap��Ap�{ApMjAp�4Ap!�Ap=qAo�FAosAn�An�An_An4Al��Als�Al��Alm]Ak��AjB�AjJAi�oAi�~Ai�Ah�Ag��AgK�Ag$�Af��Af/�Ae��Ae@OAe"hAd|Ac�0AcaAb��Ab]dAa��A`oA_;dA^�A]6zA\8�A\N<A\u�A\g8A\)�A[�]A[�A[��A[Z�AYخAX�*AW �AVS�AV:�AU�
AW7�AV��AVFtAU�AU��AUZ�AU#�AT��AT�AS�ASS�AR�AQ�]AQi�APݘAO��AN��AM�jALp;AK'RAJ��AJMjAJ�AI�AI�HAI�AI��AI�!AI2aAH��AH/�AG�AFe�AF�AE�?AE��AD$tAB��AB��AB��AA��AAn�A@}VA?�RA@K�A@ƨA@�A?p;A=��A=x�A="hA<��A<E�A;9XA9ɆA8U2A7�A6�=A6jA6�}A6�A5W�A6A�A6cA5U2A5RTA5�}A5��A6+kA6MA5��A5��A5��A5y>A5l"A5A5�A4҉A3�TA2�QA2[�A1�HA0qvA04nA00UA0OA/��A/@A.g8A-�A-�0A-��A-��A-eA,C�A+�A+PHA+�A*��A*��A*w2A*�A)��A)��A)XyA(�A(�A'HA'�A'�A'�A&�ZA&��A'	A'(A'�A'A&ߤA&H�A%�TA%�A%`BA$�aA$��A$��A$_�A#�|A"یA"|�A"b�A"A�A!�9A!v�A!\)A!K^A!
�A f�AɆAr�AiDA[WAQ�AI�A1�A�9A��Ak�A�A��AoiA�AMjA��AN<A��A��AR�AG�AH�AJ#A�oA iA?}A%A�#A��AMjA��AѷA�oASAHAe�AuA~(A�A_A4�A�A��A+A��A��Ad�A5�AA�'A&A�A��A��AOvA iA
TaA	�A	D�A�jA��A|�AJ�A��A��A��A��AA AԕADgA�RAf�A%A}�Ab�AC�A�A�WAI�A�tA!-A ��A 8�@�8�@�!@�xl@��@�e@�m�@���@��,@��@��@�YK@��@�0@�iD@�9@�Vm@�@�bN@���@�M@�{J@�O@�S&@艠@��@滙@�@��@��Z@���@�Dg@��@�9�@�x@�L0@��@��r@�<6@��@�)�@݋�@�B�@�ȴ@�	�@��2@�;�@ՠ'@��@�H@�G�@җ�@��@�G�@Ϡ�@�*�@ͣn@���@˼@ʦL@�@ə�@�!-@�z@��@���@A@��@�Dg@��@�?@��@��@���@�Xy@��.@��M@��}@�'R@��@��#@�rG@�hs@�?}@�&�@���@�Ɇ@���@��@���@��	@�tT@��M@�]�@��@���@��^@��@�M�@�Vm@���@�8@�m�@���@�-�@� �@��@�w2@�@��'@�@�@�&�@�1@�ԕ@�+@�N�@� �@��@���@���@�S&@�E9@�:�@���@��F@�E�@��@��&@���@�W?@� \@��@���@�M@�s�@���@�j@�;�@�{@���@��@�(@�7@��@��@�:�@���@�2a@���@��N@��^@��X@�o @�>�@�+�@�"�@�	@��c@��R@���@�_@�e,@���@�ff@�l"@�PH@�6@�8�@��Z@�(�@��@���@���@��@�8�@�d�@��'@���@��h@�c�@�x�@���@��F@�ԕ@���@�4�@��@�"�@��@�
=@�ȴ@�V�@�M@��@��@��@��g@�t�@�'�@���@�l�@�b@�h
@�ϫ@��)@�	@���@���@��D@�@�@�خ@��O@�tT@}�D@zR�@vOv@t�@s�@q�@pg8@n �@m@fs�@cS�@[�k@]�@]�H@[S�@W��@Sخ@N$�@Hh�@G.I@E�9@C�&@B	@@��@>�X@<��@<!@;S@:W�@8��@6��@4��@2��@10�@07@.c @,�p11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�
=@���A z�A z�A@  A`  A�Q�A�{A��A�A�  A�=qA��A�B 
=B��B�B�
B�HB'�
B/�
B7�RB?��BG�RBO�BX{B_��Bh
=Bp�Bx{B�B�
=B��B�{B�B���B��B���B�B�  B�\B�
=B�B�
=B�B�
=B���B���B��B��B�
=B�{B�  B���B�\B�\B�  B��B�B�  B�  B�\C C�C
=C  C��C	�RC�qC�qC�3C��C�3C�RC  C�qC�qC�C 
=C"�C$
=C&  C'�RC)��C+��C-��C0C2�C4  C6  C8C:
=C<�C=��C?��CB  CD�CF�CH
=CJ�CK��CNCP\CR�CT�CU��CW��CY��C\C^
=C`�Cb�Cc�RCe��Ch  Cj  ClCnCo��Cr�Ct�Cu�qCxCz  C|  C~�C��C�HC���C�HC��C��C�C�  C��C��C�HC��C��C�  C��C��C�HC�HC��C�HC�  C���C��)C���C��qC�HC�HC��C�fC���C���C��)C���C�  C���C���C�  C�HC�HC��C���C��)C��qC���C��C�HC���C�HC���C��)C�HC�C��C�HC�C�C�  C���C���C��C�HC��qC��)C��qC�HC���C��qC��C��C�  C���C�  C�  C��qC��qC���C��C��C��C�HC��C�C��C��C��C���C��)C��qC�HC�fC��C��C�  C���C���C�HC�  C�HC�  C��C�HC��)C��qC���C���C��C�C��C��C�HC�HC�  C��C���C��)C�HC���C��)C��qC��qC��)C���C���C���C��C��C�  C�HD �D \D �D��D�D� DHD��DHD~�D�\D\D��D� D�D��D  D~�D	HD	� D	��D
�HD �D~�D�\D� D �D��D  D~�D�\D� D  D~D��D~�D�\D\D  D\D��D~�D�\D~�D��D~D  D��D �D�HDHD~�D�qD\D�D��D �D\D �D�3DHD��DHD�HD �D �HD �\D!\D" �D"� D"�\D#� D$�D$��D%�D%~D%�qD&}qD&��D'� D(  D(}qD(��D)}qD)�D*� D+�D+��D,�D,��D-  D-\D-�\D.}qD.�D/� D0 �D0��D1HD1��D2  D2~D3 �D3��D4 �D4�HD5HD5��D6HD6\D7 �D7�HD8 �D8��D9  D9� D:�D:��D; �D;��D<�D<\D<��D=��D> �D>� D>�\D?\D?�\D@� D@��DA~�DB  DB� DB�DC|�DC�DD~DD�DE� DF  DF}qDF�DG~�DG�\DH\DI�DI� DJ  DJ��DKHDK�HDL �DL\DM  DM��DNHDN� DOHDO� DO�\DP��DQHDQ\DR  DR��DSHDS�HDS��DT|�DT�qDU~�DVHDV��DW �DW�HDX  DX\DX�\DY~�DY�\DZ�HDZ��D[~�D[�\D\~�D\��D]~�D^  D^� D_HD_�HD`HD`��D`�\Da\Da�\Db��Dc�Dc��Dc�\Dd\De  De�HDe�\Df}qDf�qDg�HDhHDh��DiHDi��Dj �Dj��DkHDk~Dk�\Dl��DmHDm��Dn�Dn\Dn�\Do�HDo�\Dp� Dp��Dq\Dr  Dr��DsHDs� Ds�\Dt\Du  Du��DvHDv\Dv��Dw\Dx �Dx� Dx��D{��D~1HD�=D��fD���D�qD�Y�D��fD��qD�fD�VD��qD���D��D�x D��RD��D�G\D��\D��=D�J=D�ǮD�@�D��=D�O
D��)D�L)D��RD�@�D�˅D�G\D��HD�<)D��D�K3D��3D�NfD��qD�?
DǊ�D��D�IHDڇ�D��3D��D�<�D�t{D��3B�B��B��B�B�4B�nB�QB�B��B��B�B�B�~B�B��B�@B��B�B��B��B�B�B��B��B�[B��B�aB��B�uB�BɑB��B�cB��B��B�WB�
B��B�lB��B�pB��B�EB�%B�B�@B�4B�6B�zB�@BB��B�+B�B�B�oB��B�JB��B��B�tBy<B��B��B�(B��B��B�KB��B��B�;B�[B� Bt�BwWBr,B��B��B�|B��B�@B�JB�8B��B��B�jB��B�Bx�Bt�Br#BhFB]�BR+BDjB9�B4�B3�B3�B3�B5B;,B?[BF�BD�BB�B?�B>�B6'B2�B0?B13B,�B �B>B)B&jB"0BBB�B}B'�B!WB�B�B B��B��B��B�B��B�kB�sB�2B�/B�fB��B��B��B�B��B�>B�B��B�B˖BȃB�lBĳB��B��B��B�GBŜB�B�B��B�B�-B��B�mB��B��B��B�7B~�B|�B{yB{]By�Bq�Bp�Bk7Bh�Bg�Bg�BhGBf�BboBbB_�BX�BSiBJ�BHBG�BH�BK�BL�BN�BOXBP�BQBP�BL�BH�BE�BD�B>$B;�B<B;EB99B-�B(^B'~B'�B"�B qB�BhB�B5BhB�B{B�B�BBeB
;B?B
�1B
�'B
��B
��B
�B
�B
�bB
��B
�-B
��B
ܵB
��B
��B
�-B
�BB
�IB
��B
�ZB
��B
��B
ȇB
ĸB
�B
�CB
��B
�B
��B
�TB
��B
��B
�:B
�HB
�}B
��B
�CB
�JB
�!B
��B
��B
�
B
��B
},B
{IB
y�B
x�B
wkB
vRB
rDB
m,B
j�B
h\B
fB
eLB
eB
dLB
a�B
]�B
Z�B
XgB
V�B
SB
O�B
O)B
N(B
I�B
H�B
G�B
F�B
F[B
CKB
?iB
<UB
8�B
6�B
4�B
1�B
.:B
(�B
!�B
B
�B
B
�B
oB
�B
\B
0B
PB
B

AB
�B
B
�B
9B
B
�B
�B	�PB	�B	�pB	�B	�bB	�!B	�B	�(B	��B	��B	�qB	�}B	�B	�B	�!B	�vB	��B	��B	�CB	��B	۳B	ڍB	��B	�PB	�B	��B	�B	��B	�@B	�B	�B	�B	�[B	��B	��B	��B	��B	�{B	��B	�B	�zB	�B	�MB	�hB	��B	� B	��B	�$B	�B	��B	��B	�#B	�CB	�B	�ZB	��B	�CB	��B	��B	��B	��B	�sB	��B	��B	�BB	�$B	��B	��B	��B	��B	�DB	z}B	wB	u�B	tnB	q�B	rB	mtB	m�B	mB	l|B	mB	n�B	qAB	riB	t�B	z B	|�B	}�B	�SB	��B	��B	�jB	�vB	��B	�kB	��B	��B	��B	��B	��B	��B	�kB	�mB	�2B	��B	�B	�aB	�B	}�B	~�B	zKB	y�B	y�B	y~B	{B	{�B	{�B	{�B	|;B	|IB	|(B	|XB	|B	}B	}LB	~gB	9B	�hB	�yB	��B	�iB	�^B	�lB	�<B	��B	��B	�?B	|�B	}wB	B	BB	�$B	��B	�UB	��B	�B	��B	�B	�zB	��B	�eB	��B	��B	��B	�kB	�YB	��B	��B	�B	��B	�&B	��B	��B	��B	�8B	�iB	�KB	��B	��B	��B	�4B	��B	�B	�#B	��B	��B	�B	��B	��B	��B	��B	B	ƘB	�IB	̓B	ΪB	�	B	ԡB	��B	�UB	��B	��B	�B	�XB	�.B	�B	�?B	�^B
	9B
EB
$]B
5B
=�B
E�B
KB
KB
MB
YCB
f�B
s�B
z�B
�5B
��B
�kB
�?B
�LB
�B
�B
��B
�,B
ەB
�B
�dB
�
B�Ap�HAp�HAp�0Ap�tAp��Ap�IAp}�ApcAp~(Ap~�Apr�ApY�ApW�Ap<�ApMjApU2ApH�Aph
Apy>Ap��Ap��Ap�{ApMjAp�4Ap!�Ap=qAo�FAosAn�An�An_An4Al��Als�Al��Alm]Ak��AjB�AjJAi�oAi�~Ai�Ah�Ag��AgK�Ag$�Af��Af/�Ae��Ae@OAe"hAd|Ac�0AcaAb��Ab]dAa��A`oA_;dA^�A]6zA\8�A\N<A\u�A\g8A\)�A[�]A[�A[��A[Z�AYخAX�*AW �AVS�AV:�AU�
AW7�AV��AVFtAU�AU��AUZ�AU#�AT��AT�AS�ASS�AR�AQ�]AQi�APݘAO��AN��AM�jALp;AK'RAJ��AJMjAJ�AI�AI�HAI�AI��AI�!AI2aAH��AH/�AG�AFe�AF�AE�?AE��AD$tAB��AB��AB��AA��AAn�A@}VA?�RA@K�A@ƨA@�A?p;A=��A=x�A="hA<��A<E�A;9XA9ɆA8U2A7�A6�=A6jA6�}A6�A5W�A6A�A6cA5U2A5RTA5�}A5��A6+kA6MA5��A5��A5��A5y>A5l"A5A5�A4҉A3�TA2�QA2[�A1�HA0qvA04nA00UA0OA/��A/@A.g8A-�A-�0A-��A-��A-eA,C�A+�A+PHA+�A*��A*��A*w2A*�A)��A)��A)XyA(�A(�A'HA'�A'�A'�A&�ZA&��A'	A'(A'�A'A&ߤA&H�A%�TA%�A%`BA$�aA$��A$��A$_�A#�|A"یA"|�A"b�A"A�A!�9A!v�A!\)A!K^A!
�A f�AɆAr�AiDA[WAQ�AI�A1�A�9A��Ak�A�A��AoiA�AMjA��AN<A��A��AR�AG�AH�AJ#A�oA iA?}A%A�#A��AMjA��AѷA�oASAHAe�AuA~(A�A_A4�A�A��A+A��A��Ad�A5�AA�'A&A�A��A��AOvA iA
TaA	�A	D�A�jA��A|�AJ�A��A��A��A��AA AԕADgA�RAf�A%A}�Ab�AC�A�A�WAI�A�tA!-A ��A 8�@�8�@�!@�xl@��@�e@�m�@���@��,@��@��@�YK@��@�0@�iD@�9@�Vm@�@�bN@���@�M@�{J@�O@�S&@艠@��@滙@�@��@��Z@���@�Dg@��@�9�@�x@�L0@��@��r@�<6@��@�)�@݋�@�B�@�ȴ@�	�@��2@�;�@ՠ'@��@�H@�G�@җ�@��@�G�@Ϡ�@�*�@ͣn@���@˼@ʦL@�@ə�@�!-@�z@��@���@A@��@�Dg@��@�?@��@��@���@�Xy@��.@��M@��}@�'R@��@��#@�rG@�hs@�?}@�&�@���@�Ɇ@���@��@���@��	@�tT@��M@�]�@��@���@��^@��@�M�@�Vm@���@�8@�m�@���@�-�@� �@��@�w2@�@��'@�@�@�&�@�1@�ԕ@�+@�N�@� �@��@���@���@�S&@�E9@�:�@���@��F@�E�@��@��&@���@�W?@� \@��@���@�M@�s�@���@�j@�;�@�{@���@��@�(@�7@��@��@�:�@���@�2a@���@��N@��^@��X@�o @�>�@�+�@�"�@�	@��c@��R@���@�_@�e,@���@�ff@�l"@�PH@�6@�8�@��Z@�(�@��@���@���@��@�8�@�d�@��'@���@��h@�c�@�x�@���@��F@�ԕ@���@�4�@��@�"�@��@�
=@�ȴ@�V�@�M@��@��@��@��g@�t�@�'�@���@�l�@�b@�h
@�ϫ@��)@�	@���@���@��D@�@�@�خ@��O@�tT@}�D@zR�@vOv@t�@s�@q�@pg8@n �@m@fs�@cS�@[�k@]�@]�H@[S�@W��@Sخ@N$�@Hh�@G.I@E�9@C�&@B	@@��@>�X@<��@<!@;S@:W�@8��@6��@4��@2��@10�@07@.c @,�p11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP, where dP is SURFACE PRESSURE from current cycle.                                                                                                                                                                                     N/A                                                                                                                                                                                                                                                             PSAL_ADJUSTED = celltm_sbe41(PSAL,TEMP,PRES,e_time,alpha,tau)                                                                                                                                                                                                   dP = 0                                                                                                                                                                                                                                                          N/A                                                                                                                                                                                                                                                             e_time assumes 0.09dBar/s ascent rate, alpha=0.141, tau=6.68s in continuous profile mode, or alpha=0.0267, tau=18.6s if in spot sampling mode                                                                                                                   null                                                                                                                                                                                                                                                            N/A                                                                                                                                                                                                                                                             Salinity corrected for Cell Thermal Mass (CTM), Johnson et al.(2007), JAOT & effects of pressure adjustments                                                                                                                                                    20230614081550              20230614081550  BO  BO  BO  ARGQARGQARGQRTSPPREXRTQC1.0 2.0 2.0                                                                                                                                                                                                 202306140815502023061408155020230614081553  CV  CV  QCP$                                                G�O�G�O�@�
=G�O�G�O�D��3H!� H!� H!�                                 14778366        