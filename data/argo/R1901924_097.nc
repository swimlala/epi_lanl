CDF       
      	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       	DATE_TIME         N_PROF        N_PARAM       N_LEVELS  !   N_CALIB       	N_HISTORY            	   title         Argo float vertical profile    institution       BODC   source        
Argo float     history       29-May-2023 02:55:12Zcreation      
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
_FillValue                  0  �tArgo profile    3.1 1.2 19500101000000  20230529025520  20230529025520  1901924 Argo UK                                                         Jon Turton                                                      PSAL            TEMP            PRES               aA   BO  165371                          2B  A   APEX                            8981                            2.13.1.1                        846 @�-Տ\) 1   @�-Տ\) �C����m]@DML�_�1   GPS     Primary sampling: mixed                                                                                                                                                                                                                                            9A   A   A   @��R@�  A (�A z�A@(�A`Q�A�(�A�A�  A�(�A��
A��A�(�A��
B 
=B(�B
=B�B�B'�
B/B8
=B@�BG�
BP
=BW�B_�HBh
=Bp
=Bw��B�B���B���B�  B�  B�B���B��B�B�
=B�\B���B���B��B���B�  B�
=B���B��B�
=B�\B�
=B��B�  B��B�
=B��B���B�  B�B���B�C C  C�qC�RC  C
CC�CC  C  C�C��C  C�C�C�qC"�C#�qC%�qC'��C)��C+��C-�RC/�qC2  C3�qC5��C7�3C9��C;�qC=�RC?��CA��CC�qCE��CG�RCI��CL�CN  CO�RCQ��CS�qCU��CX�CZ
=C\  C]�RC_�qCb�Cc�qCe��Cg��Cj  ClCn  Cp  Cq��Cs�RCu��Cx�CzC|  C~�C�HC�HC���C�  C�  C�  C�HC�HC�HC�HC�  C���C��C��C�HC��C�HC�HC�  C���C��qC���C�  C�  C�  C�  C���C�C�  C��)C�HC��C��)C���C�HC��C���C���C�  C�  C�HC���C���C���C���C��qC��C��C���C��)C�HC�HC��C��C�  C��)C���C���C��C�HC��)C��)C��)C��)C�HC�  C��)C��qC��qC��)C��C��C���C��)C���C���C���C���C�  C�HC�HC���C��qC��C��C��C��C�  C��qC�HC��C��C�C�  C���C��qC��C�C�  C��)C���C���C�  C�C��C��C�  C��qC�  C�  C��qC�  C���C�  C��C���C���C�  C��)C��qC��qC���C��C��C�  C��)C���C���D �D ��D�D�3DHD��DHD��D�D��D  D��D �D��D�D~D�D\D	 �D	� D
  D
� D  D�HD�D}qD �D� D�D��DHD\D��D~�D  D� D �D� D�\D��D�D~�D��D� D �D��D �D� DHD� D  D��D�D��D �D� D�\D��D�D��D  D~�D�\D~D�qD ~�D!  D!��D!�\D"� D#�D#��D$�D$�HD%  D%��D& �D&� D' �D'|�D(  D(�HD)  D)�HD* �D*�HD*�D+~�D,  D,~�D,�\D-�HD.�D.~D.�)D/~�D/�\D0��D1  D1��D2�D2��D33D3�HD4 �D4��D5 �D5�HD6 �D6��D7  D7~�D7�qD8� D9HD9� D:  D:\D:��D;~D;�\D<�HD=�D=�3D> �D>\D>�D?� D@ �D@� D@�\DA~�DA��DB� DC  DC~�DD  DD� DE  DE\DE�DF~�DF��DG~DG�\DH��DH��DI}qDI�\DJ� DK  DK~�DL  DL��DL�\DM~�DN  DN\DN�\DO��DO��DP|�DP�\DQ\DQ�qDR\DSHDS\DS�DT� DUHDU~�DV �DV�HDV�DW~DW�\DX~DY  DY��DY�\DZ~�D[  D[��D\HD\\D\��D]� D^  D^~D^��D_��D`�D`��Da�Da�3Db  Db~Db�\Dc��Dc��Dd~De  De�HDe�\Df\Df�\Dg\Dh  Dh� DiHDi~�Di�Dj\Dk  Dk��Dl �Dl��Dm �Dm� Dn  Dn��DoHDo� Dp �Dp�3Dq�Dq\Dq�\Dr� Dr��Ds� Dt�Dt��DuHDu��Dv �Dv��Dv��Dw\Dw��Dx\Dx��D|�D~��D��)D�
D��D��D�r�D���D��{D���D�,�D���D���D�:=D�vfD���D�{D�K�D���D��)D�=D��=D�A�D��qD�D{D�ȤD�C�D�ʏD�I�D��fD�B�D���D�A�D���D�O
D���D�=qD��\D�A�Dǿ\D��qD�@�Dځ�D�D�D�8�D�l)D���BV�BV�BW?BXBX�BZQB^OBcBd@B\�BT�BK�BE�BC�BCaBB�B<jB/OBB�B\B6B�B�B�8B�RB�B��B�B�fB�B�TB�B�nB�B�B�NB�B��B�B�bB�bB�bB�-B�HB�B��B��B�5BٴB�9B�FB͹B�xB�#BƨB� B�-B�EB�B�B�PB��B�	B��BοB��B�hB��B�B�8B�B�B��B�B��B�B�B�B�B�|BޞB�EB�aBѷB�6B�?B��B��B��B�.B��B�hB�hB�1B��B��B�NB��B�jB��B��B�!B�jB��B��B��B��B�B��B��B��B�WB��B�WB�IB�JB�}B��B�MB��B��B�B�B��B��B��B�B�]B��B��B�_B�2B�@B��B��B�B{Bx�Bp�BiB`�BZ�BX�BXyBW?BT{BO\BG�B>B8B2GB*B(�B.�B+�B(>B"�BB�B	7B�B��B�LB�=B��B�:BߊB�?BөBңB��B��B�B�)B��B�(B��B��B��B�qB�$B��B�_B�{B�FB��B��B��B��B��B�#B�4B|PBxlBtBl�Bh
BdZB`�BX+BI7BC�BAUB>�B9�B4B1[B/�B-�B)�B'�B&B$B!HBBqBKB?B�B�BsB~B$B%�B%FB"hB�B�BPB1B_B�B�BB�B�B 4B
��B
�B
�B
��B
�KB
�_B
��B
�B
�RB
�B
�B
�'B
�VB
ݘB
��B
ٴB
�EB
ևB
�&B
� B
�VB
�xB
ȚB
�tB
ĶB
��B
��B
�8B
�hB
��B
�*B
�B
��B
�-B
�4B
��B
��B
��B
�1B
�dB
��B
�9B
��B
�B
�)B
�B
��B
|�B
s3B
n�B
j�B
i*B
gB
d�B
b�B
a|B
`B
^OB
[qB
V�B
RTB
O�B
NVB
L~B
HKB
EB
A�B
A�B
BuB
B�B
BAB
BAB
A�B
A;B
@iB
="B
:�B
8�B
7fB
4�B
3�B
2-B
0B
,�B
(�B
$@B
"NB
 �B
�B
�B
�B
eB
EB
?B
�B
B
�B
B
DB
B

�B

#B
	B
�B
�B
�B
zB
�B
B
tB
B
�B
aB	��B	�0B	��B	�B	�B	�B	��B	�eB	�B	�B	�B	��B	�6B	�B	�B	�B	�B	��B	�B	�B	�B	��B	��B	޸B	��B	�uB	ΥB	�7B	�mB	�B	�wB	��B	��B	�ZB	�OB	��B	��B	��B	�B	��B	��B	��B	�AB	B	xlB	p�B	oiB	oB	o B	oiB	o�B	pB	oOB	o�B	q'B	s�B	s�B	tB	t9B	v�B	xlB	y$B	z*B	{dB	HB	��B	�+B	�B	��B	��B	�YB	�MB	�B	��B	�'B	��B	|�B	{JB	y>B	x8B	w�B	v�B	vzB	uZB	s�B	qvB	n�B	jB	gB	d@B	c�B	a�B	^�B	Y1B	U�B	S�B	T{B	T�B	R�B	SB	T,B	T�B	V9B	YB	Z�B	[#B	[�B	\]B	\�B	]/B	]~B	^�B	`'B	`vB	aB	gB	v�B	cB	��B	�9B	��B	��B	�jB	�B	�hB	� B	��B	�B	��B	��B	�SB	��B	�!B	�B	�B	�2B	��B	��B	��B	�B	��B	��B	��B	�iB	�iB	��B	�;B	��B	��B	�/B	��B	�iB	��B	��B	��B	�0B	�B	�.B	�'B	�{B	��B	�B	��B	��B	�xB	��B	��B	��B	��B
B
)B
�B
B
�B
-�B
8�B
?.B
@�B
Q�B
I7B
^B
yXB
}VB
��B
��B
�<B
�B
�XB
ðB
�B
�B
��B
�qB�A|��A|��A|��A|��A|rGA|\�A|7�A|8�A|	A{J�AzZ�Ay�Ay9XAyYAy�Ax��Ax�Aw��Avh�AvxAu�aAu͟Au�<At�1AsAr�"Ar�Ar�ArݘAr�,Ar��Ar��Ar��Ar��Ar� Ar��Ar�oAr~(ArzAriDArc�Arc�Arb�Ar`BAr[WArZ�ArV�ArFtAr$tAq͟Aq�AqQ�Ap�Ap��ApZ�Ao��Aod�Am	AiѷAg�Ag�PAf��Af+kAe�+Aee�Ad�wAdJ�Ac��Ac��Ab�AbOAa�Aa��Aa��A`��A`?A_�^A_��A_h
A_7A^&A]��A\��A\:*A[�,A[V�AZm�AZ33AZ�AY�yAY��AY�AX�AV>BAU��AU�AT��AT�ATںAT�6ATA�AT~AS�AS��AS|�AS`�ASW?AS?}AS1'ARخAR�AAQ�cAQ|�AQ͟AQ��AR
�AR��AR��AQ��AQ>BAP��AP��AO�AO~�AOH�AOANQAN>BAM��AM��AMe�AM+�AL��AL��AL@�AK!-AJ#�AI�#AI��AH�AG�AF��AFS�AF�AE��AE҉AE�AE=AD8�ACn�AB�8AB-AAd�AA�A@�KA@cA@=qA?��A>�?A>�A=FtA<ϫA<hsA;�A:�6A:M�A9�PA9��A8��A8��A8rGA8	A7�MA7�A7�RA7�A6E9A5~(A5VA4�KA4��A4&�A2�+A2zA2qA2�A2�A1�A1�A1x�A1]dA0��A/��A/��A/-A.�A-�A-c�A,�PA,�OA+�A*jA*7A)�A)��A)-A(�A(2aA(+A'�
A'B[A'eA&�A&�LA&S�A%��A%�_A%Z�A%xA$��A$r�A$�9A$�A%�A$�[A$t�A#ĜA#6zA"�	A!=�A �gA �A ��A �SA �cA �A �~AFtA��AB[A�3A�uA�A�AƨA��A�XA9�A�\A]dA�A�[AE�A�A�EA�+A�jAe,A�A��A1�A�A��A�~AYA:�A�?A?}A�DA6�A�`Aw�A1�A��AbNA�cA~(A�4A�NA}VA��A&�A�AS&A�LA� A
��A
�A	��A	a�A	�A��An�A+�A��A�9A_A/�A�A�A�vAf�A��A#:A�A�xA�"A~�AkQAFA&A$A�jA%�A ��A zxA �@���@�N<@���@�Q�@�$t@��S@�:*@���@���@� �@�C�@��s@�{�@�6@��@�ی@���@�ѷ@��@���@�@��@�$@�@��@��@�O@��@�B[@��)@퐗@�9�@�l"@�P@�rG@�z@�j�@���@�+@�I@�dZ@���@⻙@�1�@�\)@��\@�$�@߼�@�@�l�@�'R@�0�@�;@٬q@ح�@��@ץ@֬�@�	l@��@Ѳ-@�[�@χ�@ά�@���@��`@�($@ƝI@��/@���@���@�|�@���@��@�!-@���@��*@�(@�O@��C@�F�@� i@��@��@��|@��/@��@�kQ@�K^@�\�@�>B@��'@� \@�8@�\�@�n/@���@���@�($@�m�@��}@�g8@�D�@���@�4@�GE@���@�H�@�o@�H�@�4�@��@��Q@��	@�A�@�C@���@��)@�@�.I@���@���@���@�u@��@���@�ں@�	�@��@�|�@��@���@�@�Ta@�4@���@�h�@��@���@�\�@���@��9@�xl@�V@�3�@��6@���@��@@���@���@�|�@�:�@�+k@�M�@���@���@��2@��@���@�Z@�-@��A@���@�%F@���@���@�6z@��4@�!-@��@� i@���@��2@��g@��}@���@���@�҉@�l"@�*�@��@��@�%F@��@��3@�&@���@�GE@���@�	@��@�q�@���@��7@�Xy@�Ĝ@�+�@X�@{�	@x�5@v�@pb@nȴ@m�@j�@d��@b�L@`u�@_��@^��@[��@Y�~@TĜ@D��@C1�@F��@C�;@B�6@B�@?�w@;y�@9rG@5��@2�M@1�C@0  @/{J@.�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��R@�  A (�A z�A@(�A`Q�A�(�A�A�  A�(�A��
A��A�(�A��
B 
=B(�B
=B�B�B'�
B/B8
=B@�BG�
BP
=BW�B_�HBh
=Bp
=Bw��B�B���B���B�  B�  B�B���B��B�B�
=B�\B���B���B��B���B�  B�
=B���B��B�
=B�\B�
=B��B�  B��B�
=B��B���B�  B�B���B�C C  C�qC�RC  C
CC�CC  C  C�C��C  C�C�C�qC"�C#�qC%�qC'��C)��C+��C-�RC/�qC2  C3�qC5��C7�3C9��C;�qC=�RC?��CA��CC�qCE��CG�RCI��CL�CN  CO�RCQ��CS�qCU��CX�CZ
=C\  C]�RC_�qCb�Cc�qCe��Cg��Cj  ClCn  Cp  Cq��Cs�RCu��Cx�CzC|  C~�C�HC�HC���C�  C�  C�  C�HC�HC�HC�HC�  C���C��C��C�HC��C�HC�HC�  C���C��qC���C�  C�  C�  C�  C���C�C�  C��)C�HC��C��)C���C�HC��C���C���C�  C�  C�HC���C���C���C���C��qC��C��C���C��)C�HC�HC��C��C�  C��)C���C���C��C�HC��)C��)C��)C��)C�HC�  C��)C��qC��qC��)C��C��C���C��)C���C���C���C���C�  C�HC�HC���C��qC��C��C��C��C�  C��qC�HC��C��C�C�  C���C��qC��C�C�  C��)C���C���C�  C�C��C��C�  C��qC�  C�  C��qC�  C���C�  C��C���C���C�  C��)C��qC��qC���C��C��C�  C��)C���C���D �D ��D�D�3DHD��DHD��D�D��D  D��D �D��D�D~D�D\D	 �D	� D
  D
� D  D�HD�D}qD �D� D�D��DHD\D��D~�D  D� D �D� D�\D��D�D~�D��D� D �D��D �D� DHD� D  D��D�D��D �D� D�\D��D�D��D  D~�D�\D~D�qD ~�D!  D!��D!�\D"� D#�D#��D$�D$�HD%  D%��D& �D&� D' �D'|�D(  D(�HD)  D)�HD* �D*�HD*�D+~�D,  D,~�D,�\D-�HD.�D.~D.�)D/~�D/�\D0��D1  D1��D2�D2��D33D3�HD4 �D4��D5 �D5�HD6 �D6��D7  D7~�D7�qD8� D9HD9� D:  D:\D:��D;~D;�\D<�HD=�D=�3D> �D>\D>�D?� D@ �D@� D@�\DA~�DA��DB� DC  DC~�DD  DD� DE  DE\DE�DF~�DF��DG~DG�\DH��DH��DI}qDI�\DJ� DK  DK~�DL  DL��DL�\DM~�DN  DN\DN�\DO��DO��DP|�DP�\DQ\DQ�qDR\DSHDS\DS�DT� DUHDU~�DV �DV�HDV�DW~DW�\DX~DY  DY��DY�\DZ~�D[  D[��D\HD\\D\��D]� D^  D^~D^��D_��D`�D`��Da�Da�3Db  Db~Db�\Dc��Dc��Dd~De  De�HDe�\Df\Df�\Dg\Dh  Dh� DiHDi~�Di�Dj\Dk  Dk��Dl �Dl��Dm �Dm� Dn  Dn��DoHDo� Dp �Dp�3Dq�Dq\Dq�\Dr� Dr��Ds� Dt�Dt��DuHDu��Dv �Dv��Dv��Dw\Dw��Dx\Dx��D|�D~��D��)D�
D��D��D�r�D���D��{D���D�,�D���D���D�:=D�vfD���D�{D�K�D���D��)D�=D��=D�A�D��qD�D{D�ȤD�C�D�ʏD�I�D��fD�B�D���D�A�D���D�O
D���D�=qD��\D�A�Dǿ\D��qD�@�Dځ�D�D�D�8�D�l)D���BV�BV�BWeBXABYBZ�B^VBcUBfB_,BVIBL�BFWBDBC�BC�B>�B2%B9B?BWBzByBB�mB�~B�6B��B��B�B� B�VB��B�B��B�B�XB�&B�(B��B�eB�fB�iB�;B�KB�B�B�JB��B�:B��BսB�GB��B�XB�BƵB��B�GB��B��B��B�CB�mB�gB��B٘B�aB�B��B��B�$B�B��B��B�3B�PB��B�PB��B�&B��B� B�bB��B�aB��B�8B�AB�FB��B�_B��B�8B�?B�OB�B�fB�_B�^B�@B�B��B��B��B��B��B��B��B��B��B��B��B�*B��B�B�9B�RB�]B��B�VB�B�
B��B�XB��B�5B�/B�B�#B�_B�iB��B�$B��B�UB��B|$B{Br�Bk{Bb�B[uBY	BX�BW�BU�BQ�BI�B?RB:B49B+[B)PB/�B,gB)�B%BwBnB
jB�B PB��B�~B��B�6B�iB�B��B�xB�`B�B�lB��B��B�!B�B�gB��B��B�B�<B�IB��B�cB�2B��B�tB�B�KB�JB�0B}2By�Bu�Bm�Bi!Be.Bc+B[uBJ#BC�BBB?�B;B5HB1�B0aB/B*tB'�B&�B$�B"2B�BBB]B�BBB�B$�B&�B'B#�B�B�BiByB�B�B�BB�B�B�B
��B
��B
�.B
�B
��B
�B
��B
��B
�xB
�7B
�#B
��B
�MB
ޯB
�oB
�VB
� B
�4B
�gB
�-B
�QB
�yB
�B
��B
�oB
�0B
��B
�{B
��B
��B
�#B
��B
�	B
��B
�wB
�jB
�'B
�B
��B
�eB
��B
��B
��B
�cB
�B
��B
��B
�BB
t�B
o�B
kaB
jB
h*B
e.B
c�B
a�B
`�B
`B
]�B
X�B
S�B
P;B
O�B
NjB
I�B
FmB
A�B
BB
B�B
B�B
B�B
B�B
A�B
BMB
A�B
>B
;�B
9�B
8WB
5B
4`B
2�B
1�B
.�B
*�B
%4B
#%B
!�B
�B
aB
B
�B
B
_B
B
JB
	B
]B
nB
B

�B
SB

�B
�B
B
�B
�B
B
�B
�B
B
B
B	��B	��B	��B	��B	�rB	�7B	�nB	��B	��B	�!B	�B	�B	��B	�]B	�B	�B	�B	�+B	�lB	��B	�B	�FB	�7B	��B	݆B	�TB	�B	�mB	ơB	�B	��B	�XB	��B	��B	��B	��B	��B	�RB	�zB	��B	��B	��B	�AB	��B	{�B	qbB	o�B	oZB	n�B	obB	o�B	pSB	o�B	p
B	q"B	s�B	t�B	t�B	t-B	v�B	x]B	yB	z B	z�B	~�B	�}B	��B	�XB	�#B	��B	��B	�B	��B	��B	�?B	�B	}�B	|CB	y�B	x�B	w�B	wB	v�B	v�B	t�B	s�B	q�B	k�B	g�B	dVB	d]B	b�B	b�B	Z�B	W�B	T�B	UB	VB	UYB	T�B	T�B	UQB	V�B	Y�B	[1B	[�B	\DB	\�B	]B	]mB	^B	_B	`.B	`�B	`�B	eB	u�B	eB	��B	��B	��B	��B	��B	�`B	��B	�iB	��B	��B	�;B	��B	�bB	�WB	��B	��B	��B	�vB	�zB	��B	��B	�	B	��B	� B	��B	� B	��B	�B	�nB	��B	�B	��B	��B	��B	�$B	��B	��B	�6B	�CB	�rB	�^B	ïB	�B	�BB	�B	�B	ܳB	��B	�B	��B	��B
'B
fB
�B
'B
�B
-�B
8�B
?FB
@�B
R�B
IKB
]�B
yqB
}eB
��B
��B
�OB
�B
�jB
ýB
�B
��B
��B
�vB�A|��A|��A|��A|��A|rGA|\�A|7�A|8�A|	A{J�AzZ�Ay�Ay9XAyYAy�Ax��Ax�Aw��Avh�AvxAu�aAu͟Au�<At�1AsAr�"Ar�Ar�ArݘAr�,Ar��Ar��Ar��Ar��Ar� Ar��Ar�oAr~(ArzAriDArc�Arc�Arb�Ar`BAr[WArZ�ArV�ArFtAr$tAq͟Aq�AqQ�Ap�Ap��ApZ�Ao��Aod�Am	AiѷAg�Ag�PAf��Af+kAe�+Aee�Ad�wAdJ�Ac��Ac��Ab�AbOAa�Aa��Aa��A`��A`?A_�^A_��A_h
A_7A^&A]��A\��A\:*A[�,A[V�AZm�AZ33AZ�AY�yAY��AY�AX�AV>BAU��AU�AT��AT�ATںAT�6ATA�AT~AS�AS��AS|�AS`�ASW?AS?}AS1'ARخAR�AAQ�cAQ|�AQ͟AQ��AR
�AR��AR��AQ��AQ>BAP��AP��AO�AO~�AOH�AOANQAN>BAM��AM��AMe�AM+�AL��AL��AL@�AK!-AJ#�AI�#AI��AH�AG�AF��AFS�AF�AE��AE҉AE�AE=AD8�ACn�AB�8AB-AAd�AA�A@�KA@cA@=qA?��A>�?A>�A=FtA<ϫA<hsA;�A:�6A:M�A9�PA9��A8��A8��A8rGA8	A7�MA7�A7�RA7�A6E9A5~(A5VA4�KA4��A4&�A2�+A2zA2qA2�A2�A1�A1�A1x�A1]dA0��A/��A/��A/-A.�A-�A-c�A,�PA,�OA+�A*jA*7A)�A)��A)-A(�A(2aA(+A'�
A'B[A'eA&�A&�LA&S�A%��A%�_A%Z�A%xA$��A$r�A$�9A$�A%�A$�[A$t�A#ĜA#6zA"�	A!=�A �gA �A ��A �SA �cA �A �~AFtA��AB[A�3A�uA�A�AƨA��A�XA9�A�\A]dA�A�[AE�A�A�EA�+A�jAe,A�A��A1�A�A��A�~AYA:�A�?A?}A�DA6�A�`Aw�A1�A��AbNA�cA~(A�4A�NA}VA��A&�A�AS&A�LA� A
��A
�A	��A	a�A	�A��An�A+�A��A�9A_A/�A�A�A�vAf�A��A#:A�A�xA�"A~�AkQAFA&A$A�jA%�A ��A zxA �@���@�N<@���@�Q�@�$t@��S@�:*@���@���@� �@�C�@��s@�{�@�6@��@�ی@���@�ѷ@��@���@�@��@�$@�@��@��@�O@��@�B[@��)@퐗@�9�@�l"@�P@�rG@�z@�j�@���@�+@�I@�dZ@���@⻙@�1�@�\)@��\@�$�@߼�@�@�l�@�'R@�0�@�;@٬q@ح�@��@ץ@֬�@�	l@��@Ѳ-@�[�@χ�@ά�@���@��`@�($@ƝI@��/@���@���@�|�@���@��@�!-@���@��*@�(@�O@��C@�F�@� i@��@��@��|@��/@��@�kQ@�K^@�\�@�>B@��'@� \@�8@�\�@�n/@���@���@�($@�m�@��}@�g8@�D�@���@�4@�GE@���@�H�@�o@�H�@�4�@��@��Q@��	@�A�@�C@���@��)@�@�.I@���@���@���@�u@��@���@�ں@�	�@��@�|�@��@���@�@�Ta@�4@���@�h�@��@���@�\�@���@��9@�xl@�V@�3�@��6@���@��@@���@���@�|�@�:�@�+k@�M�@���@���@��2@��@���@�Z@�-@��A@���@�%F@���@���@�6z@��4@�!-@��@� i@���@��2@��g@��}@���@���@�҉@�l"@�*�@��@��@�%F@��@��3@�&@���@�GE@���@�	@��@�q�@���@��7@�Xy@�Ĝ@�+�@X�@{�	@x�5@v�@pb@nȴ@m�@j�@d��@b�L@`u�@_��@^��@[��@Y�~@TĜ@D��@C1�@F��@C�;@B�6@B�@?�w@;y�@9rG@5��@2�M@1�C@0  @/{J@.�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP, where dP is SURFACE PRESSURE from current cycle.                                                                                                                                                                                     N/A                                                                                                                                                                                                                                                             PSAL_ADJUSTED = celltm_sbe41(PSAL,TEMP,PRES,e_time,alpha,tau)                                                                                                                                                                                                   dP = 0                                                                                                                                                                                                                                                          N/A                                                                                                                                                                                                                                                             e_time assumes 0.09dBar/s ascent rate, alpha=0.141, tau=6.68s in continuous profile mode, or alpha=0.0267, tau=18.6s if in spot sampling mode                                                                                                                   null                                                                                                                                                                                                                                                            N/A                                                                                                                                                                                                                                                             Salinity corrected for Cell Thermal Mass (CTM), Johnson et al.(2007), JAOT & effects of pressure adjustments                                                                                                                                                    20230529023901              20230529023901  BO  BO  BO  ARGQARGQARGQRTSPPREXRTQC1.0 2.0 2.0                                                                                                                                                                                                 202305290239012023052902390120230529023904  CV  CV  QCP$                                                G�O�G�O�@��RG�O�G�O�D���H!~�H!~�H!~�                                14778366        