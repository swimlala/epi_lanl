CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS   �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-01T23:28:26Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7T   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    88   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8<   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8D   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8H   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8P   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8X   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8`   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8d   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8l   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9l   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9x   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        t  9|   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  <�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  =�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  AD   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     t  B$   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  E�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  I   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  I�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  M`   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  N@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  Q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  U(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  V   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  Y|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     t  Z\   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ]�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ^    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    a    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    d    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  g    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    g,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    g0   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    g4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    g8   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  g<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    g|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    g�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    g�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         g�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         g�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        g�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    g�Argo profile    3.1 1.2 19500101000000  5900960 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130105827  20190523124444  1514_5041_136                   2C  D   APEX                            2041                            062805                          846 @Ԭ6���1   @Ԭ6���@6(�9Xb�b�bM��1   GPS     Primary sampling: mixed [deeper than nominal 500dbar: discrete; nominal 500dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   B���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�� D	,�Ds3D!��D.�fD:�3DG9�DT�D`y�DlٚDyL�D�fD�@ D�y�D��fD��D�<�D���D���D��D�6fD�� DǶfD�  D�#3DږfD��fD��fD�9�D�` D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�33B㙚B癚B뙚BB�B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fD	  DffD!� D.��D:�fDG,�DT�D`l�Dl��Dy@ D�  D�9�D�s3D�� D�fD�6fD��3D��fD��3D�0 D���Dǰ D���D��Dڐ D�� D�� D�33D�Y�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�{A��
A�5?A���A���A���A�E�A���A���A���A�l�A�A�hsA�r�A��
A�Q�A�ȴA�dZA���A��A�|�A��;A�bA��jA�~�A�`BA�33A���A���A��7A��-A��A�E�A�A�?}A�K�A��FA�bA��A��A��A��!A� �A��A�%A�~�A�"�A�C�A�x�A��^A��`A�&�A���A}�FA|�Az�!Ay�TAy\)Ax^5Aw+At�jAp�Al�+Ai��Ag�;Ad��Ac�hAa�wA`VA_��A^^5A[|�AXv�AV�HAVQ�AU�-AT�AS�ARz�AP��ANjAH~�AEVAA�A@A?��A=��A;\)A9�wA8��A7�FA5hsA2ffA1��A1C�A0��A/��A.��A.n�A-ƨA-
=A+��A+�hA*�A)��A(jA'�A'+A&VA%�TA%7LA$E�A#|�A!|�A z�A 5?AƨAA��A�+A^5A-AJA��A�`A1AdZA�yA5?A/A�7A5?A��A�jAhsAA�A��A1A&�A��AA
~�A	��AȴA^5A�A�AE�A��A�A�AffA/A z�A bN@��@���@��T@��@��D@�;d@�v�@��9@�?}@�S�@�hs@�F@�n�@���@��@��@�@���@�V@���@�hs@��@�hs@�j@��@���@��@���@�V@۶F@�@�-@��T@٩�@��@��w@���@�33@��D@�V@��@�ƨ@��@��H@�~�@zM�@p1'@g�;@_�@W�@P�u@G\)@@Ĝ@<��@6�y@2=q@,Z@'�@$�j@ ��@hs@/@bN@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�{A��
A�5?A���A���A���A�E�A���A���A���A�l�A�A�hsA�r�A��
A�Q�A�ȴA�dZA���A��A�|�A��;A�bA��jA�~�A�`BA�33A���A���A��7A��-A��A�E�A�A�?}A�K�A��FA�bA��A��A��A��!A� �A��A�%A�~�A�"�A�C�A�x�A��^A��`A�&�A���A}�FA|�Az�!Ay�TAy\)Ax^5Aw+At�jAp�Al�+Ai��Ag�;Ad��Ac�hAa�wA`VA_��A^^5A[|�AXv�AV�HAVQ�AU�-AT�AS�ARz�AP��ANjAH~�AEVAA�A@A?��A=��A;\)A9�wA8��A7�FA5hsA2ffA1��A1C�A0��A/��A.��A.n�A-ƨA-
=A+��A+�hA*�A)��A(jA'�A'+A&VA%�TA%7LA$E�A#|�A!|�A z�A 5?AƨAA��A�+A^5A-AJA��A�`A1AdZA�yA5?A/A�7A5?A��A�jAhsAA�A��A1A&�A��AA
~�A	��AȴA^5A�A�AE�A��A�A�AffA/A z�A bN@��@���@��T@��@��D@�;d@�v�@��9@�?}@�S�@�hs@�F@�n�@���@��@��@�@���@�V@���@�hs@��@�hs@�j@��@���@��@���@�V@۶F@�@�-@��T@٩�@��@��w@���@�33@��D@�V@��@�ƨ@��@��H@�~�@zM�@p1'@g�;@_�@W�@P�u@G\)@@Ĝ@<��@6�y@2=q@,Z@'�@$�j@ ��@hs@/@bN@��@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�DB�DB�%By�Bs�Bo�Bk�BdZBYBP�BI�B@�B49B'�B"�B�B�BVB
=BB��B�B�B�B�mB��BǮB�wB�'B��B�oB�+Bv�Bn�BdZBW
BI�B2-B�B�B{B\BB
�B
�`B
�)B
ǮB
�B
��B
��B
�DB
�B
cTB
M�B
A�B
33B
)�B
$�B
�B
\B	��B	�#B	�jB	��B	�hB	{�B	iyB	`BB	S�B	K�B	@�B	2-B	�B	bB	bB	bB	JB	%B	B��B�HB�FB��B�7B�B� Bt�Bm�BffBbNBaHB^5B_;BffBn�Bx�B�B�B�B�DB�+B�+B�B�B�B� B~�B{�B|�B|�Bx�Bx�Bv�B`BB\)B[#B^5B_;B`BBcTBhsBk�Bl�Bm�Bm�BhsBffBffBdZB`BBYBW
BT�BQ�BL�BJ�BH�BH�BF�BF�BD�BC�BC�BB�BB�BB�BB�BA�BE�BA�B@�B@�B?}B@�B?}B>wB>wB@�B@�B@�B>wB>wB@�BC�B<jB>wB<jB9XB7LB6FB6FB7LB6FB33B5?B8RB9XB8RB9XB9XB9XB<jB<jB;dB<jB=qB=qB=qB=qB>wB<jB;dB�hB�yB	B	K�B	v�B	��B	�qB	�HB	��B
%B
oB
�B
$�B
,B
49B
>wB
C�B
G�B
M�B
R�B
XB
\)B
`BB
e`B
k�B
o�B
s�B
v�B
{�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�PB�PB�=B|�Bt�Bq�Bm�BgmB\)BR�BL�BE�B8RB+B$�B�B�BbBJB%B��B�B�B�B�B�BɺB��B�?B��B��B�7By�Bp�BhsBYBP�B8RB!�B�B�BoB
=B
��B
�sB
�TB
��B
�3B
��B
��B
�\B
�7B
k�B
O�B
E�B
49B
+B
&�B
�B
{B
  B	�TB	B	��B	��B	~�B	m�B	cTB	VB	N�B	F�B	9XB	�B	hB	oB	oB	\B	1B	B	  B�B�}B��B�JB�B�%Bz�Bq�BiyBe`BgmBffBaHBgmBo�Bz�B�%B�B�B�PB�7B�1B�B�B�+B�B�B}�B}�B~�B{�Bz�B{�BcTB]/B\)B`BB`BBaHBdZBiyBl�Bn�Bo�Bp�BjBgmBhsBgmBe`B]/BXBXBVBP�BO�BM�BJ�BI�BG�BG�BG�BE�BE�BD�BE�BC�BD�BG�BD�BC�BC�BB�BB�B@�B@�BA�BD�BA�BA�B@�B?}BB�BG�B?}B@�B>wB;dB8RB7LB7LB8RB6FB6FB9XB9XB9XB9XB:^B:^B;dB=qB=qB<jB>wB>wB>wB>wB=qB>wB<jB;dB�hB�B	B	K�B	v�B	��B	�wB	�HB	��B
%B
oB
�B
$�B
,B
49B
>wB
C�B
G�B
M�B
R�B
XB
\)B
aHB
e`B
k�B
o�B
s�B
v�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<49X<#�
<#�
<#�
<#�
<49X<#�
<#�
<49X<T��<#�
<#�
<#�
<#�
<D��<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<D��<49X<#�
<49X<#�
<#�
<#�
<#�
<#�
<49X<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<�C�<D��<T��<#�
<#�
<#�
<49X<#�
<#�
<#�
<49X<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201112181657592011121816575920111218165759  AO  ARGQ                                                                        20111130105827  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130105827  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20111218165759  IP                  G�O�G�O�G�O�                