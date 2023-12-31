CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS   �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-01T23:28:27Z UW 3.1 conversion   
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
resolution        =���   axis      Z          9|   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  <�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       =H   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  @P   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       A   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       D   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  G$   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       G�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  J�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       K�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       N�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  Q�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       R�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  U�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       VT   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  Y\   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    Y�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    \�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    _�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  b�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    b�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    b�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    b�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    b�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  b�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    c   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    c   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    c   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         c,   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         c0   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        c4   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    c8Argo profile    3.1 1.2 19500101000000  5900960 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130105853  20190523124444  1514_5041_139                   2C  D   APEX                            2041                            062805                          846 @Աy$�
1   @Աy$�
@6��"���b����F1   GPS     Primary sampling: mixed [deeper than nominal 500dbar: discrete; nominal 500dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   B���B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��fD	�Dy�D",�D.` D:��DGffDS��D`s3Dl�311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�ffB���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC��fC���D	�Dl�D"  D.S3D:� DGY�DS� D`ffDl�f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��A��wA���A�{A�7LA��uA��A��/A��A�{A��#A�A��mA�(�A�-A�;dA���A��A�(�A�=qA��#A�S�A�{A��7A�(�A���A��A��^A�^5A��9A�l�A���A��hA�ĜA��#A���A���A�G�A��A���A�bA�E�Ax�A~��A|�A{�;Az1Ax�Av��Av1'Ap��An�!Al�Ai��AgK�AeoAc��Ab9XA`��A^�uA]��A]7LA[�7AY33AX�\AX^5AW��AU��AT��ATJASAS�PAR�DAQdZAQ;dAO��AL�HAK�AJbNAI�AF9XAD��AC�AB1'AA�7A@��A@E�A?;dA>�+A< �A:A�A8VA6�HA6-A5�A4�!A2M�A1S�A/�A.�9A-O�A,��A++A)�mA(�A&��A&bNA&1A%�^A%x�A%S�A%"�A$��A$��A#�;A!7LA VA �A��A�#A��A��A��A`BA=qA?}A�
A�Az�A�A�FA��AK�A�AXA��Ar�AƨA�7A�DA{A��At�A
�A	33AVAhsA�\A;dA�+AM�A��AI�AVA ^5@��;@�b@�@��9@�j@� �@���@��@���@�@�Ĝ@�\@�p�@�S�@�+@�O�@�F@��y@�hs@�Q�@�V@��u@ݺ^@�dZ@�{@�`B@׶F@�n�@�&�@�ff@��D@�I�@��+@��@�V@�v�@���@�&�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��wA���A�{A�7LA��uA��A��/A��A�{A��#A�A��mA�(�A�-A�;dA���A��A�(�A�=qA��#A�S�A�{A��7A�(�A���A��A��^A�^5A��9A�l�A���A��hA�ĜA��#A���A���A�G�A��A���A�bA�E�Ax�A~��A|�A{�;Az1Ax�Av��Av1'Ap��An�!Al�Ai��AgK�AeoAc��Ab9XA`��A^�uA]��A]7LA[�7AY33AX�\AX^5AW��AU��AT��ATJASAS�PAR�DAQdZAQ;dAO��AL�HAK�AJbNAI�AF9XAD��AC�AB1'AA�7A@��A@E�A?;dA>�+A< �A:A�A8VA6�HA6-A5�A4�!A2M�A1S�A/�A.�9A-O�A,��A++A)�mA(�A&��A&bNA&1A%�^A%x�A%S�A%"�A$��A$��A#�;A!7LA VA �A��A�#A��A��A��A`BA=qA?}A�
A�Az�A�A�FA��AK�A�AXA��Ar�AƨA�7A�DA{A��At�A
�A	33AVAhsA�\A;dA�+AM�A��AI�AVA ^5@��;@�b@�@��9@�j@� �@���@��@���@�@�Ĝ@�\@�p�@�S�@�+@�O�@�F@��y@�hs@�Q�@�V@��u@ݺ^@�dZ@�{@�`B@׶F@�n�@�&�@�ff@��D@�I�@��+@��@�V@�v�@���@�&�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B�B��B��B��B|�BjBcTB^5BgmBr�Bk�Bm�Bo�BYBM�B=qB2-B.B �B�BoB
=B%BB
�TB
�B
��B
�B
m�B
Q�B
`BB
x�B
�B
p�B
�\B
�PB
w�B
iyB
P�B
;dB
�B
hB
+B
B	��B	�B	�/B	��B	��B	�^B	�'B	��B	��B	�PB	�B	}�B	{�B	{�B	p�B	dZB	`BB	dZB	dZB	iyB	jB	k�B	l�B	o�B	m�B	m�B	l�B	k�B	gmB	cTB	aHB	YB	J�B	C�B	?}B	6FB	.B	-B	$�B	�B	"�B	 �B	�B	�B	�B	PB	1B	B��B��B��B�B�B�mB�TB�5B�#B�
B��B��BĜB�jB�^B�XB�LB�FB�?B�9B�-B�B��B��B��B��B��B��B��B��B��B�oB�VB�JB�B� B� B|�B� Bx�Bm�Bk�BgmBe`Be`B[#BZBT�BXBS�BS�BK�BI�BF�BE�BE�B>wB?}B>wB>wB8RB8RB6FB49B1'B2-B1'B1'B1'B0!B0!B/B/B/B.B-B)�B)�B&�B$�B"�B!�B�B�B�B�B�B�B�B�B�B!�B\)B�hB�yB	0!B	iyB	��B	�RB	��B	�s11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B+B�B��B��B��B�Bk�BdZB_;BiyBs�Bn�Br�Bv�B]/BQ�B@�B49B2-B$�B�B�BDB	7B+B
�yB
�)B
��B
�DB
t�B
R�B
_;B
x�B
�7B
p�B
�{B
�uB
z�B
n�B
T�B
B�B
#�B
uB
	7B
B	��B	��B	�NB	��B	B	ŢB	�?B	��B	��B	�uB	�+B	�B	� B	�B	u�B	ffB	aHB	iyB	jB	k�B	k�B	m�B	q�B	q�B	o�B	n�B	m�B	n�B	jB	dZB	e`B	aHB	O�B	E�B	C�B	>wB	1'B	0!B	'�B	!�B	#�B	!�B	 �B	�B	�B	hB	PB	B��B��B��B��B�B�B�fB�NB�/B�#B��B��BɺB�wB�dB�^B�RB�LB�FB�?B�9B�-B�'B��B��B��B��B��B��B��B��B��B�hB�hB�1B�B�B~�B�B{�Bq�Bm�Bk�BhsBgmB`BB]/BVBYBT�BT�BP�BL�BH�BG�BH�B@�B@�B@�BB�B;dB:^B8RB9XB49B33B2-B2-B2-B2-B2-B33B33B2-B0!B0!B,B+B)�B%�B$�B#�B"�B!�B �B�B �B �B�B�B�B"�B\)B�hB�yB	0!B	iyB	��B	�RB	�B	�s11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  <���<#�
<#�
<#�
<T��<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D��<#�
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
<u<e`B<D��<#�
<#�
<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<e`B<#�
<#�
<49X<49X<#�
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
<D��<#�
<#�
<#�
<D��<#�
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
<D��<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.2 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201112181658002011121816580020111218165800  AO  ARGQ                                                                        20111130105853  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130105853  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20111218165800  IP                  G�O�G�O�G�O�                