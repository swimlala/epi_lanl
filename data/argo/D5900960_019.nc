CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS   �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-01T23:28:06Z UW 3.1 conversion   
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
resolution        =���   axis      Z          9|   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  <�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       =P   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  @`   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       A$   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       D4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  GD   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       H   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  K   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       K�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       N�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  Q�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       R�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  U�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o       V�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  Y�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    Y�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    \�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    _�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  b�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    c    HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    c   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    c   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    c   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  c   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    cP   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    c`   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    cd   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ct   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         cx   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        c|   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    c�Argo profile    3.1 1.2 19500101000000  5900960 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               A   AO  20111130103344  20190523124443  1514_5041_019                   2C  D   APEX                            2041                            062805                          846 @���m�?�1   @���m�?�@60��
=q�c&�x��1   GPS     Primary sampling: mixed [deeper than nominal 500dbar: discrete; nominal 500dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   B�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C���D��DffD!�3D.�fD:�fDG��DS� D`s3Dl� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B뙚B�ffB�ffB�ffB�ffB�ffC�3C�3C�3C�3C	�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C�3C!�3C#�3C%�3C'�3C)�3C+�3C-�3C/�3C1�3C3�3C5�3C7�3C9�3C;�3C=�3C?�3CA�3CC�3CE�3CG�3CI�3CK�3CM�3CO�3CQ�3CS�3CU�3CW�3CY�3C[�3C]�3C_�3Ca�3Cc�3Ce�3Cg�3Ci�3Ck�3Cm�3Co�3Cq�3Cs�3Cu�3Cw�3Cy�3C{�3C}�3C�3C�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�ٚC�s3DٚDS3D!� D.s3D:�3DGy�DS��D`` Dl��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��A��DA���A���A�S�A��\A���A��A��-A���A���A�A�VA��A�$�A��uA�x�A���A��wA�&�A��A���A��A��A�`BA���A��A���A�I�A�M�A��wA���A��TA�E�A�ƨA�M�A���A��A�hsA�A�ffA�VA��A�C�A���A��#A�M�Al�A~ �A|�AzI�AwAwG�Av��Au&�As�;Ar�uAop�AmVAj�DAhVAf�!AdbNAct�Ab��AaS�A`�\A_�A]S�A\�A[G�AX�`AV�9AU%ATE�AR�+AQ`BAO�wAN�AM�AM�AM33AL�/AL{AK
=AI��AI?}AG��AF�HAE��AEXAE�AD^5AC��AC?}AB�A?�A>�/A=�#A<��A;�PA9�
A9G�A9
=A8��A8^5A7��A6$�A3�A2�DA1�hA0r�A/��A.ĜA.{A,�A+�-A+�A*bNA)�hA(�A'�TA&ȴA&bNA%�A$��A$^5A$�A#\)A"��A"bNA!�A �A 5?Ap�A�A-At�AM�A  A��A�AZA��A�\A�wAffAO�A�HAXAn�AA�AdZA1A�PAoA�A9XAXA
�A	7LA�A�/AVA�-A��AQ�A|�AA��AC�A ��@��P@���@��@�o@���@���@�M�@���@�"�@�?}@�@�V@�C�@��y@��@�n�@��!@�@��-@�\)@��9@�o@�\)@���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��DA���A���A�S�A��\A���A��A��-A���A���A�A�VA��A�$�A��uA�x�A���A��wA�&�A��A���A��A��A�`BA���A��A���A�I�A�M�A��wA���A��TA�E�A�ƨA�M�A���A��A�hsA�A�ffA�VA��A�C�A���A��#A�M�Al�A~ �A|�AzI�AwAwG�Av��Au&�As�;Ar�uAop�AmVAj�DAhVAf�!AdbNAct�Ab��AaS�A`�\A_�A]S�A\�A[G�AX�`AV�9AU%ATE�AR�+AQ`BAO�wAN�AM�AM�AM33AL�/AL{AK
=AI��AI?}AG��AF�HAE��AEXAE�AD^5AC��AC?}AB�A?�A>�/A=�#A<��A;�PA9�
A9G�A9
=A8��A8^5A7��A6$�A3�A2�DA1�hA0r�A/��A.ĜA.{A,�A+�-A+�A*bNA)�hA(�A'�TA&ȴA&bNA%�A$��A$^5A$�A#\)A"��A"bNA!�A �A 5?Ap�A�A-At�AM�A  A��A�AZA��A�\A�wAffAO�A�HAXAn�AA�AdZA1A�PAoA�A9XAXA
�A	7LA�A�/AVA�-A��AQ�A|�AA��AC�A ��@��P@���@��@�o@���@���@�M�@���@�"�@�?}@�@�V@�C�@��y@��@�n�@��!@�@��-@�\)@��9@�o@�\)@���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�jB�?B�B��B��B��B��B�JB� Bw�Bp�B^5BW
BM�BG�B2-B$�B{B%B�B�)B��BB�?B�B��B�=Br�B]/BK�B@�B-B�BbB
�B
�B
��B
�dB
�XB
�FB
��B
�PB
�B
s�B
gmB
R�B
D�B
<jB
/B
.B
{B
B	��B	��B	�B	�B	�HB	�B	ƨB	�^B	�B	��B	��B	��B	��B	��B	�uB	�PB	�%B	� B	z�B	r�B	gmB	]/B	T�B	I�B	B�B	?}B	6FB	2-B	1'B	.B	/B	0!B	,B	�B	�B	"�B	�B	bB	VB	JB	
=B	VB	uB	1B	B��B��B��B�B�NB�TB�fB�`B�mB�NB�B��B��BĜBB�}B�}B�wB�^B�?B�3B�B�B��B��B��B��B��B��B��B��B��B��B�hB�VB�\B�uB�PB�JB�+B�B�B{�By�Bx�Bu�By�Bs�Bn�Bp�BjBe`BcTB_;B\)BZB^5BYBXB\)BVBR�BT�BM�BF�BC�BA�BA�BA�B?}B=qB:^B9XB9XB49B33B33B33B/B-B+B)�B(�B&�B%�B#�B#�B#�B#�B �B8RBA�Bw�B�dB	B	~�B	��B	��B	�y1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�wB�RB�B��B��B��B��B�\B�Bz�Bu�BaHBYBO�BM�B7LB(�B�BPB��B�HB��BƨB�RB�B��B�bBy�BbNBN�BE�B2-B!�B�B
��B
�5B
�B
�}B
�dB
�^B
�'B
�uB
�%B
w�B
jB
ZB
G�B
?}B
2-B
33B
�B
B	��B	��B	�B	�B	�mB	�)B	��B	��B	�3B	�B	��B	��B	��B	��B	��B	�hB	�7B	�B	�B	x�B	k�B	_;B	YB	L�B	E�B	A�B	8RB	33B	2-B	/B	1'B	2-B	/B	!�B	"�B	$�B	�B	oB	\B	VB	JB	\B	�B	PB	B��B��B��B�B�TB�ZB�mB�mB�yB�fB�5B��B��BǮBĜBÖB��B��B�qB�LB�?B�'B�B�B��B��B��B��B��B��B��B��B��B�uB�bB�hB��B�\B�PB�7B�B�B|�B{�By�B{�B|�Bu�Bq�Bs�Bk�BiyBe`B`BB_;B]/B`BBZBZB]/BYBVBXBO�BK�BE�BD�BC�BC�BB�B?}B=qB;dB<jB6FB5?B6FB6FB1'B0!B.B-B+B+B)�B&�B&�B$�B#�B �B8RBB�Bx�B�jB	B	~�B	��B	��B	�y1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
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
<D��<49X<#�
<#�
<#�
<#�
<#�
<#�
<49X<49X<#�
<#�
<49X<#�
<#�
<D��<D��<#�
<D��<#�
<#�
<#�
<D��<49X<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<49X<#�
<#�
<#�
<#�
<#�
<49X<49X<49X<#�
<#�
<49X<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.3 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201112181657262011121816572620111218165726  AO  ARGQ                                                                        20111130103344  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130103344  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20111218165726  IP                  G�O�G�O�G�O�                