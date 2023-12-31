CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:25Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  <l   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  =,   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  @(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  @�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  C�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  F�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  G�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  J�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  K\   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  NX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  QT   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  R   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  U   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  X�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    X�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    [�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ^�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  a�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    b(   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    b,   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    b0   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    b4   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  b8   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    bx   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    b�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    b�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         b�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         b�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        b�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    b�Argo profile    3.1 1.2 19500101000000  20181005191725  20181005191725  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��$���z1   @��%Q��b@5j~��"��d{���+1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@���A   A   A@  A`  A�  A�  A�  A�  A���A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BXffB_��Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�33B�  B���B���B�  B�  B�  B�  B���C   C  C�C�C�C
  C  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C9�fC;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT�CV  CX  CZ  C\�C^  C`  Cb  Cc�fCe�fCg�fCj  Cl�Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~�C�  C��fC��3C�  C��C�  C��C��C�  C�  C�  C�  C�  C��3C�  C��C��C��C��C��C�  C��3C�  C�  C��3C��3C��C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C��C��C�  C��3C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C��Dy� D�,{D�l{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�Q�@��A�\A"�\AB�\Ab�\A�G�A�G�A�G�A�G�A�{A�G�A�G�A�G�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BY
=B`=qBh=qBp��Bx��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B��B�Q�B��B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�B�Q�BȅB�Q�B�Q�B�Q�B�Q�B܅B�Q�B��B��B�Q�B�Q�B�Q�B�Q�B��C (�C(�CB�CB�CB�C
(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C(�C B�C"(�C$(�C&(�C((�C*(�C,(�C.(�C0(�C2(�C4(�C6(�C8(�C:\C<\C>(�C@(�CB(�CD(�CF(�CH(�CJ(�CL(�CN(�CP(�CR(�CTB�CV(�CX(�CZ(�C\B�C^(�C`(�Cb(�Cd\Cf\Ch\Cj(�ClB�CnB�Cp(�Cr(�Ct(�Cv(�Cx(�Cz(�C|(�C~B�C�{C���C��C�{C�!HC�{C�!HC�!HC�{C�{C�{C�{C�{C��C�{C�!HC�!HC�!HC�!HC�!HC�{C��C�{C�{C��C��C�!HC�!HC�!HC�!HC�!HC�{C�{C�{C�{C�{C�{C�{C�{C�{C�{C�!HC�{C�{C�!HC�!HC�{C��C��C��C��C�{C�{C�{C�{C�{C�{C�{C�{C��C�{C�!HDy�=D�1�D�q�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�  A�
=A�
=A�
=A�
=A�JA�VA�bA�bA�bA�oA�{A�{A�{A��A��A��A��A��A��A��A��A��A��A��A�1'A�5?A�M�A���A֩�Aգ�A��/A�z�A�(�A΃A̛�A˕�A�C�AȲ-A���A��A���A���A�1A�x�A��A�ȴA�ffA�A�A�=qA��^A��
A�v�A�ZA�oA�v�A���A�
=A�Q�A�$�A�+A��9A���A���A�\)A�A��#A��\A�{A��A���A�"�A�`BA�x�A���A�33A�bA�Q�A�n�A��A�p�A��wA�dZA��^A��A��DA�bA�C�A�\)A��9A�-A�ȴA�M�A��!A�t�A��A�/A�C�A��;A�O�A���Az��Av�Au��Au"�Ar�`AqoAp�`Ap�Ap(�Ao��An��Al�\Af1'A`bNA]�FA[x�AYXAY
=AX�9AXAVE�AT{AQAOC�AN$�AL�AK��AJ�AH��AFZAB�`A@�A>9XA=��A<��A<-A:�!A8ZA7`BA5t�A3O�A2~�A1��A1t�A0�/A/�wA/+A.(�A-�A+t�A)�mA(�\A'��A'+A&�/A&�+A%��A%oA$�A$1A#dZA"�HA!�A!%A r�AK�A�A��A��A��A�;A�A�+AE�A�AVA�A�TA33A�9A-A�A{A��A��AZAƨ@�@p�v@_Z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�  A�
=A�
=A�
=A�
=A�JA�VA�bA�bA�bA�oA�{A�{A�{A��A��A��A��A��A��A��A��A��A��A��A�1'A�5?A�M�A���A֩�Aգ�A��/A�z�A�(�A΃A̛�A˕�A�C�AȲ-A���A��A���A���A�1A�x�A��A�ȴA�ffA�A�A�=qA��^A��
A�v�A�ZA�oA�v�A���A�
=A�Q�A�$�A�+A��9A���A���A�\)A�A��#A��\A�{A��A���A�"�A�`BA�x�A���A�33A�bA�Q�A�n�A��A�p�A��wA�dZA��^A��A��DA�bA�C�A�\)A��9A�-A�ȴA�M�A��!A�t�A��A�/A�C�A��;A�O�A���Az��Av�Au��Au"�Ar�`AqoAp�`Ap�Ap(�Ao��An��Al�\Af1'A`bNA]�FA[x�AYXAY
=AX�9AXAVE�AT{AQAOC�AN$�AL�AK��AJ�AH��AFZAB�`A@�A>9XA=��A<��A<-A:�!A8ZA7`BA5t�A3O�A2~�A1��A1t�A0�/A/�wA/+A.(�A-�A+t�A)�mA(�\A'��A'+A&�/A&�+A%��A%oA$�A$1A#dZA"�HA!�A!%A r�AK�A�A��A��A��A�;A�A�+AE�A�AVA�A�TA33A�9A-A�A{A��A��AZAƨ@�@p�v@_Z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B+B+B+B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B+B)�B)�B)�B)�B-B0!B;dB��B�/BJ�B]/B`BBffBjBt�Bz�B��B��B�B�'B�!B�B�FB�qB��B��B�#B�/B�BB�HB�BB�ZB�fB�fB�ZB�)B�}B�!B�B�B��B��B��B��B��B�PBz�Bu�Bs�Bq�Bl�BbNB\)BW
BM�BB�B-B�B��B�TB�BÖB�9B��B�+BjBaHBYBQ�BD�B6FB(�B\B
�B
�B
�yB
�B
��B
��B
�VB
�B
x�B
iyB
:^B
�B
JB
+B	��B	�B	�yB	�fB	�TB	�;B	�B	ƨB	��B	~�B	q�B	hsB	`BB	aHB	]/B	T�B	C�B	5?B	$�B	�B	uB	JB	B��B��B�B�;B�B��B��B��BɺBǮBŢBÖBB�qB�RB�9B�'B�!B�B�'B�B��B��B��B��B��B��B��B�{B�{B�uB�oB�oB�hB�oB�uB�bB�PB�DB�1B�%B�B�B}�By�Bx�Bw�Bu�Bs�Bp�Bm�BjBhsBdZBaHB]/BZBW
BT�BS�B
�B
vB
#22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B+B+B+B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B)�B+B)�B)�B)�B)�B-B0!B;dB��B�/BJ�B]/B`BBffBjBt�Bz�B��B��B�B�'B�!B�B�FB�qB��B��B�#B�/B�BB�HB�BB�ZB�fB�fB�ZB�)B�}B�!B�B�B��B��B��B��B��B�PBz�Bu�Bs�Bq�Bl�BbNB\)BW
BM�BB�B-B�B��B�TB�BÖB�9B��B�+BjBaHBYBQ�BD�B6FB(�B\B
�B
�B
�yB
�B
��B
��B
�VB
�B
x�B
iyB
:^B
�B
JB
+B	��B	�B	�yB	�fB	�TB	�;B	�B	ƨB	��B	~�B	q�B	hsB	`BB	aHB	]/B	T�B	C�B	5?B	$�B	�B	uB	JB	B��B��B�B�;B�B��B��B��BɺBǮBŢBÖBB�qB�RB�9B�'B�!B�B�'B�B��B��B��B��B��B��B��B�{B�{B�uB�oB�oB�hB�oB�uB�bB�PB�DB�1B�%B�B�B}�By�Bx�Bw�Bu�Bs�Bp�Bm�BjBhsBdZBaHB]/BZBW
BT�BS�B
�B
vB
#22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.16 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191725                              AO  ARCAADJP                                                                    20181005191725    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191725  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191725  QCF$                G�O�G�O�G�O�8000            