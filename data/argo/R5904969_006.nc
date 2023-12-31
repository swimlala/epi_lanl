CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS   �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-24T14:13:59Z creation      
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
resolution        =���   axis      Z        L  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  <�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  =�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  @�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     L  A�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  D�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  HH   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  I   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  Lh   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  M<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  P�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  S�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  T�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                  �  W�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     L  X�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  \   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    \D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    _D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    bD   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  eD   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ep   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    et   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ex   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    e|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  e�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    e�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    e�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    e�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         e�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         e�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        e�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    e�Argo profile    3.1 1.2 19500101000000  20181024141359  20181024141359  5904969 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               A   AO  6784                            2B  A   APEX                            7725                            111215                          846 @׶����z1   @׶�@yoL@3a$�/�c�5?|�1   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      A   A   A   @9��@�  @�  @���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  Dy�D�B�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @<(�@�G�@�G�@�{A ��A@��A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B (�B(�B(�B(�B (�B((�B0(�B8(�B@(�BH(�BP(�BX(�B`(�Bh(�Bp(�Bx(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B��HB�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{C 
=C
=C
=C#�C
=C

=C
=C
=C
=C
=C
=C
=C
=C
=C
=C
=C #�C"
=C$
=C&
=C(
=C*
=C,
=C.
=C0
=C2
=C4
=C6
=C8
=C:#�C<
=C>
=C@
=CB
=CD
=CF
=CH
=CJ
=CL
=CN
=CP
=CR
=CT
=CV
=CX
=CZ
=C\
=C^
=C`
=Cb
=Cd
=Cf
=Ch
=Cj
=Cl
=Cn
=Cp
=Cr
=Ct
=Cv
=Cx
=Cz
=C|
=C~
=C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C��RC�C�C�C�C�C�C�Dy��D�C�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�S�A�ZA�XA�XA�bNA�`BA�ZA�XA�XA�VA�M�A�?}A�(�A� �A��A��A�1A��TAԑhA�+AҾwA�A�A���Aљ�A�?}A�ĜA�\)A�&�Aϰ!A�A�A�{A��A΁A�oA���A͇+A�Q�A�C�A̡�A�C�A��yA˥�A�~�A���A���A��#A��mA��A�1A��A�(�A�+A�"�A���A�1'A�1A�|�A� �AǕ�Aǣ�A�S�A�I�Ať�A�v�A�I�A��A��TAİ!A�-A�ffA�VA�?}A�"�A��A��A�oA�bA���A���A�A��DA���A�r�A�|�A��A�jA�%A�7LA�~�A�z�A���A���A�dZA���A��A�ZA���A��A���A�G�A��A���A�bA�dZA���A��uA���A���A��A��A�;dA�G�A�7LA��TA���A�5?A�-A�l�A��A�hsA���A�
=A�A�ȴA�dZA�(�A��A{S�Av �Ar^5Ao"�Al�yAi�TAg�7AeS�Ab�Ab-A`ĜA^�A\bAZ$�AYK�AX�`AW��AU�AQK�AN^5AM�wAL�AGƨAFA�AE\)ADn�AB��AA%A@{A?�7A=�A;oA9
=A5�mA4A�A3��A1C�A0-A0�A.�A-VA,�+A,A+A+O�A*-A)
=A'A'%A%�A%VA$�A#;dA"I�A!|�A ��A �9A �A�A33A5?A  A&�A�yA`BAp�A{A��A�hA��AA�A�A	�TA�-AĜAQ�AVAv�AbNAZAZAJ@��@}��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�S�A�ZA�XA�XA�bNA�`BA�ZA�XA�XA�VA�M�A�?}A�(�A� �A��A��A�1A��TAԑhA�+AҾwA�A�A���Aљ�A�?}A�ĜA�\)A�&�Aϰ!A�A�A�{A��A΁A�oA���A͇+A�Q�A�C�A̡�A�C�A��yA˥�A�~�A���A���A��#A��mA��A�1A��A�(�A�+A�"�A���A�1'A�1A�|�A� �AǕ�Aǣ�A�S�A�I�Ať�A�v�A�I�A��A��TAİ!A�-A�ffA�VA�?}A�"�A��A��A�oA�bA���A���A�A��DA���A�r�A�|�A��A�jA�%A�7LA�~�A�z�A���A���A�dZA���A��A�ZA���A��A���A�G�A��A���A�bA�dZA���A��uA���A���A��A��A�;dA�G�A�7LA��TA���A�5?A�-A�l�A��A�hsA���A�
=A�A�ȴA�dZA�(�A��A{S�Av �Ar^5Ao"�Al�yAi�TAg�7AeS�Ab�Ab-A`ĜA^�A\bAZ$�AYK�AX�`AW��AU�AQK�AN^5AM�wAL�AGƨAFA�AE\)ADn�AB��AA%A@{A?�7A=�A;oA9
=A5�mA4A�A3��A1C�A0-A0�A.�A-VA,�+A,A+A+O�A*-A)
=A'A'%A%�A%VA$�A#;dA"I�A!|�A ��A �9A �A�A33A5?A  A&�A�yA`BAp�A{A��A�hA��AA�A�A	�TA�-AĜAQ�AVAv�AbNAZAZAJ@��@}��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
ZB
YB
YB
YB
YB
YB
ZB
ZB
ZB
YB
YB
YB
XB
W
B
W
B
VB
S�B
N�B
6FB
�B
{B
\B
oB
{B
bB
PB
bB
�B
"�B
/B
=qB
O�B
l�B
��B
��B
�RB
ĜB
ĜB
�9B
�-B
�9B
�XB
ǮB
�
B
�HB
�sB
�B
�B
��B
��B
��BBBB�B�B�B:^BO�B}�B��B��BŢBŢB��B��B�B�HB)�BK�BL�BO�BR�BR�BR�B�B��B�B�XB��B�
B�ZB�5B��BÖB�XB�'B��B��B��B�{B�Br�BffB[#BD�B6FB!�B�B�BhB%B��B��B��B�B�sB�BB�'B��B�\B{�Bq�BdZB@�B.BVB
�B
��B
ŢB
�RB
��B
�B
`BB
I�B
A�B
+B
B	�TB	��B	�dB	�B	��B	��B	�PB	�7B	�+B	z�B	p�B	cTB	\)B	XB	O�B	B�B	,B	�B	hB	DB��B��B�B�B�`B�#B�B��B��BƨB��B�jB�XBB�^B�FB�LB�wB�XB�RB�FB�RB�XB�dB�dB�9B�'B�!B�B��B��B��B��B��B��B��B��B��B�B�RB�dB��B��BŢB��B�dB�-B�B��B��B��B�PB�=B�7B�7B�1B�VB��B��B��B
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B
ZB
YB
YB
YB
YB
YB
ZB
ZB
ZB
YB
YB
YB
XB
W
B
W
B
VB
S�B
N�B
6FB
�B
{B
\B
oB
{B
bB
PB
bB
�B
"�B
/B
=qB
O�B
l�B
��B
��B
�RB
ĜB
ĜB
�9B
�-B
�9B
�XB
ǮB
�
B
�HB
�sB
�B
�B
��B
��B
��BBBB�B�B�B:^BO�B}�B��B��BŢBŢB��B��B�B�HB)�BK�BL�BO�BR�BR�BR�B�B��B�B�XB��B�
B�ZB�5B��BÖB�XB�'B��B��B��B�{B�Br�BffB[#BD�B6FB!�B�B�BhB%B��B��B��B�B�sB�BB�'B��B�\B{�Bq�BdZB@�B.BVB
�B
��B
ŢB
�RB
��B
�B
`BB
I�B
A�B
+B
B	�TB	��B	�dB	�B	��B	��B	�PB	�7B	�+B	z�B	p�B	cTB	\)B	XB	O�B	B�B	,B	�B	hB	DB��B��B�B�B�`B�#B�B��B��BƨB��B�jB�XBB�^B�FB�LB�wB�XB�RB�FB�RB�XB�dB�dB�9B�'B�!B�B��B��B��B��B��B��B��B��B��B�B�RB�dB��B��BŢB��B�dB�-B�B��B��B��B�PB�=B�7B�7B�1B�VB��B��B��B
�B
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=-0.04 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181024141359                              AO  ARCAADJP                                                                    20181024141359    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181024141359  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181024141359  QCF$                G�O�G�O�G�O�0               