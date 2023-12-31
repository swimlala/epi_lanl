CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  G   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-10-05T19:17:46Z creation      
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
resolution        =���   axis      Z          9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  >�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       ?�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  D�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       F8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       KT   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  Pp   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       Q�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  V�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       X   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       ]8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  bT   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       c�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 H  h�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       j    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  o   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    oL   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    rL   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    uL   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  xL   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    xx   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    x|   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    x�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    x�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  x�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    x�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    x�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    x�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         x�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         x�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        x�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    x�Argo profile    3.1 1.2 19500101000000  20181005191746  20181005191746  5904954 US ARGO PROJECT                                                 CARL SZCZECHOWSKI                                               PRES            TEMP            PSAL               �A   AO  6557                            2B  A   APEX                            7468                            062512                          846 @��$�[�1   @��%DDV�@58Q���d�I�^51   GPS     Primary sampling: discrete []                                                                                                                                                                                                                                      �A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C�C
  C  C�C  C  C�C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\�C^�C`�Cb�Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cw�fCz  C|  C~  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C��3C�  C��C�  C��3C�  C�  C��C�  C�  C�  C��3C��3C�  C��C�  C��3C��3C�  C��C�  C�  C��3C��3C�  C�  C�  C�  C��3C�  C��3C��3C�  C�  C�  C�  C��C�  C��3C�  C��C�  C�  C��C��C��3C�  C�  C��3C��3C��3C�  C�  C�  C��3C�  C�  C��3C��C��C�  C��C��3C�  C�  C�  C��C��C��C�  C��3C�  C�  C��C��C��C��C��C��C��3C��C��C�  C�  C�  C��D fD y�DfDy�D  D�fDfD� D  Dy�DfD� D  D�fDfD�fD  D� D	fD	y�D	��D
y�D  D�fDfD� D��D� D��D� D  D� D  Dy�D��D� D  Dy�D  Dy�DfD�fD  D� D��D�fD  D� D  Dy�D  Dy�D  D�fD��Dy�D  D�fD�3D� DfDy�D  D�fD fD �fD!fD!y�D"fD"�fD#  D#�fD$fD$� D$��D%y�D&  D&� D&��D'� D(  D(� D)  D)� D*  D*� D+  D+� D,fD,� D-fD-�fD.  D.�fDy�fD�M�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��R@��RA\)A?\)A_\)A\)A��A��A��A��AϮA߮A�A��B�
B�
B�
B�
B'�
B/�
B7�
B?�
BG�
BO�
BW�
B_�
Bg�
Bo�
Bw�
B�
B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��C��C��C]C]C	��C��C]C��C��C]C]C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CZ]C\]C^]C`]Cb]Cd]Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw�)Cy��C{��C}��C��C���C���C���C���C���C��C���C���C���C���C���C���C���C��C���C���C��C���C��C���C��C���C��C���C���C��C���C���C���C��C��C���C��C���C��C��C���C��C���C���C��C��C���C���C���C���C��C���C��C��C���C���C���C���C��C���C��C���C��C���C���C��C��C��C���C���C��C��C��C���C���C���C��C���C���C��C��C�{C���C��C��C���C���C���C��C��C��C���C��C���C���C��C��C��C��C��C��C��C��C��C���C���C���C��D �D wD�DwD�qD��D�D}qD�qDwD�D}qD�qD��D�D��D�qD}qD	�D	wD	�D
wD
�qD��D�D}qD�D}qD�D}qD�qD}qD�qDwD�D}qD�qDwD�qDwD�D��D�qD}qD�D��D�qD}qD�qDwD�qDwD�qD��D�DwD�qD��D�D}qD�DwD�qD��D �D ��D!�D!wD"�D"��D"�qD#��D$�D$}qD$�D%wD%�qD&}qD&�D'}qD'�qD(}qD(�qD)}qD)�qD*}qD*�qD+}qD,�D,}qD-�D-��D-�qD.��Dy��D�L{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AAAAA+A7ADA\A�A¡�A¼jA�ȴA���A��A��;A��/A���A�x�A�XA²-A���A�{A��A��/A���A�  A���A�=qAuA��A���A�I�A�?}A�p�A�^5A�C�A�$�A�bA�oA� �A�&�A�S�A�ffA�bA�  A��A�?}A��+A�  A�7LA��mA��9A�bNA��!A�&�A��^A�?}A��A���A���A�9XA�  A���A���A��9A�VA��hA��FA�VA��A�A��A��wA�A�A���A�?}A�ȴA��mA�XA���A��PA��
A�=qA��
A�S�A���A�dZA�JA��uA�VA���A�`BA��TA�S�A�XA�|�A�A��uA�ffA��A���A�t�A��A��+A��A�A}|�Ay�FAyO�Ax��Ax$�Av�At�Ar�Ar5?Aq��ApjAn�Ah�RAg�Ae��Ad��Ac��A_C�A^�A^v�A^5?A]��A]A\9XA[��A[33AZ��AY��AX��AX$�AWG�ATA�AQhsAO��AN��AN1AMAMC�AK��AJ��AJjAH�`AFz�AB�AAoA@M�A?p�A?%A>��A=��A<�HA<1A;�wA;O�A:�A:  A9��A9?}A8bA7VA6ffA5��A4��A4A2�/A2Q�A1p�A0�`A/�A.�/A-��A,ZA+��A+A)�
A(v�A'XA&��A&�!A&E�A%A#��A"��A!�^A�A�jA��AĜAM�A��AƨA�7A%A��A ĜA �@���@��H@�x�@�l�@�1'@�@���@�Ĝ@�@ꗍ@�p�@�u@�1@��y@��@�`B@�Ĝ@�1@�@�+@�\@�E�@��@�Ĝ@�=q@� �@�n�@١�@ם�@�hs@ӶF@��@�  @͡�@̓u@ˍP@�V@�p�@��@ǍP@�n�@���@��@�|�@��@��@���@�t�@�;d@�
=@��y@�E�@���@���@��@���@�@��h@��@�p�@�G�@���@���@��D@�Z@��;@���@��@�t�@��@��@�+@�@�$�@�l�@�I�@��@���@���@�j@��
@���@���@��@���@��@���@��D@�1'@���@�\)@��!@��!@�^5@�E�@�M�@�^5@��-@��@���@�r�@�(�@�1'@�1'@��w@�"�@�@��@��R@��\@�n�@�M�@��T@���@���@��h@��@�1'@��P@�@���@���@��7@�~@zff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 AAAAA+A7ADA\A�A¡�A¼jA�ȴA���A��A��;A��/A���A�x�A�XA²-A���A�{A��A��/A���A�  A���A�=qAuA��A���A�I�A�?}A�p�A�^5A�C�A�$�A�bA�oA� �A�&�A�S�A�ffA�bA�  A��A�?}A��+A�  A�7LA��mA��9A�bNA��!A�&�A��^A�?}A��A���A���A�9XA�  A���A���A��9A�VA��hA��FA�VA��A�A��A��wA�A�A���A�?}A�ȴA��mA�XA���A��PA��
A�=qA��
A�S�A���A�dZA�JA��uA�VA���A�`BA��TA�S�A�XA�|�A�A��uA�ffA��A���A�t�A��A��+A��A�A}|�Ay�FAyO�Ax��Ax$�Av�At�Ar�Ar5?Aq��ApjAn�Ah�RAg�Ae��Ad��Ac��A_C�A^�A^v�A^5?A]��A]A\9XA[��A[33AZ��AY��AX��AX$�AWG�ATA�AQhsAO��AN��AN1AMAMC�AK��AJ��AJjAH�`AFz�AB�AAoA@M�A?p�A?%A>��A=��A<�HA<1A;�wA;O�A:�A:  A9��A9?}A8bA7VA6ffA5��A4��A4A2�/A2Q�A1p�A0�`A/�A.�/A-��A,ZA+��A+A)�
A(v�A'XA&��A&�!A&E�A%A#��A"��A!�^A�A�jA��AĜAM�A��AƨA�7A%A��A ĜA �@���@��H@�x�@�l�@�1'@�@���@�Ĝ@�@ꗍ@�p�@�u@�1@��y@��@�`B@�Ĝ@�1@�@�+@�\@�E�@��@�Ĝ@�=q@� �@�n�@١�@ם�@�hs@ӶF@��@�  @͡�@̓u@ˍP@�V@�p�@��@ǍP@�n�@���@��@�|�@��@��@���@�t�@�;d@�
=@��y@�E�@���@���@��@���@�@��h@��@�p�@�G�@���@���@��D@�Z@��;@���@��@�t�@��@��@�+@�@�$�@�l�@�I�@��@���@���@�j@��
@���@���@��@���@��@���@��D@�1'@���@�\)@��!@��!@�^5@�E�@�M�@�^5@��-@��@���@�r�@�(�@�1'@�1'@��w@�"�@�@��@��R@��\@�n�@�M�@��T@���@���@��h@��@�1'@��P@�@���@���@��7@�~@zff111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�FBƨB�`B��B  BB+B	7B
=B��B��BVB#�B1'B/B.B.BJBB�B>wBYBN�BK�BR�BdZBe`BffBdZBe`Bk�Bs�Bv�B�1B��B�'BȴB��BɺB��B��B�B�
B��B��B��B��B�qB�qB�jB�dB�XB�FB�-B��B�{B�VB�VB��B��B��B�hB�+Bm�BT�BN�BI�B>wB&�BVB��B�;B�jB�-B�B�B�B�B��B�uB�Bx�Bq�Bl�BcTBZBK�B>wB5?B!�BVB
��B
�B
�B
�B
�9B
��B
l�B
G�B
)�B
$�B
�B
�B
\B	��B	�B	�B	�`B	�#B	��B	��B	��B	�uB	�JB	~�B	cTB	]/B	]/B	[#B	YB	VB	Q�B	M�B	I�B	E�B	?}B	9XB	2-B	)�B	�B	B��B��B�B�B�B�sB�fB�TB�#B��BƨBɺBɺB��B��B�B�5B�BB�5B�NB�ZB�TB�;B�)B�B�
B�B��B��B��B��BƨBƨBƨBĜB��B�}B�jB�XB�LB�?B�9B�!B�B�B�B��B��B��B��B��B�B�B�B�B�B�B�B�B�B� B~�B|�B{�By�Bw�Bw�Bu�Bw�Bv�Bw�B{�B{�Bz�By�By�Bx�Bv�Bt�Bt�Bv�By�B|�B�B�B�B�%B�=B�%B�B�B}�B{�B{�Bz�Bv�Bu�Bx�Bz�B|�B}�B~�B� B�B�B�+B�+B�7B�VB�bB�uB�{B�{B�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�'B�XB��BBĜBȴBǮB��B��B�#B�NB�NB�NB�NB�NB�BB�)B�HB�ZB�`B�mB�B�B�B��B��B��B	B	B	%B	1B	VB	hB	hB	uB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	%�B	,B	.B	0!B	0!B	6FB	:^B	=qB	?}B	A�B	D�B	H�B	�FB
222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 B�B�B�B�B�B�B�B�B�FBƨB�`B��B  BB+B	7B
=B��B��BVB#�B1'B/B.B.BJBB�B>wBYBN�BK�BR�BdZBe`BffBdZBe`Bk�Bs�Bv�B�1B��B�'BȴB��BɺB��B��B�B�
B��B��B��B��B�qB�qB�jB�dB�XB�FB�-B��B�{B�VB�VB��B��B��B�hB�+Bm�BT�BN�BI�B>wB&�BVB��B�;B�jB�-B�B�B�B�B��B�uB�Bx�Bq�Bl�BcTBZBK�B>wB5?B!�BVB
��B
�B
�B
�B
�9B
��B
l�B
G�B
)�B
$�B
�B
�B
\B	��B	�B	�B	�`B	�#B	��B	��B	��B	�uB	�JB	~�B	cTB	]/B	]/B	[#B	YB	VB	Q�B	M�B	I�B	E�B	?}B	9XB	2-B	)�B	�B	B��B��B�B�B�B�sB�fB�TB�#B��BƨBɺBɺB��B��B�B�5B�BB�5B�NB�ZB�TB�;B�)B�B�
B�B��B��B��B��BƨBƨBƨBĜB��B�}B�jB�XB�LB�?B�9B�!B�B�B�B��B��B��B��B��B�B�B�B�B�B�B�B�B�B� B~�B|�B{�By�Bw�Bw�Bu�Bw�Bv�Bw�B{�B{�Bz�By�By�Bx�Bv�Bt�Bt�Bv�By�B|�B�B�B�B�%B�=B�%B�B�B}�B{�B{�Bz�Bv�Bu�Bx�Bz�B|�B}�B~�B� B�B�B�+B�+B�7B�VB�bB�uB�{B�{B�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�'B�XB��BBĜBȴBǮB��B��B�#B�NB�NB�NB�NB�NB�BB�)B�HB�ZB�`B�mB�B�B�B��B��B��B	B	B	%B	1B	VB	hB	hB	uB	�B	�B	�B	�B	�B	�B	�B	�B	 �B	"�B	%�B	,B	.B	0!B	0!B	6FB	:^B	=qB	?}B	A�B	D�B	H�B	�FB
222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.04 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20181005191746                              AO  ARCAADJP                                                                    20181005191746    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181005191746  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181005191746  QCF$                G�O�G�O�G�O�8000            