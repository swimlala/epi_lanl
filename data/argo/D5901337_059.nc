CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS      N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-12-05 AOML 2.2 creation; 2015-12-13T23:15:40Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z           9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�        C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�        M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�        ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �L   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �\   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        �p   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        �t   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �x   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �|Argo profile    3.1 1.2 19500101000000  5901337 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               ;A   AO  20111205113346  20190522121836  1901_5055_059                   2C  D   APEX                            2140                            040306                          846 @Ի�+@	1   @Ի��Q�@.9�"��`�cs�O�;d1   GPS     Primary sampling: mixed [deeper than nominal 990dbar: discrete; nominal 990dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�33@���@���A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  BbffBe��Bp  Bx  B�  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� DfD�fDfD� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1�fD2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;y�D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj�fDk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dys3D��3D�@ D��3D�� D��D�FfD�s3D�� D�	�D�9�D��3DǬ�D��fD�&fDڐ Dਗ਼D��fD�fD�S3D�P 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@fff@���@���A  A6ffAX  Ax  A�  A�  A�  A�  A�  A�  A�  A�  B  B  B  B  B&  B.  B6  B>  BF  BN  BV  B`ffBc��Bn  Bv  B~  B�  B�  B�  B�33B�  B���B�  B�  B�  B�  B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C� C� C� C� C	� C� C� C� C� C� C� C� C� C� C� C� C!� C#� C%� C'� C)� C+� C-� C/��C1� C3� C5� C7� C9� C;� C=� C?� CA� CC� CE� CG� CI� CK� CM� CO� CQ� CS� CU� CW� CY� C[� C]� C_��Ca� Cc� Ce� Cg� Ci� Ck� Cm� Co� Cq� Cs� Cu� Cw� Cy� C{��C}� C� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� C�� D ` D � D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D	` D	� D
` D
� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` D� D` DٚD` D�fDffD�fD` D� D ` D � D!` D!� D"` D"� D#` D#� D$` D$� D%` D%� D&` D&� D'` D'� D(` D(� D)` D)� D*` D*� D+` D+� D,` D,� D-` D-� D.` D.� D/` D/� D0` D0� D1ffD1� D2` D2� D3` D3� D4` D4� D5` D5� D6` D6� D7` D7� D8` D8� D9` D9� D:` D:� D;Y�D;� D<` D<� D=` D=� D>` D>� D?` D?� D@` D@� DA` DA� DB` DB� DC` DC� DD` DD� DE` DE� DF` DF� DG` DG� DH` DH� DI` DI� DJ` DJ� DK` DK� DL` DL�fDM` DM� DN` DN� DO` DO� DP` DP� DQ` DQ� DR` DR� DS` DS� DT` DT� DU` DU� DV` DV� DW` DW� DX` DX� DY` DY� DZ` DZ� D[` D[� D\` D\� D]` D]� D^` D^� D_` D_� D`` D`� Da` Da� Db` Db� Dc` Dc� Dd` Dd� De` De� Df` Df� Dg` Dg� Dh` Dh� Di` Di� DjffDj� Dk` Dk� Dl` Dl� Dm` Dm� Dn` Dn� Do` Do� Dp` Dp� Dq` Dq� Dr` Dr� Ds` Ds� Dt` Dt� Du` Du� DyS3D��3D�0 D�s3D�� D���D�6fD�c3D�� D���D�)�D�s3Dǜ�D��fD�fDڀ D���D��fD�fD�C3D�@ 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��#AĸRAġ�Aď\AąAăA�z�A�t�A�n�A�jA�hsA�dZA�dZA�dZA�dZA�bNA�`BA�^5A�`BA�bNA�bNA�bNA�bNA�`BA�`BA�`BA�ffA�n�A���A���A�\)A� �A���A�l�A��A�XA��A��#A�A�  A�33A���A� �A���A��A�I�A�oA�z�A�S�A�1'A�jA�\)A��A�dZA���A�S�A��#A�(�A���A�VA��A�bNA��PA��TA�n�A�?}A�ƨA�  A�A�A��
A�A�A��hA���A��7A�VA���A�+A�  A��uA�9XA��A�oA���A���A�+A���A�(�A�\)A��mA�Q�A���A�I�A�v�A�bNA�C�A�/A~Q�A|M�Ay��AuAo"�AhM�Ac7LA_�mA]AX�AT�AQp�AO+AL��AI�TAH�DAE�
AChsAA��A@�9A@A�A>�A;`BA6��A4  A1�PA0-A/A-A,�A)?}A'oA&~�A'&�A'`BA(VA*��A,Q�A,��A-�A-"�A-hsA,�/A,�+A,$�A+��A)�A)p�A%7LA!33A��A{A;dAx�AO�AO�A5?AA�A5?A$�Al�A�!A�
A?}AVAK�A%AQ�AA�-A7LA�/A�DA�`AAhsAI�A`BAG�A�\A~�AI�AbNAz�A~�A5?A33A�9AVA|�A+A�yA��A�HA�!A�!A�AoAA&�A�yA�A�mA
=A�`A��A�!A  AoA
v�A
JA	�7A	dZA	G�A	G�A	/AZA�AO�A�A�9A1'A(�A1A+AVA�TAS�A��AI�A$�A��A��A ��A ��A v�A ^5@��m@�@�ff@��T@��7@�hs@��@���@��m@�+@�-@��#@�?}@�r�@�\)@���@�&�@�b@��H@��@�bN@��;@�+@�@��/@�bN@�(�@�b@��m@�w@�@�%@�@���@�ff@���@��;@�"�@�-@��@��`@�j@߮@�K�@�
=@��@ް!@�v�@�$�@ݑh@�Ĝ@���@۶F@۶F@۶F@۶F@�o@�M�@���@�@ّh@�?}@��`@�r�@�(�@ׅ@���@�~�@ՙ�@�&�@��/@�(�@��;@ӍP@��y@җ�@�^5@�$�@ѩ�@��@мj@ϕ�@�C�@��@��@���@���@Ώ\@�M�@�J@͉7@���@̛�@�I�@��m@�\)@���@ʗ�@�5?@��@�x�@ȣ�@��@�C�@���@Ə\@�5?@���@�p�@ě�@�j@� �@ÍP@�
=@�ȴ@��#@�O�@��@�A�@��;@�S�@�o@��@��H@��R@��+@�V@��@�hs@���@���@��@�@��@�/@���@���@�9X@��
@��@���@�V@���@�?}@�%@��@���@��u@�Q�@��F@�@���@�?}@��@��j@��@�1@�\)@�@��@��+@�^5@�^5@�n�@�M�@�@��7@�?}@��@�Ĝ@�bN@�Z@�A�@��m@��@�l�@�K�@�"�@��y@��!@�$�@��@�V@��@��@��@� �@��
@���@�K�@��@���@��!@��\@�E�@�$�@��#@�`B@��/@���@�A�@���@���@�S�@�+@�+@�@�ȴ@��!@�n�@�=q@�-@�{@���@���@��@�I�@���@��@��P@�l�@��!@�@���@�p�@�V@��/@�z�@�(�@��m@��@��@�t�@�33@���@�v�@�{@��^@��7@�7L@��/@��D@�A�@��;@��F@�\)@�@���@�ff@��@��#@�G�@���@�Ĝ@��@���@��u@�r�@��@�X@���@��;@}�T@t�@k33@a�@W\)@M��@C�
@<1@4��@.�y@)7L@#S�@@1'@��@+@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��#AĸRAġ�Aď\AąAăA�z�A�t�A�n�A�jA�hsA�dZA�dZA�dZA�dZA�bNA�`BA�^5A�`BA�bNA�bNA�bNA�bNA�`BA�`BA�`BA�ffA�n�A���A���A�\)A� �A���A�l�A��A�XA��A��#A�A�  A�33A���A� �A���A��A�I�A�oA�z�A�S�A�1'A�jA�\)A��A�dZA���A�S�A��#A�(�A���A�VA��A�bNA��PA��TA�n�A�?}A�ƨA�  A�A�A��
A�A�A��hA���A��7A�VA���A�+A�  A��uA�9XA��A�oA���A���A�+A���A�(�A�\)A��mA�Q�A���A�I�A�v�A�bNA�C�A�/A~Q�A|M�Ay��AuAo"�AhM�Ac7LA_�mA]AX�AT�AQp�AO+AL��AI�TAH�DAE�
AChsAA��A@�9A@A�A>�A;`BA6��A4  A1�PA0-A/A-A,�A)?}A'oA&~�A'&�A'`BA(VA*��A,Q�A,��A-�A-"�A-hsA,�/A,�+A,$�A+��A)�A)p�A%7LA!33A��A{A;dAx�AO�AO�A5?AA�A5?A$�Al�A�!A�
A?}AVAK�A%AQ�AA�-A7LA�/A�DA�`AAhsAI�A`BAG�A�\A~�AI�AbNAz�A~�A5?A33A�9AVA|�A+A�yA��A�HA�!A�!A�AoAA&�A�yA�A�mA
=A�`A��A�!A  AoA
v�A
JA	�7A	dZA	G�A	G�A	/AZA�AO�A�A�9A1'A(�A1A+AVA�TAS�A��AI�A$�A��A��A ��A ��A v�A ^5@��m@�@�ff@��T@��7@�hs@��@���@��m@�+@�-@��#@�?}@�r�@�\)@���@�&�@�b@��H@��@�bN@��;@�+@�@��/@�bN@�(�@�b@��m@�w@�@�%@�@���@�ff@���@��;@�"�@�-@��@��`@�j@߮@�K�@�
=@��@ް!@�v�@�$�@ݑh@�Ĝ@���@۶F@۶F@۶F@۶F@�o@�M�@���@�@ّh@�?}@��`@�r�@�(�@ׅ@���@�~�@ՙ�@�&�@��/@�(�@��;@ӍP@��y@җ�@�^5@�$�@ѩ�@��@мj@ϕ�@�C�@��@��@���@���@Ώ\@�M�@�J@͉7@���@̛�@�I�@��m@�\)@���@ʗ�@�5?@��@�x�@ȣ�@��@�C�@���@Ə\@�5?@���@�p�@ě�@�j@� �@ÍP@�
=@�ȴ@��#@�O�@��@�A�@��;@�S�@�o@��@��H@��R@��+@�V@��@�hs@���@���@��@�@��@�/@���@���@�9X@��
@��@���@�V@���@�?}@�%@��@���@��u@�Q�@��F@�@���@�?}@��@��j@��@�1@�\)@�@��@��+@�^5@�^5@�n�@�M�@�@��7@�?}@��@�Ĝ@�bN@�Z@�A�@��m@��@�l�@�K�@�"�@��y@��!@�$�@��@�V@��@��@��@� �@��
@���@�K�@��@���@��!@��\@�E�@�$�@��#@�`B@��/@���@�A�@���@���@�S�@�+@�+@�@�ȴ@��!@�n�@�=q@�-@�{@���@���@��@�I�@���@��@��P@�l�@��!@�@���@�p�@�V@��/@�z�@�(�@��m@��@��@�t�@�33@���@�v�@�{@��^@��7@�7L@��/@��D@�A�@��;@��F@�\)@�@���@�ff@��@��#@�G�@���@�Ĝ@��@���@��u@�r�@��@�X@���@��;@}�T@t�@k33@a�@W\)@M��@C�
@<1@4��@.�y@)7L@#S�@@1'@��@+@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�B	�B	 �B	 �B	"�B	"�B	"�B	"�B	#�B	#�B	#�B	#�B	$�B	$�B	$�B	$�B	$�B	$�B	$�B	$�B	$�B	$�B	$�B	%�B	%�B	'�B	)�B	9XB8RB�`B�fB�sB��B��B��B�B��B~�B=qBC�BjB��B��B�
B�BB�mB�mB�sB�HB�5B�B��B��B��B��B�B��B�
B�fBB1BB��B�B�mB�`B�ZB�B�B�B�B�yB�`B�NB�`B�TB��BȴB�^B��B��B�VB�=B�B{�Bq�BYB8RB�B
��B
��B
�FB
�1B
bNB
:^B
DB	��B	�yB	��B	��B	y�B	J�B	+B	�B	+B�B�/B��B��B��B��BŢB�jB�wB��B�qB�XB�FB�'B��B��B��B��B��B��B��B��B��B�jB��B�;B	\B	YB	~�B	�VB	��B	��B	��B	��B	�B	��B	��B	ĜB	�jB	��B	�B	��B	��B	��B	�LB	�/B	�`B	�mB	��B	��B
B
B
B
  B	��B	��B
	7B
VB
oB
hB
oB
oB
hB
bB
�B
�B
�B
(�B
%�B
�B
�B
�B
�B
�B
$�B
%�B
#�B
�B
�B
 �B
�B
�B
!�B
#�B
(�B
(�B
)�B
%�B
�B
"�B
'�B
)�B
(�B
&�B
$�B
#�B
#�B
#�B
"�B
!�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
oB
bB
\B
VB
JB
JB
DB
DB

=B
	7B
	7B

=B

=B
DB

=B

=B

=B

=B

=B

=B
	7B

=B
	7B
	7B
	7B
1B
+B
+B
	7B
1B
1B
+B
%B
%B
B
B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
+B
+B
+B
1B
	7B

=B

=B

=B

=B

=B

=B

=B

=B
DB

=B

=B

=B

=B
	7B
	7B
	7B

=B

=B
DB
DB
DB
JB
PB
PB
PB
PB
VB
VB
VB
VB
VB
VB
VB
\B
\B
\B
\B
\B
\B
bB
bB
bB
bB
bB
hB
hB
hB
oB
oB
oB
oB
uB
uB
uB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
&�B
.B
2-B
6FB
=qB
C�B
H�B
O�B
VB
]/B
aHB
ffB
jB
n�B
r�B
v�B
z�B
~�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�B	 �B	!�B	!�B	"�B	"�B	"�B	"�B	#�B	#�B	#�B	#�B	$�B	$�B	$�B	$�B	$�B	$�B	$�B	$�B	$�B	$�B	$�B	%�B	%�B	'�B	)�B	9XB8RB�mB�mB�yB��B��B��B�B�)B�hB=qBB�Bm�B��B�B�/B�ZB�sB�B�B�`B�HB�)B��B��B��B�
B�)B�B�/B�sBBDBB��B�B�sB�sB�sB�B�B�B�B�B�mB�TB�mB�B�B��BB�!B��B�bB�DB�1B~�Bx�B`BB=qB�BB
�B
��B
�hB
k�B
K�B
bB
B	�B	�B	�?B	�JB	YB	49B	�B	�B��B�fB��B�B�B��B��B��BÖBB�}B�wB�wB�qB�'B��B��B��B��B��B��B��B��B�dB��B�/B		7B	VB	}�B	�VB	��B	��B	B	�B	�
B	��B	�
B	ƨB	ȴB	��B	�B	��B	��B	��B	�3B	�/B	�sB	�mB	��B	��B
B
B
B
B	��B	��B

=B
hB
uB
oB
{B
uB
oB
bB
�B
�B
�B
-B
.B
�B
�B
�B
�B
�B
$�B
&�B
&�B
�B
�B
"�B
�B
 �B
"�B
#�B
)�B
(�B
,B
'�B
�B
"�B
(�B
+B
+B
)�B
%�B
$�B
$�B
%�B
%�B
#�B
!�B
 �B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
oB
hB
hB
PB
PB
JB
PB
JB

=B

=B
DB
JB
PB
DB
DB
DB
DB
DB
DB
DB
JB
DB

=B

=B

=B
	7B

=B

=B

=B

=B
	7B
1B
+B
%B
%B
B
B
B
B
B
B
B
  B
  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
B
B
B
%B
B
%B
%B
+B
+B
+B
1B
1B
1B
1B
1B
	7B
	7B
DB
DB
DB
DB
DB
DB
DB
DB
DB

=B
DB

=B
DB

=B

=B

=B
DB
DB
JB
JB
JB
JB
VB
VB
PB
VB
\B
VB
VB
VB
\B
\B
bB
bB
bB
\B
bB
bB
bB
hB
hB
hB
hB
hB
oB
oB
oB
uB
oB
uB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
'�B
.B
2-B
6FB
=qB
C�B
I�B
P�B
VB
]/B
bNB
gmB
jB
n�B
r�B
v�B
z�B
~�B
�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<�t�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<�C�<#�
<#�
<#�
<49X<u<�t�<e`B<#�
<#�
<e`B<T��<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =0.5 dbar.                                                                                                                                                                                                                                                   none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201131250282012011312502820120113125028  AO  ARGQ                                                                        20111205113346  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111205113346  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120113125028  IP                  G�O�G�O�G�O�                