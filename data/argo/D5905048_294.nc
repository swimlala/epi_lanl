CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-10-23T00:35:40Z creation;2018-10-23T00:35:45Z conversion to V3.1;2019-12-19T07:25:54Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        `  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \4   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ol   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  sD   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �|   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     `  ˬ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ۜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20181023003540  20200116231517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              &A   JA  I2_0577_294                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @؊��� 1   @؊��ր@4\��[W?�da9����1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB/��B8  B@  BH  BO��BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C�C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D|��D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ D�|�D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D��3D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�3D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@|(�@��@��A�
A;�
A[�
A{�
A��A��A��A��A��A��A��A��B��B��B��B��B'\)B.�\B6��B>��BF��BN�\BV��B^��Bf��Bn��Bv��B~��B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�B�z�C�qC�qC�qC�qC	�qC�qC�qC�qC�qC�qC�
C�qC�qC�qC�qC�qC!�qC#�qC%�qC'�qC)�qC+�qC-�qC/�qC1�qC3�qC5�qC7�qC9�qC;�qC=�qC?�qCA�qCC�qCE�qCG�qCI�qCK�qCM�qCO�qCQ�qCS�qCU�qCW�qCY�qC[�qC]�qC_�qCa�qCc�qCe�qCg�qCi�qCk�qCm�qCo�qCq�qCs�qCu�qCw�qCy�qC{�qC}�qC�qC�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸C�޸D o\D �\Do\D�\Du�D��Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\D	o\D	�\D
o\D
�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Du�D��Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\Do\D�\D o\D �\D!o\D!�\D"o\D"�\D#o\D#�\D$o\D$�\D%o\D%�\D&o\D&�\D'o\D'�\D(o\D(�\D)o\D)�\D*o\D*�\D+o\D+�\D,o\D,�\D-o\D-�\D.o\D.�\D/o\D/�\D0o\D0�\D1o\D1�\D2o\D2�\D3o\D3�\D4o\D4�\D5o\D5�\D6o\D6�\D7o\D7�\D8o\D8�\D9o\D9�\D:o\D:�\D;o\D;�\D<o\D<�\D=o\D=�\D>o\D>�\D?o\D?�\D@o\D@�\DAo\DA�\DBo\DB�\DCo\DC�\DDo\DD�\DEo\DE�\DFo\DF�\DGo\DG�\DHo\DH�\DIo\DI�\DJo\DJ�\DKo\DK�\DLo\DL�\DMo\DM�\DNo\DN�\DOo\DO�\DPo\DP�\DQo\DQ�\DRo\DR�\DSo\DS�\DTo\DT�\DUo\DU�\DVo\DV�\DWo\DW�\DXo\DX�\DYo\DY�\DZo\DZ�\D[o\D[�\D\o\D\�\D]o\D]�\D^o\D^�\D_o\D_�\D`o\D`�\Dao\Da�\Dbo\Db�\Dco\Dc�\Ddo\Dd�\Deo\De�\Dfo\Df�\Dgo\Dg�\Dho\Dh�\Dio\Di�\Djo\Dj�\Dko\Dk�\Dlo\Dl�\Dmo\Dm�\Dno\Dn�\Doo\Do�\Dpo\Dp�\Dqo\Dq�\Dro\Dr�\Dso\Ds�\Dto\Dt�\Duo\Du�\Dvo\Dv�\Dwo\Dw�\Dxo\Dx�\Dyo\Dy�\Dzo\Dz�\D{o\D{�\D|o\D|��D}o\D}�\D~o\D~�\Do\D�\D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�z�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�D�w�D·�D���D�7�D�w�D÷�D���D�7�D�w�Dķ�D���D�7�D�t{Dŷ�D���D�7�D�w�DƷ�D���D�7�D�w�DǷ�D���D�7�D�w�Dȷ�D���D�7�D�w�Dɷ�D���D�7�D�w�Dʷ�D���D�7�D�w�D˷�D���D�7�D�w�D̺�D���D�7�D�w�Dͷ�D���D�7�D�w�Dη�D���D�7�D�w�DϷ�D���D�7�D�w�Dз�D���D�7�D�w�Dѷ�D���D�7�D�w�Dҷ�D���D�7�D�w�Dӷ�D���D�7�D�w�DԷ�D���D�7�D�w�Dշ�D���D�7�D�w�Dַ�D���D�7�D�w�D׷�D���D�7�D�w�Dط�D���D�7�D�w�Dٷ�D���D�7�D�w�Dڷ�D���D�7�D�w�D۷�D���D�7�D�w�Dܷ�D���D�7�D�w�Dݷ�D���D�7�D�w�D޷�D���D�7�D�w�D߷�D���D�7�D�w�D෮D���D�7�D�w�DᷮD���D�7�D�w�DⷮD���D�7�D�w�D㷮D���D�7�D�w�D䷮D���D�7�D�w�D差D���D�7�D�w�D淮D���D�7�D�w�D緮D���D�7�D�w�D跮D���D�7�D�w�D鷮D���D�7�D�w�D귮D���D�7�D�w�D뷮D���D�7�D�w�D췮D���D�7�D�w�D���D���D�7�D�w�DD���D�7�D�w�D﷮D���D�7�D�w�D�D���D�7�D�w�D�D���D�7�D�w�D�D���D�7�D�w�D�D���D�7�D�w�D���D���D�7�D�w�D���D���D�7�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��yA��mA��A��A��`A��;A��/A��;A��;A��;A��;A��;A��HA��;A��
A���A�ĜA�=qA�S�AиRA�v�Aȉ7A�  Aĺ^A£�A�A���A��!A�{A��A�7LA��A�ƨA��A�;dA�hsA��/A���A�x�A���A��A�$�A��RA��FA�
=A�~�A��A�O�A�z�A��jA�&�A�VA�K�A�A��9A�&�A�dZA� �A��9A��DA�K�A�t�A��A���A��A���A��/A�1'A���A��PA���A��wA�C�A�bA�XA��!A�~�A���A��A�E�A���A��A�(�A�%A�33A�VA�-A��!A}��A{��AzJAx��Au`BAr �An�RAkhsAiS�Ag�^Ae\)Ad{Ab��Aa��A`M�A^$�A\�AX�AU�AS/APA�AO`BAN�!AMVAJ�\AH��AFM�AE/AD�RAC��AA|�A?��A=�FA<JA:VA8$�A7A7x�A7A65?A4��A2�A/`BA.�A-33A,I�A+��A+l�A*��A*ȴA*�jA*��A*ffA*$�A)�A(��A(1A'?}A%�A$v�A#/A!K�A��AK�AC�A�A��A�\A  A  A;dA1'A�#A�A�!AA�HA��A�A�yAVA�jAA��An�A�^Ap�A
�`A
{A��A�;A�AC�A�A�
A��AffAXA 9X@�{@��9@�(�@��9@��@���@���@��`@� �@�ff@��D@�(�@�@�Q�@��@�V@�r�@ߍP@�"�@�C�@�S�@�\)@�dZ@�"�@�-@ܣ�@�  @٩�@�?}@�&�@��@��@��@�%@؃@�|�@�"�@���@�^5@��@�j@��;@ӕ�@�|�@�\)@��y@��@�K�@��@�n�@�ff@җ�@ҟ�@��@Ь@�bN@��y@̴9@�t�@�33@�+@���@ʧ�@��
@�9X@��m@ˮ@�33@ʗ�@���@�@�7L@Ǖ�@��;@��@��T@�V@ă@�9X@Õ�@�v�@���@�hs@���@���@���@�z�@�\)@�"�@��R@��#@�Ĝ@���@�V@��h@�(�@��m@��F@���@��!@�M�@��@�=q@��@��`@�(�@�l�@���@���@��@��P@� �@�A�@���@��w@�t�@��\@�M�@��-@��j@��;@���@�&�@��@�j@�  @��@��@��+@�=q@���@��@��u@�9X@��m@���@�t�@�K�@�@��\@�M�@��#@��@�G�@�%@��u@�I�@�(�@�1@�ƨ@�dZ@�
=@��y@�ȴ@�E�@���@��@��@�z�@�j@�I�@�1@���@��m@���@�l�@�C�@�o@�@���@�ff@��@���@��@�@��h@��@��/@���@��D@�A�@� �@��@�b@���@��;@�|�@�S�@�33@��@��y@��R@�$�@�p�@�G�@�V@�Ĝ@�Ĝ@��j@��9@��D@�z�@�Z@���@��@��@�dZ@�+@��@��!@�=q@��@���@�p�@��@��@��@��;@���@�l�@�C�@�"�@�
=@�ȴ@���@��+@�ff@�5?@�{@��^@�x�@�V@��@�1'@��w@�S�@�33@�o@��H@��@���@�v�@���@�G�@�&�@��/@�r�@��@�b@�  @��w@�t�@�l�@�\)@�C�@��@�@��y@���@��+@�-@���@���@��-@��@�V@��`@�Ĝ@���@��D@�A�@���@�|�@�33@���@�v�@�E�@�@���@��^@��h@�hs@�O�@�/@���@���@���@��D@��u@� �@���@�l�@�S�@�;d@��@�@��H@�ȴ@���@�v�@�E�@��@��^@���@��@�x�@�hs@�`B@�G�@�V@���@���@��j@���@�Z@�(�@�b@�@�@+@~�+@}��@}O�@|�@|�j@|�D@|Z@|(�@{��@{ƨ@{�@z�!@z~�@zn�@zM�@y��@x��@xQ�@w�P@w\)@v�y@v�+@vE�@v$�@u�h@u/@t�@sƨ@sdZ@r�!@q��@q�@q�#@q��@q��@q7L@p��@p�@o��@oK�@o�@n��@nȴ@n��@n��@nV@m��@m`B@l�j@k�F@kS�@j�H@j^5@j�@i��@i�7@i�@h1'@h  @g�@gK�@g|�@g�@f��@f�+@fV@f@e�@e��@d�@dj@dj@dZ@c��@cS�@b��@b~�@b^5@b�@aX@a&�@`�`@`Ĝ@`Q�@`  @_��@_l�@^ȴ@^�+@^$�@]�@]��@]?}@\��@\(�@[�m@[�
@[�F@[33@Z�!@Z�\@Z�\@Z~�@Z�@X��@X��@X�@XA�@X  @W�w@W�w@W�P@W�@Vv�@U�T@U�h@Up�@U/@T�j@T9X@T1@S�F@S"�@R��@R�\@RM�@RJ@Q�^@Qhs@P��@P�@Pb@O�w@Ol�@N�y@NE�@M��@M�h@Mp�@M`B@MV@L�j@L�@L�@LZ@K��@K��@Kt�@K"�@J��@J�!@J��@J��@J�\@J�\@J~�@J^5@I��@H��@H��@HbN@G�@G+@G�@G
=@F��@F�y@F�@Fff@F@F{@E�@E�-@E�@E?}@E/@D�j@D��@D�D@D9X@C��@CS�@C33@C@B�@B��@B�\@B^5@BM�@B-@A�#@A��@Ax�@A&�@@r�@@  @?\)@>�y@>�R@>V@=�@=��@=�-@=?}@<�@<�D@<(�@<1@;�
@;ƨ@;��@;dZ@;"�@:�!@:~�@9��@9G�@9�@8�`@8��@8�@8bN@8A�@81'@8 �@8b@8b@8b@7�@7��@7l�@7�@6�y@6��@6E�@65?@6$�@6{@5�@5��@5@5�-@5�h@5p�@5O�@5/@5V@4��@4�/@4��@4�j@4��@4j@49X@3ƨ@3C�@3S�@3dZ@3dZ@333@3o@2�H@2��@2��@2��@2��@2n�@2J@1�@1�@1�^@1hs@1G�@1G�@1%@0�@0�@0�@0bN@0 �@/�;@/K�@.�y@.�R@.v�@.$�@.{@-�@-�T@-@-�h@-�@-�@-O�@-V@,�j@,Z@,I�@,�@+�F@+�@+S�@+C�@+33@+33@+"�@+o@*��@*�!@*�\@*M�@*J@)��@)��@)x�@)G�@)�@(��@(��@(�@(r�@( �@'��@';d@&��@&V@&@%��@%p�@%O�@%?}@%/@%/@%�@%V@$��@$�D@$I�@$1@#�
@#ƨ@#��@#S�@#C�@#"�@#o@"�@"��@"�\@"~�@"=q@"J@!��@"J@!��@!��@!�7@!7L@ ��@ �`@ �9@ �u@ 1'@��@l�@+@�@��@ff@V@5?@��@p�@V@�/@�@Z@�@1@�m@dZ@o@@��@�\@^5@J@��@x�@�@%@��@�9@�@ �@�@�w@\)@+@
=@�R@��@v�@$�@��@�-@�h@�@`B@�@��@�@j@Z@I�@(�@1@��@�m@�m@�
@ƨ@��@�@dZ@S�@C�@@��@�\@n�@^5@J@��@�@�#@��@�7@G�@%@��@bN@bN@Q�@Q�@Q�@A�@1'@  @�w@l�@;d@�y@�+@E�@�T@�-@��@�h@`B@O�@/@V@�@��@�@��@j@9X@�@1@��@�m@ƨ@�@t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��yA��mA��A��A��`A��;A��/A��;A��;A��;A��;A��;A��HA��;A��
A���A�ĜA�=qA�S�AиRA�v�Aȉ7A�  Aĺ^A£�A�A���A��!A�{A��A�7LA��A�ƨA��A�;dA�hsA��/A���A�x�A���A��A�$�A��RA��FA�
=A�~�A��A�O�A�z�A��jA�&�A�VA�K�A�A��9A�&�A�dZA� �A��9A��DA�K�A�t�A��A���A��A���A��/A�1'A���A��PA���A��wA�C�A�bA�XA��!A�~�A���A��A�E�A���A��A�(�A�%A�33A�VA�-A��!A}��A{��AzJAx��Au`BAr �An�RAkhsAiS�Ag�^Ae\)Ad{Ab��Aa��A`M�A^$�A\�AX�AU�AS/APA�AO`BAN�!AMVAJ�\AH��AFM�AE/AD�RAC��AA|�A?��A=�FA<JA:VA8$�A7A7x�A7A65?A4��A2�A/`BA.�A-33A,I�A+��A+l�A*��A*ȴA*�jA*��A*ffA*$�A)�A(��A(1A'?}A%�A$v�A#/A!K�A��AK�AC�A�A��A�\A  A  A;dA1'A�#A�A�!AA�HA��A�A�yAVA�jAA��An�A�^Ap�A
�`A
{A��A�;A�AC�A�A�
A��AffAXA 9X@�{@��9@�(�@��9@��@���@���@��`@� �@�ff@��D@�(�@�@�Q�@��@�V@�r�@ߍP@�"�@�C�@�S�@�\)@�dZ@�"�@�-@ܣ�@�  @٩�@�?}@�&�@��@��@��@�%@؃@�|�@�"�@���@�^5@��@�j@��;@ӕ�@�|�@�\)@��y@��@�K�@��@�n�@�ff@җ�@ҟ�@��@Ь@�bN@��y@̴9@�t�@�33@�+@���@ʧ�@��
@�9X@��m@ˮ@�33@ʗ�@���@�@�7L@Ǖ�@��;@��@��T@�V@ă@�9X@Õ�@�v�@���@�hs@���@���@���@�z�@�\)@�"�@��R@��#@�Ĝ@���@�V@��h@�(�@��m@��F@���@��!@�M�@��@�=q@��@��`@�(�@�l�@���@���@��@��P@� �@�A�@���@��w@�t�@��\@�M�@��-@��j@��;@���@�&�@��@�j@�  @��@��@��+@�=q@���@��@��u@�9X@��m@���@�t�@�K�@�@��\@�M�@��#@��@�G�@�%@��u@�I�@�(�@�1@�ƨ@�dZ@�
=@��y@�ȴ@�E�@���@��@��@�z�@�j@�I�@�1@���@��m@���@�l�@�C�@�o@�@���@�ff@��@���@��@�@��h@��@��/@���@��D@�A�@� �@��@�b@���@��;@�|�@�S�@�33@��@��y@��R@�$�@�p�@�G�@�V@�Ĝ@�Ĝ@��j@��9@��D@�z�@�Z@���@��@��@�dZ@�+@��@��!@�=q@��@���@�p�@��@��@��@��;@���@�l�@�C�@�"�@�
=@�ȴ@���@��+@�ff@�5?@�{@��^@�x�@�V@��@�1'@��w@�S�@�33@�o@��H@��@���@�v�@���@�G�@�&�@��/@�r�@��@�b@�  @��w@�t�@�l�@�\)@�C�@��@�@��y@���@��+@�-@���@���@��-@��@�V@��`@�Ĝ@���@��D@�A�@���@�|�@�33@���@�v�@�E�@�@���@��^@��h@�hs@�O�@�/@���@���@���@��D@��u@� �@���@�l�@�S�@�;d@��@�@��H@�ȴ@���@�v�@�E�@��@��^@���@��@�x�@�hs@�`B@�G�@�V@���@���@��j@���@�Z@�(�@�b@�@�@+@~�+@}��@}O�@|�@|�j@|�D@|Z@|(�@{��@{ƨ@{�@z�!@z~�@zn�@zM�@y��@x��@xQ�@w�P@w\)@v�y@v�+@vE�@v$�@u�h@u/@t�@sƨ@sdZ@r�!@q��@q�@q�#@q��@q��@q7L@p��@p�@o��@oK�@o�@n��@nȴ@n��@n��@nV@m��@m`B@l�j@k�F@kS�@j�H@j^5@j�@i��@i�7@i�@h1'@h  @g�@gK�@g|�@g�@f��@f�+@fV@f@e�@e��@d�@dj@dj@dZ@c��@cS�@b��@b~�@b^5@b�@aX@a&�@`�`@`Ĝ@`Q�@`  @_��@_l�@^ȴ@^�+@^$�@]�@]��@]?}@\��@\(�@[�m@[�
@[�F@[33@Z�!@Z�\@Z�\@Z~�@Z�@X��@X��@X�@XA�@X  @W�w@W�w@W�P@W�@Vv�@U�T@U�h@Up�@U/@T�j@T9X@T1@S�F@S"�@R��@R�\@RM�@RJ@Q�^@Qhs@P��@P�@Pb@O�w@Ol�@N�y@NE�@M��@M�h@Mp�@M`B@MV@L�j@L�@L�@LZ@K��@K��@Kt�@K"�@J��@J�!@J��@J��@J�\@J�\@J~�@J^5@I��@H��@H��@HbN@G�@G+@G�@G
=@F��@F�y@F�@Fff@F@F{@E�@E�-@E�@E?}@E/@D�j@D��@D�D@D9X@C��@CS�@C33@C@B�@B��@B�\@B^5@BM�@B-@A�#@A��@Ax�@A&�@@r�@@  @?\)@>�y@>�R@>V@=�@=��@=�-@=?}@<�@<�D@<(�@<1@;�
@;ƨ@;��@;dZ@;"�@:�!@:~�@9��@9G�@9�@8�`@8��@8�@8bN@8A�@81'@8 �@8b@8b@8b@7�@7��@7l�@7�@6�y@6��@6E�@65?@6$�@6{@5�@5��@5@5�-@5�h@5p�@5O�@5/@5V@4��@4�/@4��@4�j@4��@4j@49X@3ƨ@3C�@3S�@3dZ@3dZ@333@3o@2�H@2��@2��@2��@2��@2n�@2J@1�@1�@1�^@1hs@1G�@1G�@1%@0�@0�@0�@0bN@0 �@/�;@/K�@.�y@.�R@.v�@.$�@.{@-�@-�T@-@-�h@-�@-�@-O�@-V@,�j@,Z@,I�@,�@+�F@+�@+S�@+C�@+33@+33@+"�@+o@*��@*�!@*�\@*M�@*J@)��@)��@)x�@)G�@)�@(��@(��@(�@(r�@( �@'��@';d@&��@&V@&@%��@%p�@%O�@%?}@%/@%/@%�@%V@$��@$�D@$I�@$1@#�
@#ƨ@#��@#S�@#C�@#"�@#o@"�@"��@"�\@"~�@"=q@"J@!��@"J@!��@!��@!�7@!7L@ ��@ �`@ �9@ �u@ 1'@��@l�@+@�@��@ff@V@5?@��@p�@V@�/@�@Z@�@1@�m@dZ@o@@��@�\@^5@J@��@x�@�@%@��@�9@�@ �@�@�w@\)@+@
=@�R@��@v�@$�@��@�-@�h@�@`B@�@��@�@j@Z@I�@(�@1@��@�m@�m@�
@ƨ@��@�@dZ@S�@C�@@��@�\@n�@^5@J@��@�@�#@��@�7@G�@%@��@bN@bN@Q�@Q�@Q�@A�@1'@  @�w@l�@;d@�y@�+@E�@�T@�-@��@�h@`B@O�@/@V@�@��@�@��@j@9X@�@1@��@�m@ƨ@�@t�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BuB{B�B�B�B�B�B�B�B�B�B�B�BuBoBhBPBB
�mB
�B2-B��B�qB��B�qB��B�B��B  BBBB-B.B"�B8RB49BJ�B>wB�BoBbB��B�B$�B+B�BhB�B�B�B%B%BJB1B��B�yB�
B�)BĜB�-B�9B��B��B}�B��B�1BXB/B2-B?}B�B�BPBBPBhB
��B
�dB
��B
ŢB
��B
��B
�1B
�B
jB
I�B
D�B
33B
.B
 �B
�B	�B	��B	��B	��B	��B	��B	�B	�B	}�B	p�B	bNB	I�B	9XB	%�B	1B	%B�B		7B	B�B�
B�B��B�
B�BÖB�'B��B��B��B��B��B�LB�FB�B��B��B�+Br�B�7B�hB�{B�uB��B��B��B��B��B��B��B�PB�7B�B�Bp�Bo�BiyBdZBhsB{�B�B}�Bw�Bq�BcTBN�B;dB]/Bl�Be`BhsBbNBXBL�BcTB[#BR�Br�Bk�Bl�Be`BjBs�Bm�BhsBbNBl�Bu�Bt�Bk�B`BBM�BS�BVBS�BO�B]/Bn�Bv�Bq�BjBcTBcTBk�B]/BQ�B@�BB�BI�B9XB1'B:^BC�BL�BT�BYBZBZBVBO�BJ�BW
BN�B`BBgmBhsBhsBgmBffBe`BffBs�Bu�Bt�Bm�Bv�Bz�B�B�JB�JB�JB�{B��B��B��B��B��B��B��B��B��B��B�hB��B��B�B�B�9BĜB��BŢBŢBÖBĜBÖB��BƨBŢB��B��B��B�
B�5B�NB�HB�NB�sB�B�B��B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B	  B��B��B��B	B	VB	
=B	PB	uB	�B	�B	�B	$�B	$�B	�B	$�B	%�B	 �B	�B	{B	�B	(�B	$�B	%�B	&�B	%�B	)�B	+B	+B	+B	49B	:^B	>wB	A�B	E�B	F�B	F�B	I�B	P�B	O�B	T�B	YB	ZB	\)B	bNB	jB	o�B	p�B	q�B	r�B	v�B	v�B	v�B	w�B	{�B	~�B	�B	�=B	�=B	�=B	�JB	�DB	�=B	�JB	�PB	�VB	�bB	�\B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�?B	�FB	�LB	�jB	�dB	�dB	�^B	�dB	�jB	�dB	�wB	��B	��B	��B	��B	B	B	ŢB	ƨB	ȴB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�
B	�
B	�B	�B	�B	�B	�B	�B	�#B	�;B	�;B	�;B	�BB	�;B	�)B	�B	�;B	�ZB	�TB	�NB	�ZB	�sB	�sB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
%B
B
%B
+B
1B
1B
+B
%B
+B

=B
DB
PB
VB
\B
bB
bB
bB
bB
\B
oB
uB
oB
bB
bB
{B
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
�B
�B
�B
�B
�B
�B
�B
 �B
 �B
�B
!�B
 �B
�B
"�B
#�B
"�B
'�B
&�B
'�B
)�B
+B
)�B
+B
)�B
(�B
)�B
-B
,B
)�B
)�B
)�B
.B
.B
.B
+B
/B
/B
/B
.B
/B
/B
0!B
.B
0!B
0!B
1'B
0!B
0!B
/B
0!B
1'B
2-B
2-B
0!B
0!B
33B
49B
33B
1'B
.B
33B
49B
49B
49B
5?B
6FB
49B
33B
2-B
33B
5?B
6FB
5?B
49B
5?B
6FB
6FB
5?B
7LB
8RB
8RB
8RB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
8RB
:^B
;dB
<jB
<jB
<jB
;dB
=qB
>wB
<jB
=qB
<jB
>wB
=qB
=qB
?}B
?}B
?}B
?}B
?}B
?}B
=qB
<jB
9XB
>wB
>wB
>wB
@�B
B�B
B�B
B�B
B�B
A�B
@�B
A�B
C�B
C�B
B�B
C�B
C�B
D�B
B�B
D�B
D�B
D�B
B�B
D�B
E�B
D�B
E�B
D�B
D�B
E�B
E�B
E�B
D�B
D�B
C�B
B�B
A�B
B�B
C�B
D�B
E�B
D�B
E�B
F�B
F�B
D�B
F�B
F�B
F�B
H�B
H�B
H�B
H�B
G�B
G�B
G�B
H�B
G�B
F�B
I�B
I�B
I�B
J�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
K�B
K�B
J�B
K�B
L�B
L�B
L�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
P�B
T�B
T�B
T�B
S�B
S�B
T�B
T�B
VB
VB
T�B
T�B
T�B
VB
W
B
VB
VB
VB
W
B
VB
VB
XB
XB
W
B
VB
VB
T�B
W
B
XB
XB
XB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
YB
YB
YB
YB
[#B
[#B
ZB
[#B
\)B
]/B
]/B
]/B
]/B
\)B
\)B
]/B
]/B
\)B
\)B
]/B
\)B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
]/B
\)B
]/B
^5B
_;B
`BB
_;B
aHB
bNB
bNB
cTB
cTB
bNB
bNB
aHB
aHB
aHB
bNB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
dZB
e`B
ffB
ffB
ffB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
e`B
e`B
e`B
ffB
ffB
gmB
gmB
hsB
gmB
ffB
ffB
gmB
hsB
iyB
hsB
iyB
jB
iyB
hsB
iyB
k�B
jB
jB
k�B
jB
jB
k�B
k�B
m�B
m�B
l�B
l�B
l�B
m�B
m�B
l�B
n�B
n�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
o�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
q�B
r�B
r�B
r�B
q�B
s�B
s�B
s�B
r�B
r�B
r�B
r�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
t�B
t�B
t�B
u�B
t�B
t�B
u�B
u�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
x�B
z�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B
�B
�]B<�B�zB��B�IB��B�sB�GB��B-BB3B�B.�B0oB&fB:^B7�BK�BA�B�BB�B��B?B&�B
�BkB�B�BkB+B�B�BB	B�xB�B��B�~B�zB�%B��B�*B��B�[B�2B��B[�B2�B4�BA B"�B;BB�B�BoB
�VB
� B
��B
ǔB
��B
�!B
��B
�mB
m�B
NpB
HfB
7B
0�B
# B
�B	��B	��B	�B	��B	�VB	��B	�B	��B	�B	r|B	d�B	L�B	<�B	)�B	B		lB�UB	
XB	�B�B�kBؓBЗB�yB�
BŢB�B�IB�BB�B�B�1B��B��B�5B�tB��B�	BwB��B��B��B��B�/B�QB�B�B�B�B�9B�pB�rB�tB�[Br�Bq�Bk�Bf�BjeB|jB�[B~wBx�Br|Bd�BQ�B?B^�BmCBf�BiDBc�BZBO�BdZB]/BUMBsMBl�BmwBgBk�BtTBn�Bi�BdZBm�Bv`BuZBl�BbBP�BU�BW�BU�BQ�B^5Bo Bv�Br�BlBezBd�BlqB^�BS�BC{BDMBKxB;B3�B;�BDMBMPBUBYKBZQBZkBV�BP�BLBW�BPbB`�Bg�Bh�Bh�Bg�Bf�Be�Bg8BtBv+BuZBn�BwLB{dB�mB��B��B��B��B��B�B�!B�B��B�FB�|B��B�XB��B�B��B�DB�]B��B�TB�BʦB�B�%B�3B�SB�gB��B�zB��B�2B��B��B��B��B��B�B�:B�B��B�CB��B��B�B�B�B�TB�|B�B��B��B�vB�B�B�<B�VB��B�JB�BB	 4B��B�}B��B	�B	"B	
�B	�B	�B	~B	�B	/B	%B	%FB	 vB	%`B	&�B	!�B	 �B	�B	VB	)*B	%zB	&fB	'�B	&�B	*eB	+�B	+�B	+�B	4�B	:�B	>�B	A�B	E�B	F�B	GB	J#B	Q4B	PbB	UgB	YeB	Z�B	\�B	b�B	j�B	o�B	qB	r-B	sB	wB	w2B	wfB	x�B	|�B	cB	�SB	�rB	��B	��B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�=B	�B	�B	�-B	�:B	�B	�B	�*B	�$B	�$B	�RB	�QB	�WB	�CB	�qB	�qB	��B	��B	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�1B	�)B	�<B	�HB	�NB	�TB	�,B	�,B	�,B	�[B	�2B	�YB	�?B	�YB	�YB	ևB	�yB	ևB	�yB	ؓB	�B	ۦB	�pB	�pB	�pB	�vB	�pB	ܬB	ںB	߾B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	�B	�B	�B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�!B	�'B	�B	�'B	�B	�%B	�+B	�B	�B	�B	�$B	�B	�B	�$B	�B	�B	�B	�.B	�B	�xB	�"B	�(B	�(B	�(B	�(B	�(B	�HB	�HB	�VB	�HB	�cB
 OB
[B
AB
-B
[B
[B
AB
[B
GB
GB
MB
YB
mB
YB
zB
fB
fB
zB
�B
�B

�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
B
�B
�B
B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
!B
 B
B
B
5B
5B
B
 B
!B
 �B
 B
!�B
!B
!B
# B
$B
# B
($B
'8B
(>B
*KB
+6B
*KB
+6B
*eB
)yB
*KB
-CB
,WB
*KB
*KB
*KB
.IB
.IB
.IB
+kB
/OB
/OB
/iB
.cB
/OB
/�B
0UB
.�B
0oB
0UB
1[B
0�B
0oB
/iB
0oB
1[B
2aB
2aB
0oB
0oB
3hB
4nB
3�B
1�B
.�B
3hB
4�B
4�B
4�B
5tB
6`B
4�B
3�B
2�B
3�B
5tB
6zB
5�B
4�B
5�B
6zB
6�B
5�B
7�B
8�B
8�B
8�B
7�B
8�B
8�B
8�B
8�B
9�B
9�B
9�B
8�B
:�B
;�B
<�B
<�B
<�B
;�B
=�B
>�B
<�B
=�B
<�B
>�B
=�B
=�B
?�B
?�B
?�B
?�B
?�B
?�B
=�B
<�B
9�B
>�B
>�B
>�B
@�B
B�B
B�B
B�B
B�B
A�B
@�B
A�B
C�B
C�B
B�B
C�B
C�B
D�B
B�B
D�B
D�B
EB
B�B
D�B
E�B
D�B
E�B
D�B
D�B
E�B
E�B
E�B
D�B
D�B
C�B
B�B
A�B
B�B
C�B
D�B
E�B
D�B
E�B
F�B
F�B
D�B
F�B
F�B
F�B
H�B
H�B
H�B
IB
G�B
G�B
HB
IB
HB
GB
I�B
I�B
I�B
KB
K�B
LB
K�B
L�B
L�B
L�B
MB
LB
LB
KB
K�B
MB
MB
MB
N�B
N�B
N�B
OB
O�B
PB
O�B
PB
P.B
PB
PB
QB
QB
RB
R B
R B
R:B
R B
R B
R:B
Q4B
UB
UB
U2B
TFB
TFB
UMB
UMB
V9B
VB
U2B
UMB
UMB
VSB
W?B
VSB
VSB
V9B
W$B
V9B
VSB
X+B
X+B
W?B
V9B
V9B
UgB
W?B
XEB
XEB
XEB
Z7B
ZkB
Z7B
ZQB
ZkB
ZQB
Z7B
YKB
YKB
YKB
YKB
[=B
[qB
ZkB
[WB
\xB
]dB
]IB
]dB
]IB
\]B
\xB
]dB
]dB
\xB
\xB
]IB
\]B
]dB
]dB
]dB
]dB
^jB
^jB
^jB
]~B
\�B
]~B
^�B
_pB
`�B
_pB
a|B
bhB
b�B
c�B
cnB
b�B
b�B
a�B
a|B
a|B
b�B
c�B
c�B
c�B
c�B
dtB
d�B
d�B
d�B
d�B
ezB
e�B
d�B
e�B
f�B
f�B
f�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
e�B
e�B
e�B
f�B
f�B
g�B
g�B
h�B
g�B
f�B
f�B
g�B
h�B
i�B
h�B
i�B
j�B
i�B
h�B
i�B
k�B
j�B
j�B
k�B
j�B
j�B
k�B
k�B
m�B
m�B
l�B
l�B
l�B
m�B
m�B
l�B
n�B
n�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
p�B
o�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
p�B
q�B
r�B
r�B
r�B
q�B
s�B
s�B
s�B
r�B
r�B
r�B
r�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
t�B
t�B
uB
u�B
t�B
uB
u�B
vB
wB
xB
w�B
xB
w�B
xB
xB
xB
y	B
y	B
y	B
y	B
y	B
y	B
y�B
y�B
zB
zB
y$B
z�B
z�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<I��<(�^<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.26(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201810270039142018102700391420181027003914201810270200242018102702002420181027020024201810280031242018102800312420181028003124  JA  ARFMdecpA19c                                                                20181023093516  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20181023003540  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20181023003543  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20181023003543  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20181023003544  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20181023003544  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20181023003544  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20181023003544  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20181023003545  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20181023003545                      G�O�G�O�G�O�                JA  ARUP                                                                        20181023005749                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20181023153539  CV  JULD            G�O�G�O�F�W�                JM  ARCAJMQC2.0                                                                 20181026153914  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20181026153914  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20181026170024  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20181027153124  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231517                      G�O�G�O�G�O�                