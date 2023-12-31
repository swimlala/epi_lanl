CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-09-06T06:35:14Z creation;2018-09-06T06:35:17Z conversion to V3.1;2019-12-23T06:15:36Z update;     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20180906063514  20200120021520  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               WA   JA  I2_0675_087                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�G��� 1   @�H����@7Ϫ͞���c;*�0�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @,��@�  @�  A��A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1y�D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՃ3D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @B�\@��H@��HA
=A%p�AEp�Aep�A��RA��A��RA��RA¸RAҸRA�RA�RB\)B	\)B\)B\)B!\)B)\)B1\)B9\)BA\)BI\)BQ\)BY\)Ba\)Bi\)Bq\)By\)B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BĮBȮB̮BЮBԮBخBܮB�B�B�B�B�B��B��B��C W
CW
CW
CW
CW
C
W
CW
CW
CW
CW
CW
CW
CW
CW
CW
CW
C W
C"W
C$W
C&W
C(W
C*W
C,W
C.W
C0W
C2W
C4W
C6W
C8W
C:W
C<W
C>W
C@W
CBW
CDW
CFW
CHW
CJW
CLW
CNW
CPW
CRW
CTW
CVW
CXW
CZW
C\W
C^W
C`W
CbW
CdW
CfW
ChW
CjW
ClW
CnW
CpW
CrW
CtW
CvW
CxW
CzW
C|W
C~W
C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�8RC�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�C�+�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1�\D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D��D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D�D���D�
�D�J�DÊ�D���D�
�D�J�DĊ�D���D�
�D�J�DŊ�D���D�
�D�J�DƊ�D���D�
�D�J�DǊ�D���D�
�D�J�DȊ�D���D�
�D�J�DɊ�D���D�
�D�J�Dʊ�D���D�
�D�J�Dˊ�D���D�
�D�J�D̊�D���D�
�D�J�D͊�D���D�
�D�J�DΊ�D���D�
�D�J�Dϊ�D���D�
�D�J�DЊ�D���D�
�D�J�Dъ�D���D�
�D�J�DҊ�D���D�
�D�J�Dӊ�D���D�
�D�J�DԊ�D���D�
�D�J�DՎD���D�
�D�J�D֊�D���D�
�D�J�D׊�D���D�
�D�J�D؊�D���D�
�D�J�Dي�D���D�
�D�J�Dڊ�D���D�
�D�J�Dۊ�D���D�
�D�J�D܊�D���D�
�D�J�D݊�D���D�
�D�J�Dފ�D���D�
�D�J�Dߊ�D���D�
�D�J�D���D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D���D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D��D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D���D�
�D�J�D���D�׮111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A��A�(�A�"�A�(�A�(�A�%A��
A�G�A���A΃A��HA͑hA̓A�n�A�VA��A��A�  A�~�A�A�E�A�VA��A���AǴ9A�\)A�1'A��A��A���A�E�A�XA�C�A��A��TA��A��A��^A�A�A�oA���A��A��!A�-A���A�bNA�33A���A�?}A��
A���A�VA��A���A�x�A�9XA�
=A��A���A���A�K�A�{A��A�-A���A�E�A�;dA��+A�v�A�hsA�A��\A�"�A��;A�t�A�E�A���A���A���A�x�A�"�A�O�A���A�G�A��A�I�A�jA��-A��\A�l�A�"�A���A��A��!A�r�A��A��A��
A�x�A�ZA�ĜA�ZA���A�Q�A�
=A���A��/A���A��A�  A�v�A���A�v�A���A��A�|�A�1'A�1A���A�?}A�JA��A��
A�A�VA/A~��A}hsAy�Aw�Au��As�Ap  An�+AlVAj��AjjAh1Af�HAfn�Ae�;Ad��Ac��Ac
=Aa�A_A]��A]XAZ�AW�TAU�ATĜAR��AP�/AO+AM�hAL�AK��AJ{AGADVAC��AB�AA�AA&�A>5?A=K�A=G�A=`BA<�uA:��A8v�A6bNA4�DA2��A/�A.bA,z�A*�/A*ffA*bA)�PA)&�A'A&jA%�mA%�A$VA#t�A"n�A"A �yA M�A�A�7A��A�
Ap�A�yA  A(�A
=A�A�7A��AG�A(�AƨA�/A{A��A�-A�7A%A
^5A	�PA	
=A�-A\)AA��A��A��A�hA�;AoA 1@�$�@�r�@���@���@�hs@���@��@�ff@��@�I�@��@�O�@�&�@�l�@�^5@�1'@���@�M�@�p�@�Ĝ@�9X@�@�@�I�@�o@�x�@ۅ@��y@�-@��T@ف@ج@�;d@�=q@�?}@�S�@ѡ�@��`@��@�l�@�ȴ@�-@�O�@���@̃@��@�t�@��H@�5?@ɲ-@�hs@�%@ȣ�@�Q�@ǝ�@ƸR@�-@�@Ĭ@� �@��;@�S�@�V@��@���@���@��@�I�@�
=@���@���@�C�@��\@�-@���@���@��@���@�o@��R@�@���@�A�@��@��F@���@��@��@�Ĝ@�1'@��@���@�=q@���@���@��@��`@��w@�C�@�
=@��@��R@�ff@�E�@��@��7@��@�ƨ@���@�J@��-@��@�x�@��@�V@��`@�Ĝ@��D@��D@�bN@�(�@� �@�  @�ƨ@���@�dZ@�33@���@���@�~�@���@�/@��@��D@�ƨ@�S�@��@�n�@�-@�$�@�5?@�ff@�n�@�n�@�@���@�%@���@���@� �@���@�C�@���@�ȴ@��\@�V@�=q@�E�@��@���@��-@�X@���@���@�/@��/@�%@�X@�&�@���@���@���@���@��`@�X@�@�M�@�E�@�E�@�p�@�?}@�hs@���@�G�@�p�@��/@�Ĝ@�V@��j@�I�@� �@�(�@�9X@�1'@� �@��;@�dZ@�C�@��y@��y@��!@�5?@�@�J@�$�@�E�@�@�/@���@��@��9@���@�Z@�ƨ@��@�dZ@�dZ@�S�@���@��\@�$�@��@��T@�p�@�`B@�hs@�O�@�7L@�hs@�?}@���@��D@�9X@�9X@�1@���@���@�|�@��
@�|�@���@���@��\@��@�@�@�@��@��@�O�@��@���@���@��/@�Ĝ@�z�@�A�@�1@��m@���@���@��@��@�S�@���@��@���@�^5@�$�@�@��7@�?}@���@��j@�j@�(�@��@�  @��;@���@��@�;d@�
=@��@�ȴ@���@�ff@�$�@���@�x�@�G�@�%@��`@��`@��j@�z�@�bN@�Q�@�1'@��@|�@+@�@~ȴ@~ff@}�h@}��@}?}@|��@|z�@|9X@{��@{�@{C�@{"�@z��@z-@y��@y��@yX@y&�@y%@x�`@x�9@xb@w\)@w+@w�@v��@v��@v5?@v{@u�T@u�h@u?}@t�/@t�D@s��@s33@r^5@rM�@r=q@r-@q�@q�^@q��@qhs@q%@p�9@pbN@ol�@n��@n��@n$�@m��@mO�@m�@l�/@lj@k�m@kC�@j~�@i�#@i�^@ix�@i&�@h�9@h1'@h  @g|�@g�@f��@f�@fȴ@f5?@ep�@d��@d�@d�/@d�j@d�D@dz�@dZ@d1@cƨ@c�@c33@b�@b��@a��@a��@a&�@`��@` �@_��@_l�@_;d@_
=@^��@^ff@^{@]��@]��@]/@\Z@[��@[��@[33@Z��@Z�!@Zn�@Z-@Y�#@YX@X��@XĜ@X�@XQ�@X  @Wl�@Vȴ@Vv�@V�+@V��@V5?@U�-@U/@T��@T��@TZ@S�m@S@Rn�@R�@Qx�@P��@P��@P�@P �@O�w@O�@N�+@NE�@M�h@L��@L�/@L��@LZ@L�@Kƨ@Ko@J~�@I��@I��@I��@IG�@H�`@H�@HA�@G��@G;d@G+@G\)@GK�@Fȴ@Fv�@FE�@E�T@E�T@E�@EO�@D�@Dz�@D9X@C�
@C�F@C��@Ct�@Ct�@CdZ@CC�@Co@B��@Bn�@B=q@B�@BJ@A�#@A��@A%@@��@@r�@?��@?�@?|�@?K�@?K�@>��@>��@>V@>@=��@=�@=?}@=?}@=/@<��@<z�@<9X@<�@;��@;��@;S�@;�@:�H@:�@:J@9�7@9hs@9G�@9X@9X@9G�@97L@8��@8bN@81'@8b@7��@7+@6ȴ@6ȴ@6�R@6��@6V@5�T@5�-@5�-@5p�@4�/@4j@4(�@4(�@4�@41@41@3�
@3t�@2�H@2��@2�!@2^5@2=q@2=q@2�@1�@1G�@1%@0��@0Ĝ@0�@0 �@0  @/��@/�w@/|�@/\)@.��@/
=@/
=@.ff@.E�@.5?@.5?@-�@-�@-�@,��@,�@,��@,�@,Z@+�
@+t�@+dZ@+dZ@+33@+"�@+@*�\@*^5@*M�@*�@)��@)hs@)7L@)�@(��@(��@(r�@(bN@(Q�@'��@'�P@'|�@'�@&�@&�@&��@&v�@&5?@%@%�@%/@$��@$��@$�D@$Z@#��@#ƨ@#dZ@#S�@#"�@"�@"��@"��@"n�@"^5@"=q@"J@!�7@!G�@ ��@ �9@ ��@ r�@�@�w@�@��@l�@\)@+@
=@�y@�@�R@��@v�@E�@5?@5?@�T@��@�-@�-@��@`B@V@��@z�@I�@I�@(�@��@��@o@~�@�@��@�@x�@�`@��@�u@r�@bN@A�@ �@b@��@�@��@��@�P@�P@l�@\)@;d@
=@�y@ȴ@ȴ@��@��@V@@@?}@�@��@�j@I�@(�@�@��@�
@�F@�@�@t�@33@@��@�!@�!@~�@~�@M�@�#@�7@G�@&�@�@%@�u@A�@b@  @��@K�@�@��@ff@$�@@�@�T@��@@�h@p�@/@�@��@�j@j@(�@1@1@1@1@�m@�
@��@dZ@33@33@
�@
�H@
��@
~�@
J@	�@	��@	�^@	��@	�^@	�7@	X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A��A�(�A�"�A�(�A�(�A�%A��
A�G�A���A΃A��HA͑hA̓A�n�A�VA��A��A�  A�~�A�A�E�A�VA��A���AǴ9A�\)A�1'A��A��A���A�E�A�XA�C�A��A��TA��A��A��^A�A�A�oA���A��A��!A�-A���A�bNA�33A���A�?}A��
A���A�VA��A���A�x�A�9XA�
=A��A���A���A�K�A�{A��A�-A���A�E�A�;dA��+A�v�A�hsA�A��\A�"�A��;A�t�A�E�A���A���A���A�x�A�"�A�O�A���A�G�A��A�I�A�jA��-A��\A�l�A�"�A���A��A��!A�r�A��A��A��
A�x�A�ZA�ĜA�ZA���A�Q�A�
=A���A��/A���A��A�  A�v�A���A�v�A���A��A�|�A�1'A�1A���A�?}A�JA��A��
A�A�VA/A~��A}hsAy�Aw�Au��As�Ap  An�+AlVAj��AjjAh1Af�HAfn�Ae�;Ad��Ac��Ac
=Aa�A_A]��A]XAZ�AW�TAU�ATĜAR��AP�/AO+AM�hAL�AK��AJ{AGADVAC��AB�AA�AA&�A>5?A=K�A=G�A=`BA<�uA:��A8v�A6bNA4�DA2��A/�A.bA,z�A*�/A*ffA*bA)�PA)&�A'A&jA%�mA%�A$VA#t�A"n�A"A �yA M�A�A�7A��A�
Ap�A�yA  A(�A
=A�A�7A��AG�A(�AƨA�/A{A��A�-A�7A%A
^5A	�PA	
=A�-A\)AA��A��A��A�hA�;AoA 1@�$�@�r�@���@���@�hs@���@��@�ff@��@�I�@��@�O�@�&�@�l�@�^5@�1'@���@�M�@�p�@�Ĝ@�9X@�@�@�I�@�o@�x�@ۅ@��y@�-@��T@ف@ج@�;d@�=q@�?}@�S�@ѡ�@��`@��@�l�@�ȴ@�-@�O�@���@̃@��@�t�@��H@�5?@ɲ-@�hs@�%@ȣ�@�Q�@ǝ�@ƸR@�-@�@Ĭ@� �@��;@�S�@�V@��@���@���@��@�I�@�
=@���@���@�C�@��\@�-@���@���@��@���@�o@��R@�@���@�A�@��@��F@���@��@��@�Ĝ@�1'@��@���@�=q@���@���@��@��`@��w@�C�@�
=@��@��R@�ff@�E�@��@��7@��@�ƨ@���@�J@��-@��@�x�@��@�V@��`@�Ĝ@��D@��D@�bN@�(�@� �@�  @�ƨ@���@�dZ@�33@���@���@�~�@���@�/@��@��D@�ƨ@�S�@��@�n�@�-@�$�@�5?@�ff@�n�@�n�@�@���@�%@���@���@� �@���@�C�@���@�ȴ@��\@�V@�=q@�E�@��@���@��-@�X@���@���@�/@��/@�%@�X@�&�@���@���@���@���@��`@�X@�@�M�@�E�@�E�@�p�@�?}@�hs@���@�G�@�p�@��/@�Ĝ@�V@��j@�I�@� �@�(�@�9X@�1'@� �@��;@�dZ@�C�@��y@��y@��!@�5?@�@�J@�$�@�E�@�@�/@���@��@��9@���@�Z@�ƨ@��@�dZ@�dZ@�S�@���@��\@�$�@��@��T@�p�@�`B@�hs@�O�@�7L@�hs@�?}@���@��D@�9X@�9X@�1@���@���@�|�@��
@�|�@���@���@��\@��@�@�@�@��@��@�O�@��@���@���@��/@�Ĝ@�z�@�A�@�1@��m@���@���@��@��@�S�@���@��@���@�^5@�$�@�@��7@�?}@���@��j@�j@�(�@��@�  @��;@���@��@�;d@�
=@��@�ȴ@���@�ff@�$�@���@�x�@�G�@�%@��`@��`@��j@�z�@�bN@�Q�@�1'@��@|�@+@�@~ȴ@~ff@}�h@}��@}?}@|��@|z�@|9X@{��@{�@{C�@{"�@z��@z-@y��@y��@yX@y&�@y%@x�`@x�9@xb@w\)@w+@w�@v��@v��@v5?@v{@u�T@u�h@u?}@t�/@t�D@s��@s33@r^5@rM�@r=q@r-@q�@q�^@q��@qhs@q%@p�9@pbN@ol�@n��@n��@n$�@m��@mO�@m�@l�/@lj@k�m@kC�@j~�@i�#@i�^@ix�@i&�@h�9@h1'@h  @g|�@g�@f��@f�@fȴ@f5?@ep�@d��@d�@d�/@d�j@d�D@dz�@dZ@d1@cƨ@c�@c33@b�@b��@a��@a��@a&�@`��@` �@_��@_l�@_;d@_
=@^��@^ff@^{@]��@]��@]/@\Z@[��@[��@[33@Z��@Z�!@Zn�@Z-@Y�#@YX@X��@XĜ@X�@XQ�@X  @Wl�@Vȴ@Vv�@V�+@V��@V5?@U�-@U/@T��@T��@TZ@S�m@S@Rn�@R�@Qx�@P��@P��@P�@P �@O�w@O�@N�+@NE�@M�h@L��@L�/@L��@LZ@L�@Kƨ@Ko@J~�@I��@I��@I��@IG�@H�`@H�@HA�@G��@G;d@G+@G\)@GK�@Fȴ@Fv�@FE�@E�T@E�T@E�@EO�@D�@Dz�@D9X@C�
@C�F@C��@Ct�@Ct�@CdZ@CC�@Co@B��@Bn�@B=q@B�@BJ@A�#@A��@A%@@��@@r�@?��@?�@?|�@?K�@?K�@>��@>��@>V@>@=��@=�@=?}@=?}@=/@<��@<z�@<9X@<�@;��@;��@;S�@;�@:�H@:�@:J@9�7@9hs@9G�@9X@9X@9G�@97L@8��@8bN@81'@8b@7��@7+@6ȴ@6ȴ@6�R@6��@6V@5�T@5�-@5�-@5p�@4�/@4j@4(�@4(�@4�@41@41@3�
@3t�@2�H@2��@2�!@2^5@2=q@2=q@2�@1�@1G�@1%@0��@0Ĝ@0�@0 �@0  @/��@/�w@/|�@/\)@.��@/
=@/
=@.ff@.E�@.5?@.5?@-�@-�@-�@,��@,�@,��@,�@,Z@+�
@+t�@+dZ@+dZ@+33@+"�@+@*�\@*^5@*M�@*�@)��@)hs@)7L@)�@(��@(��@(r�@(bN@(Q�@'��@'�P@'|�@'�@&�@&�@&��@&v�@&5?@%@%�@%/@$��@$��@$�D@$Z@#��@#ƨ@#dZ@#S�@#"�@"�@"��@"��@"n�@"^5@"=q@"J@!�7@!G�@ ��@ �9@ ��@ r�@�@�w@�@��@l�@\)@+@
=@�y@�@�R@��@v�@E�@5?@5?@�T@��@�-@�-@��@`B@V@��@z�@I�@I�@(�@��@��@o@~�@�@��@�@x�@�`@��@�u@r�@bN@A�@ �@b@��@�@��@��@�P@�P@l�@\)@;d@
=@�y@ȴ@ȴ@��@��@V@@@?}@�@��@�j@I�@(�@�@��@�
@�F@�@�@t�@33@@��@�!@�!@~�@~�@M�@�#@�7@G�@&�@�@%@�u@A�@b@  @��@K�@�@��@ff@$�@@�@�T@��@@�h@p�@/@�@��@�j@j@(�@1@1@1@1@�m@�
@��@dZ@33@33@
�@
�H@
��@
~�@
J@	�@	��@	�^@	��@	�^@	�7@	X111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B_;B_;B_;B_;B_;B_;B_;B_;B^5B`BBcTBgmBm�Bo�Bo�Bo�Bp�B~�B�B�DB�bB�oB��B��B��B��B��B��B��B��B��B�7B~�Bv�Bn�Bl�BgmBffBk�Bn�Bm�Br�Bx�Bz�B}�B�B�B�+B�+B�=B�PB�\B�bB�hB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�VB�B�B|�Bt�BgmBZBVBS�BP�BJ�BD�B>wB33B"�B�BhBJB
=B  B�`B��B�3B��B�uB�1Bu�BdZBQ�B7LB�BB
��B
�ZB
�#B
�
B
��B
�dB
�B
��B
�bB
� B
p�B
ffB
^5B
YB
Q�B
9XB
+B
�B
PB	��B	�B	�#B	��B	ȴB	�XB	�!B	�B	��B	��B	��B	�oB	�7B	r�B	ffB	aHB	R�B	<jB	.B	$�B	�B	bB	
=B��B��B��B�B�ZB��B�XB�'B�9B�LB��B��B��B��B��B��B�oB�Bz�Bw�BdZBT�BM�BJ�BJ�BI�BG�BE�BC�BA�B?}B@�B>wB>wB;dB:^B;dB8RB8RB9XB7LB5?B7LB5?B6FB7LB49B5?B33B49B5?B49B49B5?B33B6FB6FB49B5?B6FB49B5?B49B33B33B2-B2-B/B/B,B+B+B'�B)�B&�B&�B&�B%�B%�B%�B$�B#�B#�B"�B$�B"�B$�B&�B'�B'�B(�B'�B&�B&�B'�B&�B&�B&�B&�B&�B'�B'�B'�B(�B)�B)�B+B.B/B0!B1'B1'B2-B33B33B49B49B5?B6FB6FB8RB8RB9XB:^B:^B;dB=qB@�BA�BB�BF�BI�BJ�BN�BR�BT�BVBVBW
BZBYBYB[#B`BBbNBdZBe`BffBl�Bo�Br�Bt�Bz�B�B�%B�+B�DB�oB��B��B��B��B��B��B��B��B��B��B��B�B�-B�3B�9B�9B�FB�LB�RB�dB��BĜB��B��B��B�B�B�)B�;B�HB�TB�fB�sB�B�yB�yB�yB�B�B�B�B�B�B�B�B��B��B��B��B��B	B	B	+B		7B	VB	uB	{B	�B	�B	#�B	(�B	+B	,B	/B	1'B	2-B	33B	7LB	:^B	<jB	>wB	>wB	B�B	C�B	C�B	D�B	K�B	O�B	H�B	G�B	H�B	M�B	O�B	O�B	Q�B	R�B	R�B	S�B	XB	]/B	aHB	dZB	hsB	gmB	hsB	jB	n�B	o�B	r�B	r�B	s�B	v�B	v�B	u�B	t�B	u�B	w�B	}�B	� B	� B	� B	� B	�B	�B	�B	�B	�B	�%B	�7B	�PB	�bB	�uB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�?B	�?B	�LB	�XB	�dB	�jB	�wB	�}B	��B	B	ÖB	ƨB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�)B	�/B	�/B	�5B	�5B	�5B	�5B	�;B	�;B	�BB	�HB	�TB	�TB	�ZB	�`B	�fB	�fB	�mB	�mB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
%B
%B
	7B
	7B

=B

=B
DB
DB
DB
DB
DB
JB
DB
JB
PB
PB
PB
PB
VB
\B
\B
bB
bB
bB
bB
bB
hB
oB
uB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
)�B
+B
+B
,B
,B
,B
,B
,B
,B
,B
,B
,B
,B
,B
-B
-B
-B
-B
.B
/B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
33B
49B
5?B
5?B
5?B
5?B
5?B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
8RB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
:^B
:^B
:^B
<jB
=qB
<jB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
F�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
XB
XB
XB
XB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
^5B
]/B
^5B
^5B
^5B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
ffB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
m�B
n�B
n�B
n�B
n�B
o�B
n�B
o�B
o�B
n�B
o�B
o�B
o�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B_!B_!B_!B_!B_!B_!B_!B_!B^B`'Bc:BgRBmwBo�Bo�Bo�Bp�B~�B�B�)B�HB�TB�mB��B��B��B��B��B��B��B�B�B~�Bv�Bn}BlqBgRBfLBkkBn}BmwBr�Bx�Bz�B}�B��B��B�B�B�#B�6B�BB�HB�NB�[B�aB�MB�mB�yB�yB�yB�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�~B�yB�<B��B��B|�Bt�BgRBZBU�BS�BP�BJ�BD�B>]B2�B"�B�BNB0B
#B��B�FB̳B�B�B�@B�Bu�Bd@BQ�B72BmB�B
��B
�&B
�	B
��B
ʌB
�JB
��B
��B
�.B
�B
p�B
fLB
^B
X�B
Q�B
9$B
*�B
~B
B	��B	�QB	�	B	͟B	ȚB	�>B	�B	��B	��B	��B	�sB	�:B	�B	r�B	f2B	aB	R�B	<PB	-�B	$�B	kB	.B	
#B��B��B��B�qB�@B�UB�>B��B�B�2B��B�eB�eB��B��B��B�TB��Bz�Bw�Bd@BT�BM�BJ�BJ�BI�BG�BEmBCaBAoB?HB@OB>BB>]B;0B:DB;0B8B8B9$B7B5B72B5B6B72B4B5%B3B4B5%B4B4B5B3B6+B6B4B5%B6+B4B5B4B3B3B1�B2B.�B.�B+�B*�B*�B'�B)�B&�B&�B&�B%�B%�B%�B$�B#�B#�B"�B$�B"�B$�B&�B'�B'�B(�B'�B&�B&�B'�B&�B&�B&�B&�B&�B'�B'�B'�B(�B)�B)�B*�B-�B.�B0B0�B1B1�B3B3B4B4B5B6B6+B8B8B9$B:*B:DB;0B=VB@OBAoBB[BF�BI�BJ�BN�BR�BT�BU�BU�BV�BY�BX�BX�BZ�B`Bb4Bd&Be,Bf2BlWBoiBr�Bt�Bz�B��B�B�B�B�TB�MB�yB�kB��B�~B��B��B��B��B��B��B��B�B��B�B�B�B�B�B�0B�iB�gBʌB͟BѷB��B��B�B�!B�B�:B�2B�XB�KB�DB�DB�DB�KB�QB�WB�WB�WB�cB�cB�|B��B��B��B��B��B	�B	�B	�B		B	"B	[B	aB	YB	�B	#�B	(�B	*�B	+�B	/ B	0�B	1�B	3B	72B	:*B	<6B	>BB	>BB	B[B	CaB	CaB	DgB	K�B	O�B	H�B	G�B	H�B	M�B	O�B	O�B	Q�B	R�B	R�B	S�B	W�B	]B	a-B	d&B	hXB	gRB	hXB	jKB	ncB	oiB	r�B	r�B	s�B	v�B	v�B	u�B	t�B	u�B	w�B	}�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�HB	�@B	�TB	�[B	�MB	�YB	�YB	�YB	�_B	�eB	��B	�xB	�~B	�~B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�%B	�%B	�B	�>B	�0B	�6B	�]B	�HB	�oB	�[B	�aB	ƎB	ɠB	ɆB	˒B	̘B	͹B	͟B	ΥB	ϫB	��B	ϫB	бB	��B	ҽB	ҽB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�B	�B	�B	�!B	�B	�B	�:B	� B	�&B	�,B	�2B	�2B	�8B	�8B	�>B	�_B	�_B	�KB	�eB	�QB	�QB	�kB	�WB	�wB	�wB	�wB	�wB	�cB	�iB	��B	�oB	��B	�oB	�B	�|B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
	B
	B

	B

#B
B
B
B
)B
B
0B
B
B
B
B
B
B
"B
BB
BB
.B
HB
.B
.B
.B
4B
TB
[B
@B
@B
[B
[B
aB
aB
FB
FB
FB
MB
MB
SB
SB
SB
mB
YB
YB
yB
_B
_B
_B
_B
eB
eB
kB
kB
�B
xB
xB
~B
~B
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
!�B
!�B
"�B
"�B
"�B
"�B
#�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
)�B
*�B
*�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
+�B
,�B
,�B
,�B
,�B
-�B
.�B
/�B
0B
0�B
1B
1B
0�B
1B
2B
2B
3B
3B
3B
4B
5B
5%B
5B
5%B
5B
5B
5B
6B
6B
6B
6B
6+B
6+B
7B
72B
72B
7B
7B
72B
72B
8B
8B
8B
88B
88B
9>B
:*B
:*B
:DB
:*B
:*B
:*B
;0B
;0B
;0B
:*B
:*B
:*B
<PB
=VB
<6B
=<B
=<B
=<B
>BB
>BB
>]B
>BB
>]B
>BB
?cB
?cB
?HB
@iB
@iB
@OB
@iB
AUB
AUB
AUB
AUB
AUB
B[B
B[B
BuB
CaB
CaB
CaB
CaB
CaB
C{B
C{B
D�B
DgB
EmB
FtB
FtB
FtB
FtB
FtB
FtB
GzB
GzB
GzB
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
U�B
U�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
ZB
Y�B
Y�B
ZB
Y�B
Y�B
Y�B
[	B
Z�B
[	B
[	B
[	B
Z�B
Z�B
[�B
[�B
[�B
[�B
\B
\B
]B
]B
\�B
^B
\�B
^B
^B
^B
_!B
_B
`B
`'B
`B
`B
aB
aB
a-B
aB
a-B
aB
bB
b4B
bB
b4B
bB
bB
b4B
b4B
c:B
c B
c:B
c:B
c B
c B
c B
c:B
c:B
d@B
d&B
d&B
eFB
e,B
e,B
e,B
e,B
e,B
fLB
f2B
f2B
fLB
fLB
fLB
f2B
f2B
f2B
f2B
fLB
fLB
fLB
f2B
fLB
g8B
gRB
h>B
hXB
h>B
h>B
h>B
iDB
iDB
iDB
iDB
i_B
jKB
jKB
jKB
kkB
kQB
kkB
kQB
kQB
kkB
kQB
kQB
lqB
lqB
lWB
lqB
m]B
mwB
m]B
m]B
m]B
m]B
m]B
m]B
m]B
m]B
n}B
m]B
n}B
ncB
ncB
ncB
oiB
n}B
oiB
o�B
ncB
oiB
o�B
oi111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.34(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201809110037392018091100373920180911003739201809120029222018091200292220180912002922JA  ARFMdecpA19c                                                                20180906153513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180906063514  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180906063515  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180906063516  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180906063516  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180906063516  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180906063516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180906063516  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180906063517  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180906063517                      G�O�G�O�G�O�                JA  ARUP                                                                        20180906065934                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180906155328  CV  JULD            G�O�G�O�F��?                JM  ARCAJMQC2.0                                                                 20180910153739  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180910153739  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180911152922  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021520                      G�O�G�O�G�O�                