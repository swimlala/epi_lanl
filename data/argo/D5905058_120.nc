CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-01-25T06:36:57Z creation;2019-01-25T06:37:00Z conversion to V3.1;2019-12-23T06:08:05Z update;     
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
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  `  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �l   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �l   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �l   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T  �l   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �$   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �4   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �8   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �<   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �@Argo profile    3.1 1.2 19500101000000  20190125063657  20200120021523  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               xA   JA  I2_0675_120                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @آȠ΀1   @آ��8�@7�hۋ�q�c2��N;�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DTy�DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�s3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @X��@�z�@�z�A=qA&=qAF=qAf=qA��A��A��A��A��A��A��A��B�\B	�\B�\B�\B!�\B)�\B1�\B9�\BA�\BI�\BQ�\BY�\Ba�\Bi�\Bq�\By�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC c�Cc�Cc�Cc�Cc�C
c�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�C c�C"c�C$c�C&c�C(c�C*c�C,c�C.c�C0c�C2c�C4c�C6c�C8c�C:c�C<c�C>c�C@c�CBc�CDc�CFc�CHc�CJc�CLc�CNc�CPc�CRc�CTc�CVc�CXc�CZc�C\c�C^c�C`c�Cbc�Cdc�Cfc�Chc�Cjc�Clc�Cnc�Cpc�Crc�Ctc�Cvc�Cxc�Czc�C|c�C~c�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�%C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�{D�L{D��{D��HD�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D��D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��HD��{D��D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�	HD�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D{D��{D�{D�L{DÌ{D��{D�{D�L{DČ{D��{D�{D�L{DŌ{D��{D�{D�L{Dƌ{D��{D�{D�L{Dǌ{D��{D�{D�L{DȌ{D��{D�{D�L{DɌ{D��{D�{D�L{Dʌ{D��{D�{D�L{Dˌ{D��{D�{D�L{Ď{D��{D�{D�L{D͌{D��{D�{D�L{DΌ{D��{D�{D�L{Dό{D��{D�{D�L{DЌ{D��{D�{D�L{Dь{D��{D�{D�L{DҌ{D��{D�{D�L{Dӌ{D��{D�{D�L{DԌ{D��{D�{D�L{DՌ{D��{D�{D�L{D֌{D��{D�{D�L{D׌{D��{D�{D�L{D،{D��{D�{D�L{Dٌ{D��{D�{D�L{Dڌ{D��{D�{D�L{Dی{D��{D�{D�L{D܌{D��{D�{D�L{D݌{D��{D�{D�L{Dތ{D��{D�{D�L{Dߌ{D��{D�{D�L{D��{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D��{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�	HD�L{D��{D��{D�{D�L{D��{D��{D�{D�O�D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�~�A�x�A�G�A��A���A��/A�ĜA��RA��-A��A��A���A���A���A���A���A���A���A���A���A���A��A��!A��RA���A��A�%A��A��A�oA�VA�oA��A��A��A� �A�&�A�1'A�9XA�A�A�I�A�S�A�^5A�l�A�v�A�|�A�|�A�x�A�hsA�A�A�=qA��+A��A��FA��9A�t�A�$�A�;dA��HA��+A� �A�9XA�x�A�|�A�JA���A�hsA���A��A�9XA�p�A��A�bNA�{A�1'A���A�S�A���A��A�dZA�~�A��A���A��^A� �A���A�9XA���A���A��/A�$�A�(�A��yA�\)A��#A���A�(�A��A�ƨA��A��TA�n�A��hA�ĜA�\)A�(�A���A��A�9XA�K�A�C�A�|�A��TA��#A�\)A��A�G�A��9A�ffA��A�+A���A�Q�A�-A�A}A| �Az��Axn�Av�AtA�Ar5?Aq�Ao�
AnE�Al�\AiG�Ah5?AfbNAd��Ac��A`ȴA^��A[x�AY7LAW�PAU�AP�RAOp�AM�ALv�AI;dAG�#AE�TAD��ACXABffA@��A@{A@  A>�DA;t�A;XA;�A:��A9dZA8^5A5�-A4ZA4(�A3dZA2jA1��A0�A/�A.�\A-t�A,9XA+�wA+p�A+
=A*��A*�DA)��A(5?A'��A%
=A$A�A#`BA"ȴA"�A!��A!��A!��A!��A!l�A �!A�7A�A�A�jA�FAQ�Al�A��A��Ax�An�A�wAI�A"�A�A�TAXA��A�
A;dA��A=qAS�A�+A��A
��A	�7A�AXA~�AK�A��A  AS�A&�A��A�A ��@�t�@���@�J@��9@�b@���@���@���@�|�@���@�P@�+@��@��@�K�@��@�Z@��@�&�@��@�7L@�%@��/@�@��@�9X@��
@߮@��y@݁@�  @�S�@�J@�/@�  @��@��m@�M�@�G�@��@�
=@���@�V@�I�@��;@ˍP@ɑh@��
@�K�@ƸR@�@�%@Ĵ9@��;@���@��@�7L@� �@���@��@���@�v�@��h@�Z@�  @�  @���@��@��;@���@��y@�5?@��-@��/@�I�@��@��@���@�@�5?@��-@�V@�r�@���@�-@�X@��`@���@��w@�S�@�
=@�n�@�J@���@�O�@��@��9@�Q�@�S�@��+@�ff@��@�x�@��@� �@��w@�"�@�^5@�`B@��@���@�j@�I�@��@���@��;@��P@�33@��@�$�@�p�@�X@��@��j@��@���@�b@���@�l�@�S�@�"�@��@���@�~�@�5?@���@�?}@��@�O�@�O�@���@�  @�l�@�33@�
=@�~�@��^@�G�@��@��`@��u@�1'@��F@�dZ@�+@���@�ff@��T@�X@���@��`@���@���@��9@�z�@�j@�Z@�z�@��u@�Ĝ@��m@�;d@��@��@���@�l�@���@���@��w@��w@��@�;d@�"�@�;d@�;d@�\)@�;d@���@�v�@�v�@�M�@��@��^@�x�@�7L@��@�(�@� �@��@�j@�z�@�A�@��@�ƨ@��w@��;@��@��m@�ƨ@�;d@��@��!@��\@�
=@��@�ȴ@��\@�V@��@�J@��T@���@�@��^@���@��@�`B@��@���@�Q�@�  @��;@���@�K�@�o@��H@��!@�v�@�ff@���@��@�@��-@��7@�x�@�G�@��@���@�Ĝ@��u@�r�@�j@�Z@�Q�@�A�@��
@�dZ@�"�@�ȴ@���@�^5@�5?@�J@���@���@�X@�V@��/@�Ĝ@��D@� �@�1@�  @�  @��@��;@��P@�S�@��@���@���@�~�@�^5@�5?@��@�@��@�p�@�7L@�%@��j@�A�@� �@�b@�@~�y@~�+@~5?@}�-@}p�@}/@|��@|Z@|9X@{�
@{t�@{"�@z��@zn�@y�#@yx�@yG�@x�`@xĜ@x��@x1'@w�;@wl�@v�y@v��@vv�@v{@u@u�@t�/@tZ@t�@s��@sC�@r��@q��@q�7@q7L@p�u@pb@o;d@nȴ@n��@nE�@n{@m@m`B@m�@l�@lZ@k��@k��@kdZ@kS�@k33@k@jM�@i��@i��@i�@h��@hr�@h �@g|�@g;d@g+@g�@g
=@f��@f��@fff@fff@fff@f$�@e��@e�h@e�@d��@d��@d�@dj@cƨ@c��@b��@bJ@a�#@a�#@a��@a�^@a��@a�7@a7L@`�9@`�u@`r�@`Q�@`b@_��@_|�@_�@^��@^�@^v�@^5?@^{@]�@]��@]p�@]`B@]/@\�/@\�@\��@\j@\�@[��@[�@[C�@[o@Z�@Z��@ZM�@ZJ@Y�^@YX@X�`@X�u@XQ�@Xb@W�@W\)@W+@V�y@V�R@V�+@V5?@U��@U�-@Up�@U/@T��@T�@T(�@S�m@S��@St�@SS�@S33@S@R�\@R-@Q�@Qhs@Q�@P��@O�;@O�@N�+@M@MO�@L�j@L�@Lj@Lj@Lj@LZ@K�
@Kt�@Ko@J^5@J�@J�@JJ@JJ@I��@I�@I�@I�@I��@I��@I��@Ix�@IG�@H��@H��@HbN@H �@Hb@G��@G��@G|�@G+@F�R@Fff@FV@F{@E�@E�@E��@E��@E/@D�@D�/@D�j@D�@Dz�@D9X@D�@C��@C��@CdZ@CC�@C"�@B�H@B^5@BJ@A�^@Ahs@A&�@A�@@�`@@Ĝ@@�9@@�9@@�u@@bN@@ �@?�;@?��@?�w@?�P@?l�@?\)@?+@?
=@>�y@>ȴ@>v�@>5?@=�@=��@=`B@=?}@<�/@<��@<j@<Z@<Z@<Z@;�
@;S�@;o@:�@:�!@:^5@:J@9��@9x�@9hs@9G�@9�@9%@8��@8�@81'@7�;@7�@7�P@7K�@6��@6�+@6V@5�@5@5�@4�D@4I�@4(�@4(�@4(�@4�@41@3�
@3@2~�@2n�@2M�@2M�@2=q@2=q@2�@1�#@1��@1X@1%@0��@0r�@0 �@/��@/��@/\)@/
=@.��@.�+@.ff@.$�@.@-@-�@-`B@-�@-V@,�@,��@,9X@+ƨ@+S�@+"�@+o@*�H@*�\@*�@)�@)�^@)�^@)x�@)hs@)G�@)�@(Ĝ@(A�@(b@(  @'�w@'��@'K�@'�@&�y@&�R@&��@&E�@%�@%��@%`B@%?}@%�@$�/@$j@$1@#�
@#t�@#o@#@"�@"�@"�H@"��@"�!@"�\@"-@!�^@!�7@!G�@ ��@ ��@ Q�@ b@�;@�@��@��@��@��@;d@��@�R@��@��@V@@��@�h@O�@�@�/@Z@�@�m@ƨ@��@�@33@��@�!@�\@~�@^5@-@J@�@��@��@�7@hs@�`@Ĝ@r�@A�@1'@b@�@��@�w@�@\)@�@��@ȴ@�+@ff@$�@�@��@�-@��@p�@�@��@�@(�@�@�@��@�
@�F@��@33@@�H@��@n�@-@�@�#@�^@��@�7@�@Ĝ@��@r�@�;@��@K�@+@�@
=@
=@ȴ@�R@��@v�@ff@5?@$�@�T@��@�h@p�@?}@�@V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�~�A�x�A�G�A��A���A��/A�ĜA��RA��-A��A��A���A���A���A���A���A���A���A���A���A���A��A��!A��RA���A��A�%A��A��A�oA�VA�oA��A��A��A� �A�&�A�1'A�9XA�A�A�I�A�S�A�^5A�l�A�v�A�|�A�|�A�x�A�hsA�A�A�=qA��+A��A��FA��9A�t�A�$�A�;dA��HA��+A� �A�9XA�x�A�|�A�JA���A�hsA���A��A�9XA�p�A��A�bNA�{A�1'A���A�S�A���A��A�dZA�~�A��A���A��^A� �A���A�9XA���A���A��/A�$�A�(�A��yA�\)A��#A���A�(�A��A�ƨA��A��TA�n�A��hA�ĜA�\)A�(�A���A��A�9XA�K�A�C�A�|�A��TA��#A�\)A��A�G�A��9A�ffA��A�+A���A�Q�A�-A�A}A| �Az��Axn�Av�AtA�Ar5?Aq�Ao�
AnE�Al�\AiG�Ah5?AfbNAd��Ac��A`ȴA^��A[x�AY7LAW�PAU�AP�RAOp�AM�ALv�AI;dAG�#AE�TAD��ACXABffA@��A@{A@  A>�DA;t�A;XA;�A:��A9dZA8^5A5�-A4ZA4(�A3dZA2jA1��A0�A/�A.�\A-t�A,9XA+�wA+p�A+
=A*��A*�DA)��A(5?A'��A%
=A$A�A#`BA"ȴA"�A!��A!��A!��A!��A!l�A �!A�7A�A�A�jA�FAQ�Al�A��A��Ax�An�A�wAI�A"�A�A�TAXA��A�
A;dA��A=qAS�A�+A��A
��A	�7A�AXA~�AK�A��A  AS�A&�A��A�A ��@�t�@���@�J@��9@�b@���@���@���@�|�@���@�P@�+@��@��@�K�@��@�Z@��@�&�@��@�7L@�%@��/@�@��@�9X@��
@߮@��y@݁@�  @�S�@�J@�/@�  @��@��m@�M�@�G�@��@�
=@���@�V@�I�@��;@ˍP@ɑh@��
@�K�@ƸR@�@�%@Ĵ9@��;@���@��@�7L@� �@���@��@���@�v�@��h@�Z@�  @�  @���@��@��;@���@��y@�5?@��-@��/@�I�@��@��@���@�@�5?@��-@�V@�r�@���@�-@�X@��`@���@��w@�S�@�
=@�n�@�J@���@�O�@��@��9@�Q�@�S�@��+@�ff@��@�x�@��@� �@��w@�"�@�^5@�`B@��@���@�j@�I�@��@���@��;@��P@�33@��@�$�@�p�@�X@��@��j@��@���@�b@���@�l�@�S�@�"�@��@���@�~�@�5?@���@�?}@��@�O�@�O�@���@�  @�l�@�33@�
=@�~�@��^@�G�@��@��`@��u@�1'@��F@�dZ@�+@���@�ff@��T@�X@���@��`@���@���@��9@�z�@�j@�Z@�z�@��u@�Ĝ@��m@�;d@��@��@���@�l�@���@���@��w@��w@��@�;d@�"�@�;d@�;d@�\)@�;d@���@�v�@�v�@�M�@��@��^@�x�@�7L@��@�(�@� �@��@�j@�z�@�A�@��@�ƨ@��w@��;@��@��m@�ƨ@�;d@��@��!@��\@�
=@��@�ȴ@��\@�V@��@�J@��T@���@�@��^@���@��@�`B@��@���@�Q�@�  @��;@���@�K�@�o@��H@��!@�v�@�ff@���@��@�@��-@��7@�x�@�G�@��@���@�Ĝ@��u@�r�@�j@�Z@�Q�@�A�@��
@�dZ@�"�@�ȴ@���@�^5@�5?@�J@���@���@�X@�V@��/@�Ĝ@��D@� �@�1@�  @�  @��@��;@��P@�S�@��@���@���@�~�@�^5@�5?@��@�@��@�p�@�7L@�%@��j@�A�@� �@�b@�@~�y@~�+@~5?@}�-@}p�@}/@|��@|Z@|9X@{�
@{t�@{"�@z��@zn�@y�#@yx�@yG�@x�`@xĜ@x��@x1'@w�;@wl�@v�y@v��@vv�@v{@u@u�@t�/@tZ@t�@s��@sC�@r��@q��@q�7@q7L@p�u@pb@o;d@nȴ@n��@nE�@n{@m@m`B@m�@l�@lZ@k��@k��@kdZ@kS�@k33@k@jM�@i��@i��@i�@h��@hr�@h �@g|�@g;d@g+@g�@g
=@f��@f��@fff@fff@fff@f$�@e��@e�h@e�@d��@d��@d�@dj@cƨ@c��@b��@bJ@a�#@a�#@a��@a�^@a��@a�7@a7L@`�9@`�u@`r�@`Q�@`b@_��@_|�@_�@^��@^�@^v�@^5?@^{@]�@]��@]p�@]`B@]/@\�/@\�@\��@\j@\�@[��@[�@[C�@[o@Z�@Z��@ZM�@ZJ@Y�^@YX@X�`@X�u@XQ�@Xb@W�@W\)@W+@V�y@V�R@V�+@V5?@U��@U�-@Up�@U/@T��@T�@T(�@S�m@S��@St�@SS�@S33@S@R�\@R-@Q�@Qhs@Q�@P��@O�;@O�@N�+@M@MO�@L�j@L�@Lj@Lj@Lj@LZ@K�
@Kt�@Ko@J^5@J�@J�@JJ@JJ@I��@I�@I�@I�@I��@I��@I��@Ix�@IG�@H��@H��@HbN@H �@Hb@G��@G��@G|�@G+@F�R@Fff@FV@F{@E�@E�@E��@E��@E/@D�@D�/@D�j@D�@Dz�@D9X@D�@C��@C��@CdZ@CC�@C"�@B�H@B^5@BJ@A�^@Ahs@A&�@A�@@�`@@Ĝ@@�9@@�9@@�u@@bN@@ �@?�;@?��@?�w@?�P@?l�@?\)@?+@?
=@>�y@>ȴ@>v�@>5?@=�@=��@=`B@=?}@<�/@<��@<j@<Z@<Z@<Z@;�
@;S�@;o@:�@:�!@:^5@:J@9��@9x�@9hs@9G�@9�@9%@8��@8�@81'@7�;@7�@7�P@7K�@6��@6�+@6V@5�@5@5�@4�D@4I�@4(�@4(�@4(�@4�@41@3�
@3@2~�@2n�@2M�@2M�@2=q@2=q@2�@1�#@1��@1X@1%@0��@0r�@0 �@/��@/��@/\)@/
=@.��@.�+@.ff@.$�@.@-@-�@-`B@-�@-V@,�@,��@,9X@+ƨ@+S�@+"�@+o@*�H@*�\@*�@)�@)�^@)�^@)x�@)hs@)G�@)�@(Ĝ@(A�@(b@(  @'�w@'��@'K�@'�@&�y@&�R@&��@&E�@%�@%��@%`B@%?}@%�@$�/@$j@$1@#�
@#t�@#o@#@"�@"�@"�H@"��@"�!@"�\@"-@!�^@!�7@!G�@ ��@ ��@ Q�@ b@�;@�@��@��@��@��@;d@��@�R@��@��@V@@��@�h@O�@�@�/@Z@�@�m@ƨ@��@�@33@��@�!@�\@~�@^5@-@J@�@��@��@�7@hs@�`@Ĝ@r�@A�@1'@b@�@��@�w@�@\)@�@��@ȴ@�+@ff@$�@�@��@�-@��@p�@�@��@�@(�@�@�@��@�
@�F@��@33@@�H@��@n�@-@�@�#@�^@��@�7@�@Ĝ@��@r�@�;@��@K�@+@�@
=@
=@ȴ@�R@��@v�@ff@5?@$�@�T@��@�h@p�@?}@�@V111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BhsBgmBgmBffBffBe`Be`Be`Be`Be`Be`Be`Be`Be`BdZBe`Be`Be`BffBgmBiyBk�Bm�Bo�Bz�B�=B��B��B�B�B�B�B�B�!B�'B�?B�XB�wBBƨB��B��B�
B�/B�TB�B�BB\B�B+BE�B_;BhsBhsBk�BdZB�B�B��BdZBT�B33B8RB;dB:^BF�BO�BO�BN�BH�B:^B0!B2-B&�B%�BhB��B�B%�B2-B@�B$�BA�B?}B9XB+B�BbB��B��B��B��B��B�B�5B��B�RB��B{�B]/BL�BH�B@�BE�BD�B@�B.B�B\B
��B
�B
��B
��B
�dB
�9B
��B
�B
m�B
bNB
[#B
YB
O�B
F�B
?}B
9XB
'�B
�B
VB	��B	�B	�5B	��B	��B	��B	�LB	��B	��B	�JB	� B	x�B	ffB	W
B	C�B	/B	#�B	bB�B�B�5B�fB�BǮB�dB�3B�B��B��B��B��B��B�bB��B��B��B��B��B�VB�+B�%B�Bz�Bv�Bs�Bn�BjBffBaHBcTBl�Bn�Bn�Bl�BgmBe`B_;BR�BK�BH�BF�BG�BG�BG�BM�BO�BT�BN�BO�BN�BR�BL�BI�BK�BI�BJ�BJ�BI�BG�BF�BD�B@�B?}B>wB=qB<jB:^B:^B8RB7LB6FB49B2-B2-B/B.B-B,B+B)�B(�B(�B'�B&�B'�B&�B&�B$�B%�B%�B%�B$�B#�B"�B%�B%�B%�B%�B$�B$�B%�B%�B%�B%�B%�B&�B'�B&�B&�B)�B)�B+B+B)�B+B+B-B,B.B,B-B-B.B0!B1'B2-B5?B6FB9XB:^B:^B:^B?}BA�BA�BC�BE�BE�BE�BG�BJ�BK�BL�BP�BR�BR�BS�BS�BXB[#B\)B\)B\)B\)B\)B]/B`BBbNBdZBgmBhsBiyBjBjBn�Bp�Bq�Bs�Bt�By�B{�B~�B�B�B�%B�+B�1B�=B�JB�VB�bB�hB�oB��B��B��B��B��B��B��B��B�B�'B�FB�jB�wB��BŢBȴB��B��B��B��B�B�B�/B�HB�NB�yB�B�B�B��B��B��B��B	B	B	1B		7B	VB	oB	{B	�B	�B	 �B	!�B	"�B	$�B	&�B	'�B	-B	0!B	33B	49B	5?B	8RB	<jB	@�B	B�B	B�B	E�B	H�B	L�B	O�B	P�B	S�B	[#B	]/B	]/B	aHB	bNB	ffB	hsB	jB	m�B	q�B	p�B	r�B	s�B	u�B	y�B	|�B	~�B	�B	�B	�1B	�7B	�JB	�VB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�-B	�3B	�9B	�?B	�?B	�?B	�FB	�LB	�jB	�wB	�}B	�}B	��B	��B	��B	B	ÖB	ÖB	ÖB	ĜB	ŢB	ŢB	ŢB	ŢB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�/B	�;B	�HB	�NB	�NB	�TB	�ZB	�ZB	�`B	�`B	�`B	�fB	�mB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
+B
+B
1B
1B
	7B

=B

=B
DB
JB
JB
PB
PB
VB
VB
VB
VB
\B
\B
\B
bB
bB
hB
hB
hB
hB
hB
uB
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
+B
,B
,B
,B
-B
-B
.B
/B
/B
/B
/B
0!B
0!B
/B
/B
0!B
0!B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
49B
49B
49B
5?B
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
8RB
8RB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
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
B�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
I�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
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
S�B
T�B
T�B
T�B
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
W
B
XB
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
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
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
dZB
dZB
dZB
dZB
dZB
dZB
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
gmB
gmB
gmB
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
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
m�B
m�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  BhXBg8BgRBfLBfLBeFBeFBeFBeFBeFBeFBeFBeFBe,Bd@BeFBeFBeFBfLBgRBi_BkkBmwBoiBz�B�	B�yB��B��B��B��B��B��B�B�B�%B�>B�]B�uBƎB̳B��B��B�B�:B�kB�BBBByB*�BE�B_BhXBh>BkkBd@B��B��B�yBd@BT�B2�B8B;JB:*BF�BO�BO�BN�BH�B:DB/�B1�B&�B%�B4B��BsB%�B1�B@iB$�BAoB?cB9>B*�B�BHB��B�B��B��B��B�iB�B͹B�8B�YB{�B]BL�BH�B@iBE�BD�B@iB-�B�B(B
��B
��B
��B
�UB
�0B
�B
��B
��B
m]B
bB
[	B
X�B
O�B
F�B
?cB
9>B
'�B
�B
<B	��B	�]B	�B	��B	̘B	�OB	�B	��B	�SB	�0B	�B	x�B	f2B	V�B	CaB	.�B	#�B	.B�WB�KB�B�2B��BǔB�0B�B��B��B��B��B��B�yB�HB��B��B��B��B��B�"B��B��B��Bz�Bv�Bs�Bn}BjKBfLBaBc:BlqBn}BncBlqBgRBe,B_BR�BK�BH�BFtBGzBGzBGzBM�BO�BT�BN�BO�BN�BR�BL�BI�BK�BI�BJ�BJ�BI�BG�BFtBDgB@OB?HB>BB=<B<PB:*B:*B88B72B6B4B1�B1�B.�B-�B,�B+�B*�B)�B(�B(�B'�B&�B'�B&�B&�B$�B%�B%�B%�B$�B#�B"�B%�B%�B%�B%�B$�B$�B%�B%�B%�B%�B%�B&�B'�B&�B&�B)�B)�B*�B*�B)�B*�B*�B,�B+�B-�B+�B,�B,�B-�B/�B0�B1�B5B6B9$B:*B:*B:*B?HBAoBAUBCaBEmBEmBE�BGzBJ�BK�BL�BP�BR�BR�BS�BS�BW�BZ�B[�B[�B[�B[�B\B\�B`BbBd&Bg8BhXBi_BjeBjeBncBp�Bq�Bs�Bt�By�B{�B~�B��B��B�B��B��B�	B�0B�"B�HB�4B�TB�SB��B��B��B��B��B��B��B��B��B�B�6B�BB�oB�mBȀBʌB͟BбB��B��B��B��B�-B�B�DB�QB�WB�oB��B��B��B��B	 �B	�B	�B		B	"B	:B	FB	SB	qB	 �B	!�B	"�B	$�B	&�B	'�B	,�B	/�B	2�B	4B	5B	8B	<6B	@OB	B[B	B[B	EmB	H�B	L�B	O�B	P�B	S�B	Z�B	\�B	\�B	aB	a�B	f2B	h>B	jKB	mwB	qvB	poB	r|B	s�B	u�B	y�B	|�B	~�B	��B	��B	�B	�B	�B	�"B	�.B	�FB	�SB	�_B	�YB	�kB	��B	��B	��B	��B	��B	��B	�pB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�%B	�B	��B	�B	�6B	�BB	�HB	�HB	�OB	�OB	�UB	�[B	�aB	�aB	�aB	āB	�mB	�mB	�mB	�mB	�zB	ʌB	˒B	̘B	͟B	͟B	ΥB	бB	бB	ҽB	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	��B	��B	��B	��B	�B	�B	�B	�4B	� B	�@B	�&B	�FB	�,B	�B	�2B	�8B	�RB	�>B	�>B	�KB	�KB	�KB	�KB	�KB	�6B	�CB	�}B	�cB	�iB	�iB	��B	�oB	�vB	�vB	�vB	�vB	�|B	�|B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
B
�B
B
�B
�B
	B

	B

	B
B
�B
B
B
B
<B
"B
"B
"B
(B
(B
(B
.B
.B
4B
4B
4B
4B
4B
@B
@B
@B
@B
FB
FB
FB
MB
MB
MB
MB
MB
MB
9B
SB
SB
mB
SB
SB
YB
_B
_B
YB
YB
_B
eB
_B
kB
kB
kB
QB
kB
qB
qB
�B
qB
xB
xB
xB
xB
~B
~B
~B
~B
~B
~B
�B
�B
�B
�B
pB
pB
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
!|B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'�B
'�B
'�B
(�B
(�B
(�B
(�B
)�B
)�B
*�B
*�B
+�B
+�B
+�B
,�B
,�B
-�B
.�B
.�B
.�B
.�B
/�B
/�B
.�B
.�B
/�B
/�B
0�B
0�B
2B
1�B
1�B
1�B
1�B
1�B
1�B
2B
1�B
1�B
1�B
1�B
2�B
2�B
4B
4B
4B
5B
5B
5B
5B
5%B
4�B
5B
5B
6B
6B
6B
6B
6B
6B
7B
7B
6�B
7B
8B
88B
8B
8B
8B
88B
8B
8B
9$B
9$B
:B
:*B
:*B
:DB
:*B
;0B
;0B
;JB
;0B
;0B
;JB
;0B
<6B
<6B
<6B
<6B
<6B
<PB
<PB
<6B
<6B
=<B
="B
=<B
=<B
>]B
>BB
?HB
?.B
?HB
?HB
?HB
?HB
?HB
@OB
@OB
AUB
AUB
AUB
AUB
A;B
B[B
B[B
B[B
BuB
BuB
B[B
CaB
CaB
CaB
C{B
C{B
CaB
DgB
E�B
EmB
EmB
EmB
EmB
FtB
FtB
GzB
G�B
GzB
GzB
G_B
GzB
GzB
H�B
H�B
H�B
I�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
Q�B
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
S�B
T�B
T�B
T�B
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
V�B
W�B
V�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
[	B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
\B
\�B
\�B
\�B
\�B
^B
^B
^B
]�B
^B
_B
_!B
_B
_!B
_B
_B
_B
`B
`B
`B
`'B
`B
aB
aB
aB
bB
bB
bB
bB
bB
bB
bB
b4B
c B
c:B
cB
c B
c B
d&B
d@B
d&B
d&B
d&B
d&B
e,B
e,B
e,B
f2B
f2B
f2B
fB
f2B
f2B
f2B
f2B
g8B
g8B
g8B
g8B
g8B
hXB
h>B
h$B
h>B
h>B
iDB
iDB
iDB
iDB
j0B
jKB
jKB
kQB
kQB
kkB
kkB
kQB
kQB
kQB
kQB
kQB
kkB
kQB
lWB
lWB
lWB
lWB
lWB
m]B
m]111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.39(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201901300040212019013000402120190130004021201901310025182019013100251820190131002518JA  ARFMdecpA19c                                                                20190125153634  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190125063657  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190125063658  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190125063658  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190125063659  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190125063659  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190125063659  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190125063659  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190125063700  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190125063700                      G�O�G�O�G�O�                JA  ARUP                                                                        20190125065758                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190125153643  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20190129154021  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190129154021  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190130152518  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021523                      G�O�G�O�G�O�                