CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-08-28T15:36:17Z creation;2018-08-28T15:36:20Z conversion to V3.1;2019-12-23T06:16:06Z update;     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20180828153617  20200120021520  5905058                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               UA   JA  I2_0675_085                     2C  D   NAVIS_A                         0675                            ARGO 102115                     863 @�} ���1   @�}!q��@8
=p��
�c5o���1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBhffBo��Bw��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DDy�DD��DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc�fDd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@�\)A�A'�AG�Ag�A��
A��
A��
A��
A��
A��
A��
A��
B�B	�B�B�B!�B)�B1�B9�BA�BI�BQ�BY�BbQ�BjQ�Bq�By�B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���B���C z�Cz�Cz�Cz�Cz�C
z�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�Cz�C z�C"z�C$z�C&z�C(z�C*z�C,z�C.z�C0z�C2z�C4z�C6z�C8z�C:�{C<z�C>z�C@z�CBz�CDz�CFz�CHz�CJz�CLz�CNz�CPz�CRz�CTz�CVz�CXz�CZz�C\z�C^z�C`z�Cbz�Cdz�Cfz�Chz�Cjz�Clz�Cnz�Cpz�Crz�Ctz�Cvz�Cxz�Czz�C|z�C~z�C�=qC�=qC�=qC�=qC�=qC�J=C�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qC�=qD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD�RDERDE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc�Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��)D�\D�O\D��\D��\D�\D�O\D��\D��\D��D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�R�D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D\D��\D�\D�O\DÏ\D��\D�\D�O\Dď\D��\D�\D�O\Dŏ\D��\D�\D�O\DƏ\D��\D�\D�O\DǏ\D��\D�\D�O\Dȏ\D��\D�\D�O\Dɏ\D��\D�\D�O\Dʏ\D��\D�\D�O\Dˏ\D��\D�\D�O\D̏\D��\D�\D�O\D͏\D��\D�\D�O\DΏ\D��\D�\D�O\DϏ\D��\D�\D�O\DЏ\D��\D�\D�O\Dя\D��\D�\D�O\Dҏ\D��\D�\D�O\Dӏ\D��\D�\D�O\Dԏ\D��\D�\D�O\DՏ\D��\D�\D�O\D֏\D��\D�\D�O\D׏\D��\D�\D�O\D؏\D��\D�\D�O\Dُ\D��\D�\D�O\Dڏ\D��\D�\D�O\Dۏ\D��\D�\D�O\D܏\D��\D�\D�O\Dݏ\D��\D�\D�O\Dޏ\D��\D�\D�O\Dߏ\D��\D�\D�O\D��\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�)D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D��\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D�\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�\D�O\D��\D��\D�)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�+A�+A�+A�7LA�9XA�9XA�9XA�;dA�;dA�;dA�=qA�;dA�=qA�=qA�=qA�?}A�A�A�?}A�=qA�=qA�?}A�A�A�A�A�?}A�?}A�/A�  A��mA�+AɮAǴ9A�~�A�|�A�5?A��A�ĜA�~�A��\A���A�{A�(�A��A���A�A�A���A��A�VA��A��A�=qA���A�=qA��!A�;dA��jA�33A�r�A��!A�JA��#A��+A��A��9A���A���A�p�A��A���A�ZA�ĜA�I�A�O�A�r�A��A��A���A��hA��DA�ffA�?}A�
=A��#A��jA���A��DA�n�A�XA�1'A��A��yA���A�A�A���A�M�A�5?A�E�A��mA��jA�A��/A�`BA��A�|�A�ZA�dZA��HA�1'A��A�{A��mA��!A���A��7A���A���A��A�(�A�E�A�A��jA��A��HA�A��^A���A�9XA�5?A}dZAx��Av��Au�Av �As�Aq�PAot�AmO�Al�Aj9XAh�RAg�#Ae33Ac��Ab�/Aa|�A`�A_7LA^�\A]�;A[C�AZ�AZJAX�/AVVAR�RAO�AJJAB�yAB�AAO�A?
=A=�PA<1A:�jA9��A8��A6v�A4�yA3�wA1��A01A-ƨA+A+/A*��A*�/A*ȴA*��A*VA*  A'ƨA&~�A%/A$�9A"��A"  A!A��Az�A�HA�A�A��AffA�AS�A1'A��AK�A��A��A
=A-A`BA��A$�A�A
-A	�A�yA��A33A�uA��A�+A�FA?}AoA��AƨA ��A $�@�"�@�/@���@�S�@�"�@�o@��y@���@�M�@�Q�@�{@�7L@�Ĝ@�9X@�E�@�%@�1'@��@�t�@��@�v�@�E�@��#@�&�@�l�@���@��@�9X@�\)@��@�w@�n�@�@��@��;@�X@۶F@�ff@���@ف@�p�@��@�5?@�bN@�"�@�{@Ь@�1'@��@��@��;@�dZ@�=q@�%@�(�@�"�@�~�@�n�@�1'@��T@�(�@�"�@��@��7@��@�Q�@��@�@��\@�{@�`B@�r�@�9X@�ƨ@�\)@�;d@���@��R@�E�@��^@��/@�Z@��
@��P@��R@��@�@�hs@�&�@��/@��m@�;d@��H@���@�=q@��^@���@� �@��
@�;d@�v�@�@��-@��@�`B@���@��@�
=@�ff@�E�@��@�`B@��9@�Q�@���@�-@�I�@���@��R@�ȴ@�v�@�E�@��@���@�@�hs@��;@�b@��@�z�@�9X@���@���@��;@��F@��;@��@�@���@���@���@�Q�@��;@�S�@�+@�@��R@�=q@��@�V@��F@�;d@��@��@��H@��!@�V@���@��-@�p�@�?}@��@�%@���@��u@�z�@�bN@�Q�@���@��@��w@�K�@�
=@��R@�ff@�=q@�$�@��@���@�`B@�&�@��@�j@���@��@��@��y@��@�ȴ@���@�n�@�M�@�5?@���@��^@��@���@��-@��-@�O�@��@��@��/@���@�r�@�1'@�1'@�(�@���@��w@���@��@��
@�Q�@�O�@�O�@��@���@�j@��@�v�@���@���@���@��+@�ȴ@��!@��@��R@�~�@�-@��@��@�@��^@���@�X@�Ĝ@��D@�Z@� �@�1@�  @��@���@�C�@��H@���@�M�@�$�@��@���@�hs@�G�@�O�@�`B@�7L@���@���@�j@�9X@��
@��F@�\)@�@���@��!@��\@�V@�{@��T@���@�p�@�hs@�X@�&�@�%@���@�Z@�1'@�1'@��@��m@��
@�ƨ@��F@���@�|�@�l�@�K�@�33@�o@��y@��R@�n�@�M�@�5?@�@���@���@�X@��@��@���@��j@���@�r�@�I�@�  @\)@
=@~��@~@}p�@}V@|�@|I�@|9X@{ƨ@{dZ@{o@z��@y�@yhs@y7L@xQ�@w�;@w\)@vȴ@v$�@u�-@u�h@uO�@uV@t��@tI�@t(�@s�m@s�
@s�@s"�@r�\@rM�@r-@q�#@q��@qG�@p�9@pQ�@p1'@o�;@o��@oK�@n�@n��@nE�@m�T@m��@m/@m�@l�@l�@l�@k�m@kƨ@kt�@j�@j�\@j=q@i�#@i�7@ihs@i&�@h��@h�9@h�u@hQ�@hb@g�@g��@g+@fff@e�h@d��@d9X@c�
@c�
@c�
@c�
@cƨ@cdZ@b�H@b�@a�7@`�`@`A�@_��@_�@^��@^{@]`B@\�/@\�D@\Z@\�@[�
@[dZ@[33@Z�H@Z=q@Y�^@Y�7@Yx�@YX@X��@XQ�@W�;@W�w@Wl�@W\)@V��@Vȴ@Vff@V5?@V5?@V$�@U��@U?}@UV@T�@T��@Tz�@T1@S�m@SdZ@R^5@Q�#@Qx�@QX@QG�@Q7L@Q%@PĜ@P��@P�u@P �@O|�@OK�@O+@O
=@N��@Nff@M�@M��@M�h@L��@L�D@Lz�@LI�@L(�@L�@K��@K"�@K@J��@J��@J^5@JJ@I��@H�`@H�@HQ�@H  @G��@G;d@G�@F��@E��@E��@E`B@E�@D��@D�@D��@D��@Dj@D�@C�m@C��@C33@B�@BM�@A�#@A��@Ax�@AX@@��@@Ĝ@@�9@@�u@@1'@@b@@  @?�;@?�w@?�w@?l�@?;d@>��@>��@>�+@>v�@>ff@>E�@>$�@=�T@=�h@=p�@=p�@=`B@=?}@<��@<�@<Z@<(�@<1@<1@<1@<1@;ƨ@;�@;S�@;@:��@:-@9��@9��@97L@8�`@8��@8�@8�@8bN@8 �@7�@7�@7K�@6�y@6��@6v�@6E�@6{@5�@5@5`B@5V@4�j@4z�@4j@49X@41@3�F@3t�@3C�@3@2�!@2^5@2�@1��@1��@1hs@1�@0r�@/�@/�P@/\)@/\)@/\)@/K�@/�@.�y@.��@.��@.v�@.E�@-��@-�@-/@,��@,�j@,z�@,Z@,(�@+��@+ƨ@+��@+�@+dZ@+@*�H@*��@*��@*��@*=q@)��@)�7@)hs@)hs@)x�@)G�@(��@(��@(bN@(1'@(b@'�w@'l�@'K�@';d@'+@&�@&�+@&V@&$�@%��@%��@%@%��@%�@%p�@%O�@%?}@$��@$��@$�@#�
@#33@#o@#o@"�@"��@"�\@!�@!��@!�7@!&�@ �`@ �u@ 1'@  �@�@��@�@l�@;d@��@�@ȴ@��@�+@ff@E�@�@��@�@?}@/@�/@�D@j@9X@��@��@t�@S�@S�@"�@o@@@�H@�H@��@n�@M�@-@�#@�7@&�@�`@�u@ �@�@l�@\)@;d@;d@;d@�@ȴ@ff@E�@@@��@�h@p�@?}@��@�D@j@I�@1@�
@�@t�@dZ@"�@�H@n�@J@�@��@��@��@�7@x�@hs@7L@�`@��@��@�9@�u@Q�@A�@1'@��@�w@�@��@�P@|�@l�@\)@;d@
=@ȴ@v�@$�@�T@��@@�-@��@��@��@�h@p�@O�@V@��@��@Z@I�@��@��@�@dZ@C�@C�@C�@"�@
�@
��@
��@
��@
�!111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�+A�+A�+A�7LA�9XA�9XA�9XA�;dA�;dA�;dA�=qA�;dA�=qA�=qA�=qA�?}A�A�A�?}A�=qA�=qA�?}A�A�A�A�A�?}A�?}A�/A�  A��mA�+AɮAǴ9A�~�A�|�A�5?A��A�ĜA�~�A��\A���A�{A�(�A��A���A�A�A���A��A�VA��A��A�=qA���A�=qA��!A�;dA��jA�33A�r�A��!A�JA��#A��+A��A��9A���A���A�p�A��A���A�ZA�ĜA�I�A�O�A�r�A��A��A���A��hA��DA�ffA�?}A�
=A��#A��jA���A��DA�n�A�XA�1'A��A��yA���A�A�A���A�M�A�5?A�E�A��mA��jA�A��/A�`BA��A�|�A�ZA�dZA��HA�1'A��A�{A��mA��!A���A��7A���A���A��A�(�A�E�A�A��jA��A��HA�A��^A���A�9XA�5?A}dZAx��Av��Au�Av �As�Aq�PAot�AmO�Al�Aj9XAh�RAg�#Ae33Ac��Ab�/Aa|�A`�A_7LA^�\A]�;A[C�AZ�AZJAX�/AVVAR�RAO�AJJAB�yAB�AAO�A?
=A=�PA<1A:�jA9��A8��A6v�A4�yA3�wA1��A01A-ƨA+A+/A*��A*�/A*ȴA*��A*VA*  A'ƨA&~�A%/A$�9A"��A"  A!A��Az�A�HA�A�A��AffA�AS�A1'A��AK�A��A��A
=A-A`BA��A$�A�A
-A	�A�yA��A33A�uA��A�+A�FA?}AoA��AƨA ��A $�@�"�@�/@���@�S�@�"�@�o@��y@���@�M�@�Q�@�{@�7L@�Ĝ@�9X@�E�@�%@�1'@��@�t�@��@�v�@�E�@��#@�&�@�l�@���@��@�9X@�\)@��@�w@�n�@�@��@��;@�X@۶F@�ff@���@ف@�p�@��@�5?@�bN@�"�@�{@Ь@�1'@��@��@��;@�dZ@�=q@�%@�(�@�"�@�~�@�n�@�1'@��T@�(�@�"�@��@��7@��@�Q�@��@�@��\@�{@�`B@�r�@�9X@�ƨ@�\)@�;d@���@��R@�E�@��^@��/@�Z@��
@��P@��R@��@�@�hs@�&�@��/@��m@�;d@��H@���@�=q@��^@���@� �@��
@�;d@�v�@�@��-@��@�`B@���@��@�
=@�ff@�E�@��@�`B@��9@�Q�@���@�-@�I�@���@��R@�ȴ@�v�@�E�@��@���@�@�hs@��;@�b@��@�z�@�9X@���@���@��;@��F@��;@��@�@���@���@���@�Q�@��;@�S�@�+@�@��R@�=q@��@�V@��F@�;d@��@��@��H@��!@�V@���@��-@�p�@�?}@��@�%@���@��u@�z�@�bN@�Q�@���@��@��w@�K�@�
=@��R@�ff@�=q@�$�@��@���@�`B@�&�@��@�j@���@��@��@��y@��@�ȴ@���@�n�@�M�@�5?@���@��^@��@���@��-@��-@�O�@��@��@��/@���@�r�@�1'@�1'@�(�@���@��w@���@��@��
@�Q�@�O�@�O�@��@���@�j@��@�v�@���@���@���@��+@�ȴ@��!@��@��R@�~�@�-@��@��@�@��^@���@�X@�Ĝ@��D@�Z@� �@�1@�  @��@���@�C�@��H@���@�M�@�$�@��@���@�hs@�G�@�O�@�`B@�7L@���@���@�j@�9X@��
@��F@�\)@�@���@��!@��\@�V@�{@��T@���@�p�@�hs@�X@�&�@�%@���@�Z@�1'@�1'@��@��m@��
@�ƨ@��F@���@�|�@�l�@�K�@�33@�o@��y@��R@�n�@�M�@�5?@�@���@���@�X@��@��@���@��j@���@�r�@�I�@�  @\)@
=@~��@~@}p�@}V@|�@|I�@|9X@{ƨ@{dZ@{o@z��@y�@yhs@y7L@xQ�@w�;@w\)@vȴ@v$�@u�-@u�h@uO�@uV@t��@tI�@t(�@s�m@s�
@s�@s"�@r�\@rM�@r-@q�#@q��@qG�@p�9@pQ�@p1'@o�;@o��@oK�@n�@n��@nE�@m�T@m��@m/@m�@l�@l�@l�@k�m@kƨ@kt�@j�@j�\@j=q@i�#@i�7@ihs@i&�@h��@h�9@h�u@hQ�@hb@g�@g��@g+@fff@e�h@d��@d9X@c�
@c�
@c�
@c�
@cƨ@cdZ@b�H@b�@a�7@`�`@`A�@_��@_�@^��@^{@]`B@\�/@\�D@\Z@\�@[�
@[dZ@[33@Z�H@Z=q@Y�^@Y�7@Yx�@YX@X��@XQ�@W�;@W�w@Wl�@W\)@V��@Vȴ@Vff@V5?@V5?@V$�@U��@U?}@UV@T�@T��@Tz�@T1@S�m@SdZ@R^5@Q�#@Qx�@QX@QG�@Q7L@Q%@PĜ@P��@P�u@P �@O|�@OK�@O+@O
=@N��@Nff@M�@M��@M�h@L��@L�D@Lz�@LI�@L(�@L�@K��@K"�@K@J��@J��@J^5@JJ@I��@H�`@H�@HQ�@H  @G��@G;d@G�@F��@E��@E��@E`B@E�@D��@D�@D��@D��@Dj@D�@C�m@C��@C33@B�@BM�@A�#@A��@Ax�@AX@@��@@Ĝ@@�9@@�u@@1'@@b@@  @?�;@?�w@?�w@?l�@?;d@>��@>��@>�+@>v�@>ff@>E�@>$�@=�T@=�h@=p�@=p�@=`B@=?}@<��@<�@<Z@<(�@<1@<1@<1@<1@;ƨ@;�@;S�@;@:��@:-@9��@9��@97L@8�`@8��@8�@8�@8bN@8 �@7�@7�@7K�@6�y@6��@6v�@6E�@6{@5�@5@5`B@5V@4�j@4z�@4j@49X@41@3�F@3t�@3C�@3@2�!@2^5@2�@1��@1��@1hs@1�@0r�@/�@/�P@/\)@/\)@/\)@/K�@/�@.�y@.��@.��@.v�@.E�@-��@-�@-/@,��@,�j@,z�@,Z@,(�@+��@+ƨ@+��@+�@+dZ@+@*�H@*��@*��@*��@*=q@)��@)�7@)hs@)hs@)x�@)G�@(��@(��@(bN@(1'@(b@'�w@'l�@'K�@';d@'+@&�@&�+@&V@&$�@%��@%��@%@%��@%�@%p�@%O�@%?}@$��@$��@$�@#�
@#33@#o@#o@"�@"��@"�\@!�@!��@!�7@!&�@ �`@ �u@ 1'@  �@�@��@�@l�@;d@��@�@ȴ@��@�+@ff@E�@�@��@�@?}@/@�/@�D@j@9X@��@��@t�@S�@S�@"�@o@@@�H@�H@��@n�@M�@-@�#@�7@&�@�`@�u@ �@�@l�@\)@;d@;d@;d@�@ȴ@ff@E�@@@��@�h@p�@?}@��@�D@j@I�@1@�
@�@t�@dZ@"�@�H@n�@J@�@��@��@��@�7@x�@hs@7L@�`@��@��@�9@�u@Q�@A�@1'@��@�w@�@��@�P@|�@l�@\)@;d@
=@ȴ@v�@$�@�T@��@@�-@��@��@��@�h@p�@O�@V@��@��@Z@I�@��@��@�@dZ@C�@C�@C�@"�@
�@
��@
��@
��@
�!111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B  B  B  B  B  B  B  B  B  B  B  B  B  B  B  B  B  B  B  B  B  B  B  B  B  B  B  B��B��B��B#�B/B+B9XBB�BH�BE�B@�BI�BN�BYB[#BcTB`BBdZBdZBbNBe`BgmBhsBe`Bl�BjBhsBl�Bs�Bw�By�Bz�B}�B~�Bz�Bz�B� B�B�B�B� By�Bv�Bp�Bm�BhsBl�B�B�PB�hB�{B��B��B��B��B��B��B�B�B�B�B��B��B��B��B�{B�=Bv�B]/BT�BQ�B9XB	7B�NBB�B�{B�+Bl�BK�B(�B�BB
�B
�5B
�yB
��B
��B
�sB
ɺB
��B
��B
��B
��B
��B
�bB
�B
e`B
ZB
H�B
-B
DB	�B	�B
%B	��B	�B	�B	�HB	�
B	��B	��B	�dB	�B	��B	��B	�bB	�7B	�B	}�B	y�B	gmB	cTB	\)B	O�B	2-B	hB�sB�jBu�BgmB_;BQ�BN�BVBQ�BXBZBT�BQ�BL�BL�BG�BF�B?}B=qB<jB<jB;dB;dB9XB8RB8RB8RB6FB5?B5?B33B1'B1'B0!B33B6FB6FB7LB6FB7LB6FB5?B7LB7LB5?B5?B49B33B1'B1'B/B0!B.B.B-B.B-B-B-B-B-B-B,B,B,B+B,B+B-B,B-B-B,B,B,B+B-B,B+B+B+B+B+B+B+B,B,B-B,B,B,B-B,B)�B'�B'�B(�B'�B'�B&�B%�B&�B%�B(�B'�B(�B(�B'�B'�B-B-B.B0!B1'B2-B33B33B33B33B5?B8RB;dB?}B?}B@�BE�BA�B>wB?}BB�BD�BD�BF�BH�BI�BK�BL�BN�BO�BP�BQ�BR�BR�BS�BS�BVBXB[#B]/B_;B`BBdZBe`BgmBhsBiyBjBm�Bo�Bq�Bq�Bs�Bu�Bx�B}�B~�B�B�B�1B�=B�=B�=B�VB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�-B�3B�9B�FB�LB�qBBĜBĜBǮB��B��B�B�/B�NB�`B�sB�B��B	
=B	{B	�B	�B	�B	�B	�B	"�B	#�B	%�B	(�B	)�B	+B	,B	,B	/B	1'B	33B	5?B	7LB	8RB	8RB	:^B	=qB	?}B	@�B	B�B	F�B	H�B	I�B	J�B	L�B	P�B	T�B	VB	YB	ZB	\)B	]/B	^5B	`BB	bNB	e`B	gmB	iyB	jB	jB	jB	m�B	q�B	s�B	v�B	y�B	{�B	�B	�B	�B	�%B	�%B	�%B	�%B	�%B	�=B	�JB	�\B	�\B	�\B	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�B	�3B	�9B	�FB	�LB	�RB	�XB	�^B	�jB	�wB	�}B	��B	�}B	�wB	�wB	�}B	��B	��B	B	ÖB	ĜB	ƨB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�5B	�/B	�/B	�5B	�;B	�BB	�HB	�NB	�TB	�TB	�ZB	�ZB	�`B	�`B	�`B	�`B	�`B	�fB	�mB	�mB	�sB	�sB	�sB	�yB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
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
+B
+B
1B
1B
1B
	7B
	7B

=B
DB
DB
JB
JB
JB
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
\B
\B
\B
\B
\B
\B
\B
bB
bB
bB
oB
uB
uB
{B
{B
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
"�B
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
$�B
%�B
%�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
+B
,B
,B
-B
-B
-B
-B
.B
.B
/B
/B
/B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
33B
33B
49B
49B
49B
49B
49B
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
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
:^B
:^B
;dB
;dB
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
>wB
?}B
?}B
?}B
@�B
?}B
@�B
@�B
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
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
C�B
C�B
D�B
E�B
F�B
F�B
F�B
F�B
G�B
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
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
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
M�B
M�B
L�B
M�B
M�B
M�B
M�B
N�B
M�B
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
O�B
P�B
Q�B
Q�B
R�B
S�B
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
XB
XB
XB
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
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
aHB
aHB
`BB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
ffB
gmB
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
hsB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
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
l�B
l�B
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
n�B
n�B
n�B
n�B
n�B
n�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B#�B/ B*�B9>BB[BH�BEmB@iBI�BN�BX�BZ�Bc:B`'Bd&Bd@BbBe,BgRBh>Be,BlqBjKBh>BlWBs�Bw�By�Bz�B}�B~�Bz�Bz�B�B��B��B��B�By�Bv�Bp�Bm]Bh>BlWB��B�B�4B�FB�sB��B��B��B��B��B��B��B��B��B��B��B��B�xB�aB�#Bv�B\�BT�BQ�B9$B	B�B�[B��B�FB��BlWBK�B(�BMB�B
�QB
�B
�DB
��B
��B
�>B
ɆB
��B
��B
��B
��B
�SB
�.B
��B
e,B
Y�B
H�B
,�B
B	�B	�iB
�B	��B	�vB	�cB	�B	��B	̘B	�OB	�0B	��B	��B	�kB	�.B	�B	��B	}�B	y�B	g8B	c B	[�B	O�B	1�B	4B�>B�6Bu�Bg8B_BQ�BN�BU�BQ�BW�BY�BT�BQ�BL�BL�BGzBFtB?HB=<B<6B<6B;B;0B9$B8B8B8B6B5B5B2�B0�B0�B/�B2�B6B6B7B6B7B6B5B6�B7B4�B5B4B2�B0�B0�B.�B/�B-�B-�B,�B-�B,�B,�B,�B,�B,�B,�B+�B+�B+�B*�B+�B*�B,�B+�B,�B,�B+�B+�B+�B*�B,�B+�B*�B*�B*�B*�B*�B*�B*�B+�B+�B,�B+�B+�B+�B,�B+�B)�B'�B'�B(�B'�B'�B&�B%�B&�B%�B(�B'�B(�B(�B'�B'�B,�B,�B-�B/�B0�B1�B2�B2�B2�B2�B5B8B;0B?HB?HB@4BEmBA;B>BB?.BB[BDgBDgBFtBHfBI�BK�BL~BN�BO�BP�BQ�BR�BR�BS�BS�BU�BW�BZ�B\�B_B`BdBe,Bg8Bh>BiDBj0BmCBoiBq[BqvBshButBx�B}�B~�B��B��B��B��B��B�	B�B� B�_B�qB�qB�WB��B��B��B��B��B��B��B�vB��B��B��B��B��B�B��B�B�"B�[B�gB�MB�zB�~BѷB��B��B�B�B�>B�vB��B		�B	FB	_B	eB	kB	xB	]B	"�B	#�B	%�B	(�B	)�B	*�B	+�B	+�B	.�B	0�B	2�B	4�B	7B	8B	8B	:*B	="B	?.B	@4B	BAB	FtB	H�B	I�B	J�B	L�B	P�B	T�B	U�B	X�B	Y�B	[�B	\�B	]�B	_�B	bB	eB	gB	iDB	jKB	j0B	j0B	m]B	q[B	s�B	v�B	y�B	{�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�(B	�(B	�B	� B	�&B	�FB	�9B	�QB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�$B	�*B	�B	�(B	�HB	�4B	�.B	�BB	�BB	�HB	�OB	�UB	�[B	�GB	�gB	�tB	�fB	�fB	ʌB	�rB	̘B	̈́B	ΊB	ϫB	бB	ѷB	ңB	ңB	��B	өB	��B	��B	ԯB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�B	� B	� B	�&B	�&B	�,B	�,B	�B	�B	�B	�B	�8B	�B	�>B	�>B	�>B	�DB	�*B	�*B	�WB	�CB	�]B	�IB	�iB	�iB	�UB	�oB	�[B	�[B	�|B	�B	�B	�nB	�B	��B	��B	�zB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B

	B
B

�B
�B
B
B
�B
B
B
B
B
"B
"B
"B
B
"B
(B
B
(B
B
B
B
(B
.B
B
.B
 B
@B
&B
FB
,B
FB
,B
FB
,B
FB
2B
MB
9B
SB
?B
_B
EB
KB
KB
QB
QB
kB
kB
WB
qB
xB
xB
]B
xB
xB
xB
]B
xB
xB
qB
]B
xB
dB
~B
~B
~B
dB
dB
�B
�B
�B
jB
�B
pB
pB
�B
!|B
!�B
"�B
"�B
"�B
"�B
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
$�B
%�B
%�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
*�B
*�B
+�B
+�B
,�B
,�B
,�B
,�B
-�B
-�B
.�B
.�B
.�B
.�B
.�B
.�B
/�B
/�B
0�B
0�B
0�B
1�B
1�B
2�B
2�B
3�B
3�B
3�B
3�B
4B
4B
4B
3�B
4�B
4�B
5B
4�B
4�B
5B
5B
5B
6B
7B
6�B
7B
6�B
7B
7B
7B
8B
8B
8B
8B
8B
8B
9	B
9$B
9$B
:*B
:*B
:B
:*B
:*B
:*B
:*B
;0B
;B
<6B
=<B
="B
="B
>(B
>(B
>(B
>(B
>(B
>(B
>(B
?HB
?HB
?.B
@OB
?HB
@OB
@OB
@OB
@OB
@4B
@OB
AUB
AUB
A;B
A;B
A;B
B[B
BAB
BAB
BAB
CGB
CaB
CaB
DgB
DgB
DgB
DMB
CaB
CaB
DgB
EmB
FtB
FtB
FYB
FYB
GzB
FtB
FtB
GzB
GzB
GzB
G_B
GzB
GzB
G_B
H�B
HfB
HfB
HfB
HfB
HfB
H�B
H�B
HfB
IlB
I�B
I�B
JrB
J�B
JrB
JrB
KxB
KxB
KxB
KxB
L�B
L�B
M�B
M�B
L~B
M�B
M�B
M�B
M�B
N�B
M�B
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
O�B
P�B
Q�B
Q�B
R�B
S�B
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
W�B
W�B
W�B
W�B
W�B
W�B
W�B
X�B
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]�B
^B
_B
_B
^�B
`B
`B
`B
_�B
`�B
`�B
_�B
`�B
aB
bB
a�B
bB
c B
c B
c B
c B
cB
c B
d&B
c B
dB
d&B
d&B
eB
e,B
e,B
eB
eB
f2B
g8B
gB
g8B
g8B
g8B
gB
h>B
h$B
h$B
h>B
h$B
h$B
iDB
iDB
iDB
i*B
iDB
i*B
iDB
iDB
i*B
iDB
i*B
iDB
jKB
jKB
jKB
jKB
j0B
jKB
k6B
kQB
kQB
k6B
kQB
kQB
kQB
l=B
lWB
l=B
lWB
lWB
lWB
mCB
mCB
m]B
mCB
m]B
m]B
mCB
mCB
mCB
nIB
ncB
ncB
ncB
ncB
nc111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS error, 0.01(PSS-78))                                                                                                                                                                                                                    SP=-0.48(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201809030042382018090300423820180903004238201809040036532018090400365320180904003653JA  ARFMdecpA19c                                                                20180829003521  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180828153617  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180828153618  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180828153618  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180828153619  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180828153619  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180828153619  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180828153619  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180828153619  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180828153620                      G�O�G�O�G�O�                JA  ARUP                                                                        20180828155721                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180828154034  CV  JULD            G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20180828154034  CV  JULD_LOCATION   G�O�G�O�F��                JM  ARGQJMQC2.0                                                                 20180828154034  CV  LATITUDE        G�O�G�O�A�M�                JM  ARGQJMQC2.0                                                                 20180828154034  CV  LONGITUDE       G�O�G�O���J                JM  ARCAJMQC2.0                                                                 20180902154238  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180902154238  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180903153653  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200120021520                      G�O�G�O�G�O�                