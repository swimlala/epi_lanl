CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2019-09-15T09:38:22Z creation;2019-09-15T09:38:24Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p$   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �,   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �4   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �l   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �(   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �,   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �<   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �D   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �HArgo profile    3.1 1.2 19500101000000  20190915093822  20190915095404  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               3A   JA                                  2B  A   APEX                            7906                            051216                          846 @������1   @��Ѷ`�@1�G�z��e�t�j1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @9��@�  @�  A   A   A@  A`  A�  A�  A�  A�33A�  A���A�  A�  B   B  B  B  B   B(  B/��B8  B@  BH  BP  BXffB`  Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B���B�  B�  B���B���B�  B�  B�33B�ffB���B���B���B���B�  B�  B�  C �C  C  C�C�fC	�fC  C  C  C�C  C  C  C  C�fC  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CU�fCX  CZ  C\  C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx�Cz�C|  C~  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  Dy�D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D(��D)y�D*  D*� D+  D+� D,  D,� D-fD-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D3��D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR�fDSfDS� DS��DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[y�D\  D\� D]  D]� D^  D^y�D_  D_� D`  D`� Da  Da� Db  Db� Db��Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Dh��Diy�Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�3D�C3D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D��3D�� D�  D�@ D��3D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�|�D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�<�Dˀ D�� D���D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D��3D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�C3DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D��3D�  D�@ Dـ D��3D�  D�<�Dڀ D�� D�  D�@ Dۀ D�� D�  D�C3D܀ D�� D�  D�@ D݃3D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�<�D�|�D�� D�  D�<�D�|�D��D���D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D�|�D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�C3D� D��D�  D�@ D� D��3D�3D�@ D� D�� D�3D�@ D� D�� D�  D�@ D�|�D�� D�3D�C3D��3D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D���D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�y�D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @Mp�@��@��A��A$��AD��Ad��A�z�A�z�A�z�A��A�z�A�G�A�z�A�z�B=qB	=qB=qB=qB!=qB)=qB0�B9=qBA=qBI=qBQ=qBY��Ba=qBi=qBq=qBy=qB�k�B���B���B���B���B���B���B���B���B���B���B���B���B���B�B���B�k�BĞ�BȞ�B�k�B�k�BԞ�B؞�B���B�B�k�B�k�B�k�B�k�B���B���B���C h�CO\CO\Ch�C5�C
5�CO\CO\CO\Ch�CO\CO\CO\CO\C5�CO\C O\C"O\C$O\C&5�C(O\C*O\C,O\C.O\C0O\C2O\C4O\C6O\C8O\C:O\C<O\C>O\C@5�CBO\CDO\CFO\CHO\CJO\CLO\CNO\CPO\CRO\CTO\CV5�CXO\CZO\C\O\C^h�C`O\CbO\CdO\CfO\ChO\CjO\ClO\CnO\CpO\CrO\CtO\CvO\Cxh�Czh�C|O\C~O\C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C��C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�4{C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C�'�C��C�'�C�'�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�qD	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D�qD�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)qD)�qD*�D*��D+�D+��D,�D,��D-=D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4qD4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR�=DS=DS��DTqDT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[�qD\�D\��D]�D]��D^�D^�qD_�D_��D`�D`��Da�Da��Db�Db��DcqDc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��DiqDi�qDj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�D�MD���D���D�	�D�I�D���D�ƹD�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D�ƹD�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D��D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�D�I�D��D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D�ƹD�	�D�I�D���D�ƹD�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D�ƹD�	�D�I�D��D���D�	�D�I�D��D��D�D�I�D���D���D�	�D�I�D���D���D��D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D��D���D�	�D�I�D���D���D�	�D�I�D���D���D��D�F�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�D�I�D���D���D�	�D�I�D��D���D�	�D�I�D���D���D�	�D�I�D�D���D�	�D�I�DÉ�D���D�	�D�I�Dĉ�D���D�	�D�I�Dŉ�D���D�	�D�I�DƉ�D���D�	�D�I�Dǉ�D���D�	�D�I�Dȉ�D���D�	�D�I�Dɉ�D���D�	�D�I�Dʉ�D���D�	�D�F�Dˉ�D���D��D�I�D̉�D���D�	�D�I�D͉�D���D�	�D�I�DΉ�D��D�	�D�I�Dω�D���D�	�D�I�DЉ�D���D�	�D�I�Dщ�D���D�	�D�I�D҉�D���D�	�D�I�DӉ�D���D�	�D�MDԉ�D���D�	�D�I�DՉ�D���D�	�D�I�D։�D���D�	�D�I�D׉�D���D�	�D�I�D؉�D��D�	�D�I�Dى�D��D�	�D�F�Dډ�D���D�	�D�I�Dۉ�D���D�	�D�MD܉�D���D�	�D�I�DݍD���D�	�D�I�Dމ�D���D�	�D�I�D߉�D���D�	�D�I�D���D���D�	�D�I�DᆹD���D�	�D�I�D��D���D�	�D�F�DㆹD���D�	�D�F�D䆹D�ƹD��D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�F�D膹D���D�	�D�I�D��D���D�D�I�D��D���D�	�D�MD��D�ƹD�	�D�I�D��D��D�D�I�D��D���D�D�I�D��D���D�	�D�I�DD���D�D�MD��D��D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D��D���D�	�D�I�D�D�ƹD�	�D�MD��D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D���D�	�D�I�D���D� R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�"�A啁A囦A噚A�\A�+A�A�r�A�l�A�l�A�jA�jA�hsA�ffA�ffA�ffA�dZA�\)A�VA�S�A�M�A�K�A�E�A�C�A�A�A�=qA�33A�(�A��A��A� �A� �A� �A��A��A��A��A�oA�VA�1A�A���A���A�dZA�~�A�PA�ȴA��yA�JAϗ�A�I�A�E�Aȣ�AǼjA�bNA�ZA��wA�jA��mA�1'A��A���A���A�~�A�~�A���A���A�I�A��\A���A�-A�?}A�l�A�|�A���A���A�z�A�9XA�M�A��A�|�A�"�A�hsA�JA��A���A�p�A�XA�Q�A�C�A�=qA��A�
=A�%A�jA�r�A��A�oA�XA���A��A���A��mA��\A��
A�p�A��Av��Av  At�RAq;dAo33Ag
=A_ƨA^�9A]�PA[��AY�;AX��AWAS"�AP�9AP �AO��AOoAM��AL1AI�AG��AG�PAF=qAD��AB��A@z�A=A:��A:��A:�DA:bNA9��A9&�A7�;A61A4�\A1K�A.�RA,5?A*�uA)x�A(�RA({A'��A'K�A&��A&^5A%XA$bNA#"�A!/A VA��A�A�A�AoAffA�A�hA;dA��A�AE�AA�AoA�TA+A�jA��A�`A=qAx�A�Az�AA�A5?A��A��A�HA33An�AhsA
VA	�A	`BA��A�A��A�hAdZA�DA�PA%AZ@��@�\)@�G�@��P@�dZ@�D@�S�@�{@�h@���@� �@��@�D@�@旍@�^@���@��;@㕁@��@�$�@�^@�`B@��@�1'@�ȴ@��#@�O�@�V@�r�@�ƨ@�+@�~�@ٙ�@���@�bN@��m@׍P@��H@�V@�V@�=q@�$�@�J@���@�p�@ԣ�@��@ӝ�@Ӆ@�l�@�C�@�33@�
=@җ�@�x�@�/@��`@У�@�z�@�r�@�r�@�j@�Z@�(�@��m@϶F@ύP@�o@Η�@�`B@���@���@�z�@�9X@�b@��m@˥�@�t�@�|�@˅@�l�@�l�@�dZ@�C�@�
=@�ff@ɺ^@ə�@�G�@��`@� �@���@��@��#@Ų-@Ł@�7L@ļj@�dZ@�^5@���@�hs@��@��@�  @���@��w@��@��P@�K�@�@�=q@���@���@���@��h@�x�@�`B@�%@�Ĝ@��
@�33@��@��H@��@��!@�~�@�{@���@��@�/@��@��9@�j@�j@�j@�bN@��@�1@��
@��F@���@�@�/@���@�(�@��m@���@��F@�K�@���@�-@���@���@�G�@�&�@�%@��9@�j@�A�@���@��
@��F@�|�@�dZ@�;d@�ȴ@��\@�V@�$�@��^@��h@��@�&�@���@�z�@���@���@��w@���@���@���@���@�;d@��@��y@��@�~�@�@��T@��-@�x�@���@�z�@� �@��m@���@��@�dZ@�"�@���@���@���@��\@�~�@�n�@�ff@�^5@�E�@�J@��@��#@�@���@�X@��@��`@��@��w@���@�l�@�S�@�C�@�o@��y@��R@�v�@�V@�E�@���@�X@�/@�V@���@��D@�I�@�ƨ@���@�o@���@�E�@��T@�x�@�G�@�&�@��`@��9@�j@�b@�|�@�33@��@��y@��H@��@���@���@�~�@�@��h@�hs@�?}@���@�I�@���@���@���@��7@�`B@���@�Ĝ@���@��D@��@�I�@�1'@�  @���@�dZ@�33@�o@��y@�ff@��@�@���@��@�z�@�(�@�b@��@���@��@���@�t�@�33@��H@���@�v�@�-@���@��@���@��9@���@���@��D@��@��@�z�@�bN@�Q�@�I�@�Q�@�Q�@�Z@�j@�9X@���@��;@�ƨ@���@��@�K�@��@��H@��@��^@���@�p�@�?}@�7L@��@�V@���@��j@���@�r�@�bN@�9X@��@�1@��m@���@�t�@�l�@�K�@�33@�@��R@�~�@��@��@���@��D@�bN@�9X@��;@�|�@���@��!@��+@�E�@�=q@�=q@�{@�x�@�X@�G�@�?}@�?}@�/@�&�@�&�@�V@�z�@~�y@|��@|��@|�D@|z�@|�@{��@{�F@{��@{�@{dZ@{C�@z�H@z��@z��@z��@z�!@z�\@zn�@z^5@z�@yG�@y&�@x��@x��@x�@w��@v�R@u@u`B@uV@t�j@tz�@tI�@s��@s�@s@r�\@r�\@rn�@r=q@qx�@p �@ol�@o�@nȴ@nff@nE�@m��@k@i�#@i��@i�@h �@g\)@g;d@g�@f��@fV@e�T@e`B@dI�@c�
@c33@c"�@c@b�H@b��@b�\@b�@a&�@`�9@_�@]��@\Z@Z~�@Y�#@Y��@Y�7@Y%@XbN@W�w@W�P@W|�@W�@Vv�@V@U��@U��@U�@U`B@T�@TI�@S��@S"�@R^5@R�@Q��@Q��@Q��@Q��@Q��@Q�7@Qx�@Q%@P�@PA�@O��@O�@N��@NE�@M�T@M�-@M��@M�@M�@L��@K�
@Kt�@I��@I%@H�@G�@G�P@G\)@G�@FV@E��@D�@DZ@Cƨ@C�@CdZ@C@B^5@A��@A��@Ahs@A�@@  @?+@>��@>ȴ@>�+@>V@=�@=��@=�h@<�j@<�D@<�D@<z�@<I�@<�@<1@<�@;��@;��@;S�@;C�@:�!@:�\@:-@:J@:J@9��@9�#@9��@9�^@9�^@9�^@9�^@9hs@8�`@8��@8Ĝ@8�u@7l�@6��@6ff@4I�@17L@/�;@.��@.ȴ@.��@.��@.��@.��@.v�@.ff@.5?@-�T@-�-@-�h@-�h@-�h@-��@-�-@-�-@-�-@-�-@-@-@-�h@-�@-�@-�@-`B@-O�@-?}@-O�@-V@,�j@,�j@,��@,�D@,�D@,�D@,z�@,Z@,9X@,9X@,(�@+dZ@+o@*�\@*~�@*^5@*n�@*J@*-@)��@(��@(��@(�`@(�`@(�`@(�`@(��@(��@(�`@(��@)%@)G�@)�@(�`@(�9@(bN@(r�@(�u@(r�@(�9@(�9@(�u@(��@(r�@(A�@(b@(1'@'�w@%V@#�@#C�@#o@"��@"n�@"M�@"�@!��@!G�@!�@ ��@ �u@ bN@�;@�P@K�@��@E�@�/@�T@E�@O�@"�@33@o@o@@�!@n�@n�@n�@M�@=q@��@�^@hs@��@E�@5?@ff@$�@{@�@�@�T@v�@E�@5?@@�+@��@`B@�T@v�@�+@V@ff@�+@�@�y@�@
=@��@��@�T@��@@��@�-@�h@�-@�-@@��@@�T@�h@��@�-@�-@@��@�@�@�@�T@�@�T@@`B@/@�@?}@`B@?}@?}@/@?}@/@�@V@V@�j@z�@�D@(�@(�@(�@(�@(�@9X@(�@�@�D@j@��@�j@V@?}@V@�@�/@V@V@�@z�@I�@9X@9X@9X@(�@��@��@�
@�m@��@�m@ƨ@��@��@��@��@��@�@dZ@dZ@dZ@S�@��@n�@n�@=q@�@�7@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A�"�A啁A囦A噚A�\A�+A�A�r�A�l�A�l�A�jA�jA�hsA�ffA�ffA�ffA�dZA�\)A�VA�S�A�M�A�K�A�E�A�C�A�A�A�=qA�33A�(�A��A��A� �A� �A� �A��A��A��A��A�oA�VA�1A�A���A���A�dZA�~�A�PA�ȴA��yA�JAϗ�A�I�A�E�Aȣ�AǼjA�bNA�ZA��wA�jA��mA�1'A��A���A���A�~�A�~�A���A���A�I�A��\A���A�-A�?}A�l�A�|�A���A���A�z�A�9XA�M�A��A�|�A�"�A�hsA�JA��A���A�p�A�XA�Q�A�C�A�=qA��A�
=A�%A�jA�r�A��A�oA�XA���A��A���A��mA��\A��
A�p�A��Av��Av  At�RAq;dAo33Ag
=A_ƨA^�9A]�PA[��AY�;AX��AWAS"�AP�9AP �AO��AOoAM��AL1AI�AG��AG�PAF=qAD��AB��A@z�A=A:��A:��A:�DA:bNA9��A9&�A7�;A61A4�\A1K�A.�RA,5?A*�uA)x�A(�RA({A'��A'K�A&��A&^5A%XA$bNA#"�A!/A VA��A�A�A�AoAffA�A�hA;dA��A�AE�AA�AoA�TA+A�jA��A�`A=qAx�A�Az�AA�A5?A��A��A�HA33An�AhsA
VA	�A	`BA��A�A��A�hAdZA�DA�PA%AZ@��@�\)@�G�@��P@�dZ@�D@�S�@�{@�h@���@� �@��@�D@�@旍@�^@���@��;@㕁@��@�$�@�^@�`B@��@�1'@�ȴ@��#@�O�@�V@�r�@�ƨ@�+@�~�@ٙ�@���@�bN@��m@׍P@��H@�V@�V@�=q@�$�@�J@���@�p�@ԣ�@��@ӝ�@Ӆ@�l�@�C�@�33@�
=@җ�@�x�@�/@��`@У�@�z�@�r�@�r�@�j@�Z@�(�@��m@϶F@ύP@�o@Η�@�`B@���@���@�z�@�9X@�b@��m@˥�@�t�@�|�@˅@�l�@�l�@�dZ@�C�@�
=@�ff@ɺ^@ə�@�G�@��`@� �@���@��@��#@Ų-@Ł@�7L@ļj@�dZ@�^5@���@�hs@��@��@�  @���@��w@��@��P@�K�@�@�=q@���@���@���@��h@�x�@�`B@�%@�Ĝ@��
@�33@��@��H@��@��!@�~�@�{@���@��@�/@��@��9@�j@�j@�j@�bN@��@�1@��
@��F@���@�@�/@���@�(�@��m@���@��F@�K�@���@�-@���@���@�G�@�&�@�%@��9@�j@�A�@���@��
@��F@�|�@�dZ@�;d@�ȴ@��\@�V@�$�@��^@��h@��@�&�@���@�z�@���@���@��w@���@���@���@���@�;d@��@��y@��@�~�@�@��T@��-@�x�@���@�z�@� �@��m@���@��@�dZ@�"�@���@���@���@��\@�~�@�n�@�ff@�^5@�E�@�J@��@��#@�@���@�X@��@��`@��@��w@���@�l�@�S�@�C�@�o@��y@��R@�v�@�V@�E�@���@�X@�/@�V@���@��D@�I�@�ƨ@���@�o@���@�E�@��T@�x�@�G�@�&�@��`@��9@�j@�b@�|�@�33@��@��y@��H@��@���@���@�~�@�@��h@�hs@�?}@���@�I�@���@���@���@��7@�`B@���@�Ĝ@���@��D@��@�I�@�1'@�  @���@�dZ@�33@�o@��y@�ff@��@�@���@��@�z�@�(�@�b@��@���@��@���@�t�@�33@��H@���@�v�@�-@���@��@���@��9@���@���@��D@��@��@�z�@�bN@�Q�@�I�@�Q�@�Q�@�Z@�j@�9X@���@��;@�ƨ@���@��@�K�@��@��H@��@��^@���@�p�@�?}@�7L@��@�V@���@��j@���@�r�@�bN@�9X@��@�1@��m@���@�t�@�l�@�K�@�33@�@��R@�~�@��@��@���@��D@�bN@�9X@��;@�|�@���@��!@��+@�E�@�=q@�=q@�{@�x�@�X@�G�@�?}@�?}@�/@�&�@�&�@�V@�z�@~�y@|��@|��@|�D@|z�@|�@{��@{�F@{��@{�@{dZ@{C�@z�H@z��@z��@z��@z�!@z�\@zn�@z^5@z�@yG�@y&�@x��@x��@x�@w��@v�R@u@u`B@uV@t�j@tz�@tI�@s��@s�@s@r�\@r�\@rn�@r=q@qx�@p �@ol�@o�@nȴ@nff@nE�@m��@k@i�#@i��@i�@h �@g\)@g;d@g�@f��@fV@e�T@e`B@dI�@c�
@c33@c"�@c@b�H@b��@b�\@b�@a&�@`�9@_�@]��@\Z@Z~�@Y�#@Y��@Y�7@Y%@XbN@W�w@W�P@W|�@W�@Vv�@V@U��@U��@U�@U`B@T�@TI�@S��@S"�@R^5@R�@Q��@Q��@Q��@Q��@Q��@Q�7@Qx�@Q%@P�@PA�@O��@O�@N��@NE�@M�T@M�-@M��@M�@M�@L��@K�
@Kt�@I��@I%@H�@G�@G�P@G\)@G�@FV@E��@D�@DZ@Cƨ@C�@CdZ@C@B^5@A��@A��@Ahs@A�@@  @?+@>��@>ȴ@>�+@>V@=�@=��@=�h@<�j@<�D@<�D@<z�@<I�@<�@<1@<�@;��@;��@;S�@;C�@:�!@:�\@:-@:J@:J@9��@9�#@9��@9�^@9�^@9�^@9�^@9hs@8�`@8��@8Ĝ@8�u@7l�@6��@6ff@4I�@17L@/�;@.��@.ȴ@.��@.��@.��@.��@.v�@.ff@.5?@-�T@-�-@-�h@-�h@-�h@-��@-�-@-�-@-�-@-�-@-@-@-�h@-�@-�@-�@-`B@-O�@-?}@-O�@-V@,�j@,�j@,��@,�D@,�D@,�D@,z�@,Z@,9X@,9X@,(�@+dZ@+o@*�\@*~�@*^5@*n�@*J@*-@)��@(��@(��@(�`@(�`@(�`@(�`@(��@(��@(�`@(��@)%@)G�@)�@(�`@(�9@(bN@(r�@(�u@(r�@(�9@(�9@(�u@(��@(r�@(A�@(b@(1'@'�w@%V@#�@#C�@#o@"��@"n�@"M�@"�@!��@!G�@!�@ ��@ �u@ bN@�;@�P@K�@��@E�@�/@�T@E�@O�@"�@33@o@o@@�!@n�@n�@n�@M�@=q@��@�^@hs@��@E�@5?@ff@$�@{@�@�@�T@v�@E�@5?@@�+@��@`B@�T@v�@�+@V@ff@�+@�@�y@�@
=@��@��@�T@��@@��@�-@�h@�-@�-@@��@@�T@�h@��@�-@�-@@��@�@�@�@�T@�@�T@@`B@/@�@?}@`B@?}@?}@/@?}@/@�@V@V@�j@z�@�D@(�@(�@(�@(�@(�@9X@(�@�@�D@j@��@�j@V@?}@V@�@�/@V@V@�@z�@I�@9X@9X@9X@(�@��@��@�
@�m@��@�m@ƨ@��@��@��@��@��@�@dZ@dZ@dZ@S�@��@n�@n�@=q@�@�7@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	��B	�
B
$�B
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
2-B
33B
33B
33B
33B
33B
49B
49B
5?B
5?B
49B
49B
49B
49B
33B
2-B
0!B
1'B
2-B
2-B
2-B
2-B
1'B
1'B
0!B
0!B
/B
.B
.B
,B
(�B
%�B
$�B
�B
1B	��B	�B
uB
2-B
l�B
r�B
�B
�B
�)B
��BVB&�B.BF�B[#Bk�Bo�B�B�bB��B��B��B��B��B��B��B��B�{B�DB~�Bp�BgmBaHBW
BO�BG�BD�B?}B<jB;dB:^B:^B8RB8RB6FB5?B49B-BoB
��B
�;B
�B
�bB
�7B
~�B
s�B
k�B
bNB
L�B
,B	�HB	�B	��B	��B	�-B	��B	m�B	dZB	_;B	YB	O�B	I�B	?}B	7LB	(�B	%�B	"�B	�B	�B	�B	PB		7B	+B	B��B��B��B�B�B�B�yB�yB�yB�yB�yB�mB�`B�NB�HB�)B�)B�BB�NB�fB�B�B�B�B�B�B�B�yB�B�B�B�B�B�sB�`B�`B�`B�`B�mB�sB�sB�mB�sB�B�B	B	\B	uB	uB	uB	\B	\B	VB	bB	hB	{B	�B	�B	�B	�B	�B	{B	hB	JB	
=B	+B		7B	DB	DB	JB	PB	PB	\B	B��B��B��B	  B	B��B�B�B�B�B�B�B�B�B�B�B�B��B	+B		7B	DB	hB	oB	�B	�B	%�B	)�B	)�B	+B	-B	.B	/B	2-B	5?B	7LB	9XB	;dB	>wB	@�B	A�B	B�B	C�B	D�B	E�B	J�B	P�B	Q�B	P�B	P�B	Q�B	R�B	R�B	T�B	W
B	\)B	]/B	^5B	_;B	`BB	`BB	`BB	`BB	`BB	aHB	bNB	cTB	dZB	gmB	jB	q�B	r�B	t�B	w�B	y�B	y�B	y�B	y�B	z�B	z�B	{�B	}�B	|�B	}�B	}�B	~�B	�B	�B	�%B	�+B	�7B	�JB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�RB	�RB	�RB	�RB	�XB	�^B	�jB	�wB	�wB	B	B	ĜB	ĜB	ĜB	ŢB	ƨB	ǮB	ȴB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�/B	�5B	�5B	�;B	�BB	�HB	�NB	�NB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
1B

=B

=B
JB
JB
DB
DB
DB
DB
DB
DB
DB
JB
JB
VB
VB
VB
VB
VB
VB
VB
VB
\B
hB
hB
hB
hB
oB
oB
oB
uB
oB
hB
oB
uB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
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
#�B
#�B
#�B
#�B
#�B
$�B
%�B
'�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
+B
+B
)�B
+B
,B
,B
,B
,B
,B
-B
,B
,B
,B
-B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
0!B
0!B
1'B
1'B
1'B
1'B
0!B
1'B
2-B
33B
33B
33B
33B
33B
33B
33B
49B
49B
49B
49B
49B
49B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
7LB
;dB
;dB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
D�B
E�B
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
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
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
W
B
W
B
XB
XB
YB
YB
YB
YB
YB
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
[#B
[#B
[#B
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
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
^5B
aHB
cTB
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
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
gmB
ffB
ffB
gmB
gmB
gmB
gmB
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
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
iyB
jB
jB
iyB
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
k�B
k�B
k�B
k�B
k�B
l�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
r�B
s�B
u�B
s�B
t�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
v�B
w�B
v�B
w�B
w�B
w�B
x�B
z�B
z�B
z�B
{�B
z�B
z�B
{�B
{�B
z�B
z�B
z�B
z�B
y�B
{�B
|�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
y�B
y�B
y�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
{�B
{�B
|�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
|�B
}�B
}�B
~�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
�B
�o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	��B	�
B
$�B
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
2-B
33B
33B
33B
33B
33B
49B
49B
5?B
5?B
49B
49B
49B
49B
33B
2-B
0!B
1'B
2-B
2-B
2-B
2-B
1'B
1'B
0!B
0!B
/B
.B
.B
,B
(�B
%�B
$�B
�B
1B	��B	�B
uB
2-B
l�B
r�B
�B
�B
�)B
��BVB&�B.BF�B[#Bk�Bo�B�B�bB��B��B��B��B��B��B��B��B�{B�DB~�Bp�BgmBaHBW
BO�BG�BD�B?}B<jB;dB:^B:^B8RB8RB6FB5?B49B-BoB
��B
�;B
�B
�bB
�7B
~�B
s�B
k�B
bNB
L�B
,B	�HB	�B	��B	��B	�-B	��B	m�B	dZB	_;B	YB	O�B	I�B	?}B	7LB	(�B	%�B	"�B	�B	�B	�B	PB		7B	+B	B��B��B��B�B�B�B�yB�yB�yB�yB�yB�mB�`B�NB�HB�)B�)B�BB�NB�fB�B�B�B�B�B�B�B�yB�B�B�B�B�B�sB�`B�`B�`B�`B�mB�sB�sB�mB�sB�B�B	B	\B	uB	uB	uB	\B	\B	VB	bB	hB	{B	�B	�B	�B	�B	�B	{B	hB	JB	
=B	+B		7B	DB	DB	JB	PB	PB	\B	B��B��B��B	  B	B��B�B�B�B�B�B�B�B�B�B�B�B��B	+B		7B	DB	hB	oB	�B	�B	%�B	)�B	)�B	+B	-B	.B	/B	2-B	5?B	7LB	9XB	;dB	>wB	@�B	A�B	B�B	C�B	D�B	E�B	J�B	P�B	Q�B	P�B	P�B	Q�B	R�B	R�B	T�B	W
B	\)B	]/B	^5B	_;B	`BB	`BB	`BB	`BB	`BB	aHB	bNB	cTB	dZB	gmB	jB	q�B	r�B	t�B	w�B	y�B	y�B	y�B	y�B	z�B	z�B	{�B	}�B	|�B	}�B	}�B	~�B	�B	�B	�%B	�+B	�7B	�JB	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�-B	�3B	�RB	�RB	�RB	�RB	�XB	�^B	�jB	�wB	�wB	B	B	ĜB	ĜB	ĜB	ŢB	ƨB	ǮB	ȴB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�/B	�5B	�5B	�;B	�BB	�HB	�NB	�NB	�fB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
%B
%B
+B
+B
1B
1B
1B

=B

=B
JB
JB
DB
DB
DB
DB
DB
DB
DB
JB
JB
VB
VB
VB
VB
VB
VB
VB
VB
\B
hB
hB
hB
hB
oB
oB
oB
uB
oB
hB
oB
uB
oB
uB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
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
#�B
#�B
#�B
#�B
#�B
$�B
%�B
'�B
'�B
'�B
'�B
'�B
(�B
)�B
)�B
)�B
+B
+B
)�B
+B
,B
,B
,B
,B
,B
-B
,B
,B
,B
-B
/B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
0!B
1'B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
1'B
0!B
0!B
1'B
1'B
1'B
1'B
0!B
1'B
2-B
33B
33B
33B
33B
33B
33B
33B
49B
49B
49B
49B
49B
49B
5?B
6FB
6FB
7LB
7LB
7LB
7LB
7LB
;dB
;dB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
>wB
>wB
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
D�B
E�B
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
I�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
T�B
T�B
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
W
B
W
B
XB
XB
YB
YB
YB
YB
YB
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
[#B
[#B
[#B
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
]/B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
^5B
aHB
cTB
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
gmB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
ffB
gmB
ffB
ffB
gmB
gmB
gmB
gmB
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
hsB
hsB
hsB
hsB
iyB
iyB
iyB
jB
iyB
jB
jB
iyB
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
k�B
k�B
k�B
k�B
k�B
l�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
s�B
r�B
s�B
u�B
s�B
t�B
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
v�B
w�B
v�B
w�B
w�B
w�B
x�B
z�B
z�B
z�B
{�B
z�B
z�B
{�B
{�B
z�B
z�B
z�B
z�B
y�B
{�B
|�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
y�B
y�B
y�B
z�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
{�B
{�B
|�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
|�B
}�B
}�B
~�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~�B
}�B
}�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
�B
�o111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20190915183720  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190915093822  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190915093822  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190915093823  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190915093824  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190915093824  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190915093824  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190915093824  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190915093824  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190915093824                      G�O�G�O�G�O�                JA  ARUP                                                                        20190915095404                      G�O�G�O�G�O�                