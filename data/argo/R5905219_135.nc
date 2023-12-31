CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2021-12-22T03:42:59Z creation;2021-12-22T03:43:01Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \l   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `P   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �|   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ܬ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ߬   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �(   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �<   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �L   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �T   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �XArgo profile    3.1 1.2 19500101000000  20211222034259  20211222035248  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            7906                            051216                          846 @٪޳��1   @٪��q�@4>vȴ9X�dX�9Xb1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�33@�  A   A   A>ffA`  A�  A�  A�  A�  A�33A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B�33B�33B���B�  B�  B���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C  C  C�C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CU�fCX  CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� DfD� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D��D y�D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D(��D)� D*  D*� D+  D+�fD,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1�fD2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD�fDEfDE�fDF  DF� DG  DG� DH  DH� DI  DI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DP��DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DYy�DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`�fDa  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dm��Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Dsy�Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dw��Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}y�D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D��3D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�<�D�|�D�� D�  D�@ D̀ D�� D�  D�@ D̀ Dͼ�D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ D�|�D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�<�DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D���D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�C3Dހ D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D��D���D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�P D�` 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�z�A=qA&=qAD��Af=qA��A��A��A��A�Q�A��A��A��B�\B	�\B�\B�\B!�\B)�\B1�\B9�\BA�\BI�\BQ�\BY�\Ba�\Bi�\Bq�\By�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB���B���B���B���B���B��{B�ǮB�ǮB��{B�ǮBȔ{B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC c�Cc�Cc�Cc�Cc�C
c�Cc�C}qCc�Cc�C}qCc�Cc�Cc�Cc�Cc�C c�C"c�C$c�C&c�C(c�C*c�C,c�C.c�C0c�C2c�C4c�C6c�C8c�C:c�C<c�C>c�C@c�CBc�CDc�CFc�CHc�CJc�CLc�CNc�CPc�CRc�CTc�CVJ=CXc�CZ}qC\c�C^c�C`c�Cbc�Cdc�Cfc�Chc�Cjc�Clc�Cnc�Cp}qCrc�Ctc�Cvc�Cxc�Czc�C|c�C~c�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�%C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�>�C�>�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�>�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�%C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D\D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D�\D\D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+�\D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1�\D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD�\DE\DE�\DF�DF��DG�DG��DH�DH��DI�DI�\DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`�\Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�O�D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D�ϮD��D�L{D��{D��{D�{D�L{D��{D��{D��D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�IHD��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D���D�ϮD�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�O�D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D�ϮD�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��HD��HD�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�O�D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D���D�ϮD�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��HD��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��HD�{D�L{D���D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D{D��{D�{D�L{DÌ{D��{D�{D�L{DČ{D��{D�{D�L{DŌ{D��{D�{D�L{Dƌ{D��{D�{D�L{Dǌ{D��{D�{D�L{DȌ{D�ϮD�{D�L{DɌ{D��{D�{D�L{Dʌ{D��{D�{D�IHDˉHD��{D�{D�L{Ď{D��{D�{D�L{D͌{D��HD�{D�L{DΌ{D��{D�{D�L{Dό{D��{D�{D�L{DЉHD��{D�{D�L{Dь{D��{D�{D�L{DҌ{D��{D�{D�L{Dӌ{D��{D�{D�IHDԌ{D��{D�{D�L{DՌ{D��{D�{D�L{D֌{D��{D�	HD�L{D׌{D��{D�{D�L{D،{D��{D�{D�L{Dٌ{D��{D�{D�L{Dڌ{D��{D�{D�L{Dی{D��{D�{D�L{D܌{D��{D�{D�L{D݌{D��{D�{D�O�Dތ{D��{D�{D�L{D߉HD��{D�{D�L{D��{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�O�D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�O�D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{DD��{D�{D�L{D�{D��HD�	HD�L{D��{D��{D�{D�L{D�{D��{D�{D�L{D�D��{D��D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�\{D�l{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A�ĜA���A�A�ĜA�ĜA�ȴA�ȴA���A���A���A�ȴA�ƨA���A���A��A��HA��TA��;A���A�ĜA�ȴA���A���AП�AБhAЃA�~�AЁAЇ+AЇ+AЗ�A��A�A�A��yA���A�I�A�JAϾwAϑhA�E�A��AͲ-A�I�A�bA�&�A��;AĮA��#A�E�A�A���A�\)A�5?A��`A��FA��A��A�
=A�~�A��A��yA�C�A�1A�~�A�ƨA��A��HA���A�p�A��A�=qA�ZA��\A��9A�A�VA���A�A�A���A���A�VA�E�A��A��PA���A��A�jA��A��A��A�K�A���A��A��jA�bA�v�A�/A�ffA�1'A�?}A�C�A���A���A��mA�E�A�A�hsA�#A|�9A|5?A{t�Axv�Av��Au�-At��Ap�jAmC�Ak?}Aj�!Ai�Ah�uAd=qAb�RAa\)A_��A^(�A]VAX��AU�^AS�hAQ��AP��AP$�AO�7AMC�AK��AK\)AK"�AJn�AI%AG�^AE��AC�AA
=A@ �A>VA=33A<�jA:��A8$�A7��A7�PA7XA6~�A5G�A4A3"�A2�A0�!A/�-A.�DA-�wA-�A,��A, �A+�A*r�A(9XA'
=A%hsA#&�A ��A $�A�jA\)A�RAbA�A$�A�TA��A�!Az�AVA��AdZA��A1A��An�A&�AA�TA
ĜA
1'Ar�AXA��A1'A�mA��A"�A%A��A��A`BA��A1At�A�A �!@�K�@�O�@�t�@���@���@�G�@��`@�b@�M�@�o@�@�^5@� �@��@�/@�ƨ@�v�@�^@�G�@���@�w@�
=@�^5@��T@�O�@���@ް!@���@�`B@��`@��`@ܣ�@�bN@��@���@��@�-@�@ԣ�@�b@�|�@�@҇+@�&�@�A�@ϥ�@�=q@˝�@˅@�l�@�\)@�|�@ȣ�@�+@ư!@��@š�@š�@ŉ7@��@\@�Z@��@�|�@�(�@��
@�l�@���@���@���@�5?@��^@�`B@�O�@��`@�V@�j@��F@�\)@��@�^5@��^@�Q�@�|�@���@��@�z�@���@��@��`@��@���@�S�@��^@���@��T@��#@�5?@�
=@�ff@�G�@�V@�V@�G�@��@���@��j@���@�@��R@��@�  @���@�v�@�-@���@��@�=q@���@��@���@�E�@�t�@�@�ff@�=q@�p�@�X@�(�@���@���@��F@���@�C�@���@���@��@�n�@��h@���@���@�@��@�$�@���@���@���@�5?@���@�@���@�hs@�`B@�`B@�X@���@��@�A�@��@���@�1@��@��@��@�;d@���@�5?@�p�@�7L@��@��/@��@�1'@���@�S�@���@�{@���@�1'@��@�ƨ@��T@�7L@�?}@�7L@��9@�b@�ƨ@��@��@���@�E�@��@�@�@��7@���@�$�@�v�@�ȴ@���@��@���@���@��+@��@���@��@�ȴ@��R@�^5@�=q@�-@�`B@��@�z�@�ƨ@��@�S�@�+@�o@��@��\@�^5@�M�@��@��@��#@���@��-@��@���@��D@�bN@�1'@�b@�1@�  @���@�33@��H@��+@�V@�M�@�=q@�$�@�@��#@��7@�`B@�%@���@�Z@�Q�@�Q�@��@��@���@�C�@�"�@�@��y@���@��!@�~�@���@���@���@�X@���@��@� �@��m@��P@�dZ@�K�@�"�@��@���@���@���@�~�@�E�@�$�@�{@�J@���@���@���@�p�@�O�@�?}@��@���@�bN@�I�@�9X@�1@�w@|�@~�R@}@}O�@|��@|j@{�
@{�@{@zM�@y�@y��@y�@x��@x��@x�u@x �@w��@wK�@v�@v$�@u@u�h@u?}@t�D@sƨ@st�@s"�@r��@rM�@rJ@q��@q�#@q�7@p�`@p��@p�u@pQ�@pb@o��@o|�@o;d@n��@nff@nV@n@m�h@m�h@m/@l�/@lj@l9X@l(�@l1@k�m@k�@kC�@j��@j-@ihs@i%@h�`@h�9@h�@h1'@g�@g�w@f�R@f5?@f{@e@eO�@d�@d�D@dj@dZ@d9X@d1@c�F@c�@b�\@b=q@b=q@b=q@a��@a��@ahs@a7L@`�`@`bN@`b@`b@`b@_��@_|�@_K�@_
=@^�@^�R@^��@^@]�h@]O�@\�@\(�@[��@[��@[��@[dZ@[33@[@Z�@Z��@Z~�@ZJ@Yx�@Y�@X��@XĜ@X�9@XbN@W��@W\)@W\)@W\)@WK�@W�@V�+@V5?@V5?@V$�@U�@U�h@U/@T��@T�j@T�j@T��@TZ@S��@SS�@S33@R��@RM�@Q�#@QX@Q�@P�`@PA�@O�@O�@Ol�@OK�@O
=@N�R@Nv�@N5?@M@M�h@M�@M?}@L��@Lz�@L9X@L�@K��@KdZ@J��@Jn�@J�@I��@I�^@I��@Ix�@I7L@H�9@Hr�@HA�@Hb@G��@G�w@G|�@G�@F�R@F�+@Fff@F$�@E@E��@E?}@D��@D�@D(�@C��@C�
@C��@Ct�@C33@B��@B~�@B^5@B-@A��@A��@Ax�@A�@@�9@@�@@A�@@ �@?�;@?�w@?�@?|�@?\)@?�@>ȴ@>ff@>{@>@=�T@=�-@=�@=?}@<�@<�@<�D@<Z@;�m@;��@;dZ@;S�@;C�@:�@:�!@:n�@:=q@:�@:J@9�^@9��@9&�@8��@8Ĝ@8�9@8�u@8bN@81'@8  @7�@7|�@7�@6�R@6��@6v�@6@5��@5@5��@5p�@5?}@4�/@4�@4z�@4Z@41@3ƨ@3�F@3��@3t�@3dZ@3C�@3@2��@2��@2~�@2-@1��@1��@1�7@1x�@1hs@17L@0�`@0bN@01'@0b@/��@/��@/K�@.�@.��@.V@.@-�T@-@-p�@-V@,�@,�j@,z�@,9X@,1@+��@+dZ@+o@*��@*^5@*-@)��@)��@)��@)hs@)G�@)7L@)�@)%@(�u@(1'@'�;@'\)@'+@'+@'+@'
=@&�y@&�R@&v�@%��@%�-@%��@%?}@$��@$�D@$1@#�m@#�
@#��@#S�@#33@#o@"�@"��@"~�@"-@"�@!�@!��@!�7@!&�@ �`@ ��@ �@ Q�@   @�@�@��@�P@|�@|�@l�@;d@;d@+@
=@��@E�@$�@�T@p�@O�@��@�j@�D@I�@(�@�
@��@t�@C�@�@��@��@�\@~�@-@�@��@x�@7L@�@�@Ĝ@A�@1'@1'@ �@�;@|�@;d@�R@�+@E�@$�@{@�@��@��@�@`B@/@�@��@�/@�@�D@�D@I�@�@�
@dZ@33@@�H@�!@�\@-@�@�#@�^@hs@7L@&�@�@��@�`@Ĝ@�9@�@Q�@1'@b@�@��@�w@|�@l�@K�@+@��@�@ȴ@�R@�R@��@��@�+@ff@V@{@�@�@��@�h@O�@/@V@��@��@��@��@z�@I�@9X@(�@1@��@�m@�
@�
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A���A�ĜA���A�A�ĜA�ĜA�ȴA�ȴA���A���A���A�ȴA�ƨA���A���A��A��HA��TA��;A���A�ĜA�ȴA���A���AП�AБhAЃA�~�AЁAЇ+AЇ+AЗ�A��A�A�A��yA���A�I�A�JAϾwAϑhA�E�A��AͲ-A�I�A�bA�&�A��;AĮA��#A�E�A�A���A�\)A�5?A��`A��FA��A��A�
=A�~�A��A��yA�C�A�1A�~�A�ƨA��A��HA���A�p�A��A�=qA�ZA��\A��9A�A�VA���A�A�A���A���A�VA�E�A��A��PA���A��A�jA��A��A��A�K�A���A��A��jA�bA�v�A�/A�ffA�1'A�?}A�C�A���A���A��mA�E�A�A�hsA�#A|�9A|5?A{t�Axv�Av��Au�-At��Ap�jAmC�Ak?}Aj�!Ai�Ah�uAd=qAb�RAa\)A_��A^(�A]VAX��AU�^AS�hAQ��AP��AP$�AO�7AMC�AK��AK\)AK"�AJn�AI%AG�^AE��AC�AA
=A@ �A>VA=33A<�jA:��A8$�A7��A7�PA7XA6~�A5G�A4A3"�A2�A0�!A/�-A.�DA-�wA-�A,��A, �A+�A*r�A(9XA'
=A%hsA#&�A ��A $�A�jA\)A�RAbA�A$�A�TA��A�!Az�AVA��AdZA��A1A��An�A&�AA�TA
ĜA
1'Ar�AXA��A1'A�mA��A"�A%A��A��A`BA��A1At�A�A �!@�K�@�O�@�t�@���@���@�G�@��`@�b@�M�@�o@�@�^5@� �@��@�/@�ƨ@�v�@�^@�G�@���@�w@�
=@�^5@��T@�O�@���@ް!@���@�`B@��`@��`@ܣ�@�bN@��@���@��@�-@�@ԣ�@�b@�|�@�@҇+@�&�@�A�@ϥ�@�=q@˝�@˅@�l�@�\)@�|�@ȣ�@�+@ư!@��@š�@š�@ŉ7@��@\@�Z@��@�|�@�(�@��
@�l�@���@���@���@�5?@��^@�`B@�O�@��`@�V@�j@��F@�\)@��@�^5@��^@�Q�@�|�@���@��@�z�@���@��@��`@��@���@�S�@��^@���@��T@��#@�5?@�
=@�ff@�G�@�V@�V@�G�@��@���@��j@���@�@��R@��@�  @���@�v�@�-@���@��@�=q@���@��@���@�E�@�t�@�@�ff@�=q@�p�@�X@�(�@���@���@��F@���@�C�@���@���@��@�n�@��h@���@���@�@��@�$�@���@���@���@�5?@���@�@���@�hs@�`B@�`B@�X@���@��@�A�@��@���@�1@��@��@��@�;d@���@�5?@�p�@�7L@��@��/@��@�1'@���@�S�@���@�{@���@�1'@��@�ƨ@��T@�7L@�?}@�7L@��9@�b@�ƨ@��@��@���@�E�@��@�@�@��7@���@�$�@�v�@�ȴ@���@��@���@���@��+@��@���@��@�ȴ@��R@�^5@�=q@�-@�`B@��@�z�@�ƨ@��@�S�@�+@�o@��@��\@�^5@�M�@��@��@��#@���@��-@��@���@��D@�bN@�1'@�b@�1@�  @���@�33@��H@��+@�V@�M�@�=q@�$�@�@��#@��7@�`B@�%@���@�Z@�Q�@�Q�@��@��@���@�C�@�"�@�@��y@���@��!@�~�@���@���@���@�X@���@��@� �@��m@��P@�dZ@�K�@�"�@��@���@���@���@�~�@�E�@�$�@�{@�J@���@���@���@�p�@�O�@�?}@��@���@�bN@�I�@�9X@�1@�w@|�@~�R@}@}O�@|��@|j@{�
@{�@{@zM�@y�@y��@y�@x��@x��@x�u@x �@w��@wK�@v�@v$�@u@u�h@u?}@t�D@sƨ@st�@s"�@r��@rM�@rJ@q��@q�#@q�7@p�`@p��@p�u@pQ�@pb@o��@o|�@o;d@n��@nff@nV@n@m�h@m�h@m/@l�/@lj@l9X@l(�@l1@k�m@k�@kC�@j��@j-@ihs@i%@h�`@h�9@h�@h1'@g�@g�w@f�R@f5?@f{@e@eO�@d�@d�D@dj@dZ@d9X@d1@c�F@c�@b�\@b=q@b=q@b=q@a��@a��@ahs@a7L@`�`@`bN@`b@`b@`b@_��@_|�@_K�@_
=@^�@^�R@^��@^@]�h@]O�@\�@\(�@[��@[��@[��@[dZ@[33@[@Z�@Z��@Z~�@ZJ@Yx�@Y�@X��@XĜ@X�9@XbN@W��@W\)@W\)@W\)@WK�@W�@V�+@V5?@V5?@V$�@U�@U�h@U/@T��@T�j@T�j@T��@TZ@S��@SS�@S33@R��@RM�@Q�#@QX@Q�@P�`@PA�@O�@O�@Ol�@OK�@O
=@N�R@Nv�@N5?@M@M�h@M�@M?}@L��@Lz�@L9X@L�@K��@KdZ@J��@Jn�@J�@I��@I�^@I��@Ix�@I7L@H�9@Hr�@HA�@Hb@G��@G�w@G|�@G�@F�R@F�+@Fff@F$�@E@E��@E?}@D��@D�@D(�@C��@C�
@C��@Ct�@C33@B��@B~�@B^5@B-@A��@A��@Ax�@A�@@�9@@�@@A�@@ �@?�;@?�w@?�@?|�@?\)@?�@>ȴ@>ff@>{@>@=�T@=�-@=�@=?}@<�@<�@<�D@<Z@;�m@;��@;dZ@;S�@;C�@:�@:�!@:n�@:=q@:�@:J@9�^@9��@9&�@8��@8Ĝ@8�9@8�u@8bN@81'@8  @7�@7|�@7�@6�R@6��@6v�@6@5��@5@5��@5p�@5?}@4�/@4�@4z�@4Z@41@3ƨ@3�F@3��@3t�@3dZ@3C�@3@2��@2��@2~�@2-@1��@1��@1�7@1x�@1hs@17L@0�`@0bN@01'@0b@/��@/��@/K�@.�@.��@.V@.@-�T@-@-p�@-V@,�@,�j@,z�@,9X@,1@+��@+dZ@+o@*��@*^5@*-@)��@)��@)��@)hs@)G�@)7L@)�@)%@(�u@(1'@'�;@'\)@'+@'+@'+@'
=@&�y@&�R@&v�@%��@%�-@%��@%?}@$��@$�D@$1@#�m@#�
@#��@#S�@#33@#o@"�@"��@"~�@"-@"�@!�@!��@!�7@!&�@ �`@ ��@ �@ Q�@   @�@�@��@�P@|�@|�@l�@;d@;d@+@
=@��@E�@$�@�T@p�@O�@��@�j@�D@I�@(�@�
@��@t�@C�@�@��@��@�\@~�@-@�@��@x�@7L@�@�@Ĝ@A�@1'@1'@ �@�;@|�@;d@�R@�+@E�@$�@{@�@��@��@�@`B@/@�@��@�/@�@�D@�D@I�@�@�
@dZ@33@@�H@�!@�\@-@�@�#@�^@hs@7L@&�@�@��@�`@Ĝ@�9@�@Q�@1'@b@�@��@�w@|�@l�@K�@+@��@�@ȴ@�R@�R@��@��@�+@ff@V@{@�@�@��@�h@O�@/@V@��@��@��@��@z�@I�@9X@(�@1@��@�m@�
@�
111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B#�B.B1'B49B9XB:^B>wBL�BS�BYBl�Bm�Bk�Bn�Bu�Bx�Bz�B|�B� B�=B��B�B�BB�ZB��BBPBhB�B�B�B'�B+B-B33B0!B%�B�B!�B�B�B)�B1'B;dBT�B`BB_;B_;B]/B\)B[#B_;BbNB`BBXBaHBdZB\)B]/B]/BZBVB^5BZBVBQ�BO�BK�BC�B;dB33B$�BhB��B�#B�RB��B�PBt�B_;BH�B9XB49B,B"�B\B  B
�B
�B
ĜB
�LB
��B
��B
�JB
v�B
k�B
_;B
D�B
<jB
8RB
&�B
�B
oB

=B	��B	�HB	��B	��B	ŢB	�}B	�B	��B	��B	�bB	�B	{�B	k�B	^5B	P�B	C�B	=qB	7LB	5?B	/B	$�B	!�B	 �B	�B	�B	uB	\B	JB	B	B��B��B��B�B�fB�TB�NB�HB�;B�)B�
B��B��B��BȴBŢBÖBB��B�}B�dB�XB�?B�!B�B��B��B��B��B��B��B��B��B�uB�oB�uB�bB�\B�\B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�'B�B�B�B�3B�9B�FB�3B�3B�wB�dB�XB�^B�}B�wB�qB�qBÖB��B��B��B��B��B��B��B��B��B��B�
B�B�#B�#B�)B�)B�#B�#B�)B�5B�ZB�mB�yB�B�B�B�B��B�B��B��B��B��B��B��B��B��B��B��B��B	B	%B	+B	
=B	PB	�B	�B	�B	 �B	#�B	)�B	)�B	,B	+B	+B	)�B	,B	0!B	1'B	2-B	9XB	:^B	9XB	6FB	6FB	2-B	0!B	-B	/B	:^B	>wB	<jB	<jB	;dB	8RB	7LB	?}B	I�B	M�B	XB	\)B	[#B	[#B	\)B	bNB	dZB	k�B	n�B	q�B	t�B	u�B	t�B	n�B	iyB	hsB	jB	jB	k�B	k�B	p�B	r�B	w�B	�B	�JB	�hB	�bB	��B	��B	�{B	�{B	�oB	�uB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�?B	�LB	�^B	��B	ĜB	ƨB	ȴB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�;B	�BB	�HB	�NB	�TB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�yB	�mB	�`B	�`B	�`B	�`B	�ZB	�ZB	�ZB	�`B	�mB	�sB	�sB	�sB	�sB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
B
%B
+B
+B
+B
1B
1B
1B
	7B

=B

=B
DB
JB
PB
PB
PB
VB
\B
\B
\B
bB
bB
bB
bB
bB
oB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
&�B
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
)�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
,B
-B
.B
.B
.B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
49B
5?B
5?B
5?B
5?B
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
8RB
8RB
8RB
9XB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
9XB
:^B
9XB
:^B
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
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
@�B
A�B
A�B
A�B
A�B
@�B
@�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
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
M�B
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
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
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
XB
W
B
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
YB
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
\)B
\)B
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
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
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
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
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
gmB
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
jB
jB
jB
jB
jB
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
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
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
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
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
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B�B�B�B�B�B�B�B�B�B�B�B#�B.B1'B49B9XB:^B>wBL�BS�BYBl�Bm�Bk�Bn�Bu�Bx�Bz�B|�B� B�=B��B�B�BB�ZB��BBPBhB�B�B�B'�B+B-B33B0!B%�B�B!�B�B�B)�B1'B;dBT�B`BB_;B_;B]/B\)B[#B_;BbNB`BBXBaHBdZB\)B]/B]/BZBVB^5BZBVBQ�BO�BK�BC�B;dB33B$�BhB��B�#B�RB��B�PBt�B_;BH�B9XB49B,B"�B\B  B
�B
�B
ĜB
�LB
��B
��B
�JB
v�B
k�B
_;B
D�B
<jB
8RB
&�B
�B
oB

=B	��B	�HB	��B	��B	ŢB	�}B	�B	��B	��B	�bB	�B	{�B	k�B	^5B	P�B	C�B	=qB	7LB	5?B	/B	$�B	!�B	 �B	�B	�B	uB	\B	JB	B	B��B��B��B�B�fB�TB�NB�HB�;B�)B�
B��B��B��BȴBŢBÖBB��B�}B�dB�XB�?B�!B�B��B��B��B��B��B��B��B��B�uB�oB�uB�bB�\B�\B�hB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�'B�'B�B�B�B�3B�9B�FB�3B�3B�wB�dB�XB�^B�}B�wB�qB�qBÖB��B��B��B��B��B��B��B��B��B��B�
B�B�#B�#B�)B�)B�#B�#B�)B�5B�ZB�mB�yB�B�B�B�B��B�B��B��B��B��B��B��B��B��B��B��B��B	B	%B	+B	
=B	PB	�B	�B	�B	 �B	#�B	)�B	)�B	,B	+B	+B	)�B	,B	0!B	1'B	2-B	9XB	:^B	9XB	6FB	6FB	2-B	0!B	-B	/B	:^B	>wB	<jB	<jB	;dB	8RB	7LB	?}B	I�B	M�B	XB	\)B	[#B	[#B	\)B	bNB	dZB	k�B	n�B	q�B	t�B	u�B	t�B	n�B	iyB	hsB	jB	jB	k�B	k�B	p�B	r�B	w�B	�B	�JB	�hB	�bB	��B	��B	�{B	�{B	�oB	�uB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�?B	�LB	�^B	��B	ĜB	ƨB	ȴB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�)B	�;B	�BB	�HB	�NB	�TB	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�yB	�yB	�mB	�`B	�`B	�`B	�`B	�ZB	�ZB	�ZB	�`B	�mB	�sB	�sB	�sB	�sB	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B
B
%B
+B
+B
+B
1B
1B
1B
	7B

=B

=B
DB
JB
PB
PB
PB
VB
\B
\B
\B
bB
bB
bB
bB
bB
oB
uB
uB
uB
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
"�B
#�B
#�B
$�B
%�B
%�B
%�B
%�B
&�B
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
)�B
)�B
)�B
)�B
+B
+B
+B
,B
,B
,B
-B
.B
.B
.B
/B
/B
/B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
49B
5?B
5?B
5?B
5?B
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
8RB
8RB
8RB
9XB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
9XB
:^B
9XB
:^B
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
;dB
;dB
;dB
;dB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
@�B
A�B
A�B
A�B
A�B
@�B
@�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
F�B
F�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
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
M�B
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
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
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
XB
W
B
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
YB
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
\)B
\)B
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
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
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
dZB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
e`B
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
gmB
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
jB
jB
jB
jB
jB
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
l�B
l�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v�B
v�B
v�B
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
x�B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
{�B
{�B
{�B
{�B
{�B
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
~�B
~�B
~�B
~�B
~�B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�DB
�D111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20211222124132  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20211222034259  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20211222034300  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20211222034300  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20211222034301  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20211222034301  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20211222034301  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20211222034301  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20211222034301  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20211222034301                      G�O�G�O�G�O�                JA  ARUP                                                                        20211222035248                      G�O�G�O�G�O�                