CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2021-01-18T00:40:10Z creation;2021-01-18T00:40:12Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �T   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �8   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �H   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �P   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210118004010  20210118005246  5905219                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               eA   JA                                  2B  A   APEX                            7906                            051216                          846 @�Wr"�91   @�Wr��/h@4T��E��d�z�G�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�33B   B  B  BffB   B(  B0  B8ffB@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C �C33C  C�fC  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Cg�fCi�fCl  Cn  Cp�Cr  Ct  Cv  Cx  Cz  C|  C~  C��C��C�  C�  C�  C��C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3D � D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D��D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=�fD>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DL��DM� DN  DN� DOfDO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^fD^� D_  D_� D`  D`� Da  Da� Da��Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DufDu� Dv  Dv� Dw  Dw� Dx  Dx�fDy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�3D�@ Dŀ Dż�D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D��3D�3D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D���D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�C3Dك3D�� D�  D�@ Dڀ D�� D�  D�@ D�|�D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�,�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@�z�A=qA&=qAF=qAf=qA��A��A��A��A��A��A��A�Q�B�\B	�\B�\B��B!�\B)�\B1�\B9��BA�\BI�\BQ�\BY�\Ba�\Bi�\Bq�\By�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC }qC�
Cc�CJ=Cc�C
c�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�Cc�C c�C"c�C$c�C&c�C(c�C*c�C,c�C.c�C0c�C2c�C4c�C6c�C8c�C:c�C<c�C>c�C@c�CBc�CDc�CFc�CHc�CJc�CLc�CNc�CPc�CRc�CTc�CVc�CXc�CZc�C\c�C^c�C`c�Cbc�Cdc�Cfc�ChJ=CjJ=Clc�Cnc�Cp}qCrc�Ctc�Cvc�Cxc�Czc�C|c�C~c�C�>�C�>�C�1�C�1�C�1�C�>�C�1�C�1�C�1�C�>�C�>�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�>�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�%C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�>�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�%C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�%C�%C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�C�1�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D\D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=�\D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO\DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^\D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du\Du��Dv�Dv��Dw�Dw��Dx�Dx�\Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�{D�L{D��{D��{D�{D�O�D���D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��HD��{D�{D�L{D��{D��{D�{D�L{D���D�ϮD�{D�IHD��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�O�D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D�ϮD��D�L{D��{D�ϮD�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D�ϮD�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D���D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��HD�	HD�L{D���D�ϮD�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�	HD�IHD��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�O�D��{D��HD�	HD�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D{D��{D�{D�L{DÌ{D��{D�{D�L{DČ{D��{D��D�L{DŌ{D��HD�{D�L{Dƌ{D��{D�{D�L{Dǌ{D��{D�{D�L{DȌ{D��{D�{D�L{DɌ{D��{D�{D�L{Dʌ{D��{D�{D�L{Dˌ{D��{D�{D�L{Ď{D��{D�{D�L{D͌{D��{D�{D�L{DΌ{D��{D�{D�L{Dό{D��{D�{D�L{DЌ{D��{D�{D�L{Dь{D��{D�{D�L{DҌ{D�ϮD��D�L{Dӌ{D��{D�{D�L{DԌ{D��{D�{D�L{DՌ{D��{D�	HD�L{D֌{D��{D�{D�L{D׌{D��{D�{D�L{D،{D��{D�{D�O�Dُ�D��{D�{D�L{Dڌ{D��{D�{D�L{DۉHD��{D�{D�L{D܌{D��{D�{D�L{D݌{D��{D�{D�L{Dތ{D��{D�{D�L{Dߌ{D��{D�{D�L{D��{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�IHD�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{DꏮD�ϮD�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D��{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D�{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�L{D��{D��{D�{D�9H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A�ȴA�ȴA�A�ȴA���A���A���A��yA��A��A��`A��TA��
A��#A���A���A���A���A���A���A���A��AҼjAґhA�VA�A�jAд9A��`A�$�Aδ9A�
=A���A���A�hsA�K�A�$�A�33A�A�ffA�I�A�A���A��A�l�A��A�
=A���A�JA��\A�A�$�A��DA�A�A�-A���A�33A�oA��`A�1'A�t�A�O�A��A�
=A�A��A��A�p�A�JA�XA��yA�$�A��!A��;A�33A�I�A���A��uA�A�ȴA���A�5?A��A��-A��A�{A�A�$�A���A�jA��A���A�+A�ffA��/A�ƨA��TA�z�A���A�=qA�A|�Ay�FAw�TAv�As�TAp  Al~�AjQ�Ag�Ae�^AdQ�Ac�AbE�A`5?A]�^A\�A[��A[7LAZ�AZv�AZJAY�AX��AXn�AWdZAV�HAV��AVI�AU?}AT��ATAR�AQ�hAPJAM��AKt�AJ��AH�AH1'AF�9AD��ADAC�-AB��AB=qAAK�A@I�A>Q�A;A;&�A:�`A:bNA85?A6�RA65?A4�HA2n�A0�9A/K�A.��A.A-C�A,I�A+��A*�A*bNA)�wA)�A(�jA'x�A&ĜA%XA$  A"�+A!�A ��A�PA�A{A��AO�A�uA �A�hAZAQ�A7LA��A�;A�jA�#AE�A�^A��AC�A�RA��A��AJA�TAhsA�jA��A
r�A	+AVA��A�A�uA$�AVA��A��A+A-AhsA ��A Q�@���@��@�@��y@���@��@�P@�7L@�Ĝ@�r�@���@�C�@��@�ff@��@�\)@�G�@�dZ@�@��@�r�@� �@�\)@���@��@�X@�Ĝ@�  @�S�@�&�@ّh@��@֧�@��@ԃ@��;@�
=@���@���@с@��@�b@υ@�dZ@�K�@�o@θR@�M�@͙�@�Q�@���@��
@˥�@�
=@���@�/@ȼj@��m@Ƨ�@�J@�?}@�  @�
=@�n�@���@���@�A�@�dZ@���@�V@���@���@��-@���@���@��@�`B@�?}@��@�V@��@���@��
@�+@��H@�V@�@�p�@�p�@�X@��@� �@�|�@�C�@�
=@�dZ@�
=@�ff@�-@��#@��#@��T@���@��^@��^@��-@���@��@�hs@�O�@�&�@��@��j@��D@��@�(�@���@�+@��H@���@��+@�ff@�=q@�J@���@���@�p�@�G�@�V@���@��j@���@���@���@���@��D@�r�@�I�@� �@���@�\)@��R@�ff@���@��@��u@��w@�t�@�33@��R@���@�@��#@��^@�&�@��/@���@��D@���@�z�@�j@�  @���@��P@�l�@�
=@��y@�n�@���@�@�x�@�/@�%@��@�A�@���@���@��w@��P@�o@�"�@�"�@���@��@��R@�~�@��@���@���@��@�1'@�ƨ@��@���@���@��+@�M�@��\@��H@�ȴ@���@�n�@�=q@��@���@��7@��@��j@��@��@���@���@�dZ@�"�@�ȴ@�v�@�=q@�@���@��^@�p�@��@�V@���@���@���@�Z@�1@��@��@�;d@�+@�C�@��@��\@��T@���@�z�@�1@��;@�ƨ@���@�dZ@�+@��@���@�^5@�J@��^@�X@��@�Ĝ@�bN@�9X@���@��F@�\)@�33@�"�@��@�
=@�ȴ@�n�@��@��T@���@�O�@��@���@���@��@�Q�@�(�@���@��F@�dZ@�
=@��R@���@��\@�^5@�=q@�{@��^@��7@�7L@�V@��@��D@�r�@�A�@��@�b@�b@�1@��@���@�S�@�o@���@�V@�E�@�5?@�J@��@���@�hs@�O�@�%@�z�@�1'@��@��F@��@�dZ@�33@�o@�"�@�
=@�@��y@���@���@�n�@�M�@��@�J@���@���@��h@�p�@���@�A�@��@��@�w@�@\)@~��@~ff@~5?@}@}V@|Z@{��@{C�@z�H@z=q@y7L@y�@x�9@xbN@xA�@w��@wl�@v�y@v�R@v��@vV@u�-@t��@s��@s�F@s��@sS�@r��@r��@q��@q��@q�#@q%@pbN@o�P@n��@n��@m�@m�@l��@l�@l�/@k��@kt�@k33@j�@j��@j^5@i�@i��@ix�@i7L@i%@h��@hQ�@g��@g��@g;d@f�@f��@f�+@fE�@e�@e�@d�/@d�@d�@d�@e`B@dZ@cdZ@co@b�\@bJ@a��@aX@`��@`bN@_��@_K�@^�@^V@]@]�h@]`B@]�@]V@]/@]/@]V@\�D@\j@\j@\9X@\�@[��@[o@Z�@Z�@Z^5@Y��@Y%@X�`@X�`@XĜ@X�u@Xr�@XA�@X �@W�@W��@W�w@V�y@U�@U��@T��@UO�@T�@Tz�@T(�@S�F@SC�@R�H@R�\@R=q@Q��@QG�@P��@PbN@O�;@O�P@O\)@O�@N��@N{@M��@Mp�@L�@L�D@Lz�@L�@Kt�@J�@J�!@J�@Ix�@I&�@H�u@H �@G�w@G�P@G|�@G�@F�R@FE�@E�-@E�@E?}@D�/@D�D@Dz�@DI�@D1@C��@CC�@B�@B^5@B-@BJ@A�#@Ahs@A7L@A7L@A7L@A%@@��@@�@?�@?��@?�w@?��@?;d@>ȴ@>E�@=�@=�h@=p�@=?}@=V@=V@<�j@<Z@;�m@;�F@;��@;�@;33@;"�@;"�@;"�@;@:�!@:M�@:=q@:=q@:=q@:=q@:=q@9��@9��@9��@9hs@9�@8�`@8Ĝ@8�9@8��@8�u@8r�@8A�@8  @7��@7��@7|�@7l�@7K�@7
=@6�R@6V@5�h@5`B@5O�@5�@4�j@4z�@49X@3��@3ƨ@3S�@2�@2��@2�!@2��@2~�@2=q@2J@1��@1��@1G�@0��@0Q�@0 �@0b@/�@/�;@/|�@/+@/
=@/
=@.��@.�R@.v�@.V@.E�@.$�@.{@-�@-�-@-�@-O�@-/@-V@,�j@,�@,�@,�@,�@,�D@+�m@+ƨ@+��@+��@+�@+t�@+dZ@+dZ@+C�@*�@*n�@)��@)�#@)��@)hs@)X@)&�@(��@(bN@( �@( �@( �@'�@'�@'��@'|�@'
=@&ȴ@&�+@&E�@&$�@%�@%��@%�-@%O�@%V@$�@$��@$��@$I�@$�@#��@#"�@"��@"�\@"n�@"M�@!��@!G�@!&�@ �9@ �u@ �u@ A�@�w@��@�P@l�@+@��@ff@5?@@@@�-@�@O�@�@��@�j@�D@�D@j@9X@�m@�F@t�@33@o@�@�H@��@~�@-@�@�^@��@x�@hs@G�@%@�9@bN@ �@  @�;@�w@�@l�@K�@+@
=@�R@ff@5?@��@p�@?}@�@��@�/@��@��@Z@(�@1@ƨ@�@dZ@"�@"�@@��@�\@=q@��@��@��@�7@G�@7L@��@�9@��@�@bN@ �@  @�w@|�@l�@l�@\)@K�@
=@��@ȴ@��@�+@V@5?@$�@�@��@��@@��@�h@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A�ȴA�ȴA�A�ȴA���A���A���A��yA��A��A��`A��TA��
A��#A���A���A���A���A���A���A���A��AҼjAґhA�VA�A�jAд9A��`A�$�Aδ9A�
=A���A���A�hsA�K�A�$�A�33A�A�ffA�I�A�A���A��A�l�A��A�
=A���A�JA��\A�A�$�A��DA�A�A�-A���A�33A�oA��`A�1'A�t�A�O�A��A�
=A�A��A��A�p�A�JA�XA��yA�$�A��!A��;A�33A�I�A���A��uA�A�ȴA���A�5?A��A��-A��A�{A�A�$�A���A�jA��A���A�+A�ffA��/A�ƨA��TA�z�A���A�=qA�A|�Ay�FAw�TAv�As�TAp  Al~�AjQ�Ag�Ae�^AdQ�Ac�AbE�A`5?A]�^A\�A[��A[7LAZ�AZv�AZJAY�AX��AXn�AWdZAV�HAV��AVI�AU?}AT��ATAR�AQ�hAPJAM��AKt�AJ��AH�AH1'AF�9AD��ADAC�-AB��AB=qAAK�A@I�A>Q�A;A;&�A:�`A:bNA85?A6�RA65?A4�HA2n�A0�9A/K�A.��A.A-C�A,I�A+��A*�A*bNA)�wA)�A(�jA'x�A&ĜA%XA$  A"�+A!�A ��A�PA�A{A��AO�A�uA �A�hAZAQ�A7LA��A�;A�jA�#AE�A�^A��AC�A�RA��A��AJA�TAhsA�jA��A
r�A	+AVA��A�A�uA$�AVA��A��A+A-AhsA ��A Q�@���@��@�@��y@���@��@�P@�7L@�Ĝ@�r�@���@�C�@��@�ff@��@�\)@�G�@�dZ@�@��@�r�@� �@�\)@���@��@�X@�Ĝ@�  @�S�@�&�@ّh@��@֧�@��@ԃ@��;@�
=@���@���@с@��@�b@υ@�dZ@�K�@�o@θR@�M�@͙�@�Q�@���@��
@˥�@�
=@���@�/@ȼj@��m@Ƨ�@�J@�?}@�  @�
=@�n�@���@���@�A�@�dZ@���@�V@���@���@��-@���@���@��@�`B@�?}@��@�V@��@���@��
@�+@��H@�V@�@�p�@�p�@�X@��@� �@�|�@�C�@�
=@�dZ@�
=@�ff@�-@��#@��#@��T@���@��^@��^@��-@���@��@�hs@�O�@�&�@��@��j@��D@��@�(�@���@�+@��H@���@��+@�ff@�=q@�J@���@���@�p�@�G�@�V@���@��j@���@���@���@���@��D@�r�@�I�@� �@���@�\)@��R@�ff@���@��@��u@��w@�t�@�33@��R@���@�@��#@��^@�&�@��/@���@��D@���@�z�@�j@�  @���@��P@�l�@�
=@��y@�n�@���@�@�x�@�/@�%@��@�A�@���@���@��w@��P@�o@�"�@�"�@���@��@��R@�~�@��@���@���@��@�1'@�ƨ@��@���@���@��+@�M�@��\@��H@�ȴ@���@�n�@�=q@��@���@��7@��@��j@��@��@���@���@�dZ@�"�@�ȴ@�v�@�=q@�@���@��^@�p�@��@�V@���@���@���@�Z@�1@��@��@�;d@�+@�C�@��@��\@��T@���@�z�@�1@��;@�ƨ@���@�dZ@�+@��@���@�^5@�J@��^@�X@��@�Ĝ@�bN@�9X@���@��F@�\)@�33@�"�@��@�
=@�ȴ@�n�@��@��T@���@�O�@��@���@���@��@�Q�@�(�@���@��F@�dZ@�
=@��R@���@��\@�^5@�=q@�{@��^@��7@�7L@�V@��@��D@�r�@�A�@��@�b@�b@�1@��@���@�S�@�o@���@�V@�E�@�5?@�J@��@���@�hs@�O�@�%@�z�@�1'@��@��F@��@�dZ@�33@�o@�"�@�
=@�@��y@���@���@�n�@�M�@��@�J@���@���@��h@�p�@���@�A�@��@��@�w@�@\)@~��@~ff@~5?@}@}V@|Z@{��@{C�@z�H@z=q@y7L@y�@x�9@xbN@xA�@w��@wl�@v�y@v�R@v��@vV@u�-@t��@s��@s�F@s��@sS�@r��@r��@q��@q��@q�#@q%@pbN@o�P@n��@n��@m�@m�@l��@l�@l�/@k��@kt�@k33@j�@j��@j^5@i�@i��@ix�@i7L@i%@h��@hQ�@g��@g��@g;d@f�@f��@f�+@fE�@e�@e�@d�/@d�@d�@d�@e`B@dZ@cdZ@co@b�\@bJ@a��@aX@`��@`bN@_��@_K�@^�@^V@]@]�h@]`B@]�@]V@]/@]/@]V@\�D@\j@\j@\9X@\�@[��@[o@Z�@Z�@Z^5@Y��@Y%@X�`@X�`@XĜ@X�u@Xr�@XA�@X �@W�@W��@W�w@V�y@U�@U��@T��@UO�@T�@Tz�@T(�@S�F@SC�@R�H@R�\@R=q@Q��@QG�@P��@PbN@O�;@O�P@O\)@O�@N��@N{@M��@Mp�@L�@L�D@Lz�@L�@Kt�@J�@J�!@J�@Ix�@I&�@H�u@H �@G�w@G�P@G|�@G�@F�R@FE�@E�-@E�@E?}@D�/@D�D@Dz�@DI�@D1@C��@CC�@B�@B^5@B-@BJ@A�#@Ahs@A7L@A7L@A7L@A%@@��@@�@?�@?��@?�w@?��@?;d@>ȴ@>E�@=�@=�h@=p�@=?}@=V@=V@<�j@<Z@;�m@;�F@;��@;�@;33@;"�@;"�@;"�@;@:�!@:M�@:=q@:=q@:=q@:=q@:=q@9��@9��@9��@9hs@9�@8�`@8Ĝ@8�9@8��@8�u@8r�@8A�@8  @7��@7��@7|�@7l�@7K�@7
=@6�R@6V@5�h@5`B@5O�@5�@4�j@4z�@49X@3��@3ƨ@3S�@2�@2��@2�!@2��@2~�@2=q@2J@1��@1��@1G�@0��@0Q�@0 �@0b@/�@/�;@/|�@/+@/
=@/
=@.��@.�R@.v�@.V@.E�@.$�@.{@-�@-�-@-�@-O�@-/@-V@,�j@,�@,�@,�@,�@,�D@+�m@+ƨ@+��@+��@+�@+t�@+dZ@+dZ@+C�@*�@*n�@)��@)�#@)��@)hs@)X@)&�@(��@(bN@( �@( �@( �@'�@'�@'��@'|�@'
=@&ȴ@&�+@&E�@&$�@%�@%��@%�-@%O�@%V@$�@$��@$��@$I�@$�@#��@#"�@"��@"�\@"n�@"M�@!��@!G�@!&�@ �9@ �u@ �u@ A�@�w@��@�P@l�@+@��@ff@5?@@@@�-@�@O�@�@��@�j@�D@�D@j@9X@�m@�F@t�@33@o@�@�H@��@~�@-@�@�^@��@x�@hs@G�@%@�9@bN@ �@  @�;@�w@�@l�@K�@+@
=@�R@ff@5?@��@p�@?}@�@��@�/@��@��@Z@(�@1@ƨ@�@dZ@"�@"�@@��@�\@=q@��@��@��@�7@G�@7L@��@�9@��@�@bN@ �@  @�w@|�@l�@l�@\)@K�@
=@��@ȴ@��@�+@V@5?@$�@�@��@��@@��@�h@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Br�Bs�Br�Br�Bt�Bw�Bw�B|�B�1B�uB��B��B��B��B�B�?B�wB�}B�}B��BBƨB�B�B��B��BPB2-BA�BE�BJ�BL�BD�B@�B=qB>wBD�BM�BR�BW
BS�BR�B\)BbNBgmBs�B{�B}�B�B� B}�Bw�Bn�BjBhsBffBcTBdZB_;BL�B>wB9XB2-B,B+B&�B�BDBB�B�B��BȴB�wB�9B�B��B��B��B�hB�Bo�B\)BW
BT�BP�BK�B>wB33B"�B�BbB+B
��B
�5B
��B
�B
��B
��B
�bB
{�B
gmB
Q�B
=qB
33B
&�B
�B
B	�B	�5B	��B	ÖB	�^B	�9B	�B	��B	��B	�bB	�7B	�%B	�B	�B	~�B	|�B	y�B	w�B	t�B	p�B	o�B	p�B	l�B	hsB	e`B	^5B	W
B	P�B	D�B	9XB	49B	,B	&�B	!�B	�B	uB	oB	\B	VB	JB	1B	B��B��B��B��B�B�B�sB�`B�)B��B��B��BɺBƨB��B��B�}B��B�}B�}B�}B�^B�XB�?B�-B�'B�9B�?B�B��B�B�B�B��B��B��B��B��B��B��B��B��B�{B�bB�JB�\B�bB�PB�PB�+B�PB�oB�{B�oB�hB�VB�1B�B�1B�\B��B��B��B��B��B��B��B��B��B��B�oB�bB�DB�B�B�B}�Bx�Bz�B~�B~�B� B� B�B�B�B�%B�PB��B��B��B��B��B��B��B��B�-B�3B�3B�RB��BǮBĜBBB��B��B��B��BBÖBǮBɺB��B��B��B��B��B��B��B�B�B�
B�B�5B�BB�HB�`B�yB�B�B��B��B	  B	B	B	+B	DB	\B	hB	{B	�B	�B	�B	�B	 �B	#�B	&�B	/B	1'B	49B	9XB	:^B	:^B	>wB	=qB	?}B	@�B	B�B	F�B	G�B	K�B	J�B	L�B	P�B	W
B	]/B	`BB	bNB	cTB	dZB	ffB	iyB	k�B	m�B	o�B	s�B	w�B	x�B	y�B	z�B	|�B	}�B	}�B	~�B	�B	�%B	�=B	�JB	�PB	�VB	�VB	�\B	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�9B	�LB	�RB	�XB	�jB	�}B	B	ĜB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�B	�#B	�)B	�5B	�HB	�HB	�HB	�HB	�HB	�BB	�;B	�/B	�/B	�HB	�NB	�NB	�TB	�NB	�TB	�TB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
%B
1B
1B
	7B
	7B
1B
DB
PB
VB
VB
VB
VB
VB
VB
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
hB
hB
oB
oB
oB
uB
{B
{B
uB
{B
{B
{B
�B
�B
�B
�B
�B
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
"�B
!�B
!�B
"�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
%�B
&�B
&�B
&�B
'�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
,B
-B
.B
/B
/B
0!B
0!B
0!B
0!B
/B
0!B
0!B
0!B
0!B
1'B
2-B
2-B
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
5?B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
6FB
6FB
6FB
7LB
8RB
:^B
<jB
;dB
:^B
:^B
:^B
:^B
;dB
<jB
<jB
<jB
<jB
=qB
>wB
?}B
?}B
?}B
?}B
?}B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
E�B
F�B
F�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
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
T�B
T�B
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
XB
XB
XB
XB
XB
YB
YB
YB
ZB
ZB
ZB
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
^5B
^5B
_;B
_;B
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
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
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
e`B
e`B
e`B
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
k�B
k�B
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
m�B
n�B
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
o�B
o�B
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
r�B
r�B
s�B
s�B
s�B
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
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
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
x�B
x�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
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
�B
�B
�B
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
�B
�B
�B
�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111Br�Bs�Br�Br�Bt�Bw�Bw�B|�B�1B�uB��B��B��B��B�B�?B�wB�}B�}B��BBƨB�B�B��B��BPB2-BA�BE�BJ�BL�BD�B@�B=qB>wBD�BM�BR�BW
BS�BR�B\)BbNBgmBs�B{�B}�B�B� B}�Bw�Bn�BjBhsBffBcTBdZB_;BL�B>wB9XB2-B,B+B&�B�BDBB�B�B��BȴB�wB�9B�B��B��B��B�hB�Bo�B\)BW
BT�BP�BK�B>wB33B"�B�BbB+B
��B
�5B
��B
�B
��B
��B
�bB
{�B
gmB
Q�B
=qB
33B
&�B
�B
B	�B	�5B	��B	ÖB	�^B	�9B	�B	��B	��B	�bB	�7B	�%B	�B	�B	~�B	|�B	y�B	w�B	t�B	p�B	o�B	p�B	l�B	hsB	e`B	^5B	W
B	P�B	D�B	9XB	49B	,B	&�B	!�B	�B	uB	oB	\B	VB	JB	1B	B��B��B��B��B�B�B�sB�`B�)B��B��B��BɺBƨB��B��B�}B��B�}B�}B�}B�^B�XB�?B�-B�'B�9B�?B�B��B�B�B�B��B��B��B��B��B��B��B��B��B�{B�bB�JB�\B�bB�PB�PB�+B�PB�oB�{B�oB�hB�VB�1B�B�1B�\B��B��B��B��B��B��B��B��B��B��B�oB�bB�DB�B�B�B}�Bx�Bz�B~�B~�B� B� B�B�B�B�%B�PB��B��B��B��B��B��B��B��B�-B�3B�3B�RB��BǮBĜBBB��B��B��B��BBÖBǮBɺB��B��B��B��B��B��B��B�B�B�
B�B�5B�BB�HB�`B�yB�B�B��B��B	  B	B	B	+B	DB	\B	hB	{B	�B	�B	�B	�B	 �B	#�B	&�B	/B	1'B	49B	9XB	:^B	:^B	>wB	=qB	?}B	@�B	B�B	F�B	G�B	K�B	J�B	L�B	P�B	W
B	]/B	`BB	bNB	cTB	dZB	ffB	iyB	k�B	m�B	o�B	s�B	w�B	x�B	y�B	z�B	|�B	}�B	}�B	~�B	�B	�%B	�=B	�JB	�PB	�VB	�VB	�\B	�hB	�oB	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�!B	�9B	�LB	�RB	�XB	�jB	�}B	B	ĜB	ŢB	ɺB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�#B	�B	�#B	�)B	�5B	�HB	�HB	�HB	�HB	�HB	�BB	�;B	�/B	�/B	�HB	�NB	�NB	�TB	�NB	�TB	�TB	�fB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
%B
1B
1B
	7B
	7B
1B
DB
PB
VB
VB
VB
VB
VB
VB
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
hB
hB
oB
oB
oB
uB
{B
{B
uB
{B
{B
{B
�B
�B
�B
�B
�B
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
"�B
!�B
!�B
"�B
$�B
$�B
$�B
$�B
%�B
%�B
&�B
%�B
&�B
&�B
&�B
'�B
&�B
'�B
'�B
'�B
'�B
(�B
(�B
(�B
)�B
)�B
)�B
+B
+B
+B
,B
-B
.B
/B
/B
0!B
0!B
0!B
0!B
/B
0!B
0!B
0!B
0!B
1'B
2-B
2-B
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
5?B
5?B
5?B
5?B
5?B
6FB
7LB
7LB
6FB
6FB
6FB
7LB
8RB
:^B
<jB
;dB
:^B
:^B
:^B
:^B
;dB
<jB
<jB
<jB
<jB
=qB
>wB
?}B
?}B
?}B
?}B
?}B
A�B
A�B
A�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
E�B
E�B
E�B
E�B
E�B
F�B
E�B
F�B
F�B
G�B
H�B
I�B
I�B
I�B
I�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
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
T�B
T�B
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
XB
XB
XB
XB
XB
YB
YB
YB
ZB
ZB
ZB
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
^5B
^5B
_;B
_;B
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
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
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
e`B
e`B
e`B
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
k�B
k�B
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
m�B
n�B
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
o�B
o�B
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
r�B
r�B
s�B
s�B
s�B
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
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
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
x�B
x�B
y�B
y�B
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
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
�B
�B
�B
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
�B
�B
�B
�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA19c                                                                20210118093954  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20210118004010  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20210118004011  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20210118004011  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20210118004011  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20210118004011  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20210118004011  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20210118004011  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20210118004012  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20210118004012                      G�O�G�O�G�O�                JA  ARUP                                                                        20210118005246                      G�O�G�O�G�O�                