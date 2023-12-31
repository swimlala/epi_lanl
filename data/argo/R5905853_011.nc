CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:24:26Z creation;2022-06-04T17:24:26Z conversion to V3.1      
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
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604172426  20220610131506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @ؿ�}�u11   @ؿ���k@,-�hr�!�d\(�1   GPS     A   B   B   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�33A   A   A@  A`  A~ffA�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B���B�  B�  B�  B�33B�  B�  B�ffB�  B���B���B�  B�  C   C  C  C  C  C
  C  C  C�CL�C�fC�fC  C  C  C  C   C"  C$  C&  C(  C)�fC+�fC.  C0  C2  C4  C633C8�C:  C<  C=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb�Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT�fDU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@���@�(�A z�A z�A@z�A`z�A~�GA�=qA�=qA�=qA�=qA�=qA�=qA�=qB �B�B�B�B �B(�B0�B8�B@�BH�BP�BX�B`�Bh�Bp�Bx�B�\B�\B�\B�\B�B�B��)B���B�\B�\B�\B�\B�\B�\B�\B�\B�\B�B�B�u�B��)B��)B�\B�\B�\B�B�B�\B�\B�u�B�\B��)B��)B�\B�\C �C�C�C�C�C
�C�C�C!HCT{C�C�C�C�C�C�C �C"�C$�C&�C(�C)�C+�C.�C0�C2�C4�C6:�C8!HC:�C<�C=�C@�CB�CD�CF�CH�CJ�CL�CN�CP�CR�CT�CV�CX�CZ�C\�C^�C`�Cb!HCd�Cf�Ch�Cj�Cl�Cn�Cp�Cr�Ct�Cv�Cx�Cz�C|�C~�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��
C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT�RDU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D�)D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D��)D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D��)D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�@�DÀ�D���D� �D�@�DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D���D� �D�@�Dǀ�D���D� �D�@�DȀ�D���D� �D�@�Dɀ�D���D� �D�@�Dʀ�D���D� �D�@�Dˀ�D���D� �D�@�D̀�D���D� �D�@�D̀�D���D� �D�@�D΀�D���D� �D�@�Dπ�D���D� �D�@�DЀ�D���D� �D�@�Dр�D���D� �D�@�DҀ�D���D� �D�@�DӀ�D���D� �D�@�DԀ�D���D� �D�@�DՀ�D���D� �D�@�Dր�D���D� �D�@�D׀�D���D� �D�@�D؀�D���D� �D�@�Dـ�D���D� �D�@�Dڀ�D���D� �D�@�Dۀ�D���D� �D�@�D܀�D���D� �D�@�D݀�D���D� �D�@�Dހ�D���D� �D�@�D߀�D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D��D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D��D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D� �D�@�D���D���D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A�:A��A�:A�YA�A�CA�kA�	A�YA�kA� \A�!�A�	A�VA�!�A�$@A�#A�qA�oA� A�
�A��A���A��A���A��RA���A���A��?A��9A��[Aֽ�Aְ�Aօ�A�IRA��AӚ7AфMA���AЛ�AІ�A�{A�VAό�A��2A�)�A��A�s�A��Aʐ.A�%A�:�A�$A�hsAÝIA���A��HA��0A�gmA��A�҉A���A��?A���A��A�X�A��JA��9A�OA�a�A��A��+A��AA��A�]/A��A�zA��oA��A��A�0�A�X�A�\�A�ϫA��gA�_pA}1�A|!-Ay�Au?�Aq�Ap \Akv�Ad�.A_�AZ�tAV,=AP��AO�TAM4AK�1AJ��AD�ABA@8�A?SA=:*A<g�A:��A9!�A7�A7R�A6�A4��A4R�A3�A3+kA2v�A1�A1,=A0��A02�A/��A/W?A.�NA.6zA-m�A,�kA+��A*zA)��A)�A(l�A'bNA&�fA&��A%�A$�;A$q�A#�0A#�A"PHA!�2A!�MA!^5A ��A ��A N�Au�A�A�oA�A_�A-wA�6A$�A�AOA��A.IA�=A�A�A�3AG�A��A�A�AAS&AE9A�]AaA�mA�*AM�A�A��Ai�A�AU�A��Au%A�AɆA��A|�AE9A�Au�A��A+A��A�UA��AC�A
��A
�eA
��A
c�A
2aA
%A	�A	�gA	ϫA	�-A	�"A	_A	 \A	�A҉A>BA��A~�Ac�AC-A#�A�A��A��AS&AA��A~(AA�cA��A7LA��A��APHA�A�9A�hAkQA.IAqA�"A�9A�OAA �nA �DA c�A OA 
=@��	@��@��1@�>B@��&@�K�@���@��@�%@��9@�g8@��A@���@���@�-@���@��@��@��@�xl@��@��#@�&@�Ta@�7@���@�+@��@�f�@�$t@��@�@�Z�@��X@�1@���@�$t@��@脶@�9�@�J�@��@��@���@�@�h�@�|�@��?@�h@��@�^5@�ƨ@�Ft@�x@��d@�C�@�xl@�Vm@�A�@ہ�@�� @ۜ@��@څ�@�-@�|�@�c @מ�@ט�@�'�@֮}@�Z@��@կ�@�8�@�i�@���@Ӟ�@�x@��2@�~(@�:*@�7@��@мj@О�@Ї�@��a@�Z�@�<6@���@Κ�@���@�K�@�I�@˵t@��@ʕ�@���@�K�@��@Ȣ4@�e@ǝ�@��,@�5?@�J@���@Ŝ�@�zx@�G�@���@�҉@�q@�C�@�	�@Â�@���@o@�B[@�@�b@���@�4�@���@�,=@���@�IR@��I@�H@���@���@�e�@��<@��D@��K@���@��k@��:@���@��v@�J�@�?@�4n@��@�&@��L@�x�@���@��m@�E�@��@���@���@���@��!@��F@�[�@��@�?}@�	l@��h@�Z@���@�|@�N<@�q@��@��L@�z�@�c @�@�@�O@��>@��	@�@�]d@��@��f@�w2@�a�@�6z@��/@��\@�n�@�]d@�?@���@��$@�[W@�/@��f@���@�PH@�'R@�@��X@�2a@��x@�6@���@��C@�Dg@��@��@��@��?@�}V@�?@��~@�&�@�� @�c @�L0@��@���@��{@�;d@��c@�q@�I�@��@��-@�4�@��@��4@�c @�\�@�Ft@��@��@��@@�"�@��R@�M@�	@��'@�4�@��@��@�e�@���@���@�K�@��@�҉@���@�`�@�+k@���@��@���@�J�@��@��C@�dZ@�q@���@�oi@�خ@���@���@�s�@�g�@�/@�
=@�%@���@���@���@���@��u@�i�@�M@���@��{@�O�@��@���@��j@���@��A@�ff@�O@�خ@���@�n/@��@��Y@�:*@�_@��g@�N<@�&�@��2@��?@���@�~@��6@�6z@��\@�M@���@���@�RT@�S@���@�tT@�?�@��@��@��)@���@�t�@�o @�S�@��@���@��O@�p;@��&@���@��w@��[@���@�l�@�A @�@��@�Ov@��@���@���@�F�@�&�@��@���@��@���@�`B@�,�@��@���@���@��@�Ta@��^@�C�@�7L@�C@��@���@��F@��@�v�@�:*@��@~�@~��@~xl@~:*@~J@}�H@}J�@|�@|Xy@{�}@{�	@{\)@{�@zff@y��@y�@y�@y��@y�@y�@y�H@yVm@x�)@x<�@w�:@w4�@va|@v@u�@u|@t��@t?�@t�@s��@sg�@s@O@s�@r�x@r@qj@p�j@p:�@p@o��@o@O@ni�@m�@mw2@m-w@l�	@l�@k!-@j�@jz@j
�@i(�@h�u@g�@g��@f�@f��@f	@e�d@e	l@d��@d��@dPH@d�@c��@c.I@b�x@a�Z@a|@`��@`9X@_�;@_�@]��@]5�@\��@\��@\y>@\!@[ݘ@[�q@[33@ZYK@Z�@Yj@Y@X��@XN�@X/�@X�@W��@W��@W
=@V��@V-@U@Um]@U/@T�@T�$@T6@S��@S�@R��@R�@QG�@Q!�@Pѷ@P�Y@PV�@P7@Oƨ@OE9@N��@N �@M�3@M�n@M�@MX@L�[@LM@L>B@L,=@K��@K�@KP�@J�H@J�}@J��@J~�@JGE@I�S@IX@I8�@I%@H�p@H�_@HI�@G�@G��@GS�@Gqv@G6z@G�@G i@F��@F͟@F��@F)�@E�o@EX@D?�@C��@B\�@B-@Be@BJ@A�@AO�@AL�@AJ�@ADg@A&�@@��@@Ɇ@@��@@�o@?��@?��@?H�@?)_@>��@>҉@>��@>O@=�h@=+�@<��@<~(@;�	@:��@:Ta@:.�@9u�@9#�@8��@8��@8q@8K^@8@7�Q@7��@7iD@7S@6҉@6d�@6�@5�-@5=�@4�@4��@4�_@4z�@4Z@4$@3�@3��@3J#@2�@2�F@2Ta@2&�@2 �@1��@1c@1IR@18�@0�@0U2@0>B@0�@/˒@/�$@.��@.�@.v�@.i�@.6�@.	@.	@-��@-��@-ϫ@-�X@-�"@-�@-Dg@-�@,�9@,j@,�@+��@+��@+=@*�b@)�@)u�@)<6@(�@(��@(Z@(2�@("h@(�@(7@(�@'�@'�@'�	@'\)@'=@'�@&�X@&��@&��@&�!@&��@&O@%f�@%+@$��@$U2@$�@#��@#,�@#�@"�@"��@"!�@!�H@!��@!�S@!A @!@@!@@!�@ �K@ ��@ �4@ �u@ c�@ b@ݘ@�0@e�@��@Z�@Ov@L0@1�@��@S&@%@�[@�$@6@�;@�g@��@S�@'�@�@!�@@�@�@��@Vm@(�@�`@��@�O@�_@u�@1'@�r@�k@iD@.I@S@��@�@i�@$�@
�@��@��@�"@%F@��@��@�@�$@�@��@M@�@��@��@��@��@W?@.I@+@�M@��@l�@�@�C@��@��@�S@w2@ \@�@��@�u@4n@7@�r@�;@��@��@�a@��@��@y�@j�@C�@,�@o@��@��@d�@1�@��@�@�z@�"@�M@j@%@��@r�@K^@6@'R@~@��@�g111111111111111111111111111111111111111111111111111111111144411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A�:A��A�:A�YA�A�CA�kA�	A�YA�kA� \A�!�A�	A�VA�!�A�$@A�#A�qA�oA� A�
�A��A���A��A���A��RA���A���A��?A��9A��[Aֽ�Aְ�Aօ�A�IRA��AӚ7AфMA���AЛ�AІ�A�{A�VAό�A��2A�)�A��A�s�A��Aʐ.A�%A�:�A�$A�hsAÝIA���A��HA��0A�gmA��A�҉A���A��?A���A��A�X�A��JA��9A�OA�a�A��A��+A��AA��A�]/A��A�zA��oA��A��A�0�A�X�A�\�A�ϫA��gA�_pA}1�A|!-Ay�Au?�Aq�Ap \Akv�Ad�.A_�AZ�tAV,=AP��AO�TAM4AK�1AJ��AD�ABA@8�A?SA=:*A<g�A:��A9!�A7�A7R�A6�A4��A4R�A3�A3+kA2v�A1�A1,=A0��A02�A/��A/W?A.�NA.6zA-m�A,�kA+��A*zA)��A)�A(l�A'bNA&�fA&��A%�A$�;A$q�A#�0A#�A"PHA!�2A!�MA!^5A ��A ��A N�Au�A�A�oA�A_�A-wA�6A$�A�AOA��A.IA�=A�A�A�3AG�A��A�A�AAS&AE9A�]AaA�mA�*AM�A�A��Ai�A�AU�A��Au%A�AɆA��A|�AE9A�Au�A��A+A��A�UA��AC�A
��A
�eA
��A
c�A
2aA
%A	�A	�gA	ϫA	�-A	�"A	_A	 \A	�A҉A>BA��A~�Ac�AC-A#�A�A��A��AS&AA��A~(AA�cA��A7LA��A��APHA�A�9A�hAkQA.IAqA�"A�9A�OAA �nA �DA c�A OA 
=@��	@��@��1@�>B@��&@�K�@���@��@�%@��9@�g8@��A@���@���@�-@���@��@��@��@�xl@��@��#@�&@�Ta@�7@���@�+@��@�f�@�$t@��@�@�Z�@��X@�1@���@�$t@��@脶@�9�@�J�@��@��@���@�@�h�@�|�@��?@�h@��@�^5@�ƨ@�Ft@�x@��d@�C�@�xl@�Vm@�A�@ہ�@�� @ۜ@��@څ�@�-@�|�@�c @מ�@ט�@�'�@֮}@�Z@��@կ�@�8�@�i�@���@Ӟ�@�x@��2@�~(@�:*@�7@��@мj@О�@Ї�@��a@�Z�@�<6@���@Κ�@���@�K�@�I�@˵t@��@ʕ�@���@�K�@��@Ȣ4@�e@ǝ�@��,@�5?@�J@���@Ŝ�@�zx@�G�@���@�҉@�q@�C�@�	�@Â�@���@o@�B[@�@�b@���@�4�@���@�,=@���@�IR@��I@�H@���@���@�e�@��<@��D@��K@���@��k@��:@���@��v@�J�@�?@�4n@��@�&@��L@�x�@���@��m@�E�@��@���@���@���@��!@��F@�[�@��@�?}@�	l@��h@�Z@���@�|@�N<@�q@��@��L@�z�@�c @�@�@�O@��>@��	@�@�]d@��@��f@�w2@�a�@�6z@��/@��\@�n�@�]d@�?@���@��$@�[W@�/@��f@���@�PH@�'R@�@��X@�2a@��x@�6@���@��C@�Dg@��@��@��@��?@�}V@�?@��~@�&�@�� @�c @�L0@��@���@��{@�;d@��c@�q@�I�@��@��-@�4�@��@��4@�c @�\�@�Ft@��@��@��@@�"�@��R@�M@�	@��'@�4�@��@��@�e�@���@���@�K�@��@�҉@���@�`�@�+k@���@��@���@�J�@��@��C@�dZ@�q@���@�oi@�خ@���@���@�s�@�g�@�/@�
=@�%@���@���@���@���@��u@�i�@�M@���@��{@�O�@��@���@��j@���@��A@�ff@�O@�خ@���@�n/@��@��Y@�:*@�_@��g@�N<@�&�@��2@��?@���@�~@��6@�6z@��\@�M@���@���@�RT@�S@���@�tT@�?�@��@��@��)@���@�t�@�o @�S�@��@���@��O@�p;@��&@���@��w@��[@���@�l�@�A @�@��@�Ov@��@���@���@�F�@�&�@��@���@��@���@�`B@�,�@��@���@���@��@�Ta@��^@�C�@�7L@�C@��@���@��F@��@�v�@�:*@��@~�@~��@~xl@~:*@~J@}�H@}J�@|�@|Xy@{�}@{�	@{\)@{�@zff@y��@y�@y�@y��@y�@y�@y�H@yVm@x�)@x<�@w�:@w4�@va|@v@u�@u|@t��@t?�@t�@s��@sg�@s@O@s�@r�x@r@qj@p�j@p:�@p@o��@o@O@ni�@m�@mw2@m-w@l�	@l�@k!-@j�@jz@j
�@i(�@h�u@g�@g��@f�@f��@f	@e�d@e	l@d��@d��@dPH@d�@c��@c.I@b�x@a�Z@a|@`��@`9X@_�;@_�@]��@]5�@\��@\��@\y>@\!@[ݘ@[�q@[33@ZYK@Z�@Yj@Y@X��@XN�@X/�@X�@W��@W��@W
=@V��@V-@U@Um]@U/@T�@T�$@T6@S��@S�@R��@R�@QG�@Q!�@Pѷ@P�Y@PV�@P7@Oƨ@OE9@N��@N �@M�3@M�n@M�@MX@L�[@LM@L>B@L,=@K��@K�@KP�@J�H@J�}@J��@J~�@JGE@I�S@IX@I8�@I%@H�p@H�_@HI�@G�@G��@GS�@Gqv@G6z@G�@G i@F��@F͟@F��@F)�@E�o@EX@D?�@C��@B\�@B-@Be@BJ@A�@AO�@AL�@AJ�@ADg@A&�@@��@@Ɇ@@��@@�o@?��@?��@?H�@?)_@>��@>҉@>��@>O@=�h@=+�@<��@<~(@;�	@:��@:Ta@:.�@9u�@9#�@8��@8��@8q@8K^@8@7�Q@7��@7iD@7S@6҉@6d�@6�@5�-@5=�@4�@4��@4�_@4z�@4Z@4$@3�@3��@3J#@2�@2�F@2Ta@2&�@2 �@1��@1c@1IR@18�@0�@0U2@0>B@0�@/˒@/�$@.��@.�@.v�@.i�@.6�@.	@.	@-��@-��@-ϫ@-�X@-�"@-�@-Dg@-�@,�9@,j@,�@+��@+��@+=@*�b@)�@)u�@)<6@(�@(��@(Z@(2�@("h@(�@(7@(�@'�@'�@'�	@'\)@'=@'�@&�X@&��@&��@&�!@&��@&O@%f�@%+@$��@$U2@$�@#��@#,�@#�@"�@"��@"!�@!�H@!��@!�S@!A @!@@!@@!�@ �K@ ��@ �4@ �u@ c�@ b@ݘ@�0@e�@��@Z�@Ov@L0@1�@��@S&@%@�[@�$@6@�;@�g@��@S�@'�@�@!�@@�@�@��@Vm@(�@�`@��@�O@�_@u�@1'@�r@�k@iD@.I@S@��@�@i�@$�@
�@��@��@�"@%F@��@��@�@�$@�@��@M@�@��@��@��@��@W?@.I@+@�M@��@l�@�@�C@��@��@�S@w2@ \@�@��@�u@4n@7@�r@�;@��@��@�a@��@��@y�@j�@C�@,�@o@��@��@d�@1�@��@�@�z@�"@�M@j@%@��@r�@K^@6@'R@~@��@�g111111111111111111111111111111111111111111111111111111111144411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BٴBٴB��B��B��BٴBٴB��BٚB�B�eB�eB�B�eB�KBٚB�B�eB�B�B�eB�KB�B�1BخB��B��BؓBخBؓBؓB�B�sB�mB�BѝB�DB�`B��B��B�7B�yB�DB�TB�dB�`B	GB	EmB	yXB	�yB
�B
$@B
*�B
EB
�B
�FB1[B@ BJ�B
�B	��B	��B	q'B	��B	��B	�4B	��B	ϫB	�XB
��B!�BG�BESB	�B
�iB
�B
�B
��B
�B
�TB
��B
��B
�aB
~]B
XB
*B	�B	ںB	��B	ɺB	�AB	��B	�gB	{B	W�B	<6B	"�B	TB	GB	�B	2B	/�B	,B	$tB	�B	pB	#�B	+kB	-wB	;JB	N�B	T�B	X�B	[	B	WsB	Z�B	ZB	_B	kQB	}�B	�jB	��B	��B	�vB	��B	��B	�`B	�:B	�ZB	�B	ȴB	ּB	�)B	�B	��B	��B	�6B	�[B	�B	��B
UB
�B
^B
VB
�B
�B
�B
�B
�B
�B
�B
5B
 �B
"�B
-�B
1'B
3B
3hB
3hB
2�B
4�B
33B
.�B
-)B
(�B
'8B
*�B
1B
7�B
5tB
1AB
.B
,�B
,�B
,qB
+�B
+B
+�B
+6B
*�B
+QB
,�B
-wB
+�B
+kB
+6B
,"B
-B
-�B
./B
.}B
/�B
/�B
2GB
2-B
3MB
4TB
5�B
5�B
5�B
6FB
6�B
7B
7�B
7�B
7�B
88B
8�B
9�B
9�B
9>B
9	B
8�B
8�B
8B
88B
8�B
9�B
;B
;B
;�B
;�B
=�B
<�B
=<B
=VB
<�B
<�B
;dB
:xB
:�B
:�B
9�B
9�B
8�B
8�B
8lB
8lB
8B
7�B
6�B
72B
5tB
5B
4�B
4�B
4nB
49B
4B
3B
3B
2-B
2-B
1[B
1'B
.�B
-�B
,�B
-]B
-�B
-]B
+�B
+6B
*KB
(
B
'B
&2B
$�B
$tB
$B
!�B
!HB
# B
"�B
"NB
 B
!bB
$�B
$�B
#nB
"hB
 �B
�B
�B
B
?B
sB
�B
�B
�B
MB
2B
FB
�B
�B
NB
"B
�B
B
jB
 B
 B
NB
�B
�B
"B
�B
�B
�B
�B
4B
}B
�B
<B
jB
PB
�B
�B
�B
xB

=B
	�B
	7B
1B
fB
�B
�B
�B
B
�B
B
�B
�B
EB
_B
�B
	B
	B
KB
�B
�B
aB
�B
�B
uB
�B
�B
�B
�B
B
�B
�B
�B
gB
MB
gB
B
?B
YB
?B
tB
B
tB
�B
YB
YB
�B
�B
�B
B
�B
�B
�B
B
�B
EB
+B
�B
zB
zB
�B
�B
	lB

	B

=B

#B

	B
B

�B

�B

�B
)B
0B
JB
0B
JB
�B
�B
�B
�B
�B
�B
<B
�B
�B
(B
vB
vB
�B
�B
�B
 B
 B
NB
hB
�B
�B
�B
�B
�B
�B
�B
oB
&B
uB
�B
uB
@B
&B
�B
�B
�B
�B
oB
TB
oB
oB
oB
oB
TB
oB
oB
TB
oB
�B
�B
HB
HB
�B
�B
 B
:B
oB
�B
B
�B
 B
B
�B
�B
�B
�B
B
oB
�B
�B
�B
�B
�B
�B
�B
�B
oB
�B
�B
,B
�B
�B
gB
�B
�B
YB
?B
�B
sB
�B
_B
�B
�B
�B
�B
B
QB
kB
�B
7B
�B
#B
�B
)B
�B
)B
B
�B
CB
�B
;B
pB
 \B
 �B
 'B
�B
VB
;B
pB
;B
;B
 BB
 �B
!�B
"hB
"�B
"�B
"�B
"�B
# B
#nB
#:B
#:B
#B
#�B
#�B
$B
$&B
$�B
%FB
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'mB
'�B
(>B
(�B
)B
)_B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
+�B
,B
,"B
,=B
,qB
-wB
-]B
-CB
-]B
-]B
-�B
-�B
-�B
/ B
/iB
0!B
0�B
0�B
1B
1B
0�B
0�B
1�B
1�B
1�B
2B
2�B
33B
3�B
3�B
3�B
4B
4nB
4�B
4�B
5B
5tB
5�B
5�B
5�B
6`B
7B
72B
7fB
7�B
7�B
7�B
7�B
8B
8B
8�B
9	B
9>B
9$B
9$B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:^B
:�B
:�B
;B
;B
;dB
;�B
<B
<B
<PB
<jB
<�B
<�B
<�B
<�B
="B
=�B
>B
>BB
>wB
>�B
>�B
?}B
?}B
@ B
?�B
?�B
@�B
?�B
?�B
?�B
?�B
@�B
@�B
@iB
@�B
AoB
A�B
A�B
B'B
B�B
C-B
C�B
C�B
C�B
C�B
D3B
DgB
D�B
D�B
E9B
ESB
ESB
E�B
F�B
F�B
F�B
F�B
F�B
G_B
G�B
GzB
G�B
G�B
H�B
IlB
IRB
I�B
J=B
J�B
J�B
K)B
KDB
K�B
LB
LJB
L0B
L0B
LdB
L�B
MB
MjB
M�B
NB
N<B
N<B
NVB
N<B
N�B
N�B
N�B
OB
N�B
OBB
OB
N�B
N�B
N�B
N�B
N�B
O�B
PbB
P}B
P}B
P�B
P}B
P�B
Q B
Q B
P�B
P�B
P�B
P�B
QB
Q4B
Q4B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
RoB
R�B
R�B
R�B
R�B
R�B
SB
S[B
SB
S[B
SuB
T,B
UB
T�B
T�B
T�B
U2B
U�B
U�B
UgB
UgB
UgB
U�B
U�B
U�B
U�B
VB
VmB
V�B
W
B
W
B
W
B
W?B
W�B
XB
XEB
X_B
XyB
X�B
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
\)B
\B
\B
\]B
\]B
\�B
\�B
\�B
]~B
]IB
]dB
]�B
]�B
^B
^B
^5B
^�B
_B
_;B
_;B
_!B
_VB
_VB
_pB
_�B
_�B
_�B
`BB
`�B
`�B
aB
a�B
a�B
a|B
a|B
a�B
abB
a|B
a�B
a|B
a�B
a�B
a�B
bB
bNB
bNB
b�B
b4B
b�B
b�B
cB
c�B
d@B
dZB
d@B
d�B
d�B
d�B
e,B
eFB
e�B
e�B
e�B
e�B
e�B
fB
ffB
f�B
f�B
f�B
f�B
g8B
hsB
h�B
iB
i�B
iyB
i�B
i�B
i_B
i_B
j�B
k�B
k�B
l=B
l"B
l�B
lqB
l�B
lWB
lqB
lqB
lqB
l�B
lWB
l"B
lWB
lWB
l�B
l�B
mCB
mCB
m)B
l�B
mCB
m�B
nB
m�B
m�B
m�B
n/B
o�B
p;B
pB
pB
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
rB
rB
raB
r�B
r�B
sB
sB
sB
sB
sB
sMB
shB
s�B
tB
tB
t9B
tTB
t�B
t�B
uB
uB
u?B
u%B
utB
u�B
u�B
u�B
u�B
vB
vB
v+B
vzB
v�B
v�B
v�B
v�B
v�B
wLB
wfB
wLB
w�B
w�B
xB
x�B
y$B
y	B
y$B
y	B
y>B
y�B
yrB
y�B
z*B
zDB
z^B
z�B
z�B
zxB
z�B
z�B
z�B
z�B
z�B
{0B
{dB
{B
{0B
{�B
|PB
|�B
|�B
}<B
}<B
}qB
}�B
}�B
}�B
~(B
~wB
~�B
~�B
~�B
~�B
~�B
.B
� 111111111111111111111111111111111111111111111111111111111144411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BٴBٴB��B��B��BٴBٴB��BٚB�B�eB�eB�B�eB�KBٚB�B�eB�B�B�eB�KB�B�1BخB��B��BؓBخBؓBؓB�B�sB�mB�BѝB�DB�`B��B��B�7B�yB�DB�TB�dB�`B	GB	EmB	yXB	�yB
�B
$@B
*�B
EB
�B
�FB1[B@ BJ�B
�B	��B	��B	q'B	��B	��B	�4B	��B	ϫB	�XB
��B!�BG�BESB	�B
�iB
�B
�B
��B
�B
�TB
��B
��B
�aB
~]B
XB
*B	�B	ںB	��B	ɺB	�AB	��B	�gB	{B	W�B	<6B	"�B	TB	GB	�B	2B	/�B	,B	$tB	�B	pB	#�B	+kB	-wB	;JB	N�B	T�B	X�B	[	B	WsB	Z�B	ZB	_B	kQB	}�B	�jB	��B	��B	�vB	��B	��B	�`B	�:B	�ZB	�B	ȴB	ּB	�)B	�B	��B	��B	�6B	�[B	�B	��B
UB
�B
^B
VB
�B
�B
�B
�B
�B
�B
�B
5B
 �B
"�B
-�B
1'B
3B
3hB
3hB
2�B
4�B
33B
.�B
-)B
(�B
'8B
*�B
1B
7�B
5tB
1AB
.B
,�B
,�B
,qB
+�B
+B
+�B
+6B
*�B
+QB
,�B
-wB
+�B
+kB
+6B
,"B
-B
-�B
./B
.}B
/�B
/�B
2GB
2-B
3MB
4TB
5�B
5�B
5�B
6FB
6�B
7B
7�B
7�B
7�B
88B
8�B
9�B
9�B
9>B
9	B
8�B
8�B
8B
88B
8�B
9�B
;B
;B
;�B
;�B
=�B
<�B
=<B
=VB
<�B
<�B
;dB
:xB
:�B
:�B
9�B
9�B
8�B
8�B
8lB
8lB
8B
7�B
6�B
72B
5tB
5B
4�B
4�B
4nB
49B
4B
3B
3B
2-B
2-B
1[B
1'B
.�B
-�B
,�B
-]B
-�B
-]B
+�B
+6B
*KB
(
B
'B
&2B
$�B
$tB
$B
!�B
!HB
# B
"�B
"NB
 B
!bB
$�B
$�B
#nB
"hB
 �B
�B
�B
B
?B
sB
�B
�B
�B
MB
2B
FB
�B
�B
NB
"B
�B
B
jB
 B
 B
NB
�B
�B
"B
�B
�B
�B
�B
4B
}B
�B
<B
jB
PB
�B
�B
�B
xB

=B
	�B
	7B
1B
fB
�B
�B
�B
B
�B
B
�B
�B
EB
_B
�B
	B
	B
KB
�B
�B
aB
�B
�B
uB
�B
�B
�B
�B
B
�B
�B
�B
gB
MB
gB
B
?B
YB
?B
tB
B
tB
�B
YB
YB
�B
�B
�B
B
�B
�B
�B
B
�B
EB
+B
�B
zB
zB
�B
�B
	lB

	B

=B

#B

	B
B

�B

�B

�B
)B
0B
JB
0B
JB
�B
�B
�B
�B
�B
�B
<B
�B
�B
(B
vB
vB
�B
�B
�B
 B
 B
NB
hB
�B
�B
�B
�B
�B
�B
�B
oB
&B
uB
�B
uB
@B
&B
�B
�B
�B
�B
oB
TB
oB
oB
oB
oB
TB
oB
oB
TB
oB
�B
�B
HB
HB
�B
�B
 B
:B
oB
�B
B
�B
 B
B
�B
�B
�B
�B
B
oB
�B
�B
�B
�B
�B
�B
�B
�B
oB
�B
�B
,B
�B
�B
gB
�B
�B
YB
?B
�B
sB
�B
_B
�B
�B
�B
�B
B
QB
kB
�B
7B
�B
#B
�B
)B
�B
)B
B
�B
CB
�B
;B
pB
 \B
 �B
 'B
�B
VB
;B
pB
;B
;B
 BB
 �B
!�B
"hB
"�B
"�B
"�B
"�B
# B
#nB
#:B
#:B
#B
#�B
#�B
$B
$&B
$�B
%FB
%�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'mB
'�B
(>B
(�B
)B
)_B
)�B
)�B
)�B
)�B
*�B
*�B
*�B
*�B
*�B
+�B
+�B
+�B
+�B
,B
,"B
,=B
,qB
-wB
-]B
-CB
-]B
-]B
-�B
-�B
-�B
/ B
/iB
0!B
0�B
0�B
1B
1B
0�B
0�B
1�B
1�B
1�B
2B
2�B
33B
3�B
3�B
3�B
4B
4nB
4�B
4�B
5B
5tB
5�B
5�B
5�B
6`B
7B
72B
7fB
7�B
7�B
7�B
7�B
8B
8B
8�B
9	B
9>B
9$B
9$B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:^B
:�B
:�B
;B
;B
;dB
;�B
<B
<B
<PB
<jB
<�B
<�B
<�B
<�B
="B
=�B
>B
>BB
>wB
>�B
>�B
?}B
?}B
@ B
?�B
?�B
@�B
?�B
?�B
?�B
?�B
@�B
@�B
@iB
@�B
AoB
A�B
A�B
B'B
B�B
C-B
C�B
C�B
C�B
C�B
D3B
DgB
D�B
D�B
E9B
ESB
ESB
E�B
F�B
F�B
F�B
F�B
F�B
G_B
G�B
GzB
G�B
G�B
H�B
IlB
IRB
I�B
J=B
J�B
J�B
K)B
KDB
K�B
LB
LJB
L0B
L0B
LdB
L�B
MB
MjB
M�B
NB
N<B
N<B
NVB
N<B
N�B
N�B
N�B
OB
N�B
OBB
OB
N�B
N�B
N�B
N�B
N�B
O�B
PbB
P}B
P}B
P�B
P}B
P�B
Q B
Q B
P�B
P�B
P�B
P�B
QB
Q4B
Q4B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
RoB
R�B
R�B
R�B
R�B
R�B
SB
S[B
SB
S[B
SuB
T,B
UB
T�B
T�B
T�B
U2B
U�B
U�B
UgB
UgB
UgB
U�B
U�B
U�B
U�B
VB
VmB
V�B
W
B
W
B
W
B
W?B
W�B
XB
XEB
X_B
XyB
X�B
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
\)B
\B
\B
\]B
\]B
\�B
\�B
\�B
]~B
]IB
]dB
]�B
]�B
^B
^B
^5B
^�B
_B
_;B
_;B
_!B
_VB
_VB
_pB
_�B
_�B
_�B
`BB
`�B
`�B
aB
a�B
a�B
a|B
a|B
a�B
abB
a|B
a�B
a|B
a�B
a�B
a�B
bB
bNB
bNB
b�B
b4B
b�B
b�B
cB
c�B
d@B
dZB
d@B
d�B
d�B
d�B
e,B
eFB
e�B
e�B
e�B
e�B
e�B
fB
ffB
f�B
f�B
f�B
f�B
g8B
hsB
h�B
iB
i�B
iyB
i�B
i�B
i_B
i_B
j�B
k�B
k�B
l=B
l"B
l�B
lqB
l�B
lWB
lqB
lqB
lqB
l�B
lWB
l"B
lWB
lWB
l�B
l�B
mCB
mCB
m)B
l�B
mCB
m�B
nB
m�B
m�B
m�B
n/B
o�B
p;B
pB
pB
p�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
rB
rB
raB
r�B
r�B
sB
sB
sB
sB
sB
sMB
shB
s�B
tB
tB
t9B
tTB
t�B
t�B
uB
uB
u?B
u%B
utB
u�B
u�B
u�B
u�B
vB
vB
v+B
vzB
v�B
v�B
v�B
v�B
v�B
wLB
wfB
wLB
w�B
w�B
xB
x�B
y$B
y	B
y$B
y	B
y>B
y�B
yrB
y�B
z*B
zDB
z^B
z�B
z�B
zxB
z�B
z�B
z�B
z�B
z�B
{0B
{dB
{B
{0B
{�B
|PB
|�B
|�B
}<B
}<B
}qB
}�B
}�B
}�B
~(B
~wB
~�B
~�B
~�B
~�B
~�B
.B
� 111111111111111111111111111111111111111111111111111111111144411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104846  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172426  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172426  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172426                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022434  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022434  QCF$                G�O�G�O�G�O�            4000JA  ARUP                                                                        20220610131506                      G�O�G�O�G�O�                