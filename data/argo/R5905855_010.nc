CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:12:22Z creation;2022-06-04T19:12:23Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191222  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               
A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @ؾ����u1   @ؾ�3���@0(�\�d��Q�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A!��AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx��B~ffB���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B癚B�  B�  B�  B�  B�  C   C  C  C33C�fC	�fC  C  C  C  C  C  C  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:�C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C��C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%�fD&fD&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�<�Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D��3D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@
=@��@��A ��A"�]AB�]A`��A�z�A�z�A�z�A�z�A�z�A�z�A�z�A�z�B =qB=qB=qB=qB =qB(=qB0=qB8=qB@=qBH=qBP=qBX=qB`=qBh=qBp=qBy
>B~��B��B��B��B��B��B��B��B��B��B��B��B��B��B�Q�B��B��B��B��B��B��B��B��B��B��B��B�RB��B��B��B��B��C \C\C\CB�C��C	��C\C\C\C\C\C\C\C\C��C\C \C"\C$\C&\C(\C*\C,\C.\C0\C2\C4\C6\C8(�C:(�C<\C>\C@\CB\CD\CF\CH\CJ\CL\CN\CP\CR\CT\CV\CW��CY��C\\C^\C`\Cb\Cd\Cf\Ch\Cj\Cl\Cn\Cp\Cr\Ct\Cv\Cx\Cz\C|\C~\C��C��C�{C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C�{C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C���C��C��C��C��C��C��C��C��C��C�{C�{C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%�=D&
=D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�ED���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D�D���D��D�A�DÁ�D���D��D�A�Dā�D���D��D�A�DŁ�D���D��D�A�DƁ�D���D��D�A�Dǁ�D���D��D�A�Dȁ�D���D��D�>�DɁ�D���D��D�A�Dʁ�D���D��D�A�Dˁ�D���D��D�A�D́�D���D��D�A�D́�D���D��D�A�D΁�D���D��D�A�Dρ�D���D��D�A�DЁ�D���D��D�A�Dс�D���D��D�A�Dҁ�D���D��D�A�DӁ�D���D��D�A�Dԁ�D���D��D�A�DՁ�D���D��D�A�Dց�D���D��D�A�Dׁ�D���D��D�A�D؁�D���D��D�A�Dف�D���D��D�A�Dځ�D���D��D�A�Dہ�D���D��D�A�D܁�D���D��D�A�D݁�D���D��D�A�Dށ�D���D��D�A�D߁�D���D��D�A�D��D��D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D���D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D��D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��D�A�D��D���D��D�A�D���D���D��D�A�D���D���D��D�A�D���D���D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�A�YA�.A��A��BAӲ-Aӗ�A�[WA�)�A��cA��A���A��}AҿHAҪ0AҝAҊ�A�o5A�FA��.Aѓ�A�ŢA�6�A��A�S&A��DA���A�0�A�9XA��AΨ�A��A̘+A�CAɍ�Aȡ�A�A��AǵA�e�AƲ�A��KAĤ�A�1AÆ�A�@�A��,A�PHA��A��A�z�A���A�&�A���A���A���A��A�ޞA���A�(�A�e`A�x�A�jA��sA��A��&A���A��IA���A���A� �A���A�
rA�,=A���A���A�zxA���A�XEA�~(A�{A��4A�VA�d&A���A��A���A�b�A���A��	A���A��A�q�A��aA��cA��VA��A~6zA|AyAxkQAtS&ApxlAn/�Alg�Ah0�Af�)Ad��Aa'RA^�vA[~�AY�AWںAW~AVSAT�AR�AN��AL��AL�AJ�PAK�AJ�)AJ=�AI��AI  AG=�AF�AE��AC��A>�A:S�A8�rA7�:A7`BA5��A2��A1?�A/��A/1�A.��A/A.��A-Z�A+�A'�1A&}VA%��A%�A$A!H�A�EA��A��A�}A#:A�fA�rAB[A �A��ACAƨA�A�$A��A�AqvAw�An/A�A��A�`A|�A`BA	Ap;A1A;�AdZA�AAiDA(�A��A�KA.IA
�A�AcA?�A�~A��A��A;�A��As�A�"A��AN�AFA
�ZA
$�A�	A��A��A��AZ�A}�A�6A:�A�"A3�A;dAm]A-�A�sA��AF�AC�A^5A �>@���@�1'@� \@�J�@���@��"@��j@�M@�O@���@���@�8�@��0@�@�Ĝ@��@�9�@�S�@��Y@��@��}@��@�p;@���@��@�$�@��m@�@@�@� i@�9X@��N@��@���@�F@���@�8�@��B@��@詓@�V�@��@��@篸@�4�@�@勬@�iD@�+@�-�@��@�?@��@��@�'@��5@�"h@�Y@ެ@�z@�1�@�~�@��5@�U2@ۥ@�K�@��v@�"h@�s�@ؔF@�u@׬q@��@���@��@��@��'@�bN@��@��@տH@��y@ԂA@��@�m�@�x@� \@з�@���@Ч@�\�@�=q@�}�@�]�@�c@ϨX@�ߤ@��@���@�K^@�G@˜@��2@ʎ�@ʎ�@�5?@��j@ɯ�@ɜ�@�/@�-�@ƞ@�Dg@��A@� i@���@�C@�~(@�{J@�B�@��A@�h
@�B[@��3@��4@�F�@��s@�a|@��@��@��S@�N<@��f@�� @�n�@�:�@���@�]�@�%@�J�@���@���@���@�dZ@�e�@�C�@�!-@��@�1@�B�@��y@�6@�x@��@��h@�;�@�  @��6@�c�@�7L@�	l@���@�oi@�:*@�~@��@��@��V@��"@�x@�O@�+@��b@�Z�@�2�@��@�N<@��@���@�d�@���@�q@�
=@���@���@�[�@�
�@���@�>�@�)_@�+@�%@��/@���@�I�@���@��~@�Mj@��8@���@���@�H�@��@��@��@@�B�@� i@���@�Ta@�2�@�x@���@�X�@�Ov@��D@��Q@��	@�X@��@��U@���@�bN@�;�@���@��@�A�@���@�kQ@�7�@���@��*@�g�@��H@�{@��3@�c�@�S@��@�r�@�#:@���@��@�_p@�A @�:�@�/@���@�y>@�I�@��9@��7@�:�@�@��s@�c @�'R@��Z@��@�N<@��@��@���@�l�@�4n@��3@�(@���@���@�-@���@���@��V@�c�@�=�@�6z@�%F@��@�o@��@���@��v@��X@��'@���@�z@�@��4@�Vm@��@���@��$@��@�>B@�  @�o�@�(@��U@�S�@��@��>@��@��@���@�n/@�Q�@��@���@��L@�;�@�	@�1@���@�@�ѷ@��O@�$@��Q@���@�U�@�J#@��@��)@���@�xl@�^5@�H@�*�@��@��@���@�qv@�Z�@�7L@��@��r@�1'@��D@��w@�~�@�m]@�e�@�T�@�=�@�@@���@��@�Ɇ@��e@���@�c�@�7�@���@��}@��@��"@�H�@�C@��6@�w�@�7�@��@�@��@��@��@�{@Z�@K�@
=@~��@~�@}O�@|�[@|�U@|�o@|@{��@{�@z��@z!�@y(�@x��@xh�@x �@w��@w�@vB[@u�^@uF@t��@t@s6z@r��@r6�@q�@qT�@p��@py>@o��@o��@oRT@oC@n�@n��@n)�@m�H@ma�@mV@l�@l,=@k��@j�y@j�@ji�@i�j@i4@hg8@hG@g�f@g i@f+k@e�Z@e�)@e�@ex�@d��@d��@c��@cW?@b}V@b=q@b8�@a��@a�@`��@`<�@_�+@_�$@_y�@_"�@^�@^�B@^u%@]�Z@\��@\~@[�@[�{@[�@Z��@Z��@Z�@Y�@Yzx@Y%F@X�)@Xr�@X6@W�@W�[@W�	@W�@V�@Vd�@V�@V�@U\�@T�/@T`�@T�@S�A@S��@S~�@S�@R��@R��@R�@Q�@Q�)@Q��@Q:�@Pe�@P*�@P�@O�@O˒@O]�@N�2@N�@NM�@N
�@M��@M��@Ms�@M5�@M�@L��@L�@K��@Ko@J͟@J��@J�!@Jp;@J@�@I�o@Ip�@I�@H�v@H��@H��@HH@H!@H@G�@G�V@Gs@G;d@G@F�,@F�@E�T@E�@E�~@D��@D��@D7�@D�@C� @C{J@CdZ@C@O@B�c@B\�@A��@A��@AL�@A�@@�@@��@@bN@?��@?��@?K�@>ߤ@>�@>M�@=��@=u�@=G�@<�K@<�@<Q�@<9X@;�@;�@;j�@;Mj@;+@;�@:��@:ff@:#:@9��@9m]@9&�@8�p@8��@8c�@8�@7�K@7RT@7�@6�@6�X@6�+@66�@5�D@5��@55�@4�@4�I@4�D@49X@4�@3�K@3��@3�	@3t�@3a@3K�@3�@3@2��@2Q@2B[@24@1�N@1�X@1}�@1L�@1�@0ѷ@0�@0z�@0I�@0-�@0~@/�@/��@/E9@.��@.�b@.s�@.L0@.#:@-�@-��@-s�@-e,@-Dg@-@,�@,bN@,@+˒@+��@+O@+�@*҉@*�h@*u%@*Q@*5?@*�@)�@)��@)zx@)@(�@(c�@(>B@'�W@'��@'��@'j�@&ں@&��@&i�@&!�@%�>@%ϫ@%��@%��@%<6@$��@$��@$�.@#ݘ@#��@#6z@#
=@"��@"�A@"_�@"	@!�@!�=@!�@ ��@ ��@ ��@ ��@ ��@ ~(@ <�@ 'R@��@�[@��@�4@g�@=@�@�@��@V@ �@�=@�7@u�@:�@	l@�@�z@��@`�@,=@��@�F@��@�@s@;d@��@�s@�@�b@kQ@0U@J@�@��@k�@7L@V@�5@��@��@��@y>@g8@"h@�A@˒@��@W?@�@�M@��@��@�}@~�@a|@B[@.�@
�@��@@��@k�@B�@@@��@��@PH@-�@�0@�	@;d@'�@!-@Y@ i@ں@��@�r@z@H�@��@��@�@e,@L�@5�@%F@�@��@�@Ĝ@�@u�@Z@PH@"h@�W@ݘ@��@�@��@v`@\)@6z@)_@�@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�A�YA�.A��A��BAӲ-Aӗ�A�[WA�)�A��cA��A���A��}AҿHAҪ0AҝAҊ�A�o5A�FA��.Aѓ�A�ŢA�6�A��A�S&A��DA���A�0�A�9XA��AΨ�A��A̘+A�CAɍ�Aȡ�A�A��AǵA�e�AƲ�A��KAĤ�A�1AÆ�A�@�A��,A�PHA��A��A�z�A���A�&�A���A���A���A��A�ޞA���A�(�A�e`A�x�A�jA��sA��A��&A���A��IA���A���A� �A���A�
rA�,=A���A���A�zxA���A�XEA�~(A�{A��4A�VA�d&A���A��A���A�b�A���A��	A���A��A�q�A��aA��cA��VA��A~6zA|AyAxkQAtS&ApxlAn/�Alg�Ah0�Af�)Ad��Aa'RA^�vA[~�AY�AWںAW~AVSAT�AR�AN��AL��AL�AJ�PAK�AJ�)AJ=�AI��AI  AG=�AF�AE��AC��A>�A:S�A8�rA7�:A7`BA5��A2��A1?�A/��A/1�A.��A/A.��A-Z�A+�A'�1A&}VA%��A%�A$A!H�A�EA��A��A�}A#:A�fA�rAB[A �A��ACAƨA�A�$A��A�AqvAw�An/A�A��A�`A|�A`BA	Ap;A1A;�AdZA�AAiDA(�A��A�KA.IA
�A�AcA?�A�~A��A��A;�A��As�A�"A��AN�AFA
�ZA
$�A�	A��A��A��AZ�A}�A�6A:�A�"A3�A;dAm]A-�A�sA��AF�AC�A^5A �>@���@�1'@� \@�J�@���@��"@��j@�M@�O@���@���@�8�@��0@�@�Ĝ@��@�9�@�S�@��Y@��@��}@��@�p;@���@��@�$�@��m@�@@�@� i@�9X@��N@��@���@�F@���@�8�@��B@��@詓@�V�@��@��@篸@�4�@�@勬@�iD@�+@�-�@��@�?@��@��@�'@��5@�"h@�Y@ެ@�z@�1�@�~�@��5@�U2@ۥ@�K�@��v@�"h@�s�@ؔF@�u@׬q@��@���@��@��@��'@�bN@��@��@տH@��y@ԂA@��@�m�@�x@� \@з�@���@Ч@�\�@�=q@�}�@�]�@�c@ϨX@�ߤ@��@���@�K^@�G@˜@��2@ʎ�@ʎ�@�5?@��j@ɯ�@ɜ�@�/@�-�@ƞ@�Dg@��A@� i@���@�C@�~(@�{J@�B�@��A@�h
@�B[@��3@��4@�F�@��s@�a|@��@��@��S@�N<@��f@�� @�n�@�:�@���@�]�@�%@�J�@���@���@���@�dZ@�e�@�C�@�!-@��@�1@�B�@��y@�6@�x@��@��h@�;�@�  @��6@�c�@�7L@�	l@���@�oi@�:*@�~@��@��@��V@��"@�x@�O@�+@��b@�Z�@�2�@��@�N<@��@���@�d�@���@�q@�
=@���@���@�[�@�
�@���@�>�@�)_@�+@�%@��/@���@�I�@���@��~@�Mj@��8@���@���@�H�@��@��@��@@�B�@� i@���@�Ta@�2�@�x@���@�X�@�Ov@��D@��Q@��	@�X@��@��U@���@�bN@�;�@���@��@�A�@���@�kQ@�7�@���@��*@�g�@��H@�{@��3@�c�@�S@��@�r�@�#:@���@��@�_p@�A @�:�@�/@���@�y>@�I�@��9@��7@�:�@�@��s@�c @�'R@��Z@��@�N<@��@��@���@�l�@�4n@��3@�(@���@���@�-@���@���@��V@�c�@�=�@�6z@�%F@��@�o@��@���@��v@��X@��'@���@�z@�@��4@�Vm@��@���@��$@��@�>B@�  @�o�@�(@��U@�S�@��@��>@��@��@���@�n/@�Q�@��@���@��L@�;�@�	@�1@���@�@�ѷ@��O@�$@��Q@���@�U�@�J#@��@��)@���@�xl@�^5@�H@�*�@��@��@���@�qv@�Z�@�7L@��@��r@�1'@��D@��w@�~�@�m]@�e�@�T�@�=�@�@@���@��@�Ɇ@��e@���@�c�@�7�@���@��}@��@��"@�H�@�C@��6@�w�@�7�@��@�@��@��@��@�{@Z�@K�@
=@~��@~�@}O�@|�[@|�U@|�o@|@{��@{�@z��@z!�@y(�@x��@xh�@x �@w��@w�@vB[@u�^@uF@t��@t@s6z@r��@r6�@q�@qT�@p��@py>@o��@o��@oRT@oC@n�@n��@n)�@m�H@ma�@mV@l�@l,=@k��@j�y@j�@ji�@i�j@i4@hg8@hG@g�f@g i@f+k@e�Z@e�)@e�@ex�@d��@d��@c��@cW?@b}V@b=q@b8�@a��@a�@`��@`<�@_�+@_�$@_y�@_"�@^�@^�B@^u%@]�Z@\��@\~@[�@[�{@[�@Z��@Z��@Z�@Y�@Yzx@Y%F@X�)@Xr�@X6@W�@W�[@W�	@W�@V�@Vd�@V�@V�@U\�@T�/@T`�@T�@S�A@S��@S~�@S�@R��@R��@R�@Q�@Q�)@Q��@Q:�@Pe�@P*�@P�@O�@O˒@O]�@N�2@N�@NM�@N
�@M��@M��@Ms�@M5�@M�@L��@L�@K��@Ko@J͟@J��@J�!@Jp;@J@�@I�o@Ip�@I�@H�v@H��@H��@HH@H!@H@G�@G�V@Gs@G;d@G@F�,@F�@E�T@E�@E�~@D��@D��@D7�@D�@C� @C{J@CdZ@C@O@B�c@B\�@A��@A��@AL�@A�@@�@@��@@bN@?��@?��@?K�@>ߤ@>�@>M�@=��@=u�@=G�@<�K@<�@<Q�@<9X@;�@;�@;j�@;Mj@;+@;�@:��@:ff@:#:@9��@9m]@9&�@8�p@8��@8c�@8�@7�K@7RT@7�@6�@6�X@6�+@66�@5�D@5��@55�@4�@4�I@4�D@49X@4�@3�K@3��@3�	@3t�@3a@3K�@3�@3@2��@2Q@2B[@24@1�N@1�X@1}�@1L�@1�@0ѷ@0�@0z�@0I�@0-�@0~@/�@/��@/E9@.��@.�b@.s�@.L0@.#:@-�@-��@-s�@-e,@-Dg@-@,�@,bN@,@+˒@+��@+O@+�@*҉@*�h@*u%@*Q@*5?@*�@)�@)��@)zx@)@(�@(c�@(>B@'�W@'��@'��@'j�@&ں@&��@&i�@&!�@%�>@%ϫ@%��@%��@%<6@$��@$��@$�.@#ݘ@#��@#6z@#
=@"��@"�A@"_�@"	@!�@!�=@!�@ ��@ ��@ ��@ ��@ ��@ ~(@ <�@ 'R@��@�[@��@�4@g�@=@�@�@��@V@ �@�=@�7@u�@:�@	l@�@�z@��@`�@,=@��@�F@��@�@s@;d@��@�s@�@�b@kQ@0U@J@�@��@k�@7L@V@�5@��@��@��@y>@g8@"h@�A@˒@��@W?@�@�M@��@��@�}@~�@a|@B[@.�@
�@��@@��@k�@B�@@@��@��@PH@-�@�0@�	@;d@'�@!-@Y@ i@ں@��@�r@z@H�@��@��@�@e,@L�@5�@%F@�@��@�@Ĝ@�@u�@Z@PH@"h@�W@ݘ@��@�@��@v`@\)@6z@)_@�@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	vB	BB	�B	hB	�B	aB	B	7B	OB	#nB	'B	(�B	*0B	+QB	,B	-B	.cB	.�B	.�B	0B	7�B	A�B	GEB	� B	��B	��B	�nB	˒B	�CB	�B	�}B	��B	�:B	��B	�;B	�B	��B	�B	�B	�%B
�B
D�B
fLB
|�B
�B
�]B
�bB
�xB
�B
�B
�B	�B�B	B.}BDBO�BS�BYeBT�B^B^jBe�BWYBX�B�#B�$B�IB��B�B�xB�`B��B�B�hBȚB�ZB��B�MBK�B&2B:B
��B
�7B
��B
�lB
��B
�;B
c�B
R:B
9�B
(�B
 �B
�B
�B

=B
;B	��B	�B	�7B	�VB	�xB	��B	��B	�B	��B	s�B	g�B	Z�B	Q4B	N�B	C�B	;�B	5�B	.�B	%�B	�B	
#B��B��B��B	3B	�B	�B	B	 4B�B�8B	 iB	�B��B߾B�BB�B�B�B��B�XB�nB�B�>B	3B	!-B	-]B	&fB	DB	%B	�B	�B��B��B�MB�B��B	 �B	�B	uB	�B	B	�B	�B	�B	�B		RB	�B	�B	�B	1�B	B[B	H�B	G�B	?}B	D3B	M�B	G�B	>BB	@ B	@ B	L�B	TaB	l�B	p�B	t�B	t�B	x�B	y>B	��B	�SB	�B	�_B	��B	�'B	�
B	�-B	�B	�UB	��B	�\B	�B	��B	��B	��B	��B	��B	�HB	�FB	��B	�B	�	B	�CB	�xB	��B	�@B	�7B	��B	�9B	��B	�fB	�xB	��B	��B	��B	�JB	�B	�mB	�B	�-B	�B	��B	��B	��B	�B	��B	�`B	�pB	�#B	�	B	��B	�B	�B	�xB	��B	�B	�9B	��B	�xB	��B	�6B	�}B	�B	��B	�B	��B	̳B	ʦB	ǮB	�B	ĶB	�PB	��B	�(B	бB	�B	՛B	�
B	�
B	�
B	�B	�_B	�B	�_B	�B	�1B	ٴB	�B	چB	��B	��B	רB	��B	��B	��B	��B	�eB	�_B	՛B	өB	��B	ΊB	��B	��B	ɠB	�JB	�=B	�XB	�~B	�BB	�4B	ңB	��B	�B	�aB	��B	��B	�B	��B	�gB	ՁB	�2B	׍B	ڠB	�	B	�WB	چB	ڠB	ۦB	�B	�xB	ݘB	�/B	ߤB	��B	�B	�NB	��B	��B	�2B	�B	�B	�2B	��B	�B	�HB	�B	�tB	�zB	�B	�mB	�$B	�
B	�B	�
B	��B	�XB	�sB	�>B	�XB	��B	��B	��B	��B	��B	�_B	�KB	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	�B	�=B	�]B	��B	��B	��B	�cB	�/B	��B	��B	�5B	�B	��B	��B	�;B	�AB	�B	�B	�B	�3B	�B	�B	�B	�B	�B	�B	��B	�TB	�nB	�%B	�%B	�B	��B	��B	�FB	��B	��B	��B	��B	��B	��B	�$B	��B	�>B	�DB	�xB	�DB	�DB	�^B	�B	��B	��B	��B	��B	��B	�.B	��B	��B
 iB
 �B
 �B
 B
 B
B
 �B
 �B
oB
'B
�B
�B	��B	��B	��B	��B
  B
 OB
 �B
�B
�B
oB
'B
�B
�B
aB
{B
�B
�B
gB
�B
SB
�B
�B
�B
EB
�B
�B
�B
	�B
	�B

	B

XB

�B

�B
^B
�B
B
�B
6B
�B
<B
�B
vB
�B
�B
�B
NB
 B
NB
�B
�B
�B
�B
�B
�B
�B
�B
B
YB
KB
7B
QB
QB
QB
QB
QB
kB
�B
=B
�B
�B
�B
CB
/B
5B
�B
�B
B
5B
�B
�B
;B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
 B
!�B
"hB
"B
"NB
!�B
!HB
!�B
# B
"NB
"hB
# B
"�B
"�B
# B
"�B
"�B
#�B
#TB
#:B
#:B
#�B
$&B
#�B
#�B
#�B
#�B
$�B
%�B
&LB
&�B
&�B
'B
'8B
'�B
'�B
'�B
(
B
'�B
'�B
'�B
'�B
(
B
($B
(>B
)B
)B
)*B
)�B
*KB
*B
+�B
,qB
-)B
-B
-)B
-�B
.IB
.cB
.}B
.�B
.�B
.�B
/5B
/5B
/�B
0�B
0�B
0�B
1B
0�B
0�B
1�B
1�B
1�B
1�B
1�B
2B
2aB
2�B
3B
3�B
4B
4B
4nB
4B
5ZB
6`B
6B
5�B
5tB
5�B
5�B
6B
6�B
6�B
7fB
7�B
7�B
6�B
72B
7�B
8B
8B
8�B
8�B
9	B
8�B
9>B
9rB
:*B
:^B
:�B
;B
<6B
<�B
<�B
<�B
="B
>B
>B
>�B
>�B
>�B
>�B
>�B
>�B
?B
?HB
?�B
?�B
@ B
@4B
@�B
AB
@�B
@�B
@OB
AUB
A�B
A�B
B�B
C-B
CaB
C�B
C�B
C�B
D3B
DB
DMB
DMB
DMB
D�B
DMB
DgB
D�B
EB
E�B
F%B
E�B
F%B
E�B
E�B
FtB
FtB
FtB
F�B
F�B
F�B
GB
G_B
G_B
GEB
G_B
G�B
H1B
H�B
H�B
H�B
H�B
IB
IB
IRB
I�B
I�B
J#B
J=B
JXB
JrB
J�B
J�B
KDB
KxB
LB
LJB
LdB
LJB
L�B
L�B
L�B
MPB
M�B
M�B
M�B
NB
N<B
NVB
NVB
NpB
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
PbB
PbB
QB
P�B
Q4B
QhB
QhB
QhB
Q�B
R:B
R�B
R�B
R�B
S&B
S&B
S[B
S�B
T�B
T�B
T�B
U2B
U2B
UgB
VB
VB
VB
VmB
V�B
V�B
V�B
W?B
W?B
W�B
W�B
W�B
W�B
XB
X_B
XyB
X�B
Y1B
YKB
YB
YB
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
Z�B
[#B
[qB
[�B
[�B
\B
\CB
\�B
\�B
\�B
]/B
]/B
]IB
]~B
]~B
]~B
]�B
]�B
]�B
^B
^jB
^OB
^jB
^�B
^�B
^�B
_B
_;B
_�B
_�B
_�B
_�B
`B
`B
`'B
`vB
`�B
`�B
a-B
aHB
abB
abB
a�B
a�B
a�B
a�B
bB
b4B
bNB
b�B
b�B
cB
c B
c:B
cnB
c�B
c�B
c�B
c�B
dB
dB
d@B
dZB
dZB
d�B
eB
e,B
e`B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
g8B
g8B
g8B
gRB
g�B
g�B
g�B
h
B
h�B
h�B
iDB
iDB
i�B
i�B
i�B
i�B
j0B
j0B
j�B
j�B
kB
j�B
kB
j�B
k6B
kQB
k6B
k�B
k�B
k�B
k�B
k�B
l"B
lqB
l�B
l�B
l�B
mB
mwB
mwB
mwB
m�B
m�B
m�B
nIB
nIB
ncB
n�B
n�B
o B
o5B
o5B
o5B
oOB
o�B
o�B
o�B
o�B
o�B
p!B
p!B
p!B
p;B
poB
p�B
p�B
p�B
q'B
qAB
q[B
q[B
qAB
q[B
q�B
q�B
q�B
r-B
rGB
r|B
raB
r|B
r�B
r�B
r�B
r�B
r�B
sB
s3B
s3B
shB
s�B
s�B
s�B
tB
t9B
tTB
t�B
t�B
u%B
utB
utB
utB
utB
u�B
u�B
u�B
vB
u�B
v+B
vzB
v�B
v�B
v�B
v�B
v�B
v�B
wB
w2B
wB
wfB
w�B
w�B
w�B
w�B
w�B
x8B
xB
xRB
xRB
x�B
x�B
x�B
x�B
x�B
x�B
y$B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	vB	BB	�B	hB	�B	aB	B	7B	OB	#nB	'B	(�B	*0B	+QB	,B	-B	.cB	.�B	.�B	0B	7�B	A�B	GEB	� B	��B	��B	�nB	˒B	�CB	�B	�}B	��B	�:B	��B	�;B	�B	��B	�B	�B	�%B
�B
D�B
fLB
|�B
�B
�]B
�bB
�xB
�B
�B
�B	�B�B	B.}BDBO�BS�BYeBT�B^B^jBe�BWYBX�B�#B�$B�IB��B�B�xB�`B��B�B�hBȚB�ZB��B�MBK�B&2B:B
��B
�7B
��B
�lB
��B
�;B
c�B
R:B
9�B
(�B
 �B
�B
�B

=B
;B	��B	�B	�7B	�VB	�xB	��B	��B	�B	��B	s�B	g�B	Z�B	Q4B	N�B	C�B	;�B	5�B	.�B	%�B	�B	
#B��B��B��B	3B	�B	�B	B	 4B�B�8B	 iB	�B��B߾B�BB�B�B�B��B�XB�nB�B�>B	3B	!-B	-]B	&fB	DB	%B	�B	�B��B��B�MB�B��B	 �B	�B	uB	�B	B	�B	�B	�B	�B		RB	�B	�B	�B	1�B	B[B	H�B	G�B	?}B	D3B	M�B	G�B	>BB	@ B	@ B	L�B	TaB	l�B	p�B	t�B	t�B	x�B	y>B	��B	�SB	�B	�_B	��B	�'B	�
B	�-B	�B	�UB	��B	�\B	�B	��B	��B	��B	��B	��B	�HB	�FB	��B	�B	�	B	�CB	�xB	��B	�@B	�7B	��B	�9B	��B	�fB	�xB	��B	��B	��B	�JB	�B	�mB	�B	�-B	�B	��B	��B	��B	�B	��B	�`B	�pB	�#B	�	B	��B	�B	�B	�xB	��B	�B	�9B	��B	�xB	��B	�6B	�}B	�B	��B	�B	��B	̳B	ʦB	ǮB	�B	ĶB	�PB	��B	�(B	бB	�B	՛B	�
B	�
B	�
B	�B	�_B	�B	�_B	�B	�1B	ٴB	�B	چB	��B	��B	רB	��B	��B	��B	��B	�eB	�_B	՛B	өB	��B	ΊB	��B	��B	ɠB	�JB	�=B	�XB	�~B	�BB	�4B	ңB	��B	�B	�aB	��B	��B	�B	��B	�gB	ՁB	�2B	׍B	ڠB	�	B	�WB	چB	ڠB	ۦB	�B	�xB	ݘB	�/B	ߤB	��B	�B	�NB	��B	��B	�2B	�B	�B	�2B	��B	�B	�HB	�B	�tB	�zB	�B	�mB	�$B	�
B	�B	�
B	��B	�XB	�sB	�>B	�XB	��B	��B	��B	��B	��B	�_B	�KB	�B	�B	�B	�B	�B	��B	�B	�B	�B	��B	�B	�=B	�]B	��B	��B	��B	�cB	�/B	��B	��B	�5B	�B	��B	��B	�;B	�AB	�B	�B	�B	�3B	�B	�B	�B	�B	�B	�B	��B	�TB	�nB	�%B	�%B	�B	��B	��B	�FB	��B	��B	��B	��B	��B	��B	�$B	��B	�>B	�DB	�xB	�DB	�DB	�^B	�B	��B	��B	��B	��B	��B	�.B	��B	��B
 iB
 �B
 �B
 B
 B
B
 �B
 �B
oB
'B
�B
�B	��B	��B	��B	��B
  B
 OB
 �B
�B
�B
oB
'B
�B
�B
aB
{B
�B
�B
gB
�B
SB
�B
�B
�B
EB
�B
�B
�B
	�B
	�B

	B

XB

�B

�B
^B
�B
B
�B
6B
�B
<B
�B
vB
�B
�B
�B
NB
 B
NB
�B
�B
�B
�B
�B
�B
�B
�B
B
YB
KB
7B
QB
QB
QB
QB
QB
kB
�B
=B
�B
�B
�B
CB
/B
5B
�B
�B
B
5B
�B
�B
;B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
 B
!�B
"hB
"B
"NB
!�B
!HB
!�B
# B
"NB
"hB
# B
"�B
"�B
# B
"�B
"�B
#�B
#TB
#:B
#:B
#�B
$&B
#�B
#�B
#�B
#�B
$�B
%�B
&LB
&�B
&�B
'B
'8B
'�B
'�B
'�B
(
B
'�B
'�B
'�B
'�B
(
B
($B
(>B
)B
)B
)*B
)�B
*KB
*B
+�B
,qB
-)B
-B
-)B
-�B
.IB
.cB
.}B
.�B
.�B
.�B
/5B
/5B
/�B
0�B
0�B
0�B
1B
0�B
0�B
1�B
1�B
1�B
1�B
1�B
2B
2aB
2�B
3B
3�B
4B
4B
4nB
4B
5ZB
6`B
6B
5�B
5tB
5�B
5�B
6B
6�B
6�B
7fB
7�B
7�B
6�B
72B
7�B
8B
8B
8�B
8�B
9	B
8�B
9>B
9rB
:*B
:^B
:�B
;B
<6B
<�B
<�B
<�B
="B
>B
>B
>�B
>�B
>�B
>�B
>�B
>�B
?B
?HB
?�B
?�B
@ B
@4B
@�B
AB
@�B
@�B
@OB
AUB
A�B
A�B
B�B
C-B
CaB
C�B
C�B
C�B
D3B
DB
DMB
DMB
DMB
D�B
DMB
DgB
D�B
EB
E�B
F%B
E�B
F%B
E�B
E�B
FtB
FtB
FtB
F�B
F�B
F�B
GB
G_B
G_B
GEB
G_B
G�B
H1B
H�B
H�B
H�B
H�B
IB
IB
IRB
I�B
I�B
J#B
J=B
JXB
JrB
J�B
J�B
KDB
KxB
LB
LJB
LdB
LJB
L�B
L�B
L�B
MPB
M�B
M�B
M�B
NB
N<B
NVB
NVB
NpB
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
PbB
PbB
QB
P�B
Q4B
QhB
QhB
QhB
Q�B
R:B
R�B
R�B
R�B
S&B
S&B
S[B
S�B
T�B
T�B
T�B
U2B
U2B
UgB
VB
VB
VB
VmB
V�B
V�B
V�B
W?B
W?B
W�B
W�B
W�B
W�B
XB
X_B
XyB
X�B
Y1B
YKB
YB
YB
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
Z�B
[#B
[qB
[�B
[�B
\B
\CB
\�B
\�B
\�B
]/B
]/B
]IB
]~B
]~B
]~B
]�B
]�B
]�B
^B
^jB
^OB
^jB
^�B
^�B
^�B
_B
_;B
_�B
_�B
_�B
_�B
`B
`B
`'B
`vB
`�B
`�B
a-B
aHB
abB
abB
a�B
a�B
a�B
a�B
bB
b4B
bNB
b�B
b�B
cB
c B
c:B
cnB
c�B
c�B
c�B
c�B
dB
dB
d@B
dZB
dZB
d�B
eB
e,B
e`B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
g8B
g8B
g8B
gRB
g�B
g�B
g�B
h
B
h�B
h�B
iDB
iDB
i�B
i�B
i�B
i�B
j0B
j0B
j�B
j�B
kB
j�B
kB
j�B
k6B
kQB
k6B
k�B
k�B
k�B
k�B
k�B
l"B
lqB
l�B
l�B
l�B
mB
mwB
mwB
mwB
m�B
m�B
m�B
nIB
nIB
ncB
n�B
n�B
o B
o5B
o5B
o5B
oOB
o�B
o�B
o�B
o�B
o�B
p!B
p!B
p!B
p;B
poB
p�B
p�B
p�B
q'B
qAB
q[B
q[B
qAB
q[B
q�B
q�B
q�B
r-B
rGB
r|B
raB
r|B
r�B
r�B
r�B
r�B
r�B
sB
s3B
s3B
shB
s�B
s�B
s�B
tB
t9B
tTB
t�B
t�B
u%B
utB
utB
utB
utB
u�B
u�B
u�B
vB
u�B
v+B
vzB
v�B
v�B
v�B
v�B
v�B
v�B
wB
w2B
wB
wfB
w�B
w�B
w�B
w�B
w�B
x8B
xB
xRB
xRB
x�B
x�B
x�B
x�B
x�B
x�B
y$B
x�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105227  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191222  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191223  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191223                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041230  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041230  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                