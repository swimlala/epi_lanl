CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:17:15Z creation;2022-06-04T19:17:15Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191715  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               &A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��q���1   @���ۗ@.� ě��c���S��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @   @�33@�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A���B   B  B  B  B   B(  B0  B8ffB@ffBH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�33B���B���B�  B�  B���B�  B�  B�  B�33B���Bۙ�B�  B�  B�  B���B�  B�  B�  B�  C   C  C  C  C�C
�C�fC  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`�Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN�fDO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZfDZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� DnfDn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@"�\@�z�@�G�A ��A ��A?
=A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A��B (�B(�B(�B(�B (�B((�B0(�B8�\B@�\BH(�BP(�BX(�B`(�Bh(�Bp(�Bx(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B��HB�G�B��HB��HB�{B�{B��HB�{B�{B�{B�G�B��HBۮB�{B�{B�{B��HB�{B�{B�{B�{C 
=C
=C
=C
=C#�C
#�C�C
=C
=C
=C
=C
=C
=C
=C
=C
=C 
=C"
=C$
=C&
=C(
=C*
=C,
=C.
=C0
=C2#�C4
=C6
=C8
=C:
=C<
=C>
=C@
=CB
=CD
=CF
=CH
=CJ
=CL
=CN
=CP
=CR
=CT
=CV
=CX
=CZ
=C\
=C^#�C`#�Cb
=Cd
=Cf
=Ch
=Cj
=Cl
=Cn
=Cp
=Cr
=Ct
=Cv
=Cx
=Cz
=C|
=C~
=C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHDHD��HD�HD�AHDÁHD��HD�HD�AHDāHD��HD�HD�AHDŁHD��HD�HD�AHDƁHD��HD�HD�AHDǁHD��HD�HD�AHDȁHD��HD�HD�AHDɁHD��HD�HD�AHDʁHD��HD�HD�AHDˁHD��HD�HD�AHD́HD��HD�HD�AHD́HD��HD�HD�AHD΁HD��HD�HD�AHDρHD��HD�HD�AHDЁHD��HD�HD�AHDсHD��HD�HD�AHDҁHD��HD�HD�AHDӁHD��HD�HD�AHDԁHD��HD�HD�AHDՁHD��HD�HD�AHDցHD��HD�HD�AHDׁHD��HD�HD�AHD؁HD��HD�HD�AHDفHD��HD�HD�AHDځHD��HD�HD�AHDہHD��HD�HD�AHD܁HD��HD�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��A���A�  A�A�A�1A�DA�A�
�A�	lA�A��A�FA�A��A��A��A��A��A�4A�	7A�qA�(�A�+kA�+�A�-�A�-wA�(�A�&�A�{�A��A�MjAɆ�Aɯ�AɬAɝ~AɜxAɅ�A�m�A�4A���A�a|AƏ�A�6zA��"AÃGA�*eA �A���A�]dA��ZA��A��WA�+kA���A��A�t�A���A��A��tA�4nA���A��%A��aA�.A��A�.�A�+�A��A�A��CA�jA��A���A��{A�e�A��yA�՛A�(�A�eA��A�!�A�ݘA�tTA�)�A�_�A�&A��1A�-wA���A�3�A~|A|=Ay��Au�Ap\�Af�AaϫAZ�WAXN�AV��AS�AAL
�AGg8AD�AB�AA]�A?�hA=4A:�jA9��A7�KA7YA6dZA5��A3�jA1i�A/�A-OvA)(�A&Q�A&�A%�A%��A$A"�5A"W?A"�A!��A!|�A �rA ��A�AzxA�MA>�A֡A��A�A�wAN�A��A�FAB�A��AیAa|A��A��A�AA A�FAخA^�A+A��A��A�"A�Am�Af�A
��A
Y�A
SA	�{A�A��A��A��AیA	��A
 \A
�6A
�AW�AH�A�AVA��A
$�A
�A'�A
��A�A9XA��AYKA	A�A
�\An/A:�A�mA�<A��A��A�A �AیA�A��A+AA!Al�A��A�A �I@�Xy@��@���@�2�@�6z@���@���@�T�@���@�-�@�@���@���@�w�@���@�-w@�L�@�Ov@�;@�@��@��@�zx@�"@���@ꄶ@�kQ@�Ov@��@���@�;�@癚@�s�@�ݘ@�T�@䗍@�[@�@� �@�"@��	@�z@�a|@�e�@��@��@�
=@���@��'@�Z@��@��a@�Vm@ܻ�@܊r@�(�@ۖS@��@�m�@� �@�@٦�@�d�@׫�@�"�@փ�@�	@�}�@�GE@�s@�#�@���@���@�ѷ@��@�J�@���@���@ϟV@��y@�!�@͸�@�rG@�RT@�2a@���@�N�@˾w@ʃ�@���@�RT@��@�Ov@��@Ĉ�@�4@�;@��@£@�@�@���@��@���@�Q�@�9X@���@�8�@��b@�\�@�~(@�g8@���@�Y�@�$t@��4@��@��=@�=q@��;@��A@��W@�S�@�v�@�(�@�c�@��+@�?�@�ݘ@���@�a�@��.@���@��V@���@��h@�K�@��@��A@��m@��n@�O�@�*0@�n�@�G@�m]@��@��`@��B@��@��@�d�@�@�@�2�@��@��K@�u�@��m@�2�@�˒@���@�33@��@���@�i�@�2�@�M@��w@���@���@��@�\)@�ں@���@�"h@�ϫ@�1�@��@���@��x@���@�L0@���@�xl@�.�@��)@���@��=@�w2@�@O@��@��P@���@�=q@���@�qv@�;@��6@�n�@�S�@�O@��A@���@�x@�/�@���@�C�@��@��Z@���@��-@���@�C@��O@�-�@��K@��h@�#�@��/@���@�"h@��0@�_p@��P@���@�:*@���@��@�dZ@�=@��K@�Q@��@���@�iD@�%F@��v@���@���@��@��d@���@���@�m]@�,�@�+@���@���@�6@��T@���@���@�~�@�g�@�Y@��B@���@�ff@�_@�Ov@�x@�j�@�F@��@��@�J�@�@���@���@��:@�@O@��y@���@��,@��F@�8�@��@���@�u�@�o @��@���@���@�q@�N�@��@��'@���@��@��b@�� @��.@���@�h
@�0U@�@���@�}�@��@���@���@�}�@�IR@�1�@�0�@�+@��@�@���@��X@��u@�Xy@���@���@��h@�w2@�Q�@�/@���@��9@�?@�J@���@���@��	@�`B@�*0@�@��@��6@�^5@�H@��@��T@��w@��:@�Y�@��@��@��9@�U2@��@�Y�@�4�@�"�@��E@��6@��F@�d�@�C�@�4n@�e@g�@~�@~��@~O@|�@|�@|1@{Z�@z�@z��@zz@zu@y��@y�@x`�@w��@wW?@w33@v�!@v)�@u��@u(�@u�@tɆ@t�.@tm�@tZ@s�r@s�@s>�@r�@r�+@rxl@r� @rOv@r-@r4@r
�@r �@q��@q��@q�@q!�@pA�@o��@o'�@n�"@n�\@n�@m��@mS&@m@l�@l�@k��@k�P@kS@j�@jxl@jp;@ja|@j	@i�d@i@i�t@iB�@h�E@h��@hg8@g�@g�:@f�@f��@f� @f?@f	@e�9@e+@d��@d�@dѷ@d�@cخ@c)_@b҉@bYK@b�@a��@a8�@`��@`U2@_e�@^�]@]��@]�@]�3@]e,@]�@\��@\*�@[�@[�@[�@@["�@Z��@Z��@ZGE@Y�7@Y-w@X��@X�@W��@WC�@V��@V�@V��@V8�@V�@U��@UF@T��@T�@S�g@Sv`@R�X@R�@Q�j@Q�@Q��@Qc�@P�5@P>B@O��@O]�@OS@N��@NGE@M�h@M%@L�O@LXy@LG@K�r@K�@K9�@Jߤ@J?@I�N@I�~@IL�@I#�@H��@HA�@G��@GU�@F�y@FL0@F)�@E�j@E�S@E+�@D�@D4n@C��@C�@B�@Bff@A��@A=�@@��@@@?�6@?o�@?RT@?>�@?33@?�@>ں@>}V@>�@=�@<�_@<e�@<6@;��@;@O@;'�@:�'@:��@:Z�@:�@9��@9�t@9��@9j@9�@8~(@8<�@8�@7�Q@7��@7s@7A�@7@6� @6M�@6 �@5�^@5[W@5&�@4�@4�O@4h�@4Ft@4/�@3�W@3��@3dZ@3Mj@2�M@2��@21�@2u@1��@1�@1Y�@1N<@1*0@0�@0u�@0*�@0  @/�W@/˒@/ƨ@/RT@.��@.�x@.{�@.h
@.W�@.L0@.H�@.5?@.+k@.J@-�#@-��@-�@-�-@-�'@-j@,�K@,`�@,C-@,/�@,�@+�&@+�4@+4�@+�@*��@*͟@*�@*�b@*^5@)��@)�@)e,@)\�@)L�@(�v@(g8@'�+@'�@@'=@' i@&u%@&-@&�@%�@%��@%�3@%hs@%�@$��@$��@$~(@$1@#�@#��@#�&@#��@#�f@#6z@#@"�M@"�1@";�@".�@"J@!�Z@!�o@!�N@!�S@!Dg@ �K@ �[@ �@ g8@ (�@ �@��@W?@ i@͟@�@Ov@=q@O@��@�C@?}@�@%@��@��@�D@N�@<�@,=@�&@s@C�@
=@�@��@i�@ԕ@��@\�@0�@q@�@;@�@��@M@"h@x@�@�@��@X�@>�@�@͟@kQ@��@��@��@^�@#�@�@��@�E@�@��@�@~(@7@� @�V@Z�@o@�h@z@Z�@J�@+k@��@�@��@o @X@<6@�@��@��@r�@Ft@@�&@��@��@o�@b�@Mj@�@�,@�x@u%@a|@=q@�@��@�@�t@�M@\�@(�@�@�@��@Ĝ@��@��@l"@6@"h@�@ƨ@�$@x@;d@
��@
��@
GE@
�@	�)@	��@	��@	X@	V@�E@Ĝ@�@Ft@Ft@'R@	�@��@��@�@�4@dZ@RT@J#111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A��A���A�  A�A�A�1A�DA�A�
�A�	lA�A��A�FA�A��A��A��A��A��A�4A�	7A�qA�(�A�+kA�+�A�-�A�-wA�(�A�&�A�{�A��A�MjAɆ�Aɯ�AɬAɝ~AɜxAɅ�A�m�A�4A���A�a|AƏ�A�6zA��"AÃGA�*eA �A���A�]dA��ZA��A��WA�+kA���A��A�t�A���A��A��tA�4nA���A��%A��aA�.A��A�.�A�+�A��A�A��CA�jA��A���A��{A�e�A��yA�՛A�(�A�eA��A�!�A�ݘA�tTA�)�A�_�A�&A��1A�-wA���A�3�A~|A|=Ay��Au�Ap\�Af�AaϫAZ�WAXN�AV��AS�AAL
�AGg8AD�AB�AA]�A?�hA=4A:�jA9��A7�KA7YA6dZA5��A3�jA1i�A/�A-OvA)(�A&Q�A&�A%�A%��A$A"�5A"W?A"�A!��A!|�A �rA ��A�AzxA�MA>�A֡A��A�A�wAN�A��A�FAB�A��AیAa|A��A��A�AA A�FAخA^�A+A��A��A�"A�Am�Af�A
��A
Y�A
SA	�{A�A��A��A��AیA	��A
 \A
�6A
�AW�AH�A�AVA��A
$�A
�A'�A
��A�A9XA��AYKA	A�A
�\An/A:�A�mA�<A��A��A�A �AیA�A��A+AA!Al�A��A�A �I@�Xy@��@���@�2�@�6z@���@���@�T�@���@�-�@�@���@���@�w�@���@�-w@�L�@�Ov@�;@�@��@��@�zx@�"@���@ꄶ@�kQ@�Ov@��@���@�;�@癚@�s�@�ݘ@�T�@䗍@�[@�@� �@�"@��	@�z@�a|@�e�@��@��@�
=@���@��'@�Z@��@��a@�Vm@ܻ�@܊r@�(�@ۖS@��@�m�@� �@�@٦�@�d�@׫�@�"�@փ�@�	@�}�@�GE@�s@�#�@���@���@�ѷ@��@�J�@���@���@ϟV@��y@�!�@͸�@�rG@�RT@�2a@���@�N�@˾w@ʃ�@���@�RT@��@�Ov@��@Ĉ�@�4@�;@��@£@�@�@���@��@���@�Q�@�9X@���@�8�@��b@�\�@�~(@�g8@���@�Y�@�$t@��4@��@��=@�=q@��;@��A@��W@�S�@�v�@�(�@�c�@��+@�?�@�ݘ@���@�a�@��.@���@��V@���@��h@�K�@��@��A@��m@��n@�O�@�*0@�n�@�G@�m]@��@��`@��B@��@��@�d�@�@�@�2�@��@��K@�u�@��m@�2�@�˒@���@�33@��@���@�i�@�2�@�M@��w@���@���@��@�\)@�ں@���@�"h@�ϫ@�1�@��@���@��x@���@�L0@���@�xl@�.�@��)@���@��=@�w2@�@O@��@��P@���@�=q@���@�qv@�;@��6@�n�@�S�@�O@��A@���@�x@�/�@���@�C�@��@��Z@���@��-@���@�C@��O@�-�@��K@��h@�#�@��/@���@�"h@��0@�_p@��P@���@�:*@���@��@�dZ@�=@��K@�Q@��@���@�iD@�%F@��v@���@���@��@��d@���@���@�m]@�,�@�+@���@���@�6@��T@���@���@�~�@�g�@�Y@��B@���@�ff@�_@�Ov@�x@�j�@�F@��@��@�J�@�@���@���@��:@�@O@��y@���@��,@��F@�8�@��@���@�u�@�o @��@���@���@�q@�N�@��@��'@���@��@��b@�� @��.@���@�h
@�0U@�@���@�}�@��@���@���@�}�@�IR@�1�@�0�@�+@��@�@���@��X@��u@�Xy@���@���@��h@�w2@�Q�@�/@���@��9@�?@�J@���@���@��	@�`B@�*0@�@��@��6@�^5@�H@��@��T@��w@��:@�Y�@��@��@��9@�U2@��@�Y�@�4�@�"�@��E@��6@��F@�d�@�C�@�4n@�e@g�@~�@~��@~O@|�@|�@|1@{Z�@z�@z��@zz@zu@y��@y�@x`�@w��@wW?@w33@v�!@v)�@u��@u(�@u�@tɆ@t�.@tm�@tZ@s�r@s�@s>�@r�@r�+@rxl@r� @rOv@r-@r4@r
�@r �@q��@q��@q�@q!�@pA�@o��@o'�@n�"@n�\@n�@m��@mS&@m@l�@l�@k��@k�P@kS@j�@jxl@jp;@ja|@j	@i�d@i@i�t@iB�@h�E@h��@hg8@g�@g�:@f�@f��@f� @f?@f	@e�9@e+@d��@d�@dѷ@d�@cخ@c)_@b҉@bYK@b�@a��@a8�@`��@`U2@_e�@^�]@]��@]�@]�3@]e,@]�@\��@\*�@[�@[�@[�@@["�@Z��@Z��@ZGE@Y�7@Y-w@X��@X�@W��@WC�@V��@V�@V��@V8�@V�@U��@UF@T��@T�@S�g@Sv`@R�X@R�@Q�j@Q�@Q��@Qc�@P�5@P>B@O��@O]�@OS@N��@NGE@M�h@M%@L�O@LXy@LG@K�r@K�@K9�@Jߤ@J?@I�N@I�~@IL�@I#�@H��@HA�@G��@GU�@F�y@FL0@F)�@E�j@E�S@E+�@D�@D4n@C��@C�@B�@Bff@A��@A=�@@��@@@?�6@?o�@?RT@?>�@?33@?�@>ں@>}V@>�@=�@<�_@<e�@<6@;��@;@O@;'�@:�'@:��@:Z�@:�@9��@9�t@9��@9j@9�@8~(@8<�@8�@7�Q@7��@7s@7A�@7@6� @6M�@6 �@5�^@5[W@5&�@4�@4�O@4h�@4Ft@4/�@3�W@3��@3dZ@3Mj@2�M@2��@21�@2u@1��@1�@1Y�@1N<@1*0@0�@0u�@0*�@0  @/�W@/˒@/ƨ@/RT@.��@.�x@.{�@.h
@.W�@.L0@.H�@.5?@.+k@.J@-�#@-��@-�@-�-@-�'@-j@,�K@,`�@,C-@,/�@,�@+�&@+�4@+4�@+�@*��@*͟@*�@*�b@*^5@)��@)�@)e,@)\�@)L�@(�v@(g8@'�+@'�@@'=@' i@&u%@&-@&�@%�@%��@%�3@%hs@%�@$��@$��@$~(@$1@#�@#��@#�&@#��@#�f@#6z@#@"�M@"�1@";�@".�@"J@!�Z@!�o@!�N@!�S@!Dg@ �K@ �[@ �@ g8@ (�@ �@��@W?@ i@͟@�@Ov@=q@O@��@�C@?}@�@%@��@��@�D@N�@<�@,=@�&@s@C�@
=@�@��@i�@ԕ@��@\�@0�@q@�@;@�@��@M@"h@x@�@�@��@X�@>�@�@͟@kQ@��@��@��@^�@#�@�@��@�E@�@��@�@~(@7@� @�V@Z�@o@�h@z@Z�@J�@+k@��@�@��@o @X@<6@�@��@��@r�@Ft@@�&@��@��@o�@b�@Mj@�@�,@�x@u%@a|@=q@�@��@�@�t@�M@\�@(�@�@�@��@Ĝ@��@��@l"@6@"h@�@ƨ@�$@x@;d@
��@
��@
GE@
�@	�)@	��@	��@	X@	V@�E@Ĝ@�@Ft@Ft@'R@	�@��@��@�@�4@dZ@RT@J#111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	=�B	=�B	=�B	=�B	=qB	=�B	=qB	=qB	=qB	=VB	=�B	=�B	=�B	=qB	=VB	>(B	>]B	="B	="B	DMB	QNB	i�B	�oB	�xB	�"B	�BB	��B	�TB	�@B	��B	�_B
B[B
c�B
�#B
�cB
��B
��B
�B
��B
�=B
��B�B-]BU�Bj�By�B}"B�B|�Bw�Br�B��B�#B�B�)B��B�B��B��B��B��B��B�3B��B�B�2B�B�7B�2B��B��BI�B;�B��B��B��BcBv�Bh�BIB4�B�B�BKB
��B
��B
l�B
B�B
�B
+B
	�B
(B
	�B	�dB	��B	�B	�aB	}"B	SuB	&�B	uB	�B	3B��B��B�B�1B�gB��B��B��B��B�B��B�B�GB��B��B��B��B�vB��B��B��B��B��B�*B��B��B��B��B�tB��B�B��B��B��B��B��B�fB��B�jB��B�B��B��BĶB��BƎBɆB�VB�NB��B��B��BۦB�B�NB�B�cB�B�B�B�xB	�B	�B	B	bB	�B	 'B	&�B	5?B	MPB	_�B	c�B	o�B	�aB	�#B	�MB	��B	��B	�)B	��B	��B	��B	�rB	�"B	�.B	�bB	�B	ɠB	�RB	��B	�B	�B	��B	��B	��B	�B	��B	�yB	��B	�wB	�`B	��B	�CB	��B	�fB	��B	�*B	�8B	��B	��B	��B	��B	�B	�B	��B	�B	��B	�B	�(B	�pB	͹B	�jB	�xB	�	B	ƎB	ɺB	ɠB	ȴB	�B	ȴB	�_B	��B	ƨB	�?B	ŢB	ĶB	�3B	�{B	��B	��B	� B	�B	��B	��B	��B	�BB	��B	�}B	�iB	�aB	ðB	��B	�3B	�gB	ŢB	ƨB	��B	�KB	�rB	��B	�B	�JB	�"B	�pB	οB	�BB	��B	��B	҉B	�&B	�FB	��B	ԯB	ԕB	ּB	�sB	׍B	׍B	�?B	�$B	�_B	��B	�1B	ؓB	�=B	ۦB	�xB	ܬB	��B	��B	��B	��B	��B	�=B	�B	��B	�NB	�HB	�(B	��B	�\B	ЗB	�}B	��B	� B	уB	�:B	�,B	��B	��B	�B	��B	�B	�B	یB	�dB	�~B	�B	�~B	�~B	ݘB	�IB	��B	�B	�B	�@B	�FB	��B	�B	�*B	��B	��B	�B	�sB	�>B	�yB	�B	��B	�B	��B	�GB	�MB	�B	�B	��B	�B	�9B	�B	�?B	�?B	��B	�2B	��B	��B	��B	��B	�B	�8B	�8B	�RB	��B	��B	��B	�rB	��B	��B	�B	�DB	�JB	��B	��B	�B	�PB	�B	�B	�6B	�6B	��B	�B	�jB	�PB	�"B	��B	��B	��B	��B	��B
 �B
B
B
oB
B
AB
uB
B
�B
�B
aB
�B
MB
MB
�B
�B
�B
SB
�B
B
YB
%B
tB
%B
%B
B
�B
B
�B
�B
tB
tB
�B
	B
	B
�B
	7B
	B

#B

�B
�B
�B
JB
JB
dB
�B
�B
�B
B
PB
6B
�B
pB
BB
(B
(B
�B
�B
�B
HB
HB
�B
 B
 B
 B
�B
oB
B
uB
�B
�B
�B
FB
�B
2B
�B
gB
gB
�B
�B
�B
�B
�B
EB
B
�B
7B
�B
�B
�B
B
OB
�B
OB
jB
B
�B
;B
VB
pB
!B
B
B
�B
�B
VB
VB
VB
pB
pB
pB
�B
�B
�B
 'B
 'B
!-B
"NB
#TB
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#TB
#TB
#nB
#�B
"�B
#B
# B
#�B
#�B
$ZB
$�B
$�B
$�B
%B
%B
%,B
%FB
%�B
%�B
%�B
&B
&LB
&2B
&�B
'B
'B
'B
'RB
'�B
'�B
'�B
(>B
)DB
+B
+B
+B
,WB
,�B
,�B
-)B
-]B
-CB
-)B
-�B
.IB
.cB
.}B
/�B
/iB
0!B
0�B
0�B
1B
1'B
1�B
1�B
2-B
2�B
3B
3hB
3�B
49B
4B
4�B
5?B
5�B
6�B
72B
7�B
8B
8�B
8�B
8�B
8�B
8�B
9�B
:�B
;�B
;�B
<B
<B
<6B
<6B
<B
<B
<6B
;�B
;�B
;�B
;�B
<B
<6B
<6B
<6B
<B
<B
<6B
<�B
<�B
=VB
=qB
=�B
>BB
?B
>�B
?.B
?cB
?�B
@4B
?�B
?�B
?}B
?}B
?}B
?�B
?�B
@4B
@�B
@�B
A B
A�B
A�B
A�B
AoB
A�B
A�B
BB
B[B
BuB
BuB
B'B
A�B
A B
A;B
A�B
A�B
B�B
CB
CGB
C�B
DB
DgB
EB
EmB
EmB
EmB
E�B
E�B
E�B
FtB
F?B
FtB
F�B
F�B
G+B
GB
GEB
GEB
G�B
HB
HB
H�B
H�B
H�B
IB
I7B
IRB
I�B
J�B
J�B
JrB
J�B
J�B
J�B
KxB
K�B
K�B
LB
L~B
L~B
M6B
M�B
M�B
NB
N"B
NB
NVB
N�B
N�B
OvB
O�B
O�B
PB
PB
PHB
P�B
QB
Q4B
QNB
Q�B
Q�B
Q�B
R:B
RoB
R�B
SB
S�B
T,B
T,B
T�B
T�B
T�B
UB
U�B
U�B
U�B
VB
VB
VB
VSB
VSB
V�B
V�B
W�B
XyB
X�B
XyB
X�B
YB
X�B
Y1B
X�B
X�B
YB
YKB
YeB
YB
Y�B
Y�B
Z7B
Z�B
Z�B
Z�B
Z�B
Z�B
[	B
[#B
[�B
[�B
\)B
\CB
\]B
\xB
\�B
\�B
\�B
\�B
\�B
]/B
]~B
]~B
]~B
]�B
^B
^5B
^OB
^�B
^�B
^�B
^�B
^�B
_pB
_VB
_�B
_�B
_�B
_�B
_pB
_�B
`\B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
aB
`�B
`�B
`�B
aB
a�B
a�B
a�B
a�B
bB
bNB
b�B
b�B
b�B
cB
cB
c B
cB
c:B
c�B
c�B
c�B
c�B
c�B
d@B
d�B
eB
e,B
e`B
ezB
fB
f2B
fLB
fLB
fLB
ffB
f�B
f�B
gB
g8B
gRB
g�B
g�B
g�B
g�B
g�B
h
B
hsB
hsB
hsB
h�B
i*B
iB
iDB
i_B
iDB
i_B
i�B
i�B
jB
j0B
jKB
j�B
j�B
j�B
kB
k6B
k�B
k�B
k�B
l=B
l=B
l=B
l�B
l�B
mCB
m]B
m]B
m]B
m�B
m�B
n/B
n/B
n/B
ncB
n�B
o B
o5B
o�B
oiB
o�B
p;B
poB
p�B
p�B
p�B
p�B
p�B
p�B
qAB
q�B
q�B
q�B
q�B
q�B
rB
raB
raB
r�B
r�B
sB
s�B
s�B
s�B
s�B
tB
t9B
tTB
tTB
tnB
t�B
t�B
t�B
t�B
u%B
uZB
utB
u�B
v+B
vzB
v`B
vzB
v�B
v�B
v�B
wB
w2B
wLB
wfB
w�B
xB
x8B
x8B
xlB
x�B
x�B
x�B
y	B
y$B
y>B
y>B
y�B
y�B
zB
z*B
zDB
z�B
z�B
z�B
z�B
z�B
{0B
{dB
{B
{B
{�B
{�B
{�B
|B
|B
|B
|PB
|PB
|�B
|�B
|�B
}B
}B
}VB
}VB
}�B
}�B
}�B
~(B
~BB
~]B
~�B
B
~�B
}B
�B
�B
�B
�B
�B
� B
�4B
�OB
�iB
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	=�B	=�B	=�B	=�B	=qB	=�B	=qB	=qB	=qB	=VB	=�B	=�B	=�B	=qB	=VB	>(B	>]B	="B	="B	DMB	QNB	i�B	�oB	�xB	�"B	�BB	��B	�TB	�@B	��B	�_B
B[B
c�B
�#B
�cB
��B
��B
�B
��B
�=B
��B�B-]BU�Bj�By�B}"B�B|�Bw�Br�B��B�#B�B�)B��B�B��B��B��B��B��B�3B��B�B�2B�B�7B�2B��B��BI�B;�B��B��B��BcBv�Bh�BIB4�B�B�BKB
��B
��B
l�B
B�B
�B
+B
	�B
(B
	�B	�dB	��B	�B	�aB	}"B	SuB	&�B	uB	�B	3B��B��B�B�1B�gB��B��B��B��B�B��B�B�GB��B��B��B��B�vB��B��B��B��B��B�*B��B��B��B��B�tB��B�B��B��B��B��B��B�fB��B�jB��B�B��B��BĶB��BƎBɆB�VB�NB��B��B��BۦB�B�NB�B�cB�B�B�B�xB	�B	�B	B	bB	�B	 'B	&�B	5?B	MPB	_�B	c�B	o�B	�aB	�#B	�MB	��B	��B	�)B	��B	��B	��B	�rB	�"B	�.B	�bB	�B	ɠB	�RB	��B	�B	�B	��B	��B	��B	�B	��B	�yB	��B	�wB	�`B	��B	�CB	��B	�fB	��B	�*B	�8B	��B	��B	��B	��B	�B	�B	��B	�B	��B	�B	�(B	�pB	͹B	�jB	�xB	�	B	ƎB	ɺB	ɠB	ȴB	�B	ȴB	�_B	��B	ƨB	�?B	ŢB	ĶB	�3B	�{B	��B	��B	� B	�B	��B	��B	��B	�BB	��B	�}B	�iB	�aB	ðB	��B	�3B	�gB	ŢB	ƨB	��B	�KB	�rB	��B	�B	�JB	�"B	�pB	οB	�BB	��B	��B	҉B	�&B	�FB	��B	ԯB	ԕB	ּB	�sB	׍B	׍B	�?B	�$B	�_B	��B	�1B	ؓB	�=B	ۦB	�xB	ܬB	��B	��B	��B	��B	��B	�=B	�B	��B	�NB	�HB	�(B	��B	�\B	ЗB	�}B	��B	� B	уB	�:B	�,B	��B	��B	�B	��B	�B	�B	یB	�dB	�~B	�B	�~B	�~B	ݘB	�IB	��B	�B	�B	�@B	�FB	��B	�B	�*B	��B	��B	�B	�sB	�>B	�yB	�B	��B	�B	��B	�GB	�MB	�B	�B	��B	�B	�9B	�B	�?B	�?B	��B	�2B	��B	��B	��B	��B	�B	�8B	�8B	�RB	��B	��B	��B	�rB	��B	��B	�B	�DB	�JB	��B	��B	�B	�PB	�B	�B	�6B	�6B	��B	�B	�jB	�PB	�"B	��B	��B	��B	��B	��B
 �B
B
B
oB
B
AB
uB
B
�B
�B
aB
�B
MB
MB
�B
�B
�B
SB
�B
B
YB
%B
tB
%B
%B
B
�B
B
�B
�B
tB
tB
�B
	B
	B
�B
	7B
	B

#B

�B
�B
�B
JB
JB
dB
�B
�B
�B
B
PB
6B
�B
pB
BB
(B
(B
�B
�B
�B
HB
HB
�B
 B
 B
 B
�B
oB
B
uB
�B
�B
�B
FB
�B
2B
�B
gB
gB
�B
�B
�B
�B
�B
EB
B
�B
7B
�B
�B
�B
B
OB
�B
OB
jB
B
�B
;B
VB
pB
!B
B
B
�B
�B
VB
VB
VB
pB
pB
pB
�B
�B
�B
 'B
 'B
!-B
"NB
#TB
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#�B
#TB
#TB
#nB
#�B
"�B
#B
# B
#�B
#�B
$ZB
$�B
$�B
$�B
%B
%B
%,B
%FB
%�B
%�B
%�B
&B
&LB
&2B
&�B
'B
'B
'B
'RB
'�B
'�B
'�B
(>B
)DB
+B
+B
+B
,WB
,�B
,�B
-)B
-]B
-CB
-)B
-�B
.IB
.cB
.}B
/�B
/iB
0!B
0�B
0�B
1B
1'B
1�B
1�B
2-B
2�B
3B
3hB
3�B
49B
4B
4�B
5?B
5�B
6�B
72B
7�B
8B
8�B
8�B
8�B
8�B
8�B
9�B
:�B
;�B
;�B
<B
<B
<6B
<6B
<B
<B
<6B
;�B
;�B
;�B
;�B
<B
<6B
<6B
<6B
<B
<B
<6B
<�B
<�B
=VB
=qB
=�B
>BB
?B
>�B
?.B
?cB
?�B
@4B
?�B
?�B
?}B
?}B
?}B
?�B
?�B
@4B
@�B
@�B
A B
A�B
A�B
A�B
AoB
A�B
A�B
BB
B[B
BuB
BuB
B'B
A�B
A B
A;B
A�B
A�B
B�B
CB
CGB
C�B
DB
DgB
EB
EmB
EmB
EmB
E�B
E�B
E�B
FtB
F?B
FtB
F�B
F�B
G+B
GB
GEB
GEB
G�B
HB
HB
H�B
H�B
H�B
IB
I7B
IRB
I�B
J�B
J�B
JrB
J�B
J�B
J�B
KxB
K�B
K�B
LB
L~B
L~B
M6B
M�B
M�B
NB
N"B
NB
NVB
N�B
N�B
OvB
O�B
O�B
PB
PB
PHB
P�B
QB
Q4B
QNB
Q�B
Q�B
Q�B
R:B
RoB
R�B
SB
S�B
T,B
T,B
T�B
T�B
T�B
UB
U�B
U�B
U�B
VB
VB
VB
VSB
VSB
V�B
V�B
W�B
XyB
X�B
XyB
X�B
YB
X�B
Y1B
X�B
X�B
YB
YKB
YeB
YB
Y�B
Y�B
Z7B
Z�B
Z�B
Z�B
Z�B
Z�B
[	B
[#B
[�B
[�B
\)B
\CB
\]B
\xB
\�B
\�B
\�B
\�B
\�B
]/B
]~B
]~B
]~B
]�B
^B
^5B
^OB
^�B
^�B
^�B
^�B
^�B
_pB
_VB
_�B
_�B
_�B
_�B
_pB
_�B
`\B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
aB
`�B
`�B
`�B
aB
a�B
a�B
a�B
a�B
bB
bNB
b�B
b�B
b�B
cB
cB
c B
cB
c:B
c�B
c�B
c�B
c�B
c�B
d@B
d�B
eB
e,B
e`B
ezB
fB
f2B
fLB
fLB
fLB
ffB
f�B
f�B
gB
g8B
gRB
g�B
g�B
g�B
g�B
g�B
h
B
hsB
hsB
hsB
h�B
i*B
iB
iDB
i_B
iDB
i_B
i�B
i�B
jB
j0B
jKB
j�B
j�B
j�B
kB
k6B
k�B
k�B
k�B
l=B
l=B
l=B
l�B
l�B
mCB
m]B
m]B
m]B
m�B
m�B
n/B
n/B
n/B
ncB
n�B
o B
o5B
o�B
oiB
o�B
p;B
poB
p�B
p�B
p�B
p�B
p�B
p�B
qAB
q�B
q�B
q�B
q�B
q�B
rB
raB
raB
r�B
r�B
sB
s�B
s�B
s�B
s�B
tB
t9B
tTB
tTB
tnB
t�B
t�B
t�B
t�B
u%B
uZB
utB
u�B
v+B
vzB
v`B
vzB
v�B
v�B
v�B
wB
w2B
wLB
wfB
w�B
xB
x8B
x8B
xlB
x�B
x�B
x�B
y	B
y$B
y>B
y>B
y�B
y�B
zB
z*B
zDB
z�B
z�B
z�B
z�B
z�B
{0B
{dB
{B
{B
{�B
{�B
{�B
|B
|B
|B
|PB
|PB
|�B
|�B
|�B
}B
}B
}VB
}VB
}�B
}�B
}�B
~(B
~BB
~]B
~�B
B
~�B
}B
�B
�B
�B
�B
�B
� B
�4B
�OB
�iB
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105234  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191715  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191715  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191715                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041723  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041723  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                