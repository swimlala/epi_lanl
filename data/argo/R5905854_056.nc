CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:54:36Z creation;2022-06-04T17:54:37Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604175436  20220610141506  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               8A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�14j��s1   @�14���0@.�G�z��c/��w1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�ffB�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�ffB���B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
33C�fC  C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2L�C3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CW�fCZ  C\  C^  C`L�Cb  Cd  Cf  Ch  Cj  Cl  Cm�fCo�fCr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D���D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @(�@�G�@�G�A ��A ��A@��A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B (�B(�B(�B(�B (�B((�B0(�B8(�B@(�BH(�BP(�BX(�B`(�BgBp(�Bx(�B�{B�{B�{B�{B�{B�z�B�{B�{B�{B��HB�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�z�B��HB�z�B��HB�{B�{B�{B�{B�{B�{B�{C 
=C
=C
=C
=C
=C
=pC�C
=C�C
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
=C2W
C3�C6
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
=CW�CZ
=C\
=C^
=C`W
Cb
=Cd
=Cf
=Ch
=Cj
=Cl
=Cm�Co�Cr
=Ct
=Cv
=Cx
=Cz
=C|
=C~
=C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��RC�C�C�C�C�C�C�C�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD��D�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�D{D��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�{D�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��D��D�AHD��HD��HD�HD�AHD��HD��HD�HD�AHDHD��HD�HD�AHDÁHD��HD�HD�AHDāHD��HD�HD�AHDŁHD��HD�HD�AHDƁHD��HD�HD�AHDǁHD��HD�HD�AHDȁHD��HD�HD�AHDɁHD��HD�HD�AHDʁHD��HD�HD�AHDˁHD��HD��D�AHD́HD��HD�HD�AHD́HD��HD�HD�AHD΁HD��HD�HD�AHDρHD��HD�HD�AHDЁHD��HD�HD�AHDсHD��HD�HD�AHDҁHD��HD�HD�AHDӁHD��HD�HD�AHDԁHD��HD�HD�AHDՁHD��HD�HD�AHDցHD��HD�HD�AHDׁHD��HD�HD�AHD؁HD��HD�HD�AHDفHD��HD�HD�AHDځHD��HD�HD�AHDہHD��HD�HD�AHD܁HD��HD�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�D{D��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD�g�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aӥ�AӠ�AӒ:Aӊ�AӒAӒ�AӔFAӅ�AӅ�AӄAӄAӃ�Aӂ�Aӂ�AӃ�AӄAӃAӂuAӂ�Aӄ�AӇ+AӉ�AӁ�A�z�A�{�A�tTA�]dAҰ!A�-�A�+Aȩ_A���A�q�AìqA�:^A��A���A���A�H�A���A�49A���A���A�9�A���A���A�1�A�NpA�u�A�$@A��_A�.A��A���A�IRA��bA���A��oA�רA�c A�F�A�3�A���A��uA�qA�[�A�n�A���A�i�A�)_A���A�@A��vA�	�A��rA��A���A�E�A��A���A�˒A��EA��4A��A��A���A���A���A�"A�v�A{HAum]Ant�AfԕA`�A[�DAX�}AU��ASrGAQ�dAO��AM��AK�$AI��AF�ADb�AB�sAA�#A?�jA?�A>+A<�:A:^�A8�]A80�A7�oA5�-A2�)A0��A.� A+��A*jA*
=A*
=A)��A'�A&��A&VA$�A$(�A#��A"�SA"VA"A!�A ��A�]AߤA�AԕA��A�A��A	�A%FA{A�9A.IA�OA�A�A�}A��A�AqA��A.IA!�A�A��A�DA$tA��A�|A�jACAQ�A�{A��A��A�A�A�[A
�xA	��A��A�A�AOvA\)A]dA@�AzxA�A$tA��A�!A�AZA��A�7A�A�&A�
A��Al�A�\A��A��AuA�_A�A��AZA�AĜA� A�=Al�A �@���@�[W@��s@�\)@��V@��@��@�ff@�s�@�N�@��
@��@�@�u@���@�d�@�_p@�D@��X@���@얼@�o@얼@�S@�H�@���@�Z�@�N<@�Ft@�Ov@�c @��@��,@�@�H@�<6@��K@�_�@��@�Y�@��@�@�S@�`�@���@�k�@�O@�R�@���@�@��@�ƨ@�+@� �@��[@�1@�w2@��s@�N�@�V@��D@ۧ�@�+@�Ta@�-�@��6@� i@��@�J#@֕@շ@Ԩ�@���@�zx@���@�'R@�,=@�@��@Ј�@�M�@�(�@Ϧ�@�֡@�r�@��a@�m�@�"h@�O@��@ˮ�@˶F@�S@ʹ$@�r�@�V@��@ɓ�@�%F@ȷ�@�S�@��@Ǽ�@�s�@�@���@�m�@œ�@��H@Ğ�@�-@�1@��@��@ç�@�Q�@��'@��@�=@��@�^5@���@�%@���@�� @�C-@���@�S@��$@��@��v@�A�@��T@�F@��@��@��}@�	@���@�RT@��K@�a|@��-@�/�@�L�@��!@�,=@��3@���@�=@�u%@��@��{@�=�@�!-@���@��!@�Z�@��@�t�@��@��}@��@���@�q@�7�@�4@���@�u�@��@��/@���@�L0@�8�@��@���@�b�@�K�@�+@�ȴ@�kQ@�GE@�@���@�$t@�ی@��@�i�@��A@��g@���@�s�@�(�@���@�1'@��@���@��@�m]@��@���@�GE@��+@��d@���@�U�@��@�Ov@���@���@�v`@�4@�+@�ȴ@�@�@��@��4@�`B@�5�@�#�@�q@�;@�u�@�!@��w@�=@���@���@�m�@�{@���@�S&@� \@���@�z�@�1�@��@���@�A @���@�^5@�	@���@���@�J�@���@�E�@��;@��q@�_p@�@���@���@���@�L0@��@�s@�8�@��8@��\@�:*@���@���@�zx@��@��!@�s�@�/�@��6@�T�@�%@��'@���@��@���@�l"@�=q@�e@���@���@�#�@�֡@��@��+@�c�@�-�@� �@��@���@�u�@�Z�@�8�@��"@���@���@�s�@�@���@�Y�@�+@��@��@�~�@�*�@���@���@���@�:�@��@�%@�%@��@��@���@�bN@�M�@�D�@��r@���@���@��~@�b�@�Dg@�&�@��@��"@���@��_@�N�@�7�@�)�@�!�@���@���@�A @��@�GE@���@���@���@�e�@�A @�8@��P@��D@�Xy@�<�@�{@��6@�X�@�1�@��@��X@�_@�;�@ݘ@�V@/�@~�6@~\�@~#:@}�7@}V@|C-@{��@{�@z��@z�L@z�@yc�@x��@x>B@w�f@v�@v��@vxl@v&�@u��@um]@t�@tM@s��@sJ#@r��@r�@q��@q�S@qw2@p��@pU2@p�@o�k@o�@n��@m��@m��@m��@mG�@m�@l�v@l�?@ly>@k�;@k��@kx@kl�@kA�@j�<@jV@j8�@j{@j@i��@i�X@i;@h�z@h��@h[�@h6@h-�@g��@g��@gA�@g�@f��@f	@e�@eV@dm�@d�@c��@c;d@b�@b��@b�b@b�1@b{�@b�@a��@aS&@`��@`H@`  @_�;@_qv@_/�@^��@^�1@^?@]�N@]x�@]p�@]&�@\�9@\�@\7�@[��@[iD@Z҉@ZZ�@Z�@Y�3@Y��@Yo @Y@X��@Xc�@XXy@XC-@W�A@Wj�@Vxl@VL0@V�@U��@T�@T1'@S��@S��@S>�@R�'@R	@Q�@Q�=@P�f@P~(@P@O��@O�:@N��@N�@M��@M}�@M�@L�?@Lq@K�@K�[@Kqv@J��@J~�@J�@I��@Ic�@I7L@H�U@H�@H2�@G��@GC@F��@F�s@F�}@F^5@E�@Ef�@E�@D��@D  @C�6@C�@Co�@CX�@C�@B��@A�T@A��@Ac�@A?}@@�@@I�@?��@?�	@?C�@?6z@?S@>�B@>�6@>u%@=�T@=�@=��@=��@==�@<��@<�$@<_@</�@;��@;K�@:�R@:_�@:GE@:E�@:.�@9ԕ@9T�@92a@9�@9�@8�)@8�@7��@7=@6�@6Q@6�@5�.@5�N@5��@5�n@5}�@5�@4h�@4 �@3��@3��@3X�@3'�@3o@2�"@2��@2p;@2	@1��@1�7@1[W@1 \@0�@0��@0�@/� @/�w@/��@/��@/x@/S�@/@.�'@.V@-��@-��@-hs@-*0@-�@,ѷ@,K^@,�@+�@+�g@+�@+�[@+]�@+"�@*�@*� @*Ta@*$�@)�)@)��@)s�@)=�@)%@(��@(��@(Q�@'�r@'��@'Mj@'Y@&͟@&��@&-@&
�@%��@%��@%��@%�h@%A @$�@$�Y@$N�@$1@#�@#خ@#�6@#�k@#~�@#qv@#U�@#A�@#1�@"�1@"B[@"�@!��@!�S@!(�@ ��@ �Y@ Z@ *�@�@��@=@�@��@GE@O@
�@u@�@�9@��@x�@-w@ی@��@I�@@�@l�@�@�<@�b@GE@�z@�@c�@V@Ɇ@�@*�@�{@�@�s@��@��@i�@-@�@��@u�@&�@�@��@�[@�D@S�@<�@�@��@g�@A�@/�@�@�H@�!@~�@Ta@E�@&�@@�@�)@@a�@0�@�@�|@�[@��@oi@H@ �@��@�@n/@Mj@�@�@�@�m@�r@c @8�@
�@�T@�d@��@Y�@�@Ĝ@��@�4@�u@�@y>@tT@h�@Z@N�@:�@x@�;@{J@O@)_@
�H@
��@
�@
�\@
V@
+k@	�D@	�9@	��@	o @	O�@	B�@	?}@		l@�@ѷ@�4@K^@�@�W@�&@ƨ@��@�*@��@n/@P�@J#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aӥ�AӠ�AӒ:Aӊ�AӒAӒ�AӔFAӅ�AӅ�AӄAӄAӃ�Aӂ�Aӂ�AӃ�AӄAӃAӂuAӂ�Aӄ�AӇ+AӉ�AӁ�A�z�A�{�A�tTA�]dAҰ!A�-�A�+Aȩ_A���A�q�AìqA�:^A��A���A���A�H�A���A�49A���A���A�9�A���A���A�1�A�NpA�u�A�$@A��_A�.A��A���A�IRA��bA���A��oA�רA�c A�F�A�3�A���A��uA�qA�[�A�n�A���A�i�A�)_A���A�@A��vA�	�A��rA��A���A�E�A��A���A�˒A��EA��4A��A��A���A���A���A�"A�v�A{HAum]Ant�AfԕA`�A[�DAX�}AU��ASrGAQ�dAO��AM��AK�$AI��AF�ADb�AB�sAA�#A?�jA?�A>+A<�:A:^�A8�]A80�A7�oA5�-A2�)A0��A.� A+��A*jA*
=A*
=A)��A'�A&��A&VA$�A$(�A#��A"�SA"VA"A!�A ��A�]AߤA�AԕA��A�A��A	�A%FA{A�9A.IA�OA�A�A�}A��A�AqA��A.IA!�A�A��A�DA$tA��A�|A�jACAQ�A�{A��A��A�A�A�[A
�xA	��A��A�A�AOvA\)A]dA@�AzxA�A$tA��A�!A�AZA��A�7A�A�&A�
A��Al�A�\A��A��AuA�_A�A��AZA�AĜA� A�=Al�A �@���@�[W@��s@�\)@��V@��@��@�ff@�s�@�N�@��
@��@�@�u@���@�d�@�_p@�D@��X@���@얼@�o@얼@�S@�H�@���@�Z�@�N<@�Ft@�Ov@�c @��@��,@�@�H@�<6@��K@�_�@��@�Y�@��@�@�S@�`�@���@�k�@�O@�R�@���@�@��@�ƨ@�+@� �@��[@�1@�w2@��s@�N�@�V@��D@ۧ�@�+@�Ta@�-�@��6@� i@��@�J#@֕@շ@Ԩ�@���@�zx@���@�'R@�,=@�@��@Ј�@�M�@�(�@Ϧ�@�֡@�r�@��a@�m�@�"h@�O@��@ˮ�@˶F@�S@ʹ$@�r�@�V@��@ɓ�@�%F@ȷ�@�S�@��@Ǽ�@�s�@�@���@�m�@œ�@��H@Ğ�@�-@�1@��@��@ç�@�Q�@��'@��@�=@��@�^5@���@�%@���@�� @�C-@���@�S@��$@��@��v@�A�@��T@�F@��@��@��}@�	@���@�RT@��K@�a|@��-@�/�@�L�@��!@�,=@��3@���@�=@�u%@��@��{@�=�@�!-@���@��!@�Z�@��@�t�@��@��}@��@���@�q@�7�@�4@���@�u�@��@��/@���@�L0@�8�@��@���@�b�@�K�@�+@�ȴ@�kQ@�GE@�@���@�$t@�ی@��@�i�@��A@��g@���@�s�@�(�@���@�1'@��@���@��@�m]@��@���@�GE@��+@��d@���@�U�@��@�Ov@���@���@�v`@�4@�+@�ȴ@�@�@��@��4@�`B@�5�@�#�@�q@�;@�u�@�!@��w@�=@���@���@�m�@�{@���@�S&@� \@���@�z�@�1�@��@���@�A @���@�^5@�	@���@���@�J�@���@�E�@��;@��q@�_p@�@���@���@���@�L0@��@�s@�8�@��8@��\@�:*@���@���@�zx@��@��!@�s�@�/�@��6@�T�@�%@��'@���@��@���@�l"@�=q@�e@���@���@�#�@�֡@��@��+@�c�@�-�@� �@��@���@�u�@�Z�@�8�@��"@���@���@�s�@�@���@�Y�@�+@��@��@�~�@�*�@���@���@���@�:�@��@�%@�%@��@��@���@�bN@�M�@�D�@��r@���@���@��~@�b�@�Dg@�&�@��@��"@���@��_@�N�@�7�@�)�@�!�@���@���@�A @��@�GE@���@���@���@�e�@�A @�8@��P@��D@�Xy@�<�@�{@��6@�X�@�1�@��@��X@�_@�;�@ݘ@�V@/�@~�6@~\�@~#:@}�7@}V@|C-@{��@{�@z��@z�L@z�@yc�@x��@x>B@w�f@v�@v��@vxl@v&�@u��@um]@t�@tM@s��@sJ#@r��@r�@q��@q�S@qw2@p��@pU2@p�@o�k@o�@n��@m��@m��@m��@mG�@m�@l�v@l�?@ly>@k�;@k��@kx@kl�@kA�@j�<@jV@j8�@j{@j@i��@i�X@i;@h�z@h��@h[�@h6@h-�@g��@g��@gA�@g�@f��@f	@e�@eV@dm�@d�@c��@c;d@b�@b��@b�b@b�1@b{�@b�@a��@aS&@`��@`H@`  @_�;@_qv@_/�@^��@^�1@^?@]�N@]x�@]p�@]&�@\�9@\�@\7�@[��@[iD@Z҉@ZZ�@Z�@Y�3@Y��@Yo @Y@X��@Xc�@XXy@XC-@W�A@Wj�@Vxl@VL0@V�@U��@T�@T1'@S��@S��@S>�@R�'@R	@Q�@Q�=@P�f@P~(@P@O��@O�:@N��@N�@M��@M}�@M�@L�?@Lq@K�@K�[@Kqv@J��@J~�@J�@I��@Ic�@I7L@H�U@H�@H2�@G��@GC@F��@F�s@F�}@F^5@E�@Ef�@E�@D��@D  @C�6@C�@Co�@CX�@C�@B��@A�T@A��@Ac�@A?}@@�@@I�@?��@?�	@?C�@?6z@?S@>�B@>�6@>u%@=�T@=�@=��@=��@==�@<��@<�$@<_@</�@;��@;K�@:�R@:_�@:GE@:E�@:.�@9ԕ@9T�@92a@9�@9�@8�)@8�@7��@7=@6�@6Q@6�@5�.@5�N@5��@5�n@5}�@5�@4h�@4 �@3��@3��@3X�@3'�@3o@2�"@2��@2p;@2	@1��@1�7@1[W@1 \@0�@0��@0�@/� @/�w@/��@/��@/x@/S�@/@.�'@.V@-��@-��@-hs@-*0@-�@,ѷ@,K^@,�@+�@+�g@+�@+�[@+]�@+"�@*�@*� @*Ta@*$�@)�)@)��@)s�@)=�@)%@(��@(��@(Q�@'�r@'��@'Mj@'Y@&͟@&��@&-@&
�@%��@%��@%��@%�h@%A @$�@$�Y@$N�@$1@#�@#خ@#�6@#�k@#~�@#qv@#U�@#A�@#1�@"�1@"B[@"�@!��@!�S@!(�@ ��@ �Y@ Z@ *�@�@��@=@�@��@GE@O@
�@u@�@�9@��@x�@-w@ی@��@I�@@�@l�@�@�<@�b@GE@�z@�@c�@V@Ɇ@�@*�@�{@�@�s@��@��@i�@-@�@��@u�@&�@�@��@�[@�D@S�@<�@�@��@g�@A�@/�@�@�H@�!@~�@Ta@E�@&�@@�@�)@@a�@0�@�@�|@�[@��@oi@H@ �@��@�@n/@Mj@�@�@�@�m@�r@c @8�@
�@�T@�d@��@Y�@�@Ĝ@��@�4@�u@�@y>@tT@h�@Z@N�@:�@x@�;@{J@O@)_@
�H@
��@
�@
�\@
V@
+k@	�D@	�9@	��@	o @	O�@	B�@	?}@		l@�@ѷ@�4@K^@�@�W@�&@ƨ@��@�*@��@n/@P�@J#1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�B	�eB	�1B	��B	�B	�eB	ٚB	�KB	�KB	�1B	�B	�B	��B	�B	�B	��B	��B	�1B	�B	�1B	�1B	�KB	��B	�_B	�yB	��B	�SB	�:B	ȴB	�B	��B	�EB	�mB	˒B	�TB
{B
�B
B
#TB
0!B
8B
UgB
i_B
�B
��B
�B
�B
��B
��B
�B
��B
�_B
�/B
�GB@B&�BN�B`�Bd@B.B�lB�\B�6B�vB�LB�HB��B��B�Bm�BNVB<�B1�B=�B;JB$@B�B
��B
��B
��B
��B
�lB
�<B
��B
��B
�B
}qB
T�B
eB
DB	�B	�vB	{dB	RoB	2aB	KB	0B	�B�rB��B��B�,B�B�
B��B�=B�B�KB�B��B�B��BɺB��BچB�B�@B�|B߾B�B��B��B	zB	�B	sB	�B	�B	�B	�B	�B	7B	qB	 �B	$�B	2�B	2�B	33B	>(B	?.B	@�B	C�B	B�B	BAB	<�B	7�B	2�B	8�B	8lB	=�B	F�B	E�B	B�B	H1B	H�B	]IB	��B	�;B	�?B	�eB	�!B	��B	��B	�BB	~�B	o�B	hXB	p;B	yrB	�?B	��B	��B	�B	o�B	f2B	`�B	\�B	`�B	_VB	\)B	X+B	V�B	h�B	�B	��B	��B	�hB	�
B	�5B	�BB	��B	�7B	��B	�|B	�FB	��B	�>B	�/B	��B	�iB	�3B	�MB	�{B	�UB	�4B	�B	�(B	� B	�zB	��B	ȚB	�}B	�<B	�<B	āB	��B	�B	�SB	� B	�B	��B	�B	��B	�"B	�9B	�|B	�hB	��B	��B	��B	�qB	�qB	��B	�cB	�ZB	�B	��B	��B	��B	��B	��B	ΊB	�mB	��B	�kB	�kB	�B	�B	��B	��B	�$B	�KB	ܒB	�]B	��B	�/B	�B	��B	ܒB	�IB	�OB	ބB	ۦB	��B	רB	��B	ѝB	��B	��B	�}B	�B	ٚB	�CB	��B	ܬB	�xB	��B	��B	�WB	��B	ٴB	�+B	ּB	��B	ԕB	�+B	�sB	�B	ܒB	�#B	��B	�#B	�]B	�IB	�)B	��B	��B	�QB	�B	چB	�kB	�	B	��B	��B	�\B	�vB	��B	�'B	�\B	��B	�hB	�B	�4B	��B	��B	�B	�B	�LB	�B	��B	�B	�@B	�2B	�B	�yB	�6B	��B	�B	�DB	�
B	�B	�8B	�B	�B	�B	�B	��B	�_B	��B	�B	�)B	��B	��B	�/B	� B	�}B	�IB	��B	��B	�B	�iB	�iB	��B	��B	�B	��B	��B	��B	�B	��B	�3B	�B	��B	�-B	��B	��B	�MB	��B	�TB	�B	�TB	�B	�nB	�%B	��B	�fB	��B	�B	�B	��B	��B	��B	��B	��B	�B	�8B	��B	��B	��B	�>B	�rB	��B	��B	�*B	�JB	��B	�PB	��B	��B	��B	�(B	�]B	��B	�}B	��B
 �B
 OB
 �B
 OB
 �B
 �B
 �B
 4B
 4B
 B
 iB
 �B
 B
�B
B
AB
AB
�B
uB
�B
�B
�B
MB
�B
SB
�B
�B
�B
�B
9B
%B
_B
�B
�B
B
KB
�B
	RB
	RB
	�B

rB

�B

�B
xB
�B
~B
PB
6B
�B
"B
"B
�B
(B
�B
�B
B
B
�B
B
�B
�B
�B
B
 B
�B
:B
�B
�B
�B
�B
�B
B
{B
�B
�B
�B
9B
�B
�B
�B
$B
?B
sB
�B
�B
�B
B
B
7B
QB
�B
qB
qB
�B
/B
IB
IB
IB
~B
�B
�B
B
�B
�B
;B
VB
�B
�B
�B
VB
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
�B
�B
 'B
!�B
"�B
# B
#�B
#�B
#�B
#�B
$tB
$�B
%zB
%�B
&B
&2B
&�B
&�B
&�B
&�B
&LB
%�B
$�B
$�B
$tB
$ZB
$�B
$�B
%B
%�B
%�B
%�B
%�B
%�B
&B
&2B
&2B
%�B
&B
&�B
'�B
(�B
(�B
)B
)*B
(�B
(�B
)DB
*KB
*B
)�B
*KB
*KB
*B
+B
+B
+�B
,WB
,�B
-CB
-CB
-CB
-wB
-�B
.}B
/OB
/iB
/OB
/�B
0;B
0�B
0�B
0�B
0�B
1AB
1AB
1[B
1�B
2-B
2aB
3B
2�B
33B
3MB
3�B
4B
4TB
4B
4�B
4�B
5?B
5%B
5?B
5�B
5�B
6FB
6�B
6�B
6�B
6�B
7�B
8RB
88B
8RB
8lB
8lB
9$B
9>B
9$B
9XB
:*B
:�B
;JB
;JB
;dB
;B
;0B
;B
:�B
;0B
;JB
<6B
<�B
<�B
<�B
<jB
<�B
=VB
=�B
=�B
>B
>�B
>�B
>�B
>�B
>�B
>�B
?cB
@ B
?�B
?�B
?�B
@ B
@�B
@�B
@B
@4B
@iB
@�B
AB
A�B
A�B
A�B
A�B
B[B
BuB
B�B
B�B
B�B
B�B
C�B
DMB
D�B
D�B
EmB
E�B
F�B
GB
F�B
F�B
G_B
G+B
HKB
H�B
I7B
IlB
IlB
I�B
JrB
KDB
K�B
K�B
LdB
L�B
L�B
MB
MPB
M�B
N<B
N�B
N�B
N�B
O(B
O\B
OvB
O�B
PB
PB
P.B
P�B
Q B
Q�B
Q�B
RB
R�B
R�B
R�B
SB
S&B
S@B
S�B
TFB
TFB
T{B
TFB
T�B
T�B
T�B
U2B
U2B
U2B
U�B
U�B
U�B
VB
VSB
V�B
V�B
VmB
V�B
V�B
W
B
W�B
W�B
W�B
X+B
XB
X+B
X+B
XEB
X+B
X�B
X�B
X�B
YB
YB
Y1B
Y�B
YeB
Y�B
ZkB
Z�B
Z�B
[=B
[�B
[�B
[�B
[�B
[�B
\]B
\xB
\xB
\]B
\�B
\�B
\�B
\�B
\�B
]IB
]dB
]dB
]�B
]�B
]�B
]�B
^B
^�B
^�B
^�B
^�B
^�B
^�B
_B
_VB
_�B
_�B
`BB
`�B
`�B
`�B
`�B
`�B
a�B
a�B
a�B
a�B
a�B
a�B
bNB
bNB
bhB
b�B
cB
cTB
cnB
cnB
dB
c�B
d@B
d�B
d�B
d�B
d�B
e,B
e`B
ezB
ezB
e�B
e�B
e�B
e�B
e�B
fB
fB
f�B
f�B
gB
gRB
g�B
g�B
g�B
g�B
h
B
h$B
h$B
h$B
h
B
g�B
hXB
h�B
h�B
h�B
h�B
iyB
i�B
i�B
jB
j0B
jeB
j�B
kB
kB
k�B
lB
l"B
l=B
l"B
l=B
lWB
l�B
l�B
m)B
m]B
mwB
m�B
m�B
m�B
n/B
ncB
n}B
n�B
o5B
pB
poB
p�B
p�B
q'B
qAB
q�B
q�B
q�B
rGB
raB
r�B
r�B
r�B
r�B
s3B
shB
s�B
t9B
tB
tTB
t�B
t�B
t�B
uB
u�B
u�B
u�B
vB
v+B
vFB
vzB
v�B
v�B
v�B
v�B
v�B
v�B
wB
w2B
wfB
w�B
w�B
w�B
w�B
xB
xlB
x�B
x�B
x�B
yXB
yrB
y�B
y�B
y�B
y�B
zDB
z^B
zxB
z�B
z�B
{B
{B
{B
{�B
|B
|6B
|6B
|PB
|jB
|jB
|�B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
}qB
}�B
}�B
}�B
}�B
~B
~(B
~]B
~�B
~�B
~�B
~�B
HB
cB
HB
cB
�B
�B
�B
� B
�OB
��B
��B
��B
��B
��B
��B
� B
�;B
�UB
�U1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�B	�eB	�1B	��B	�B	�eB	ٚB	�KB	�KB	�1B	�B	�B	��B	�B	�B	��B	��B	�1B	�B	�1B	�1B	�KB	��B	�_B	�yB	��B	�SB	�:B	ȴB	�B	��B	�EB	�mB	˒B	�TB
{B
�B
B
#TB
0!B
8B
UgB
i_B
�B
��B
�B
�B
��B
��B
�B
��B
�_B
�/B
�GB@B&�BN�B`�Bd@B.B�lB�\B�6B�vB�LB�HB��B��B�Bm�BNVB<�B1�B=�B;JB$@B�B
��B
��B
��B
��B
�lB
�<B
��B
��B
�B
}qB
T�B
eB
DB	�B	�vB	{dB	RoB	2aB	KB	0B	�B�rB��B��B�,B�B�
B��B�=B�B�KB�B��B�B��BɺB��BچB�B�@B�|B߾B�B��B��B	zB	�B	sB	�B	�B	�B	�B	�B	7B	qB	 �B	$�B	2�B	2�B	33B	>(B	?.B	@�B	C�B	B�B	BAB	<�B	7�B	2�B	8�B	8lB	=�B	F�B	E�B	B�B	H1B	H�B	]IB	��B	�;B	�?B	�eB	�!B	��B	��B	�BB	~�B	o�B	hXB	p;B	yrB	�?B	��B	��B	�B	o�B	f2B	`�B	\�B	`�B	_VB	\)B	X+B	V�B	h�B	�B	��B	��B	�hB	�
B	�5B	�BB	��B	�7B	��B	�|B	�FB	��B	�>B	�/B	��B	�iB	�3B	�MB	�{B	�UB	�4B	�B	�(B	� B	�zB	��B	ȚB	�}B	�<B	�<B	āB	��B	�B	�SB	� B	�B	��B	�B	��B	�"B	�9B	�|B	�hB	��B	��B	��B	�qB	�qB	��B	�cB	�ZB	�B	��B	��B	��B	��B	��B	ΊB	�mB	��B	�kB	�kB	�B	�B	��B	��B	�$B	�KB	ܒB	�]B	��B	�/B	�B	��B	ܒB	�IB	�OB	ބB	ۦB	��B	רB	��B	ѝB	��B	��B	�}B	�B	ٚB	�CB	��B	ܬB	�xB	��B	��B	�WB	��B	ٴB	�+B	ּB	��B	ԕB	�+B	�sB	�B	ܒB	�#B	��B	�#B	�]B	�IB	�)B	��B	��B	�QB	�B	چB	�kB	�	B	��B	��B	�\B	�vB	��B	�'B	�\B	��B	�hB	�B	�4B	��B	��B	�B	�B	�LB	�B	��B	�B	�@B	�2B	�B	�yB	�6B	��B	�B	�DB	�
B	�B	�8B	�B	�B	�B	�B	��B	�_B	��B	�B	�)B	��B	��B	�/B	� B	�}B	�IB	��B	��B	�B	�iB	�iB	��B	��B	�B	��B	��B	��B	�B	��B	�3B	�B	��B	�-B	��B	��B	�MB	��B	�TB	�B	�TB	�B	�nB	�%B	��B	�fB	��B	�B	�B	��B	��B	��B	��B	��B	�B	�8B	��B	��B	��B	�>B	�rB	��B	��B	�*B	�JB	��B	�PB	��B	��B	��B	�(B	�]B	��B	�}B	��B
 �B
 OB
 �B
 OB
 �B
 �B
 �B
 4B
 4B
 B
 iB
 �B
 B
�B
B
AB
AB
�B
uB
�B
�B
�B
MB
�B
SB
�B
�B
�B
�B
9B
%B
_B
�B
�B
B
KB
�B
	RB
	RB
	�B

rB

�B

�B
xB
�B
~B
PB
6B
�B
"B
"B
�B
(B
�B
�B
B
B
�B
B
�B
�B
�B
B
 B
�B
:B
�B
�B
�B
�B
�B
B
{B
�B
�B
�B
9B
�B
�B
�B
$B
?B
sB
�B
�B
�B
B
B
7B
QB
�B
qB
qB
�B
/B
IB
IB
IB
~B
�B
�B
B
�B
�B
;B
VB
�B
�B
�B
VB
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
�B
�B
 'B
!�B
"�B
# B
#�B
#�B
#�B
#�B
$tB
$�B
%zB
%�B
&B
&2B
&�B
&�B
&�B
&�B
&LB
%�B
$�B
$�B
$tB
$ZB
$�B
$�B
%B
%�B
%�B
%�B
%�B
%�B
&B
&2B
&2B
%�B
&B
&�B
'�B
(�B
(�B
)B
)*B
(�B
(�B
)DB
*KB
*B
)�B
*KB
*KB
*B
+B
+B
+�B
,WB
,�B
-CB
-CB
-CB
-wB
-�B
.}B
/OB
/iB
/OB
/�B
0;B
0�B
0�B
0�B
0�B
1AB
1AB
1[B
1�B
2-B
2aB
3B
2�B
33B
3MB
3�B
4B
4TB
4B
4�B
4�B
5?B
5%B
5?B
5�B
5�B
6FB
6�B
6�B
6�B
6�B
7�B
8RB
88B
8RB
8lB
8lB
9$B
9>B
9$B
9XB
:*B
:�B
;JB
;JB
;dB
;B
;0B
;B
:�B
;0B
;JB
<6B
<�B
<�B
<�B
<jB
<�B
=VB
=�B
=�B
>B
>�B
>�B
>�B
>�B
>�B
>�B
?cB
@ B
?�B
?�B
?�B
@ B
@�B
@�B
@B
@4B
@iB
@�B
AB
A�B
A�B
A�B
A�B
B[B
BuB
B�B
B�B
B�B
B�B
C�B
DMB
D�B
D�B
EmB
E�B
F�B
GB
F�B
F�B
G_B
G+B
HKB
H�B
I7B
IlB
IlB
I�B
JrB
KDB
K�B
K�B
LdB
L�B
L�B
MB
MPB
M�B
N<B
N�B
N�B
N�B
O(B
O\B
OvB
O�B
PB
PB
P.B
P�B
Q B
Q�B
Q�B
RB
R�B
R�B
R�B
SB
S&B
S@B
S�B
TFB
TFB
T{B
TFB
T�B
T�B
T�B
U2B
U2B
U2B
U�B
U�B
U�B
VB
VSB
V�B
V�B
VmB
V�B
V�B
W
B
W�B
W�B
W�B
X+B
XB
X+B
X+B
XEB
X+B
X�B
X�B
X�B
YB
YB
Y1B
Y�B
YeB
Y�B
ZkB
Z�B
Z�B
[=B
[�B
[�B
[�B
[�B
[�B
\]B
\xB
\xB
\]B
\�B
\�B
\�B
\�B
\�B
]IB
]dB
]dB
]�B
]�B
]�B
]�B
^B
^�B
^�B
^�B
^�B
^�B
^�B
_B
_VB
_�B
_�B
`BB
`�B
`�B
`�B
`�B
`�B
a�B
a�B
a�B
a�B
a�B
a�B
bNB
bNB
bhB
b�B
cB
cTB
cnB
cnB
dB
c�B
d@B
d�B
d�B
d�B
d�B
e,B
e`B
ezB
ezB
e�B
e�B
e�B
e�B
e�B
fB
fB
f�B
f�B
gB
gRB
g�B
g�B
g�B
g�B
h
B
h$B
h$B
h$B
h
B
g�B
hXB
h�B
h�B
h�B
h�B
iyB
i�B
i�B
jB
j0B
jeB
j�B
kB
kB
k�B
lB
l"B
l=B
l"B
l=B
lWB
l�B
l�B
m)B
m]B
mwB
m�B
m�B
m�B
n/B
ncB
n}B
n�B
o5B
pB
poB
p�B
p�B
q'B
qAB
q�B
q�B
q�B
rGB
raB
r�B
r�B
r�B
r�B
s3B
shB
s�B
t9B
tB
tTB
t�B
t�B
t�B
uB
u�B
u�B
u�B
vB
v+B
vFB
vzB
v�B
v�B
v�B
v�B
v�B
v�B
wB
w2B
wfB
w�B
w�B
w�B
w�B
xB
xlB
x�B
x�B
x�B
yXB
yrB
y�B
y�B
y�B
y�B
zDB
z^B
zxB
z�B
z�B
{B
{B
{B
{�B
|B
|6B
|6B
|PB
|jB
|jB
|�B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
}qB
}�B
}�B
}�B
}�B
~B
~(B
~]B
~�B
~�B
~�B
~�B
HB
cB
HB
cB
�B
�B
�B
� B
�OB
��B
��B
��B
��B
��B
��B
� B
�;B
�UB
�U1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104958  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175436  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175437  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175437                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025444  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025444  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141506                      G�O�G�O�G�O�                