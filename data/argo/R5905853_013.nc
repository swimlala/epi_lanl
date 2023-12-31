CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:24:49Z creation;2022-06-04T17:24:50Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604172449  20220610131506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @������1   @���=�/h@-$Z�1�c�Z�11   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @&ff@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�ffA�33A�33B   B  B  B  B   B(  B0  B8  B?��BH  BP  BX  B`  Bh  Bq33Bw��B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  C   C�fC  CL�C��C	�fC�fC  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C433C6  C8  C:  C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`33Ca�fCc�fCf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP�fDQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}�fD~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�s31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @(��@�G�@�G�A ��A ��A@��A`��A�Q�A�Q�A�Q�A�Q�A�Q�AҸRA߅A�B (�B(�B(�B(�B (�B((�B0(�B8(�B?BH(�BP(�BX(�B`(�Bh(�Bq\)BwBB�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�G�B�z�B�{B�{B��HB�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�G�B��HB�{B�{C 
=C�C
=CW
C�
C	�C�C
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
=C2
=C4=pC6
=C8
=C:
=C;�C>
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
=C^
=C`=pCa�Cc�Cf
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
=C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�D{D��{D��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHDHD��HD�HD�AHDÁHD��HD�HD�AHDāHD��HD�HD�AHDŁHD��HD�HD�AHDƁHD��HD�HD�AHDǁHD��HD�HD�AHDȁHD��HD�HD�AHDɁHD��HD�HD�AHDʁHD��HD�HD�AHDˁHD��HD�HD�AHD́HD��HD�HD�AHD́HD��HD�HD�AHD΁HD��HD�HD�AHDρHD��HD�HD�AHDЁHD��HD�HD�AHDсHD��HD�HD�AHDҁHD��HD�HD�AHDӁHD��HD�HD�AHDԁHD��HD�HD�AHDՁHD��HD�HD�AHDցHD��HD�HD�AHDׁHD��HD�HD�AHD؁HD��HD�HD�AHDفHD��HD�HD�AHDځHD��HD�HD�AHDہHD��HD�HD�AHD܁HD��HD�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD�t{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��JA��A�;A��A�{A��A��A�1A�A�
�A�+A�A��A���A׻0A�!-A��zA֝�A֒A�x�A�NA� �A��AՑ A�F�AѓuAЁoA��cÄ́�A��AʃGA�.}AɈ�A�qvA� A���Aǉ�A�~]A�R�A�*eAƵ?A��A��QAĘ�A�A��AĘ�A�A��GA�!A��dA��3A�JXA�OA�v�A�~�A��A�gmA��A���A���A�>wA�J�A���A��A���A��sA�Z�A�OA��A�I�A�@�A�NpA�)�A��A�ɆA��5A���A���A�/A��XA���A��A�ncA�B'A��]A�r�A��A�2�A�0�A�JXA}o Ar�zAo��Ajj�Ah�Ag��AgaAe	lAcrGAa.IA\�rA[i�AY��AU$�AR�AMںAJl"AH�WAE�FAC^�A>�sA<�	A:iDA9u�A9��A9%A7�HA6˒A5b�A5�mA5N�A3U�A5@A2�hA2g�A1�A1�0A1�A.��A.l�A-�jA,�zA,�A+zA*c�A(��A({JA'�A'M�A&یA&C-A%�jA%J�A$�[A#�A#��A"}�A!A 	�A!�A!+A!A A MjA}VA��A 6zA �~A��A�A��A�A"�A��A��Aa�AA'RA|A�A�kASA˒A��Ag8A�AA��A	A�'A~(A��A��A!�A��A|�A8A�A�yAh
AMA�A��A+A҉AW?A:*A!A��A�A=A�/Ae,A�A�$A�MA$tA͟A��A/�A��A{JA,�A�A�"Aq�A
�)A
T�A
GA	��A	�	A	OA	7LA�`Al�A&�A��A�AI�A��A��A�:A��A�uA�5A`BA1�A+A�$ATaA=�A �A P�A J#@�j@��@��3@�A�@�'R@�$t@��p@��@�PH@��N@��Q@�`B@�@�oi@�Ov@��@��@���@�\�@��C@��4@�@�@@�l"@��@�.I@�C@�-�@��w@�+@�Ĝ@썹@�1�@�0@�m]@�]�@� \@��@�R@�{�@�@�@�$@���@�|�@�:@��X@���@�n/@�@O@���@�Q@�1@��@��@≠@��@�j�@���@�4@�	@߇�@��@�5?@�\)@�҉@�w�@�@�@�F@�+k@��z@�s�@��@�h�@׾w@�v`@�@@ֈ�@��@�s�@��@ԸR@�D�@ӿH@��@ҹ$@��@Ѻ^@ѥ@ъ	@�t�@�dZ@�]�@�IR@��@�kQ@ϧ�@�|�@��@�;d@̬�@̋D@˵t@ʇ�@�($@�
�@ɥ@�+@���@ȷ�@�h�@���@�@O@ƕ�@�S�@��d@�S&@���@�Ta@��@Õ�@�Z�@�?}@�q@¼j@�,=@�ϫ@�`B@��@��|@���@�V�@�_@���@���@�U�@�a|@���@���@��"@���@�6�@��@���@��@���@���@�c @��9@��@�X�@�?}@��M@�I�@� �@�k�@�=@��|@��D@�q�@�Ft@��@���@�~�@�a�@�*0@���@���@�� @�@���@�N<@�,�@��L@�=q@�ԕ@���@�a�@��c@��@�j�@�>�@���@���@�M@��@��F@�Mj@��@��/@��@�K^@���@��z@��^@���@�Z�@�Vm@�O@�,�@�҉@�y>@�:�@���@���@�zx@�b�@�IR@�C@��P@�ی@��'@��!@�v�@�%�@���@���@�8�@��@��.@��.@�@�Ɇ@�7@���@�a�@��@��@��@�C�@��@��y@�PH@�4n@�7@��@��d@�{J@�!-@��!@���@�YK@��@��@��@���@���@�PH@��N@��@� i@���@�<�@�$@��9@���@�}�@�'�@��@��,@���@�A�@�u�@�C@��B@�u%@�"h@���@�o�@�@O@�-w@�&@��,@�_�@�j�@�)_@��2@��O@�@�@���@�j�@�L�@�"�@���@�e@��m@��0@��@�S�@�0�@� i@��@�C�@��^@���@�w2@�Dg@�/@���@���@�Xy@�	@��&@���@�o�@�]�@�W?@�@���@��6@�M�@�@�@���@���@�~�@��@�خ@���@�Dg@��f@���@��L@�C�@���@���@�zx@�o�@�W?@�?}@�&�@��/@��o@�)�@��@���@���@�H�@�4@��j@�z�@�c�@�A�@��@���@�N<@�!�@��f@�ȴ@��z@�Ta@��+@���@���@���@�_p@�;d@�Y@���@���@�I�@��@�$@~�@~�X@~��@}�Z@}�@}k�@|��@|tT@{�+@{|�@{$t@z($@y\�@x��@xw�@x%�@w�}@w��@w�@v��@va|@u�@u?}@t�z@t�@s��@sO@r�@r�m@rTa@q�@qk�@p��@p�@o1�@n��@n�\@nu%@n{@m�'@m	l@l�)@lr�@k��@jv�@io @i�@h��@h~(@h$@gƨ@g;d@f��@f\�@e�@d�@c�w@c��@cRT@c
=@b��@b��@b�h@b�F@bz@a��@aG�@`�.@_�F@__p@_�@^��@^�@]p�@\�@\bN@\M@[��@[$t@Z�h@Z��@Z��@Z�@Z��@Z}V@Z#:@Y�@Y�@Y}�@Y	l@X�@Xw�@X!@W�{@W/�@W�@V҉@Vu%@Uԕ@Uo @U�@T�@T[�@TK^@T'R@S˒@S9�@R�'@R^5@Q��@Q�@QQ�@Q(�@P�U@P�Y@Pm�@PPH@O�r@O�k@O=@N�@N��@N	@M��@M��@MT�@M�@L�@L��@L9X@LG@K�@K��@K~�@K+@J�2@J�b@JQ@I��@I��@I?}@H�f@H��@HC-@H7@G��@G��@Gj�@G�@F�c@F�B@F��@FC�@E��@E�X@E��@E�~@E[W@D�@D[�@C�]@C�g@Cv`@B��@B�\@B�@A��@AX@A \@A�@@��@@PH@@A�@?��@?�q@?@O@>�y@>R�@>�@=�@=��@=�@=N<@<�@<u�@;��@;��@;v`@;J#@;
=@:��@:1�@:@:�@9ԕ@9N<@8��@8�@7�w@7qv@6҉@6�1@6c @6#:@5��@5Dg@4֡@4�o@4oi@46@3K�@2�M@2�@2�@2�R@2q�@2B[@2 �@1N<@1q@0��@0U2@/��@/��@/Mj@.�2@.��@.V@.L0@-�@-c�@-&�@,�`@,~(@,/�@+��@+=@*�@*��@*�@*3�@)�@)��@)@(bN@'��@'{J@'8@' i@&��@&s�@&5?@&�@%�@%��@%��@%m]@%^�@%A @%!�@%@$�@$N�@$1@#��@#,�@"��@"+k@!�T@!��@!k�@! \@!�@ �@ ��@ ��@ w�@ e�@ [�@ 4n@ 'R@ �@��@�@ߤ@�r@kQ@#:@�@c@5�@Ɇ@�.@z�@6@/�@/�@-�@"h@��@��@@O@,�@Y@��@�,@�A@�@�N@�M@[W@=�@�@�$@K^@�+@��@RT@=@6z@(@��@�s@�<@�@��@YK@�@��@�'@��@�@m�@M@D�@9X@1'@�]@��@@O@(@ں@�b@d�@($@4@�.@��@ϫ@�^@�=@�M@[W@�@�@|�@D�@خ@~�@P�@)_@ں@��@�@��@�b@�x@s�@C�@_@��@��@��@�@��@��@��@��@�M@j@5�@�@��@��@�I@m�@%�@�]@��@�V@��@�:@y�@qv@j�@e�@U�@1�@Y@
�c@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��JA��A�;A��A�{A��A��A�1A�A�
�A�+A�A��A���A׻0A�!-A��zA֝�A֒A�x�A�NA� �A��AՑ A�F�AѓuAЁoA��cÄ́�A��AʃGA�.}AɈ�A�qvA� A���Aǉ�A�~]A�R�A�*eAƵ?A��A��QAĘ�A�A��AĘ�A�A��GA�!A��dA��3A�JXA�OA�v�A�~�A��A�gmA��A���A���A�>wA�J�A���A��A���A��sA�Z�A�OA��A�I�A�@�A�NpA�)�A��A�ɆA��5A���A���A�/A��XA���A��A�ncA�B'A��]A�r�A��A�2�A�0�A�JXA}o Ar�zAo��Ajj�Ah�Ag��AgaAe	lAcrGAa.IA\�rA[i�AY��AU$�AR�AMںAJl"AH�WAE�FAC^�A>�sA<�	A:iDA9u�A9��A9%A7�HA6˒A5b�A5�mA5N�A3U�A5@A2�hA2g�A1�A1�0A1�A.��A.l�A-�jA,�zA,�A+zA*c�A(��A({JA'�A'M�A&یA&C-A%�jA%J�A$�[A#�A#��A"}�A!A 	�A!�A!+A!A A MjA}VA��A 6zA �~A��A�A��A�A"�A��A��Aa�AA'RA|A�A�kASA˒A��Ag8A�AA��A	A�'A~(A��A��A!�A��A|�A8A�A�yAh
AMA�A��A+A҉AW?A:*A!A��A�A=A�/Ae,A�A�$A�MA$tA͟A��A/�A��A{JA,�A�A�"Aq�A
�)A
T�A
GA	��A	�	A	OA	7LA�`Al�A&�A��A�AI�A��A��A�:A��A�uA�5A`BA1�A+A�$ATaA=�A �A P�A J#@�j@��@��3@�A�@�'R@�$t@��p@��@�PH@��N@��Q@�`B@�@�oi@�Ov@��@��@���@�\�@��C@��4@�@�@@�l"@��@�.I@�C@�-�@��w@�+@�Ĝ@썹@�1�@�0@�m]@�]�@� \@��@�R@�{�@�@�@�$@���@�|�@�:@��X@���@�n/@�@O@���@�Q@�1@��@��@≠@��@�j�@���@�4@�	@߇�@��@�5?@�\)@�҉@�w�@�@�@�F@�+k@��z@�s�@��@�h�@׾w@�v`@�@@ֈ�@��@�s�@��@ԸR@�D�@ӿH@��@ҹ$@��@Ѻ^@ѥ@ъ	@�t�@�dZ@�]�@�IR@��@�kQ@ϧ�@�|�@��@�;d@̬�@̋D@˵t@ʇ�@�($@�
�@ɥ@�+@���@ȷ�@�h�@���@�@O@ƕ�@�S�@��d@�S&@���@�Ta@��@Õ�@�Z�@�?}@�q@¼j@�,=@�ϫ@�`B@��@��|@���@�V�@�_@���@���@�U�@�a|@���@���@��"@���@�6�@��@���@��@���@���@�c @��9@��@�X�@�?}@��M@�I�@� �@�k�@�=@��|@��D@�q�@�Ft@��@���@�~�@�a�@�*0@���@���@�� @�@���@�N<@�,�@��L@�=q@�ԕ@���@�a�@��c@��@�j�@�>�@���@���@�M@��@��F@�Mj@��@��/@��@�K^@���@��z@��^@���@�Z�@�Vm@�O@�,�@�҉@�y>@�:�@���@���@�zx@�b�@�IR@�C@��P@�ی@��'@��!@�v�@�%�@���@���@�8�@��@��.@��.@�@�Ɇ@�7@���@�a�@��@��@��@�C�@��@��y@�PH@�4n@�7@��@��d@�{J@�!-@��!@���@�YK@��@��@��@���@���@�PH@��N@��@� i@���@�<�@�$@��9@���@�}�@�'�@��@��,@���@�A�@�u�@�C@��B@�u%@�"h@���@�o�@�@O@�-w@�&@��,@�_�@�j�@�)_@��2@��O@�@�@���@�j�@�L�@�"�@���@�e@��m@��0@��@�S�@�0�@� i@��@�C�@��^@���@�w2@�Dg@�/@���@���@�Xy@�	@��&@���@�o�@�]�@�W?@�@���@��6@�M�@�@�@���@���@�~�@��@�خ@���@�Dg@��f@���@��L@�C�@���@���@�zx@�o�@�W?@�?}@�&�@��/@��o@�)�@��@���@���@�H�@�4@��j@�z�@�c�@�A�@��@���@�N<@�!�@��f@�ȴ@��z@�Ta@��+@���@���@���@�_p@�;d@�Y@���@���@�I�@��@�$@~�@~�X@~��@}�Z@}�@}k�@|��@|tT@{�+@{|�@{$t@z($@y\�@x��@xw�@x%�@w�}@w��@w�@v��@va|@u�@u?}@t�z@t�@s��@sO@r�@r�m@rTa@q�@qk�@p��@p�@o1�@n��@n�\@nu%@n{@m�'@m	l@l�)@lr�@k��@jv�@io @i�@h��@h~(@h$@gƨ@g;d@f��@f\�@e�@d�@c�w@c��@cRT@c
=@b��@b��@b�h@b�F@bz@a��@aG�@`�.@_�F@__p@_�@^��@^�@]p�@\�@\bN@\M@[��@[$t@Z�h@Z��@Z��@Z�@Z��@Z}V@Z#:@Y�@Y�@Y}�@Y	l@X�@Xw�@X!@W�{@W/�@W�@V҉@Vu%@Uԕ@Uo @U�@T�@T[�@TK^@T'R@S˒@S9�@R�'@R^5@Q��@Q�@QQ�@Q(�@P�U@P�Y@Pm�@PPH@O�r@O�k@O=@N�@N��@N	@M��@M��@MT�@M�@L�@L��@L9X@LG@K�@K��@K~�@K+@J�2@J�b@JQ@I��@I��@I?}@H�f@H��@HC-@H7@G��@G��@Gj�@G�@F�c@F�B@F��@FC�@E��@E�X@E��@E�~@E[W@D�@D[�@C�]@C�g@Cv`@B��@B�\@B�@A��@AX@A \@A�@@��@@PH@@A�@?��@?�q@?@O@>�y@>R�@>�@=�@=��@=�@=N<@<�@<u�@;��@;��@;v`@;J#@;
=@:��@:1�@:@:�@9ԕ@9N<@8��@8�@7�w@7qv@6҉@6�1@6c @6#:@5��@5Dg@4֡@4�o@4oi@46@3K�@2�M@2�@2�@2�R@2q�@2B[@2 �@1N<@1q@0��@0U2@/��@/��@/Mj@.�2@.��@.V@.L0@-�@-c�@-&�@,�`@,~(@,/�@+��@+=@*�@*��@*�@*3�@)�@)��@)@(bN@'��@'{J@'8@' i@&��@&s�@&5?@&�@%�@%��@%��@%m]@%^�@%A @%!�@%@$�@$N�@$1@#��@#,�@"��@"+k@!�T@!��@!k�@! \@!�@ �@ ��@ ��@ w�@ e�@ [�@ 4n@ 'R@ �@��@�@ߤ@�r@kQ@#:@�@c@5�@Ɇ@�.@z�@6@/�@/�@-�@"h@��@��@@O@,�@Y@��@�,@�A@�@�N@�M@[W@=�@�@�$@K^@�+@��@RT@=@6z@(@��@�s@�<@�@��@YK@�@��@�'@��@�@m�@M@D�@9X@1'@�]@��@@O@(@ں@�b@d�@($@4@�.@��@ϫ@�^@�=@�M@[W@�@�@|�@D�@خ@~�@P�@)_@ں@��@�@��@�b@�x@s�@C�@_@��@��@��@�@��@��@��@��@�M@j@5�@�@��@��@�I@m�@%�@�]@��@�V@��@�:@y�@qv@j�@e�@U�@1�@Y@
�c@
�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	2�B	3B	2�B	2�B	2�B	2�B	2�B	33B	2GB	2aB	2�B	2�B	2�B	;�B	I�B	f2B	m�B	nIB	m�B	l�B	jeB	g�B	e�B	d�B	n�B	�AB	��B	� B	��B	x�B	�MB	�B	��B	��B	�yB	��B	��B	�B	�pB	ՁB	�QB
B
;�B
q�B
��B
̈́B
��B
�PB
�iB
�fB
ƎB
� B
�B�B*0B5%BH�B\�Bk�Bx�B��B�XB�BB�tB�	B��BxBs3Bf�BmwBu�BxBpBNB8B#nB�B
��B
��B
�MB
�AB
�3B
�*B
�4B
��B
��B
��B
.B
i�B
;0B
(�B
�B	��B	� B	��B	v�B	t�B	r�B	jB	bB	U�B	D3B	:DB	0B	�B	B	�B�.B�BB�DB�B��B��B�B�GB	B	{B	B	
�B	
rB	B	9�B	6�B	X�B	[�B	o�B	�B	��B	�B	��B	��B	�cB	�	B	̘B	�fB	��B	� B	ªB	ǮB	ɠB	�}B	�B	�	B	�;B	�B	�B	�OB	�KB	өB	�HB	��B	��B	�B	�iB	�mB	�B	��B

�B
	�B
	�B
�B
aB
�B
�B	�qB	��B	�B	��B
 OB
	�B

�B
B
�B
<B
�B
FB
�B
B
�B
 �B
#�B
%�B
&�B
(sB
)�B
,�B
/iB
/�B
0oB
0�B
0�B
3B
6�B
7LB
6zB
5tB
6+B
7�B
9�B
:�B
;dB
<B
=�B
<�B
<jB
<B
<B
<B
<�B
<�B
=<B
=�B
=�B
<�B
;�B
:�B
:xB
9�B
9	B
8�B
88B
7�B
7�B
6�B
6+B
4�B
49B
4B
2�B
1[B
-wB
'B
,�B
*B
'�B
$�B
$&B
#�B
"hB
 �B
 vB
�B
OB
!�B
�B
�B
�B
�B
FB
�B
�B
�B
B
�B
OB
B
�B
/B
�B
�B
�B
qB
�B
B
EB
�B
�B
0B

�B
�B

	B
�B
SB
gB
-B
�B
uB
'B
B
%B
	�B
jB
�B
�B
jB
"B
�B
PB
~B

rB
	�B
�B
	RB

	B

=B
^B
JB
^B

�B

	B
	7B

	B

XB

=B
	lB
	lB
+B
B
�B
�B
'B
SB
�B
SB
9B
�B
�B
mB
9B
9B
�B
�B
YB
tB
tB
?B
tB
�B
�B
�B
�B
B
�B
�B
+B
EB
�B
�B
�B
zB
?B
MB
�B
'B
'B
�B
UB
UB
oB
'B
�B
B
9B
�B
zB
?B
%B
EB
zB
zB
�B
�B
B
�B
fB
�B
�B
�B
	B
	�B
	�B
	�B
	�B
	�B

#B

rB

�B

=B
	�B

�B
)B
B
)B
xB
�B
�B
�B
dB
dB
JB
dB
6B
B
6B
B
PB
�B
<B
�B
�B
BB
�B
BB
vB
�B
�B
vB
�B
�B
B
B
B
�B
�B
�B
(B
�B
\B
\B
\B
�B
vB
pB
�B
�B
jB
VB
�B
VB
B
�B
.B
vB
�B
�B
\B
�B
�B
�B
 B
�B
�B
�B
�B
uB
uB
�B
�B
�B
B
B
B
B
,B
B
B
aB
{B
FB
�B
@B
�B
oB
B
[B
�B
�B
�B
�B
�B
�B
+B
�B
B
�B
yB
EB
_B
_B
EB
B
EB
EB
yB
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
KB
eB
KB
B
eB
1B
�B
�B
B
�B
7B
�B
	B
�B
CB
�B
�B
�B
/B
�B
�B
!B
B
�B
pB
�B
 'B
 B
�B
 �B
!B
!B
!B
!bB
!HB
"4B
"�B
#TB
#B
"�B
"�B
"�B
#TB
#nB
$B
$@B
$&B
$�B
%�B
&B
&LB
&LB
&�B
&�B
&B
&LB
%�B
&B
&�B
'B
'mB
)�B
,"B
+�B
,"B
,�B
-)B
,�B
,�B
-B
-�B
.cB
.}B
/ B
.�B
/B
/�B
/iB
0;B
0�B
0�B
1B
0�B
1[B
2GB
2|B
2-B
2|B
2aB
2�B
2�B
2�B
2�B
3MB
3hB
3�B
3hB
3�B
4TB
5?B
5B
4�B
5�B
5�B
5�B
6FB
6�B
7B
7fB
7LB
7fB
7fB
7�B
7LB
7�B
7�B
7�B
8lB
8�B
9�B
9�B
9�B
:�B
:�B
:�B
:�B
;B
;dB
;dB
<B
<6B
<�B
<�B
<�B
=<B
=VB
=VB
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>B
>(B
>wB
>wB
>]B
>�B
?}B
@4B
@OB
@iB
@�B
@�B
@�B
A;B
AUB
AoB
A�B
BuB
B�B
B�B
B�B
B�B
B�B
CB
C{B
C�B
C�B
C�B
C�B
D�B
DMB
D�B
D�B
EB
EB
E9B
D�B
DgB
DMB
DgB
DgB
D�B
D�B
ESB
E�B
E�B
FB
FYB
FtB
FtB
F�B
F�B
F�B
GB
G+B
G_B
GzB
G�B
G�B
G�B
HfB
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I7B
I�B
J=B
J�B
J�B
J�B
J�B
K^B
KxB
KxB
KxB
K�B
K�B
L0B
L~B
L�B
M6B
MjB
M�B
M�B
M�B
NB
NpB
N�B
N�B
N�B
N�B
OBB
O�B
PB
P.B
P}B
P�B
P�B
Q B
QB
Q B
Q�B
Q�B
RB
RoB
RoB
R�B
R�B
R�B
R�B
S&B
S�B
S�B
S�B
S�B
T,B
T�B
T{B
TaB
T�B
T�B
T�B
T�B
UMB
U�B
U�B
U�B
U�B
VB
VmB
V�B
V�B
V�B
WsB
W�B
XEB
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
ZB
ZQB
Z�B
Z�B
[	B
Z�B
[WB
[�B
\]B
\CB
\)B
\CB
\�B
\�B
]IB
]dB
]dB
]/B
]�B
^B
^OB
^�B
_B
_pB
_�B
_�B
`'B
`vB
`�B
`�B
`�B
`�B
`�B
`�B
a-B
`�B
a-B
`�B
a-B
a�B
a|B
b�B
cTB
c�B
dB
d�B
d�B
d�B
e,B
e,B
e`B
eFB
e�B
e�B
ezB
e�B
e�B
ffB
ffB
f�B
f�B
gB
g8B
gmB
g�B
g�B
g�B
h
B
h$B
h$B
hXB
hsB
h�B
h�B
iB
iDB
i�B
i�B
j�B
j�B
kB
kB
kkB
k�B
k�B
k�B
lB
l"B
l"B
l=B
lWB
lWB
l=B
lWB
l�B
m)B
mCB
m�B
mwB
m�B
nB
nIB
n�B
o5B
o�B
o�B
pB
p;B
p!B
p!B
p;B
p;B
p�B
qAB
q�B
q�B
q�B
q�B
r-B
raB
r�B
r�B
r�B
r�B
r�B
r�B
sB
shB
s�B
tB
tB
tB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
uB
u?B
uZB
u�B
u�B
vFB
v`B
vzB
vzB
v`B
vzB
v�B
wB
wLB
w�B
w�B
xB
x8B
x8B
x8B
xRB
x�B
x�B
x�B
y	B
y$B
y�B
z*B
zB
zDB
z�B
z�B
{0B
{B
{�B
{�B
{�B
{�B
{�B
{�B
|B
|6B
|6B
|jB
|jB
|�B
|�B
|�B
|�B
}B
}VB
}qB
}�B
~B
~]B
~�B
~�B
.B
.B
HB
cB
�B
�B
�4B
�B
�B
�B
�B
��B
� B
�oB
� B
� B
�U1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	2�B	3B	2�B	2�B	2�B	2�B	2�B	33B	2GB	2aB	2�B	2�B	2�B	;�B	I�B	f2B	m�B	nIB	m�B	l�B	jeB	g�B	e�B	d�B	n�B	�AB	��B	� B	��B	x�B	�MB	�B	��B	��B	�yB	��B	��B	�B	�pB	ՁB	�QB
B
;�B
q�B
��B
̈́B
��B
�PB
�iB
�fB
ƎB
� B
�B�B*0B5%BH�B\�Bk�Bx�B��B�XB�BB�tB�	B��BxBs3Bf�BmwBu�BxBpBNB8B#nB�B
��B
��B
�MB
�AB
�3B
�*B
�4B
��B
��B
��B
.B
i�B
;0B
(�B
�B	��B	� B	��B	v�B	t�B	r�B	jB	bB	U�B	D3B	:DB	0B	�B	B	�B�.B�BB�DB�B��B��B�B�GB	B	{B	B	
�B	
rB	B	9�B	6�B	X�B	[�B	o�B	�B	��B	�B	��B	��B	�cB	�	B	̘B	�fB	��B	� B	ªB	ǮB	ɠB	�}B	�B	�	B	�;B	�B	�B	�OB	�KB	өB	�HB	��B	��B	�B	�iB	�mB	�B	��B

�B
	�B
	�B
�B
aB
�B
�B	�qB	��B	�B	��B
 OB
	�B

�B
B
�B
<B
�B
FB
�B
B
�B
 �B
#�B
%�B
&�B
(sB
)�B
,�B
/iB
/�B
0oB
0�B
0�B
3B
6�B
7LB
6zB
5tB
6+B
7�B
9�B
:�B
;dB
<B
=�B
<�B
<jB
<B
<B
<B
<�B
<�B
=<B
=�B
=�B
<�B
;�B
:�B
:xB
9�B
9	B
8�B
88B
7�B
7�B
6�B
6+B
4�B
49B
4B
2�B
1[B
-wB
'B
,�B
*B
'�B
$�B
$&B
#�B
"hB
 �B
 vB
�B
OB
!�B
�B
�B
�B
�B
FB
�B
�B
�B
B
�B
OB
B
�B
/B
�B
�B
�B
qB
�B
B
EB
�B
�B
0B

�B
�B

	B
�B
SB
gB
-B
�B
uB
'B
B
%B
	�B
jB
�B
�B
jB
"B
�B
PB
~B

rB
	�B
�B
	RB

	B

=B
^B
JB
^B

�B

	B
	7B

	B

XB

=B
	lB
	lB
+B
B
�B
�B
'B
SB
�B
SB
9B
�B
�B
mB
9B
9B
�B
�B
YB
tB
tB
?B
tB
�B
�B
�B
�B
B
�B
�B
+B
EB
�B
�B
�B
zB
?B
MB
�B
'B
'B
�B
UB
UB
oB
'B
�B
B
9B
�B
zB
?B
%B
EB
zB
zB
�B
�B
B
�B
fB
�B
�B
�B
	B
	�B
	�B
	�B
	�B
	�B

#B

rB

�B

=B
	�B

�B
)B
B
)B
xB
�B
�B
�B
dB
dB
JB
dB
6B
B
6B
B
PB
�B
<B
�B
�B
BB
�B
BB
vB
�B
�B
vB
�B
�B
B
B
B
�B
�B
�B
(B
�B
\B
\B
\B
�B
vB
pB
�B
�B
jB
VB
�B
VB
B
�B
.B
vB
�B
�B
\B
�B
�B
�B
 B
�B
�B
�B
�B
uB
uB
�B
�B
�B
B
B
B
B
,B
B
B
aB
{B
FB
�B
@B
�B
oB
B
[B
�B
�B
�B
�B
�B
�B
+B
�B
B
�B
yB
EB
_B
_B
EB
B
EB
EB
yB
�B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
KB
eB
KB
B
eB
1B
�B
�B
B
�B
7B
�B
	B
�B
CB
�B
�B
�B
/B
�B
�B
!B
B
�B
pB
�B
 'B
 B
�B
 �B
!B
!B
!B
!bB
!HB
"4B
"�B
#TB
#B
"�B
"�B
"�B
#TB
#nB
$B
$@B
$&B
$�B
%�B
&B
&LB
&LB
&�B
&�B
&B
&LB
%�B
&B
&�B
'B
'mB
)�B
,"B
+�B
,"B
,�B
-)B
,�B
,�B
-B
-�B
.cB
.}B
/ B
.�B
/B
/�B
/iB
0;B
0�B
0�B
1B
0�B
1[B
2GB
2|B
2-B
2|B
2aB
2�B
2�B
2�B
2�B
3MB
3hB
3�B
3hB
3�B
4TB
5?B
5B
4�B
5�B
5�B
5�B
6FB
6�B
7B
7fB
7LB
7fB
7fB
7�B
7LB
7�B
7�B
7�B
8lB
8�B
9�B
9�B
9�B
:�B
:�B
:�B
:�B
;B
;dB
;dB
<B
<6B
<�B
<�B
<�B
=<B
=VB
=VB
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>B
>(B
>wB
>wB
>]B
>�B
?}B
@4B
@OB
@iB
@�B
@�B
@�B
A;B
AUB
AoB
A�B
BuB
B�B
B�B
B�B
B�B
B�B
CB
C{B
C�B
C�B
C�B
C�B
D�B
DMB
D�B
D�B
EB
EB
E9B
D�B
DgB
DMB
DgB
DgB
D�B
D�B
ESB
E�B
E�B
FB
FYB
FtB
FtB
F�B
F�B
F�B
GB
G+B
G_B
GzB
G�B
G�B
G�B
HfB
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I7B
I�B
J=B
J�B
J�B
J�B
J�B
K^B
KxB
KxB
KxB
K�B
K�B
L0B
L~B
L�B
M6B
MjB
M�B
M�B
M�B
NB
NpB
N�B
N�B
N�B
N�B
OBB
O�B
PB
P.B
P}B
P�B
P�B
Q B
QB
Q B
Q�B
Q�B
RB
RoB
RoB
R�B
R�B
R�B
R�B
S&B
S�B
S�B
S�B
S�B
T,B
T�B
T{B
TaB
T�B
T�B
T�B
T�B
UMB
U�B
U�B
U�B
U�B
VB
VmB
V�B
V�B
V�B
WsB
W�B
XEB
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
ZB
ZQB
Z�B
Z�B
[	B
Z�B
[WB
[�B
\]B
\CB
\)B
\CB
\�B
\�B
]IB
]dB
]dB
]/B
]�B
^B
^OB
^�B
_B
_pB
_�B
_�B
`'B
`vB
`�B
`�B
`�B
`�B
`�B
`�B
a-B
`�B
a-B
`�B
a-B
a�B
a|B
b�B
cTB
c�B
dB
d�B
d�B
d�B
e,B
e,B
e`B
eFB
e�B
e�B
ezB
e�B
e�B
ffB
ffB
f�B
f�B
gB
g8B
gmB
g�B
g�B
g�B
h
B
h$B
h$B
hXB
hsB
h�B
h�B
iB
iDB
i�B
i�B
j�B
j�B
kB
kB
kkB
k�B
k�B
k�B
lB
l"B
l"B
l=B
lWB
lWB
l=B
lWB
l�B
m)B
mCB
m�B
mwB
m�B
nB
nIB
n�B
o5B
o�B
o�B
pB
p;B
p!B
p!B
p;B
p;B
p�B
qAB
q�B
q�B
q�B
q�B
r-B
raB
r�B
r�B
r�B
r�B
r�B
r�B
sB
shB
s�B
tB
tB
tB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
uB
u?B
uZB
u�B
u�B
vFB
v`B
vzB
vzB
v`B
vzB
v�B
wB
wLB
w�B
w�B
xB
x8B
x8B
x8B
xRB
x�B
x�B
x�B
y	B
y$B
y�B
z*B
zB
zDB
z�B
z�B
{0B
{B
{�B
{�B
{�B
{�B
{�B
{�B
|B
|6B
|6B
|jB
|jB
|�B
|�B
|�B
|�B
}B
}VB
}qB
}�B
~B
~]B
~�B
~�B
.B
.B
HB
cB
�B
�B
�4B
�B
�B
�B
�B
��B
� B
�oB
� B
� B
�U1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104847  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172449  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172450  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172450                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022457  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022457  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131506                      G�O�G�O�G�O�                