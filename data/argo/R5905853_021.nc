CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:26:21Z creation;2022-06-04T17:26:21Z conversion to V3.1      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t    TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Ͱ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �x   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �x   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �x   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �x   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �$Argo profile    3.1 1.2 19500101000000  20220604172621  20220610131506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @��	F��1   @��
�A�@/��hr��cq�S���1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @&ff@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0ffB8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�33B�  B�33B�  B�ffB癚B�  B�  B�33B�33B���B�  C�C  C  C�fC
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0�C2�C4  C6  C7�fC:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`�Cb�Cd�3Ce��Cg�fCi�fCl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4�fD5fD5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� DZ��D[y�D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� DfD� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�s311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @(��@�G�@�G�A ��A ��A@��A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B (�B(�B(�B(�B (�B(�\B0�\B8(�B@(�BH(�BP(�BX(�B`(�Bh(�Bp(�Bx(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�G�B��HB�{B�G�B�{B�G�B�{B�z�B�B�{B�{B�G�B�G�B��HB�{C#�C
=C
=C�C

=C
=C
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
=C0#�C2#�C4
=C6
=C7�C:
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
=C^
=C`#�Cb#�Cd�pCe�
Cg�Ci�Cl
=Cn
=Cp
=Cr
=Ct
=Cv
=Cx
=Cz
=C|
=C~
=C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��RC�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��RC�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��DZ�)D[|)D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��{D��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��{D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHDHD��HD�HD�AHDÁHD��HD�HD�AHDāHD��HD�HD�AHDŁHD��HD�HD�AHDƁHD��HD�HD�AHDǁHD��HD�HD�AHDȁHD��HD�HD�AHDɁHD��HD�HD�AHDʁHD��HD�HD�AHDˁHD��HD�HD�AHD́HD��HD�HD�AHD́HD��HD�HD�AHD΁HD��HD�HD�AHDρHD��HD�HD�AHDЁHD��HD�HD�AHDсHD��HD�HD�AHDҁHD��HD�HD�AHDӁHD��HD�HD�AHDԁHD��HD�HD�AHDՁHD��HD�HD�AHDցHD��HD�HD�AHDׁHD��HD�HD�AHD؁HD��HD�HD�AHDفHD��HD�HD�AHDځHD��HD�HD�AHDہHD��HD�HD�AHD܁HD��HD�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�~D��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD�t{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A���A��8A���A��A��`A���A���A��A☓A�e�A�I�A�?�A�5�A�1�A�'A� \A�\A�
�A� �A��BAᧇA�u�A�`A�!�A��A��A���A�~�A�a|A�AUA��A��TA߫6A�{A�J#A��8A޴nA��A���Aݤ@A�c�A�+kA�A�ԕA���Aܳ�A�d&A�h>A��vA�k�A��AA��A�|�A���AȞ�A�k�A��A��A�_A��}A�-CA�R A�t�A�4�A�OA��A��XA|��Az9�AuںAs�nAp)_Am��Ak��Ah��Ae!�Ad�Ad�nAc��Ac(�Ab�Aa��A`Z�A_�	A[[�AY�AXh
AW�#AV�AO��AI�YAD�AC8�AAcA>($A<��A<%FA9�)A8��A79�A6?�A5��A5(�A3��A3$A2rGA0c�A-xlA-=qA+}�A*�A*r�A*GA(��A&�A%OA#�0A"�aA"�nA"C�A!��A!E�A �tA IRA �A�AA �6A"�A"�yA#-wA#FtA#;�A$~(A% iA$1�A"�A!��A!�A ��A�jA�A��AFA;A�,A�A�WA��A�8A-A�A��A4A� A�gA�rA]�AB�A_�A|A%A��A)_AXyAjA�ZAf�A��A�AĜArGA�A�AF�A�A�A!-AA AffAY�A!A�A�PA
�A
��A	�A	y�A	TaA	2�A	�A��A��A�kA��AS�A��A�A-A�eA>�A �A��AOvA�A�`A��AkQAN<A{�A�	AC�A ��@�Mj@���@��	@��H@�s�@��@���@���@��@�:�@��@�l"@��A@�@��V@���@��E@�1�@��+@�
=@��@�C@�PH@�f@��@�x@�?�@��@�@�V@�@�d�@��@���@��;@�rG@��5@�@�Xy@�+k@��@맇@�5�@�h@�)�@�@�@O@�z�@�$@�@@��'@�YK@��m@��|@�_�@�s@��@�&�@�	@� \@ව@�d�@��@ߛ=@�Z�@� i@� �@ݼ@��@�Q@�u@��}@�$t@��X@ڥz@�?@ٳ�@�J#@�{�@��@�GE@Ւ:@�?}@��@Բ�@��m@ӥ�@�.I@�c�@�b�@���@��p@з�@�z@�S�@Ͼw@ϥ�@�Y@�_�@��Z@��@ͶF@͟V@��@�C�@��)@˵t@�H�@��@��,@ʝI@�g8@�$�@�e@��D@�ی@�u�@�^5@���@��|@�{�@�;�@ţn@��|@�J�@��N@â�@�hs@��@�@��o@���@�b�@��@���@�`�@�x@���@���@��@��@�f�@��E@�c @��@� �@��@�~�@���@�p;@��@��@�U�@�!�@��L@�5?@���@�O�@���@���@�&�@���@��m@�c�@��@���@��X@�E9@��@�oi@�
�@�zx@��@��x@�4n@���@��{@�\)@�(�@��]@���@���@�n�@�x@���@���@�]�@�A @�8@���@��A@�"h@���@���@��n@���@�˒@�o @�!�@��v@���@�kQ@���@�y�@��@���@�R�@�.�@�!@��@�b@�  @�7@��+@���@��3@��0@�|�@�@��@�^5@�
�@���@�rG@�&�@��@�M@�M�@�_@���@���@�-w@���@�:*@�;�@�<�@�'R@���@��m@���@��@���@�j@�)_@���@���@���@��1@�~@��@�X@�C@�ی@��I@���@��A@�Q@�1�@�!@�@�	@��.@�@��	@�O�@�V@��M@���@�`�@�1'@�~@���@��H@�|�@�&�@�Ĝ@��x@�q@��@�@��@���@��^@���@�]�@�&�@���@�tT@�9X@��r@��=@�RT@�S@��@�3�@���@�s@�\)@�S&@�Vm@�@O@�$t@���@���@�\�@�D�@���@�8�@��"@���@���@���@�w�@�;�@���@�zx@��4@�4@� \@��@��@��@���@�&�@���@�e,@�F@�&@�@��H@��y@��,@��o@�GE@���@��H@�5�@�҉@��$@���@���@��b@��@�h
@�H@��@���@�=@��@��@�y>@�W�@�1'@���@���@���@�@O@��b@�YK@�9X@�ݘ@��@���@�dZ@�+�@�ȴ@�h
@�B[@�!�@��]@��+@��@��a@��k@��@�֡@�Ĝ@�q�@�J@��D@��K@���@�O@� \@���@���@��+@�`�@� �@���@�Mj@�.I@�V@���@��j@���@�g8@�~@�f@�@~�@~($@}}�@}/@|?�@{��@{8@z�h@z:*@y�z@y%F@x��@x�$@xS�@w��@w{J@w i@v��@v�F@v�A@vR�@v�@u��@t֡@t�@s��@sW?@s/�@r�"@r��@r�\@q��@q+�@p��@pC-@p/�@p?�@o��@o��@o�@o;d@n͟@m�@m&�@l�U@l��@lQ�@lz�@k��@k=@j�]@j��@jh
@j�@i��@is�@i#�@h��@h��@hI�@g��@gX�@g i@f�,@f��@e�T@e�@d`�@d$@cخ@c�:@ca@c)_@b�s@b��@bp;@b_�@bR�@b4@a�@a��@a�"@a!�@`�@`y>@`%�@_�}@_�	@_J#@^��@]��@]��@]0�@\��@\��@\e�@\�@[�F@[Mj@[!-@Z��@Z�H@Z��@Zd�@Y��@Yo @X��@X]d@Xb@W��@W��@W��@W�@V�@V8�@VO@U��@U^�@UL�@T��@T1@SiD@R��@Q�z@Qo @P�5@Py>@P1'@O�@O��@OF�@N��@NTa@M�@Mc@L�P@LtT@K��@K4�@J��@J��@J^5@I�)@I��@IN<@H�K@HbN@H�@G�@F��@Fq�@E�@Ek�@E`B@E0�@D�p@D�@Dy>@DM@C��@CC@C�@B�<@B�+@BTa@B-@A�@Ao @@�K@@֡@@�9@@�Y@@�@?�{@?o@>ں@>�<@>�@=��@=`B@=F@=A @<�P@<q@<h�@<S�@</�@;��@;t�@;o@:��@9�D@9�@9O�@8�@8u�@8G@7˒@7��@7�@7��@7��@733@6R�@5��@5�M@5[W@5Vm@5T�@5J�@58�@4��@4�.@4�o@41'@3e�@3@2�8@2�@2�@2��@2�@2c @1��@1�n@15�@1�@0��@0��@0��@0ѷ@0m�@/�@/��@/{J@/'�@.�@.�y@.ȴ@.�1@.l�@.@�@.!�@-��@-�z@-��@-�@,�	@,��@,Z@,1'@+�@+j�@+\)@*�@*��@*�+@*z@*�@)��@)@)�n@)a�@)%F@(֡@(>B@'�6@'�q@'�$@'�4@'/�@&�c@&�,@&��@%��@%+�@%%F@$��@$�I@$�u@$�u@$�D@$U2@$@#�}@#�4@#j�@#_p@#W?@#H�@#=@#,�@#$t@#
=@"�X@"h
@!�9@!��@!x�@!k�@!?}@!;@ �)@ |�@ 6@خ@�f@/�@��@�@��@��@3�@��@�@f�@�f@�?@�@�@Ft@�A@�F@��@qv@U�@6z@�M@YK@)�@�#@s�@L�@<6@0�@�@��@��@[�@��@�K@��@�q@��@U�@@O@C@��@z@;�@�@�H@��@}�@L�@Dg@@@�5@�?@�@S�@/�@�m@˒@{J@qv@8@�@�X@�b@��@kQ@YK@�@�3@�S@o @A @+�@�@��@r�@S�@6@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A���A��8A���A��A��`A���A���A��A☓A�e�A�I�A�?�A�5�A�1�A�'A� \A�\A�
�A� �A��BAᧇA�u�A�`A�!�A��A��A���A�~�A�a|A�AUA��A��TA߫6A�{A�J#A��8A޴nA��A���Aݤ@A�c�A�+kA�A�ԕA���Aܳ�A�d&A�h>A��vA�k�A��AA��A�|�A���AȞ�A�k�A��A��A�_A��}A�-CA�R A�t�A�4�A�OA��A��XA|��Az9�AuںAs�nAp)_Am��Ak��Ah��Ae!�Ad�Ad�nAc��Ac(�Ab�Aa��A`Z�A_�	A[[�AY�AXh
AW�#AV�AO��AI�YAD�AC8�AAcA>($A<��A<%FA9�)A8��A79�A6?�A5��A5(�A3��A3$A2rGA0c�A-xlA-=qA+}�A*�A*r�A*GA(��A&�A%OA#�0A"�aA"�nA"C�A!��A!E�A �tA IRA �A�AA �6A"�A"�yA#-wA#FtA#;�A$~(A% iA$1�A"�A!��A!�A ��A�jA�A��AFA;A�,A�A�WA��A�8A-A�A��A4A� A�gA�rA]�AB�A_�A|A%A��A)_AXyAjA�ZAf�A��A�AĜArGA�A�AF�A�A�A!-AA AffAY�A!A�A�PA
�A
��A	�A	y�A	TaA	2�A	�A��A��A�kA��AS�A��A�A-A�eA>�A �A��AOvA�A�`A��AkQAN<A{�A�	AC�A ��@�Mj@���@��	@��H@�s�@��@���@���@��@�:�@��@�l"@��A@�@��V@���@��E@�1�@��+@�
=@��@�C@�PH@�f@��@�x@�?�@��@�@�V@�@�d�@��@���@��;@�rG@��5@�@�Xy@�+k@��@맇@�5�@�h@�)�@�@�@O@�z�@�$@�@@��'@�YK@��m@��|@�_�@�s@��@�&�@�	@� \@ව@�d�@��@ߛ=@�Z�@� i@� �@ݼ@��@�Q@�u@��}@�$t@��X@ڥz@�?@ٳ�@�J#@�{�@��@�GE@Ւ:@�?}@��@Բ�@��m@ӥ�@�.I@�c�@�b�@���@��p@з�@�z@�S�@Ͼw@ϥ�@�Y@�_�@��Z@��@ͶF@͟V@��@�C�@��)@˵t@�H�@��@��,@ʝI@�g8@�$�@�e@��D@�ی@�u�@�^5@���@��|@�{�@�;�@ţn@��|@�J�@��N@â�@�hs@��@�@��o@���@�b�@��@���@�`�@�x@���@���@��@��@�f�@��E@�c @��@� �@��@�~�@���@�p;@��@��@�U�@�!�@��L@�5?@���@�O�@���@���@�&�@���@��m@�c�@��@���@��X@�E9@��@�oi@�
�@�zx@��@��x@�4n@���@��{@�\)@�(�@��]@���@���@�n�@�x@���@���@�]�@�A @�8@���@��A@�"h@���@���@��n@���@�˒@�o @�!�@��v@���@�kQ@���@�y�@��@���@�R�@�.�@�!@��@�b@�  @�7@��+@���@��3@��0@�|�@�@��@�^5@�
�@���@�rG@�&�@��@�M@�M�@�_@���@���@�-w@���@�:*@�;�@�<�@�'R@���@��m@���@��@���@�j@�)_@���@���@���@��1@�~@��@�X@�C@�ی@��I@���@��A@�Q@�1�@�!@�@�	@��.@�@��	@�O�@�V@��M@���@�`�@�1'@�~@���@��H@�|�@�&�@�Ĝ@��x@�q@��@�@��@���@��^@���@�]�@�&�@���@�tT@�9X@��r@��=@�RT@�S@��@�3�@���@�s@�\)@�S&@�Vm@�@O@�$t@���@���@�\�@�D�@���@�8�@��"@���@���@���@�w�@�;�@���@�zx@��4@�4@� \@��@��@��@���@�&�@���@�e,@�F@�&@�@��H@��y@��,@��o@�GE@���@��H@�5�@�҉@��$@���@���@��b@��@�h
@�H@��@���@�=@��@��@�y>@�W�@�1'@���@���@���@�@O@��b@�YK@�9X@�ݘ@��@���@�dZ@�+�@�ȴ@�h
@�B[@�!�@��]@��+@��@��a@��k@��@�֡@�Ĝ@�q�@�J@��D@��K@���@�O@� \@���@���@��+@�`�@� �@���@�Mj@�.I@�V@���@��j@���@�g8@�~@�f@�@~�@~($@}}�@}/@|?�@{��@{8@z�h@z:*@y�z@y%F@x��@x�$@xS�@w��@w{J@w i@v��@v�F@v�A@vR�@v�@u��@t֡@t�@s��@sW?@s/�@r�"@r��@r�\@q��@q+�@p��@pC-@p/�@p?�@o��@o��@o�@o;d@n͟@m�@m&�@l�U@l��@lQ�@lz�@k��@k=@j�]@j��@jh
@j�@i��@is�@i#�@h��@h��@hI�@g��@gX�@g i@f�,@f��@e�T@e�@d`�@d$@cخ@c�:@ca@c)_@b�s@b��@bp;@b_�@bR�@b4@a�@a��@a�"@a!�@`�@`y>@`%�@_�}@_�	@_J#@^��@]��@]��@]0�@\��@\��@\e�@\�@[�F@[Mj@[!-@Z��@Z�H@Z��@Zd�@Y��@Yo @X��@X]d@Xb@W��@W��@W��@W�@V�@V8�@VO@U��@U^�@UL�@T��@T1@SiD@R��@Q�z@Qo @P�5@Py>@P1'@O�@O��@OF�@N��@NTa@M�@Mc@L�P@LtT@K��@K4�@J��@J��@J^5@I�)@I��@IN<@H�K@HbN@H�@G�@F��@Fq�@E�@Ek�@E`B@E0�@D�p@D�@Dy>@DM@C��@CC@C�@B�<@B�+@BTa@B-@A�@Ao @@�K@@֡@@�9@@�Y@@�@?�{@?o@>ں@>�<@>�@=��@=`B@=F@=A @<�P@<q@<h�@<S�@</�@;��@;t�@;o@:��@9�D@9�@9O�@8�@8u�@8G@7˒@7��@7�@7��@7��@733@6R�@5��@5�M@5[W@5Vm@5T�@5J�@58�@4��@4�.@4�o@41'@3e�@3@2�8@2�@2�@2��@2�@2c @1��@1�n@15�@1�@0��@0��@0��@0ѷ@0m�@/�@/��@/{J@/'�@.�@.�y@.ȴ@.�1@.l�@.@�@.!�@-��@-�z@-��@-�@,�	@,��@,Z@,1'@+�@+j�@+\)@*�@*��@*�+@*z@*�@)��@)@)�n@)a�@)%F@(֡@(>B@'�6@'�q@'�$@'�4@'/�@&�c@&�,@&��@%��@%+�@%%F@$��@$�I@$�u@$�u@$�D@$U2@$@#�}@#�4@#j�@#_p@#W?@#H�@#=@#,�@#$t@#
=@"�X@"h
@!�9@!��@!x�@!k�@!?}@!;@ �)@ |�@ 6@خ@�f@/�@��@�@��@��@3�@��@�@f�@�f@�?@�@�@Ft@�A@�F@��@qv@U�@6z@�M@YK@)�@�#@s�@L�@<6@0�@�@��@��@[�@��@�K@��@�q@��@U�@@O@C@��@z@;�@�@�H@��@}�@L�@Dg@@@�5@�?@�@S�@/�@�m@˒@{J@qv@8@�@�X@�b@��@kQ@YK@�@�3@�S@o @A @+�@�@��@r�@S�@6@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	LB	K�B	LJB	K�B	K^B	K�B	K^B	JrB	G+B	C�B	?�B	>�B	A�B	IB	L�B	T{B	^B	pUB	x�B	yXB	y�B	z�B	yXB	x�B	u�B	�B	��B	�'B	��B	�B	�3B	�B	�_B	��B	��B	�EB	}�B	v�B	h�B	d�B	_B	Z�B	\�B	`\B	X�B	]~B	X�B	MB	T�B	d�B	7�B	G�B	Q�B	|B	`vB	a�B	j�B	d@B	*�B	�B	}B	yB	�B	%zB	EB	G_B	UB	ZB	]B	b4B	v`B	oOB	b4B	^B	_pB	j�B	R B	O(B	MPB	H�B	CB	A B	=<B	33B	,�B	"�B	�B	�B	bB	3B�#B�B��B��B��B��B�B��B�>B�B��B��B�ZB��B�2B��B��B��B��B��B��B�mB��B��B�EB�BӏB�7BߊB�yB��B��B�B	
�B	$B	OB	3�B	PB	p�B	��B	�6B	��B	�;B	�B	��B	ЗB	ĶB	�B	�PB	�.B	��B	��B	�B	��B	�_B	�B	�[B	��B	�$B	�B	�XB	�B	ԯB	�yB	�dB	�WB	�B	�}B	ңB	�B	��B	�B	�B	ߤB	��B	��B	�B	��B	��B	�B	�HB	��B	��B	�BB	�OB	�[B	�MB	�B	�PB	�FB	�B	ٴB	ٚB	�_B	�
B	�MB	՛B	��B	�?B	��B	�EB	��B	یB	��B	ݲB	�B	��B	��B	�]B	�B	��B	�B	��B	�WB	ܬB	�/B	�IB	��B	�hB	�B	��B	�xB	�B	�7B	�B	��B	�SB	�MB	�aB	�FB	��B	ԯB	�B	��B	�IB	ٚB	ևB	�dB	�B	�OB	�/B	�B	�zB	��B	ߤB	�B	�'B	�BB	�-B	��B	�-B	��B	��B	�,B	�B	��B	�B	�B	��B	�*B	�XB	��B	�B	�B	�>B	�XB	�>B	�XB	�
B	�B	�$B	�mB	�B	�B	��B	�B	�XB	�B	�mB	�B	�B	�fB	�LB	��B	��B	��B	�0B	�B	�B	��B	�B	�}B	�B	�B	�B	��B	�B	�[B	�vB	��B	�oB	�B	�iB	�]B	�=B	�"B	�B	�B	�B	�B	��B	�6B	�_B	�>B	��B	�B	��B	�DB	��B	�*B	�>B	�RB	��B	��B	��B	�KB	�B	�WB	�qB	�B	�wB	�]B	��B	��B	�B	�"B	�B	�kB	�B	�B	�6B	�"B	�B	��B	�B	��B	�B	��B	�=B	�qB	��B	��B	�B	�B	�B	��B	�B	�B	�IB	� B	�OB	�OB	��B	��B	��B	��B	�aB	�B	��B	��B	�B	�B	�nB	�B	�B	�B	�2B	��B	�B	��B	�	B	�>B	��B	�xB	��B	��B	��B	�]B	��B	�}B
 4B
oB
�B
B
�B
�B
�B
GB
GB
-B
{B
�B
�B
3B
�B
9B
mB
9B
�B
�B
�B
�B
tB
mB
�B
�B
9B
�B
�B
�B
B
�B
zB
�B
1B
	B

	B

	B
	B
�B
fB
�B
�B
	RB
B
~B
B
�B
~B
JB
0B
�B
�B
DB

�B

�B

rB
	�B
	�B
	RB
	�B

�B
DB
)B

rB
0B
�B
~B
dB
�B
�B
�B
6B
PB
�B
VB
�B
HB
HB
.B
.B
hB
�B
�B
�B
,B
�B
�B
B
B
B
�B
�B
�B

B
$B
�B
B
�B
�B
�B
�B
�B
�B
eB
B
B
�B
B
QB
�B
=B
WB
WB
�B
�B
�B
CB
�B
�B
�B
�B
�B
�B
�B
IB
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
;B
pB
VB
�B
 'B
 \B
 �B
 �B
 �B
 �B
 �B
!�B
# B
$@B
$�B
%,B
&LB
&�B
&�B
'RB
(
B
'mB
'RB
'B
&�B
'B
'RB
'�B
(�B
(�B
($B
'mB
'mB
'mB
'RB
'�B
($B
(�B
)DB
)�B
*B
*0B
+B
+�B
,B
,"B
,�B
,�B
-B
-B
-]B
-�B
-wB
-�B
.�B
.�B
.�B
/�B
/iB
/5B
/�B
/�B
0!B
0!B
0;B
0;B
0�B
0UB
0UB
0UB
0;B
/�B
/iB
0�B
0;B
/�B
/�B
0oB
0�B
1B
0�B
2B
2B
2�B
3MB
3�B
3�B
2�B
3hB
3�B
4TB
49B
3�B
4�B
5%B
5B
5?B
5tB
5tB
5ZB
6zB
6�B
7fB
8B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8lB
8�B
9$B
9>B
9>B
9>B
9	B
9�B
9>B
9$B
9	B
9$B
9XB
9XB
9�B
9�B
9�B
9�B
:�B
;0B
;�B
<B
<B
<jB
<�B
<�B
=VB
="B
=VB
=�B
=�B
>wB
>�B
>�B
?�B
@ B
@4B
@ B
@ B
@iB
@�B
A;B
A;B
A�B
A�B
B'B
B[B
BAB
BuB
B�B
C{B
C�B
C{B
C�B
C�B
C�B
DMB
D�B
D�B
D�B
D�B
D�B
EB
ESB
E9B
ESB
E�B
E�B
E�B
F%B
F?B
F�B
FYB
F�B
GzB
G�B
G�B
G�B
G�B
H�B
H�B
H�B
IB
IB
IB
IB
IB
IRB
I�B
I�B
IlB
I7B
IlB
I�B
I�B
I�B
J	B
K�B
K�B
K�B
L0B
LdB
L0B
LdB
L�B
LJB
L�B
L0B
L�B
MjB
N"B
NVB
N<B
NpB
O\B
O�B
PbB
Q B
Q B
Q4B
QNB
Q�B
RoB
RTB
R:B
RB
RB
R:B
R�B
R�B
SB
SB
SuB
S�B
S�B
T{B
T�B
UB
UgB
U�B
U�B
VB
VSB
V�B
W?B
WYB
WsB
W�B
W�B
W�B
W�B
XEB
XyB
XEB
X_B
XyB
YB
YB
Y�B
Y�B
Y�B
ZQB
ZQB
Z�B
Z�B
Z�B
Z�B
[#B
[#B
[qB
[qB
\)B
\)B
\]B
\�B
\�B
\�B
]/B
]�B
]�B
^jB
^�B
^�B
^jB
^�B
^�B
^�B
^�B
^5B
^�B
^�B
^�B
^�B
^�B
^�B
_;B
^�B
^�B
^�B
_;B
_�B
_pB
_�B
_pB
_pB
_pB
_�B
_�B
`B
`�B
`�B
`�B
`�B
`�B
aB
abB
a�B
a�B
a�B
b4B
b4B
bNB
bhB
b�B
b�B
b�B
cB
c:B
c:B
c:B
c B
c�B
c�B
c�B
c�B
d&B
d�B
d�B
eFB
ezB
ezB
ezB
f2B
fLB
ffB
ffB
f�B
f�B
gB
g�B
h$B
h$B
h>B
h>B
h�B
h�B
h�B
iB
i�B
jKB
j0B
j�B
j�B
j�B
j�B
j�B
j�B
kB
kQB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l"B
l�B
mB
mB
m)B
m)B
mCB
m]B
m�B
m�B
m�B
ncB
n�B
o B
oB
o5B
oB
oB
o�B
o�B
o�B
poB
p�B
p�B
p�B
p�B
qAB
q[B
q�B
q�B
q�B
q�B
q�B
r-B
r�B
r�B
r�B
shB
s�B
s�B
s�B
s�B
s�B
s�B
tnB
t�B
t�B
t�B
t�B
t�B
u%B
u?B
utB
u�B
u�B
vFB
v+B
v`B
vzB
v�B
wB
v�B
w�B
wLB
w�B
w�B
w�B
w�B
xB
w�B
x�B
xlB
x�B
y$B
y$B
y	B
y$B
y�B
x�B
y�B
zB
y�B
zDB
zxB
z�B
y�B
z�B
z�B
z�B
{B
{011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	LB	K�B	LJB	K�B	K^B	K�B	K^B	JrB	G+B	C�B	?�B	>�B	A�B	IB	L�B	T{B	^B	pUB	x�B	yXB	y�B	z�B	yXB	x�B	u�B	�B	��B	�'B	��B	�B	�3B	�B	�_B	��B	��B	�EB	}�B	v�B	h�B	d�B	_B	Z�B	\�B	`\B	X�B	]~B	X�B	MB	T�B	d�B	7�B	G�B	Q�B	|B	`vB	a�B	j�B	d@B	*�B	�B	}B	yB	�B	%zB	EB	G_B	UB	ZB	]B	b4B	v`B	oOB	b4B	^B	_pB	j�B	R B	O(B	MPB	H�B	CB	A B	=<B	33B	,�B	"�B	�B	�B	bB	3B�#B�B��B��B��B��B�B��B�>B�B��B��B�ZB��B�2B��B��B��B��B��B��B�mB��B��B�EB�BӏB�7BߊB�yB��B��B�B	
�B	$B	OB	3�B	PB	p�B	��B	�6B	��B	�;B	�B	��B	ЗB	ĶB	�B	�PB	�.B	��B	��B	�B	��B	�_B	�B	�[B	��B	�$B	�B	�XB	�B	ԯB	�yB	�dB	�WB	�B	�}B	ңB	�B	��B	�B	�B	ߤB	��B	��B	�B	��B	��B	�B	�HB	��B	��B	�BB	�OB	�[B	�MB	�B	�PB	�FB	�B	ٴB	ٚB	�_B	�
B	�MB	՛B	��B	�?B	��B	�EB	��B	یB	��B	ݲB	�B	��B	��B	�]B	�B	��B	�B	��B	�WB	ܬB	�/B	�IB	��B	�hB	�B	��B	�xB	�B	�7B	�B	��B	�SB	�MB	�aB	�FB	��B	ԯB	�B	��B	�IB	ٚB	ևB	�dB	�B	�OB	�/B	�B	�zB	��B	ߤB	�B	�'B	�BB	�-B	��B	�-B	��B	��B	�,B	�B	��B	�B	�B	��B	�*B	�XB	��B	�B	�B	�>B	�XB	�>B	�XB	�
B	�B	�$B	�mB	�B	�B	��B	�B	�XB	�B	�mB	�B	�B	�fB	�LB	��B	��B	��B	�0B	�B	�B	��B	�B	�}B	�B	�B	�B	��B	�B	�[B	�vB	��B	�oB	�B	�iB	�]B	�=B	�"B	�B	�B	�B	�B	��B	�6B	�_B	�>B	��B	�B	��B	�DB	��B	�*B	�>B	�RB	��B	��B	��B	�KB	�B	�WB	�qB	�B	�wB	�]B	��B	��B	�B	�"B	�B	�kB	�B	�B	�6B	�"B	�B	��B	�B	��B	�B	��B	�=B	�qB	��B	��B	�B	�B	�B	��B	�B	�B	�IB	� B	�OB	�OB	��B	��B	��B	��B	�aB	�B	��B	��B	�B	�B	�nB	�B	�B	�B	�2B	��B	�B	��B	�	B	�>B	��B	�xB	��B	��B	��B	�]B	��B	�}B
 4B
oB
�B
B
�B
�B
�B
GB
GB
-B
{B
�B
�B
3B
�B
9B
mB
9B
�B
�B
�B
�B
tB
mB
�B
�B
9B
�B
�B
�B
B
�B
zB
�B
1B
	B

	B

	B
	B
�B
fB
�B
�B
	RB
B
~B
B
�B
~B
JB
0B
�B
�B
DB

�B

�B

rB
	�B
	�B
	RB
	�B

�B
DB
)B

rB
0B
�B
~B
dB
�B
�B
�B
6B
PB
�B
VB
�B
HB
HB
.B
.B
hB
�B
�B
�B
,B
�B
�B
B
B
B
�B
�B
�B

B
$B
�B
B
�B
�B
�B
�B
�B
�B
eB
B
B
�B
B
QB
�B
=B
WB
WB
�B
�B
�B
CB
�B
�B
�B
�B
�B
�B
�B
IB
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
;B
pB
VB
�B
 'B
 \B
 �B
 �B
 �B
 �B
 �B
!�B
# B
$@B
$�B
%,B
&LB
&�B
&�B
'RB
(
B
'mB
'RB
'B
&�B
'B
'RB
'�B
(�B
(�B
($B
'mB
'mB
'mB
'RB
'�B
($B
(�B
)DB
)�B
*B
*0B
+B
+�B
,B
,"B
,�B
,�B
-B
-B
-]B
-�B
-wB
-�B
.�B
.�B
.�B
/�B
/iB
/5B
/�B
/�B
0!B
0!B
0;B
0;B
0�B
0UB
0UB
0UB
0;B
/�B
/iB
0�B
0;B
/�B
/�B
0oB
0�B
1B
0�B
2B
2B
2�B
3MB
3�B
3�B
2�B
3hB
3�B
4TB
49B
3�B
4�B
5%B
5B
5?B
5tB
5tB
5ZB
6zB
6�B
7fB
8B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8lB
8�B
9$B
9>B
9>B
9>B
9	B
9�B
9>B
9$B
9	B
9$B
9XB
9XB
9�B
9�B
9�B
9�B
:�B
;0B
;�B
<B
<B
<jB
<�B
<�B
=VB
="B
=VB
=�B
=�B
>wB
>�B
>�B
?�B
@ B
@4B
@ B
@ B
@iB
@�B
A;B
A;B
A�B
A�B
B'B
B[B
BAB
BuB
B�B
C{B
C�B
C{B
C�B
C�B
C�B
DMB
D�B
D�B
D�B
D�B
D�B
EB
ESB
E9B
ESB
E�B
E�B
E�B
F%B
F?B
F�B
FYB
F�B
GzB
G�B
G�B
G�B
G�B
H�B
H�B
H�B
IB
IB
IB
IB
IB
IRB
I�B
I�B
IlB
I7B
IlB
I�B
I�B
I�B
J	B
K�B
K�B
K�B
L0B
LdB
L0B
LdB
L�B
LJB
L�B
L0B
L�B
MjB
N"B
NVB
N<B
NpB
O\B
O�B
PbB
Q B
Q B
Q4B
QNB
Q�B
RoB
RTB
R:B
RB
RB
R:B
R�B
R�B
SB
SB
SuB
S�B
S�B
T{B
T�B
UB
UgB
U�B
U�B
VB
VSB
V�B
W?B
WYB
WsB
W�B
W�B
W�B
W�B
XEB
XyB
XEB
X_B
XyB
YB
YB
Y�B
Y�B
Y�B
ZQB
ZQB
Z�B
Z�B
Z�B
Z�B
[#B
[#B
[qB
[qB
\)B
\)B
\]B
\�B
\�B
\�B
]/B
]�B
]�B
^jB
^�B
^�B
^jB
^�B
^�B
^�B
^�B
^5B
^�B
^�B
^�B
^�B
^�B
^�B
_;B
^�B
^�B
^�B
_;B
_�B
_pB
_�B
_pB
_pB
_pB
_�B
_�B
`B
`�B
`�B
`�B
`�B
`�B
aB
abB
a�B
a�B
a�B
b4B
b4B
bNB
bhB
b�B
b�B
b�B
cB
c:B
c:B
c:B
c B
c�B
c�B
c�B
c�B
d&B
d�B
d�B
eFB
ezB
ezB
ezB
f2B
fLB
ffB
ffB
f�B
f�B
gB
g�B
h$B
h$B
h>B
h>B
h�B
h�B
h�B
iB
i�B
jKB
j0B
j�B
j�B
j�B
j�B
j�B
j�B
kB
kQB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l"B
l�B
mB
mB
m)B
m)B
mCB
m]B
m�B
m�B
m�B
ncB
n�B
o B
oB
o5B
oB
oB
o�B
o�B
o�B
poB
p�B
p�B
p�B
p�B
qAB
q[B
q�B
q�B
q�B
q�B
q�B
r-B
r�B
r�B
r�B
shB
s�B
s�B
s�B
s�B
s�B
s�B
tnB
t�B
t�B
t�B
t�B
t�B
u%B
u?B
utB
u�B
u�B
vFB
v+B
v`B
vzB
v�B
wB
v�B
w�B
wLB
w�B
w�B
w�B
w�B
xB
w�B
x�B
xlB
x�B
y$B
y$B
y	B
y$B
y�B
x�B
y�B
zB
y�B
zDB
zxB
z�B
y�B
z�B
z�B
z�B
{B
{011111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104851  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172621  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172621  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172621                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022628  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022628  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131506                      G�O�G�O�G�O�                