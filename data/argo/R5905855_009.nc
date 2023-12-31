CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:12:11Z creation;2022-06-04T19:12:12Z conversion to V3.1      
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
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M8   PRES_ADJUSTED_QC         
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
_FillValue                 �  �l   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �X   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �x   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �|   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220604191211  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               	A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @ؼ _�Q�1   @ؼ �3�@0e�Q��d����1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bi��Bo��Bx  B��B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���C   C  C  C  C  C
  C  C  C  C  C  C  C33C�fC  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR�CT  CU�fCX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!fD!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/y�D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQfDQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D��3D�3D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ D�|�D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�&f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @(�@�G�@�G�A ��A ��A@��Ab=qA�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B (�B(�B(�B(�B (�B((�B0(�B8(�B@(�BH(�BP(�BX(�B`(�BiBoBx(�BB��HB�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�z�B��HB�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B��B��HC 
=C
=C
=C
=C
=C

=C
=C
=C
=C
=C
=C
=C=pC�C
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
=C4
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
=CR#�CT
=CU�CX
=CZ
=C\
=C^
=C`
=Cb
=Cd
=Cf
=Ch#�Cj
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
=C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/|)D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�HD�AHD��HD��{D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD�~D��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHDHD��HD�HD�AHDÁHD��{D�{D�AHDāHD��HD�HD�AHDŁHD��HD�HD�AHDƁHD��HD�HD�AHDǁHD��HD�HD�AHDȁHD��HD�HD�AHDɁHD��HD�HD�AHDʁHD��HD�HD�AHDˁHD��HD�HD�AHD́HD��HD�HD�AHD́HD��HD�HD�AHD΁HD��HD�HD�AHDρHD��HD�HD�AHDЁHD��HD�HD�AHDсHD��HD�HD�AHDҁHD��HD�HD�AHDӁHD��HD�HD�AHDԁHD��HD�HD�AHDՁHD��HD�HD�AHDցHD��HD�HD�AHDׁHD��HD�HD�AHD؁HD��HD�HD�AHDفHD��HD�HD�AHDځHD��HD�HD�AHDہHD��HD�HD�AHD܁HD��HD�HD�AHD݁HD��HD�HD�AHD�~D��HD�HD�AHD߁HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�'�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aѻ0A��AЭ�AІ�A�q�A�_�A�UgA�M�A�F?A�E�A�:*A�:^A�OA��&Aϰ�A�v�A�xA���AΦ�AΏ\A�~]A�RTA��A͘�A�'�A��	A�CA�
�A��
A�j�ǍĀ�A˷LA�HKA�
�Aɵ�A�X�A��XAǿHAǚAǕ�A�6�AƳ�AƂuA�C�A�A���Aġ�AA���A��A��YA�bA���A�uA���A��-A��A��5A��`A��A��EA���A���A� A��A���A�m)A���A���A�%�A���A�,A���A�`vA��A��VA��#A��:A���A���A��+A�o5A�@A��	A��A��IA�-CA���A� �A���A�.IA���A��A���A�'A��A�rGA�p;A��AA��A�J�A�Q�A�IA��A��A�,qA�՛A�ߤA���A}��Ax^5Au��As1�An�Ak��Afp�A]��AY�$AUy�AR \AK�2AH�)AF�AE��AD��AB]dAA�A?��A>��A>�A>�A;��A7g8A5>�A4xlA3g8A2�ZA2s�A1A/�)A-�YA,\)A*��A)Q�A(+kA'�KA'�A&��A&�A$/�A#҉A#�A"kQA"�A!�5A#'�A"^�A"A!��A! �A �sA m�A "�Ac A�`A�A1A,�A�!A0UA��AJ�A�A-�A��A��AOAg8A��A �A��AS&A��A�A^�A�pA+kA��A~(A��A%FA�A>�Av�At�A��A
�.A
=�A	>BA��A��A�CA&Am�A��A�A�hAVA�#A�bAJA�EA��A iA֡A|�A ��@��@���@��2@��1@�Xy@�1�@���@�@��@��@�V@�ԕ@�s�@��@�PH@���@�8�@�o @�@�.I@�ߤ@���@�{J@�Xy@��@�ff@�!�@���@@��@�2�@�hs@��@�I@��&@�c�@��@��@�Q�@�@�P@�/�@�V@仙@�a|@�H�@㔯@�=�@��[@�+@�y�@�ѷ@�\@�V@��
@�X@��|@�($@���@��@�@O@ް!@�U2@ݣn@�s�@�p�@�J#@�Mj@ڵ�@ي�@ع$@�~�@�
�@�b�@֏\@�'�@�z@�5?@�O@���@�ϫ@ӧ�@�y�@�S�@�֡@��g@�@П�@�1@Ϭq@��@�Ta@��@��N@͙�@��@�;�@˔�@��@ʈ�@ɿH@Ț@�	�@Ƿ@��@�[�@��@�hs@��]@�d�@�($@��@Ô�@��E@�_@��@��m@��@�o�@��@��@��@���@�a@�`B@��X@���@�~@�ϫ@���@�qv@�S&@�+@���@��9@��@�~(@�=q@���@���@���@�E9@��K@�i�@�@���@��M@���@��!@���@�Q@��@��q@���@��{@�qv@�e,@�Z�@�9�@��@�u�@��@�ݘ@��@���@�a|@��]@��P@�6z@��@�Ɇ@�kQ@���@�zx@�7L@��]@�ff@��H@�O@���@���@�M�@��@�4�@��"@��}@�Ov@�@��)@�e�@�%@���@�7@��D@���@���@�_p@�&�@��y@���@� �@���@�b�@���@��@���@��8@�6@��@�P�@�(�@��@�<�@��@���@��@�;d@��U@�`�@��@�ϫ@�˒@��3@�ƨ@���@�X�@�@���@�^5@��@��D@��#@��@��@���@��P@�p�@�RT@�1�@��@�U2@��.@���@��f@�S&@�=�@�.I@��@���@��@���@��O@�p;@�Ov@�($@��@��@�H�@���@�Ĝ@���@�_�@�H@�e@��d@��^@���@�J�@��@��.@�)�@���@��	@�/�@��'@��@�5?@���@���@�$t@��"@��@��r@�@��@��K@���@��@���@��L@�l�@���@��@���@��@�;d@�@�u%@���@���@�c�@�	l@���@��b@�,=@��@���@��@���@�!@��@�w2@�=�@��@��p@�~�@�C-@� �@�>B@�Z�@��f@��@��?@��F@�PH@�2�@�;�@��@���@�Vm@��@���@��1@�`�@�M@���@���@�O�@�Y@��|@��p@���@���@�u@��w@���@��V@��~@�y�@�e,@�\)@�P�@�G�@�!-@��@���@�U2@��@�	�@��@�@s@K�@K�@iD@iD@E9@~��@~Z�@~($@}��@}�z@}�^@}X@|�e@|6@|�@{��@{� @{��@{��@{v`@{�@z��@z{�@z1�@y��@yu�@y	l@x�j@xPH@w�w@wy�@w1�@v�@v��@u}�@u \@t�j@tb@s��@sn/@s'�@rں@ra|@r)�@q�@q[W@p�f@pV�@o��@odZ@o@n�!@n�@m�C@m-w@l�U@lm�@lH@k�@k��@k�+@k�0@kX�@j��@i��@h��@h��@gn/@g�@fkQ@e��@e�@d��@d��@dtT@c~�@c!-@a�@a�n@a�7@a��@a�@`��@`e�@`C-@` �@_�@_��@^�@]��@]�@\��@\e�@\�@[˒@[�	@[ i@Z� @Y�)@Y2a@Y;@X�D@X@W��@V�s@VB[@U��@UA @U�@T�?@Ty>@T�@S��@Sx@S(@R�<@Qo @P�o@PI�@P:�@P �@O�;@O�$@Oqv@O\)@O33@N�s@N�F@NZ�@N �@M�)@M��@M��@M��@Mx�@Me,@M#�@L��@Lѷ@L��@L@K�}@K�:@J��@JkQ@J$�@I��@Izx@H�|@H�.@H�@H`�@Gݘ@G�@G��@G]�@G"�@F��@F��@Fv�@FYK@FO@E�D@E�N@E��@E�@DĜ@D��@D�D@D��@D/�@C��@C�w@Ca@B��@B�A@BYK@B�@A|@A4@@�@@��@@<�@?��@?l�@?U�@?=@>�8@>�]@>҉@>�@>��@>!�@>
�@=�'@=\�@<�@<��@<oi@;�
@;X�@;.I@;�@:��@:��@:kQ@9�^@9[W@9*0@8��@8��@8m�@8bN@8V�@8N�@8M@8>B@8�@7��@7�:@7@O@6��@6h
@6�@5ϫ@5G�@5@5@@4�@4`�@4"h@3�@3�	@3K�@2��@2_�@2-@2u@1��@1��@1u�@1e,@1G�@1+�@1�@0�@0�e@0�@06@01@/y�@/X�@/;d@/"�@/�@.��@.�X@.�h@.�F@.��@.xl@._�@.{@-��@-�3@-�@-%F@-�@,��@,S�@,C-@,G@+��@+@O@+;d@+�@*�H@*?@*�@)�@)#�@(��@(�z@(b@'ƨ@'�@'A�@'�@&��@&��@&d�@&E�@&{@& �@%��@%�-@%p�@%8�@$��@$Ɇ@$��@$�.@$[�@$�@#�Q@#��@#�	@#o�@#Mj@#'�@"�y@"��@"��@"�\@"c @"-@"e@!�@!�C@!��@!zx@!=�@ �	@ �E@ ��@ ��@ y>@ �@خ@��@��@��@�*@y�@RT@6z@
=@��@�@��@��@�M@@��@�_@6@��@�}@�P@U�@,�@ i@�@�]@��@��@v�@E�@�@ �@�Z@�T@Vm@�@��@��@�I@g8@6@�@ݘ@��@�@s@U�@)_@ں@{�@3�@��@��@�=@m]@5�@ \@��@�u@�@�@oi@`�@V�@~@�@.I@Y@�@�@(@�"@��@Ta@.�@�@ �@��@�o@��@�H@�@?}@�@�@�I@A�@2�@ �@�w@��@�4@8@�@@��@��@�\@�+@xl@n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aѻ0A��AЭ�AІ�A�q�A�_�A�UgA�M�A�F?A�E�A�:*A�:^A�OA��&Aϰ�A�v�A�xA���AΦ�AΏ\A�~]A�RTA��A͘�A�'�A��	A�CA�
�A��
A�j�ǍĀ�A˷LA�HKA�
�Aɵ�A�X�A��XAǿHAǚAǕ�A�6�AƳ�AƂuA�C�A�A���Aġ�AA���A��A��YA�bA���A�uA���A��-A��A��5A��`A��A��EA���A���A� A��A���A�m)A���A���A�%�A���A�,A���A�`vA��A��VA��#A��:A���A���A��+A�o5A�@A��	A��A��IA�-CA���A� �A���A�.IA���A��A���A�'A��A�rGA�p;A��AA��A�J�A�Q�A�IA��A��A�,qA�՛A�ߤA���A}��Ax^5Au��As1�An�Ak��Afp�A]��AY�$AUy�AR \AK�2AH�)AF�AE��AD��AB]dAA�A?��A>��A>�A>�A;��A7g8A5>�A4xlA3g8A2�ZA2s�A1A/�)A-�YA,\)A*��A)Q�A(+kA'�KA'�A&��A&�A$/�A#҉A#�A"kQA"�A!�5A#'�A"^�A"A!��A! �A �sA m�A "�Ac A�`A�A1A,�A�!A0UA��AJ�A�A-�A��A��AOAg8A��A �A��AS&A��A�A^�A�pA+kA��A~(A��A%FA�A>�Av�At�A��A
�.A
=�A	>BA��A��A�CA&Am�A��A�A�hAVA�#A�bAJA�EA��A iA֡A|�A ��@��@���@��2@��1@�Xy@�1�@���@�@��@��@�V@�ԕ@�s�@��@�PH@���@�8�@�o @�@�.I@�ߤ@���@�{J@�Xy@��@�ff@�!�@���@@��@�2�@�hs@��@�I@��&@�c�@��@��@�Q�@�@�P@�/�@�V@仙@�a|@�H�@㔯@�=�@��[@�+@�y�@�ѷ@�\@�V@��
@�X@��|@�($@���@��@�@O@ް!@�U2@ݣn@�s�@�p�@�J#@�Mj@ڵ�@ي�@ع$@�~�@�
�@�b�@֏\@�'�@�z@�5?@�O@���@�ϫ@ӧ�@�y�@�S�@�֡@��g@�@П�@�1@Ϭq@��@�Ta@��@��N@͙�@��@�;�@˔�@��@ʈ�@ɿH@Ț@�	�@Ƿ@��@�[�@��@�hs@��]@�d�@�($@��@Ô�@��E@�_@��@��m@��@�o�@��@��@��@���@�a@�`B@��X@���@�~@�ϫ@���@�qv@�S&@�+@���@��9@��@�~(@�=q@���@���@���@�E9@��K@�i�@�@���@��M@���@��!@���@�Q@��@��q@���@��{@�qv@�e,@�Z�@�9�@��@�u�@��@�ݘ@��@���@�a|@��]@��P@�6z@��@�Ɇ@�kQ@���@�zx@�7L@��]@�ff@��H@�O@���@���@�M�@��@�4�@��"@��}@�Ov@�@��)@�e�@�%@���@�7@��D@���@���@�_p@�&�@��y@���@� �@���@�b�@���@��@���@��8@�6@��@�P�@�(�@��@�<�@��@���@��@�;d@��U@�`�@��@�ϫ@�˒@��3@�ƨ@���@�X�@�@���@�^5@��@��D@��#@��@��@���@��P@�p�@�RT@�1�@��@�U2@��.@���@��f@�S&@�=�@�.I@��@���@��@���@��O@�p;@�Ov@�($@��@��@�H�@���@�Ĝ@���@�_�@�H@�e@��d@��^@���@�J�@��@��.@�)�@���@��	@�/�@��'@��@�5?@���@���@�$t@��"@��@��r@�@��@��K@���@��@���@��L@�l�@���@��@���@��@�;d@�@�u%@���@���@�c�@�	l@���@��b@�,=@��@���@��@���@�!@��@�w2@�=�@��@��p@�~�@�C-@� �@�>B@�Z�@��f@��@��?@��F@�PH@�2�@�;�@��@���@�Vm@��@���@��1@�`�@�M@���@���@�O�@�Y@��|@��p@���@���@�u@��w@���@��V@��~@�y�@�e,@�\)@�P�@�G�@�!-@��@���@�U2@��@�	�@��@�@s@K�@K�@iD@iD@E9@~��@~Z�@~($@}��@}�z@}�^@}X@|�e@|6@|�@{��@{� @{��@{��@{v`@{�@z��@z{�@z1�@y��@yu�@y	l@x�j@xPH@w�w@wy�@w1�@v�@v��@u}�@u \@t�j@tb@s��@sn/@s'�@rں@ra|@r)�@q�@q[W@p�f@pV�@o��@odZ@o@n�!@n�@m�C@m-w@l�U@lm�@lH@k�@k��@k�+@k�0@kX�@j��@i��@h��@h��@gn/@g�@fkQ@e��@e�@d��@d��@dtT@c~�@c!-@a�@a�n@a�7@a��@a�@`��@`e�@`C-@` �@_�@_��@^�@]��@]�@\��@\e�@\�@[˒@[�	@[ i@Z� @Y�)@Y2a@Y;@X�D@X@W��@V�s@VB[@U��@UA @U�@T�?@Ty>@T�@S��@Sx@S(@R�<@Qo @P�o@PI�@P:�@P �@O�;@O�$@Oqv@O\)@O33@N�s@N�F@NZ�@N �@M�)@M��@M��@M��@Mx�@Me,@M#�@L��@Lѷ@L��@L@K�}@K�:@J��@JkQ@J$�@I��@Izx@H�|@H�.@H�@H`�@Gݘ@G�@G��@G]�@G"�@F��@F��@Fv�@FYK@FO@E�D@E�N@E��@E�@DĜ@D��@D�D@D��@D/�@C��@C�w@Ca@B��@B�A@BYK@B�@A|@A4@@�@@��@@<�@?��@?l�@?U�@?=@>�8@>�]@>҉@>�@>��@>!�@>
�@=�'@=\�@<�@<��@<oi@;�
@;X�@;.I@;�@:��@:��@:kQ@9�^@9[W@9*0@8��@8��@8m�@8bN@8V�@8N�@8M@8>B@8�@7��@7�:@7@O@6��@6h
@6�@5ϫ@5G�@5@5@@4�@4`�@4"h@3�@3�	@3K�@2��@2_�@2-@2u@1��@1��@1u�@1e,@1G�@1+�@1�@0�@0�e@0�@06@01@/y�@/X�@/;d@/"�@/�@.��@.�X@.�h@.�F@.��@.xl@._�@.{@-��@-�3@-�@-%F@-�@,��@,S�@,C-@,G@+��@+@O@+;d@+�@*�H@*?@*�@)�@)#�@(��@(�z@(b@'ƨ@'�@'A�@'�@&��@&��@&d�@&E�@&{@& �@%��@%�-@%p�@%8�@$��@$Ɇ@$��@$�.@$[�@$�@#�Q@#��@#�	@#o�@#Mj@#'�@"�y@"��@"��@"�\@"c @"-@"e@!�@!�C@!��@!zx@!=�@ �	@ �E@ ��@ ��@ y>@ �@خ@��@��@��@�*@y�@RT@6z@
=@��@�@��@��@�M@@��@�_@6@��@�}@�P@U�@,�@ i@�@�]@��@��@v�@E�@�@ �@�Z@�T@Vm@�@��@��@�I@g8@6@�@ݘ@��@�@s@U�@)_@ں@{�@3�@��@��@�=@m]@5�@ \@��@�u@�@�@oi@`�@V�@~@�@.I@Y@�@�@(@�"@��@Ta@.�@�@ �@��@�o@��@�H@�@?}@�@�@�I@A�@2�@ �@�w@��@�4@8@�@@��@��@�\@�+@xl@n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�B		B	qB	�B	]B	�B	~B	jB	�B	 vB	 �B	#�B	$�B	*KB	/5B	9�B	K^B	V�B	Y�B	ZB	ZB	a�B	eFB	_�B	`�B	d�B	��B	��B	��B	�UB
 �B
8lB
L�B
gB
g�B
g�B
n�B
r�B
wB
��B
�VB
��B
�B
�jB
��B
��B
�_B
��B�BQB$ZB*B1'B@ BfLB��B�XB�B��B��B�}B�>B�zB��B��B�?BĜB�	B��B�B�B�=B�9B�lB͟B�LB��B�6B�PB�2B��B��B��B�aB��By�BpB_;BNpBG�B@�B6+B&�BpBKB�B
�zB
�]B
�}B
�B
��B
��B
�HB
��B
�NB
�SB
kB
Y1B
I7B
7fB
�B	�0B	�8B	�B	�>B	��B	x�B	D�B	,�B	�B	 �B�B�jB׍BөB�&B�BB�B��B	mB	�B	�B	�B��B�B��B�B�RB��B��B��B��B�NB�8B��B�qB��B�B�B�+B��B	B	<B	
B	dB	"�B	RoB	c B	v+B	�B	��B	��B	�sB	��B	�B	��B	�B	��B	��B	�_B	��B	�RB	�LB	��B	��B	��B	��B	��B	�IB	�qB	�5B	��B	��B	��B	�LB	��B	�8B	��B	�fB	�bB	��B	�7B	�B	��B	��B	�=B	�$B	��B	�-B	�cB	�]B	��B	��B	�_B	�nB	��B	��B	�(B	�wB	��B	�3B	��B	��B	�OB	��B	�'B	��B	��B	�GB	��B	��B	��B	�;B	�TB	��B	��B	��B	��B	�TB	�B	�DB	��B	�9B	�B	��B	��B	��B	��B	��B	��B	�dB	�B	�lB	�^B	��B	˒B	�0B	��B	�DB	�)B	��B	̘B	�jB	�dB	��B	�0B	�6B	�PB	�PB	��B	�lB	�zB	��B	�B	��B	��B	��B	��B	��B	�EB	�?B	ևB	�B	�&B	ӏB	�B	��B	�B	՛B	�{B	��B	� B	��B	��B	�B	ѷB	��B	ѷB	� B	уB	҉B	��B	�aB	��B	ԕB	�aB	�,B	��B	өB	�&B	�B	��B	��B	��B	�sB	�KB	ںB	�	B	یB	�IB	��B	��B	ܬB	ܬB	��B	��B	��B	ݲB	ޞB	ߊB	�\B	�BB	�B	�B	�B	�nB	��B	�B	�B	�B	�B	�B	�8B	�B	��B	�B	�B	��B	�B	��B	��B	�B	��B	��B	�B	�B	�B	�B	��B	�B	��B	�*B	�yB	�B	�yB	��B	�KB	�B	��B	��B	�KB	��B	�6B	�kB	��B	�B	��B	��B	��B	�OB	�OB	�OB	�B	�B	�B	�iB	�iB	�B	�oB	��B	��B	�oB	��B	�B	�'B	��B	��B	��B	��B	��B	�GB	��B	��B	��B	��B	�nB	��B	�tB	�B	�`B	��B	��B	��B	��B	�>B	�XB	�DB	�dB	�B	��B	��B	��B	�JB	��B	��B	�VB	��B	�(B	�B	��B	��B	��B	��B	�]B	�]B	��B	�wB	��B	�wB	��B	�.B	�HB	��B	��B
 B
 �B
B
�B
�B
�B
�B
�B
�B
'B
uB
B
�B
�B
B
MB
gB
�B
gB
�B
�B
�B
�B
B
%B
YB
�B
EB
�B
�B
�B
�B
�B
�B
�B
	RB
	�B
	�B
	RB
	RB

�B

�B

�B

�B

XB

�B
�B
�B
B
B
B
�B
pB
(B
.B
�B
NB
�B
oB
oB
TB
�B
�B
FB
aB
FB
2B
�B
�B
�B
�B
QB
�B
�B
B
�B
CB
)B
B
�B
�B
�B
~B
�B
B
�B
B
�B
jB
!B
pB
�B
�B
OB
OB
�B
�B
�B
jB
B
OB
�B
!�B
 �B
 �B
!�B
"�B
"�B
"�B
#:B
$tB
$�B
#nB
#�B
#�B
#�B
#�B
#�B
$�B
%FB
%�B
&�B
'B
'mB
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)_B
)�B
*KB
*eB
*�B
*�B
+6B
+6B
+�B
+�B
,�B
,�B
-wB
-B
-CB
-)B
-CB
,�B
-CB
-�B
-�B
-�B
-�B
-�B
-�B
.B
.cB
.�B
.�B
.�B
/5B
/OB
/�B
0UB
0�B
1B
1[B
1AB
1[B
1'B
1'B
1B
1�B
1�B
2GB
3�B
33B
2�B
3B
4B
4B
4TB
4nB
4�B
4nB
4nB
5%B
5�B
5�B
6+B
6`B
6�B
7B
72B
7�B
7�B
8RB
9�B
:xB
:DB
:xB
:�B
:�B
:�B
;B
:�B
:�B
;JB
;B
;�B
;�B
=B
="B
=VB
>(B
>(B
>BB
>BB
>�B
>�B
?B
?.B
?HB
?�B
@4B
@iB
AB
B'B
BAB
BAB
B[B
B[B
B[B
B�B
BuB
CGB
DMB
D3B
D�B
D�B
D�B
EB
EB
F%B
F?B
F�B
GB
G_B
G�B
G�B
H1B
H1B
G�B
IRB
I�B
I�B
I�B
J	B
J#B
J#B
J=B
J=B
J�B
J�B
KB
KDB
K^B
KxB
KxB
KxB
K�B
K�B
K�B
KxB
K�B
K�B
LB
L0B
LJB
MB
M�B
N"B
NVB
N�B
N�B
O\B
O�B
O�B
O�B
PbB
P}B
P}B
P}B
P�B
Q B
QNB
Q4B
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
RB
R B
R B
R B
R�B
R�B
R�B
S&B
S�B
S�B
S�B
TB
S�B
S�B
S�B
S�B
TB
TaB
T�B
T�B
T�B
UgB
U�B
UgB
U�B
U�B
U�B
U�B
VSB
V9B
V�B
V�B
W$B
W�B
W�B
W�B
XB
XyB
X�B
X�B
X�B
X�B
Y1B
YeB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
ZQB
ZkB
Z�B
[#B
[=B
[qB
[�B
\)B
\)B
\)B
\CB
\�B
\�B
\�B
]/B
]/B
]~B
^B
^B
^B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_B
_!B
_�B
_�B
`\B
`BB
`vB
`�B
`vB
`�B
`vB
`vB
`�B
`�B
`�B
`�B
abB
a|B
a�B
a�B
bNB
b4B
b�B
b�B
b�B
cB
cnB
c�B
c�B
c�B
c�B
d&B
d@B
dZB
eB
e,B
d�B
e�B
e�B
e�B
f2B
f2B
ffB
f�B
f�B
f�B
gB
gB
g8B
g8B
gmB
g�B
g�B
g�B
h
B
g�B
g�B
h$B
h$B
h>B
h>B
hXB
hsB
h�B
h�B
h�B
h�B
iB
iDB
iDB
iDB
iyB
i�B
i�B
i�B
i�B
j0B
jeB
jB
jB
jeB
j�B
kB
kB
kB
kB
kB
kB
k6B
k6B
kkB
k�B
lqB
lqB
lqB
l�B
mB
m]B
m�B
m�B
nIB
n/B
n�B
n�B
n�B
o B
o B
o B
o5B
oiB
oOB
o�B
o�B
o�B
o�B
o�B
pUB
poB
p�B
p�B
p�B
q'B
qAB
qAB
q�B
q�B
q�B
q�B
q�B
r-B
rGB
r�B
r�B
sMB
shB
shB
shB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
t9B
t�B
t�B
uB
u?B
u?B
utB
u�B
vFB
vzB
vzB
vzB
vzB
vzB
v`B
vzB
vzB
v�B
v�B
wB
w�B
w�B
w�B
w�B
xB
xB
xRB
x�B
xRB
xlB
x�B
x�B
x�B
x�B
x�B
y	1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	�B		B	qB	�B	]B	�B	~B	jB	�B	 vB	 �B	#�B	$�B	*KB	/5B	9�B	K^B	V�B	Y�B	ZB	ZB	a�B	eFB	_�B	`�B	d�B	��B	��B	��B	�UB
 �B
8lB
L�B
gB
g�B
g�B
n�B
r�B
wB
��B
�VB
��B
�B
�jB
��B
��B
�_B
��B�BQB$ZB*B1'B@ BfLB��B�XB�B��B��B�}B�>B�zB��B��B�?BĜB�	B��B�B�B�=B�9B�lB͟B�LB��B�6B�PB�2B��B��B��B�aB��By�BpB_;BNpBG�B@�B6+B&�BpBKB�B
�zB
�]B
�}B
�B
��B
��B
�HB
��B
�NB
�SB
kB
Y1B
I7B
7fB
�B	�0B	�8B	�B	�>B	��B	x�B	D�B	,�B	�B	 �B�B�jB׍BөB�&B�BB�B��B	mB	�B	�B	�B��B�B��B�B�RB��B��B��B��B�NB�8B��B�qB��B�B�B�+B��B	B	<B	
B	dB	"�B	RoB	c B	v+B	�B	��B	��B	�sB	��B	�B	��B	�B	��B	��B	�_B	��B	�RB	�LB	��B	��B	��B	��B	��B	�IB	�qB	�5B	��B	��B	��B	�LB	��B	�8B	��B	�fB	�bB	��B	�7B	�B	��B	��B	�=B	�$B	��B	�-B	�cB	�]B	��B	��B	�_B	�nB	��B	��B	�(B	�wB	��B	�3B	��B	��B	�OB	��B	�'B	��B	��B	�GB	��B	��B	��B	�;B	�TB	��B	��B	��B	��B	�TB	�B	�DB	��B	�9B	�B	��B	��B	��B	��B	��B	��B	�dB	�B	�lB	�^B	��B	˒B	�0B	��B	�DB	�)B	��B	̘B	�jB	�dB	��B	�0B	�6B	�PB	�PB	��B	�lB	�zB	��B	�B	��B	��B	��B	��B	��B	�EB	�?B	ևB	�B	�&B	ӏB	�B	��B	�B	՛B	�{B	��B	� B	��B	��B	�B	ѷB	��B	ѷB	� B	уB	҉B	��B	�aB	��B	ԕB	�aB	�,B	��B	өB	�&B	�B	��B	��B	��B	�sB	�KB	ںB	�	B	یB	�IB	��B	��B	ܬB	ܬB	��B	��B	��B	ݲB	ޞB	ߊB	�\B	�BB	�B	�B	�B	�nB	��B	�B	�B	�B	�B	�B	�8B	�B	��B	�B	�B	��B	�B	��B	��B	�B	��B	��B	�B	�B	�B	�B	��B	�B	��B	�*B	�yB	�B	�yB	��B	�KB	�B	��B	��B	�KB	��B	�6B	�kB	��B	�B	��B	��B	��B	�OB	�OB	�OB	�B	�B	�B	�iB	�iB	�B	�oB	��B	��B	�oB	��B	�B	�'B	��B	��B	��B	��B	��B	�GB	��B	��B	��B	��B	�nB	��B	�tB	�B	�`B	��B	��B	��B	��B	�>B	�XB	�DB	�dB	�B	��B	��B	��B	�JB	��B	��B	�VB	��B	�(B	�B	��B	��B	��B	��B	�]B	�]B	��B	�wB	��B	�wB	��B	�.B	�HB	��B	��B
 B
 �B
B
�B
�B
�B
�B
�B
�B
'B
uB
B
�B
�B
B
MB
gB
�B
gB
�B
�B
�B
�B
B
%B
YB
�B
EB
�B
�B
�B
�B
�B
�B
�B
	RB
	�B
	�B
	RB
	RB

�B

�B

�B

�B

XB

�B
�B
�B
B
B
B
�B
pB
(B
.B
�B
NB
�B
oB
oB
TB
�B
�B
FB
aB
FB
2B
�B
�B
�B
�B
QB
�B
�B
B
�B
CB
)B
B
�B
�B
�B
~B
�B
B
�B
B
�B
jB
!B
pB
�B
�B
OB
OB
�B
�B
�B
jB
B
OB
�B
!�B
 �B
 �B
!�B
"�B
"�B
"�B
#:B
$tB
$�B
#nB
#�B
#�B
#�B
#�B
#�B
$�B
%FB
%�B
&�B
'B
'mB
'�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
(�B
)_B
)�B
*KB
*eB
*�B
*�B
+6B
+6B
+�B
+�B
,�B
,�B
-wB
-B
-CB
-)B
-CB
,�B
-CB
-�B
-�B
-�B
-�B
-�B
-�B
.B
.cB
.�B
.�B
.�B
/5B
/OB
/�B
0UB
0�B
1B
1[B
1AB
1[B
1'B
1'B
1B
1�B
1�B
2GB
3�B
33B
2�B
3B
4B
4B
4TB
4nB
4�B
4nB
4nB
5%B
5�B
5�B
6+B
6`B
6�B
7B
72B
7�B
7�B
8RB
9�B
:xB
:DB
:xB
:�B
:�B
:�B
;B
:�B
:�B
;JB
;B
;�B
;�B
=B
="B
=VB
>(B
>(B
>BB
>BB
>�B
>�B
?B
?.B
?HB
?�B
@4B
@iB
AB
B'B
BAB
BAB
B[B
B[B
B[B
B�B
BuB
CGB
DMB
D3B
D�B
D�B
D�B
EB
EB
F%B
F?B
F�B
GB
G_B
G�B
G�B
H1B
H1B
G�B
IRB
I�B
I�B
I�B
J	B
J#B
J#B
J=B
J=B
J�B
J�B
KB
KDB
K^B
KxB
KxB
KxB
K�B
K�B
K�B
KxB
K�B
K�B
LB
L0B
LJB
MB
M�B
N"B
NVB
N�B
N�B
O\B
O�B
O�B
O�B
PbB
P}B
P}B
P}B
P�B
Q B
QNB
Q4B
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
RB
R B
R B
R B
R�B
R�B
R�B
S&B
S�B
S�B
S�B
TB
S�B
S�B
S�B
S�B
TB
TaB
T�B
T�B
T�B
UgB
U�B
UgB
U�B
U�B
U�B
U�B
VSB
V9B
V�B
V�B
W$B
W�B
W�B
W�B
XB
XyB
X�B
X�B
X�B
X�B
Y1B
YeB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
ZQB
ZkB
Z�B
[#B
[=B
[qB
[�B
\)B
\)B
\)B
\CB
\�B
\�B
\�B
]/B
]/B
]~B
^B
^B
^B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_B
_!B
_�B
_�B
`\B
`BB
`vB
`�B
`vB
`�B
`vB
`vB
`�B
`�B
`�B
`�B
abB
a|B
a�B
a�B
bNB
b4B
b�B
b�B
b�B
cB
cnB
c�B
c�B
c�B
c�B
d&B
d@B
dZB
eB
e,B
d�B
e�B
e�B
e�B
f2B
f2B
ffB
f�B
f�B
f�B
gB
gB
g8B
g8B
gmB
g�B
g�B
g�B
h
B
g�B
g�B
h$B
h$B
h>B
h>B
hXB
hsB
h�B
h�B
h�B
h�B
iB
iDB
iDB
iDB
iyB
i�B
i�B
i�B
i�B
j0B
jeB
jB
jB
jeB
j�B
kB
kB
kB
kB
kB
kB
k6B
k6B
kkB
k�B
lqB
lqB
lqB
l�B
mB
m]B
m�B
m�B
nIB
n/B
n�B
n�B
n�B
o B
o B
o B
o5B
oiB
oOB
o�B
o�B
o�B
o�B
o�B
pUB
poB
p�B
p�B
p�B
q'B
qAB
qAB
q�B
q�B
q�B
q�B
q�B
r-B
rGB
r�B
r�B
sMB
shB
shB
shB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
t9B
t�B
t�B
uB
u?B
u?B
utB
u�B
vFB
vzB
vzB
vzB
vzB
vzB
v`B
vzB
vzB
v�B
v�B
wB
w�B
w�B
w�B
w�B
xB
xB
xRB
x�B
xRB
xlB
x�B
x�B
x�B
x�B
x�B
y	1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105227  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191211  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191212  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191212                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041219  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041219  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                