CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:24:38Z creation;2022-06-04T17:24:39Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604172438  20220610131506  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @��h�m�51   @��i�r(@,���"���c�Ƨ1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B�  B�  B�  B�  B�  B�  B�ffBߙ�B�  B���B���B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj�Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)�fD*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR�fDSfDS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}�fD~fD~�fD  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�3D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @(�@�z�@�G�A ��A ��A@��A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A��A�Q�B (�B(�B(�B(�B (�B((�B0(�B8(�B@(�BH(�BP(�BX(�B`(�Bh(�Bp(�Bx(�B�{B�z�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�G�B��HB�{B�{B�{B�{B�{B�{B�z�B߮B�{B��HB��HB�{B�{B�{B�{C 
=C
=C
=C
=C
=C

=C
=C#�C#�C
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
=C4
=C6#�C8#�C:
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
=C`
=Cb
=Cd
=Cf
=Ch
=Cj#�Cl#�Cn
=Cp
=Cr
=Ct
=Cv
=Cx
=Cz
=C|
=C~
=C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��RC�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C��C��C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHDHD��HD�HD�AHDÁHD��HD�HD�AHDāHD��HD�HD�AHDŁHD��HD�HD�AHDƁHD��HD�HD�AHDǁHD��HD�{D�AHDȁHD��HD�HD�AHDɁHD��HD�HD�AHDʁHD��HD�HD�AHDˁHD��HD�HD�AHD́HD��HD�HD�AHD́HD��HD�HD�AHD΁HD��HD�HD�AHDρHD��HD�HD�AHDЁHD��HD�HD�AHDсHD��HD�HD�AHDҁHD��HD�HD�AHDӁHD��HD�HD�AHDԁHD��HD�HD�AHDՁHD��HD�HD�AHDցHD��HD�HD�AHDׁHD��HD�HD�AHD؁HD��HD�HD�AHDفHD��HD�HD�AHDځHD��HD�HD�AHDہHD��HD�HD�AHD܁HD��HD�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD��HD�HD�AHD��HD��{D�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD�j�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A׍�AאbAג�Aג:Aו�Aי1Aי�AלCAם~Aמ�Aן�Aך7Aי1Aם�Aח�A�{�A���A�h�A�H�A�zDAθ�A�B�À�A���A�c�A�H�A�+6A��Aˮ�A��AʋA�sMAɾA�|�Aș�A�V9A��A�6�A�bNA��2A�_AċDA��|AÀ�A�A���A���A�.}A�o A��RA���A��2A��yA��LA�K)A�J#A�~�A�8A�MA���A��A�=<A���A�&LA���A���A���A���A�)_A��A��HA��7A���A��qA���A���A��A���A�;dA���A�ɺA�bNA�]/A���A��-A�y	A�9$A��~A���Az��Ay��Au�Au�At�As�6Ar#�Ap_pAm�'Aj�
AhCAe�A]�AZ��AV_AT��AR~AOh
AN��AM$�AIYAI!�AHSAD~�A@�^A@�A?o�A=�A;l"A98A3��A3�:A2��A1P�A0��A/7LA-ߤA-�A,�A,U�A+��A*��A*A(�A'e�A'�A&�]A&�AA&OA%MjA$zxA#��A"�~A"��A!� A N�A�`Ax�A�OA�	AGEA��A_AIRAuAl�AF�A�PA��A��Ao�A��A?}AoA=�A�kA��Au�A�A�|A1A�KA�hA1'A�hA��A�pAr�A�&AZ�A|�A�YAS&A \A��Al�A��A�kAd�A4�A@A�KAl�AA
�_A
f�A
GA	aA�fA
�A�Aq�A7�AV�A�fA#:A�KA.�A(�A!�A�dAN�A�A�KA��AƨA��A�A�3A
�A��A��A{�ARTA	lAخA��A:�A~�A�AaA?�A �A =@�<6@�/@�V@�Dg@���@���@���@��@�33@��R@�n�@� �@�e�@� �@�q@� �@��@��~@�n/@���@���@�(@�z@�@�:*@�S�@�O@�@�L@�1'@�w@�F@��@�8�@��@�L0@���@�x@��@겖@��@�!@���@�X@�S@��@�$�@�ȴ@�)�@��@�k�@��)@�}�@��@���@�@�xl@��@���@ߌ~@��]@�Xy@�J�@�.�@�
�@��m@��@���@���@��d@�U�@��@�	@��j@פ@@�^�@�F@��@ֿ�@��@��@ԛ�@��d@ӄM@�>�@��@���@�kQ@��)@�[W@��8@�y>@���@�c�@��8@�l"@��m@͎�@́@�qv@�X@�"�@���@̟�@�u%@�)�@�ƨ@˦�@��@ʼj@ʱ�@ʖ�@�D�@ɦ�@��2@Ȏ�@�]d@�6@���@ǹ�@Ǔ@�X�@��@Ɓo@�3�@���@�O@�Y@Ĝx@�Q@�@���@�c@�F@���@�N�@�e@�ƨ@��/@�i�@�c @�1'@��r@���@�/�@��'@�?@��T@��F@�Dg@�V@�e�@�ƨ@�x�@�^�@�@O@��@���@�@�?}@�Ĝ@�U2@�.�@�5�@��@���@�Ov@�e@�ԕ@���@��)@�v�@�5?@��;@�B�@���@���@�~�@�I�@��@���@�Mj@�'�@�ѷ@�`�@�ԕ@�]�@���@���@���@���@��o@�e@��@�J#@��|@��r@�E�@�M@���@��a@�y�@���@�kQ@�b@��=@�*0@�
=@��E@���@�� @�r�@�c�@�C-@��
@�y�@�P�@���@�^5@��+@���@��@��\@�^5@�>B@�1�@�!�@��@�/@��@�L0@��3@��'@�k�@�$t@���@��@��@���@���@��9@���@�=q@���@��Q@��~@�@�r�@��T@��N@���@�u�@�8@���@���@�{�@�R�@�@���@�@@�Ɇ@�d�@�M�@��#@���@��M@�@���@�]d@��W@���@���@���@�G�@� i@���@�ȴ@�d�@��@��;@���@�`B@���@�c�@�Z@�)�@�J@��K@�~�@�\�@�)_@�֡@��@�]d@�1�@���@��F@�o @��@��@��@�1�@��}@�}�@�T�@�!-@���@�@�@��A@��[@�{J@�U�@�E9@�2a@��@���@��@��!@�a|@�~@��;@��h@�Vm@��@��P@��?@��.@�M@��@�خ@��@�t�@�K�@�4@�Y@��M@��@��@�Z@���@��F@���@�33@��@���@�~(@�<�@���@��[@�o�@�Q�@��"@��!@��_@�1'@��r@��j@���@���@�|@�!�@��H@�Q�@�*�@�M@�@�@}�-@}T�@|��@|�z@|N�@{��@{X�@z�@z�2@z҉@z��@zz@z�@y7L@xM@x"h@x7@x�@w�@wv`@v�6@v��@vYK@v5?@u�d@u<6@t,=@sP�@r�m@r\�@q��@qrG@p�v@p<�@oMj@n�\@n+k@m�^@m4@l�.@k�A@k��@k]�@k�@j�2@js�@j	@i@iIR@h�K@h�@g�g@g]�@f�@f^5@f:*@e�@e��@d��@d7�@d�@d	�@c�@c��@c>�@bں@bl�@b�@aO�@`ی@`��@`��@`l"@`7�@_�@_�:@_!-@^{�@]�7@]�@\��@\z�@[�]@[��@[a@[@Z��@ZJ�@Y��@X��@X�.@X6@W�]@W˒@W_p@W�@V��@V҉@V�1@Vz@V_�@V:*@U��@Uc@UG�@T�@TH@S��@SW?@SH�@S>�@S�@S�@R�M@R�@R �@Q�>@Q��@P�/@O��@O|�@O"�@N҉@N�L@N$�@M��@L�@L��@L-�@K��@K@J�2@J�@JGE@I�@I�S@H�@HXy@G��@Gl�@GRT@F�y@Fp;@E�@E(�@D~(@D	�@C��@C�g@C�@C�P@B�h@Ac�@@��@@tT@@%�@?�r@?ݘ@?��@?qv@>�,@>�@=�-@=m]@=!�@<�5@<��@<�@<��@<u�@<�@;ݘ@;�V@;�	@;"�@:��@:�'@:^5@:�@9��@9��@9��@9�@8�Y@8Xy@8A�@8?�@82�@8*�@7�m@7�:@6�@6{�@6?@6�@5�H@5Y�@5�@4��@3�r@3�F@3�@3��@3y�@3\)@2�H@2��@2Ta@2�@1c�@1�@0��@0|�@0<�@0C-@0,=@/ƨ@/qv@/�@.�"@.�@.��@.�2@.��@.a|@.@-��@-+@,�@,q@,�@+��@+��@+/�@*�,@*�@*�'@*R�@*�@)a�@(�f@(�@(�o@(K^@(�@'�@'��@'RT@'@&��@&�6@&kQ@&
�@%�@%�H@%��@%��@%��@%IR@$�|@$�.@#��@#@O@"��@"�b@"�F@"��@"u%@"GE@"�@!�@!m]@!=�@ �5@ �E@ �p@ ��@ A�@ x@��@'�@�X@��@�!@i�@B[@&�@�.@�@�^@��@4@�@]d@@G@��@�@@��@�{@W?@Mj@�@�s@��@z@5?@�D@L�@��@q@�@��@��@v`@8@"�@�@��@͟@�R@z@!�@	@u@�o@��@m]@^�@%F@��@��@e�@	�@��@�k@Mj@�@�@�]@�B@i�@3�@��@�@��@��@O�@<6@4@(�@�@�v@�@oi@PH@$@�@�@�@�@�:@|�@\)@P�@K�@F�@�@��@��@��@�s@�R@�!@xl@M�@��@�@��@|@A @��@��@�[@��@��@��@�.@|�@%�@  @��@�@�r@��@�@~�@qv@dZ@W?@O@>�@@
�"@
��@
�F1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A׍�AאbAג�Aג:Aו�Aי1Aי�AלCAם~Aמ�Aן�Aך7Aי1Aם�Aח�A�{�A���A�h�A�H�A�zDAθ�A�B�À�A���A�c�A�H�A�+6A��Aˮ�A��AʋA�sMAɾA�|�Aș�A�V9A��A�6�A�bNA��2A�_AċDA��|AÀ�A�A���A���A�.}A�o A��RA���A��2A��yA��LA�K)A�J#A�~�A�8A�MA���A��A�=<A���A�&LA���A���A���A���A�)_A��A��HA��7A���A��qA���A���A��A���A�;dA���A�ɺA�bNA�]/A���A��-A�y	A�9$A��~A���Az��Ay��Au�Au�At�As�6Ar#�Ap_pAm�'Aj�
AhCAe�A]�AZ��AV_AT��AR~AOh
AN��AM$�AIYAI!�AHSAD~�A@�^A@�A?o�A=�A;l"A98A3��A3�:A2��A1P�A0��A/7LA-ߤA-�A,�A,U�A+��A*��A*A(�A'e�A'�A&�]A&�AA&OA%MjA$zxA#��A"�~A"��A!� A N�A�`Ax�A�OA�	AGEA��A_AIRAuAl�AF�A�PA��A��Ao�A��A?}AoA=�A�kA��Au�A�A�|A1A�KA�hA1'A�hA��A�pAr�A�&AZ�A|�A�YAS&A \A��Al�A��A�kAd�A4�A@A�KAl�AA
�_A
f�A
GA	aA�fA
�A�Aq�A7�AV�A�fA#:A�KA.�A(�A!�A�dAN�A�A�KA��AƨA��A�A�3A
�A��A��A{�ARTA	lAخA��A:�A~�A�AaA?�A �A =@�<6@�/@�V@�Dg@���@���@���@��@�33@��R@�n�@� �@�e�@� �@�q@� �@��@��~@�n/@���@���@�(@�z@�@�:*@�S�@�O@�@�L@�1'@�w@�F@��@�8�@��@�L0@���@�x@��@겖@��@�!@���@�X@�S@��@�$�@�ȴ@�)�@��@�k�@��)@�}�@��@���@�@�xl@��@���@ߌ~@��]@�Xy@�J�@�.�@�
�@��m@��@���@���@��d@�U�@��@�	@��j@פ@@�^�@�F@��@ֿ�@��@��@ԛ�@��d@ӄM@�>�@��@���@�kQ@��)@�[W@��8@�y>@���@�c�@��8@�l"@��m@͎�@́@�qv@�X@�"�@���@̟�@�u%@�)�@�ƨ@˦�@��@ʼj@ʱ�@ʖ�@�D�@ɦ�@��2@Ȏ�@�]d@�6@���@ǹ�@Ǔ@�X�@��@Ɓo@�3�@���@�O@�Y@Ĝx@�Q@�@���@�c@�F@���@�N�@�e@�ƨ@��/@�i�@�c @�1'@��r@���@�/�@��'@�?@��T@��F@�Dg@�V@�e�@�ƨ@�x�@�^�@�@O@��@���@�@�?}@�Ĝ@�U2@�.�@�5�@��@���@�Ov@�e@�ԕ@���@��)@�v�@�5?@��;@�B�@���@���@�~�@�I�@��@���@�Mj@�'�@�ѷ@�`�@�ԕ@�]�@���@���@���@���@��o@�e@��@�J#@��|@��r@�E�@�M@���@��a@�y�@���@�kQ@�b@��=@�*0@�
=@��E@���@�� @�r�@�c�@�C-@��
@�y�@�P�@���@�^5@��+@���@��@��\@�^5@�>B@�1�@�!�@��@�/@��@�L0@��3@��'@�k�@�$t@���@��@��@���@���@��9@���@�=q@���@��Q@��~@�@�r�@��T@��N@���@�u�@�8@���@���@�{�@�R�@�@���@�@@�Ɇ@�d�@�M�@��#@���@��M@�@���@�]d@��W@���@���@���@�G�@� i@���@�ȴ@�d�@��@��;@���@�`B@���@�c�@�Z@�)�@�J@��K@�~�@�\�@�)_@�֡@��@�]d@�1�@���@��F@�o @��@��@��@�1�@��}@�}�@�T�@�!-@���@�@�@��A@��[@�{J@�U�@�E9@�2a@��@���@��@��!@�a|@�~@��;@��h@�Vm@��@��P@��?@��.@�M@��@�خ@��@�t�@�K�@�4@�Y@��M@��@��@�Z@���@��F@���@�33@��@���@�~(@�<�@���@��[@�o�@�Q�@��"@��!@��_@�1'@��r@��j@���@���@�|@�!�@��H@�Q�@�*�@�M@�@�@}�-@}T�@|��@|�z@|N�@{��@{X�@z�@z�2@z҉@z��@zz@z�@y7L@xM@x"h@x7@x�@w�@wv`@v�6@v��@vYK@v5?@u�d@u<6@t,=@sP�@r�m@r\�@q��@qrG@p�v@p<�@oMj@n�\@n+k@m�^@m4@l�.@k�A@k��@k]�@k�@j�2@js�@j	@i@iIR@h�K@h�@g�g@g]�@f�@f^5@f:*@e�@e��@d��@d7�@d�@d	�@c�@c��@c>�@bں@bl�@b�@aO�@`ی@`��@`��@`l"@`7�@_�@_�:@_!-@^{�@]�7@]�@\��@\z�@[�]@[��@[a@[@Z��@ZJ�@Y��@X��@X�.@X6@W�]@W˒@W_p@W�@V��@V҉@V�1@Vz@V_�@V:*@U��@Uc@UG�@T�@TH@S��@SW?@SH�@S>�@S�@S�@R�M@R�@R �@Q�>@Q��@P�/@O��@O|�@O"�@N҉@N�L@N$�@M��@L�@L��@L-�@K��@K@J�2@J�@JGE@I�@I�S@H�@HXy@G��@Gl�@GRT@F�y@Fp;@E�@E(�@D~(@D	�@C��@C�g@C�@C�P@B�h@Ac�@@��@@tT@@%�@?�r@?ݘ@?��@?qv@>�,@>�@=�-@=m]@=!�@<�5@<��@<�@<��@<u�@<�@;ݘ@;�V@;�	@;"�@:��@:�'@:^5@:�@9��@9��@9��@9�@8�Y@8Xy@8A�@8?�@82�@8*�@7�m@7�:@6�@6{�@6?@6�@5�H@5Y�@5�@4��@3�r@3�F@3�@3��@3y�@3\)@2�H@2��@2Ta@2�@1c�@1�@0��@0|�@0<�@0C-@0,=@/ƨ@/qv@/�@.�"@.�@.��@.�2@.��@.a|@.@-��@-+@,�@,q@,�@+��@+��@+/�@*�,@*�@*�'@*R�@*�@)a�@(�f@(�@(�o@(K^@(�@'�@'��@'RT@'@&��@&�6@&kQ@&
�@%�@%�H@%��@%��@%��@%IR@$�|@$�.@#��@#@O@"��@"�b@"�F@"��@"u%@"GE@"�@!�@!m]@!=�@ �5@ �E@ �p@ ��@ A�@ x@��@'�@�X@��@�!@i�@B[@&�@�.@�@�^@��@4@�@]d@@G@��@�@@��@�{@W?@Mj@�@�s@��@z@5?@�D@L�@��@q@�@��@��@v`@8@"�@�@��@͟@�R@z@!�@	@u@�o@��@m]@^�@%F@��@��@e�@	�@��@�k@Mj@�@�@�]@�B@i�@3�@��@�@��@��@O�@<6@4@(�@�@�v@�@oi@PH@$@�@�@�@�@�:@|�@\)@P�@K�@F�@�@��@��@��@�s@�R@�!@xl@M�@��@�@��@|@A @��@��@�[@��@��@��@�.@|�@%�@  @��@�@�r@��@�@~�@qv@dZ@W?@O@>�@@
�"@
��@
�F1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�,B�,B�FB�,B�,B�,B�,B�B�B�,B�B�zB�B�FB�B�B�B�ZB	%,B	`vB	^�B	cB	cB	i*B	s�B	s�B	tnB	x8B	��B	�B	��B	��B	��B	�7B	�B	��B	��B	��B	�B	��B	��B
B
BAB
ZB
}�B
�{B
�B
�fB
�wBVB�B�B�B6+B'RB./BG�B�hB  B]B�B�JB��B BGB�B�B�B��B�B��B�-B��B��By�Bf�BQ BE�B?.B9	B-�B�B B
�LB
̘B
�\B
��B
W?B
�B	�B	��B	�_B	�wB	��B	��B	��B	��B	�@B	�AB	p�B	]�B	6�B	$�B	�B	B��B�ZB�-B�|B�B	FB	B	"�B	%B	+�B	:�B	O�B	_�B	U�B	@�B	YB	sMB	{JB	}VB	~�B	�AB	��B	��B	�MB	�lB	�(B	��B	�IB	��B	�vB	�RB	��B	�B	ÖB	�B	��B	�_B	͹B	��B	��B	̈́B	�"B	��B	�oB	�_B	��B	�1B	�TB	�mB	��B	�B	�%B	��B
uB
SB
B
jB
BB
�B
�B
�B
�B
�B
�B
)B
CB
pB
!�B
(�B
)�B
+kB
-�B
,qB
)DB
%�B
#�B
(>B
)�B
.B
/�B
-wB
)_B
(�B
(�B
(�B
)�B
&LB
&LB
#�B
$�B
$�B
"�B
�B
�B
�B
�B
B
sB
gB
hB
�B
�B
�B
1B
�B
�B
B
1B
�B
_B
�B
WB
�B
$�B
&2B
%�B
'B
&LB
%,B
#�B
$tB
#�B
"�B
!|B
�B
pB
IB
1B
B
�B
gB
oB
�B
JB
�B
�B
�B
xB
0B
bB
�B
SB
�B
�B
�B
uB
oB
4B
�B
�B
JB
�B
<B
gB
�B
MB
uB
TB
�B
4B
�B
�B
NB
.B
�B
BB
�B
�B
PB
�B
6B
�B
�B
DB

�B
	RB
�B
zB
EB
�B
�B
�B
B
GB
�B
uB
'B
�B
�B
�B
B
fB
	�B
%B
�B
�B
�B
�B
SB
�B
�B
	�B
�B
�B
�B

�B

�B
	�B
�B
	B

	B
	lB
	B
�B
B
zB
fB
�B
�B
�B
zB
�B
zB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
_B
�B
B
�B
_B
�B
�B
�B
�B
�B
�B
B
�B
KB
�B
�B
	RB
	7B
	B
	B
	RB
	B
	�B
	�B
	�B
	lB
	B
	lB
	�B

�B

�B

�B
B

�B
�B
^B
)B
^B
B
^B
�B
�B
�B
�B
�B
xB
�B
xB
xB
~B
dB
�B
0B
~B
�B
"B
�B
<B
(B
(B
B
(B
�B
�B
�B
vB
�B
vB
\B
�B
(B
\B
BB
�B
<B
�B
�B
�B
�B
�B
(B
bB
NB
�B
:B
TB
�B
�B
�B
FB
aB
2B
gB
�B
9B
B
mB
SB
mB
SB
B
B
SB
mB
SB
mB
�B
B
�B
SB
�B
B
B
B
�B
�B
B
�B
aB
�B
�B
�B
�B
�B
�B
@B
�B
B
�B
B
B
�B
�B
B
aB
�B
9B
mB
�B
?B
?B
+B
_B
EB
yB
_B
EB
B
B
B
�B
=B
�B
�B
WB
qB
�B
�B
~B
dB
�B
�B
�B
�B
 vB
 �B
!HB
!�B
!�B
!�B
# B
#TB
#:B
#�B
#�B
#�B
$@B
$ZB
$�B
$�B
%,B
%`B
%�B
%�B
&B
&fB
&�B
'B
'B
'�B
($B
(sB
(sB
(�B
)DB
)�B
)�B
*KB
*B
*�B
*�B
*�B
*B
*�B
*�B
*�B
+B
+B
+kB
+�B
,B
,=B
,qB
,�B
,�B
-B
-CB
-�B
.B
.IB
.�B
/ B
/�B
0!B
0UB
0oB
0�B
1�B
1�B
1vB
2GB
2aB
2�B
2�B
2�B
2�B
3MB
3�B
3hB
4B
49B
4TB
5%B
5?B
5ZB
5?B
5ZB
5tB
5�B
6B
6zB
6�B
6�B
6�B
7LB
8B
7�B
88B
88B
8lB
8�B
8�B
8�B
8�B
9	B
8�B
9$B
9	B
9�B
:DB
:DB
:DB
:*B
:^B
:�B
;0B
;B
;0B
;B
;B
;dB
<B
<�B
="B
=VB
=�B
=�B
=�B
>(B
>�B
>]B
>wB
?HB
?}B
?HB
?HB
>�B
?.B
?cB
?HB
?HB
?cB
?�B
?�B
@ B
@4B
@�B
@�B
AoB
A�B
A�B
A�B
B'B
C-B
CaB
C-B
CB
C-B
C-B
CGB
C�B
C{B
C{B
C�B
DB
C�B
C�B
D�B
EmB
E�B
FB
FB
F�B
G�B
G�B
HfB
HKB
H�B
H�B
H�B
IB
IB
I�B
JXB
J�B
J�B
KDB
K)B
K^B
K�B
K�B
K�B
K�B
L0B
LB
L0B
L0B
LdB
LdB
L~B
L~B
L�B
L~B
L~B
L�B
MPB
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
MPB
N<B
N�B
O�B
O\B
O�B
O�B
O�B
O�B
P.B
P}B
P�B
Q B
QB
P�B
P}B
Q B
Q�B
Q�B
R B
R�B
R�B
S@B
S&B
S�B
S�B
S�B
S�B
S�B
S�B
TFB
T�B
T�B
UMB
UgB
UMB
UgB
U�B
U�B
VSB
V�B
W$B
W?B
W�B
W�B
W�B
W�B
W�B
XB
X�B
XyB
X�B
X�B
X�B
X�B
Y1B
Y�B
Y�B
Y�B
Y�B
Y�B
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[�B
[�B
\)B
\)B
\�B
\�B
\�B
]/B
]�B
]�B
]�B
]�B
]�B
]�B
^OB
^OB
^�B
^�B
_VB
_�B
_�B
_�B
`vB
`�B
`�B
`�B
b4B
b�B
b�B
cB
c B
c B
c B
b�B
cTB
dZB
d�B
d�B
ezB
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e,B
e`B
e�B
e�B
fLB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
gB
gRB
g�B
g�B
g�B
h$B
h$B
hXB
h�B
iB
i�B
jKB
k6B
kkB
k�B
k�B
k�B
l=B
lqB
l�B
l�B
l�B
mB
m]B
m�B
nB
n}B
nIB
n�B
n�B
oB
o5B
o�B
pB
pUB
p�B
p�B
p�B
qB
p�B
q[B
q�B
q�B
q�B
q�B
r-B
raB
raB
r|B
r�B
r�B
sB
shB
s�B
s�B
s�B
s�B
tB
s�B
s�B
tB
tB
t9B
t�B
t�B
t�B
t�B
t�B
uB
uB
u�B
u�B
u�B
u�B
u�B
vzB
vzB
vzB
v�B
v�B
wB
wLB
w�B
w�B
w�B
xRB
xlB
x�B
x�B
x�B
y$B
yXB
y�B
zB
zB
zDB
z�B
z�B
z�B
z�B
z�B
z�B
{B
{JB
{dB
{�B
{�B
{�B
{�B
|6B
|6B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}B
}<B
}B
}VB
}qB
}�B
}�B
}�B
~BB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
.B
}B
cB
cB
HB
� B
�B
�4B
� B
�OB
�4B
�OB
�OB
�iB
��B
�B
� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�,B�,B�FB�,B�,B�,B�,B�B�B�,B�B�zB�B�FB�B�B�B�ZB	%,B	`vB	^�B	cB	cB	i*B	s�B	s�B	tnB	x8B	��B	�B	��B	��B	��B	�7B	�B	��B	��B	��B	�B	��B	��B
B
BAB
ZB
}�B
�{B
�B
�fB
�wBVB�B�B�B6+B'RB./BG�B�hB  B]B�B�JB��B BGB�B�B�B��B�B��B�-B��B��By�Bf�BQ BE�B?.B9	B-�B�B B
�LB
̘B
�\B
��B
W?B
�B	�B	��B	�_B	�wB	��B	��B	��B	��B	�@B	�AB	p�B	]�B	6�B	$�B	�B	B��B�ZB�-B�|B�B	FB	B	"�B	%B	+�B	:�B	O�B	_�B	U�B	@�B	YB	sMB	{JB	}VB	~�B	�AB	��B	��B	�MB	�lB	�(B	��B	�IB	��B	�vB	�RB	��B	�B	ÖB	�B	��B	�_B	͹B	��B	��B	̈́B	�"B	��B	�oB	�_B	��B	�1B	�TB	�mB	��B	�B	�%B	��B
uB
SB
B
jB
BB
�B
�B
�B
�B
�B
�B
)B
CB
pB
!�B
(�B
)�B
+kB
-�B
,qB
)DB
%�B
#�B
(>B
)�B
.B
/�B
-wB
)_B
(�B
(�B
(�B
)�B
&LB
&LB
#�B
$�B
$�B
"�B
�B
�B
�B
�B
B
sB
gB
hB
�B
�B
�B
1B
�B
�B
B
1B
�B
_B
�B
WB
�B
$�B
&2B
%�B
'B
&LB
%,B
#�B
$tB
#�B
"�B
!|B
�B
pB
IB
1B
B
�B
gB
oB
�B
JB
�B
�B
�B
xB
0B
bB
�B
SB
�B
�B
�B
uB
oB
4B
�B
�B
JB
�B
<B
gB
�B
MB
uB
TB
�B
4B
�B
�B
NB
.B
�B
BB
�B
�B
PB
�B
6B
�B
�B
DB

�B
	RB
�B
zB
EB
�B
�B
�B
B
GB
�B
uB
'B
�B
�B
�B
B
fB
	�B
%B
�B
�B
�B
�B
SB
�B
�B
	�B
�B
�B
�B

�B

�B
	�B
�B
	B

	B
	lB
	B
�B
B
zB
fB
�B
�B
�B
zB
�B
zB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
_B
�B
B
�B
_B
�B
�B
�B
�B
�B
�B
B
�B
KB
�B
�B
	RB
	7B
	B
	B
	RB
	B
	�B
	�B
	�B
	lB
	B
	lB
	�B

�B

�B

�B
B

�B
�B
^B
)B
^B
B
^B
�B
�B
�B
�B
�B
xB
�B
xB
xB
~B
dB
�B
0B
~B
�B
"B
�B
<B
(B
(B
B
(B
�B
�B
�B
vB
�B
vB
\B
�B
(B
\B
BB
�B
<B
�B
�B
�B
�B
�B
(B
bB
NB
�B
:B
TB
�B
�B
�B
FB
aB
2B
gB
�B
9B
B
mB
SB
mB
SB
B
B
SB
mB
SB
mB
�B
B
�B
SB
�B
B
B
B
�B
�B
B
�B
aB
�B
�B
�B
�B
�B
�B
@B
�B
B
�B
B
B
�B
�B
B
aB
�B
9B
mB
�B
?B
?B
+B
_B
EB
yB
_B
EB
B
B
B
�B
=B
�B
�B
WB
qB
�B
�B
~B
dB
�B
�B
�B
�B
 vB
 �B
!HB
!�B
!�B
!�B
# B
#TB
#:B
#�B
#�B
#�B
$@B
$ZB
$�B
$�B
%,B
%`B
%�B
%�B
&B
&fB
&�B
'B
'B
'�B
($B
(sB
(sB
(�B
)DB
)�B
)�B
*KB
*B
*�B
*�B
*�B
*B
*�B
*�B
*�B
+B
+B
+kB
+�B
,B
,=B
,qB
,�B
,�B
-B
-CB
-�B
.B
.IB
.�B
/ B
/�B
0!B
0UB
0oB
0�B
1�B
1�B
1vB
2GB
2aB
2�B
2�B
2�B
2�B
3MB
3�B
3hB
4B
49B
4TB
5%B
5?B
5ZB
5?B
5ZB
5tB
5�B
6B
6zB
6�B
6�B
6�B
7LB
8B
7�B
88B
88B
8lB
8�B
8�B
8�B
8�B
9	B
8�B
9$B
9	B
9�B
:DB
:DB
:DB
:*B
:^B
:�B
;0B
;B
;0B
;B
;B
;dB
<B
<�B
="B
=VB
=�B
=�B
=�B
>(B
>�B
>]B
>wB
?HB
?}B
?HB
?HB
>�B
?.B
?cB
?HB
?HB
?cB
?�B
?�B
@ B
@4B
@�B
@�B
AoB
A�B
A�B
A�B
B'B
C-B
CaB
C-B
CB
C-B
C-B
CGB
C�B
C{B
C{B
C�B
DB
C�B
C�B
D�B
EmB
E�B
FB
FB
F�B
G�B
G�B
HfB
HKB
H�B
H�B
H�B
IB
IB
I�B
JXB
J�B
J�B
KDB
K)B
K^B
K�B
K�B
K�B
K�B
L0B
LB
L0B
L0B
LdB
LdB
L~B
L~B
L�B
L~B
L~B
L�B
MPB
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
M�B
MPB
N<B
N�B
O�B
O\B
O�B
O�B
O�B
O�B
P.B
P}B
P�B
Q B
QB
P�B
P}B
Q B
Q�B
Q�B
R B
R�B
R�B
S@B
S&B
S�B
S�B
S�B
S�B
S�B
S�B
TFB
T�B
T�B
UMB
UgB
UMB
UgB
U�B
U�B
VSB
V�B
W$B
W?B
W�B
W�B
W�B
W�B
W�B
XB
X�B
XyB
X�B
X�B
X�B
X�B
Y1B
Y�B
Y�B
Y�B
Y�B
Y�B
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[�B
[�B
\)B
\)B
\�B
\�B
\�B
]/B
]�B
]�B
]�B
]�B
]�B
]�B
^OB
^OB
^�B
^�B
_VB
_�B
_�B
_�B
`vB
`�B
`�B
`�B
b4B
b�B
b�B
cB
c B
c B
c B
b�B
cTB
dZB
d�B
d�B
ezB
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e�B
e,B
e`B
e�B
e�B
fLB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
f�B
gB
gRB
g�B
g�B
g�B
h$B
h$B
hXB
h�B
iB
i�B
jKB
k6B
kkB
k�B
k�B
k�B
l=B
lqB
l�B
l�B
l�B
mB
m]B
m�B
nB
n}B
nIB
n�B
n�B
oB
o5B
o�B
pB
pUB
p�B
p�B
p�B
qB
p�B
q[B
q�B
q�B
q�B
q�B
r-B
raB
raB
r|B
r�B
r�B
sB
shB
s�B
s�B
s�B
s�B
tB
s�B
s�B
tB
tB
t9B
t�B
t�B
t�B
t�B
t�B
uB
uB
u�B
u�B
u�B
u�B
u�B
vzB
vzB
vzB
v�B
v�B
wB
wLB
w�B
w�B
w�B
xRB
xlB
x�B
x�B
x�B
y$B
yXB
y�B
zB
zB
zDB
z�B
z�B
z�B
z�B
z�B
z�B
{B
{JB
{dB
{�B
{�B
{�B
{�B
|6B
|6B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}B
}<B
}B
}VB
}qB
}�B
}�B
}�B
~BB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
.B
}B
cB
cB
HB
� B
�B
�4B
� B
�OB
�4B
�OB
�OB
�iB
��B
�B
� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104847  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604172438  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604172439  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604172439                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605022446  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605022446  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131506                      G�O�G�O�G�O�                