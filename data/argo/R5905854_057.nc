CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:54:47Z creation;2022-06-04T17:54:48Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604175447  20220610141506  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               9A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�3����H1   @�3�-��.@.0��
=q�c�t�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @�  A   A   A@  A`  A���A���A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B���B�  B�33B���B�  B���B�  B�  B���B���B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B���B�  B�  C   C  C  C  C  C
  C�C33C  C  C�C�fC�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:�C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Ce�fCg�fCj  Cl  Cn  Cp�Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D �fD!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�p 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @@�G�@�G�A ��A ��A@��A`��A��A��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B (�B(�B(�B(�B (�B((�B0(�B8(�B@(�BH(�BP(�BX(�B`(�Bh(�Bp(�Bx(�B�{B�{B��HB�{B�G�B��B�{B��HB�{B�{B��B��HB�{B�{B�{B��HB�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�G�B�G�B�{B��HB�{B�{C 
=C
=C
=C
=C
=C

=C#�C=pC
=C
=C#�C�C�C
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
=C6
=C8#�C:#�C<#�C>
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
=Ce�Cg�Cj
=Cl
=Cn
=Cp#�Cr#�Ct
=Cv
=Cx
=Cz
=C|
=C~
=C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��RC�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�)D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�)Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�{D�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD�~D��HD�HD�AHDÁHD��HD�HD�AHDāHD��HD�HD�AHDŁHD��HD�HD�AHDƁHD��HD�HD�AHDǁHD��HD�HD�AHDȁHD��HD�HD�AHDɁHD��HD�HD�AHDʁHD��HD�HD�AHDˁHD��HD�HD�AHD́HD��HD�HD�AHD́HD��HD�HD�AHD΁HD��HD�HD�AHDρHD��HD�HD�AHDЁHD��HD�HD�AHDсHD��HD�HD�AHDҁHD��HD�HD�AHDӁHD��HD�HD�AHDԁHD��HD�HD�AHDՁHD��HD�HD�AHDցHD��HD�HD�AHDׁHD��HD�HD�AHD؁HD��HD�HD�AHDفHD��HD�HD�AHDځHD��HD�HD�AHDہHD��HD�HD�AHD܁HD��HD�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�~D��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD�qH1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A׸�A׶�A״Aש_AצAךAדA׎�A׌�A׌�A׍�Aׇ�A׆�A׈�AׅAׄ�Aׄ�Aׄ�Aׄ�AׅAׅSAׅA׃GAׂAA�{�A�sA�U�A���A�cTA�R�Aї�A�*�A�K�A̒�Aʕ�A��aA��A���A�YA�YA�rGA�P�A��A��CA�'A�!-A�\)A�u�A�5A���A�u�A��dA�ߤA�`A���A��GA��A���A�n�A��A��NA���A��A�>BA���A���A��OA���A���A�}VA�D3A���A�~�A�5A�͟A���A���A��mA���A��xA���A�aHA�:�A���A��=A�%zA�K)A��gA��A��KA��|A|��Ay��Au�0An�VAkA�AgA�Ab�dA]ѷA[l�AZ��AY�SAW�LAT�1AP/�AN�*AMRTAK0UAIc AE��AAGA? �A<w�A:�4A9dZA6��A4hsA2~(A1v`A0�HA0l�A/c�A.SA-ݘA.A�A,��A,K�A+��A+�rA*��A(�A&�fA&�,A$e�A%($A$q�A#V�A"��A#~�A$i�A$qvA!�A ��A M�A !�AA}�A�KA��A��A}VA1A��A!-A��A�A�'AiDA��A�A� A�zA��AA8�A�&AAw�A*�A7A<�A_A��A�A��A\�A��AS�A�
AOvA�'AO�A�A�4A��A
�A
�tA
ZA
uA	A A	�A�7AMA�MA6A��A&AzAiDA�A6zA��Ak�A ��A \�A�A �?A �A xlA �A JA x@���@��j@��F@��@�'�@�r�@��@�M@���@�+@��]@���@��@��@�1@�e@�-@��@�l"@��@��@�$t@��@�.�@��@�~�@�f�@��@齥@��?@�u@�<�@��j@��@�@@�R@�Dg@Ṍ@�@@�t�@�e,@��v@�+@��@ව@��@߶F@�H�@���@އ+@��@��@ܙ1@�h�@��@���@�e@�`B@ؿ�@�]d@��
@��@�)�@շ�@�j@�F�@���@�7@ӖS@�!-@��@Ґ.@�:*@��D@ѓ�@���@�^5@�@��@��@΢4@�kQ@���@�b�@���@�~�@�'R@���@�5�@��@ʙ1@��.@�s@���@�D�@Ƿ@�+@Ɖ�@��@ż@�o�@��@�}V@��@�)_@���@�)�@��
@�j@�1�@��'@���@�I�@��+@�.I@���@�q�@���@�.I@���@�u%@���@�F�@�z@��t@�@�֡@��@��@��M@�ff@�G@��{@��P@���@� �@��;@���@�m]@��	@��1@�7�@��~@�Dg@��5@��@���@�~�@�.I@�ں@�{�@��@��@�B�@��@��@��2@���@�Q�@�-@�*�@���@��[@��4@�:�@��@�\�@�@��@�X�@��|@��@�ff@�B[@���@�*0@��<@�Ta@��;@�J#@��y@��O@���@�W�@�$@��S@�e�@� i@���@��9@���@�g8@��@�	@���@��@��@�!-@��@�6�@��r@�o @��@��H@���@���@���@�I�@���@��H@�v`@��@��H@���@���@�$@��@�خ@��P@�v`@�X@�7L@��@��@��'@�q�@� �@�� @��*@��4@�c@�O�@��@���@�?@���@���@�X�@��m@��D@�\�@��@��@�X@�)_@�#�@��@���@�l"@�g8@�_@�Mj@��@��8@�ѷ@���@���@��+@�:*@���@��N@��[@��S@�l�@���@���@�kQ@�*�@��9@���@�X@�!�@� i@���@��$@���@��o@���@��4@�7L@��@���@��@�n�@�8�@�@��d@���@���@�_p@��@��,@�u�@�2�@���@��g@�^�@��]@���@�z@�L0@��N@�P�@��@���@���@�~(@�~@��@��+@��@���@��3@��@�m]@�E9@��@���@��x@��@�D�@��@�ƨ@��@�]�@�Ĝ@�S�@�.�@���@��C@�a�@�O�@�/�@��@���@��'@���@�U2@�J@�@��@,�@~�1@}�@}�-@}��@|�@|Ft@{�0@{&@z�\@z
�@y�@x�@x�e@x*�@w��@w�0@we�@w�@v�@vR�@u�9@um]@uV@t��@tĜ@tU2@sJ#@r�}@rs�@r�@q��@q�@pg8@o� @o��@o��@oA�@o"�@n�@nȴ@nȴ@n^5@n+k@n$�@n�@m�-@m�@mY�@l�@lg8@l?�@l  @j�y@i�@i�z@i�h@i[W@h�/@hM@gݘ@g�@g�@f�h@fOv@f@e�@e��@d��@d�@d>B@c�
@cl�@c�@b�A@aϫ@a�@ao @aT�@a:�@`��@`��@`Z@`G@_ƨ@_�:@_Y@^��@^�c@^��@^��@]�@]^�@]?}@] \@\�5@\4n@[� @[��@[s@[C@Z�\@Z�@Y�7@YF@Y%@XĜ@X��@XQ�@W˒@W�$@WK�@W i@VJ�@U�.@U�T@U��@U?}@T��@T2�@S��@S6z@R��@R+k@Q��@Q8�@Q�@P��@P�4@P�@PM@O�@O��@O6z@Nں@N��@NJ�@M�o@M�"@MJ�@Mq@L�	@L��@L�@L�[@L��@L@K�&@K�{@K�@J{�@I�>@I�@I[W@H��@H�U@H"h@Gv`@G@F�@F�2@F��@F�<@F-@E�@Ec@E \@D��@Dѷ@D��@D�_@D>B@C�k@Ce�@C9�@B��@B��@BYK@A�@A�j@A��@A�S@A%@@��@?�@?��@?P�@>�@>�@>ff@>@�@>	@=�@=��@=G�@<�v@<u�@<�@;�&@;��@;�@:�L@:z@:^5@:6�@9�N@9��@9 \@8��@8[�@7�*@7X�@7@6�c@6�A@6	@5ԕ@5�~@5�@4ѷ@4��@4c�@4*�@3��@3l�@3o@2��@2��@2h
@2�@1�@1X@0��@0Ɇ@0�z@0��@0C-@/ݘ@/��@/�:@/�@.��@.q�@.R�@-�^@-q@,�@,�@,�@,ѷ@,�z@,]d@,$@+ƨ@+��@+�@+F�@+.I@+�@*͟@*R�@)�@)J�@(��@(��@(c�@'��@''�@&ȴ@&_�@&8�@%��@%�T@%�X@%%@$�@$��@$Ft@$�@#�@#W?@#�@"�b@"i�@"�@!�o@!��@!��@!:�@!q@!!�@!�@ �@ ��@ �z@ c�@ ~@�K@n/@o@��@1�@�@c�@�@��@�O@j@�r@��@�@��@�x@M�@8�@�.@��@�~@O�@��@��@��@tT@K^@(�@�r@�;@��@|�@P�@8@@�X@� @M�@�@��@��@�@7L@@�@��@��@K^@b@��@��@o�@>�@(@�s@��@�@��@��@l�@GE@($@�@u@�>@�N@�t@��@��@s�@\�@4@!�@�@Ɇ@u�@S�@�@�@��@v`@W?@;d@�H@�@ff@($@�D@ϫ@�H@��@zx@f�@J�@�@�E@��@��@r�@Z@�@�@�K@�@y�@dZ@Mj@�@
�"@
�@
�h@
��@
Z�@
?@
.�@
@	�T@	�d@	��@	�M@	S&@	7L@		l@��@�E@�U@�_@l"@:�@�@��@�@�@�@y�@dZ@�@�y@҉@�'@�r@^5@3�@($@�@��@G�@�@�v@�U@�@�j@�4@z�@]d1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A׸�A׶�A״Aש_AצAךAדA׎�A׌�A׌�A׍�Aׇ�A׆�A׈�AׅAׄ�Aׄ�Aׄ�Aׄ�AׅAׅSAׅA׃GAׂAA�{�A�sA�U�A���A�cTA�R�Aї�A�*�A�K�A̒�Aʕ�A��aA��A���A�YA�YA�rGA�P�A��A��CA�'A�!-A�\)A�u�A�5A���A�u�A��dA�ߤA�`A���A��GA��A���A�n�A��A��NA���A��A�>BA���A���A��OA���A���A�}VA�D3A���A�~�A�5A�͟A���A���A��mA���A��xA���A�aHA�:�A���A��=A�%zA�K)A��gA��A��KA��|A|��Ay��Au�0An�VAkA�AgA�Ab�dA]ѷA[l�AZ��AY�SAW�LAT�1AP/�AN�*AMRTAK0UAIc AE��AAGA? �A<w�A:�4A9dZA6��A4hsA2~(A1v`A0�HA0l�A/c�A.SA-ݘA.A�A,��A,K�A+��A+�rA*��A(�A&�fA&�,A$e�A%($A$q�A#V�A"��A#~�A$i�A$qvA!�A ��A M�A !�AA}�A�KA��A��A}VA1A��A!-A��A�A�'AiDA��A�A� A�zA��AA8�A�&AAw�A*�A7A<�A_A��A�A��A\�A��AS�A�
AOvA�'AO�A�A�4A��A
�A
�tA
ZA
uA	A A	�A�7AMA�MA6A��A&AzAiDA�A6zA��Ak�A ��A \�A�A �?A �A xlA �A JA x@���@��j@��F@��@�'�@�r�@��@�M@���@�+@��]@���@��@��@�1@�e@�-@��@�l"@��@��@�$t@��@�.�@��@�~�@�f�@��@齥@��?@�u@�<�@��j@��@�@@�R@�Dg@Ṍ@�@@�t�@�e,@��v@�+@��@ව@��@߶F@�H�@���@އ+@��@��@ܙ1@�h�@��@���@�e@�`B@ؿ�@�]d@��
@��@�)�@շ�@�j@�F�@���@�7@ӖS@�!-@��@Ґ.@�:*@��D@ѓ�@���@�^5@�@��@��@΢4@�kQ@���@�b�@���@�~�@�'R@���@�5�@��@ʙ1@��.@�s@���@�D�@Ƿ@�+@Ɖ�@��@ż@�o�@��@�}V@��@�)_@���@�)�@��
@�j@�1�@��'@���@�I�@��+@�.I@���@�q�@���@�.I@���@�u%@���@�F�@�z@��t@�@�֡@��@��@��M@�ff@�G@��{@��P@���@� �@��;@���@�m]@��	@��1@�7�@��~@�Dg@��5@��@���@�~�@�.I@�ں@�{�@��@��@�B�@��@��@��2@���@�Q�@�-@�*�@���@��[@��4@�:�@��@�\�@�@��@�X�@��|@��@�ff@�B[@���@�*0@��<@�Ta@��;@�J#@��y@��O@���@�W�@�$@��S@�e�@� i@���@��9@���@�g8@��@�	@���@��@��@�!-@��@�6�@��r@�o @��@��H@���@���@���@�I�@���@��H@�v`@��@��H@���@���@�$@��@�خ@��P@�v`@�X@�7L@��@��@��'@�q�@� �@�� @��*@��4@�c@�O�@��@���@�?@���@���@�X�@��m@��D@�\�@��@��@�X@�)_@�#�@��@���@�l"@�g8@�_@�Mj@��@��8@�ѷ@���@���@��+@�:*@���@��N@��[@��S@�l�@���@���@�kQ@�*�@��9@���@�X@�!�@� i@���@��$@���@��o@���@��4@�7L@��@���@��@�n�@�8�@�@��d@���@���@�_p@��@��,@�u�@�2�@���@��g@�^�@��]@���@�z@�L0@��N@�P�@��@���@���@�~(@�~@��@��+@��@���@��3@��@�m]@�E9@��@���@��x@��@�D�@��@�ƨ@��@�]�@�Ĝ@�S�@�.�@���@��C@�a�@�O�@�/�@��@���@��'@���@�U2@�J@�@��@,�@~�1@}�@}�-@}��@|�@|Ft@{�0@{&@z�\@z
�@y�@x�@x�e@x*�@w��@w�0@we�@w�@v�@vR�@u�9@um]@uV@t��@tĜ@tU2@sJ#@r�}@rs�@r�@q��@q�@pg8@o� @o��@o��@oA�@o"�@n�@nȴ@nȴ@n^5@n+k@n$�@n�@m�-@m�@mY�@l�@lg8@l?�@l  @j�y@i�@i�z@i�h@i[W@h�/@hM@gݘ@g�@g�@f�h@fOv@f@e�@e��@d��@d�@d>B@c�
@cl�@c�@b�A@aϫ@a�@ao @aT�@a:�@`��@`��@`Z@`G@_ƨ@_�:@_Y@^��@^�c@^��@^��@]�@]^�@]?}@] \@\�5@\4n@[� @[��@[s@[C@Z�\@Z�@Y�7@YF@Y%@XĜ@X��@XQ�@W˒@W�$@WK�@W i@VJ�@U�.@U�T@U��@U?}@T��@T2�@S��@S6z@R��@R+k@Q��@Q8�@Q�@P��@P�4@P�@PM@O�@O��@O6z@Nں@N��@NJ�@M�o@M�"@MJ�@Mq@L�	@L��@L�@L�[@L��@L@K�&@K�{@K�@J{�@I�>@I�@I[W@H��@H�U@H"h@Gv`@G@F�@F�2@F��@F�<@F-@E�@Ec@E \@D��@Dѷ@D��@D�_@D>B@C�k@Ce�@C9�@B��@B��@BYK@A�@A�j@A��@A�S@A%@@��@?�@?��@?P�@>�@>�@>ff@>@�@>	@=�@=��@=G�@<�v@<u�@<�@;�&@;��@;�@:�L@:z@:^5@:6�@9�N@9��@9 \@8��@8[�@7�*@7X�@7@6�c@6�A@6	@5ԕ@5�~@5�@4ѷ@4��@4c�@4*�@3��@3l�@3o@2��@2��@2h
@2�@1�@1X@0��@0Ɇ@0�z@0��@0C-@/ݘ@/��@/�:@/�@.��@.q�@.R�@-�^@-q@,�@,�@,�@,ѷ@,�z@,]d@,$@+ƨ@+��@+�@+F�@+.I@+�@*͟@*R�@)�@)J�@(��@(��@(c�@'��@''�@&ȴ@&_�@&8�@%��@%�T@%�X@%%@$�@$��@$Ft@$�@#�@#W?@#�@"�b@"i�@"�@!�o@!��@!��@!:�@!q@!!�@!�@ �@ ��@ �z@ c�@ ~@�K@n/@o@��@1�@�@c�@�@��@�O@j@�r@��@�@��@�x@M�@8�@�.@��@�~@O�@��@��@��@tT@K^@(�@�r@�;@��@|�@P�@8@@�X@� @M�@�@��@��@�@7L@@�@��@��@K^@b@��@��@o�@>�@(@�s@��@�@��@��@l�@GE@($@�@u@�>@�N@�t@��@��@s�@\�@4@!�@�@Ɇ@u�@S�@�@�@��@v`@W?@;d@�H@�@ff@($@�D@ϫ@�H@��@zx@f�@J�@�@�E@��@��@r�@Z@�@�@�K@�@y�@dZ@Mj@�@
�"@
�@
�h@
��@
Z�@
?@
.�@
@	�T@	�d@	��@	�M@	S&@	7L@		l@��@�E@�U@�_@l"@:�@�@��@�@�@�@y�@dZ@�@�y@҉@�'@�r@^5@3�@($@�@��@G�@�@�v@�U@�@�j@�4@z�@]d1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�B	��B	�B	�>B	�>B	�B	�XB	�>B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�&B	�EB	��B	�"B	�pB	��B	��B	��B	�B	�@B	�B	�	B	�pB
B
B
�B
&fB
7B
LJB
lWB
��B
��B
��B
��B
��B
��B
��B
��B
�dB�B�B�B2GBO�BC�B>wB_�BYeB{Bu�BYBp�B5�B3hB0�BBEBH�B�BBy�Bm)BW?BA�B0�B�B
�jB
��B
�oB
�dB
�;B
��B
cnB
F�B
qB	��B	�B	��B	m)B	TFB	;�B	!�B	
=B�B��B�|B��B� B�7B�oB�"BƎB�HB�B�GB��B��B�$B�B� B�'B�vB�B�B�aB�gB��B��B�RB	�B	xB	�B	0�B	+B	 �B	�B	,�B	(�B	>�B	J�B	R:B	S�B	o5B	��B	�sB	��B	��B	�B	�!B	�TB	�B	��B	�1B	��B	��B	�B	��B	��B	��B	��B	�B	�
B	��B	��B	��B	��B	�mB	�;B	�*B	�"B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�qB	�"B	��B	�]B	�B	�4B	�iB	��B	�B	�oB	�OB	�UB	� B	ªB	��B	ňB	āB	�aB	��B	��B	�uB	��B	��B	��B	��B	��B	�/B	�BB	�}B	��B	��B	��B	�BB	��B	�uB	�2B	�_B	��B	�TB	ӏB	��B	�PB	̳B	�B	�1B	�tB	��B	��B	�6B	��B	�oB	�TB	��B	�4B	�B	��B	ΥB	�2B	�yB	��B	خB	ּB	�B	�MB	��B	ٚB	��B	��B	�2B	ҽB	ѝB	�HB	ҽB	ӏB	�$B	�mB	ݲB	ބB	��B	�!B	�;B	߾B	�VB	��B	ߊB	��B	��B	ߊB	�B	�B	��B	��B	��B	�HB	��B	�bB	��B	��B	��B	�hB	�B	�B	�B	�B	�B	�B	�B	�B	�:B	�nB	�B	�nB	�B	�B	�B	�@B	�B	�&B	��B	��B	��B	�,B	�B	�B	�B	�zB	�,B	�`B	�,B	��B	�2B	��B	�2B	�B	��B	�B	�B	��B	�B	�B	��B	�DB	�B	��B	�B	�B	�B	��B	�B	�"B	�WB	�]B	��B	��B	��B	��B	�}B	��B	�B	�B	�vB	�nB	��B	�?B	�+B	�B	�fB	��B	��B	�rB	�>B	�>B	�rB	�*B	�^B	��B	��B	��B	�dB	�dB	��B	��B	��B	��B	�*B	�^B	�*B	�6B	��B	�"B	��B	��B	��B	�jB	�PB	��B	�dB	��B	�B	��B	�JB	�JB	��B	�B	��B	�(B	��B	�B	�}B	��B	��B
 4B
 B
 OB
 �B
 �B
 �B
B
 �B
�B
�B
AB
�B
�B
{B
�B
�B
gB
�B
�B
9B
SB
�B
gB
�B
YB
B
EB
�B
fB
KB
B
�B
�B
�B
	B
	RB
	7B
�B
�B
�B
�B
	7B

	B

�B
)B
xB
DB
B

�B

	B

	B
	�B

=B
�B
�B
�B
�B
B
�B
�B
pB
�B
�B
�B
(B
�B
:B
:B
TB
�B
[B
B
�B
�B
B
:B
 B
:B
:B
oB
oB
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
B
mB
$B
B
�B
�B
B
KB
_B
�B
�B
�B
=B
�B
xB
)B
�B
�B
�B
�B
xB
B
�B
VB
!B
VB
!HB
!HB
 BB
 BB
 B
 \B
!B
 �B
!B
!�B
#�B
$�B
$�B
$�B
$�B
%,B
%�B
&�B
&�B
'B
'mB
'�B
'mB
'RB
'B
'RB
'mB
'�B
'�B
'�B
'�B
(
B
(
B
'�B
'�B
'�B
)_B
)�B
)�B
*eB
*eB
*�B
+�B
+B
*B
*eB
*B
)�B
(�B
*B
*�B
*�B
*�B
*�B
*�B
*�B
+�B
+�B
,=B
,"B
-B
-CB
-CB
-�B
-�B
-�B
.}B
/ B
/5B
/�B
/�B
/�B
0!B
0oB
0�B
1�B
2B
2aB
2aB
2�B
3B
3B
3MB
4B
4�B
5B
5tB
5�B
5�B
6+B
6FB
6�B
6�B
72B
72B
6�B
6�B
6�B
7LB
6�B
6FB
6`B
6�B
6�B
8�B
9	B
9	B
9�B
9rB
:xB
;dB
;�B
;�B
;�B
<6B
<6B
<jB
<�B
<�B
<PB
<B
;JB
<PB
<�B
<6B
<6B
;�B
<B
<6B
<�B
<�B
=B
=�B
>�B
>�B
?.B
>�B
?cB
?HB
@ B
@iB
@iB
@�B
?�B
?�B
?�B
@�B
AB
A�B
A�B
A�B
A�B
A�B
A�B
BuB
C�B
C�B
C�B
C�B
DMB
DgB
D�B
EB
EB
EB
EmB
E�B
F%B
FYB
F�B
F�B
G�B
HfB
H�B
I�B
I�B
J#B
J�B
J�B
KB
KDB
K^B
K�B
K�B
L0B
L~B
L�B
L�B
MB
MB
L�B
MB
MjB
MjB
M�B
NB
N�B
O(B
O(B
O(B
O(B
O(B
O\B
PHB
P�B
P�B
P�B
P�B
P�B
QhB
QNB
Q�B
R B
RTB
R:B
R:B
RTB
R�B
S&B
S&B
S[B
SuB
SuB
S�B
S�B
S�B
S�B
TB
T�B
T�B
UMB
U�B
U�B
U�B
VB
VB
V9B
VB
VB
VSB
V9B
V�B
W?B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
X+B
XyB
X�B
X�B
YKB
YB
ZB
ZB
ZQB
Z7B
Z�B
Z�B
[	B
[WB
[�B
\B
\B
\CB
\�B
\�B
]/B
]dB
]dB
]~B
]�B
^B
^B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_!B
_B
_�B
_�B
_�B
_�B
`�B
aHB
abB
abB
abB
a|B
a�B
b�B
c B
b�B
b�B
cTB
c�B
c�B
c�B
dZB
e�B
e�B
e�B
e�B
e�B
e�B
e�B
fLB
f�B
f�B
gB
gmB
gmB
g�B
h
B
iDB
iyB
iDB
i�B
i�B
iyB
iB
iB
h�B
i*B
iDB
i�B
j0B
jB
jB
kQB
k�B
l"B
lWB
lqB
l�B
l�B
l�B
l�B
mCB
m�B
m�B
m�B
nIB
n}B
n�B
n�B
o B
o�B
o�B
pB
o�B
p!B
pUB
pUB
p�B
p�B
p�B
p�B
q'B
qB
qAB
qvB
qvB
q�B
q�B
q�B
q�B
r-B
rGB
rGB
r|B
r�B
r�B
s3B
shB
s�B
s�B
s�B
t9B
tTB
tnB
t�B
t�B
t�B
u%B
u?B
utB
u�B
u�B
vB
vFB
v`B
vzB
v�B
v�B
v�B
v�B
v�B
wB
wB
w2B
w2B
wLB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x8B
x�B
x�B
x�B
y	B
yXB
yrB
yrB
y�B
zB
z*B
zxB
z�B
z�B
z�B
z�B
{B
{JB
{dB
{B
{�B
|B
|B
|B
|jB
|jB
|�B
|�B
}B
}<B
}qB
}qB
}�B
}�B
}�B
}�B
~B
~]B
~wB
~�B
~�B
~�B
~�B
B
B
HB
}B
�B
�B
�B
�B
�B
�4B
�iB
��B
��B
��B
�B
�;B
�UB
�oB
��B
��B
��B
�B
�B
�AB
�uB
��B
��B
��B
�B
�{B
��B
��B
��B
��B
��B
��B
�B
�M1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�B	��B	�B	�>B	�>B	�B	�XB	�>B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�&B	�EB	��B	�"B	�pB	��B	��B	��B	�B	�@B	�B	�	B	�pB
B
B
�B
&fB
7B
LJB
lWB
��B
��B
��B
��B
��B
��B
��B
��B
�dB�B�B�B2GBO�BC�B>wB_�BYeB{Bu�BYBp�B5�B3hB0�BBEBH�B�BBy�Bm)BW?BA�B0�B�B
�jB
��B
�oB
�dB
�;B
��B
cnB
F�B
qB	��B	�B	��B	m)B	TFB	;�B	!�B	
=B�B��B�|B��B� B�7B�oB�"BƎB�HB�B�GB��B��B�$B�B� B�'B�vB�B�B�aB�gB��B��B�RB	�B	xB	�B	0�B	+B	 �B	�B	,�B	(�B	>�B	J�B	R:B	S�B	o5B	��B	�sB	��B	��B	�B	�!B	�TB	�B	��B	�1B	��B	��B	�B	��B	��B	��B	��B	�B	�
B	��B	��B	��B	��B	�mB	�;B	�*B	�"B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�qB	�"B	��B	�]B	�B	�4B	�iB	��B	�B	�oB	�OB	�UB	� B	ªB	��B	ňB	āB	�aB	��B	��B	�uB	��B	��B	��B	��B	��B	�/B	�BB	�}B	��B	��B	��B	�BB	��B	�uB	�2B	�_B	��B	�TB	ӏB	��B	�PB	̳B	�B	�1B	�tB	��B	��B	�6B	��B	�oB	�TB	��B	�4B	�B	��B	ΥB	�2B	�yB	��B	خB	ּB	�B	�MB	��B	ٚB	��B	��B	�2B	ҽB	ѝB	�HB	ҽB	ӏB	�$B	�mB	ݲB	ބB	��B	�!B	�;B	߾B	�VB	��B	ߊB	��B	��B	ߊB	�B	�B	��B	��B	��B	�HB	��B	�bB	��B	��B	��B	�hB	�B	�B	�B	�B	�B	�B	�B	�B	�:B	�nB	�B	�nB	�B	�B	�B	�@B	�B	�&B	��B	��B	��B	�,B	�B	�B	�B	�zB	�,B	�`B	�,B	��B	�2B	��B	�2B	�B	��B	�B	�B	��B	�B	�B	��B	�DB	�B	��B	�B	�B	�B	��B	�B	�"B	�WB	�]B	��B	��B	��B	��B	�}B	��B	�B	�B	�vB	�nB	��B	�?B	�+B	�B	�fB	��B	��B	�rB	�>B	�>B	�rB	�*B	�^B	��B	��B	��B	�dB	�dB	��B	��B	��B	��B	�*B	�^B	�*B	�6B	��B	�"B	��B	��B	��B	�jB	�PB	��B	�dB	��B	�B	��B	�JB	�JB	��B	�B	��B	�(B	��B	�B	�}B	��B	��B
 4B
 B
 OB
 �B
 �B
 �B
B
 �B
�B
�B
AB
�B
�B
{B
�B
�B
gB
�B
�B
9B
SB
�B
gB
�B
YB
B
EB
�B
fB
KB
B
�B
�B
�B
	B
	RB
	7B
�B
�B
�B
�B
	7B

	B

�B
)B
xB
DB
B

�B

	B

	B
	�B

=B
�B
�B
�B
�B
B
�B
�B
pB
�B
�B
�B
(B
�B
:B
:B
TB
�B
[B
B
�B
�B
B
:B
 B
:B
:B
oB
oB
�B
�B
�B
�B
B
�B
�B
�B
�B
�B
�B
B
mB
$B
B
�B
�B
B
KB
_B
�B
�B
�B
=B
�B
xB
)B
�B
�B
�B
�B
xB
B
�B
VB
!B
VB
!HB
!HB
 BB
 BB
 B
 \B
!B
 �B
!B
!�B
#�B
$�B
$�B
$�B
$�B
%,B
%�B
&�B
&�B
'B
'mB
'�B
'mB
'RB
'B
'RB
'mB
'�B
'�B
'�B
'�B
(
B
(
B
'�B
'�B
'�B
)_B
)�B
)�B
*eB
*eB
*�B
+�B
+B
*B
*eB
*B
)�B
(�B
*B
*�B
*�B
*�B
*�B
*�B
*�B
+�B
+�B
,=B
,"B
-B
-CB
-CB
-�B
-�B
-�B
.}B
/ B
/5B
/�B
/�B
/�B
0!B
0oB
0�B
1�B
2B
2aB
2aB
2�B
3B
3B
3MB
4B
4�B
5B
5tB
5�B
5�B
6+B
6FB
6�B
6�B
72B
72B
6�B
6�B
6�B
7LB
6�B
6FB
6`B
6�B
6�B
8�B
9	B
9	B
9�B
9rB
:xB
;dB
;�B
;�B
;�B
<6B
<6B
<jB
<�B
<�B
<PB
<B
;JB
<PB
<�B
<6B
<6B
;�B
<B
<6B
<�B
<�B
=B
=�B
>�B
>�B
?.B
>�B
?cB
?HB
@ B
@iB
@iB
@�B
?�B
?�B
?�B
@�B
AB
A�B
A�B
A�B
A�B
A�B
A�B
BuB
C�B
C�B
C�B
C�B
DMB
DgB
D�B
EB
EB
EB
EmB
E�B
F%B
FYB
F�B
F�B
G�B
HfB
H�B
I�B
I�B
J#B
J�B
J�B
KB
KDB
K^B
K�B
K�B
L0B
L~B
L�B
L�B
MB
MB
L�B
MB
MjB
MjB
M�B
NB
N�B
O(B
O(B
O(B
O(B
O(B
O\B
PHB
P�B
P�B
P�B
P�B
P�B
QhB
QNB
Q�B
R B
RTB
R:B
R:B
RTB
R�B
S&B
S&B
S[B
SuB
SuB
S�B
S�B
S�B
S�B
TB
T�B
T�B
UMB
U�B
U�B
U�B
VB
VB
V9B
VB
VB
VSB
V9B
V�B
W?B
W�B
W�B
W�B
W�B
W�B
W�B
W�B
X+B
XyB
X�B
X�B
YKB
YB
ZB
ZB
ZQB
Z7B
Z�B
Z�B
[	B
[WB
[�B
\B
\B
\CB
\�B
\�B
]/B
]dB
]dB
]~B
]�B
^B
^B
^�B
^�B
^�B
^�B
^�B
^�B
^�B
_!B
_B
_�B
_�B
_�B
_�B
`�B
aHB
abB
abB
abB
a|B
a�B
b�B
c B
b�B
b�B
cTB
c�B
c�B
c�B
dZB
e�B
e�B
e�B
e�B
e�B
e�B
e�B
fLB
f�B
f�B
gB
gmB
gmB
g�B
h
B
iDB
iyB
iDB
i�B
i�B
iyB
iB
iB
h�B
i*B
iDB
i�B
j0B
jB
jB
kQB
k�B
l"B
lWB
lqB
l�B
l�B
l�B
l�B
mCB
m�B
m�B
m�B
nIB
n}B
n�B
n�B
o B
o�B
o�B
pB
o�B
p!B
pUB
pUB
p�B
p�B
p�B
p�B
q'B
qB
qAB
qvB
qvB
q�B
q�B
q�B
q�B
r-B
rGB
rGB
r|B
r�B
r�B
s3B
shB
s�B
s�B
s�B
t9B
tTB
tnB
t�B
t�B
t�B
u%B
u?B
utB
u�B
u�B
vB
vFB
v`B
vzB
v�B
v�B
v�B
v�B
v�B
wB
wB
w2B
w2B
wLB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
x8B
x�B
x�B
x�B
y	B
yXB
yrB
yrB
y�B
zB
z*B
zxB
z�B
z�B
z�B
z�B
{B
{JB
{dB
{B
{�B
|B
|B
|B
|jB
|jB
|�B
|�B
}B
}<B
}qB
}qB
}�B
}�B
}�B
}�B
~B
~]B
~wB
~�B
~�B
~�B
~�B
B
B
HB
}B
�B
�B
�B
�B
�B
�4B
�iB
��B
��B
��B
�B
�;B
�UB
�oB
��B
��B
��B
�B
�B
�AB
�uB
��B
��B
��B
�B
�{B
��B
��B
��B
��B
��B
��B
�B
�M1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104958  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175447  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175447  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175448                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025455  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025455  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141506                      G�O�G�O�G�O�                