CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:51:32Z creation;2022-06-04T17:51:32Z conversion to V3.1      
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
_FillValue                 �  I,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p\   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tH   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �p   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �t   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �x   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220604175132  20220610141505  5905854                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               'A   JA                                  2B  A   APEX                            8422                            2.11.2                          846 @�
�1   @�|�/�@0~��"���cM����1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @,��@y��@�  A��A   A@  Aa��A�  A�  A�  A�  A�  A�  A�  A���B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�33B�ffB���B�  B���B�  B���B���C  C  C  C  C
  C  C33C  C  C33C� C  C  C  C  C   C!�fC$  C&  C(�C*�C,�C.  C/�fC2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C��C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD fD � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9y�D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DYfDY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� DjfDj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�3D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @/\)@|(�@�G�A=qA ��A@��Ab=qA�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A��B �\B(�B(�B(�B (�B((�B0(�B8(�B@(�BH(�BP(�BX(�B`(�Bh(�Bp(�Bx(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�G�B�{B�{B�{B�{B�{B�{B�{B�{B�G�B�G�B�{B�{B�G�B�z�B��HB�{B��B�{B��HB��HC
=C
=C
=C
=C

=C
=C=pC
=C
=C=pC�=C
=C
=C
=C
=C 
=C!�C$
=C&
=C(#�C*#�C,#�C.
=C/�C2
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
=CR
=CT
=CV
=CX
=CZ
=C\#�C^
=C`
=Cb
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
=C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��RC�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C��C�C�C�C�C�C�C�C��C�C�C�C�C��C��C�C��RC�C�C�C�C�C�C�C�C�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9|)D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D}�D}��D~�D~��D�D��D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHDHD��HD�HD�AHDÁHD��HD�HD�AHDāHD��HD�HD�AHDŁHD��HD�HD�AHDƁHD��HD�HD�AHDǁHD��HD�HD�AHDȁHD��HD�HD�AHDɁHD��HD�HD�AHDʁHD��HD�HD�AHDˁHD��HD�HD�AHD́HD��HD�HD�AHD́HD��HD�HD�AHD΁HD��HD�HD�AHDρHD��HD�HD�AHDЁHD��HD�HD�AHDсHD��HD�HD�AHDҁHD��HD�HD�AHDӁHD��HD�HD�AHDԁHD��HD�HD�AHDՁHD��HD�HD�AHDցHD��HD�{D�AHDׁHD��HD�HD�AHD؁HD��HD�HD�AHDفHD��HD�HD�AHDځHD��HD�HD�AHDہHD��HD�HD�AHD܁HD��HD�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�h>A�kQA�jA�j�A�k�A�kA�m�A�o5A�l�A�l�A�f2A�bA�b�A�V�A�S[A�GEA�+A�+�A�)�A�)�A�)_A�)�A�)�A�*�A�*eA�)�A�)�A�*�A�+6A�*eA�*eA�)_A�)�A�*0A�*�A�+6A�+�A�,qA�-A�-�A�.�A�-wA�.IA�.�A�/�A�0�A�1�A�33A�5�A�6�A�8�A�/�A�"hAŉ�A�_�A���AÏ�A��A��^A���A��A�d�A�-�A���A��6A�c A�n/A�/�A���A�j�A��6A�~]A�u�A�;dA�Z�A�HA�@�A�ߤA�d�A�ҽA�_�A�͟A~�A|5?Ay~�Av�zAs-Ap+�Ak� Ah�Ad��Abl"AaYKA_�OA]�fAX\)AV�aAUԕAR�:AO�ANںAM��AHAG	AF-�AC�A@�A@�FA?4�A=\�A<HA;y�A9��A8L0A7��A7y�A7	lA6_A5e�A4�A3\)A2��A0]dA/�A,m]A+5?A*��A*�fA*zxA*X�A(a�A'�aA%i�A$�A#��A#m�A#=qA"<�A!�A /�A��A-wA�-AB[A{JA2�A�wA{A�|A��AA��A�aAL�Am�AIRA�A
=AIRAA�A��A�Ac�A�A��AѷAn/A%FA�A�]Ay>A��A��A��AW?A+kA��Au%AVAƨA��AR�A�IA�A+A;AxA�A�^AݘA�A��A8�A�AĜA-A
jA	��A	<�A	�A�XA^�A��A>BA�cA��A�"A?}A~(AW?AJ�A33A7AݘA��Ap�AHA�ZA�A��A%�AJA��A��A��Au�Ae,A6A�A��A�A ��A 5?A +A .I@���@���@���@�t�@�%@�Ĝ@��6@�]d@��{@��@�)�@���@�]d@���@��f@��@���@�:�@���@��>@�@�:@��]@�Ft@�@�(@��@�Ɇ@��)@�`B@��@�p;@��@��@�9@�dZ@�E9@褩@�Y�@��@�-w@�8�@�@@��@���@�)_@�@�8�@���@߶F@��@�͟@��@���@��.@���@�*0@ؿ�@�)�@���@�\)@��H@��@�+@֣�@� �@�e@ա�@�Y�@�p�@�e�@���@ԇ�@�	@ӫ�@��f@щ7@�y>@ρ@�;@�kQ@�,=@���@͡�@�@O@�"�@��@̓u@�1@˩�@�-@��@�H�@�N<@�\�@�'�@�l�@��@ɟV@�Ɇ@ǜ@Ƭ�@�>B@�M@���@���@�x@�!�@�:�@ƕ�@�Z�@��@Ŋ�@�a�@���@��@Ą�@ĥz@ć+@�V@�A�@��@��@Èf@�|@��@�v�@�c�@���@�oi@�2�@��5@��.@���@�(�@�Ĝ@��+@�B[@���@��>@���@��c@�~�@�YK@�-@��@��Z@��}@�e,@���@��:@��@�8�@��@��g@��d@��N@���@��@��)@���@��]@��.@���@��W@��f@�!-@��`@��,@���@�N�@�F�@���@�/�@�\)@�I�@�o�@�b�@�K�@� \@���@��@�q�@�L0@��X@�@O@�ѷ@�A�@�  @�T�@���@��@���@�^�@�ی@�q@��Q@���@���@�H�@���@��6@�y>@�
�@���@�^�@��|@���@�1�@��6@�n/@�q@���@��@��,@��6@��H@���@�7L@��@�	@��m@��g@��@���@���@�J#@��@���@�bN@�,=@�4@���@�Z�@�+@���@��<@��4@�Q�@��]@���@�0�@�q@��$@�Q�@�E�@�1�@���@���@�s�@�:�@�+@�@���@�bN@��@��)@���@��S@��@��@���@��1@�|�@�h�@�E�@�!@���@�s@��@���@��5@��@��P@��M@���@���@�>B@��@��j@��-@��P@�F�@��@��]@��$@�I�@�G@�ԕ@���@�g�@���@��r@�?@��@���@�q@���@�;�@��@�@���@��;@��@��@@���@�\�@�9�@���@���@�h
@�M@�@�@�6@�H@�/�@�ϫ@���@�V@���@���@�YK@�-�@��@���@��{@�a@���@��<@���@���@���@�(�@�	@���@���@���@�qv@�O�@�4@��@�ȴ@���@�l"@�Q�@�I�@�{@���@��X@���@��~@�K�@�8@�5�@�@@��2@���@���@�[�@��Q@�x�@�E9@�+@�͟@�4n@��@���@�v`@�#�@��'@�j@�L0@�	@��@>�@�@~҉@~R�@}o @{�V@{P�@{'�@{
=@z��@z�'@z�x@z-@yx�@x��@x`�@w��@w��@wl�@w>�@v��@v�!@vl�@v+k@u�@t�@s�
@s�[@s��@st�@sC�@r��@r+k@qs�@qG�@q%@p�Y@pXy@o�@o;d@n0U@n�@m�@m�~@m \@l��@k�@k�0@kiD@k)_@j�y@j��@j�1@j$�@i�#@i��@i�h@iT�@hq@g��@gW?@f��@f��@f�R@f��@fa|@f�@e�@e��@e�7@e4@d��@d:�@c��@c��@c�f@b�M@b�]@b��@b^5@b�@a��@ahs@a&�@`��@`�@`S�@_�w@_��@_X�@_6z@_/�@_&@_C@_�@_�@_�@_@_ i@^�@^�@]�D@]�>@]@]�=@]s�@\_@[�@Z͟@Zp;@Y�@Y�h@YT�@Y(�@Y�@X��@X��@X��@XH@X �@W��@W�F@WW?@V��@V�@V��@V{�@V-@U�D@U��@U�n@Ux�@U�@TC-@S33@S@R�@R҉@R�b@RTa@Q�T@Q�"@Qx�@Q=�@P�@Pl"@P�@O�}@O�w@O�k@Oj�@O�@NJ�@M��@MY�@M!�@M@@L�p@L��@L~@L�@Kx@K;d@K@J��@J�'@J�b@Jc @JC�@J�@I��@I�X@Ie,@I*0@HFt@G�@G��@G]�@F��@F	@E�#@E��@E?}@E@@D��@D~(@Dj@D,=@C
=@B��@B�s@B��@BJ�@A�@A\�@A-w@A@@@��@@Z@?��@?�f@?v`@?E9@?.I@>�@>�m@>c @>e@>@=�@=�h@=&�@<z�@<2�@;��@:�@:{�@:1�@9��@8�)@8@7�w@7_p@7;d@7�@6�y@6��@6n�@6�@5�o@5�#@5��@5��@5}�@5m]@5S&@5J�@5?}@5&�@4�@4tT@3��@3��@3�:@3J#@2�@2ȴ@2ȴ@2�\@2kQ@2c @2Z�@2\�@2M�@2�@2_@1�H@1��@0��@01'@0%�@/�@/��@/�:@/dZ@.�@-o @-N<@-<6@,�	@,��@,�D@,e�@,!@+��@+�P@+g�@*��@*\�@*3�@*�@*J@)�z@)�t@)��@)��@)c�@)+@(�/@(��@(��@(��@(tT@(Ft@'�r@'��@'��@'j�@'P�@'4�@''�@'�@&�m@&��@&��@&��@&ff@&!�@%��@%��@%?}@%%F@%�@%�@$��@$�o@$I�@$4n@#�@#��@#�q@#��@#�	@#O@#�@"�@"�R@"��@"a|@"&�@"@!��@!�M@!s�@!L�@!@@ U2@��@qv@g�@+@C@�@S@�2@p;@:*@e@�@@�S@�7@m]@Q�@/@�`@oi@>B@�@��@��@Mj@1�@�8@��@L0@�@��@=�@�|@��@PH@6@@�@s�@J�@@��@��@p�@F@%F@�	@��@U2@b@�]@��@�:@s@$t@҉@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�h>A�kQA�jA�j�A�k�A�kA�m�A�o5A�l�A�l�A�f2A�bA�b�A�V�A�S[A�GEA�+A�+�A�)�A�)�A�)_A�)�A�)�A�*�A�*eA�)�A�)�A�*�A�+6A�*eA�*eA�)_A�)�A�*0A�*�A�+6A�+�A�,qA�-A�-�A�.�A�-wA�.IA�.�A�/�A�0�A�1�A�33A�5�A�6�A�8�A�/�A�"hAŉ�A�_�A���AÏ�A��A��^A���A��A�d�A�-�A���A��6A�c A�n/A�/�A���A�j�A��6A�~]A�u�A�;dA�Z�A�HA�@�A�ߤA�d�A�ҽA�_�A�͟A~�A|5?Ay~�Av�zAs-Ap+�Ak� Ah�Ad��Abl"AaYKA_�OA]�fAX\)AV�aAUԕAR�:AO�ANںAM��AHAG	AF-�AC�A@�A@�FA?4�A=\�A<HA;y�A9��A8L0A7��A7y�A7	lA6_A5e�A4�A3\)A2��A0]dA/�A,m]A+5?A*��A*�fA*zxA*X�A(a�A'�aA%i�A$�A#��A#m�A#=qA"<�A!�A /�A��A-wA�-AB[A{JA2�A�wA{A�|A��AA��A�aAL�Am�AIRA�A
=AIRAA�A��A�Ac�A�A��AѷAn/A%FA�A�]Ay>A��A��A��AW?A+kA��Au%AVAƨA��AR�A�IA�A+A;AxA�A�^AݘA�A��A8�A�AĜA-A
jA	��A	<�A	�A�XA^�A��A>BA�cA��A�"A?}A~(AW?AJ�A33A7AݘA��Ap�AHA�ZA�A��A%�AJA��A��A��Au�Ae,A6A�A��A�A ��A 5?A +A .I@���@���@���@�t�@�%@�Ĝ@��6@�]d@��{@��@�)�@���@�]d@���@��f@��@���@�:�@���@��>@�@�:@��]@�Ft@�@�(@��@�Ɇ@��)@�`B@��@�p;@��@��@�9@�dZ@�E9@褩@�Y�@��@�-w@�8�@�@@��@���@�)_@�@�8�@���@߶F@��@�͟@��@���@��.@���@�*0@ؿ�@�)�@���@�\)@��H@��@�+@֣�@� �@�e@ա�@�Y�@�p�@�e�@���@ԇ�@�	@ӫ�@��f@щ7@�y>@ρ@�;@�kQ@�,=@���@͡�@�@O@�"�@��@̓u@�1@˩�@�-@��@�H�@�N<@�\�@�'�@�l�@��@ɟV@�Ɇ@ǜ@Ƭ�@�>B@�M@���@���@�x@�!�@�:�@ƕ�@�Z�@��@Ŋ�@�a�@���@��@Ą�@ĥz@ć+@�V@�A�@��@��@Èf@�|@��@�v�@�c�@���@�oi@�2�@��5@��.@���@�(�@�Ĝ@��+@�B[@���@��>@���@��c@�~�@�YK@�-@��@��Z@��}@�e,@���@��:@��@�8�@��@��g@��d@��N@���@��@��)@���@��]@��.@���@��W@��f@�!-@��`@��,@���@�N�@�F�@���@�/�@�\)@�I�@�o�@�b�@�K�@� \@���@��@�q�@�L0@��X@�@O@�ѷ@�A�@�  @�T�@���@��@���@�^�@�ی@�q@��Q@���@���@�H�@���@��6@�y>@�
�@���@�^�@��|@���@�1�@��6@�n/@�q@���@��@��,@��6@��H@���@�7L@��@�	@��m@��g@��@���@���@�J#@��@���@�bN@�,=@�4@���@�Z�@�+@���@��<@��4@�Q�@��]@���@�0�@�q@��$@�Q�@�E�@�1�@���@���@�s�@�:�@�+@�@���@�bN@��@��)@���@��S@��@��@���@��1@�|�@�h�@�E�@�!@���@�s@��@���@��5@��@��P@��M@���@���@�>B@��@��j@��-@��P@�F�@��@��]@��$@�I�@�G@�ԕ@���@�g�@���@��r@�?@��@���@�q@���@�;�@��@�@���@��;@��@��@@���@�\�@�9�@���@���@�h
@�M@�@�@�6@�H@�/�@�ϫ@���@�V@���@���@�YK@�-�@��@���@��{@�a@���@��<@���@���@���@�(�@�	@���@���@���@�qv@�O�@�4@��@�ȴ@���@�l"@�Q�@�I�@�{@���@��X@���@��~@�K�@�8@�5�@�@@��2@���@���@�[�@��Q@�x�@�E9@�+@�͟@�4n@��@���@�v`@�#�@��'@�j@�L0@�	@��@>�@�@~҉@~R�@}o @{�V@{P�@{'�@{
=@z��@z�'@z�x@z-@yx�@x��@x`�@w��@w��@wl�@w>�@v��@v�!@vl�@v+k@u�@t�@s�
@s�[@s��@st�@sC�@r��@r+k@qs�@qG�@q%@p�Y@pXy@o�@o;d@n0U@n�@m�@m�~@m \@l��@k�@k�0@kiD@k)_@j�y@j��@j�1@j$�@i�#@i��@i�h@iT�@hq@g��@gW?@f��@f��@f�R@f��@fa|@f�@e�@e��@e�7@e4@d��@d:�@c��@c��@c�f@b�M@b�]@b��@b^5@b�@a��@ahs@a&�@`��@`�@`S�@_�w@_��@_X�@_6z@_/�@_&@_C@_�@_�@_�@_@_ i@^�@^�@]�D@]�>@]@]�=@]s�@\_@[�@Z͟@Zp;@Y�@Y�h@YT�@Y(�@Y�@X��@X��@X��@XH@X �@W��@W�F@WW?@V��@V�@V��@V{�@V-@U�D@U��@U�n@Ux�@U�@TC-@S33@S@R�@R҉@R�b@RTa@Q�T@Q�"@Qx�@Q=�@P�@Pl"@P�@O�}@O�w@O�k@Oj�@O�@NJ�@M��@MY�@M!�@M@@L�p@L��@L~@L�@Kx@K;d@K@J��@J�'@J�b@Jc @JC�@J�@I��@I�X@Ie,@I*0@HFt@G�@G��@G]�@F��@F	@E�#@E��@E?}@E@@D��@D~(@Dj@D,=@C
=@B��@B�s@B��@BJ�@A�@A\�@A-w@A@@@��@@Z@?��@?�f@?v`@?E9@?.I@>�@>�m@>c @>e@>@=�@=�h@=&�@<z�@<2�@;��@:�@:{�@:1�@9��@8�)@8@7�w@7_p@7;d@7�@6�y@6��@6n�@6�@5�o@5�#@5��@5��@5}�@5m]@5S&@5J�@5?}@5&�@4�@4tT@3��@3��@3�:@3J#@2�@2ȴ@2ȴ@2�\@2kQ@2c @2Z�@2\�@2M�@2�@2_@1�H@1��@0��@01'@0%�@/�@/��@/�:@/dZ@.�@-o @-N<@-<6@,�	@,��@,�D@,e�@,!@+��@+�P@+g�@*��@*\�@*3�@*�@*J@)�z@)�t@)��@)��@)c�@)+@(�/@(��@(��@(��@(tT@(Ft@'�r@'��@'��@'j�@'P�@'4�@''�@'�@&�m@&��@&��@&��@&ff@&!�@%��@%��@%?}@%%F@%�@%�@$��@$�o@$I�@$4n@#�@#��@#�q@#��@#�	@#O@#�@"�@"�R@"��@"a|@"&�@"@!��@!�M@!s�@!L�@!@@ U2@��@qv@g�@+@C@�@S@�2@p;@:*@e@�@@�S@�7@m]@Q�@/@�`@oi@>B@�@��@��@Mj@1�@�8@��@L0@�@��@=�@�|@��@PH@6@@�@s�@J�@@��@��@p�@F@%F@�	@��@U2@b@�]@��@�:@s@$t@҉@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BUgBT�BT�BUBU2BU2BUBUBUBUMBT�BT�BT�BU�BU�BV�BYBYBY�BZQBZ�BZ�B[=B[#B[�B\�B\�B\�B\�B]�B^5B_pB_�B`BB`�BbhBc:Bc�BdZBd�Be�BgBgmBh$Bh�Bi_Bj0BkBmBmwBqvB��B��B
��B
�TB
�BB
��B
�'B
��B
��B
�B
�TB
��B
�VB
�TB
��B
��B
�HB
��B
��B
�$B
�PB
�jB
q�B
V�B	��B	��B	�FB	�6B	�\B	�xB	��B	�gB	�DB	��B	�	B	�B	qB	Y�B	D�B	33B	*0B	&�B	 'B	sB		�B	�B��B�B��B�B�:B�:B��B�B�DB�AB	 OB		�B	\B	gB	IB	"�B	+�B	D�B	K)B	L�B	S@B	fLB	m]B	k�B	g�B	fLB	cnB	\B	V�B	X�B	XEB	YKB	Y�B	ZkB	T�B	O�B	MjB	R:B	bhB	jKB	r�B	{B	vzB	v`B	y�B	~�B	��B	��B	��B	�B	�~B	��B	�4B	�gB	��B	��B	��B	�_B	�B	��B	�AB	��B	�B	��B	өB	��B	�+B	�pB	�eB	��B	߾B	�BB	��B	��B	�pB	޸B	�B	�B	�B	��B	��B	�WB	��B	ۦB	�QB	�$B	�YB	��B	��B	��B	ޞB	�B	��B	�dB	�!B	�B	��B	�pB	�;B	��B	�pB	�vB	��B	��B	��B	�@B	�B	�hB	��B	�ZB	��B	�B	�nB	�B	�@B	�B	��B	�>B	��B	�B	��B	�>B	�XB	��B	�LB	�LB	�LB	�RB	�B	��B	�8B	�mB	��B	��B	�B	��B	�4B	�hB	�B	�B	��B	�0B	�KB	�B	�B	��B	��B	�FB	�RB	�
B	�B	�fB	��B	��B	�B	�NB	��B	�B	�B	�B	��B	��B	�B	�6B	�"B	�B	�B	��B	�B	�_B	�B	�0B	�B	�KB	��B	��B	��B	�B	�ZB	��B	�`B	��B	�B	�0B	�*B	�_B	��B	��B	��B	�B	�cB	�DB	��B	�FB	�B	��B	�B	�&B	�B	��B	��B	��B	�$B	�DB	�B	�)B	�[B	�3B	��B	��B	��B	�B	��B	�B	�>B	� B	�OB	��B	�/B	��B	�~B	�5B	�VB	�'B	�B	�B	�B	�|B	��B	�
B	�B	��B	�)B	�B	�CB	��B	�0B	��B	�tB	�B	��B	�fB	�*B	�B	�B	��B	�}B	��B	�B	��B	�*B	��B	�^B	��B	�B	�jB	�B	�jB	�jB	��B	�(B	��B	��B	�cB	�BB	�6B	�0B	�dB	��B	��B	�LB	��B	�B	�ZB	��B	�B	�B	�B	�B	��B	��B	�$B	��B	��B	��B	�xB	�B	�PB	��B	��B	�0B	��B	�B	��B	��B	�B	�PB	�6B	��B	��B	�jB	�jB	��B	�BB	��B	��B	��B	��B	��B
 OB	��B	�<B	��B	��B	�dB	��B	��B	�"B	��B
;B
�B
'B
�B
mB
�B
%B
�B
�B
tB
�B
EB
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	lB
	�B

=B

�B

�B
DB
^B
�B
�B
�B
�B
�B
�B
�B
�B
jB
�B
�B
jB
PB
6B
6B
B
�B
pB
�B
vB
\B
(B
:B
�B
uB
&B
�B
�B
 B
�B
 B
TB
 B
�B
B
�B
@B
�B
uB
uB
&B
@B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
@B
B
&B
&B
&B
FB
2B
�B
B
SB
�B
YB
?B
?B
sB

B

B
+B
yB
�B
YB

B
�B
EB
�B
�B
�B
�B
�B
1B
1B
eB
QB
7B
QB
#B
WB
WB
�B
�B
�B
B
�B
�B
B
�B
dB
dB
dB
�B
�B
OB
B
�B
�B
pB
pB
�B
 B
�B
 vB
 �B
!HB
!�B
#TB
# B
#�B
$�B
%,B
%zB
&B
&�B
&�B
&�B
&�B
&LB
&LB
'mB
(XB
)*B
)_B
)�B
)�B
*eB
+B
+�B
+�B
+�B
,qB
,�B
,�B
,�B
-B
-wB
-�B
-�B
,�B
,�B
,�B
,�B
-CB
.B
.�B
.�B
/iB
/�B
/�B
0oB
0�B
1'B
1�B
1�B
1�B
1�B
1vB
1�B
3hB
3B
33B
3B
3MB
3MB
33B
3�B
49B
4�B
4�B
5B
4�B
5B
5B
5?B
5ZB
5tB
5tB
5�B
6�B
7B
7LB
7LB
72B
7fB
7fB
88B
8�B
8�B
8�B
9>B
9	B
9�B
9�B
;B
:�B
:�B
;JB
;�B
<6B
="B
=<B
=�B
=�B
=�B
=�B
=�B
>(B
>]B
>]B
>]B
>]B
?cB
?cB
?�B
?�B
?�B
?�B
@ B
@4B
@�B
@�B
@�B
@�B
@�B
A B
AUB
AoB
AoB
A�B
BB
A�B
A�B
BAB
B[B
BuB
BuB
BuB
B�B
B�B
B�B
CaB
CGB
C{B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C{B
C{B
C�B
DB
DB
C�B
DB
DB
C�B
D�B
D�B
E�B
E�B
FB
F?B
F?B
FtB
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G+B
G�B
G�B
G�B
G�B
G�B
H1B
HKB
HKB
HKB
HfB
IB
I�B
I�B
I�B
I�B
I�B
J#B
J�B
J�B
J�B
J�B
KB
K^B
K�B
K�B
K�B
K�B
K�B
K�B
L~B
L�B
M6B
M6B
MPB
MjB
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O(B
OB
OBB
O\B
OBB
OvB
O\B
PB
PB
PB
PB
P�B
QNB
Q4B
QhB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
RoB
RoB
RoB
RoB
R�B
SB
S@B
S[B
S�B
S�B
TB
TFB
TaB
TaB
TaB
TaB
T{B
T{B
T�B
T�B
T�B
T�B
T�B
UB
U�B
UgB
VB
VSB
V�B
V�B
W$B
W�B
XyB
X�B
X�B
X�B
Y1B
YKB
ZB
Z7B
Z�B
[	B
[#B
[=B
[WB
[�B
[�B
[�B
[�B
[�B
[�B
\B
\�B
\�B
\�B
\�B
]/B
]/B
]�B
]~B
]�B
^B
^B
]�B
]�B
]�B
]�B
^B
^B
^B
_VB
_VB
_;B
_�B
_�B
_�B
_�B
`'B
a-B
a-B
a-B
abB
abB
a|B
a|B
a�B
b4B
a�B
a�B
bhB
b�B
cB
cTB
c�B
c�B
d&B
d@B
dZB
d�B
d�B
eB
d�B
d�B
eB
e�B
ezB
eFB
e`B
f2B
ffB
f�B
f�B
gRB
gmB
g�B
g�B
g�B
g�B
h
B
hXB
hXB
h�B
iB
iB
i*B
iB
i*B
i�B
i�B
i�B
jB
jB
j0B
jKB
j0B
j�B
j�B
j�B
j�B
kB
k6B
kkB
k�B
k�B
lB
k�B
k�B
lB
l�B
mCB
m]B
m]B
m�B
m�B
m�B
m�B
m�B
nIB
nIB
ncB
ncB
n�B
n�B
n�B
o B
o5B
oB
o�B
o�B
o�B
p!B
p!B
poB
p�B
p�B
p�B
qAB
q[B
qvB
q�B
r-B
r|B
r�B
r�B
r�B
r�B
t9B
t�B
t�B
t�B
u%B
uB
u%B
uZB
u�B
u�B
vB
vFB
vzB
v�B
w2B
wB
wLB
w�B
w�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BUgBT�BT�BUBU2BU2BUBUBUBUMBT�BT�BT�BU�BU�BV�BYBYBY�BZQBZ�BZ�B[=B[#B[�B\�B\�B\�B\�B]�B^5B_pB_�B`BB`�BbhBc:Bc�BdZBd�Be�BgBgmBh$Bh�Bi_Bj0BkBmBmwBqvB��B��B
��B
�TB
�BB
��B
�'B
��B
��B
�B
�TB
��B
�VB
�TB
��B
��B
�HB
��B
��B
�$B
�PB
�jB
q�B
V�B	��B	��B	�FB	�6B	�\B	�xB	��B	�gB	�DB	��B	�	B	�B	qB	Y�B	D�B	33B	*0B	&�B	 'B	sB		�B	�B��B�B��B�B�:B�:B��B�B�DB�AB	 OB		�B	\B	gB	IB	"�B	+�B	D�B	K)B	L�B	S@B	fLB	m]B	k�B	g�B	fLB	cnB	\B	V�B	X�B	XEB	YKB	Y�B	ZkB	T�B	O�B	MjB	R:B	bhB	jKB	r�B	{B	vzB	v`B	y�B	~�B	��B	��B	��B	�B	�~B	��B	�4B	�gB	��B	��B	��B	�_B	�B	��B	�AB	��B	�B	��B	өB	��B	�+B	�pB	�eB	��B	߾B	�BB	��B	��B	�pB	޸B	�B	�B	�B	��B	��B	�WB	��B	ۦB	�QB	�$B	�YB	��B	��B	��B	ޞB	�B	��B	�dB	�!B	�B	��B	�pB	�;B	��B	�pB	�vB	��B	��B	��B	�@B	�B	�hB	��B	�ZB	��B	�B	�nB	�B	�@B	�B	��B	�>B	��B	�B	��B	�>B	�XB	��B	�LB	�LB	�LB	�RB	�B	��B	�8B	�mB	��B	��B	�B	��B	�4B	�hB	�B	�B	��B	�0B	�KB	�B	�B	��B	��B	�FB	�RB	�
B	�B	�fB	��B	��B	�B	�NB	��B	�B	�B	�B	��B	��B	�B	�6B	�"B	�B	�B	��B	�B	�_B	�B	�0B	�B	�KB	��B	��B	��B	�B	�ZB	��B	�`B	��B	�B	�0B	�*B	�_B	��B	��B	��B	�B	�cB	�DB	��B	�FB	�B	��B	�B	�&B	�B	��B	��B	��B	�$B	�DB	�B	�)B	�[B	�3B	��B	��B	��B	�B	��B	�B	�>B	� B	�OB	��B	�/B	��B	�~B	�5B	�VB	�'B	�B	�B	�B	�|B	��B	�
B	�B	��B	�)B	�B	�CB	��B	�0B	��B	�tB	�B	��B	�fB	�*B	�B	�B	��B	�}B	��B	�B	��B	�*B	��B	�^B	��B	�B	�jB	�B	�jB	�jB	��B	�(B	��B	��B	�cB	�BB	�6B	�0B	�dB	��B	��B	�LB	��B	�B	�ZB	��B	�B	�B	�B	�B	��B	��B	�$B	��B	��B	��B	�xB	�B	�PB	��B	��B	�0B	��B	�B	��B	��B	�B	�PB	�6B	��B	��B	�jB	�jB	��B	�BB	��B	��B	��B	��B	��B
 OB	��B	�<B	��B	��B	�dB	��B	��B	�"B	��B
;B
�B
'B
�B
mB
�B
%B
�B
�B
tB
�B
EB
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B
	lB
	�B

=B

�B

�B
DB
^B
�B
�B
�B
�B
�B
�B
�B
�B
jB
�B
�B
jB
PB
6B
6B
B
�B
pB
�B
vB
\B
(B
:B
�B
uB
&B
�B
�B
 B
�B
 B
TB
 B
�B
B
�B
@B
�B
uB
uB
&B
@B
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
@B
B
&B
&B
&B
FB
2B
�B
B
SB
�B
YB
?B
?B
sB

B

B
+B
yB
�B
YB

B
�B
EB
�B
�B
�B
�B
�B
1B
1B
eB
QB
7B
QB
#B
WB
WB
�B
�B
�B
B
�B
�B
B
�B
dB
dB
dB
�B
�B
OB
B
�B
�B
pB
pB
�B
 B
�B
 vB
 �B
!HB
!�B
#TB
# B
#�B
$�B
%,B
%zB
&B
&�B
&�B
&�B
&�B
&LB
&LB
'mB
(XB
)*B
)_B
)�B
)�B
*eB
+B
+�B
+�B
+�B
,qB
,�B
,�B
,�B
-B
-wB
-�B
-�B
,�B
,�B
,�B
,�B
-CB
.B
.�B
.�B
/iB
/�B
/�B
0oB
0�B
1'B
1�B
1�B
1�B
1�B
1vB
1�B
3hB
3B
33B
3B
3MB
3MB
33B
3�B
49B
4�B
4�B
5B
4�B
5B
5B
5?B
5ZB
5tB
5tB
5�B
6�B
7B
7LB
7LB
72B
7fB
7fB
88B
8�B
8�B
8�B
9>B
9	B
9�B
9�B
;B
:�B
:�B
;JB
;�B
<6B
="B
=<B
=�B
=�B
=�B
=�B
=�B
>(B
>]B
>]B
>]B
>]B
?cB
?cB
?�B
?�B
?�B
?�B
@ B
@4B
@�B
@�B
@�B
@�B
@�B
A B
AUB
AoB
AoB
A�B
BB
A�B
A�B
BAB
B[B
BuB
BuB
BuB
B�B
B�B
B�B
CaB
CGB
C{B
C�B
C�B
C�B
C�B
C�B
C�B
C�B
C{B
C{B
C�B
DB
DB
C�B
DB
DB
C�B
D�B
D�B
E�B
E�B
FB
F?B
F?B
FtB
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G+B
G�B
G�B
G�B
G�B
G�B
H1B
HKB
HKB
HKB
HfB
IB
I�B
I�B
I�B
I�B
I�B
J#B
J�B
J�B
J�B
J�B
KB
K^B
K�B
K�B
K�B
K�B
K�B
K�B
L~B
L�B
M6B
M6B
MPB
MjB
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O(B
OB
OBB
O\B
OBB
OvB
O\B
PB
PB
PB
PB
P�B
QNB
Q4B
QhB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
RoB
RoB
RoB
RoB
R�B
SB
S@B
S[B
S�B
S�B
TB
TFB
TaB
TaB
TaB
TaB
T{B
T{B
T�B
T�B
T�B
T�B
T�B
UB
U�B
UgB
VB
VSB
V�B
V�B
W$B
W�B
XyB
X�B
X�B
X�B
Y1B
YKB
ZB
Z7B
Z�B
[	B
[#B
[=B
[WB
[�B
[�B
[�B
[�B
[�B
[�B
\B
\�B
\�B
\�B
\�B
]/B
]/B
]�B
]~B
]�B
^B
^B
]�B
]�B
]�B
]�B
^B
^B
^B
_VB
_VB
_;B
_�B
_�B
_�B
_�B
`'B
a-B
a-B
a-B
abB
abB
a|B
a|B
a�B
b4B
a�B
a�B
bhB
b�B
cB
cTB
c�B
c�B
d&B
d@B
dZB
d�B
d�B
eB
d�B
d�B
eB
e�B
ezB
eFB
e`B
f2B
ffB
f�B
f�B
gRB
gmB
g�B
g�B
g�B
g�B
h
B
hXB
hXB
h�B
iB
iB
i*B
iB
i*B
i�B
i�B
i�B
jB
jB
j0B
jKB
j0B
j�B
j�B
j�B
j�B
kB
k6B
kkB
k�B
k�B
lB
k�B
k�B
lB
l�B
mCB
m]B
m]B
m�B
m�B
m�B
m�B
m�B
nIB
nIB
ncB
ncB
n�B
n�B
n�B
o B
o5B
oB
o�B
o�B
o�B
p!B
p!B
poB
p�B
p�B
p�B
qAB
q[B
qvB
q�B
r-B
r|B
r�B
r�B
r�B
r�B
t9B
t�B
t�B
t�B
u%B
uB
u%B
uZB
u�B
u�B
vB
vFB
vzB
v�B
w2B
wB
wLB
w�B
w�B
w�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104950  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604175132  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604175132  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604175132                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605025139  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605025139  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610141505                      G�O�G�O�G�O�                