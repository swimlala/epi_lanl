CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-02-07T00:35:25Z creation;2018-02-07T00:35:32Z conversion to V3.1;2019-12-19T07:49:58Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I,   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �l   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �T   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ݔ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �$   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �d   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �t   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �x   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180207003525  20200115121518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_207                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�Ju� � 1   @�Jv}'Ҁ@:ˋ�q��d[�g��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZy�D[  D[� D\  D\� D]  D]y�D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D|��D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׃3D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�z�@�G�A ��A ��A@��A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B (�B(�B(�B(�B (�B((�B0(�B8(�B@(�BH(�BP(�BX(�B`(�Bh(�Bp(�Bx(�B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{C 
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
=C^
=C`
=Cb
=Cd
=Cf
=Ch
=Cj
=Cl
=Cn#�Cp
=Cr
=Ct
=Cv
=Cx
=Cz
=C|
=C~
=C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�C�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ|)D[�D[��D\�D\��D]�D]|)D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Du�Du��Dv�Dv��Dw�Dw��Dx�Dx��Dy�Dy��Dz�Dz��D{�D{��D|�D|��D|�)D}��D~�D~��D�D��D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�D{D��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��D�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�D{D��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��{D��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD��D�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHDHD��HD�HD�AHDÁHD��HD�HD�AHDāHD��HD�HD�AHDŁHD��HD�HD�AHDƁHD��HD�HD�AHDǁHD��HD�HD�AHDȁHD��HD�HD�AHDɁHD��HD�HD�AHDʁHD��HD�HD�AHDˁHD��HD�HD�AHD́HD��HD�HD�AHD́HD��HD�HD�AHD΁HD��HD�HD�AHDρHD��HD�HD�AHDЁHD��HD�HD�AHDсHD��HD�HD�AHDҁHD��HD�HD�AHDӁHD��HD�HD�AHDԁHD��HD�HD�AHDՁHD��HD�HD�AHDցHD��HD�HD�AHDׄ{D��HD�HD�AHD؁HD��HD�HD�AHDفHD��HD�HD�AHDځHD��HD�HD�AHDہHD��HD�HD�AHD܁HD��HD�HD�AHD݁HD��HD�HD�AHDށHD��HD�HD�AHD߁HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�D{D�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD�HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�AHD��HD��HD�HD�J�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A��A��A��A��A��A��A��yA��mA��`A��`A��#A�A���A��\A�v�A�K�A��yA�z�A�{A��A�ȴA��FA��FA��RA��!A���A���A��\A�S�A�(�A���A��yA��;A���A�ĜA��wA��wA��-A���A��uA��A�bNA� �A��mA���A�n�A��A�=qA�^5A���A���A�`BA���A��
A��A�A�(�A��9A�hsA�+A���A�"�A�ȴA���A�p�A�ƨA���A�E�A�33A�"�A�oA���A�-A��A��A�v�A���A�9XA�|�A�bNA�33A���A���A�l�A�z�A�
=A�9XA��A��;A��A��#A��A�jA��yA���A�l�AoA}+A{��AyO�Av  Ar��Ar �Aq�Aq;dApz�Ao��An(�AmoAk�FAjr�Ai��Ahz�Ag�-Ad�Ab�9A`��A]�#A[hsA[%AZ��AY`BAW?}AV1'AU��ATJARbNAPAO�AN�`AN�/ANĜAN�RAN��AN  AM/ALbNAK/AJI�AI33AGVAD��AC|�AAXA@ffA?�mA?`BA?VA>��A>n�A>$�A=�A=�
A=��A=�FA=A<�A;�A;+A:M�A9�TA9ƨA9��A9S�A8��A7��A6�+A5�wA4A�A3t�A1�TA0�!A0 �A/�7A/%A-�;A,9XA+C�A*�A*��A)33A(-A'�FA'��A'hsA'%A&M�A%��A%7LA#�mA#VA"z�A!�#A z�AXA��A��A��A9XA�A?}AAz�A�A{A��A?}Az�AQ�A �A��An�A�wA"�AȴAE�A�9A`BAQ�AA
�!A
(�A	A�yA��AC�A�`AjA{A��At�A�A�FAVA�`A�9A�\A�A33A ~�@���@�V@��D@�;d@�@���@��@��@�S�@�33@�@���@��y@��@�@�%@�l�@�-@�X@�D@��@�;d@��@��@��@�A�@�1@��;@�K�@�7L@ۍP@�@ج@�1@�~�@�`B@ԋD@�33@��T@Гu@ϕ�@Χ�@���@�?}@��@��y@�p�@���@�C�@�M�@�?}@��/@�1'@��;@å�@�;d@�^5@�%@��@�Ĝ@��D@�dZ@�=q@�p�@�r�@��
@�t�@�S�@�;d@��@�{@��j@��\@��h@�G�@���@��j@�z�@�  @���@���@�"�@�ȴ@�=q@�z�@���@�p�@�C�@���@���@��j@�9X@���@��^@�bN@�t�@�
=@�^5@���@�`B@��9@�r�@��@��F@���@�-@�{@�@��^@�p�@�/@�%@��@��j@��D@�  @���@��@�33@���@�G�@�z�@�Z@��;@���@���@�v�@�^5@�=q@���@��h@�x�@�G�@�/@��`@�bN@���@�v�@�{@�G�@��j@���@�\)@��@���@���@��+@�n�@��h@�O�@�G�@�hs@�`B@�V@��`@���@��@�z�@�b@�;d@���@�33@���@��;@��@�C�@��@��\@�V@�-@��@��@��-@�`B@��@�V@��j@�I�@��@��w@��F@��@���@�
=@�J@���@�hs@�`B@�X@�G�@���@�1'@��;@��@��R@��\@�v�@�^5@�$�@���@��@��#@���@��-@��h@�X@���@��D@�Q�@�(�@�;@�P@\)@;d@+@�@~��@~5?@~{@}�h@|�j@|9X@|1@{��@{ƨ@{dZ@{33@{33@z��@zn�@y�^@y�@x��@w�;@vv�@v5?@v{@up�@t��@tI�@s�@st�@s��@so@r��@q�#@q��@p�9@pr�@pA�@p1'@pA�@pA�@p1'@p �@p �@p �@p �@p �@pb@o�;@p  @pbN@pQ�@o��@n��@n�+@l�@lj@lZ@lj@lj@l�D@lI�@k�@ko@j�!@jJ@iX@i&�@h��@hb@g�w@gl�@g+@f$�@f$�@fE�@fff@e�-@d��@c��@c��@c��@c��@d�@d�@c�
@co@b��@b~�@b^5@b�@a�#@a��@a�^@a��@a��@a��@a�7@a��@a��@a�^@a�^@ax�@aX@aG�@a&�@`��@`�9@`�@`bN@_�;@_
=@]��@]?}@]V@\��@Z�H@Zn�@Z�@Y�#@Y��@Yhs@Y�@X��@XbN@W�@W�@W�w@Wl�@W\)@WK�@V��@U�@T�/@T�@T�D@Tz�@TZ@TI�@TI�@T(�@T(�@T1@Sƨ@St�@S"�@RJ@Q�^@Q��@Q��@Qx�@Q�@P��@P��@P��@PQ�@O\)@N�y@N��@N$�@M@M`B@M/@MV@L�D@Lj@LZ@L9X@K��@K��@Kt�@K@J�@J��@I�#@I�@H��@HĜ@H��@I%@I�@I�@I�@H��@G�;@G�@G�@G\)@G
=@G�@Fȴ@F�R@F��@F�+@F��@F��@Fv�@F5?@F@E�-@E�h@E�h@Ep�@E�@D�@D��@D�@D�D@DZ@C�
@C�F@C��@CdZ@C"�@C@B�H@B�!@BM�@A�#@AG�@A�@A&�@A�@A%@@�`@@Ĝ@@Ĝ@@�9@@�9@@��@@bN@@A�@?��@?\)@?
=@>��@>�R@>�+@>ff@>V@=��@=?}@=V@<��@<z�@<9X@<1@;dZ@;o@;o@;"�@;o@:�@:�H@:�!@:~�@:-@9�@9��@9x�@9%@8�u@8�@8�@8r�@8r�@8A�@7�@7�@7K�@6$�@5@5O�@5�@4�/@4(�@3ƨ@3dZ@3C�@3"�@333@3o@3o@3@2�@2�@2�H@2��@2��@2��@2n�@1��@1�^@1��@1��@1�7@1hs@1G�@17L@1%@0r�@0 �@/��@/l�@/\)@/\)@/K�@/�@.��@.V@-��@-�h@-�@-�h@-�@-�h@-�h@-�@-p�@-`B@,��@,I�@,�@,1@+��@+��@+�F@+�@+dZ@+S�@+S�@+S�@*�H@*��@*�\@*~�@*M�@*J@)��@)hs@(�`@(�9@(�@(r�@(Q�@( �@(  @'�;@'�@'l�@'�@&ȴ@&��@&��@&��@&v�@&ff@%@%O�@$�/@$��@$�D@$�D@$�D@$z�@$Z@#��@#��@#t�@#o@#@"�@"^5@"�@"J@!��@!�@!x�@!%@ �9@ �u@ Q�@  �@�;@�P@;d@��@�@v�@V@{@�@�j@Z@�@1@�m@S�@@��@�!@~�@~�@n�@n�@J@�#@��@�^@��@X@&�@�9@��@��@bN@b@�@�;@��@��@�@�@
=@��@��@ff@$�@�-@p�@O�@?}@/@�@V@��@�@��@��@�@j@(�@1@1@�m@ƨ@��@t�@C�@��@-@�@��@��@�7@X@&�@%@�`@��@�9@�u@�u@�@r�@A�@�@�w@��@;d@�y@�y@�@�R@V@@�-@��@��@�@p�@p�@�@p�@O�@V@��@�@�@z�@z�@j@Z@I�@9X@(�@�@ƨ@ƨ@��@t�@S�@33@"�@o@
��@
-@	�@	x�@	hs@	G�@	7L@	&�@	%@��@��@�`@�9@�u@A�@b@b@�;@�@|�@K�@+@�y@�R@�R@��@��@ff@V@V@E�@5?@5?@5?@$�@$�@{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A��A��A��A��A��A��A��yA��mA��`A��`A��#A�A���A��\A�v�A�K�A��yA�z�A�{A��A�ȴA��FA��FA��RA��!A���A���A��\A�S�A�(�A���A��yA��;A���A�ĜA��wA��wA��-A���A��uA��A�bNA� �A��mA���A�n�A��A�=qA�^5A���A���A�`BA���A��
A��A�A�(�A��9A�hsA�+A���A�"�A�ȴA���A�p�A�ƨA���A�E�A�33A�"�A�oA���A�-A��A��A�v�A���A�9XA�|�A�bNA�33A���A���A�l�A�z�A�
=A�9XA��A��;A��A��#A��A�jA��yA���A�l�AoA}+A{��AyO�Av  Ar��Ar �Aq�Aq;dApz�Ao��An(�AmoAk�FAjr�Ai��Ahz�Ag�-Ad�Ab�9A`��A]�#A[hsA[%AZ��AY`BAW?}AV1'AU��ATJARbNAPAO�AN�`AN�/ANĜAN�RAN��AN  AM/ALbNAK/AJI�AI33AGVAD��AC|�AAXA@ffA?�mA?`BA?VA>��A>n�A>$�A=�A=�
A=��A=�FA=A<�A;�A;+A:M�A9�TA9ƨA9��A9S�A8��A7��A6�+A5�wA4A�A3t�A1�TA0�!A0 �A/�7A/%A-�;A,9XA+C�A*�A*��A)33A(-A'�FA'��A'hsA'%A&M�A%��A%7LA#�mA#VA"z�A!�#A z�AXA��A��A��A9XA�A?}AAz�A�A{A��A?}Az�AQ�A �A��An�A�wA"�AȴAE�A�9A`BAQ�AA
�!A
(�A	A�yA��AC�A�`AjA{A��At�A�A�FAVA�`A�9A�\A�A33A ~�@���@�V@��D@�;d@�@���@��@��@�S�@�33@�@���@��y@��@�@�%@�l�@�-@�X@�D@��@�;d@��@��@��@�A�@�1@��;@�K�@�7L@ۍP@�@ج@�1@�~�@�`B@ԋD@�33@��T@Гu@ϕ�@Χ�@���@�?}@��@��y@�p�@���@�C�@�M�@�?}@��/@�1'@��;@å�@�;d@�^5@�%@��@�Ĝ@��D@�dZ@�=q@�p�@�r�@��
@�t�@�S�@�;d@��@�{@��j@��\@��h@�G�@���@��j@�z�@�  @���@���@�"�@�ȴ@�=q@�z�@���@�p�@�C�@���@���@��j@�9X@���@��^@�bN@�t�@�
=@�^5@���@�`B@��9@�r�@��@��F@���@�-@�{@�@��^@�p�@�/@�%@��@��j@��D@�  @���@��@�33@���@�G�@�z�@�Z@��;@���@���@�v�@�^5@�=q@���@��h@�x�@�G�@�/@��`@�bN@���@�v�@�{@�G�@��j@���@�\)@��@���@���@��+@�n�@��h@�O�@�G�@�hs@�`B@�V@��`@���@��@�z�@�b@�;d@���@�33@���@��;@��@�C�@��@��\@�V@�-@��@��@��-@�`B@��@�V@��j@�I�@��@��w@��F@��@���@�
=@�J@���@�hs@�`B@�X@�G�@���@�1'@��;@��@��R@��\@�v�@�^5@�$�@���@��@��#@���@��-@��h@�X@���@��D@�Q�@�(�@�;@�P@\)@;d@+@�@~��@~5?@~{@}�h@|�j@|9X@|1@{��@{ƨ@{dZ@{33@{33@z��@zn�@y�^@y�@x��@w�;@vv�@v5?@v{@up�@t��@tI�@s�@st�@s��@so@r��@q�#@q��@p�9@pr�@pA�@p1'@pA�@pA�@p1'@p �@p �@p �@p �@p �@pb@o�;@p  @pbN@pQ�@o��@n��@n�+@l�@lj@lZ@lj@lj@l�D@lI�@k�@ko@j�!@jJ@iX@i&�@h��@hb@g�w@gl�@g+@f$�@f$�@fE�@fff@e�-@d��@c��@c��@c��@c��@d�@d�@c�
@co@b��@b~�@b^5@b�@a�#@a��@a�^@a��@a��@a��@a�7@a��@a��@a�^@a�^@ax�@aX@aG�@a&�@`��@`�9@`�@`bN@_�;@_
=@]��@]?}@]V@\��@Z�H@Zn�@Z�@Y�#@Y��@Yhs@Y�@X��@XbN@W�@W�@W�w@Wl�@W\)@WK�@V��@U�@T�/@T�@T�D@Tz�@TZ@TI�@TI�@T(�@T(�@T1@Sƨ@St�@S"�@RJ@Q�^@Q��@Q��@Qx�@Q�@P��@P��@P��@PQ�@O\)@N�y@N��@N$�@M@M`B@M/@MV@L�D@Lj@LZ@L9X@K��@K��@Kt�@K@J�@J��@I�#@I�@H��@HĜ@H��@I%@I�@I�@I�@H��@G�;@G�@G�@G\)@G
=@G�@Fȴ@F�R@F��@F�+@F��@F��@Fv�@F5?@F@E�-@E�h@E�h@Ep�@E�@D�@D��@D�@D�D@DZ@C�
@C�F@C��@CdZ@C"�@C@B�H@B�!@BM�@A�#@AG�@A�@A&�@A�@A%@@�`@@Ĝ@@Ĝ@@�9@@�9@@��@@bN@@A�@?��@?\)@?
=@>��@>�R@>�+@>ff@>V@=��@=?}@=V@<��@<z�@<9X@<1@;dZ@;o@;o@;"�@;o@:�@:�H@:�!@:~�@:-@9�@9��@9x�@9%@8�u@8�@8�@8r�@8r�@8A�@7�@7�@7K�@6$�@5@5O�@5�@4�/@4(�@3ƨ@3dZ@3C�@3"�@333@3o@3o@3@2�@2�@2�H@2��@2��@2��@2n�@1��@1�^@1��@1��@1�7@1hs@1G�@17L@1%@0r�@0 �@/��@/l�@/\)@/\)@/K�@/�@.��@.V@-��@-�h@-�@-�h@-�@-�h@-�h@-�@-p�@-`B@,��@,I�@,�@,1@+��@+��@+�F@+�@+dZ@+S�@+S�@+S�@*�H@*��@*�\@*~�@*M�@*J@)��@)hs@(�`@(�9@(�@(r�@(Q�@( �@(  @'�;@'�@'l�@'�@&ȴ@&��@&��@&��@&v�@&ff@%@%O�@$�/@$��@$�D@$�D@$�D@$z�@$Z@#��@#��@#t�@#o@#@"�@"^5@"�@"J@!��@!�@!x�@!%@ �9@ �u@ Q�@  �@�;@�P@;d@��@�@v�@V@{@�@�j@Z@�@1@�m@S�@@��@�!@~�@~�@n�@n�@J@�#@��@�^@��@X@&�@�9@��@��@bN@b@�@�;@��@��@�@�@
=@��@��@ff@$�@�-@p�@O�@?}@/@�@V@��@�@��@��@�@j@(�@1@1@�m@ƨ@��@t�@C�@��@-@�@��@��@�7@X@&�@%@�`@��@�9@�u@�u@�@r�@A�@�@�w@��@;d@�y@�y@�@�R@V@@�-@��@��@�@p�@p�@�@p�@O�@V@��@�@�@z�@z�@j@Z@I�@9X@(�@�@ƨ@ƨ@��@t�@S�@33@"�@o@
��@
-@	�@	x�@	hs@	G�@	7L@	&�@	%@��@��@�`@�9@�u@A�@b@b@�;@�@|�@K�@+@�y@�R@�R@��@��@ff@V@V@E�@5?@5?@5?@$�@$�@{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BBBBBBBBBBBBB��B��B��B��B��B��B�B��BBBB+BDBDB	7B1BBB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�mB��B�-B�DBK�B#�B��B�mB��B�B�B�B�B�B�`B��B��B�BjBo�B\)B^5BK�B-B&�BoB�B+B"�BuBB
�sB
�TB
��B
�B
�BB
ɺB
�B
B
��B
�B
�^B
�}B
B
�wB
�FB
�B
��B
��B
v�B
I�B
I�B
H�B
,B
�B
%B
%�B
 �B
 �B
{B
	7B	��B	�B	�sB	�5B	�#B	��B	�qB	��B	��B	�\B	s�B	s�B	�PB	�DB	u�B	_;B	iyB	hsB	S�B	I�B	:^B	M�B	ZB	\)B	[#B	YB	T�B	I�B	=qB	<jB	,B	#�B	�B	B��B��B�B��B	B	  B	B	  B	B	B	  B	B��B��B�B�ZB�B�fB�5B�sB�B�sB�BB�BǮBÖB��B�FB�FB�B�B�XB�-B�!B��B��B��B�B��B��B��B��B��B��B��B�{B�oB�hB�B�B�+B|�Bp�BjBbNBVBbNBo�Bk�Bn�Bl�BdZB\)BQ�B_;B`BBZBaHB_;BW
BK�BN�BP�BN�BH�B6FB6FB<jB@�B:^B=qB?}B49B.B=qB:^B9XB:^B<jB8RB2-B+B.B:^B7LB5?B-B&�B'�B#�B$�B�B!�B!�B%�B�BoB�B/B/B0!B.B+B$�B�B��B�B'�B'�B$�B$�B �B!�B%�B%�B$�B"�B�B	7B�B\B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B �B(�B&�B)�B)�B'�B$�B"�B0!B/B+B#�B&�B+B,B2-B5?B7LB6FB33B,B'�B(�B49B?}B>wB@�B@�B>wBA�B@�B>wB>wB9XB33B1'B8RB5?B>wBK�BP�BL�BD�BL�BL�BYBaHBaHBe`BffBhsBn�Bn�Bn�Bl�Bv�B{�B{�Bz�B{�B}�B~�B�B� B~�B}�B�B�B� B� B�B�=B�{B�oB�oB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�?B�XB�jB��B�}B�}B�jBŢB��B��B��B��B��B�
B�B��B��B��B�B�)B�mB�B�B�B�B�B��B��B��B��B	  B	B	%B	1B	+B	
=B	bB	�B	�B	�B	�B	{B	�B	�B	$�B	&�B	'�B	%�B	!�B	�B	 �B	�B	%�B	)�B	+B	,B	+B	-B	0!B	0!B	0!B	0!B	1'B	2-B	33B	6FB	;dB	=qB	>wB	A�B	C�B	D�B	E�B	E�B	C�B	F�B	I�B	I�B	K�B	O�B	S�B	S�B	S�B	S�B	VB	W
B	T�B	XB	W
B	YB	\)B	]/B	^5B	hsB	jB	iyB	k�B	p�B	p�B	u�B	w�B	v�B	w�B	y�B	� B	� B	�%B	�+B	�1B	�7B	�7B	�=B	�=B	�=B	�=B	�=B	�=B	�=B	�DB	�VB	�uB	�oB	�bB	�\B	�bB	�PB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�LB	�RB	�?B	�9B	�LB	�jB	�wB	�wB	�wB	�wB	�wB	��B	ÖB	ƨB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�
B	�B	�B	�B	�B	�
B	�B	�)B	�5B	�)B	�B	�HB	�ZB	�ZB	�ZB	�ZB	�TB	�TB	�`B	�fB	�sB	�mB	�mB	�sB	�mB	�`B	�NB	�mB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B

=B
JB
JB
JB
DB

=B
	7B
1B

=B
DB
DB

=B
PB
JB
PB
VB
VB
bB
bB
\B
\B
bB
\B
hB
oB
hB
hB
hB
oB
oB
oB
oB
hB
uB
{B
uB
uB
�B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
"�B
"�B
"�B
"�B
!�B
!�B
 �B
!�B
!�B
!�B
!�B
"�B
$�B
%�B
$�B
$�B
#�B
#�B
#�B
#�B
!�B
$�B
&�B
'�B
'�B
%�B
'�B
(�B
+B
,B
-B
-B
.B
.B
.B
.B
.B
.B
.B
/B
.B
-B
/B
1'B
1'B
1'B
1'B
1'B
1'B
0!B
/B
0!B
2-B
2-B
49B
5?B
49B
33B
33B
2-B
33B
6FB
8RB
8RB
8RB
8RB
8RB
8RB
7LB
7LB
5?B
6FB
8RB
9XB
9XB
9XB
8RB
9XB
:^B
:^B
:^B
:^B
9XB
:^B
<jB
;dB
:^B
:^B
:^B
;dB
;dB
=qB
=qB
>wB
=qB
=qB
>wB
>wB
>wB
>wB
>wB
?}B
@�B
@�B
@�B
@�B
?}B
=qB
>wB
@�B
A�B
C�B
C�B
C�B
C�B
B�B
A�B
B�B
C�B
B�B
C�B
C�B
B�B
D�B
E�B
E�B
D�B
C�B
C�B
E�B
F�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
F�B
G�B
G�B
E�B
E�B
I�B
I�B
K�B
J�B
I�B
J�B
L�B
M�B
M�B
N�B
N�B
M�B
L�B
M�B
N�B
O�B
N�B
M�B
M�B
N�B
P�B
P�B
O�B
O�B
P�B
Q�B
P�B
P�B
O�B
R�B
R�B
R�B
Q�B
Q�B
R�B
Q�B
S�B
T�B
VB
VB
VB
VB
VB
VB
VB
VB
VB
T�B
VB
W
B
XB
W
B
W
B
W
B
VB
VB
T�B
T�B
YB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
\)B
\)B
\)B
\)B
[#B
ZB
\)B
\)B
[#B
\)B
^5B
^5B
]/B
\)B
]/B
^5B
_;B
`BB
_;B
`BB
aHB
`BB
_;B
_;B
_;B
`BB
`BB
`BB
`BB
bNB
bNB
bNB
bNB
aHB
bNB
bNB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
bNB
aHB
aHB
cTB
cTB
e`B
e`B
e`B
ffB
e`B
ffB
ffB
e`B
e`B
e`B
e`B
ffB
gmB
ffB
ffB
ffB
gmB
gmB
gmB
hsB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
jB
jB
jB
k�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BBBBBBBB-BB�BBB�.B�.B�BB�<B�^B��B��B��B[BSB9B+BDB^B	RBKBSB�B�DB�>B�B��B�B�B��B��B��B�"B�B�"B�JB�lB�RB�`B�hB�B�B��B�BO�B'�BB�B��B�MB��B�B�UB�CB�B��B�yB�Bm�BqAB^�B_VBNVB/�B)_BMB�B+QB#nB�B�B
��B
��B
��B
�GB
��B
��B
��B
�B
�>B
��B
��B
��B
�uB
��B
��B
��B
�-B
��B
y�B
M�B
LB
J�B
/iB
7B
	�B
&fB
!�B
!HB
�B

rB	��B	�3B	��B	߾B	�CB	�<B	�.B	��B	��B	�B	wB	vB	��B	��B	w�B	a�B	j�B	iyB	V9B	K�B	<�B	N�B	ZQB	\CB	[WB	YKB	U2B	J�B	>�B	=�B	-�B	%FB	;B	�B�qB��B�-B��B	�B	 �B	�B	 �B	[B	oB	 OB	;B�B�*B�B�B��B�mB�VB��B��B��B��B�$B�7B�B��B�B��B�B��B�*B�B�B��B��B��B��B��B�xB��B�ZB��B�:B�dB��B�[B�:B��B�3B��B~(Br�Bl=Bd�BX�Bc�Bp!BlqBo BmBe`B]�BS�B_�B`�B[#Ba�B_�BW�BMjBO�BQ�BO�BI�B8lB8B=�BAoB;�B>BB@OB5�B/�B=�B:�B9�B:�B<�B9	B33B,qB/ B:�B7�B5�B.B($B(�B$�B%�B�B"�B"�B&�B�BFB�B/5B/OB0;B.IB+QB%�BB �B�B(XB(�B%zB%zB!�B"hB&2B&2B%,B#:BeBB�'BbB_B=B�B_BWB�BmB�BkBWBWB5B_BsB�B5B�B"hB!�B)DB'RB*0B*KB(sB%�B#�B0UB/iB+kB$�B'�B+�B,�B2�B5�B7fB6`B3�B,�B)B*eB4�B?�B>�B@�B@�B>�BA�B@�B>�B>�B:B4nB2|B9XB6�B?}BLJBQ4BMjBE�BM�BM�BY�Ba�Ba�Be�Bf�Bh�Bn�Bo BoBm]BwB|B|B{B|6B~(B.B�;B�BHB~]B�AB�-B��B��B��B��B��B��B�B��B��B��B��B�B�B��B��B�B�&B�4B�bB��B�>B��B��B��B��B��B��B��B��B��B��B��B̳B��B��B�4B�B�$B�9B�MB�@B�hB�B�B�B�B��B��B��B��B��B�B�B�"B	 4B	aB	YB	fB	_B	
�B	�B	�B	�B	�B	�B	�B	$B	�B	$�B	'B	(
B	%�B	"4B	 BB	!B	 BB	&2B	*B	+B	,"B	+B	-)B	0;B	0!B	0!B	0!B	1[B	2aB	3�B	6�B	;�B	=�B	>�B	A�B	C�B	D�B	E�B	E�B	C�B	F�B	I�B	I�B	K�B	O�B	S�B	TB	S�B	T,B	VB	W$B	UB	XEB	WYB	YKB	\CB	]�B	^�B	h�B	j�B	i�B	k�B	p�B	p�B	u�B	w�B	v�B	xB	z*B	� B	�OB	�?B	�+B	�1B	�7B	�7B	�=B	�=B	�=B	�=B	�=B	�=B	�=B	�DB	�VB	�@B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	� B	�B	�B	�6B	�CB	�qB	�B	�LB	�lB	�tB	��B	��B	�PB	�wB	�]B	�wB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�
B	�+B	�B	�$B	�B	�1B	�1B	�_B	�YB	�mB	�]B	�OB	�]B	ٚB	�bB	�ZB	�tB	�tB	�tB	�B	�B	�`B	�B	�sB	�B	�B	�B	�B	�B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	� B	��B	��B	��B	��B	�B	��B	��B	�B	�B	�$B	�B	�B	�(B	�.B
B
B
-B
'B
9B
9B
9B
3B
-B
3B
-B
B
B
;B
AB
?B

=B
JB
0B
JB
)B

XB
	lB
fB

XB
)B
^B

XB
PB
JB
PB
VB
VB
HB
HB
vB
vB
}B
�B
�B
TB
hB
�B
�B
�B
�B
�B
�B
�B
uB
{B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
"�B
"�B
"�B
"�B
"�B
!�B
!�B
 �B
!�B
!�B
!�B
!�B
"�B
$�B
%�B
$�B
$�B
#�B
#�B
#�B
$B
"B
%B
'B
'�B
(
B
&B
(
B
)B
+B
,B
,�B
-)B
.B
.B
-�B
.B
.B
./B
./B
/ B
./B
-CB
/5B
1'B
1'B
1AB
1AB
1'B
1AB
0;B
/5B
0;B
2GB
2GB
49B
5?B
4TB
3MB
3MB
2aB
3hB
6`B
8RB
8RB
8RB
8RB
8RB
8RB
7LB
7fB
5tB
6zB
8RB
9XB
9>B
9XB
8lB
9XB
:xB
:DB
:^B
:xB
9�B
:xB
<PB
;B
:xB
:xB
:�B
;dB
;B
=qB
=qB
>wB
=�B
=�B
>�B
>�B
>wB
>wB
>�B
?}B
@iB
@�B
@�B
@�B
?}B
=�B
>�B
@�B
A�B
C�B
C{B
C�B
C�B
B�B
A�B
B�B
C�B
B�B
C{B
C�B
B�B
D�B
E�B
E�B
D�B
C�B
C�B
E�B
F�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
F�B
G�B
G�B
E�B
E�B
I�B
I�B
K�B
J�B
I�B
J�B
L�B
M�B
M�B
N�B
N�B
M�B
L�B
M�B
N�B
O�B
N�B
M�B
M�B
N�B
P�B
P�B
O�B
O�B
Q B
Q�B
Q B
Q B
O�B
R�B
R�B
SB
RB
RB
R�B
RB
TB
T�B
U�B
VB
VB
VB
VB
U�B
VB
VB
VB
T�B
VB
W$B
XB
W$B
W$B
W
B
VB
VB
U2B
U2B
Y1B
Z7B
Z7B
ZB
Z7B
Z7B
[#B
[=B
[#B
[=B
\)B
\)B
\)B
\)B
[#B
Z7B
\CB
\CB
[WB
\CB
^5B
^OB
]IB
\CB
]/B
^OB
_;B
`'B
_;B
`BB
aHB
`BB
_;B
_VB
_;B
`BB
`\B
`\B
`\B
bNB
bNB
bNB
bNB
aHB
bNB
bNB
aHB
b4B
bhB
bhB
bhB
cnB
cnB
bNB
a|B
a|B
cnB
cnB
e`B
e`B
e`B
fLB
ezB
fLB
ffB
ezB
ezB
ezB
ezB
ffB
gRB
ffB
ffB
ffB
gmB
gmB
g�B
hsB
i_B
iyB
iyB
i�B
iyB
jB
jB
jB
jeB
jeB
jeB
jB
jB
kk111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.04(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201802110032472018021100324720180211003247201806221237232018062212372320180622123723201804050434062018040504340620180405043406  JA  ARFMdecpA19c                                                                20180207093519  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180207003525  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180207003529  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180207003529  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180207003530  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180207003530  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180207003531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180207003531  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20180207003532  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180207003532                      G�O�G�O�G�O�                JA  ARUP                                                                        20180207005637                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180207154042  CV  JULD            G�O�G�O�F�S�                JM  ARCAJMQC2.0                                                                 20180210153247  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180210153247  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193406  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033723  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121518                      G�O�G�O�G�O�                