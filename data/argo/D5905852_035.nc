CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-01-19T09:38:34Z creation;2020-01-19T09:38:36Z conversion to V3.1;2022-08-02T05:11:32Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         BPRIMARY|https://orcid.org/0000-0001-9150-6442|Kanako Sato, JAMSTEC        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7,   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7<   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7@   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7D   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7T   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7d   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7t   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7|   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8,   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    80   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    84   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     88   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8X   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8\   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8`   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        :    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  :   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]l   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a\   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  u   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  τ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �P   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �T   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �X   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �\   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �`   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200119093834  20220818091504  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               #A   JA  A30_8420_035                    2C  D   APEX                            8420                            2.11.2                          846 @������1   @��n�@-�6��C�c �-1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�ffB���B���B�  B���B���B�  B�  B�33BǙ�B˙�B�  B�  B�  B�  B�33B�33B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C�C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�3D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�@~�R@��A   A�
A?�A_�A�  A�(�A�(�A�(�A�  A�A�{A�Q�B �B  B�HB  B (�B(  B0  B8
=B@=qBH��BP�BX  B_��Bh  Bo��Bx  B�
=B�
=B�\B�
=B���B�B�
=B��B�W
B�k�B��\B��qB�\B��B�ǮB�B��B�33BǸRB˸RB�  B���B���B�
=B�33B�G�B��)B�  B�B���B�  B��B���C�RC��C�qC�qC	�RC�C
C)C�RC  C  C  C�qC�qC  C   C!�qC#�qC&C(
=C*�C,�C.  C0  C2�C4  C6  C8�C:C<�C=��C@�CB�CD�CE��CH�CJCL  CM�qCPCR�CT�CVCX�CZ�C\�C^  C`�CbCd�Cf�Ch�Cj�Cl
=CnCpCr�Ct�Cv�Cx�Cz  C|C~�C�  C�HC�  C�HC���C�HC��C�  C�  C�C�fC��C�HC�  C��C��C�fC��C��C��C�HC��C�fC���C��qC���C�HC��C�HC��C�C���C�HC�HC���C�  C�HC�HC�  C��C��C��C�HC�HC��C���C�  C�  C��qC�HC�HC�  C�C��C�C�fC��C��C�HC�HC��C�HC���C��qC�  C�  C���C��C�HC���C�  C���C���C��C�  C���C��C�C���C��qC���C�  C�C�  C��)C�  C���C���C�  C��C�C��C��C�HC���C�HC��C�HC�HC�HC��C�C�  C���C��qC�HC��C�fC��C�HC�C�HC��)C�HC��C��C��C�HC��C��C�HC�  C�HC�  C�  C���C���C���D  �D ��D�D��D�D��D �D��DHD�HD�D��D�D��D�D� D �D��D	�D	��D
�D
��D�D��D�D� DHD~�D�D\D  D� DHD�HD��D~D�\D�HDHD��D  D��DHD�3D3D��D  D\D�\D\D �D��DHD\DHD\D�D��D�\D~DHD�HDHD�HD HD �HD! �D!��D"�D"� D#�D#��D#��D$�HD% �D%~�D%�\D&� D' �D'\D(  D(�HD)�D)��D*�D*��D*��D+~D+�\D,�HD-�D-��D. �D.~�D.��D/\D/�\D0\D1 �D1� D2  D2�HD3HD3��D4  D4\D5HD5�HD6 �D6�HD7�D7��D8HD8�HD9  D9� D:�D:�HD;�D;��D<HD<\D=HD=��D>�D>� D>�D?\D?�\D@� DA �DA� DB  DB��DC�DC\DC��DD~�DE  DE� DE�\DF� DGHDG��DG�DH~DI  DI��DJ �DJ��DKHDK~�DK�qDL\DM�DM��DM�\DN��DO�DO�3DP�DP�HDQ  DQ��DR  DR� DS  DS��DT�DT��DU �DU~�DV  DV�HDW  DW��DX  DX� DY�DY��DZ�DZ�HD[  D[��D\HD\� D]HD]�3D^HD^� D_�D_��D`  D`� Da �Da� Da�\Db��Dc�Dc��Dd�Dd��DeHDe�HDf�Df� Dg �Dg��Dg�\Dh�HDi �Di�HDj3Dj��Dk �Dk��Dl�Dl\Dm  Dm� Dm��Dn��Do  Do��Dp�Dp��Dq�Dq�HDr  Dr~Ds  Ds��DtHDt� Dt�Du}qDv �Dv�3Dw3Dw��Dw�Dx\DyHDy� Dy�\Dz�HD{  D{~�D| �D|� D} �D}�HD~HD~��D  D��D� �D�@ D���D��RD�  D�?�D�\D��RD��D�AHD���D�� D�  D�@RD�� D���D� RD�@�D�� D���D�  D�?�D�� D�� D���D�?\D�� D���D� RD�?\D��RD��RD�  D�@RD��D��\D�  D�@RD�� D���D�  D�@�D��D�� D� �D�@�D�� D���D��D�@�D���D�� D��\D�?
D�\D�� D� �D�@�D��D�� D� �D�@ D�
D��RD��D�AHD��RD��
D��\D�@RD��HD���D���D�?�D��D��RD� RD�?�D��RD���D� RD�@�D���D�� D���D�@ D���D��HD��D�@�D���D���D� �D�@�D�
D��RD�HD�@�D�� D��\D�  D�@�D���D���D�HD�@ D�\D���D� RD�?�D��RD���D��\D�AHD���D��HD� �D�@ D��RD���D� �D�@�D��HD���D� RD�@ D���D���D� RD�?\D�~�D�� D� �D�@�D��RD���D� RD�@ D��RD��RD���D�?\D�
D��\D� �D�@�D���D��RD� �D�AHD��RD�� D� RD�@�D���D��\D��\D�?\D��RD�� D��\D�?
D�\D���D� �D�B�D��=D��RD���D�@ D��RD�� D�  D�@ D��RD��HD� RD�@ D���D�� D��\D�@ D�� D���D���D�@RD��RD�� D� �D�@�D���D���D���D�?�D��RD���D���D�@RD���D���D�HD�@�D���D���D�  D�@ D�� D���D� �D�@ D��RD��HD� �D�@�D��HD���D��D�@�D��D��RD� �D�?�D�
D�� D� �D�@RD��D�� D� �D�@ D�
D��RD� �D�@ D��D�� D�HD�@�D���D��HD� �D�?�D��RD���D� RD�?
D�\D�� D�  D�@�D��RD�� D���D�?�D�� D��RD�HD�@�D��RD���D�HD�@�D���D���D���D�@RD�D���D���D�?�DÀ D�� D��
D�@ DĀ�D���D� �D�@�Dŀ�D���D� �D�@�Dƀ�D�� D� RD�@�Dǀ�D�� D��
D�?
DȀ D�� D��\D�@ Dɀ D��RD� RD�?
Dʀ Dʿ�D� RD�AHDˁHD���D���D�@RD̀RD�� D� �D�?�D�
D��RD� �D�@�D΀�D��RD� RD�?�D��D��RD��\D�?
D�\D�� D� RD�@ D��D�� D��
D�@ DҁHD�� D��\D�@�DӀ�D��RD� RD�@RDԀ�D���D� �D�@�DՁHD��RD��\D�@�Dր�D���D��D�@RD�\D�� D�HD�@�D؀�D���D�=D�AHD��D�� D�HD�@�D��Dڿ�D�  D�@ Dۀ�D���D���D�?\D܀RD��HD� �D�@�D݀RD���D� �D�@RDހ�D���D� �D�@�D߀�D߿�D��\D�?�D�� D�\D��\D�@ D��D�� D� �D�@RD�RD���D� �D�@ D�\D�\D���D�@ D�
D修D� �D�@ D��D���D� �D�@�D�\D��RD�HD�@�D瀤D���D��\D�@RD�HD���D�  D�?�D逤D�� D�  D�AHD�HD��HD��D�@�D��D�
D�  D�@�D� D쿮D�  D�@�D� D��
D���D�@RDD���D�HD�@RD� D￮D��
D�@�D��RD�D� �D�@RD� D��HD�HD�AHD�RD�D�  D�@ D�RD���D��D�@�D�RD��RD���D�@�D��=D���D��\D�@ D���D��HD��D�@�D�� D���D���D�?�D�� D���D�HD�A�D���D��\D� �D�A�D��HD���D� �D�@�D���D��q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A�YA�1A�1A�7A�A��A��A�A�A��A��A� 'A�!-A�"4A�"�A�#�A�#nA�%A�%FA�#:A�OA��A�+A��A�oA��A��`A��|A���A��A���A��A��A��(A��cA� �A��A�VA��A��A̍�A�oA���A�iDA���A��A���A�Z�A��A��?A�m�A�_A���A��A�o�A�qAA��A��zA��2A�{JA���A���A��Awo�Aql"Ak4Ad��AbF�Aa($A^s�AX�AT�ARԕAQD�AP9XAL�EAH�LAFAD�AABU�AAw�A@�rA?�}A=��A<�A9a|A6�:A4'RA4�A3kQA/�>A/5?A.�A.��A-�A+��A*ݘA(�vA&�\A%��A%1A#{JA"	lA!xA �A�A\)A4A�AN�A��A�MA��A)_AOvAA AQA�	A�FA��A��A�_A@OA��A�[A!-A�?A_pA��A�A��AF�A�AV�AC�A�A�IA�)Aw2A�A�A��A�A�gA�A�4A-wA�yA\�ARTA��A��A�4AAl�A�$A;dA��A��AYKA��A5?A��A
�MA
7LA�A6A��AjA��A\)A>BAg�A�,AHA\�A�A�SA"hA ��A �A qv@��j@���@���@��@�.�@���@� �@��9@��=@�A�@���@�M@���@�;d@�\�@�~�@���@��x@�P�@��@�k�@��@�A @�u@��}@�hs@�L@�l�@���@��@��@詓@��@�~�@��@繌@���@���@�@�w�@幌@��m@㋬@��@�V�@�˒@�u�@��@��@�b@��@ޝI@��@ݑh@�,�@��?@܊r@�2�@���@۬q@��@ڮ}@ڀ�@�]d@�L0@�4@�9�@؜x@���@י�@�-w@�q�@���@դ@@�x@���@�-@�6�@ӈf@���@�v�@�  @ђ:@��@Е�@Ѓ@�YK@�1@ϕ�@�@�~�@��@͘�@�A @��v@̘_@�GE@��@˖S@�B�@���@ʎ�@��@ɮ�@�C�@��`@ȷ�@�~(@�_�@�?@�.�@���@�>�@Ƴh@�c @�@ŗ$@�\)@�,�@��@Ĥ�@�B[@��@�N<@��P@��,@¸R@@�q@�4n@��@���@�+@���@�:*@��@�o�@��@�֡@���@�W�@��@�l�@���@�3�@��}@��q@���@�;@��\@�PH@��>@�dZ@�1�@�n�@�!�@���@�p�@�"�@��!@��@�s@���@�j@� �@��@�=@��_@�a|@�@�@��.@��@�hs@�%F@��@�V@���@��I@��*@�(�@��s@�xl@��@�#�@��"@��/@���@�M�@��K@�iD@�=�@�%F@��@�͟@�e�@�/�@�M@��@�}�@���@���@�zx@��@�s�@��@��@��@���@��}@�O�@�%@��m@�_�@�&�@���@�2a@���@��@���@�Q@��@���@�8@��@���@�4n@�O@�b@��]@��Q@�|�@�\�@�F@�4�@��@��Y@�>B@��@���@�=�@��}@��Y@�[�@�0U@��@���@��8@�m�@�,=@�@�T�@��@���@�6@�1�@��>@�w2@���@���@�d�@�7@���@��@��4@�A�@�o@���@��_@�_�@��@�e�@�5�@�'�@��@��@��f@��]@��@��.@�N�@�%�@�
�@��a@��h@���@�}�@�X@��@��5@�͟@�W�@�G@��^@�}�@�hs@���@�a@���@��p@��!@���@��@�K�@��$@�@��[@���@�@@���@��@���@�dZ@�T�@�1�@��X@��@��*@�a@�@��v@��,@��,@��@��K@��@�S�@��@�e@��&@���@��7@�iD@�X�@�H�@�&�@��@� �@���@���@�9�@��@��@���@��M@�?}@���@��_@�]d@���@��3@�}�@�?}@�@���@��I@�q@�PH@�(�@��@��@���@��n@�*0@��]@���@��@�_�@�W@�@~��@~3�@~�@~u@}�Z@}�@}�@}��@}�H@}�=@}<6@|�K@|��@{�;@{/�@z��@zYK@y��@yS&@y%@x�?@x�@wl�@w;d@w�@v�8@v�X@vGE@u��@u��@t�v@tPH@s�+@sx@s�@r�}@r?@q�@q�S@qO�@pĜ@pe�@o��@o6z@nߤ@n�x@nM�@ne@m��@m \@l�@l4n@k�@j��@ju@i�H@iIR@h��@h��@h`�@g�@g$t@f�L@fn�@f6�@e�@e�=@e��@ej@d��@d�@d��@d2�@c�@@cW?@c@O@b�@b+k@b&�@bO@a�@aA @`e�@_��@_s@^+k@]q@\�e@\?�@\�@[��@[e�@[�@Z��@Z\�@Z_@Y��@YT�@Y@Xѷ@Xu�@W�@W��@W1�@V�B@VL0@V�@U��@U;@T��@TbN@TM@S�K@Sqv@S,�@R�@R��@R��@Q��@P��@P�o@P>B@P  @O�
@OdZ@N��@Nu%@N6�@M�H@Mf�@M!�@M�@L�`@L�$@Lh�@L?�@L"h@K�W@Ky�@K(@J�2@J�m@J��@J
�@I��@I^�@I�@H~(@G�@G�q@G��@G8@Go@F�2@Fں@F�X@F��@Fxl@F^5@E��@E�@E�"@Ehs@ES&@EDg@E-w@E@@D�@D�$@D�D@DH@C�@CRT@C=@B��@B��@BW�@BH�@A�o@A�'@A/@A	l@@�?@@��@@�Y@@u�@@`�@@"h@?�@?�	@>��@>z@>=q@=�)@=x�@<��@<�9@<�@<-�@;��@;s@;x@;E9@:�@:q�@9Vm@9 \@9�@9@9�@8��@8�$@8tT@7�a@7S�@6��@6~�@6�@5��@5!�@4�_@4g8@3�&@3\)@2��@1�d@1#�@0�@/�@/��@/y�@/RT@/;d@/S@.�@.E�@.@-�@-�h@-B�@,��@,��@,h�@+�r@+�@@+.I@*�M@*ȴ@*�\@*+k@)�D@)�@)u�@)B�@)&�@)%@(�@(֡@(�@(�@([�@($@(	�@'�@'��@'S�@'@&�X@&}V@&#:@%�9@%��@%�~@%A @$�	@$��@$z�@$1'@#�@#�@#�k@"�2@"�b@"W�@"0U@"�@!��@!o @!F@!<6@!(�@ �P@ �?@ �9@ ��@ ��@ 1'@�q@t�@=@)_@�@ں@�@^5@:*@	@�9@�C@Y�@2a@�@�@�|@�U@�I@Z@$@�@��@�}@t�@iD@H�@�@��@҉@��@��@Q@8�@��@�@}�@?}@�@	l@	l@��@�v@��@��@[�@@x@�+@�g@_p@.I@�8@͟@�x@d�@.�@��@�T@�@��@��@�M@\�@A @&�@+@V@	l@�@�p@�p@�p@Ɇ@�U@��@g8@Q�@C-@�+@�*@�4@4�@C@��@�<@�b@�+@~�@{�@@�@ �@�@�@��@��@X@:�@ \@��@�p@�@~(@u�@j@M@2�@~@�@�@��@�w@�P@,�@�H@��@Z�@B[@ �@�T@��@��@�-@�t@�S@m]@Dg@(�@%@��@�E@�I@q@�]@�@��@�@��@�@U�@
=@
�@
ں@
��@
n�@
_�@
GE@
)�@
�@	�D@	��@	�M@	Vm@	(�@��@�|@֡11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A�YA�1A�1A�7A�A��A��A�A�A��A��A� 'A�!-A�"4A�"�A�#�A�#nA�%A�%FA�#:A�OA��A�+A��A�oA��A��`A��|A���A��A���A��A��A��(A��cA� �A��A�VA��A��A̍�A�oA���A�iDA���A��A���A�Z�A��A��?A�m�A�_A���A��A�o�A�qAA��A��zA��2A�{JA���A���A��Awo�Aql"Ak4Ad��AbF�Aa($A^s�AX�AT�ARԕAQD�AP9XAL�EAH�LAFAD�AABU�AAw�A@�rA?�}A=��A<�A9a|A6�:A4'RA4�A3kQA/�>A/5?A.�A.��A-�A+��A*ݘA(�vA&�\A%��A%1A#{JA"	lA!xA �A�A\)A4A�AN�A��A�MA��A)_AOvAA AQA�	A�FA��A��A�_A@OA��A�[A!-A�?A_pA��A�A��AF�A�AV�AC�A�A�IA�)Aw2A�A�A��A�A�gA�A�4A-wA�yA\�ARTA��A��A�4AAl�A�$A;dA��A��AYKA��A5?A��A
�MA
7LA�A6A��AjA��A\)A>BAg�A�,AHA\�A�A�SA"hA ��A �A qv@��j@���@���@��@�.�@���@� �@��9@��=@�A�@���@�M@���@�;d@�\�@�~�@���@��x@�P�@��@�k�@��@�A @�u@��}@�hs@�L@�l�@���@��@��@詓@��@�~�@��@繌@���@���@�@�w�@幌@��m@㋬@��@�V�@�˒@�u�@��@��@�b@��@ޝI@��@ݑh@�,�@��?@܊r@�2�@���@۬q@��@ڮ}@ڀ�@�]d@�L0@�4@�9�@؜x@���@י�@�-w@�q�@���@դ@@�x@���@�-@�6�@ӈf@���@�v�@�  @ђ:@��@Е�@Ѓ@�YK@�1@ϕ�@�@�~�@��@͘�@�A @��v@̘_@�GE@��@˖S@�B�@���@ʎ�@��@ɮ�@�C�@��`@ȷ�@�~(@�_�@�?@�.�@���@�>�@Ƴh@�c @�@ŗ$@�\)@�,�@��@Ĥ�@�B[@��@�N<@��P@��,@¸R@@�q@�4n@��@���@�+@���@�:*@��@�o�@��@�֡@���@�W�@��@�l�@���@�3�@��}@��q@���@�;@��\@�PH@��>@�dZ@�1�@�n�@�!�@���@�p�@�"�@��!@��@�s@���@�j@� �@��@�=@��_@�a|@�@�@��.@��@�hs@�%F@��@�V@���@��I@��*@�(�@��s@�xl@��@�#�@��"@��/@���@�M�@��K@�iD@�=�@�%F@��@�͟@�e�@�/�@�M@��@�}�@���@���@�zx@��@�s�@��@��@��@���@��}@�O�@�%@��m@�_�@�&�@���@�2a@���@��@���@�Q@��@���@�8@��@���@�4n@�O@�b@��]@��Q@�|�@�\�@�F@�4�@��@��Y@�>B@��@���@�=�@��}@��Y@�[�@�0U@��@���@��8@�m�@�,=@�@�T�@��@���@�6@�1�@��>@�w2@���@���@�d�@�7@���@��@��4@�A�@�o@���@��_@�_�@��@�e�@�5�@�'�@��@��@��f@��]@��@��.@�N�@�%�@�
�@��a@��h@���@�}�@�X@��@��5@�͟@�W�@�G@��^@�}�@�hs@���@�a@���@��p@��!@���@��@�K�@��$@�@��[@���@�@@���@��@���@�dZ@�T�@�1�@��X@��@��*@�a@�@��v@��,@��,@��@��K@��@�S�@��@�e@��&@���@��7@�iD@�X�@�H�@�&�@��@� �@���@���@�9�@��@��@���@��M@�?}@���@��_@�]d@���@��3@�}�@�?}@�@���@��I@�q@�PH@�(�@��@��@���@��n@�*0@��]@���@��@�_�@�W@�@~��@~3�@~�@~u@}�Z@}�@}�@}��@}�H@}�=@}<6@|�K@|��@{�;@{/�@z��@zYK@y��@yS&@y%@x�?@x�@wl�@w;d@w�@v�8@v�X@vGE@u��@u��@t�v@tPH@s�+@sx@s�@r�}@r?@q�@q�S@qO�@pĜ@pe�@o��@o6z@nߤ@n�x@nM�@ne@m��@m \@l�@l4n@k�@j��@ju@i�H@iIR@h��@h��@h`�@g�@g$t@f�L@fn�@f6�@e�@e�=@e��@ej@d��@d�@d��@d2�@c�@@cW?@c@O@b�@b+k@b&�@bO@a�@aA @`e�@_��@_s@^+k@]q@\�e@\?�@\�@[��@[e�@[�@Z��@Z\�@Z_@Y��@YT�@Y@Xѷ@Xu�@W�@W��@W1�@V�B@VL0@V�@U��@U;@T��@TbN@TM@S�K@Sqv@S,�@R�@R��@R��@Q��@P��@P�o@P>B@P  @O�
@OdZ@N��@Nu%@N6�@M�H@Mf�@M!�@M�@L�`@L�$@Lh�@L?�@L"h@K�W@Ky�@K(@J�2@J�m@J��@J
�@I��@I^�@I�@H~(@G�@G�q@G��@G8@Go@F�2@Fں@F�X@F��@Fxl@F^5@E��@E�@E�"@Ehs@ES&@EDg@E-w@E@@D�@D�$@D�D@DH@C�@CRT@C=@B��@B��@BW�@BH�@A�o@A�'@A/@A	l@@�?@@��@@�Y@@u�@@`�@@"h@?�@?�	@>��@>z@>=q@=�)@=x�@<��@<�9@<�@<-�@;��@;s@;x@;E9@:�@:q�@9Vm@9 \@9�@9@9�@8��@8�$@8tT@7�a@7S�@6��@6~�@6�@5��@5!�@4�_@4g8@3�&@3\)@2��@1�d@1#�@0�@/�@/��@/y�@/RT@/;d@/S@.�@.E�@.@-�@-�h@-B�@,��@,��@,h�@+�r@+�@@+.I@*�M@*ȴ@*�\@*+k@)�D@)�@)u�@)B�@)&�@)%@(�@(֡@(�@(�@([�@($@(	�@'�@'��@'S�@'@&�X@&}V@&#:@%�9@%��@%�~@%A @$�	@$��@$z�@$1'@#�@#�@#�k@"�2@"�b@"W�@"0U@"�@!��@!o @!F@!<6@!(�@ �P@ �?@ �9@ ��@ ��@ 1'@�q@t�@=@)_@�@ں@�@^5@:*@	@�9@�C@Y�@2a@�@�@�|@�U@�I@Z@$@�@��@�}@t�@iD@H�@�@��@҉@��@��@Q@8�@��@�@}�@?}@�@	l@	l@��@�v@��@��@[�@@x@�+@�g@_p@.I@�8@͟@�x@d�@.�@��@�T@�@��@��@�M@\�@A @&�@+@V@	l@�@�p@�p@�p@Ɇ@�U@��@g8@Q�@C-@�+@�*@�4@4�@C@��@�<@�b@�+@~�@{�@@�@ �@�@�@��@��@X@:�@ \@��@�p@�@~(@u�@j@M@2�@~@�@�@��@�w@�P@,�@�H@��@Z�@B[@ �@�T@��@��@�-@�t@�S@m]@Dg@(�@%@��@�E@�I@q@�]@�@��@�@��@�@U�@
=@
�@
ں@
��@
n�@
_�@
GE@
)�@
�@	�D@	��@	�M@	Vm@	(�@��@�|@֡11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BtBs�BtBtBs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�BtBtBt9BtBs�BsMBshBsMBraBq'Bm�Bm�BnIBoOBr|BsMBu�By>ByrBz^B{B}�B��B��B��B	��B	�MB	ԯB	�B	�B	�,B	��B	��B	��B	�%B	��B	յB	��B	ðB	��B	�TB	�B	�B	�vB	�MB	��B	��B	y$B	eB	MB	1[B	$B	jB	�B��B�KB�B��B�kB�B��B��B՛B�QB�B�B	 4B	6B	�B	�B	�B	sB	�B	�B	B	/ B	C�B	IB	I�B	Q�B	T�B	MjB	6�B	/�B	(
B	�B	�B	B	(B	B�GB�B�UB	�B	�B	"�B	=�B	R�B	W�B	]~B	^�B	\�B	dB	f�B	i*B	w�B	��B	��B	��B	� B	��B	��B	�TB	��B	�{B	�SB	�%B	�uB	B	��B	�'B	�B	��B	��B	��B	�UB	�}B	�[B	ҽB	�oB	�[B	ՁB	�NB	�lB	��B	��B	� B	ΥB	οB	�B	��B	�5B	�4B	�nB	�B	�B	�/B	�B	�7B	��B	�	B	�B	ܬB	�IB	��B	�dB	�CB	�/B	�B	�qB	�IB	ބB	ބB	�5B	�B	��B	�OB	�5B	�~B	ܒB	�B	ևB	��B	�@B	ңB	ѷB	�hB	�}B	�bB	уB	�YB	��B	��B	�B	�B	��B	�B	ҽB	�B	�:B	�:B	�B	�uB	�aB	�9B	�{B	�,B	��B	ՁB	֡B	׍B	�YB	��B	��B	��B	�B	�7B	�B	��B	�B	�KB	�KB	�KB	�=B	�~B	�dB	�IB	یB	�WB	��B	�]B	�CB	�)B	�IB	ߤB	�pB	߾B	߾B	�B	�B	�HB	�B	�:B	�B	��B	�B	�B	�fB	�8B	�mB	�RB	�B	�`B	�mB	�
B	�mB	�DB	�DB	�_B	�B	�*B	�B	��B	�yB	��B	��B	��B	�6B	�B	�B	�kB	�B	�=B	��B	�WB	�"B	��B	�B	�CB	�B	��B	��B	��B	��B	�IB	�B	�5B	�B	�B	�;B	�B	�B	�;B	�!B	�!B	��B	�B	��B	�B	�B	��B	��B	�B	�B	�MB	�B	��B	�+B	��B	��B	�B	�FB	��B	�`B	�`B	�2B	�lB	�lB	�B	�lB	�RB	�8B	�B	�B	�XB	�rB	�B	��B	�PB	��B	��B	��B	��B	��B	��B	��B	�0B	��B	�PB	�"B	�"B	��B	�wB	��B	��B	��B	��B	�.B
 4B
 �B
�B
�B
B
MB
�B
gB
3B
�B
9B
SB
mB
SB
mB
�B
B
zB
�B
�B
_B
B
�B
�B
�B
	B
	�B

�B
�B
jB
�B
JB

�B
�B
PB
�B
�B
�B
6B
"B
�B
"B
�B
�B
6B
<B
�B
\B
�B
�B
�B
}B
hB
oB
�B
B
&B
[B
B
�B
�B
�B
�B
uB
�B
�B
 B
NB
HB
�B
vB
vB
.B
 B
�B
FB
�B
[B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
$B
B
EB
_B
EB
_B
yB
�B
�B
�B
�B
�B
B
7B
QB
kB
7B
QB
#B
�B
�B
5B
5B
B
�B
~B
/B
OB
�B
�B
B
]B
B
�B
;B
!B
pB
�B
5B
B
jB
�B
!�B
$&B
$�B
$�B
$@B
"�B
!�B
!�B
!�B
!�B
!bB
!HB
 �B
!|B
!�B
!�B
#�B
$@B
%�B
'mB
($B
)�B
*0B
*�B
*�B
+QB
+�B
+�B
+�B
+�B
+�B
+�B
,WB
,=B
,=B
,qB
,WB
,"B
+�B
+�B
+�B
,"B
,�B
,�B
-B
-�B
-�B
.B
.cB
.cB
.�B
.�B
/B
/5B
/iB
/�B
/�B
/�B
/�B
0oB
0�B
0�B
0�B
0�B
1[B
2GB
2aB
2�B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
3MB
3�B
3�B
4B
4TB
4�B
4�B
5ZB
5�B
5�B
5�B
6FB
6�B
6�B
6�B
6�B
6�B
7�B
7fB
7�B
8RB
8�B
8�B
9	B
9>B
9�B
9�B
:B
:DB
:^B
:�B
:�B
;�B
;�B
<B
<PB
<�B
<�B
<�B
="B
=�B
=�B
=�B
>�B
?B
>�B
?HB
?}B
?�B
?�B
@OB
@iB
@�B
@�B
@�B
@�B
@�B
AB
A;B
A�B
A�B
A�B
B[B
B�B
B�B
B�B
CaB
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
E�B
F?B
F�B
F�B
F�B
F�B
F�B
G+B
GB
G_B
G�B
G�B
HKB
H�B
H�B
I7B
I�B
I�B
I�B
J	B
J	B
J=B
J�B
J�B
K)B
KxB
K�B
K�B
LB
LJB
L0B
LB
L�B
M�B
M�B
M�B
M�B
M�B
N"B
NVB
N�B
N�B
O(B
OvB
O�B
O�B
O�B
O�B
O�B
PB
PB
PHB
P�B
P�B
Q B
P�B
Q B
QNB
Q�B
Q�B
Q�B
RB
RTB
RTB
RoB
R�B
R�B
R�B
R�B
R�B
SB
SB
SB
SuB
SuB
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T,B
TB
TaB
T�B
UMB
U2B
VB
VB
VmB
VSB
V�B
V�B
W�B
W�B
W�B
XEB
XyB
X�B
X�B
YB
Y1B
YKB
Z7B
ZkB
ZkB
Z�B
[	B
[=B
[#B
[#B
[�B
\)B
\]B
\)B
\CB
\]B
\�B
^OB
^OB
^jB
^OB
^OB
^OB
^OB
^jB
^�B
^�B
_;B
_VB
_pB
_�B
_�B
`B
`B
`\B
`vB
`�B
a�B
b4B
b�B
c B
c:B
c�B
c�B
c�B
c�B
d@B
d�B
d�B
d�B
eB
e`B
ezB
e�B
e�B
fLB
f�B
gRB
g�B
g�B
g�B
h>B
h>B
hsB
h�B
h�B
h�B
h�B
i*B
i_B
iDB
iyB
i�B
i�B
i�B
i�B
jB
jKB
jKB
jB
j�B
kB
kkB
k�B
k�B
lB
l=B
lqB
l�B
l�B
l�B
l�B
l�B
mwB
m�B
m�B
m�B
m�B
m�B
n}B
ncB
n}B
n}B
n�B
n�B
n�B
n�B
n�B
oOB
o�B
pB
p;B
p;B
p;B
p�B
p�B
p�B
p�B
p�B
q[B
qAB
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r-B
r|B
r�B
r�B
r�B
s3B
sB
shB
s�B
s�B
s�B
s�B
t9B
tnB
tnB
t�B
t�B
u?B
utB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v+B
vzB
vzB
v�B
v�B
wB
wLB
w�B
w�B
w�B
w�B
xB
xRB
xRB
xlB
xlB
xlB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
y	B
y$B
y$B
y$B
y$B
y>B
yXB
yXB
yrB
y�B
y�B
zB
z*B
z*B
z^B
zxB
z�B
z�B
z�B
z�B
{0B
{dB
{dB
{dB
{�B
{�B
{�B
|B
|B
|B
|6B
|�B
|�B
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
}qB
}�B
~B
~(B
~(B
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
B
.B
.B
.B
HB
�B
�B
�4B
�OB
�iB
��B
��B
�iB
��B
�B
� B
� B
�;B
�oB
�UB
�UB
�oB
�oB
��B
��B
�'B
�AB
��B
��B
��B
�-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BtBs�BtBtBs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�Bs�BtBtBt9BtBs�BsMBshBshBr|Bq[Bm�Bm�BnIBo5BraBsMBu�By>ByrBzxB{�B}B�B�1B��B	�8B	�aB	�B
 OB	�fB	��B	��B	�B	�B	��B	�0B	ںB	��B	��B	�YB	��B	�AB	��B	�`B	��B	��B	��B	��B	l�B	S�B	4nB	&fB	"�B	+B�<B��B��B�HB��BڠB�aBՁB�EBیB�-B��B	�B	�B	qB	YB	B	+B	KB	xB	=B	/�B	DgB	J�B	LB	S&B	W$B	O�B	8RB	0�B	)�B	�B	gB	�B	�B	�B�B�B�;B	AB	B	!�B	="B	R�B	X+B	^�B	_�B	]B	d&B	f�B	iyB	w�B	�lB	�
B	�kB	��B	�UB	�B	��B	��B	��B	��B	��B	��B	�B	�aB	�B	��B	�iB	�B	��B	�B	ЗB	ӏB	�&B	�B	�,B	�YB	҉B	�#B	�B	�<B	ҽB	�\B	��B	ܬB	��B	�;B	�B	�&B	�tB	�B	�5B	�QB	��B	ںB	ۦB	��B	ݘB	�B	�5B	ބB	�/B	�B	�B	��B	��B	�B	��B	ޞB	ޞB	ߊB	��B	ޞB	�B	ݲB	��B	��B	��B	�uB	��B	�B	ѷB	��B	бB	ѷB	��B	�sB	��B	�
B	��B	�{B	��B	�[B	҉B	ҽB	ңB	ӏB	��B	��B	��B	�MB	ԯB	��B	ՁB	��B	��B	�YB	��B	�7B	ڠB	�B	��B	��B	�EB	�_B	ٴB	ٚB	ٚB	ۦB	��B	�B	ݲB	��B	��B	�B	ܒB	ܒB	�xB	�~B	��B	߾B	��B	��B	��B	�-B	�B	�NB	�B	�tB	�NB	�B	��B	�B	�mB	�B	�B	��B	�B	��B	�sB	��B	�B	�yB	��B	��B	�DB	�DB	�*B	��B	�0B	�6B	�6B	�B	��B	��B	�B	��B	�B	�=B	�B	�qB	�B	��B	�B	��B	�/B	��B	�B	��B	�cB	�B	�B	�;B	�!B	��B	��B	�UB	�oB	�UB	�UB	�!B	�UB	��B	�vB	��B	��B	��B	�B	�MB	�B	�TB	�B	�zB	��B	�fB	�fB	�zB	�FB	��B	��B	��B	��B	��B	�lB	��B	��B	�RB	�8B	�lB	��B	��B	�DB	�dB	��B	�6B	�0B	�JB	�B	��B	��B	�0B	��B	�PB	��B	�VB	�qB	��B	��B	��B	��B	��B	�B	�cB
 iB
B
B
�B
�B
�B
B
�B
�B
SB
�B
mB
�B
mB
�B
?B
EB
zB
B
B
�B
fB
	B
�B
	B
	7B
	�B

�B
�B
�B
<B
~B
B
�B
jB
�B
�B
B
jB
pB
�B
�B
�B
B
PB
VB
(B
�B
B
(B
�B
�B
�B
oB
B
B
@B
uB
,B
B
�B
B
FB
�B
B
�B
oB
�B
bB
�B
�B
�B
}B
�B
�B
{B
B
�B
&B
B
�B
�B
B
YB
$B
B
�B
�B
�B
?B
EB
yB
�B
yB
�B
�B
�B
KB
�B
B
B
7B
QB
kB
�B
QB
�B
=B
�B
B
OB
OB
B
B
�B
dB
�B
�B
�B
IB
xB
)B
�B
VB
pB
�B
�B
jB
jB
�B
�B
!�B
$@B
%B
%FB
$�B
#:B
!�B
!�B
!�B
!�B
!�B
!�B
!B
!�B
"B
"B
#�B
$&B
%�B
'mB
(XB
)�B
*KB
*�B
+B
+�B
+�B
,B
,B
+�B
,B
,WB
,�B
,WB
,qB
,�B
,�B
,WB
,B
+�B
,B
,qB
,�B
,�B
-]B
-�B
-�B
.IB
.}B
.�B
.�B
/ B
/5B
/OB
/�B
/�B
/�B
/�B
/�B
0�B
0�B
0�B
0�B
1'B
1�B
2aB
2|B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
2�B
3B
3hB
3�B
3�B
49B
4nB
4�B
5%B
5�B
5�B
5�B
6B
6�B
6�B
6�B
6�B
7B
72B
7�B
7�B
7�B
8�B
8�B
8�B
9>B
9rB
9�B
:B
:*B
:^B
:�B
;B
;JB
;�B
<B
<6B
<jB
<�B
<�B
="B
=VB
=�B
=�B
>BB
>�B
?HB
>�B
?}B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
AB
AoB
A�B
BB
BB
B�B
B�B
B�B
B�B
C{B
C�B
C�B
C�B
D3B
DMB
EB
EB
D�B
E�B
F%B
FtB
F�B
F�B
F�B
GB
G+B
G+B
G+B
G�B
G�B
G�B
HfB
H�B
IB
IlB
I�B
J	B
J#B
J	B
J=B
JXB
J�B
J�B
KDB
K�B
K�B
LB
L0B
LdB
LJB
LdB
MB
M�B
M�B
M�B
M�B
M�B
NVB
N�B
N�B
N�B
OBB
OvB
O�B
O�B
O�B
O�B
PB
PB
P.B
P}B
P�B
P�B
QB
Q B
QB
Q�B
Q�B
Q�B
Q�B
R:B
RoB
RoB
R�B
R�B
R�B
R�B
R�B
R�B
S@B
S&B
S@B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
TB
TFB
T,B
T{B
T�B
UgB
UgB
VB
V9B
V�B
VSB
V�B
W?B
W�B
W�B
XB
X_B
XyB
X�B
X�B
Y1B
YKB
YB
ZQB
Z�B
ZkB
Z�B
[=B
[WB
[=B
[WB
[�B
\CB
\]B
\CB
\xB
\�B
]/B
^jB
^OB
^jB
^5B
^jB
^jB
^�B
^�B
^�B
_!B
_VB
_�B
_�B
_�B
`B
`'B
`BB
`�B
`�B
a-B
a�B
bhB
b�B
c:B
cTB
c�B
c�B
c�B
dB
dZB
d�B
d�B
eB
e,B
ezB
e�B
e�B
fB
ffB
gB
gRB
g�B
g�B
g�B
hXB
h>B
h�B
h�B
h�B
h�B
iB
iB
iyB
iyB
i�B
i�B
i�B
i�B
i�B
jKB
jeB
jeB
j�B
j�B
k6B
k�B
k�B
k�B
l"B
lWB
l�B
l�B
l�B
l�B
l�B
m)B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
ncB
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
pB
p!B
p;B
pUB
pUB
p�B
p�B
p�B
qB
p�B
q[B
q[B
q�B
q�B
q�B
q�B
q�B
q�B
rB
rGB
r�B
r�B
r�B
r�B
s3B
s3B
s�B
s�B
s�B
tB
tB
tTB
t�B
t�B
t�B
uB
uZB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
vFB
v�B
v�B
v�B
v�B
w2B
wLB
w�B
w�B
w�B
xB
xB
xRB
xRB
xlB
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
y$B
y	B
y$B
y$B
y$B
yXB
yXB
yrB
yrB
y�B
y�B
y�B
zDB
zDB
z*B
zxB
zxB
z�B
z�B
z�B
z�B
{0B
{dB
{dB
{B
{�B
{�B
{�B
|B
|B
|6B
|PB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}B
}"B
}qB
}�B
}�B
~(B
~(B
~(B
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
.B
HB
HB
cB
HB
�B
�B
�OB
�OB
��B
��B
��B
�iB
��B
� B
�;B
�;B
�UB
�oB
�oB
�oB
��B
��B
��B
�B
�AB
�uB
��B
��B
��B
�-33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<9#�<��p<#�
<��I<��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<%zx<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.01(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202001300050392020013000503920200130005039202207271134262022072711342620220727113426202207271536502022072715365020220727153650  JA  ARFMdecpA30a                                                                20200119093741  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200119093834  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200119093835  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200119093835  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200119093836  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200119093836  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200119093836                      G�O�G�O�G�O�                JA  ARUP                                                                        20200119095505                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200120000000  CF  PSAL_ADJUSTED_QC@
=@~{G�O�                JM  ARCAJMQC2.0                                                                 20200129155039  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200129155039  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023426  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063650  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091504                      G�O�G�O�G�O�                