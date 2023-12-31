CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-05-23T10:00:19Z creation;2019-05-23T10:00:21Z conversion to V3.1;2022-08-02T05:12:55Z update;     
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
_FillValue                 �  ]�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  at   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  u,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �T   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ߠ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �0   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �0   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �0   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �0   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �$   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �,   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20190523100019  20220818081507  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_004                    2C  D   APEX                            8420                            2.11.2                          846 @خ+F�Z�1   @خ+�z� @*ק��&��d��s�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@�33@�  A   A   A@  A`  A�  A�  A���A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B9��B?33BG��BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C�C  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C<�C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  CjL�Ck�fCn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(�fD)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}fD}� D~  D~� D  D� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր Dּ�D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�&f1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�R@���@ÅA ��A ��A@��A`��A�ffA��RA�
=A���A�z�A�z�A��\A��HB \)BG�B33BG�B \)B(\)B0\)B9�
B?p�BG�HBP�BX=qB`Q�Bh\)BpQ�BxG�B�(�B�#�B�(�B�.B��B��B�.B�8RB�33B�.B��B��B��B��B�
=B�#�B�u�B�\B�#�B�#�B�.B�.B�33B�33B�.B�8RB��B�#�B�.B��B�{B�#�C {C
C\C�C{C
�C�C�C�C�C+�C!HC
C�C�C\C 
C"\C${C&{C(�C*
=C,�C.�C0�C2�C4{C6{C8�C:#�C<+�C>�C@\CB
CD{CF\CH\CJ�CL{CN)CP�CR)CT�CV{CX{CZ
C\�C^�C`{Cb{Cd{Cf{Ch�Cj^�Cl�Cn
=Cp\Cr�Ct{Cv�Cx{Cz{C|
C~{C�
=C�fC�C�fC��C��C��C�fC�
=C��C��C��C��C��C��C��C��C��C�
=C��C��C��C�
=C��C�
=C�\C�
=C��C��C��C��C��C�fC��C��C��C��C��C�fC��C�C��C��C��C��C��C��C��C��C�C��C��C��C��C��C��C�
=C��C�
=C��C��C��C��C��C�fC��C�fC�
=C��C�fC��C�
=C��C��C�
=C��C�fC��C�fC��C��C��C��C�
=C��C��C��C��C��C��C�C��C�
=C�\C��C�fC�fC�fC��C��C��C��C��C��C��C�
=C�C��C��C��C�C�
=C��C��C��C��C��C�fC�
=C��C�fC��C��C�
=C��C��C�C�
=D D �D{D�fDD�D�D�{D{D�D�D�fDD�D�D�{D{D�D	{D	�{D
�D
�{DD�DD��D�D�D�D��D{D��DD�
D�D�D�D��D3D�3D{D��D{D�D{D��D�D�D{D�{D{D�D�D��D�D�fD
D��DD�fD{D�3D�D�D 
D ��D!3D!�D"fD"��D#fD#��D$�D$��D%3D%�3D&�D&�{D'fD'��D(�D(��D)�D)�3D*D*�fD+fD+�fD,�D,�{D-{D-�fD.fD.��D/{D/�3D0D0��D1�D1�D2D2��D3fD3�fD4�D4�fD5fD5�{D6�D6�{D7{D7�{D8{D8�3D9D9�fD:D:�D;D;�D<
D<�
D=�D=��D>fD>�D?D?��D@�D@�fDA�DA�fDB�DB�3DC3DC��DD�DD�DE
DE�fDFfDF��DG�DG�{DHDH��DIfDI�{DJ3DJ�DKfDK�{DL�DL�3DMDM�{DN�DN�{DO{DO�{DPDP�fDQ{DQ�3DRDR�fDSDS�{DT�DT�{DUfDU�DVfDV�fDWfDW�DX{DX�fDY{DY��DZDZ��D[fD[�RD\fD\��D]{D]��D^�D^�fD_
D_�D`{D`��Da�Da�Db�Db��Dc{Dc��Dd�Dd��DeDe�{Df�Df�{DgDg�DhDh�fDifDi��Dj{Dj�Dk�Dk�fDl�Dl��Dm{Dm�fDnDn�{Do{Do��Dp
Dp�fDq3Dq�Dr{Dr��Ds{Ds�Dt{Dt��Du�Du�DvfDv��DwfDw��DxDx��Dy�Dy�fDzDz�D{{D{�{D|�D|�fD}�D}�fD~�D~��D�D�fD��D�C3D���D��RD�=D�C�D���D�D��D�@�D��=D���D��D�B�D��=D��HD� �D�B�D��)D�ÅD��D�A�D���D���D�=D�B=D��=D���D��D�B�D���D�D��D�A�D��HD���D��D�C�D��D��{D��D�B�D���D���D��D�B=D���D��=D�=D�B�D���D��=D��D�C3D���D��=D��D�B�D��3D�D��D�C�D��=D���D��D�B=D���D�ÅD��D�B=D���D�D��D�C�D���D�ÅD��D�AHD��HD�ÅD�3D�B�D��3D��=D�=D�A�D���D���D�3D�B�D���D��3D�=D�B=D���D�D��D�B=D���D���D�3D�C�D���D��=D�=D�B�D���D��=D��D�B�D���D��3D��D�B=D���D��3D��D�A�D���D���D��D�B�D��3D��3D�=D�A�D���D���D��D�C�D��3D��3D��D�B�D���D�D��D�B=D��3D��=D��D�A�D���D��=D�3D�B�D���D���D�3D�C3D��3D��3D��D�B�D���D���D��D�C�D��3D���D� �D�A�D���D���D��D�C3D��3D���D��D�B�D���D���D�3D�C�D���D���D�HD�A�D���D�D��D�B�D��3D���D��D�B�D���D��{D��D�B=D���D��=D��D�B�D���D���D�3D�C3D���D���D��D�B�D��=D��=D��D�A�D���D��3D��D�C3D���D���D��D�B�D���D���D�=D�B�D��=D�D��D�C3D���D���D��D�B�D��3D��)D�3D�B�D���D���D��D�B=D��=D���D��D�B=D��=D��=D�3D�B�D���D���D��D�C3D��=D�D�3D�B=D���D�D�)D�B�D��HD���D��D�C�D��3D���D��D�AHD��=D�ÅD�=D�B�D3D�D��D�A�DÂ�D��3D��D�B�Dā�D�D��D�C�Dł�D���D��D�B=DƂ�D�D�=D�B�Dǂ�D��3D��D�B�DȂ�D��=D��D�B�DɃ3D���D��D�B=Dʂ�D��3D��D�C3D˂=D���D��D�B=D́�D���D��D�B�D͂�D���D�=D�A�D΂�D�D��D�@�Dς�D���D�=D�B=DЁHD���D��D�B�Dт�D��=D�=D�B�D҂�D��=D��D�C3DӃ�D��=D��D�C�Dԃ3D��3D�=D�B�DՃ�D��3D�3D�C�Dց�D���D��D�B�Dׂ�D��=D��D�C3D؃�D�ÅD��D�A�Dق�D���D��D�B�Dځ�D���D��D�B�Dۂ�D��=D��D�B�D܂�D�D�=D�B=D݂=D���D��D�B=Dނ=D�D�3D�C�D߃�D���D��D�C3D���D���D��D�AHDၚD�D�=D�B�D�3D��3D��D�A�DずD��HD��D�B=D�=D���D�3D�B�D��D�D�=D�C3D�3D��=D�=D�B�D�3D�D��D�C3D胅D��3D�3D�B�D�3D��3D�=D�B=D��D���D��D�A�D낏D���D��D�B�D�=D���D��D�A�D큚D���D��D�B=DD�D��D�C3D�=D��=D�=D�A�D���D�ÅD��D�B�D�HD���D��D�C�D��D�ÅD��D�C�D�=D��HD�=D�B�D��D�ÅD��D�A�D���D���D��D�B�D��3D���D��D�C�D���D�D��D�B�D���D��=D�=D�B=D��=D���D��D�C�D��3D��3D�3D�B=D���D�D��D�(�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aˡ-A˥zA˪�A˦LA˥A˪�A˩�A˪�A˫kA˫kA˭�A˨�Aˤ�A˪�A˫�A˫6A˯�A˳�A˪0A˗�A˓uA�S�A���A���AʞOA�{JA�6FA���A���A�ƨAɢhAɓ�AɄA�x�A�Z�A�4AȽA�6FA��A�OvA�L�A��A���A��kA�xA�}�A���A�d&A�+A��gA���A�ޞA��YA��A��;A��#A�y�A�!bA��8A�&A�@A�%zA�:�A��A�^�A�A�2�A��0A�OBA��ZA�O�A���A�9�A�yrA��A���A�r|A���A��A�O�A� �A��:A��A��A�)*A�5�A��OA��fA��CA���A~��Az��Ax��Av
�As�8ApQ�Am��Aj�Ag��AcXAb7A_�AX��AV��AV�AU%AP��AM��AK�rAHVmAE(�AB��A@ϫA?8�A:!A3ƨA/z�A,�QA*�A)PHA)Z�A(��A(�A)(A)sA)� A)�}A*4�A*C�A*�XA*��A*�"A)�fA(�9A'��A&�A&�A%	lA$oA#��A#?A")_A!s�A ��A7LA�AL�A9�A!A�+A�{A��A7A�A=�A��A�\Al�A�;A�A�A��Ad�A�@A�hA8A�A�RA�NAN<AĜA�AbNAw2A��A
��A
E�A	�HA	خA	��A	�;A	�#A	�!A��A1'A�?Aq�A+At�AA�A�'A<�A�xA;�A%A�>A� A�DAcAg�A��A�'A��AbNA �A q�A U�@��o@�e�@�	l@�V@���@�;d@�Y@�}V@�m�@��{@��@���@��-@���@���@�2a@�+@��@���@���@���@��N@�Y@��<@��}@�j@�(@���@�b@�W?@��5@�O@�Ta@��@�@�$@�S&@�8@�.I@��@�I@�?}@��@븻@�o�@��@��@�y>@�E�@趮@�@�7�@�V@�o@�2�@�y�@�\@�@@�@�@�J#@��@��K@���@�hs@�<6@���@ܹ$@�y>@�Ft@�!�@��@�ݘ@۱[@�@ڵ@٠�@�҉@؎�@�ff@�P�@�h�@���@շ�@Փ@�x@�Y�@�B�@��@��)@Բ�@ԛ�@�6@ӵt@���@�Z@�D�@�>B@Ѻ^@�y>@Σ@�I�@�x@��#@͘�@�#�@̴9@���@�RT@�Ĝ@��N@��@�ں@��)@ȉ�@�M�@��o@�iD@�q@���@�g8@��@�6@�!�@�@��Z@���@Æ�@�X�@��@�ƨ@�`B@�|�@���@���@�N<@�7L@�)_@�$t@�@���@��)@��x@�l"@�(�@��w@�v`@��<@�k�@�9X@���@�X@���@�V@��Z@��7@�]d@�~@��@��@�dZ@�Mj@�B�@�4@�@�m�@�=@�e@���@�u�@�/�@���@�:*@��#@���@�zx@�m]@�[W@�O�@�RT@�T�@�Q�@�U�@���@���@�-@���@��*@��	@�(�@��@���@�M@�{J@�n/@�A @�@���@�ߤ@�ȴ@���@��@�A�@�J@���@�t�@�hs@�Dg@�
=@���@�h�@�.�@��@��@�Mj@��F@��@�ϫ@�a@��c@��@�8�@�(@��b@�p;@�%�@���@��$@���@�iD@���@�bN@���@�J#@��@�ں@��'@���@�u�@�#:@��Z@���@�qv@��@��Y@�9X@�#:@��@��H@�_p@�o@���@��@�|�@�YK@��+@���@�	l@���@��@��r@���@��	@�+@���@���@��E@��b@�bN@�
�@��&@���@��@���@�/@���@��_@�z�@�Z@�I�@�%�@�x@��@���@��A@��@��Q@��M@�Mj@��@��@��@��@�p;@�N�@�*�@���@���@�s@��M@�ߤ@��@��@�@���@�'�@��	@��@��?@�tT@�<�@��@���@��@��U@���@�\�@�J�@�	@��+@���@��~@�w2@�c�@���@��+@�Ov@���@���@���@��@�y�@�7L@�(�@��@���@���@���@�`B@��@��`@�Ɇ@���@�C-@���@�7L@��@�xl@��@��r@�@O@��@�kQ@��@��X@�&@�Ɇ@��@�M�@�A@�4@~��@~)�@}�7@}rG@}[W@}%F@|b@{n/@z� @zp;@z?@y��@y;@x�@w�
@w_p@v�'@v~�@vff@vJ�@v($@v@u��@uO�@t-�@sخ@s��@s�f@s8@rTa@qk�@p��@ptT@p1'@o�:@nJ�@m��@m��@m��@m�@mzx@m!�@l��@lc�@k]�@j�+@i�T@i�n@iY�@i#�@i;@h��@h<�@h �@g�}@f҉@e�T@e�h@e|@e=�@d��@d,=@c��@c��@ce�@c
=@b��@a@ahs@aO�@a/@`z�@`6@`~@_��@_o@^n�@^&�@]�@]�H@]G�@\�p@\PH@\x@[�:@[v`@[1�@Z�8@Z�m@Z;�@X��@X��@X�@XQ�@W��@W�4@V�h@U��@U+�@T�?@Tr�@T@S�@SJ#@S�@R�1@R�r@Rl�@R;�@Q`B@Pm�@O��@N�@NTa@Mԕ@M(�@M%F@M#�@M�@MV@L*�@K�@Kl�@K8@J�@J��@JJ�@I��@I��@IG�@H��@G�f@F��@Fq�@F($@E��@E�t@Ea�@EA @E7L@D�5@DS�@DG@C�@@Co�@C i@BH�@A�M@A`B@AL�@A@@u�@@�@?��@>�]@>�x@>1�@=|@<�v@<'R@;x@:��@9��@9#�@8�@8�Y@8m�@82�@8	�@7��@7Z�@7�@6��@6_�@5�@5��@5�M@5@@4N�@3�]@4�@4�@4G@4@4�@4�@3�g@3�{@3A�@3�@2҉@2��@2�@2xl@2W�@2)�@2e@2�@2�@1�)@1�C@1hs@17L@0�?@0I�@/�6@/��@/t�@/E9@/o@.�c@.�B@.��@.ff@.C�@-�@-��@-^�@-S&@-<6@-7L@-�@,��@,�K@,�@,֡@,��@,�@+��@+e�@+@O@+$t@*�@*�X@*\�@)��@)�T@)�T@)�@)@)��@)�@)��@)�S@)m]@)%@(�|@(�@(��@(C-@'�;@'|�@'E9@'�@&��@&:*@%��@%��@%S&@%�@$֡@$��@$��@$�z@$��@$|�@$h�@$V�@$A�@$4n@$7@#��@#X�@#9�@#'�@#S@"ں@"��@"v�@"p;@"s�@"l�@"Z�@"8�@".�@!�.@!�@!��@!c@!x�@!p�@!T�@!-w@!	l@ �K@ �[@ Ɇ@ �j@ �@ ��@ �I@ ��@ oi@ V�@ :�@ ,=@�W@��@��@qv@]�@W?@F�@�@�@ i@�@�@�c@��@�!@�+@��@{�@}V@~�@p;@d�@.�@��@��@m]@\�@[W@S&@F@:�@0�@@�@�v@��@�@Z@V�@V�@Q�@�@�@�w@��@�@��@��@��@�4@,�@ i@�c@�@�@n�@.�@��@�@��@�'@c@T�@�@�[@��@��@��@�Y@z�@c�@K^@$@b@�@�A@�@�m@�q@Mj@�@��@^5@=q@:*@$�@_@�@-w@;@�@��@�o@C-@!@��@\)@"�@o@�8@�R@}V@a|@Ta@B[@$�@4@
�@��@Q�@�5@�/@�@��@�@��@l�@Z�@A�@E9@>�@!-@�@�@�@�@ں@��@�1@u%@!�@ԕ@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aˡ-A˥zA˪�A˦LA˥A˪�A˩�A˪�A˫kA˫kA˭�A˨�Aˤ�A˪�A˫�A˫6A˯�A˳�A˪0A˗�A˓uA�S�A���A���AʞOA�{JA�6FA���A���A�ƨAɢhAɓ�AɄA�x�A�Z�A�4AȽA�6FA��A�OvA�L�A��A���A��kA�xA�}�A���A�d&A�+A��gA���A�ޞA��YA��A��;A��#A�y�A�!bA��8A�&A�@A�%zA�:�A��A�^�A�A�2�A��0A�OBA��ZA�O�A���A�9�A�yrA��A���A�r|A���A��A�O�A� �A��:A��A��A�)*A�5�A��OA��fA��CA���A~��Az��Ax��Av
�As�8ApQ�Am��Aj�Ag��AcXAb7A_�AX��AV��AV�AU%AP��AM��AK�rAHVmAE(�AB��A@ϫA?8�A:!A3ƨA/z�A,�QA*�A)PHA)Z�A(��A(�A)(A)sA)� A)�}A*4�A*C�A*�XA*��A*�"A)�fA(�9A'��A&�A&�A%	lA$oA#��A#?A")_A!s�A ��A7LA�AL�A9�A!A�+A�{A��A7A�A=�A��A�\Al�A�;A�A�A��Ad�A�@A�hA8A�A�RA�NAN<AĜA�AbNAw2A��A
��A
E�A	�HA	خA	��A	�;A	�#A	�!A��A1'A�?Aq�A+At�AA�A�'A<�A�xA;�A%A�>A� A�DAcAg�A��A�'A��AbNA �A q�A U�@��o@�e�@�	l@�V@���@�;d@�Y@�}V@�m�@��{@��@���@��-@���@���@�2a@�+@��@���@���@���@��N@�Y@��<@��}@�j@�(@���@�b@�W?@��5@�O@�Ta@��@�@�$@�S&@�8@�.I@��@�I@�?}@��@븻@�o�@��@��@�y>@�E�@趮@�@�7�@�V@�o@�2�@�y�@�\@�@@�@�@�J#@��@��K@���@�hs@�<6@���@ܹ$@�y>@�Ft@�!�@��@�ݘ@۱[@�@ڵ@٠�@�҉@؎�@�ff@�P�@�h�@���@շ�@Փ@�x@�Y�@�B�@��@��)@Բ�@ԛ�@�6@ӵt@���@�Z@�D�@�>B@Ѻ^@�y>@Σ@�I�@�x@��#@͘�@�#�@̴9@���@�RT@�Ĝ@��N@��@�ں@��)@ȉ�@�M�@��o@�iD@�q@���@�g8@��@�6@�!�@�@��Z@���@Æ�@�X�@��@�ƨ@�`B@�|�@���@���@�N<@�7L@�)_@�$t@�@���@��)@��x@�l"@�(�@��w@�v`@��<@�k�@�9X@���@�X@���@�V@��Z@��7@�]d@�~@��@��@�dZ@�Mj@�B�@�4@�@�m�@�=@�e@���@�u�@�/�@���@�:*@��#@���@�zx@�m]@�[W@�O�@�RT@�T�@�Q�@�U�@���@���@�-@���@��*@��	@�(�@��@���@�M@�{J@�n/@�A @�@���@�ߤ@�ȴ@���@��@�A�@�J@���@�t�@�hs@�Dg@�
=@���@�h�@�.�@��@��@�Mj@��F@��@�ϫ@�a@��c@��@�8�@�(@��b@�p;@�%�@���@��$@���@�iD@���@�bN@���@�J#@��@�ں@��'@���@�u�@�#:@��Z@���@�qv@��@��Y@�9X@�#:@��@��H@�_p@�o@���@��@�|�@�YK@��+@���@�	l@���@��@��r@���@��	@�+@���@���@��E@��b@�bN@�
�@��&@���@��@���@�/@���@��_@�z�@�Z@�I�@�%�@�x@��@���@��A@��@��Q@��M@�Mj@��@��@��@��@�p;@�N�@�*�@���@���@�s@��M@�ߤ@��@��@�@���@�'�@��	@��@��?@�tT@�<�@��@���@��@��U@���@�\�@�J�@�	@��+@���@��~@�w2@�c�@���@��+@�Ov@���@���@���@��@�y�@�7L@�(�@��@���@���@���@�`B@��@��`@�Ɇ@���@�C-@���@�7L@��@�xl@��@��r@�@O@��@�kQ@��@��X@�&@�Ɇ@��@�M�@�A@�4@~��@~)�@}�7@}rG@}[W@}%F@|b@{n/@z� @zp;@z?@y��@y;@x�@w�
@w_p@v�'@v~�@vff@vJ�@v($@v@u��@uO�@t-�@sخ@s��@s�f@s8@rTa@qk�@p��@ptT@p1'@o�:@nJ�@m��@m��@m��@m�@mzx@m!�@l��@lc�@k]�@j�+@i�T@i�n@iY�@i#�@i;@h��@h<�@h �@g�}@f҉@e�T@e�h@e|@e=�@d��@d,=@c��@c��@ce�@c
=@b��@a@ahs@aO�@a/@`z�@`6@`~@_��@_o@^n�@^&�@]�@]�H@]G�@\�p@\PH@\x@[�:@[v`@[1�@Z�8@Z�m@Z;�@X��@X��@X�@XQ�@W��@W�4@V�h@U��@U+�@T�?@Tr�@T@S�@SJ#@S�@R�1@R�r@Rl�@R;�@Q`B@Pm�@O��@N�@NTa@Mԕ@M(�@M%F@M#�@M�@MV@L*�@K�@Kl�@K8@J�@J��@JJ�@I��@I��@IG�@H��@G�f@F��@Fq�@F($@E��@E�t@Ea�@EA @E7L@D�5@DS�@DG@C�@@Co�@C i@BH�@A�M@A`B@AL�@A@@u�@@�@?��@>�]@>�x@>1�@=|@<�v@<'R@;x@:��@9��@9#�@8�@8�Y@8m�@82�@8	�@7��@7Z�@7�@6��@6_�@5�@5��@5�M@5@@4N�@3�]@4�@4�@4G@4@4�@4�@3�g@3�{@3A�@3�@2҉@2��@2�@2xl@2W�@2)�@2e@2�@2�@1�)@1�C@1hs@17L@0�?@0I�@/�6@/��@/t�@/E9@/o@.�c@.�B@.��@.ff@.C�@-�@-��@-^�@-S&@-<6@-7L@-�@,��@,�K@,�@,֡@,��@,�@+��@+e�@+@O@+$t@*�@*�X@*\�@)��@)�T@)�T@)�@)@)��@)�@)��@)�S@)m]@)%@(�|@(�@(��@(C-@'�;@'|�@'E9@'�@&��@&:*@%��@%��@%S&@%�@$֡@$��@$��@$�z@$��@$|�@$h�@$V�@$A�@$4n@$7@#��@#X�@#9�@#'�@#S@"ں@"��@"v�@"p;@"s�@"l�@"Z�@"8�@".�@!�.@!�@!��@!c@!x�@!p�@!T�@!-w@!	l@ �K@ �[@ Ɇ@ �j@ �@ ��@ �I@ ��@ oi@ V�@ :�@ ,=@�W@��@��@qv@]�@W?@F�@�@�@ i@�@�@�c@��@�!@�+@��@{�@}V@~�@p;@d�@.�@��@��@m]@\�@[W@S&@F@:�@0�@@�@�v@��@�@Z@V�@V�@Q�@�@�@�w@��@�@��@��@��@�4@,�@ i@�c@�@�@n�@.�@��@�@��@�'@c@T�@�@�[@��@��@��@�Y@z�@c�@K^@$@b@�@�A@�@�m@�q@Mj@�@��@^5@=q@:*@$�@_@�@-w@;@�@��@�o@C-@!@��@\)@"�@o@�8@�R@}V@a|@Ta@B[@$�@4@
�@��@Q�@�5@�/@�@��@�@��@l�@Z�@A�@E9@>�@!-@�@�@�@�@ں@��@�1@u%@!�@ԕ@�@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	$&B	#�B	$B	#�B	#�B	$B	$B	#�B	$B	#�B	$B	$B	#�B	#�B	#�B	$B	$&B	$&B	$�B	$@B	$B	&fB	^�B	x�B

=B
��B
�AB
��B
ԯB
�FB
ԯB
�FB
��B
ּB
��B
� B
�IBL�BW�Bh�Bg�B�UB�B�B�kB�8B��B�B9B�BkB�B!�B!�B#�B/B1BSB�B{BB  B�B��B�ZB�BٚB�6B��B��B��B��B��B��B�<B��Br�Be,B6�B�B
�(B
�
B
�sB
ȚB
�B
kB
@B
3�B
,�B
B	��B	�B	�B	�cB	�GB	��B	�&B	��B	�4B	{B	x�B	raB	UgB	H�B	F�B	EB	2�B	'�B	 vB	�B	�B	�B��B�B�BؓBӏB��B��B��B�XB	
XB	?B	+�B	J�B	`\B	fB	q'B	y	B	�+B	��B	��B	�dB	�<B	��B	��B	��B	�JB	��B	��B	�B	��B	�DB	ǮB	�B	�-B	ǮB	�	B	ʌB	�=B	�_B	�B	��B	��B	��B	�-B	��B	�]B	�
B	�B	��B	�2B	�B	��B	�RB	�RB	��B	��B	��B	�DB	��B	�6B	�=B	�B	��B	�kB	�B	��B	��B	��B	��B	�fB	��B	�0B	�dB	�B	�jB	�]B	�4B	B	āB	�B	ňB	��B	��B	�fB	ȀB	ȚB	ɆB	�RB	�RB	��B	�B	��B	�B	�B	�bB	�}B	� B	�TB	�@B	�MB	��B	׍B	רB	چB	�~B	�B	�/B	��B	�"B	�kB	�B	�B	��B	��B	�B	��B	�B	�cB	��B	�=B	��B	��B	�B	��B	��B	�WB	��B	�B	�QB	�B	�"B	�=B	��B	��B	�B	�6B	��B	�B	�B	�DB	�B	�B	��B	��B	�B	�aB	��B	��B	�B	�B	�GB	�'B	��B	�OB	��B	��B	�B	�wB	�QB	�QB	�B	�/B	�iB	�B	�-B	�B	��B	�B	�B	��B	�B	��B	�-B	��B	�B	�OB	�IB	�qB	��B	��B	�/B	�IB	�IB	�B	��B	�B	�}B	�IB	�B	�B	�aB	�B	�|B	��B	�3B	�aB	�hB	��B	��B	��B	��B	�LB	��B	��B	��B	��B	��B	��B	�zB	�zB	��B	��B	��B	�RB	�B	�B	��B	��B	��B	��B	��B	��B	��B	�*B	�B	�^B	�0B	�dB	�B	�PB	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�]B	�]B	�cB
oB
B
aB
�B
�B
-B
aB
{B
9B
mB
�B
%B
?B
?B
?B
%B
B
EB
	7B
	�B
	7B
	7B
	�B
	�B
	�B

rB
)B
�B
�B
�B
�B
�B
�B
�B
DB
B
dB
�B
�B
6B
B
�B
�B
jB
�B
�B
pB
�B
vB
�B
HB
�B
�B
�B
NB
NB
oB
B
B
&B
&B
�B
uB
�B
�B
B
aB
�B
2B
B
�B
�B
�B
�B
�B
B
B
�B
B
�B
�B
�B
�B
�B
�B
B
EB
_B
yB
yB
�B
�B
�B
B
KB
�B
�B
#B
#B
#B
#B
�B
)B
�B
xB
�B
xB
�B
B
�B
B
B
�B
!B
!B
�B
 vB
 \B
 BB
 �B
 �B
 �B
 �B
 �B
 �B
 vB
 \B
 �B
!B
!-B
!-B
!HB
!�B
!�B
!�B
!�B
!�B
!�B
!|B
!�B
"4B
"�B
#B
#B
#B
#:B
# B
#:B
#�B
#nB
#�B
$�B
$�B
$ZB
$ZB
%,B
%�B
&�B
&�B
&�B
&�B
'B
'8B
'B
(
B
(XB
(�B
(�B
)*B
)*B
)�B
)�B
)�B
*B
*eB
*B
+�B
+�B
+�B
,�B
,�B
,�B
,qB
,qB
,WB
/5B
.�B
/ B
/ B
.�B
/OB
/�B
/�B
/�B
/�B
0B
0;B
1'B
1'B
1�B
1�B
1�B
2|B
2�B
2�B
3�B
3�B
49B
4�B
4�B
4�B
5ZB
5tB
5�B
6B
6`B
6zB
6`B
6FB
7fB
7fB
8B
8B
8B
8B
8�B
9$B
9	B
9XB
9�B
9�B
9�B
9�B
:B
9�B
9�B
:^B
;0B
;JB
;JB
;JB
;dB
<B
<�B
<�B
=B
=B
=B
>B
>(B
>(B
>wB
>�B
>�B
?B
>�B
?B
?�B
@ B
@iB
@�B
@�B
@�B
@�B
AB
A B
AB
A B
A�B
BuB
B�B
BuB
B�B
CB
CaB
C�B
C�B
C�B
DMB
D�B
EB
ESB
ESB
EB
E�B
F?B
F�B
GB
G�B
G�B
G�B
HB
H1B
H�B
H�B
H�B
IB
I7B
IlB
I�B
I�B
IlB
I�B
JrB
JXB
J�B
J�B
KB
J�B
K�B
K�B
L~B
L�B
MB
MjB
M�B
M�B
M�B
NVB
N<B
N<B
N"B
OBB
O�B
O�B
P�B
P�B
Q4B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
SB
SB
SB
SuB
S�B
S�B
TB
T�B
U�B
V9B
VSB
V�B
V�B
V�B
W?B
WYB
W?B
WsB
W�B
X+B
XyB
XyB
X�B
YeB
ZB
Z7B
Z7B
ZQB
Z�B
Z�B
[	B
[�B
[�B
[�B
\CB
\xB
\�B
]�B
^5B
^�B
_VB
_�B
_�B
_�B
_�B
_�B
`'B
`\B
`vB
`�B
a-B
a|B
a|B
a�B
bB
b�B
cB
b�B
b�B
b�B
b�B
b�B
b�B
b�B
c B
cTB
cnB
c�B
c�B
c�B
c�B
c�B
dB
dB
dB
c�B
dB
d@B
dZB
dZB
d�B
e,B
e�B
e�B
e�B
e�B
fB
fB
f2B
f�B
f�B
f�B
gB
g8B
gmB
gRB
g�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h>B
h�B
h�B
h�B
iB
iDB
i*B
i�B
jB
j0B
j0B
j0B
jeB
jeB
jB
jB
jB
jB
j�B
kB
j�B
kB
k�B
k�B
lB
l"B
l"B
l�B
l�B
m)B
mwB
m�B
m�B
nB
n/B
nIB
nIB
nIB
ncB
n}B
n}B
n�B
n}B
n}B
n�B
oiB
oiB
o�B
o�B
o�B
pB
p;B
p!B
pB
pB
p;B
p!B
pB
pUB
p�B
p�B
p�B
p�B
p�B
p�B
q'B
q'B
qAB
qvB
qvB
qvB
qvB
qvB
q[B
q[B
q[B
qAB
q�B
q�B
q�B
q�B
rB
r-B
r-B
rGB
rGB
raB
raB
raB
r�B
r|B
r|B
r|B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
tB
tB
tB
tB
t9B
t9B
t9B
tTB
tnB
t�B
tnB
t�B
t�B
t�B
t�B
t�B
uB
u?B
uZB
uZB
utB
utB
uZB
utB
uZB
u�B
u�B
u�B
u�B
u�B
vFB
vFB
v`B
vFB
v`B
v`B
vzB
vzB
vzB
v�B
wLB
w�B
w�B
w�B
xB
xB
x8B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
y	B
y�B
y�B
y�B
y�B
y�B
zB
zB
z^B
z�B
z�B
z�B
{B
{B
{0B
{JB
{�B
|B
|6B
|6B
|6B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
~B
}�B
~]B
~�B
.B
HB
�B
�B
�B
�B
�B
� B
� B
� B
�B
�B
�B
�4B
�OB
�iB
��B
� B
� B
� 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	$&B	#�B	$&B	$B	#�B	$B	$&B	$B	$&B	$B	$&B	$&B	#�B	#�B	$B	$B	$@B	$ZB	$�B	$�B	$�B	'�B	^�B	z^B

�B
�B
�uB
�B
ԯB
�{B
��B
�{B
�2B
�$B
��B
�FB
��BM�BXyBi�Bk6B��B��B�gB�IB��B�B.BsBB)B �B#:B$@B%�BOB#B�B�BYB�B�B��B�KB�B�4B�]B��B�^B��B��B��B�B�B�NB�Bv�BlqB<jB�B�B
��B
ڠB
�B
�B
pUB
B'B
5�B
1AB
�B	��B	�TB	�[B	ªB	�`B	�tB	�
B	�B	��B	}�B	|�B	y$B	W�B	J#B	IB	I�B	6FB	*�B	$�B	�B	�B	�B	UB	�B��B�dB�?B�B�jB��B��B	
XB	�B	+kB	JrB	_�B	e�B	p�B	x�B	�+B	��B	� B	��B	��B	��B	�B	� B	��B	�uB	�_B	ƎB	��B	�~B	�7B	��B	�{B	��B	�#B	�B	�)B	�7B	��B	�ZB	�nB	�ZB	��B	�vB	��B	�0B	�KB	�>B	��B	�8B	��B	��B	��B	��B	��B	�B	�0B	�B	�"B	�wB	�B	��B	�=B	�}B	��B	��B	��B	�2B	�RB	��B	��B	�B	��B	�B	�.B	��B	��B	�B	ňB	�?B	�zB	��B	ȀB	ȴB	��B	ɆB	ɠB	��B	�B	�xB	�0B	��B	ϑB	бB	� B	�oB	ңB	өB	՛B	�
B	��B	��B	�kB	�B	�B	�B	�cB	�=B	�kB	�B	�"B	�B	�IB	�B	�B	�B	��B	�)B	��B	�6B	�=B	�cB	�IB	�cB	�B	�B	�"B	�B	�B	�B	�qB	�B	��B	�B	�B	�B	�6B	��B	�B	�eB	�qB	�0B	��B	�B	�B	�B	�TB	�3B	�MB	��B	��B	�AB	��B	�cB	�wB	��B	�/B	��B	�B	�B	�IB	�B	��B	�GB	��B	��B	��B	�B	��B	�ZB	��B	�B	�!B	�B	�B	��B	��B	��B	��B	�/B	�IB	�IB	�IB	� B	�B	�B	�B	�B	�UB	�B	��B	�B	�3B	�B	�MB	�B	�%B	��B	�+B	�LB	��B	��B	�B	�2B	�B	�B	��B	��B	��B	��B	�B	�B	��B	�lB	��B	��B	�$B	��B	��B	��B	��B	�B	�^B	�xB	��B	��B	�B	�jB	�jB	��B	��B	��B	�B	��B	��B	��B	��B	��B	�"B	��B	��B	��B
 OB
'B
{B
�B
-B
B
{B
�B
3B
SB
�B
�B
?B
?B
YB
YB
YB
�B
B
	�B
	�B
	lB
	�B
	�B
	�B

#B

�B
)B
�B
�B
�B
�B
�B
�B
�B
�B
JB
�B
B
B
PB
PB
B
B
�B
<B
�B
�B
B
vB
�B
bB
�B
�B
B
hB
�B
�B
&B
&B
@B
[B
�B
�B
�B
�B
�B
�B
�B
gB
MB
�B
B
B
mB

B
SB
SB
�B
B
�B
�B
�B
SB

B
�B
EB
_B
yB
�B
�B
�B
�B
B
KB
�B
B
�B
=B
=B
=B
qB
�B
]B
�B
�B
�B
�B
B
IB
�B
5B
�B
B
VB
pB
�B
 vB
 \B
 \B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
!B
!-B
!-B
!-B
!bB
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"4B
"NB
"�B
# B
#B
#:B
#TB
#:B
#TB
#�B
#�B
$@B
$�B
$�B
$�B
$�B
%zB
%�B
&�B
&�B
&�B
&�B
'8B
'mB
'RB
(sB
(�B
(�B
(�B
)*B
)DB
)�B
)�B
*0B
*�B
*�B
*�B
+�B
+�B
+�B
,�B
,�B
,�B
,�B
,�B
,�B
/iB
.�B
/ B
/ B
/B
/�B
/�B
/�B
/�B
0B
0;B
0�B
1[B
1vB
1�B
2B
2B
2�B
3B
3hB
3�B
4B
4�B
4�B
5B
5B
5�B
5�B
6B
6FB
6`B
6zB
6zB
6�B
7�B
7�B
88B
8B
88B
8lB
8�B
9$B
9>B
9�B
9�B
9�B
:B
:B
:B
9�B
:*B
:�B
;JB
;dB
;dB
;dB
;�B
<PB
<�B
=B
=<B
=VB
=qB
>(B
>(B
>BB
>�B
>�B
>�B
?.B
?B
?cB
?�B
@4B
@iB
@�B
@�B
@�B
@�B
A B
A;B
A B
A�B
BB
B�B
B�B
B�B
B�B
C-B
C{B
C�B
C�B
DB
D�B
D�B
E9B
ESB
EmB
ESB
FB
FYB
F�B
G_B
G�B
G�B
G�B
H1B
HfB
H�B
H�B
H�B
I7B
IRB
I�B
I�B
I�B
I�B
I�B
J�B
JXB
J�B
J�B
KDB
J�B
K�B
L0B
L�B
L�B
M6B
M�B
M�B
M�B
M�B
NpB
N<B
NpB
NVB
O�B
O�B
O�B
P�B
P�B
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S&B
SB
S@B
SuB
S�B
S�B
TaB
T�B
U�B
V9B
VmB
V�B
V�B
W
B
WYB
WsB
WYB
W�B
W�B
X+B
XyB
X�B
X�B
YB
ZB
ZQB
ZQB
Z�B
[	B
[	B
[=B
[�B
[�B
\CB
\�B
\�B
]IB
]�B
^jB
^�B
_�B
_�B
_�B
_�B
_�B
_�B
`BB
`vB
`vB
`�B
aHB
a|B
a�B
a�B
bNB
b�B
cB
b�B
b�B
b�B
b�B
b�B
b�B
b�B
c B
cTB
c�B
c�B
c�B
c�B
c�B
dB
c�B
dB
dB
c�B
dB
dZB
dZB
dtB
d�B
eFB
e�B
e�B
e�B
e�B
f2B
f2B
fLB
f�B
f�B
f�B
gB
g8B
gmB
gRB
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h
B
hXB
h�B
h�B
iB
i*B
i_B
i_B
i�B
jB
j0B
j0B
jKB
jKB
jeB
jB
jB
j�B
j�B
j�B
kB
j�B
kQB
k�B
k�B
l"B
l=B
lWB
l�B
l�B
mCB
mwB
m�B
m�B
m�B
n/B
nIB
n/B
nIB
nIB
n}B
n}B
n}B
n�B
n�B
n�B
o�B
oiB
o�B
o�B
o�B
p!B
p;B
p!B
pB
o�B
pUB
p!B
p!B
pUB
p�B
p�B
p�B
p�B
p�B
p�B
qAB
qAB
q'B
qvB
qvB
q[B
qvB
qvB
qAB
qAB
q[B
qAB
q�B
q�B
q�B
q�B
r-B
rB
r-B
rGB
raB
raB
raB
raB
r�B
raB
raB
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sB
s�B
s�B
tB
tB
tB
tB
tB
t9B
t9B
t9B
t�B
tnB
tnB
t�B
t�B
t�B
t�B
t�B
u%B
u?B
u?B
uZB
utB
uZB
uZB
u�B
utB
u�B
u�B
u�B
u�B
u�B
vFB
vFB
vFB
v`B
vzB
v`B
v�B
vzB
v�B
v�B
wLB
w�B
w�B
w�B
w�B
x8B
xRB
xlB
x�B
x�B
x�B
x�B
x�B
x�B
y>B
y�B
y�B
y�B
y�B
zB
zB
z*B
zxB
z�B
z�B
z�B
{B
{0B
{JB
{dB
{�B
|B
|B
|PB
|PB
|�B
|�B
|�B
|�B
|�B
|�B
}B
}B
}�B
}�B
~(B
}�B
~wB
B
HB
HB
}B
�B
�B
�B
�B
� B
�B
� B
�B
�B
�B
�4B
�OB
��B
��B
�B
� B
�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.08(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201903240045352019032400453520190324004535202207271130202022072711302020220727113020202207271533082022072715330820220727153308  JA  ARFMdecpA30a                                                                20190523095840  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190523100019  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190523100020  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190523100020  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190523100020  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20190523100021                      G�O�G�O�G�O�                JA  ARUP                                                                        20190523111514                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190314000000  CF  PSAL_ADJUSTED_QC@��@��G�O�                JM  ARCAJMQC2.0                                                                 20190323154535  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190323154535  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023020  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063308  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818081507                      G�O�G�O�G�O�                