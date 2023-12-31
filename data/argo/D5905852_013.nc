CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-06-12T03:38:05Z creation;2019-06-12T03:38:07Z conversion to V3.1;2022-08-02T05:12:31Z update;     
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
_FillValue                 �  ]d   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  aT   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ߬   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �0   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �8   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �<   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190612033805  20220818081508  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_013                    2C  D   APEX                            8420                            2.11.2                          846 @���Y�k 1   @����^�@,;~���$�dw|�Q1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�33B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB���Bϙ�B���Bؙ�B�  B�  B㙚B�ffB뙚B���B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C�C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C3�fC6  C8  C:33C;��C=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch�Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!�fD"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DPfDP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�C3Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�vf1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @p�@���@�=qAG�A ��A@��A`��A�ffA�Q�A�ffA���A���A���A���A��HB ffBQ�BQ�BQ�B Q�B(G�B033B8=qB@Q�BHQ�BPG�BX33B`=qBhG�Bp=qBxffB�L�B�L�B��B�\B��B�#�B�(�B�(�B�(�B�.B�(�B�#�B��B�#�B��B�.B�{B�#�BȅB��fB�B�  BظRB��B��B���B�\B�RB�B��B��B��C �C�C�C\C�C
�C\C�C
=C�C�C{C0�C�C
=C\C {C"�C$�C&\C({C*
=C,�C.{C0�C2
=C4C6�C8)C:EC;��C>C@{CB
CD�CF�CH
=CJ
=CL�CN�CP\CR\CT
=CV
=CX�CZ�C\�C^�C`�Cb{Cd�Cf�Ch!HCj�Cl{Cn{Cp{Cr
Ct{Cv{Cx�Cz
=C|�C~
C��C��C�fC��C�
=C��C��C�C��C��C�
=C�fC�fC��C��C��C��C�
=C��C��C��C�
=C�
=C��C�
=C��C��C��C��C��C�fC��C�
=C��C��C��C��C��C�fC�fC��C��C�C��C�fC��C��C��C�
=C��C�
=C�C�C�
=C�fC��C��C�
=C�
=C�
=C��C�
=C��C�C�fC�fC�
=C�\C��C��C��C�
=C�
=C�fC��C��C��C�
=C��C�
=C�fC�fC��C�\C��C��C�fC�
=C��C��C��C�C��C��C�fC�fC�C��C��C�C�fC��C��C�
=C��C��C��C��C��C��C�
=C�
=C��C��C�fC�C�fC��C�C��C��C��C��C��C�
=C��C�fC��D �D �fDD�{DD��DfD�3D�D�D�D��D�D�{D�D��D�D�{D	{D	��D
�D
��DfD��DfD�
D{D��D�D��D�D��D�D��DD��DD��DfD��DfD��D�D��D�D��D�D�D�D��D{D��D�D�{D3D�{D3D�{D{D�3D{D�D�D��D {D ��D!fD!��D"�D"��D#3D#��D$D$�{D%�D%�D&{D&�3D'{D'�{D(3D(��D)3D)��D*3D*�{D+�D+�3D,�D,��D-{D-��D.�D.�D/�D/�{D0�D0�{D1D1�{D2{D2��D3D3�3D4HD4��D53D5�{D6�D6�D7{D7��D8�D8�{D9D9��D:�D:�{D;{D;�fD<
D<�D=D=��D>{D>�
D?D?��D@D@�DA�DA��DB�DB�fDC
DC��DDDD�fDE{DE�DFfDF�fDG�DG��DH3DH�{DIfDI��DJ�DJ�{DKDK�DL3DL��DM�DM�fDN
DN�fDODO�fDPRDP�fDQDQ�{DR�DR��DS�DS�{DT{DT�3DU3DU�DV�DV�DWDW�3DX3DX��DYDY�DZ{DZ��D[�D[��D\3D\��D]�D]�D^�D^��D_3D_��D`{D`��Da�Da�3Db{Db��DcDc��Dd3Dd��De3De��Df3Df��DgDg��Dh�Dh�{Di�Di��DjDj�fDkDk��Dl�Dl�Dm�Dm��Dn�Dn��Do�Do�{Dp�Dp��Dq3Dq�3Dr3Dr�3Ds�Ds��DtDt�Du�Du�{Dv3Dv�{DwDw�{Dx�Dx��Dy�Dy��DzDz�
D{{D{��D|�D|�D}�D}�D~D~��D�D�{D��D�A�D��HD���D��D�B=D��3D��=D��D�B=D���D��=D�3D�B�D���D���D��D�A�D��=D��=D��D�B=D���D���D��D�A�D��HD��=D��D�B�D���D��=D�=D�A�D���D���D��D�B�D��=D��=D�=D�AHD���D��HD��D�B=D���D��HD��D�AHD��=D��=D� �D�A�D��HD���D��D�B�D���D��=D��D�A�D��=D���D� �D�B=D���D��=D��D�C�D��=D���D��D�A�D���D��=D��D�B�D���D��HD�=D�B�D���D�D��D�A�D���D���D��D�B�D���D���D�HD�A�D���D��3D��D�B=D���D���D��D�C3D���D���D�=D�B�D���D��=D��D�B�D���D���D��D�B=D���D���D��D�B�D���D���D��D�B�D��3D��3D�=D�AHD���D��HD��D�B�D���D���D��D�B�D���D���D��D�A�D���D���D��D�B�D���D��HD�=D�B�D��=D��=D��D�A�D���D�D��D�B�D��HD��HD�=D�A�D���D��HD��D�C3D���D�ÅD��D�@�D��HD�D��D�B�D���D��HD��D�A�D���D���D��D�A�D���D�D�=D�B�D��=D��HD��D�B=D���D���D��D�B�D���D��3D��D�C3D��=D���D�=D�B=D��=D�D�=D�AHD���D�D��D�B�D��=D���D�HD�A�D���D�D�HD�A�D���D��3D�=D�A�D���D���D��D�A�D���D��=D��D�B�D���D�D��D�C3D���D�ÅD�=D�B=D���D��=D��D�B=D���D���D��D�A�D���D��3D��D�B=D���D���D��D�AHD���D��=D��D�B�D��3D�D�=D�B=D���D��=D��D�A�D���D�D�=D�B=D�D���D�=D�B�DÂ�D��=D�HD�B�Dă�D���D��D�D{Dł=D��=D��D�A�DƂ=D���D��D�B�Dǂ�D�D��D�AHDȂ�D�D��D�B�Dɂ=D��=D�=D�B�Dʁ�D���D��D�B=Dˁ�D��HD��D�A�D́�D��HD�HD�B=D͂�D���D�=D�B=D΀�D���D��D�A�Dς�D�D�3D�B=DЁ�D�D��D�B�Dт=D��=D��D�C3D҂=D���D��D�C�DӁ�D���D��D�B�DԂ�D�D��D�C�DՃ�D�ÅD��D�A�Dւ=D��=D��D�B�Dׂ�D��3D��D�B�D؂�D��=D��D�B=Dك3D�D��D�B�Dڃ�D��=D��D�B=Dہ�D���D��D�A�D܂=D�D��D�@�D݂�D���D�HD�@�Dށ�D��3D��D�B=D߂=D�D��D�B�D��3D���D��D�B�D�=D�D��D�B�D�3D��=D�HD�B=DずD���D�=D�B�D�3D��=D�=D�B=D�=D��=D�=D�B=D�HD��HD� �D�A�D�=D��=D��D�C3D�3D���D�HD�B=D�=D��=D�=D�B=D��D��=D��D�B=D��D���D�HD�A�D�=D�D��D�B�D��D���D��D�A�D�HD��HD��D�B�D�=D���D�3D�B=D���D��3D��D�C�D�3D��3D��D�C3D�D���D��D�C�D�=D���D�=D�A�D�D��3D�=D�A�D��HD��HD��D�B�D���D���D�3D�C�D��=D���D�HD�@�D���D�D��D�A�D���D��=D��D�C�D��3D��HD��D�B�D�y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�~�A�}�A�PHA��AۗYAۥzAۅ�A�W�A�EA�>wA�6FA�4nA�4A�0�A�/A�-�A�*�A�&�A�%zA�$@A�#�A�$tA� \A��A�qA��A�SA��A��Aڇ�A�~(A� 'A�J#AՁoA�J#A�7A�A���A���AҵAҟVAҒ:AҋxA�iyA�*0A�m)A��>A�y�AϿ}A͊	A�QNA��aA��mA®IA���A��_A��AA�=A�jKA���A�A�A��uA��xA��A�F�A�Q�A�%�A���A�e,A��]A��_A�GA��A�I�A��XA�GzA�PA���A��AA��&A�?}A���A���A�	A���A�3�A���A�)�A��A�IA��rAAy��Au�AnAfSAc�A`�AY��AV�AQr�AM�pAL��AL+�AKݘAJMAHϫAHI�AH\)AHp;AF�6ADdZA@�'A=��A;��A8��A66zA5uA4xlA4O�A4{�A3�A2V�A1��A0�A0�>A0~(A/�]A/X�A.�A,��A*��A)w�A(��A(�A(QA'�KA'�~A'+A&^�A%A$�A$i�A#�A#7�A"��A"OvA"6�A!��A �.A G�A�AiDA��A�A@OA��A@�A�jAu%AQ�A8A��A��A��A��Av`A�A��A2�A��Am]AHA33AOA�A�oA�AߤAqA��A7�A��AMjA&A�|A�QA��Ak�A8�A��AJ�A�A�hAGEA �A�bA�A��AĜA�VA[�A��AVA��A�fA�A
�6A
e,A
($A	��A	/�A	�A�AϫAA��Av`AoiA[�A-wA�PA�$AϫA%A�FA��A~�Al�AY�AC�A-A�A�A��AB�A��A6�A �A �gA ��A jA =�@���@�qv@��]@�+k@��@��@��@�9�@��O@��C@�W?@��2@��@���@��!@�1@�_�@��}@�:�@���@�d�@�g�@�RT@���@��@��W@��@�W�@�hs@��@�@��@릵@�6@�PH@��o@��@��
@�H@��@�:@�c@�a�@��@�{@�o @�R@�:*@�u�@�4@���@�^�@�!@�h
@�n�@�\)@�]�@�c@ۯ�@���@�_p@���@��@�L�@�h
@��p@�/�@�8�@��@ѐ�@�u%@��@��N@��@ϝ�@���@�b@�ѷ@�G@�6z@��U@��@��@�rG@��X@�*�@�	�@�ԕ@Ǚ�@�C@��	@��H@�ں@��@��v@�ȴ@Ƈ+@�h
@�L0@�J�@��@�x�@���@�(�@���@Î�@�Mj@��@��@º�@§@A@�a|@�@�@��@���@�iD@�:�@��@���@�H@��@���@�hs@��!@���@�n/@�RT@���@�{@�J@���@�<6@���@��E@��9@���@�~@��h@�o@��I@�� @�d�@�)�@�˒@�=�@�n�@�7�@��@���@�n/@�@�ѷ@�?@���@��@�V@��@���@��@��	@�A�@��M@��@�A @���@��@�Q�@���@�*�@���@�@O@��)@�c @��@�O�@��@�tT@�C�@��]@��@��@�oi@�:�@���@�Vm@��@�Ĝ@���@��h@��6@���@���@�u�@�`�@�@���@��$@�^�@�!�@���@�N�@�	�@��@��^@�j@��@��L@��F@��@�YK@�"h@�@�4�@��@���@��_@�2�@�ԕ@��H@��@@���@�u�@�j�@�b�@���@��2@��<@�q@�;�@�	�@�ݘ@��g@��a@�;d@���@���@�z@�{@��@��@�A�@�@���@���@���@�r�@�bN@�{@��}@�X@�҉@��}@��_@��^@�<6@�7L@�%F@��@�͟@��@���@��@��@��k@�RT@�*0@�@��|@��<@���@�_�@�*�@�ݘ@��^@��:@�N<@��m@�Xy@���@�T�@��@���@��F@��o@�,=@�|�@�J#@��@���@���@��@��4@���@�u%@�x@�@��.@��Q@��t@�4�@���@���@��@��@��,@�ѷ@�҉@���@��,@��s@�?�@��6@���@�A @��"@��@��@�҉@���@���@�l"@�3�@��@��@�Y�@�?}@�1�@��@��9@��_@�z�@�s�@�Z@�6�@��
@�|@�0�@���@���@���@��@�B[@�&�@�_@���@��n@�\)@�G�@�E9@�6z@�)_@�o@��@�ں@�kQ@ƨ@~�@~��@~E�@}rG@|�v@|�@|bN@|�@{�&@{��@z��@y��@yj@yF@x�@x��@xM@w��@w=@w(@v��@vH�@u�C@u+�@u%F@t�`@t|�@tm�@tI�@s�@s��@rȴ@r��@rGE@q��@qX@q�@p��@p�@o�
@oH�@ni�@n�@m�#@m�C@mJ�@l�.@k��@j��@jxl@j�@i�z@i��@i&�@h�@h��@h$@g�	@go�@gA�@g)_@f�y@f�B@f��@f;�@e�h@d��@d��@d��@d9X@c�K@ciD@c@O@c�@b��@bM�@a�3@`q@_g�@_�@^�8@^�'@^��@^@�@^�@]�d@]zx@]&�@]V@\��@\��@\e�@\4n@\�@[��@[�@Z��@Z:*@Z	@Y�Z@Y�9@Y��@YO�@Y4@X��@XZ@X9X@X1'@W��@Wqv@V��@V�@Uf�@U@T��@TC-@S�q@R��@R��@R��@R:*@Q�@Q\�@P�`@P~(@P1'@P~@O{J@N��@Nߤ@N͟@NR�@N�@N4@M��@Ms�@M@L[�@K��@K�V@K�@J��@J��@JM�@J)�@I�)@I��@I@H��@Hz�@G�	@GMj@G>�@G+@F�@F�b@F�\@F#:@E��@EF@D�@D�Y@D7�@D1@C�a@C��@C�{@CU�@C i@B�L@B}V@B6�@A�z@Ae,@A/@A+@A%@@��@@S�@?4�@>��@>�A@>M�@=��@=��@=�T@=u�@=%@<u�@;˒@;�@:��@:d�@:3�@9��@9;@8�u@8H@8'R@7�6@7{J@78@6�@6u%@64@5��@5m]@5V@4��@4Ɇ@4�e@4PH@42�@4�@3˒@3��@3S�@2��@2��@2�@2V@2O@1��@1(�@0�E@0�u@0S�@/�]@/t�@.�8@.�@.i�@.6�@-�@-��@-hs@-IR@-4@,�@,��@,m�@,D�@,,=@+�&@+�:@+C�@+@*�@*�<@*��@*E�@*�@*�@*�@)�)@)�z@)��@)}�@)O�@)�@(�v@(�U@(r�@(9X@'��@'�q@'qv@',�@&�@&�F@&~�@&:*@&�@%��@%e,@%Dg@%!�@$ѷ@$�@$��@$y>@$>B@$@#�
@#��@#�$@#X�@#9�@#@"�@"_�@"M�@"�@!�@!�=@!O�@!@@ ��@ �_@ �@ PH@ M@�0@U�@�@�@�+@n�@.�@�.@�C@w2@k�@N<@��@��@�@l"@U2@!@��@o�@K�@33@�]@s�@L0@J@�n@s�@�@�@��@��@"h@��@�
@��@��@6z@@��@�L@~�@@�@e@��@��@hs@O�@4@�@�@�/@�p@�e@[�@9X@*�@ƨ@o�@H�@9�@/�@Y@��@ں@Z�@4@��@�@ԕ@�H@�@�@c�@A @0�@!�@�P@�)@��@�z@��@��@q@M@$@��@��@��@]�@�@�@��@�<@�!@�x@��@n�@3�@�@��@��@��@w21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�~�A�}�A�PHA��AۗYAۥzAۅ�A�W�A�EA�>wA�6FA�4nA�4A�0�A�/A�-�A�*�A�&�A�%zA�$@A�#�A�$tA� \A��A�qA��A�SA��A��Aڇ�A�~(A� 'A�J#AՁoA�J#A�7A�A���A���AҵAҟVAҒ:AҋxA�iyA�*0A�m)A��>A�y�AϿ}A͊	A�QNA��aA��mA®IA���A��_A��AA�=A�jKA���A�A�A��uA��xA��A�F�A�Q�A�%�A���A�e,A��]A��_A�GA��A�I�A��XA�GzA�PA���A��AA��&A�?}A���A���A�	A���A�3�A���A�)�A��A�IA��rAAy��Au�AnAfSAc�A`�AY��AV�AQr�AM�pAL��AL+�AKݘAJMAHϫAHI�AH\)AHp;AF�6ADdZA@�'A=��A;��A8��A66zA5uA4xlA4O�A4{�A3�A2V�A1��A0�A0�>A0~(A/�]A/X�A.�A,��A*��A)w�A(��A(�A(QA'�KA'�~A'+A&^�A%A$�A$i�A#�A#7�A"��A"OvA"6�A!��A �.A G�A�AiDA��A�A@OA��A@�A�jAu%AQ�A8A��A��A��A��Av`A�A��A2�A��Am]AHA33AOA�A�oA�AߤAqA��A7�A��AMjA&A�|A�QA��Ak�A8�A��AJ�A�A�hAGEA �A�bA�A��AĜA�VA[�A��AVA��A�fA�A
�6A
e,A
($A	��A	/�A	�A�AϫAA��Av`AoiA[�A-wA�PA�$AϫA%A�FA��A~�Al�AY�AC�A-A�A�A��AB�A��A6�A �A �gA ��A jA =�@���@�qv@��]@�+k@��@��@��@�9�@��O@��C@�W?@��2@��@���@��!@�1@�_�@��}@�:�@���@�d�@�g�@�RT@���@��@��W@��@�W�@�hs@��@�@��@릵@�6@�PH@��o@��@��
@�H@��@�:@�c@�a�@��@�{@�o @�R@�:*@�u�@�4@���@�^�@�!@�h
@�n�@�\)@�]�@�c@ۯ�@���@�_p@���@��@�L�@�h
@��p@�/�@�8�@��@ѐ�@�u%@��@��N@��@ϝ�@���@�b@�ѷ@�G@�6z@��U@��@��@�rG@��X@�*�@�	�@�ԕ@Ǚ�@�C@��	@��H@�ں@��@��v@�ȴ@Ƈ+@�h
@�L0@�J�@��@�x�@���@�(�@���@Î�@�Mj@��@��@º�@§@A@�a|@�@�@��@���@�iD@�:�@��@���@�H@��@���@�hs@��!@���@�n/@�RT@���@�{@�J@���@�<6@���@��E@��9@���@�~@��h@�o@��I@�� @�d�@�)�@�˒@�=�@�n�@�7�@��@���@�n/@�@�ѷ@�?@���@��@�V@��@���@��@��	@�A�@��M@��@�A @���@��@�Q�@���@�*�@���@�@O@��)@�c @��@�O�@��@�tT@�C�@��]@��@��@�oi@�:�@���@�Vm@��@�Ĝ@���@��h@��6@���@���@�u�@�`�@�@���@��$@�^�@�!�@���@�N�@�	�@��@��^@�j@��@��L@��F@��@�YK@�"h@�@�4�@��@���@��_@�2�@�ԕ@��H@��@@���@�u�@�j�@�b�@���@��2@��<@�q@�;�@�	�@�ݘ@��g@��a@�;d@���@���@�z@�{@��@��@�A�@�@���@���@���@�r�@�bN@�{@��}@�X@�҉@��}@��_@��^@�<6@�7L@�%F@��@�͟@��@���@��@��@��k@�RT@�*0@�@��|@��<@���@�_�@�*�@�ݘ@��^@��:@�N<@��m@�Xy@���@�T�@��@���@��F@��o@�,=@�|�@�J#@��@���@���@��@��4@���@�u%@�x@�@��.@��Q@��t@�4�@���@���@��@��@��,@�ѷ@�҉@���@��,@��s@�?�@��6@���@�A @��"@��@��@�҉@���@���@�l"@�3�@��@��@�Y�@�?}@�1�@��@��9@��_@�z�@�s�@�Z@�6�@��
@�|@�0�@���@���@���@��@�B[@�&�@�_@���@��n@�\)@�G�@�E9@�6z@�)_@�o@��@�ں@�kQ@ƨ@~�@~��@~E�@}rG@|�v@|�@|bN@|�@{�&@{��@z��@y��@yj@yF@x�@x��@xM@w��@w=@w(@v��@vH�@u�C@u+�@u%F@t�`@t|�@tm�@tI�@s�@s��@rȴ@r��@rGE@q��@qX@q�@p��@p�@o�
@oH�@ni�@n�@m�#@m�C@mJ�@l�.@k��@j��@jxl@j�@i�z@i��@i&�@h�@h��@h$@g�	@go�@gA�@g)_@f�y@f�B@f��@f;�@e�h@d��@d��@d��@d9X@c�K@ciD@c@O@c�@b��@bM�@a�3@`q@_g�@_�@^�8@^�'@^��@^@�@^�@]�d@]zx@]&�@]V@\��@\��@\e�@\4n@\�@[��@[�@Z��@Z:*@Z	@Y�Z@Y�9@Y��@YO�@Y4@X��@XZ@X9X@X1'@W��@Wqv@V��@V�@Uf�@U@T��@TC-@S�q@R��@R��@R��@R:*@Q�@Q\�@P�`@P~(@P1'@P~@O{J@N��@Nߤ@N͟@NR�@N�@N4@M��@Ms�@M@L[�@K��@K�V@K�@J��@J��@JM�@J)�@I�)@I��@I@H��@Hz�@G�	@GMj@G>�@G+@F�@F�b@F�\@F#:@E��@EF@D�@D�Y@D7�@D1@C�a@C��@C�{@CU�@C i@B�L@B}V@B6�@A�z@Ae,@A/@A+@A%@@��@@S�@?4�@>��@>�A@>M�@=��@=��@=�T@=u�@=%@<u�@;˒@;�@:��@:d�@:3�@9��@9;@8�u@8H@8'R@7�6@7{J@78@6�@6u%@64@5��@5m]@5V@4��@4Ɇ@4�e@4PH@42�@4�@3˒@3��@3S�@2��@2��@2�@2V@2O@1��@1(�@0�E@0�u@0S�@/�]@/t�@.�8@.�@.i�@.6�@-�@-��@-hs@-IR@-4@,�@,��@,m�@,D�@,,=@+�&@+�:@+C�@+@*�@*�<@*��@*E�@*�@*�@*�@)�)@)�z@)��@)}�@)O�@)�@(�v@(�U@(r�@(9X@'��@'�q@'qv@',�@&�@&�F@&~�@&:*@&�@%��@%e,@%Dg@%!�@$ѷ@$�@$��@$y>@$>B@$@#�
@#��@#�$@#X�@#9�@#@"�@"_�@"M�@"�@!�@!�=@!O�@!@@ ��@ �_@ �@ PH@ M@�0@U�@�@�@�+@n�@.�@�.@�C@w2@k�@N<@��@��@�@l"@U2@!@��@o�@K�@33@�]@s�@L0@J@�n@s�@�@�@��@��@"h@��@�
@��@��@6z@@��@�L@~�@@�@e@��@��@hs@O�@4@�@�@�/@�p@�e@[�@9X@*�@ƨ@o�@H�@9�@/�@Y@��@ں@Z�@4@��@�@ԕ@�H@�@�@c�@A @0�@!�@�P@�)@��@�z@��@��@q@M@$@��@��@��@]�@�@�@��@�<@�!@�x@��@n�@3�@�@��@��@��@w21111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	MjB	MB	MB	KDB	K^B	I�B	J	B	I�B	IRB	IlB	IRB	IRB	IRB	IlB	I�B	I�B	I�B	I�B	I�B	I�B	J	B	J#B	J#B	J#B	J	B	I�B	I�B	IB	F?B	:DB	B��B�[B��B��B�)B��B�B��B�UB� B��B��B��B�hBԕB��B	)B	%�B	W�B	`�B	��B	��B
/�B
HKB
MB
��Bq[B�UB~�B}VB��B��B��B��B��B��B�B�XB�+B�B�B�B�B�]B}�B^�BAoB%BUB
��B
�,B
�TB
f�B
W�B
K�B
*B
 �B
4B

#B	�B	��B	�DB	��B	��B	mwB	aB	J�B	-B	�B	�B	xB	
�B	tB	MB	�B	%�B	/ B	MjB	e�B	i_B	cB	N�B	1'B	"�B	#B	�B	/B	.�B	@ B	M�B	U�B	QB	T�B	S�B	bB	n}B	}"B	�B	��B	��B	�eB	�;B	��B	��B	��B	��B	�PB	B	��B	ϫB	��B	�:B	�eB	��B	�5B	�jB	�B	�B	�IB	�B	�AB	�aB	�?B	�`B	�dB	��B
uB
�B
tB
zB
�B
	�B
�B
&B
�B
�B
�B
�B
/B
'B
)�B
)�B
*0B
+�B
.}B
1�B
2�B
2�B
33B
3�B
6+B
7�B
7�B
8B
8�B
8�B
9rB
:DB
:*B
<B
;�B
<jB
<PB
<�B
<PB
="B
=�B
=<B
="B
=B
<�B
;�B
>�B
<�B
<�B
=<B
<�B
<�B
<6B
<�B
;�B
;�B
;dB
;JB
;B
;0B
:�B
:�B
:DB
9�B
9�B
8B
7�B
6+B
5�B
5ZB
5ZB
5?B
5%B
4�B
49B
3�B
4B
2�B
2�B
2|B
1�B
1�B
1[B
1B
0�B
0�B
0UB
/�B
/B
.}B
.IB
-wB
,�B
,B
+QB
*�B
)_B
(�B
(sB
(>B
'�B
'�B
&�B
&B
%�B
%B
$�B
$ZB
#�B
#�B
# B
"NB
!�B
 �B
�B
jB
B
)B
	B
�B
�B
�B
�B

B
YB
�B
�B
B
yB
�B
eB
�B
�B
�B
�B
�B
B
B
pB
	�B
�B
oB
�B
�B
B
�B	��B	��B	��B	�hB	��B	�)B	�B	��B	�nB	�?B	�B	��B	��B	�B	��B	�B	�KB	��B	��B	�B	��B	�B	�B	��B	�)B	�wB	�CB	�B	�B	�B	�B	�B	��B	�B	��B	�RB	��B
�B
	B
	lB

�B
�B
^B
	�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
B
B
�B
�B
�B
B
�B
�B
�B
�B
B

rB
	�B
KB
�B
�B
SB
�B
�B
�B
+B
�B

�B

�B
�B
�B
�B
"B
VB
�B
�B
�B
�B
.B
.B
vB
�B
VB
�B
	�B
�B
�B
_B
)B
�B
�B

�B

	B
	lB
	�B
	�B
6B
�B
~B
�B
~B
PB
�B
�B
�B
�B
(B
.B
�B
�B
�B
oB
oB
�B
�B
B
B
B
&B
B
&B
&B
@B
&B
&B
�B
B
{B
�B
{B
�B
�B
�B
�B
�B
�B
gB
B
�B
�B
MB
�B
�B
�B
mB
mB
�B
�B
�B
KB
KB
�B
�B
�B
�B
=B
�B
�B
�B
/B
IB
/B
B
�B
�B
�B
�B
�B
�B
�B
!B
VB
�B
�B
 BB
 BB
 BB
 'B
 vB
 BB
 �B
!�B
!bB
 �B
"�B
#nB
#TB
#�B
#�B
#�B
$B
#�B
#�B
$�B
$�B
%,B
%`B
%FB
%zB
%�B
%�B
&B
&fB
&�B
&�B
&�B
&�B
&�B
&�B
'B
'B
'B
'RB
'B
&�B
&�B
&LB
&LB
&fB
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'B
(>B
(�B
(�B
(�B
(�B
)B
)*B
)DB
)_B
)*B
(�B
)�B
)�B
*KB
+6B
+�B
+�B
+�B
+�B
+�B
+�B
,WB
,�B
,�B
-�B
-�B
-�B
-�B
-�B
.�B
.�B
/ B
.�B
.�B
.�B
/iB
/�B
0�B
1AB
1AB
1[B
1[B
1�B
1�B
1�B
1�B
2�B
3B
3B
3B
33B
33B
3MB
33B
3hB
4B
5%B
5tB
5tB
5�B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
8�B
9�B
9�B
9�B
:^B
:xB
:�B
:�B
;dB
;JB
;dB
;�B
<B
<PB
<PB
<jB
<�B
<�B
<�B
<�B
=B
=�B
=�B
=�B
>BB
>(B
>BB
>wB
>]B
>�B
>�B
?}B
?�B
?�B
?�B
?�B
?�B
@iB
@�B
A B
A;B
AoB
A�B
A�B
A�B
BB
BuB
B�B
B�B
B�B
B�B
CB
B�B
B�B
CGB
C�B
C�B
C�B
C�B
D3B
D�B
D�B
D�B
D�B
E9B
EB
EmB
FYB
G_B
GzB
G�B
G�B
G�B
HKB
H�B
H�B
IB
IlB
IlB
IlB
I�B
I�B
I�B
I�B
J=B
J�B
J�B
K^B
KxB
KxB
K�B
K�B
K�B
K�B
K�B
LdB
LdB
LJB
LdB
L�B
M�B
N"B
N�B
N�B
OB
OBB
O�B
P.B
PHB
PHB
P�B
P�B
P�B
QhB
Q�B
Q�B
Q�B
RTB
RoB
RTB
R:B
RoB
R�B
R�B
RoB
R�B
RTB
R�B
R�B
R�B
S@B
S�B
S�B
S�B
S�B
S�B
TB
TFB
TaB
TaB
UMB
U2B
U2B
U2B
UgB
U�B
U�B
VB
VSB
V�B
V�B
W
B
W?B
WYB
W�B
W�B
W�B
W�B
W�B
X_B
X_B
X�B
X�B
YKB
YKB
YKB
YKB
YeB
Y�B
Z�B
Z�B
Z�B
[	B
[WB
[=B
[#B
[�B
\B
\]B
\�B
]�B
^B
^B
^B
^�B
_VB
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`\B
`�B
`�B
a-B
abB
a|B
a�B
a�B
a�B
a�B
a�B
bB
bNB
b�B
b�B
cB
c B
c:B
c:B
cTB
c�B
d&B
d@B
d@B
d�B
d�B
eFB
ezB
e�B
e�B
e�B
fLB
ffB
ffB
f�B
f�B
gB
gB
gRB
gRB
g�B
g�B
h$B
h>B
h>B
h�B
h�B
i*B
i*B
iDB
iDB
i_B
iyB
i�B
i�B
i�B
jB
jB
j0B
jB
j�B
j�B
kB
kkB
k�B
k�B
k�B
k�B
lWB
lqB
l�B
l�B
l�B
mB
m]B
m�B
m�B
m�B
m�B
m�B
nIB
nIB
n}B
n�B
n�B
n�B
o B
oOB
oOB
o�B
o�B
o�B
p;B
poB
p�B
p�B
p�B
p�B
qB
qAB
q�B
q�B
q�B
rGB
raB
r�B
r�B
r�B
s3B
s3B
sMB
s�B
s�B
s�B
tB
tB
t9B
t�B
t�B
t�B
uB
utB
u�B
u�B
u�B
v`B
vzB
v�B
v�B
wB
wLB
w�B
w�B
w�B
xB
xRB
x�B
x�B
x�B
y	B
y$B
yrB
y�B
y�B
y�B
y�B
zB
z*B
zDB
z^B
z^B
z^B
zxB
z�B
z�B
z�B
{dB
{�B
{�B
{�B
{�B
|B
|B
|B
|�B
}"B
}"B
}<B
}"B
}<B
}<B
}VB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~B
~(B
~(B
~BB
~�B
~�B
~�B
~�B
cB
}B
}B
}B
�B
�B
�B
�B
� B
�B
�iB
�iB
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	M�B	M�B	M�B	LB	KxB	J=B	JrB	J	B	IlB	I�B	IlB	IlB	IlB	I�B	I�B	I�B	I�B	I�B	I�B	I�B	J#B	J#B	J=B	J=B	J#B	J	B	J	B	I�B	G�B	<�B	�B�|BּB�B��B��B��B�9B��B��B�;B�B�B��B� B՛B��B	B	)�B	ZB	g8B	�mB	�~B
2B
K�B
YB
�qBvB��B�UB~�B�B��B��B��B�{B��B��B�^B��B��B�!B�]B��B�aB�oBc BG+B)�BB
�-B
��B
�"B
kB
^jB
Q�B
,�B
$tB
�B
 B	�B	�6B	ҽB	��B	��B	q'B	e�B	R�B	2B	 B	�B	�B	�B	zB	tB	4B	&2B	/iB	NB	h>B	l�B	gmB	RTB	4B	&LB	B	[B	�B	/5B	@B	N�B	W
B	R B	U�B	T,B	b�B	oiB	~BB	��B	��B	��B	��B	�B	�3B	��B	�?B	�LB	�"B	ðB	�+B	�.B	�.B	�B	�7B	ݲB	ޞB	޸B	�B	��B	�B	�;B	��B	�hB	�B	�fB	�B
 4B
B
9B
�B
�B
B

#B
�B
[B
mB
B
#B
=B
�B
'RB
)�B
)�B
*eB
,"B
/B
2B
3B
3�B
3�B
4�B
6�B
8B
7�B
8RB
8�B
9>B
9�B
:�B
:�B
<jB
<B
<�B
<�B
=B
<�B
=�B
=�B
=qB
=VB
=�B
=�B
=�B
?HB
="B
=qB
=�B
=qB
=B
<�B
=B
<B
;�B
;B
;JB
;JB
;JB
:�B
:�B
:�B
:�B
:�B
9	B
8�B
6�B
5�B
5tB
5tB
5?B
5ZB
5%B
4�B
5B
4nB
3�B
3�B
3B
1�B
1�B
1�B
1vB
1'B
1B
0�B
/�B
/�B
.�B
.�B
./B
-]B
,�B
,=B
+�B
)�B
(�B
(sB
(�B
(XB
(sB
'B
&�B
%�B
%�B
%FB
$�B
#�B
#�B
#�B
"�B
"hB
!-B
 BB
�B
dB
xB
�B
)B
?B
�B

B
$B
YB
�B
�B
EB
�B
kB
�B
KB
EB
YB
B
,B
�B
�B
�B

�B
mB
oB
�B
�B
�B
�B	��B	�xB	��B	�9B	�-B	�cB	��B	�XB	��B	��B	�GB	�B	��B	�B	�iB	�CB	�B	�B	�sB	�B	�RB	�=B	��B	�B	�B	�B	�wB	�B	�B	�B	�B	�B	��B	�+B	��B	�lB	�B
�B
	B
	�B
B
B
�B

=B
�B
�B
�B
�B
B
�B
B
B
B
B
B
6B
6B
B
B
B
6B
B
�B
B
B
dB

�B
	�B
�B
B
�B
�B
B
�B
�B
_B
KB
B

�B
�B
�B
�B
<B
�B
(B
B
.B
�B
bB
bB
�B
B
�B
�B

XB
�B
B
�B
xB
6B
�B
B

�B

	B

#B

XB
�B
6B
�B
0B
�B
�B
�B
B
<B
(B
�B
�B
4B
�B
 B
oB
�B
�B
B
@B
&B
B
&B
&B
&B
@B
[B
@B
@B
�B
FB
�B
�B
�B
�B
�B
�B
�B
2B
�B
�B
2B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
KB
eB
B
�B
�B
KB
=B
B
�B
/B
dB
dB
/B
/B
B
B
�B
�B
B
�B
B
;B
�B
�B
�B
 BB
 \B
 BB
 \B
 �B
 vB
!-B
!�B
!�B
!|B
#B
#nB
#TB
#�B
$B
$B
$&B
#�B
$@B
$�B
%B
%FB
%`B
%`B
%�B
%�B
%�B
&LB
&�B
&�B
&�B
&�B
'B
'B
'B
'RB
'B
'B
'mB
'8B
'B
'B
&fB
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
&�B
'B
&�B
'B
'mB
(>B
(sB
(�B
(�B
(�B
)B
)B
)DB
)yB
)*B
)_B
*0B
)�B
*B
+QB
+�B
+�B
+�B
+�B
+�B
,"B
,qB
,�B
-)B
-�B
-�B
.B
-�B
.IB
.�B
.�B
/B
.�B
/ B
/B
/�B
/�B
1'B
1[B
1[B
1vB
1�B
1�B
1�B
1�B
2-B
2�B
33B
3B
33B
33B
3MB
3hB
3hB
3�B
4nB
5ZB
5�B
5�B
6FB
7�B
7�B
8B
8B
7�B
7�B
8B
9>B
:B
:B
:*B
:�B
:�B
;B
;0B
;dB
;dB
;�B
;�B
<PB
<PB
<PB
<�B
<�B
<�B
<�B
<�B
=<B
=�B
=�B
>B
>BB
>BB
>]B
>�B
>�B
>�B
?.B
?}B
?�B
?�B
?�B
@ B
@B
@�B
A B
A;B
AUB
A�B
A�B
A�B
A�B
B'B
B�B
B�B
B�B
B�B
B�B
C-B
CB
C-B
C{B
C�B
C�B
DB
DB
DgB
D�B
D�B
D�B
EB
EmB
E9B
E�B
F�B
G_B
GzB
G�B
G�B
G�B
HfB
H�B
H�B
IB
I�B
IlB
I�B
I�B
J	B
J	B
J	B
JXB
J�B
KB
KxB
KxB
KxB
K�B
K�B
LB
K�B
L0B
L~B
LdB
LdB
L�B
MB
M�B
NVB
N�B
N�B
O(B
OvB
O�B
PHB
PbB
P}B
P�B
Q B
QB
Q�B
Q�B
Q�B
Q�B
RoB
RoB
RoB
RTB
RoB
R�B
R�B
R�B
R�B
RoB
R�B
R�B
R�B
S[B
S�B
S�B
S�B
S�B
S�B
TFB
TaB
T{B
T�B
UgB
U2B
UMB
UMB
U�B
U�B
U�B
V9B
VmB
V�B
V�B
W$B
WYB
WYB
W�B
W�B
W�B
W�B
W�B
XyB
XyB
X�B
YB
YKB
YeB
YKB
YeB
Y�B
Y�B
Z�B
[	B
[	B
[#B
[=B
[=B
[=B
[�B
\CB
\�B
]B
]�B
^B
^5B
^OB
^�B
_�B
_�B
_�B
_�B
_�B
`B
_�B
_�B
`�B
`�B
aB
aHB
a|B
a�B
a�B
a�B
a�B
a�B
bB
b4B
bNB
b�B
b�B
c B
c B
cTB
cnB
c�B
dB
d@B
dZB
dtB
d�B
d�B
eFB
ezB
e�B
e�B
e�B
fLB
ffB
f�B
f�B
f�B
g8B
g8B
gmB
gmB
g�B
g�B
h>B
h>B
h>B
h�B
h�B
i*B
i*B
iDB
i_B
iyB
i�B
i�B
i�B
i�B
jB
j0B
j0B
j�B
j�B
j�B
k6B
k�B
k�B
lB
k�B
k�B
lqB
l�B
l�B
l�B
l�B
m)B
mwB
m�B
m�B
m�B
m�B
nB
nIB
nIB
n}B
n�B
n�B
n�B
o B
oOB
oiB
o�B
o�B
pB
p;B
p�B
p�B
p�B
p�B
p�B
q'B
q[B
q�B
q�B
rB
rGB
r|B
r�B
r�B
r�B
sB
s3B
shB
s�B
s�B
tB
t9B
t9B
tnB
t�B
t�B
t�B
uB
utB
u�B
u�B
vB
vzB
vzB
v�B
wB
w2B
wLB
w�B
xB
w�B
xB
xlB
x�B
x�B
y	B
y$B
y$B
y�B
y�B
y�B
y�B
y�B
z*B
zDB
zDB
z^B
z^B
zxB
z�B
z�B
z�B
z�B
{dB
{�B
{�B
{�B
{�B
|B
|B
|PB
|�B
}<B
}B
}"B
}"B
}"B
}VB
}VB
}�B
}�B
}�B
}�B
}�B
}�B
}�B
}�B
~B
~(B
~BB
~(B
~BB
~�B
~�B
~�B
~�B
HB
cB
}B
}B
}B
�B
�B
�B
�B
�B
�OB
�iB
��B
��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<?�[<<j<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.07(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201906230051172019062300511720190623005117202207271131312022072711313120220727113131202207271534122022072715341220220727153412  JA  ARFMdecpA30a                                                                20190612033751  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190612033805  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190612033806  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190612033807  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190612033807  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190612033807  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190612033807                      G�O�G�O�G�O�                JA  ARUP                                                                        20190612035635                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190613000000  CF  PSAL_ADJUSTED_QC@��@��G�O�                JM  ARCAJMQC2.0                                                                 20190622155117  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190622155117  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023131  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063412  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818081508                      G�O�G�O�G�O�                