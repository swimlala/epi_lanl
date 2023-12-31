CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-12-09T09:39:24Z creation;2019-12-09T09:39:26Z conversion to V3.1;2022-08-02T05:11:43Z update;     
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
_FillValue                 �  ]P   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a<   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �$   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �\   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �,   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �0   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �@   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �D   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �LArgo profile    3.1 1.2 19500101000000  20191209093924  20220818091504  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_031                    2C  D   APEX                            8420                            2.11.2                          846 @��v�� 1   @����� @.�C,�zx�cT�n��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�  A��A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�33B�  B�  B�  B�  B���B�33B���B�  B���B�  B�33B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B���Bߙ�B�  B虚B���B�  B�  B�  B�  C   C�C�C  C  C
  C�fC  C  C  C  C  C  C  C  C  C   C"  C$�C%��C'�fC*  C,�C-�fC/�fC1�fC3�fC5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C[�fC]�fC`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS�fDT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� Dd��De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�C3Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�z�@���A{A!�A@��A`��A�z�A�z�A�z�A��\A���AУ�A��\A��B Q�B33BQ�B=qB =qB(=qB0=qB8=qB@G�BH(�BP{BX33B`Q�BhG�Bp\)Bxz�B�=qB�.B�#�B�.B�8RB��RB�W
B���B�
=B��)B�\B�W
B�B�.B�8RB���B���B�\B�#�B��B��B��B�(�B��B�B�{B��B��B�
=B��B�B�C \C!HC0�C�C{C
C�CC
=C
=C�C
=C�C
=C
=C�C �C"
=C$.C%�)C(  C*
C,#�C-��C0�C2  C4  C6�C8C:�C<�C>�C@\CB�CD
CF
CH{CJ�CL�CN
CP)CR�CT\CV
=CX�CZ
=C\  C^�C`Cb�Cd!HCf{Ch
=Cj
=Cl�Cn\Cp�Cr�Ct
=Cv
=Cx�Cz�C|\C~
=C��C��C�fC�C��C��C�fC��C�C�C��C�C�
=C��C�
=C��C�C��C�fC�C�fC�fC�fC�fC��C��C�fC��C��C��C�
=C��C��C��C�C�C��C��C�fC�C��C�
=C��C��C�C��C�fC�C�fC�C�fC��C��C��C�C��C��C��C��C��C�
=C�fC��C�C�fC�C��C�C��C�C��C�fC��C��C��C�
=C�fC��C�
=C�C��C��C�fC�fC�C�C��C��C��C�fC�
=C��C�fC�C��C��C��C��C�
=C�
=C��C�C��C�fC��C��C�C��C�fC��C��C��C��C��C�fC��C��C�
=C�
=C��C�fC�C�fC��C�
=C��C�C�D 3D ��D�D�{D{D��D�D��D3D��D�D�3DD�{D�D��D�D��D	�D	��D
�D
��D3D�3DD��D�D�fD{D��D�D��D3D�3D�D�{DD��D�D��DD�{D3D�{D�D�3D3D�3D3D��D3D�3D3D��D�D��D{D�{DD�D�D�DD�fD �D ��D!�D!�3D"�D"�{D#D#�D$�D$�3D%D%�D&{D&�D'�D'�3D(3D(�3D)�D)��D*�D*�3D+�D+��D,fD,��D-�D-��D.�D.��D/HD/��D0{D0��D13D1��D23D2�{D3{D3�3D43D4�D5{D5�{D6D6�{D7�D7�3D83D8��D9�D9��D:3D:�3D;D;�3D<�D<�3D=�D=�D>�D>��D?D?�{D@�D@��DA3DA��DB�DB�DCDC�DD�DD��DE3DE�3DF�DF�{DG3DG��DH3DH��DI{DI�{DJ�DJ�3DK�DK�3DL�DL�{DMDM��DNfDN�{DOHDO�HDP�DP��DQ3DQ�3DR3DR��DS{DS��DT
DT�fDU�DU�3DVDV��DW�DW�3DX3DX�3DY3DY�3DZ3DZ�3D[{D[�{D\�D\�{D]�D]��D^3D^�{D_�D_��D`�D`��Da{Da�3Db�Db�{Dc�Dc�{Dd3Dd�HDe  De��Df �Df��Dg�Dg�{Dh�Dh��Di�Di�3Dj{Dj�{DkDk�{Dl�Dl�3Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr3Dr��Ds{Ds�DtfDt�{Du�Du��Dv3Dv�3Dw�Dw�Dx3Dx��Dy�Dy��DzDz��D{�D{��D|
D|��D}{D}�D~D~��D3D�3D��D�A�D���D���D� �D�@�D���D��=D��D�A�D���D�D�=D�AHD���D��=D��D�A�D��HD��HD��D�A�D���D���D��D�B�D��=D���D�HD�@�D��=D�D��D�A�D��HD��HD�HD�@�D��HD���D�=D�A�D���D�D��D�A�D��HD���D��D�A�D���D���D�  D�A�D��3D��=D��D�B�D���D���D� �D�@�D���D��HD��D�A�D��HD���D� �D�@�D��HD��HD��D�A�D���D���D��D�B�D���D��HD�=D�A�D���D���D� �D�B�D���D���D��D�AHD���D�D��D�B=D��3D���D��D�B=D��3D���D�HD�A�D��=D�D�=D�A�D���D���D�=D�B�D��=D���D� �D�A�D���D���D�=D�A�D���D��HD��D�A�D���D���D��D�A�D��=D��=D��D�B=D���D���D��D�A�D���D���D�HD�AHD���D��HD��D�B�D���D�D��D�B�D��=D��=D��D�B�D���D���D�=D�A�D���D��3D�3D�A�D���D���D�HD�AHD��=D���D� �D�B=D���D���D��D�@�D���D�D�=D�B=D���D�D��D�B�D���D���D��D�B=D��RD���D��D�B�D���D���D��D�@�D��HD��=D��D�B=D��=D�D�=D�B=D���D���D��D�B=D��HD���D�=D�A�D���D��=D��D�AHD��HD���D� �D�A�D���D���D��D�B=D��=D��=D��D�B=D��HD���D�=D�A�D��=D���D�HD�B=D���D���D�HD�@�D���D���D��D�B=D���D��3D�=D�AHD���D��RD�=D�A�D��RD���D��D�B�D���D���D�HD�A�D���D���D� �D�AHD���D��=D��D�B�D���D��HD�HD�@�D�D�D��D�B=DÁ�D���D� �D�AHDā�D���D��D�C�DŃ3D���D�HD�AHDƁ�D���D��D�B=Dǁ�D���D�=D�B=Dȁ�D���D��D�A�Dɂ=D���D�HD�A�Dʁ�D��HD��D�A�DˁHD���D�HD�A�D́HD���D��D�A�D́�D��=D��D�@�D΁�D���D��D�B=Dς=D��HD��D�B�DЂ=D���D�=D�A�Dс�D��=D�HD�B=D҃3D�ÅD��D�AHDӁHD��HD��D�A�DԁHD��HD�HD�A�DՁ�D���D��D�AHDր�D��HD�=D�A�Dׁ�D��HD�HD�@�D؀�D���D��D�B=Dق�D��HD� RD�@�Dځ�D���D�HD�B�Dۂ�D��HD�HD�AHD܁�D���D��D�@�D݁�D��=D��D�C�Dނ�D���D� �D�@�D߀�D���D��D�C3D���D�D��D�B=D��D���D�HD�AHD⁚D���D�=D�C�DヅD��=D��D�A�D䁚D�D�=D�A�D�=D���D��D�@�D�=D���D�HD�@�D��D���D� �D�AHD肏D���D�HD�A�D��D��=D��D�B=D��D���D��D�A�D�=D��=D�=D�A�D��D���D�=D�B�D�HD��HD��D�AHDD��HD�=D�B�D�=D���D��D�B�D���D�D��D�@�D�=D���D��D�@�D�D�D��D�@�D��D���D��D�B�D�D���D��D�B�D���D�D�3D�C3D��=D��=D��D�A�D���D���D��D�B�D��3D���D� �D�A�D���D�D��D�B�D���D�D�=D�@R11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�m]A�wfA�|�A�f2A��NAخ�Aج�AخIAة�Aء�A؟�Aؠ�A؟�A؟VA؝~A؜CA؝IA؝�A؝�A؜Aؙ�Aؕ�AؑhA،�A؋A؋xA؋xA؋DA؎�Aؓ�Aؕ�AؗYAؖ�Aؓ�A�}�AםA�@�A� �AΒ�A�T�A�o�A��A�d�A�HKA�	�A���A���A��A���A���A��MA���A��{A�MjA��dA��iA�x�A�GA�}VA��[A�S&A��4A�sA��DA���A��8A�W
A���A�_A�}�A�_;A�#A��/A���A��A��A��A���A~�}Ay�ZAw��Av�Au �An��Ah�Ab��A]N�AZOAW�)AT5?AP$�AG�AEACy�AA�A?�A>c A=��A<�A<p�A<A;�A:9�A9S&A9�A8R�A7��A5�A3�A3E�A1�KA0]�A/�?A.�tA-��A-;A,kQA+�ZA+��A*�A*U2A)YKA'/A%n/A#W?A!�8A![WA�hA�oA�	A�AoAGA��A�QA��A��A�AMjA'RA&�A~�A�HA�Ac�A��A��A�AaAp;ACA1'AA!�A�=AF�A1A�wAdZA�A�A4�A��A�Ae,A7LA{A�A�fA��Ag�A�;A�*As�A iA��A
�ZA
�XA
OvA	ԕA	N�A�pA�:A4nA�'A�A��A|AC�AAĜA�YA�	A�AXA�pA�.Aw�AZ�A	�AW�A�A�FAzxA�A �?A �A �@A Mj@��F@�33@��@���@�� @��@�A @��Y@�B[@�D�@���@�w2@���@��h@�}�@�*0@��p@�.�@��@�A�@��@�@��@�l�@��)@�6@�y>@� �@���@�T�@�!�@�@��@��@�u�@��
@�&�@�q@� @��@�o�@��@��@�YK@���@���@���@ߋ�@���@�y>@�6�@��Z@�Ɇ@�,=@ۗ$@�1�@ڧ�@���@�m]@�!�@�ѷ@؟�@�y>@�N�@�$�@��}@�+@��@��Q@�rG@�A�@ԫ6@�.�@�	@�S&@��@�M@��@ѷ�@�/�@��@��@Б @�C-@��@��E@��@�RT@���@̑�@��@�l�@ʸR@��D@ɘ�@��@ȵ�@Ȓ�@�q@�*�@ǨX@�E9@��p@Ɓo@��@�rG@�%@�s�@�O�@���@¾�@�e@��@�GE@�	@���@���@���@�Ov@��D@��{@��@�c�@��@��@���@�J#@���@��2@���@���@���@�z�@�/�@��@���@�dZ@��@��F@��.@���@�,�@��X@���@�H@�7@��@���@�8@��@��j@�W�@�($@��@���@�zx@�L�@��"@��z@�~(@�`�@��@��a@���@��@@�O�@��@���@��)@��@�K^@��@�g�@�V@��@��@�B[@���@�F�@�/�@��5@�}V@�:�@�7@��#@���@�A�@���@��m@���@�z�@�Z�@�#:@��N@�W?@� i@���@�h�@�ݘ@�O@��@���@�YK@��@��N@�^�@���@�y>@�YK@�'R@��r@���@�`B@�E9@��v@�W�@�0U@�@�u@���@���@��S@�s@��@��e@�^5@�@��@���@�l�@�0�@��y@��.@�Ov@�#:@��m@���@�\�@��f@���@�\�@� �@��Z@�c@�N<@�+�@���@�u%@�C�@�4@��@��@��	@�t�@�<6@�ȴ@���@�h
@�7�@��@��@��j@���@�k�@�Y�@��@�҉@��U@���@�� @�kQ@�Q�@�4n@�'R@���@�~�@�e�@�O�@�F�@�A @�=@�/@��M@���@�J@��@��j@��d@���@���@�j@��@��?@���@�c @�C�@��@���@���@�e,@��@��@�ں@���@���@���@��+@��@�p�@�=�@��@�V�@�@��@���@��M@�S&@���@�z�@�C�@�J@�ݘ@��K@���@��0@�rG@��@�Ĝ@���@�`�@��@���@�v`@�O@���@���@���@�?@�{@���@�H�@��2@�~�@�9X@�W�@��+@�{J@�K�@�S&@�J#@��@���@�J�@�8�@�2�@�0U@�(�@�"h@��@�	@���@��w@���@�^�@�;@���@�]d@��@��.@��j@��S@�/�@��@��1@�9X@�+k@��@��@o�@$t@~��@}�D@}0�@|��@|�$@|��@|*�@{�@{��@{@O@z҉@z~�@zH�@y��@y5�@x�u@w��@wS@v�}@v?@u��@t��@tC-@s�a@s�:@sJ#@r�@r�F@rl�@r�@q�S@qV@p7�@o>�@nv�@nd�@nR�@nO@m��@l�@l��@l �@k��@k)_@j�@i�@i\�@h�E@hw�@g��@gqv@f�,@fxl@f!�@ej@e!�@e�@d�/@d��@dC-@c�Q@b��@b{�@b^5@b�@a��@aIR@`��@`�@`A�@`7@_�]@_�a@_��@_O@^�@^��@^~�@^?@]�@]�t@]Y�@]�@\�@\��@\[�@\,=@[��@[\)@[@O@[&@Z�B@Zff@Z?@Z�@Y��@Y:�@X��@X/�@W��@Wv`@W@V�1@Vxl@VJ�@U�.@Uzx@T��@T�E@Ty>@S��@S�q@S��@S8@R1�@Q�>@Q��@QrG@Q�@P�)@Py>@P<�@P�@O�@O�f@N�2@N�B@N�h@Ni�@M�@M�@M�@Mc�@M@L�)@LM@K��@Kl�@K)_@J�2@J��@JkQ@J�@I�@IN<@Hی@HK^@H2�@G��@G_p@GC�@G�@F��@Fd�@F!�@E��@E�h@EN<@E2a@D��@D��@Dc�@D�@C��@C.I@B��@B��@BTa@A�)@AF@@�K@@��@@G@?�Q@?�
@?�$@?8@>��@>��@>$�@=�-@=�~@=N<@<��@<7@;� @;�k@;Z�@:�@:͟@:�R@:�+@:�@9��@9j@9IR@8�E@87�@7�@7�@7S�@6�@6�L@6V@5�D@5T�@4�?@4��@4�.@4I�@3خ@3��@3,�@2�A@2	@1�@1��@1��@1[W@1�@0�5@0��@0(�@/�@@/;d@/�@.�c@.��@.u%@.?@.	@-�H@-�"@-G�@-*0@,ѷ@,��@,w�@,PH@,,=@+�6@+{J@+/�@*�"@*��@*h
@*Ov@*#:@)�@)��@)��@)��@)^�@)�@(��@(Ɇ@(��@(�@(m�@(*�@'��@'�;@'�;@'�F@'~�@'dZ@'U�@'�@&��@&@�@&
�@%o @$�@$��@$�@$e�@$>B@$(�@$�@#�]@#�&@#��@#�@"�@"�]@"��@"L0@!�^@!��@!�'@!�"@!+�@ ��@ �@ h�@ 'R@ x@�m@��@=@��@�A@0U@�@�.@ԕ@�S@Q�@�	@�@�_@Ft@!@��@�k@�@�2@��@�@��@d�@W�@5?@ԕ@��@o @`B@0�@��@�@w�@Q�@�@�Q@�	@O@)_@�M@�,@��@�b@��@^5@Q@C�@3�@
�@�@a�@O�@=�@;@��@�@oi@M@1@ݘ@_p@�@�@��@Q@($@J@ԕ@��@�"@rG@O�@*0@�@��@�p@��@H@ �@��@�K@�@s@\)@.I@�@�@�@��@��@W�@@�@&�@e@�d@f�@�@��@ی@��@��@r�@c�@PH@@�@ݘ@˒@��@t�@e�@W?@;d@o@
�@
͟@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�m]A�wfA�|�A�f2A��NAخ�Aج�AخIAة�Aء�A؟�Aؠ�A؟�A؟VA؝~A؜CA؝IA؝�A؝�A؜Aؙ�Aؕ�AؑhA،�A؋A؋xA؋xA؋DA؎�Aؓ�Aؕ�AؗYAؖ�Aؓ�A�}�AםA�@�A� �AΒ�A�T�A�o�A��A�d�A�HKA�	�A���A���A��A���A���A��MA���A��{A�MjA��dA��iA�x�A�GA�}VA��[A�S&A��4A�sA��DA���A��8A�W
A���A�_A�}�A�_;A�#A��/A���A��A��A��A���A~�}Ay�ZAw��Av�Au �An��Ah�Ab��A]N�AZOAW�)AT5?AP$�AG�AEACy�AA�A?�A>c A=��A<�A<p�A<A;�A:9�A9S&A9�A8R�A7��A5�A3�A3E�A1�KA0]�A/�?A.�tA-��A-;A,kQA+�ZA+��A*�A*U2A)YKA'/A%n/A#W?A!�8A![WA�hA�oA�	A�AoAGA��A�QA��A��A�AMjA'RA&�A~�A�HA�Ac�A��A��A�AaAp;ACA1'AA!�A�=AF�A1A�wAdZA�A�A4�A��A�Ae,A7LA{A�A�fA��Ag�A�;A�*As�A iA��A
�ZA
�XA
OvA	ԕA	N�A�pA�:A4nA�'A�A��A|AC�AAĜA�YA�	A�AXA�pA�.Aw�AZ�A	�AW�A�A�FAzxA�A �?A �A �@A Mj@��F@�33@��@���@�� @��@�A @��Y@�B[@�D�@���@�w2@���@��h@�}�@�*0@��p@�.�@��@�A�@��@�@��@�l�@��)@�6@�y>@� �@���@�T�@�!�@�@��@��@�u�@��
@�&�@�q@� @��@�o�@��@��@�YK@���@���@���@ߋ�@���@�y>@�6�@��Z@�Ɇ@�,=@ۗ$@�1�@ڧ�@���@�m]@�!�@�ѷ@؟�@�y>@�N�@�$�@��}@�+@��@��Q@�rG@�A�@ԫ6@�.�@�	@�S&@��@�M@��@ѷ�@�/�@��@��@Б @�C-@��@��E@��@�RT@���@̑�@��@�l�@ʸR@��D@ɘ�@��@ȵ�@Ȓ�@�q@�*�@ǨX@�E9@��p@Ɓo@��@�rG@�%@�s�@�O�@���@¾�@�e@��@�GE@�	@���@���@���@�Ov@��D@��{@��@�c�@��@��@���@�J#@���@��2@���@���@���@�z�@�/�@��@���@�dZ@��@��F@��.@���@�,�@��X@���@�H@�7@��@���@�8@��@��j@�W�@�($@��@���@�zx@�L�@��"@��z@�~(@�`�@��@��a@���@��@@�O�@��@���@��)@��@�K^@��@�g�@�V@��@��@�B[@���@�F�@�/�@��5@�}V@�:�@�7@��#@���@�A�@���@��m@���@�z�@�Z�@�#:@��N@�W?@� i@���@�h�@�ݘ@�O@��@���@�YK@��@��N@�^�@���@�y>@�YK@�'R@��r@���@�`B@�E9@��v@�W�@�0U@�@�u@���@���@��S@�s@��@��e@�^5@�@��@���@�l�@�0�@��y@��.@�Ov@�#:@��m@���@�\�@��f@���@�\�@� �@��Z@�c@�N<@�+�@���@�u%@�C�@�4@��@��@��	@�t�@�<6@�ȴ@���@�h
@�7�@��@��@��j@���@�k�@�Y�@��@�҉@��U@���@�� @�kQ@�Q�@�4n@�'R@���@�~�@�e�@�O�@�F�@�A @�=@�/@��M@���@�J@��@��j@��d@���@���@�j@��@��?@���@�c @�C�@��@���@���@�e,@��@��@�ں@���@���@���@��+@��@�p�@�=�@��@�V�@�@��@���@��M@�S&@���@�z�@�C�@�J@�ݘ@��K@���@��0@�rG@��@�Ĝ@���@�`�@��@���@�v`@�O@���@���@���@�?@�{@���@�H�@��2@�~�@�9X@�W�@��+@�{J@�K�@�S&@�J#@��@���@�J�@�8�@�2�@�0U@�(�@�"h@��@�	@���@��w@���@�^�@�;@���@�]d@��@��.@��j@��S@�/�@��@��1@�9X@�+k@��@��@o�@$t@~��@}�D@}0�@|��@|�$@|��@|*�@{�@{��@{@O@z҉@z~�@zH�@y��@y5�@x�u@w��@wS@v�}@v?@u��@t��@tC-@s�a@s�:@sJ#@r�@r�F@rl�@r�@q�S@qV@p7�@o>�@nv�@nd�@nR�@nO@m��@l�@l��@l �@k��@k)_@j�@i�@i\�@h�E@hw�@g��@gqv@f�,@fxl@f!�@ej@e!�@e�@d�/@d��@dC-@c�Q@b��@b{�@b^5@b�@a��@aIR@`��@`�@`A�@`7@_�]@_�a@_��@_O@^�@^��@^~�@^?@]�@]�t@]Y�@]�@\�@\��@\[�@\,=@[��@[\)@[@O@[&@Z�B@Zff@Z?@Z�@Y��@Y:�@X��@X/�@W��@Wv`@W@V�1@Vxl@VJ�@U�.@Uzx@T��@T�E@Ty>@S��@S�q@S��@S8@R1�@Q�>@Q��@QrG@Q�@P�)@Py>@P<�@P�@O�@O�f@N�2@N�B@N�h@Ni�@M�@M�@M�@Mc�@M@L�)@LM@K��@Kl�@K)_@J�2@J��@JkQ@J�@I�@IN<@Hی@HK^@H2�@G��@G_p@GC�@G�@F��@Fd�@F!�@E��@E�h@EN<@E2a@D��@D��@Dc�@D�@C��@C.I@B��@B��@BTa@A�)@AF@@�K@@��@@G@?�Q@?�
@?�$@?8@>��@>��@>$�@=�-@=�~@=N<@<��@<7@;� @;�k@;Z�@:�@:͟@:�R@:�+@:�@9��@9j@9IR@8�E@87�@7�@7�@7S�@6�@6�L@6V@5�D@5T�@4�?@4��@4�.@4I�@3خ@3��@3,�@2�A@2	@1�@1��@1��@1[W@1�@0�5@0��@0(�@/�@@/;d@/�@.�c@.��@.u%@.?@.	@-�H@-�"@-G�@-*0@,ѷ@,��@,w�@,PH@,,=@+�6@+{J@+/�@*�"@*��@*h
@*Ov@*#:@)�@)��@)��@)��@)^�@)�@(��@(Ɇ@(��@(�@(m�@(*�@'��@'�;@'�;@'�F@'~�@'dZ@'U�@'�@&��@&@�@&
�@%o @$�@$��@$�@$e�@$>B@$(�@$�@#�]@#�&@#��@#�@"�@"�]@"��@"L0@!�^@!��@!�'@!�"@!+�@ ��@ �@ h�@ 'R@ x@�m@��@=@��@�A@0U@�@�.@ԕ@�S@Q�@�	@�@�_@Ft@!@��@�k@�@�2@��@�@��@d�@W�@5?@ԕ@��@o @`B@0�@��@�@w�@Q�@�@�Q@�	@O@)_@�M@�,@��@�b@��@^5@Q@C�@3�@
�@�@a�@O�@=�@;@��@�@oi@M@1@ݘ@_p@�@�@��@Q@($@J@ԕ@��@�"@rG@O�@*0@�@��@�p@��@H@ �@��@�K@�@s@\)@.I@�@�@�@��@��@W�@@�@&�@e@�d@f�@�@��@ی@��@��@r�@c�@PH@@�@ݘ@˒@��@t�@e�@W?@;d@o@
�@
͟@
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B}VB|�B|�B{�BzxBz�B}�B�4B��B��B��B�?B�zB��B�KB��B�	B��B��B��B��B��B�6B�6B�B�B�B�BB�bB�B��B�&B�B��B��B�B�B	z�B	�B	��B
2�B
raB
�kB
��B
��B
��B
��B
�BBB(�B%`BB �BB@B
��B
�}B
XEB
YeB
U�B
X�B
v�B
��B
}�B
|6B
� B
l�B
NB
=�B
-�B
,WB
+�B
'mB
fB	��B	ˬB	�iB	�UB	�'B	�OB	�{B	��B	g�B	L�B	C�B	>�B	BAB	CaB	G�B	F�B	B�B�tB�
B��B�YB�B߾B��B	�B	�B	B	�B	+B	�B	eB	!bB	'B	%�B	(>B	"NB	�B	;B	�B	$�B	-)B	D�B	S�B	ZB	e�B	m�B	gRB	d&B	X+B	R B	T�B	T,B	UB	c�B	k�B	tnB	~]B	�GB	� B	�UB	�SB	��B	�B	��B	��B	��B	��B	�9B	�YB	��B	��B	�NB	�B	��B	��B	��B	�B	��B	�|B	�|B	��B	��B	�-B	��B	��B	�MB	��B	�3B	�9B	�nB	��B	�%B	��B	�B	��B	�B	��B	��B	�JB	��B	�*B	��B	�^B	��B	�xB	�BB	��B	�}B	��B	�]B	��B	��B	�]B	��B	�UB	�B	��B	ĶB	��B	�EB	�B	�B	��B	�B	�=B	�RB	�fB	ǮB	�B	��B	ɺB	��B	�B	��B	��B	�UB	�;B	��B	��B	�iB	�oB	�'B	B	�{B	�3B	��B	�XB	�B	�B	�VB	��B	�VB	�B	͹B	�VB	��B	��B	��B	ѷB	�2B	�
B	��B	ؓB	�B	��B	�=B	�)B	�)B	��B	�=B	��B	یB	�)B	�)B	��B	ܒB	�xB	�CB	��B	�IB	��B	�B	�5B	�B	�OB	ߤB	�BB	�vB	�B	�B	�-B	��B	�B	�TB	�B	�B	�B	�B	��B	�,B	�B	��B	�B	�B	�mB	�8B	�B	�B	��B	�B	�KB	�B	�B	��B	�B	�B	�KB	�B	�B	�B	�B	��B	�"B	��B	��B	�/B	��B	�B	� B	��B	��B	��B	� B	�OB	�iB	��B	��B	��B	�vB	�[B	�GB	��B	��B	��B	��B	��B	�nB	�B	��B	��B	�tB	�B	�`B	��B	�B	�fB	�8B	��B	�$B	��B	��B	�B	�B	�JB	��B	�PB	��B	��B	��B	�PB	��B	��B	��B	��B	��B	��B	�qB	��B	�B	��B	��B	��B	��B	�wB	�B	��B
 �B
�B
B
B
�B
�B
�B
�B
�B
GB
aB
�B
3B
MB
B
�B
B
SB
�B
�B
�B
�B
B
+B
B
�B
�B
	B
	RB
	�B
	�B

XB

XB
B
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
�B
pB
�B
pB
VB
VB
�B
B
.B
�B
\B
BB
�B
�B
}B
�B
�B
�B
�B
&B
[B
[B
�B
�B
�B
B
�B
�B
B
2B
MB
MB
MB
gB
�B
9B
SB
mB
�B

B
�B
�B
�B
EB
EB
EB
B
�B
B
7B
B
�B
B
�B
7B
kB
kB
�B
�B
�B
WB
qB
�B
CB
~B
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
�B
�B
jB
B
!B
;B
B
B
�B
�B
;B
�B
 vB
 vB
 \B
 vB
 �B
!B
!HB
!�B
!HB
!B
!-B
!bB
!�B
"�B
"�B
#:B
#�B
#�B
$B
$ZB
$@B
$B
#�B
$@B
$B
$ZB
%`B
&2B
&2B
&LB
&2B
&2B
&fB
&�B
'B
'�B
'�B
'�B
(>B
(sB
(�B
)DB
)�B
)�B
)�B
)�B
*KB
*�B
+�B
,WB
,�B
,�B
-)B
-�B
-�B
.�B
.�B
.IB
-�B
.B
/�B
/iB
/5B
/�B
0UB
0!B
/�B
/�B
/�B
0;B
0UB
0UB
0;B
0;B
0;B
0!B
0�B
1[B
1AB
0�B
0oB
/�B
0UB
1B
1vB
1vB
1�B
1�B
1[B
1�B
1�B
1�B
1�B
1�B
1�B
2B
2|B
2�B
3�B
3�B
3�B
3�B
4B
4TB
4TB
4nB
4�B
4�B
4�B
5�B
5tB
5�B
6�B
6�B
7LB
7LB
7�B
8lB
8�B
8�B
8�B
8�B
9XB
9�B
9�B
9�B
:*B
:xB
;0B
;�B
<�B
<jB
<�B
<�B
<�B
=<B
="B
=�B
>B
>(B
>]B
>�B
?B
?}B
?�B
@B
@OB
@�B
@�B
@�B
A�B
A�B
A�B
A�B
BB
BB
BAB
C{B
CGB
CaB
C{B
C�B
DB
D3B
D�B
D�B
D�B
D�B
D�B
D�B
EB
E�B
E�B
E�B
FB
F%B
FYB
F�B
F�B
F�B
G+B
G+B
G+B
G�B
G�B
G�B
G�B
HB
HKB
H�B
H�B
H�B
H�B
I7B
I�B
I�B
I�B
I�B
J	B
I�B
I�B
J	B
J=B
J#B
J	B
JXB
J�B
KB
K)B
K�B
L0B
LJB
L~B
L�B
L�B
L�B
M6B
M�B
M�B
N"B
N<B
NVB
NVB
NVB
NpB
N�B
OB
OBB
O�B
O�B
O�B
PB
PbB
P}B
P�B
P�B
P�B
P�B
Q4B
Q�B
Q�B
R B
RTB
RoB
R�B
SB
R�B
SB
S[B
S�B
SuB
S�B
S�B
T,B
TB
T{B
TaB
TaB
T{B
T�B
UB
U2B
UgB
U�B
U�B
V9B
V�B
V�B
W?B
W?B
W
B
W?B
WsB
W�B
W�B
XB
X_B
XEB
X_B
YB
YB
Y�B
Y�B
Y�B
ZQB
Z7B
ZQB
ZkB
Z�B
Z�B
[=B
[	B
[�B
\B
\)B
\]B
\�B
\�B
\�B
]/B
]IB
]�B
^OB
^jB
^OB
^�B
^�B
_B
_!B
_�B
_�B
`'B
`B
`'B
`\B
`�B
`�B
`�B
a-B
a�B
a�B
bB
bB
b4B
b�B
b�B
b�B
b�B
c B
cTB
cTB
c�B
c�B
c�B
dB
c�B
dtB
d�B
d�B
eB
ezB
e�B
e�B
e�B
fB
fB
fB
f2B
f�B
f�B
f�B
f�B
f�B
gB
gB
gmB
g�B
g�B
g�B
g�B
g�B
g�B
g�B
h$B
hsB
h�B
h�B
i_B
i�B
i�B
i�B
jKB
jeB
jKB
jeB
jeB
jeB
jB
kQB
kQB
kQB
k�B
k�B
lqB
lWB
l=B
l=B
l�B
l�B
l�B
m)B
m]B
m]B
mwB
m�B
m�B
ncB
n�B
o B
o B
oB
o5B
oOB
o�B
p!B
p;B
pUB
p�B
p�B
p�B
q'B
q�B
q�B
q�B
rB
rB
r-B
r-B
rGB
r�B
r�B
sMB
sMB
s�B
s�B
s�B
tB
tB
tnB
t�B
t�B
t�B
u%B
uZB
utB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
v+B
v�B
v�B
v�B
v�B
wB
w2B
wfB
w�B
xB
xB
xB
x�B
x�B
y	B
y>B
y�B
y�B
y�B
z*B
z*B
z*B
z^B
z^B
z�B
z�B
z�B
z�B
{0B
{dB
{B
{�B
{�B
|B
|6B
|PB
|�B
|�B
|�B
|�B
}B
}<B
}VB
}qB
}�B
}�B
}�B
~BB
~wB
~�B
~�B
~�B
~�B
~�B
~�B
B
cB
�B
�B
�B
�B
�B
�B
�B
�OB
�iB
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B}VB|�B}<B|�Bz�Bz�B}�B�4B��B��B��B�?B�zB��B�KB��B�	B��B��B��B��B��B�6B�6B�B�B�B�BB�bB� B��B�[B�{B��B�#BðB�"B	��B	�B	��B
6+B
t�B
��B
��B
�DB
��B
�[B
�B�B�B,�B'�B�B#B �B�B	lB
�yB
^5B
]�B
[�B
]�B
zxB
��B
��B
.B
�B
tB
R�B
BB
0;B
-�B
/�B
.IB
BB	�0B	��B	�B	�?B	��B	��B	��B	�:B	o5B	T�B	I�B	B�B	E�B	H�B	NB	N�B	B��B��B�B՛B�EB�B�BB�<B	�B	YB		B	1B	KB	7B	�B	#�B	($B	'�B	)�B	#TB	!B	 vB	 �B	%�B	-�B	EB	T{B	[=B	gmB	p�B	i�B	f�B	Y�B	S@B	V�B	UB	U�B	dZB	l�B	u�B	B	�B	��B	�B	��B	��B	�lB	��B	��B	��B	�:B	��B	�EB	�+B	�?B	��B	�B	�}B	�'B	�`B	�2B	�tB	��B	��B	�3B	��B	��B	�B	��B	��B	��B	��B	�nB	��B	�%B	�%B	�%B	��B	��B	�RB	��B	��B	��B	��B	��B	��B	��B	�xB	�0B	��B	��B	�OB	�.B	��B	��B	��B	��B	�4B	��B	��B	�3B	�9B	�tB	�zB	�KB	�fB	ȀB	��B	��B	ɠB	ȴB	�B	�EB	��B	�#B	˒B	�=B	��B	��B	��B	��B	�oB	��B	��B	��B	�AB	��B	��B	�B	�B	�rB	�dB	�jB	οB	�(B	ΥB	�VB	�<B	�(B	�vB	�bB	ӏB	�B	ՁB	�YB	�1B	�B	��B	چB	یB	ܬB	ܒB	ܒB	�CB	ܒB	�B	�xB	ܬB	��B	ܬB	��B	��B	�~B	ݲB	�5B	�OB	ބB	ބB	�B	��B	�B	�B	�vB	�B	�|B	��B	�B	�nB	�B	�B	��B	��B	�tB	�B	�B	�8B	�B	�RB	�B	�mB	�
B	�_B	�_B	��B	�B	��B	�QB	��B	�B	��B	�B	��B	�B	�B	�B	�=B	�B	�WB	�wB	�B	�B	�}B	�5B	��B	�B	�B	�iB	�B	�B	�!B	�UB	�AB	��B	��B	��B	�B	�B	�3B	�B	�9B	��B	��B	�B	�?B	��B	�FB	��B	�B	�LB	��B	�lB	��B	�XB	�B	��B	�0B	�0B	�JB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�<B	��B	��B	��B	��B	�(B	�(B	�B	��B	��B	��B	�.B	��B
 �B
�B
'B
AB
�B
�B
�B
�B
�B
aB
{B
�B
gB
MB
MB
�B
9B
�B
?B
�B
�B
�B
_B
zB
1B
�B
�B
	RB
	�B
	�B
	�B

�B

�B
)B
�B
�B
�B
�B
�B
B
0B
�B
�B
PB
�B
�B
B
�B
pB
�B
�B
BB
bB
�B
vB
vB
�B
B
�B
 B
B
 B
�B
@B
uB
uB
�B
�B
B
aB
�B
B
MB
MB
�B
�B
gB
�B
B
mB
�B
�B

B
YB
�B
�B
+B
yB
yB
�B
KB
�B
7B
�B
7B
�B
B
B
kB
�B
�B
	B
�B
�B
�B
�B
�B
]B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
B
B
B
�B
!B
;B
;B
B
B
�B
B
�B
 BB
 �B
 vB
 \B
 �B
 �B
!HB
!�B
!�B
!|B
!B
!-B
!�B
!�B
"�B
"�B
#nB
#�B
$B
$B
$tB
$ZB
$@B
$@B
$�B
$@B
$�B
%�B
&LB
&2B
&fB
&fB
&fB
&�B
'B
'8B
(
B
'�B
'�B
(XB
(�B
(�B
)�B
)�B
)�B
*B
*B
*B
*�B
+�B
,�B
,�B
,�B
-CB
-�B
-�B
.�B
/5B
.}B
.B
.B
/�B
/�B
/5B
/�B
0UB
0;B
0!B
/�B
/�B
0;B
0UB
0UB
0!B
0UB
0;B
0!B
0�B
1vB
1[B
1AB
0�B
0!B
0�B
1'B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
2B
2-B
2�B
33B
3�B
3�B
3�B
3�B
49B
4nB
4nB
4�B
4�B
4�B
5B
5�B
5�B
6FB
7B
72B
7�B
7�B
7�B
8�B
8�B
8�B
8�B
9$B
9rB
9�B
9�B
:B
:^B
:�B
;B
<B
<�B
<�B
<�B
<�B
=B
=VB
=VB
=�B
>BB
>]B
>�B
?B
?HB
?�B
?�B
@OB
@�B
@�B
@�B
A B
A�B
A�B
A�B
A�B
B'B
BAB
BuB
C�B
CaB
CaB
C�B
C�B
DB
D3B
D�B
D�B
D�B
D�B
EB
D�B
EB
E�B
E�B
E�B
F%B
F%B
FYB
F�B
F�B
F�B
GEB
GEB
G_B
G�B
G�B
G�B
G�B
HB
HfB
H�B
H�B
H�B
IB
IRB
I�B
I�B
I�B
I�B
J#B
I�B
J	B
J=B
JrB
J#B
J=B
JrB
J�B
K)B
K^B
K�B
LJB
LJB
L�B
L�B
L�B
L�B
M6B
M�B
M�B
N<B
NpB
NpB
NpB
NVB
N�B
OB
OB
OBB
O�B
O�B
O�B
P.B
PbB
P�B
P�B
P�B
P�B
Q B
QhB
Q�B
Q�B
RTB
RoB
R�B
R�B
S&B
SB
S@B
SuB
S�B
SuB
S�B
S�B
TFB
T,B
T{B
T{B
T{B
T�B
T�B
U2B
UMB
U�B
U�B
U�B
VSB
V�B
V�B
WYB
WYB
W$B
WYB
W�B
W�B
W�B
X+B
XyB
X_B
X�B
YKB
YB
Y�B
Y�B
ZB
ZQB
ZQB
ZkB
Z�B
Z�B
[	B
[WB
[=B
[�B
\B
\CB
\xB
\�B
\�B
]B
]dB
]~B
^B
^jB
^�B
^jB
^�B
_B
_!B
_VB
_�B
`B
`'B
`'B
`BB
`vB
`�B
`�B
`�B
aHB
a�B
a�B
bB
b4B
b4B
b�B
b�B
b�B
b�B
c:B
cTB
cnB
c�B
c�B
dB
d&B
d&B
d�B
d�B
d�B
e,B
ezB
e�B
e�B
e�B
fB
e�B
f2B
f2B
f�B
f�B
f�B
f�B
f�B
gB
g8B
gmB
g�B
g�B
g�B
g�B
h
B
h
B
g�B
h>B
h�B
h�B
h�B
i�B
i�B
i�B
jB
jeB
jKB
j0B
jeB
jB
jB
j�B
kkB
kQB
kQB
k�B
l"B
lqB
lWB
lWB
l=B
l�B
l�B
l�B
mCB
mwB
mwB
m�B
m�B
nB
n}B
n�B
oB
o B
o5B
oOB
oiB
o�B
p;B
p;B
poB
p�B
p�B
p�B
q[B
q�B
q�B
q�B
rB
r-B
r-B
rGB
raB
r�B
r�B
shB
shB
s�B
s�B
s�B
tB
t9B
t�B
t�B
t�B
uB
u?B
utB
u�B
u�B
u�B
u�B
u�B
u�B
vB
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
xB
xB
x8B
x�B
x�B
y	B
yXB
y�B
y�B
y�B
zDB
zB
zDB
z^B
z^B
z�B
z�B
z�B
z�B
{JB
{dB
{B
{�B
{�B
|B
|PB
|jB
|�B
|�B
|�B
|�B
}B
}<B
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
.B
cB
�B
�B
�B
�B
�B
�B
�4B
�iB
��B
��B
��B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<0�|<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<XD�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.06(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201912200049242019122000492420191220004924202207271133542022072711335420220727113354202207271536222022072715362220220727153622  JA  ARFMdecpA30a                                                                20191209093809  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191209093924  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191209093925  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191209093925  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191209093926  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191209093926  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191209093926                      G�O�G�O�G�O�                JA  ARUP                                                                        20191209095435                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191219154924  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191219154924  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023354  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063622  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091504                      G�O�G�O�G�O�                