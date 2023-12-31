CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-05-08T06:38:21Z creation;2020-05-08T06:38:24Z conversion to V3.1;2022-08-02T05:11:03Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200508063821  20220818091505  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               .A   JA  A30_8420_046                    2C  D   APEX                            8420                            2.11.2                          846 @��o���1   @���6� @/˅�Q��cYJ���1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bj��BnffBw33B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�33B�  B�  B���B���B�  C   C  C  C  C  C
  C  C�C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6�C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fD  Dy�D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dny�Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt�fDu  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�s31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @   @�=q@��A Q�A Q�A@��A`��A�=qA�(�A�(�A�=qA�Q�A�Q�A�Q�A�Q�B 33BG�B33B�B �B(33B033B8G�B@33BH�BP{BX{B`33Bk
=Bn��Bw�\B�\B�\B�B�
=B��B��B��B�B�B�p�B��fB�
=B�
=B�{B�\B��B�{B�
=B�  B�B�{B�\B��B��B�\B�{B�k�B�G�B��B�(�B��B��B�C �C{C\C
=C\C
\C�C&fC�C�CCCC�CC�C �C"\C$
C&{C(�C*\C,�C.�C0�C2\C4{C6+�C8&fC:C<�C>
=C@CB\CD
=CF
=CH�CJ�CL�CN
=CPCR�CT�CV�CX\CZC\C^
=C`�Cb{Cd(�CfCh�Cj�Cl
=CnCp�Cr\Ct\Cv\Cx�Cz\C|�C~�C��C��C��C�C�fC�C�fC��C��C��C�C��C��C�
=C��C�fC��C�fC�C�fC��C��C�fC�fC�fC��C��C��C��C��C��C�C�C�fC��C��C��C��C�fC�C��C��C�fC�fC��C�fC��C��C�C�C�fC��C��C��C�C��C�
=C��C�C��C��C�C�C�fC��C�HC�HC�fC��C�C�C��C��C�fC�
=C��C��C��C�fC��C�C�fC�fC��C��C��C�fC��C�fC�C�fC�
=C��C��C��C��C�fC�fC�fC��C��C�
=C��C��C�C�C��C��C�C�fC�C�HC��C�fC��C�C�fC��C�C�fC�fC�C�C�fC�fC�
=C�
=C�fD �D ��D�D��D�D��D{D�{D�D��DD�3D�D��D3D��D�D�3D	�D	�HD
�D
�3D�D��DD�3D �D�HD�D��D{D�{D�D��D�D��D �D��D�D�{D�D��D{D��D�D�{D{D�{D�D��D�D��D{D�D�D�
D�D\DHD��D�D��D�D�HD HD ��D!�D!�D"�D"��D#3D#��D$�D$�3D%�D%��D& �D&��D'�D'�{D({D(��D){D)�3D*3D*�{D+3D+��D,�D,�3D-�D-��D.�D.��D/�D/�3D0D0��D1�D1��D2�D2��D33D3�3D4�D4��D5�D5��D6�D6��D7�D7�HD8�D8�{D93D9��D:�D:�3D;�D;�3D<{D<�3D=�D=��D>3D>��D?3D?��D@�D@��DA�DA��DB3DB�{DC{DC�3DD{DD�fDEfDE�DF�DF�3DG�DG��DH3DH�DI�DI��DJ3DJ��DK{DK��DL{DL��DMDM�3DN�DN��DO�DO�3DP3DP�{DQDQ��DR�DR��DSDS�DT{DT�HDU �DU��DV�DV�{DW�DW��DX�DX��DY�DY�DZ�DZ�3D[�D[��D\�D\��D]�D]��D^ �D^�3D_{D_�3D`�D`�Da3Da�HDbHDb��Dc�Dc��DdDd�{De{De��Df3Df�Dg�Dg��Dh{Dh��Di�Di�{Dj3Dj��Dk3Dk��Dl�Dl��Dm3Dm�3Dn�Dn~�Do�Do��Dp3Dp��Dq�Dq��Dr�Dr�Ds{Ds��Dt�Dt�RDu�Du�3Dv3Dv�3Dw�Dw�3Dx�Dx�{Dy�Dy��Dz3Dz��D{�D{��D|�D|�3D}�D}��D~�D~��D3D��D�=D�B=D���D���D��D�A�D���D���D��D�B=D���D���D��D�B=D���D���D�HD�@�D���D���D� RD�@RD��HD���D��D�AHD��HD��HD��D�A�D���D�D��D�B=D��=D��HD� RD�@�D��HD���D�=D�AHD���D���D��D�A�D���D���D��D�B�D���D���D��D�A�D��=D���D��D�B�D���D��HD� �D�AHD���D��HD� �D�AHD��HD���D�HD�A�D���D���D��D�C3D���D���D� �D�AHD���D���D��D�AHD���D���D�HD�AHD��3D��3D�HD�AHD��HD��HD�HD�A�D��=D�D��D�@�D��HD���D��D�A�D��=D��=D� �D�@�D���D��=D��D�@�D���D���D��D�B�D���D��=D�=D�A�D���D���D��D�A�D���D���D�HD�A�D���D��RD��D�A�D��HD���D��D�B=D��HD���D� �D�@�D��HD��=D�=D�B=D��HD���D�HD�B�D���D���D�=D�A�D���D���D��D�AHD���D���D�HD�A�D���D���D�3D�B=D���D���D�HD�A�D���D��HD��D�B�D��=D�D�=D�B=D���D���D�=D�A�D���D�D��D�@�D���D��HD� �D�AHD���D���D� �D�@�D���D���D��D�@�D���D��=D�HD�@�D���D���D��D�A�D���D��HD� �D�@�D��HD���D��D�B�D���D��=D��D�AHD��HD���D��D�A�D��=D��=D��D�B�D���D��=D��D�B�D���D���D�=D�A�D���D���D��D�A�D���D���D��D�A�D��HD���D� �D�@�D���D���D��D�A�D��=D���D�HD�A�D���D���D�=D�B=D��=D�D��D�A�D���D���D� �D�@RD��RD���D��D�B=D�D�D��D�AHDÀ�D���D��D�B=DāHD��RD��D�C3Dł�D���D� �D�@�Dƀ�D���D�HD�A�Dǁ�D���D�=D�AHDȁHD��=D�HD�AHDɀ�D��RD� �D�A�Dʁ�D���D��D�A�Dˀ�D���D� �D�@�D́HD��=D�=D�AHD̀RD��HD� �D�@�D΂=D��HD�HD�A�Dπ�D���D� �D�AHDЁHD��RD� �D�A�Dс�D��=D�=D�A�DҀ�D��=D�=D�A�DӁHD��HD�HD�AHDԁ�D�D��D�A�DՂ=D���D� RD�@ DցHD���D� �D�@RD׀�D���D��D�B�D؂=D���D�=D�@�Dـ�D���D�HD�A�Dځ�D���D�  D�@�DہHD��RD� �D�A�D܀�D��RD�  D�A�D݂�D�D��D�B=Dށ�D���D��D�A�D߁HD��HD� �D�AHD���D���D�=D�A�D�=D���D� �D�@�D�HD���D� �D�AHD��D��HD��D�B�D��D���D� �D�@�D��D��HD�HD�A�D��D��HD�  D�@�D��D��HD� �D�@�D聚D���D�=D�A�D邏D���D��D�AHD��D���D� �D�AHD끚D��=D��D�AHD��D���D��D�A�D큚D��=D��D�B=DD���D�HD�@�D��D���D��D�B�D��=D��=D�=D�A�D��D���D�=D�@�D��D���D��D�A�D�D���D��D�B=D�=D���D� �D�AHD���D��3D�=D�B=D��=D�D�3D�B=D���D���D��D�A�D��HD���D��D�A�D���D���D��D�AHD���D���D��D�AHD�u1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aˢ�A˵�A�}VA�6A��A�fA��%A��>A���A�ٴA���A��BA���A���A��'A���AɼjAɹ$AɸRAɵ�AɳhAɱ�AɰUAɮ�Aɬ�AɥzAɖA�u�A��A��RA���A�2�A�1�A�7�A�R A��SA�p�A���A�(A��	A���A�YA�eA��A�ȀA��A�5?A�A��FA�T�A�z�A���A��VA���A���A��KA��!A�<�A��yA��A�K�A��A��A�7A��A���A�B�A���A�D�A��'A���A��jA��}A���A���A���A��A�,�A�.}A�O�A��?A��A��A���A�Q�A�k�A���A���A�"�A��A� iA�ffA���A|?�Av�Ar�An��Al2aAhb�Ac��A]!�AW�rAS��AN^�AI$tAF��AC��AB��A@hsA<��A;�A8�2A7��A6!�A4k�A3%FA1YKA0�AA1��A1�MA1M�A1K^A1eA3�EA7�}A95�A:!�A9��A5�<A3FtA3�HA3�MA3�$A1A�A+�[A+TaA*��A)	A&�.A"��A ��A{�A�A�Ap�A?A:�A��AN<A#:A!A�6A�>A�A4A_A��A�7A
=A�:AJ#A-wA��A1Ae,A=AH�A�A�bA=�A�A�`A`BA&�A�AA�UA͟AxA>BA7LAm]A�nA{�A�|A��AI�A?A
=Ag8ADgAT�A��A�A0�A
�xA	�]A
A
�A
jA	�A��A�BAںAB[A~AzxA��A�"A�4A��A��A��AZ�A!�A�Au�A�AݘAt�A��A��A|�A�A��A��A��APHA �@��W@�Z�@��h@�hs@���@�-@�(�@�ff@��@��@�_@�Ft@�Y�@��@� i@�^5@��9@�%@��b@�M@�|@��|@�a�@�RT@�@�v�@�l"@��@�dZ@��@�k@��*@��@�Z@�-@��@�IR@�@�@��@�ԕ@��8@�
�@��@�9�@�<6@�T�@�q@�|�@�bN@�e�@��,@�d�@��@�V@�d�@��@�@��@��+@�Z@�ԕ@߃{@��@ުe@�W�@ݩ*@݌~@݊�@�RT@��y@ܔF@���@�Vm@���@�Z�@�	@ـ4@�u%@ױ[@�s@�Y@�bN@ռ@Չ7@�0�@��@�u%@�<�@��@ӄM@��p@��@ѿH@ѕ�@�4@�{�@���@�*0@�H@�~�@�z@��@�33@��@��j@���@��'@�Ft@ɮ@ȥz@��r@��@ǡ�@���@���@�-w@ă�@��@Ó�@�o @�@��@���@�4@��K@��.@��+@��~@�dZ@�1�@��@�L0@�e,@��@�U2@���@�9�@��@�(@��@���@���@�6@��m@�(�@�?�@��j@�<6@��m@��_@���@���@��@�&�@�
=@��$@�D�@�G@��K@�!�@��s@�H@��@�dZ@��D@���@��k@�~�@��@��E@��'@�}V@��@�X@��s@�H@�b@�c@�K�@� \@���@�V@�1'@�	@���@�m]@���@�@��@���@���@�1�@���@�J�@�G@���@��@���@��4@�]�@�C�@�2a@�"�@�@��@���@��L@�h
@�6�@��@�S&@��@��[@���@�R�@�>B@�6�@��@���@�y�@�qv@�T�@��@��9@���@��D@�w�@�l�@�7@��6@��4@���@�C�@�#:@��@���@��F@��'@�=q@��@��@���@�w2@�E9@��@�u%@�Q�@�W�@��@��z@���@�g�@�RT@�]�@�!�@��j@�z�@��T@���@��V@��U@�V@�(�@��
@���@�o@��\@�c�@�*�@���@�|@��S@�{J@�v`@�iD@�C@��@��<@�l"@�(�@���@��6@��F@���@�X�@�+�@��@�ں@��b@�	@�w2@�A @�"�@�(@��/@���@�/�@��Z@���@���@�y�@�a@�6z@��@���@�bN@�J�@�E�@�5?@��+@�P�@���@�PH@�1�@�x@�O@�U2@���@��@��@�Q�@�� @� �@���@���@���@���@�ƨ@��d@��a@���@��-@��"@�]�@�>�@�C@���@�\�@�<�@�'R@�2�@�%�@��q@�qv@�Mj@�!-@���@�ی@��@�q@�,=@�@��]@���@�s�@�&@�
=@��@��@�d�@�9X@��@��M@�c@�>�@�Y@�(@��@��$@��@��o@�6@~�H@~�@~0U@~�@}O�@|ѷ@|��@|PH@|�@{��@{�a@{v`@{O@{�@zp;@y��@y��@yhs@y5�@x��@x�@v��@uS&@t�.@tS�@tq@s�m@s�P@s\)@r�@rq�@r&�@q�S@q�M@q0�@p��@o�]@n�@n�+@n@�@n4@m��@m+�@l�@l��@lV�@k�@k��@k=@j��@j�+@i��@iDg@i�@h�K@hbN@hx@g�q@g�{@g�@fe@e�3@e\�@d�P@d�4@d��@dr�@d@c�}@cA�@b�}@bW�@b
�@a��@a%F@`�K@`�I@_��@_C�@^�M@^�H@^^5@]��@]��@]7L@\�P@\�$@\U2@\?�@[�
@[��@['�@Z��@Z�b@Z=q@Y@Y�C@Y(�@X�	@X��@X~(@X]d@X*�@W�@W�@V�h@U�.@U�#@U�N@U��@U�t@U�X@UB�@T�p@T'R@S��@SRT@S4�@SY@R�@R�+@RO@Qԕ@Q4@Q�@P��@Poi@P2�@P~@O��@O�:@O.I@N�s@N�}@Ns�@N-@N@N
�@M@Mp�@M8�@L�@Lm�@LQ�@L/�@K� @K��@Kv`@K]�@K9�@K"�@J��@Jc @JO@I�D@I}�@IB�@I5�@I�@H�@Hu�@HH@G�@GE9@G�@Fߤ@F��@F}V@F�@F@Ec@E(�@D��@D��@D�Y@C�@C��@C{J@CZ�@C!-@B�c@B��@B�!@B~�@B$�@A�N@Ao @ADg@A%F@A�@@��@@�@@?�@?��@?�@>=q@=��@=p�@=B�@<�U@<Q�@;�@;�:@;\)@;F�@;+@;
=@:�6@:C�@9�)@9��@9%@8��@8��@8r�@8,=@8"h@7�@7�[@7y�@7o@6�6@6L0@6+k@6_@5�@5/@4��@4>B@4'R@3��@3�{@3@2��@2�R@2Ta@1�N@1a�@1-w@0�/@0��@0>B@/��@/8@/ i@.s�@.!�@-��@-�'@-<6@,�/@,��@,y>@,C-@,�@+�K@+��@+RT@*�@*��@*��@*^5@)��@)ϫ@)��@)��@)��@)��@)rG@)^�@(��@(��@(/�@'��@'\)@'K�@&��@&��@&�A@&=q@%��@%�9@%�-@%�h@%L�@%0�@%@$�v@$|�@#�@#��@#�@#j�@#@"͟@"�@"Q@"{@!�@!c@!G�@ �@ Ĝ@ ��@ 'R@�@˒@�*@�4@C�@!-@(@S@�y@�s@� @�j@��@��@��@�@\�@5�@2a@�@�@��@y>@g8@4n@7@��@� @��@�:@a@=@�M@��@{�@@�@6�@�@�@�-@��@:�@�@��@��@�D@oi@~@�@��@�$@x@K�@4�@��@�+@h
@M�@e@��@�-@�7@|@[W@4@�@��@��@y>@�@��@�k@j�@�@�s@�<@��@v�@O@�@@�S@T�@�/@�$@�Y@Q�@4n@7@G@��@\)@W?@33@�@ȴ@�6@��@��@v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aˢ�A˵�A�}VA�6A��A�fA��%A��>A���A�ٴA���A��BA���A���A��'A���AɼjAɹ$AɸRAɵ�AɳhAɱ�AɰUAɮ�Aɬ�AɥzAɖA�u�A��A��RA���A�2�A�1�A�7�A�R A��SA�p�A���A�(A��	A���A�YA�eA��A�ȀA��A�5?A�A��FA�T�A�z�A���A��VA���A���A��KA��!A�<�A��yA��A�K�A��A��A�7A��A���A�B�A���A�D�A��'A���A��jA��}A���A���A���A��A�,�A�.}A�O�A��?A��A��A���A�Q�A�k�A���A���A�"�A��A� iA�ffA���A|?�Av�Ar�An��Al2aAhb�Ac��A]!�AW�rAS��AN^�AI$tAF��AC��AB��A@hsA<��A;�A8�2A7��A6!�A4k�A3%FA1YKA0�AA1��A1�MA1M�A1K^A1eA3�EA7�}A95�A:!�A9��A5�<A3FtA3�HA3�MA3�$A1A�A+�[A+TaA*��A)	A&�.A"��A ��A{�A�A�Ap�A?A:�A��AN<A#:A!A�6A�>A�A4A_A��A�7A
=A�:AJ#A-wA��A1Ae,A=AH�A�A�bA=�A�A�`A`BA&�A�AA�UA͟AxA>BA7LAm]A�nA{�A�|A��AI�A?A
=Ag8ADgAT�A��A�A0�A
�xA	�]A
A
�A
jA	�A��A�BAںAB[A~AzxA��A�"A�4A��A��A��AZ�A!�A�Au�A�AݘAt�A��A��A|�A�A��A��A��APHA �@��W@�Z�@��h@�hs@���@�-@�(�@�ff@��@��@�_@�Ft@�Y�@��@� i@�^5@��9@�%@��b@�M@�|@��|@�a�@�RT@�@�v�@�l"@��@�dZ@��@�k@��*@��@�Z@�-@��@�IR@�@�@��@�ԕ@��8@�
�@��@�9�@�<6@�T�@�q@�|�@�bN@�e�@��,@�d�@��@�V@�d�@��@�@��@��+@�Z@�ԕ@߃{@��@ުe@�W�@ݩ*@݌~@݊�@�RT@��y@ܔF@���@�Vm@���@�Z�@�	@ـ4@�u%@ױ[@�s@�Y@�bN@ռ@Չ7@�0�@��@�u%@�<�@��@ӄM@��p@��@ѿH@ѕ�@�4@�{�@���@�*0@�H@�~�@�z@��@�33@��@��j@���@��'@�Ft@ɮ@ȥz@��r@��@ǡ�@���@���@�-w@ă�@��@Ó�@�o @�@��@���@�4@��K@��.@��+@��~@�dZ@�1�@��@�L0@�e,@��@�U2@���@�9�@��@�(@��@���@���@�6@��m@�(�@�?�@��j@�<6@��m@��_@���@���@��@�&�@�
=@��$@�D�@�G@��K@�!�@��s@�H@��@�dZ@��D@���@��k@�~�@��@��E@��'@�}V@��@�X@��s@�H@�b@�c@�K�@� \@���@�V@�1'@�	@���@�m]@���@�@��@���@���@�1�@���@�J�@�G@���@��@���@��4@�]�@�C�@�2a@�"�@�@��@���@��L@�h
@�6�@��@�S&@��@��[@���@�R�@�>B@�6�@��@���@�y�@�qv@�T�@��@��9@���@��D@�w�@�l�@�7@��6@��4@���@�C�@�#:@��@���@��F@��'@�=q@��@��@���@�w2@�E9@��@�u%@�Q�@�W�@��@��z@���@�g�@�RT@�]�@�!�@��j@�z�@��T@���@��V@��U@�V@�(�@��
@���@�o@��\@�c�@�*�@���@�|@��S@�{J@�v`@�iD@�C@��@��<@�l"@�(�@���@��6@��F@���@�X�@�+�@��@�ں@��b@�	@�w2@�A @�"�@�(@��/@���@�/�@��Z@���@���@�y�@�a@�6z@��@���@�bN@�J�@�E�@�5?@��+@�P�@���@�PH@�1�@�x@�O@�U2@���@��@��@�Q�@�� @� �@���@���@���@���@�ƨ@��d@��a@���@��-@��"@�]�@�>�@�C@���@�\�@�<�@�'R@�2�@�%�@��q@�qv@�Mj@�!-@���@�ی@��@�q@�,=@�@��]@���@�s�@�&@�
=@��@��@�d�@�9X@��@��M@�c@�>�@�Y@�(@��@��$@��@��o@�6@~�H@~�@~0U@~�@}O�@|ѷ@|��@|PH@|�@{��@{�a@{v`@{O@{�@zp;@y��@y��@yhs@y5�@x��@x�@v��@uS&@t�.@tS�@tq@s�m@s�P@s\)@r�@rq�@r&�@q�S@q�M@q0�@p��@o�]@n�@n�+@n@�@n4@m��@m+�@l�@l��@lV�@k�@k��@k=@j��@j�+@i��@iDg@i�@h�K@hbN@hx@g�q@g�{@g�@fe@e�3@e\�@d�P@d�4@d��@dr�@d@c�}@cA�@b�}@bW�@b
�@a��@a%F@`�K@`�I@_��@_C�@^�M@^�H@^^5@]��@]��@]7L@\�P@\�$@\U2@\?�@[�
@[��@['�@Z��@Z�b@Z=q@Y@Y�C@Y(�@X�	@X��@X~(@X]d@X*�@W�@W�@V�h@U�.@U�#@U�N@U��@U�t@U�X@UB�@T�p@T'R@S��@SRT@S4�@SY@R�@R�+@RO@Qԕ@Q4@Q�@P��@Poi@P2�@P~@O��@O�:@O.I@N�s@N�}@Ns�@N-@N@N
�@M@Mp�@M8�@L�@Lm�@LQ�@L/�@K� @K��@Kv`@K]�@K9�@K"�@J��@Jc @JO@I�D@I}�@IB�@I5�@I�@H�@Hu�@HH@G�@GE9@G�@Fߤ@F��@F}V@F�@F@Ec@E(�@D��@D��@D�Y@C�@C��@C{J@CZ�@C!-@B�c@B��@B�!@B~�@B$�@A�N@Ao @ADg@A%F@A�@@��@@�@@?�@?��@?�@>=q@=��@=p�@=B�@<�U@<Q�@;�@;�:@;\)@;F�@;+@;
=@:�6@:C�@9�)@9��@9%@8��@8��@8r�@8,=@8"h@7�@7�[@7y�@7o@6�6@6L0@6+k@6_@5�@5/@4��@4>B@4'R@3��@3�{@3@2��@2�R@2Ta@1�N@1a�@1-w@0�/@0��@0>B@/��@/8@/ i@.s�@.!�@-��@-�'@-<6@,�/@,��@,y>@,C-@,�@+�K@+��@+RT@*�@*��@*��@*^5@)��@)ϫ@)��@)��@)��@)��@)rG@)^�@(��@(��@(/�@'��@'\)@'K�@&��@&��@&�A@&=q@%��@%�9@%�-@%�h@%L�@%0�@%@$�v@$|�@#�@#��@#�@#j�@#@"͟@"�@"Q@"{@!�@!c@!G�@ �@ Ĝ@ ��@ 'R@�@˒@�*@�4@C�@!-@(@S@�y@�s@� @�j@��@��@��@�@\�@5�@2a@�@�@��@y>@g8@4n@7@��@� @��@�:@a@=@�M@��@{�@@�@6�@�@�@�-@��@:�@�@��@��@�D@oi@~@�@��@�$@x@K�@4�@��@�+@h
@M�@e@��@�-@�7@|@[W@4@�@��@��@y>@�@��@�k@j�@�@�s@�<@��@v�@O@�@@�S@T�@�/@�$@�Y@Q�@4n@7@G@��@\)@W?@33@�@ȴ@�6@��@��@v�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  By>By>Bx�BwLBv`ButBt�Bt�Bt�Bt�Bt�Bt�Bt�Bt�Bt�Bt�Bt�Bt�Bt�Bt�BuButButBu�BvBv�Bx�B}"B�1B� B	�B	YKB	�uB	�UB	�B	��B
aB
$B
>B
^jB
k�B
r�B
w�B
�1B
��B
��B
�B
��B
�sB
��BUB	7B�B'�B&�B/5B[WBgB��B��B�2B��B}<B��B��B��B�vB�GB�B޸B��B��B�B��B��BwBO�BSB
�B
��B
�.B
�nB
�>B
�[B
��B
��B
�5B
�bB
}VB
c:B
NVB
4�B
$�B	�B	ҽB	�9B	�NB	~BB	g�B	IRB	 �B�wB�}B�KB�B��B��B��B�FB��B�CB��B�|B��B�B�5B��B��B�jB�:B�B	gB	aB	R�B	�ZB	�B	��B	��B	�1B	͟B	�B	�ZB	�bB	�HB	�OB	��B	��B	�B	y�B	[�B	O(B	>�B	=qB	?�B	S@B	X�B	_�B	YeB	Z�B	`vB	jB	x�B	�~B	��B	�/B	��B	�QB	��B	�cB	��B	�rB	��B	��B	�6B	��B	�(B	�+B	��B	�JB	�B	��B	��B	�kB	�CB	�]B	��B	��B	�B	��B	�3B	�B	��B	�"B	�8B	�ZB	��B	��B	�;B	�"B	͟B	ԕB	�B	�BB	��B	�VB	��B	�xB	՛B	�MB	�B	�)B	�%B	��B	�?B	�oB	�pB	ʌB	�lB	�VB	бB	��B	��B	ˬB	�6B	�pB	�jB	˒B	��B	ɆB	��B	�B	�rB	��B	� B	�gB	յB	׍B	�MB	�(B	��B	ʌB	ȀB	ǔB	ȴB	�dB	��B	�pB	�	B	��B	��B	�B	�~B	��B	ɆB	�#B	��B	�xB	ˬB	��B	̳B	�xB	�B	�B	ɺB	�6B	��B	��B	��B	уB	��B	�2B	�MB	�B	��B	�@B	ЗB	��B	ϫB	бB	�jB	ʦB	�RB	��B	��B	͟B	�bB	�B	��B	бB	�B	��B	�_B	�EB	ؓB	�yB	�sB	�mB	��B	׍B	ٚB	�B	ڠB	�B	��B	�WB	��B	ںB	�#B	ںB	چB	�	B	�qB	��B	�=B	�kB	�B	�/B	��B	ݘB	��B	�jB	�OB	�B	�/B	��B	��B	�OB	ޞB	ݘB	�B	�xB	�)B	�OB	�jB	ݘB	��B	��B	�kB	�dB	�B	�hB	�B	��B	�B	�B	� B	�B	�nB	�@B	�@B	�B	�B	�&B	�B	��B	�B	��B	��B	�B	�B	�B	�fB	��B	�mB	�$B	��B	��B	�_B	�DB	�DB	��B	�B	�B	�B	�B	�B	�B	�*B	��B	�B	�B	��B	�B	�B	�B	��B	��B	�}B	�B	�B	�;B	�'B	�AB	�AB	��B	��B	��B	��B	�3B	��B	�TB	��B	�9B	��B	��B	��B	�B	�hB	��B	�|B	�GB	�[B	�'B	�B	�B	�B	�B	�`B	�B	�2B	�fB	��B	�rB	�xB	��B	��B	�B	��B	��B	��B	�jB	�qB	�<B	�<B	��B	��B	�HB	�HB	�.B	�.B	�HB
 4B
�B
[B
�B
�B
-B
�B
AB
�B
GB
�B
B
�B
�B
�B
�B
�B
	�B
	�B
	�B
	�B

#B

=B
^B
�B
�B
"B
�B
pB
�B
6B
B
6B
jB
�B
�B
BB
�B
�B
(B
B
 B
�B
B
�B
4B
�B
 B
�B
�B
�B
�B
�B
�B
_B
�B
B
qB
7B
�B
�B
�B
]B
�B
�B
B
B
�B
]B
�B
�B
�B
;B
VB
�B
�B
 \B
!bB
!�B
!�B
"B
"hB
"NB
"4B
"�B
#B
#nB
#�B
#B
#B
#B
#B
"�B
#B
#:B
#�B
#�B
#�B
$@B
$@B
$tB
$tB
$�B
$tB
$B
$B
$B
# B
!�B
 BB
B
�B
 BB
!�B
$B
$tB
#�B
$&B
#�B
"hB
!|B
!�B
!�B
"B
$�B
&B
'�B
(�B
(�B
(�B
)_B
)DB
)yB
)�B
*�B
)�B
)�B
*�B
+6B
+�B
,�B
.B
./B
.cB
.�B
.�B
.�B
/iB
/�B
/�B
/�B
0B
0UB
0�B
0�B
0�B
1[B
1AB
1'B
1�B
1�B
2aB
33B
33B
33B
3hB
3�B
3�B
3�B
2�B
1[B
0�B
2�B
4B
5�B
6�B
6�B
7LB
7�B
7�B
7�B
7�B
7�B
7�B
88B
8�B
8�B
8�B
8�B
9�B
9�B
8�B
8RB
7�B
7�B
9rB
9>B
8�B
8�B
8�B
8�B
9�B
:xB
:xB
:�B
:�B
:xB
:�B
:�B
:�B
:�B
;B
;�B
<B
<jB
<�B
<�B
<�B
="B
=�B
=qB
=�B
>]B
>]B
>wB
>�B
>�B
?.B
?HB
?�B
@�B
@�B
AoB
A�B
B[B
B[B
B�B
B�B
B[B
CB
C�B
C�B
D�B
D3B
DgB
D3B
DB
C�B
DB
DB
C�B
D�B
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
F�B
F�B
F�B
GEB
G_B
GzB
G�B
G�B
G�B
G�B
H1B
HKB
H�B
I7B
IRB
IlB
I�B
I�B
I�B
J#B
JrB
J�B
JrB
J�B
J�B
J�B
J�B
J�B
KB
K^B
K�B
K�B
K�B
LB
LJB
L0B
LdB
L�B
MB
MPB
MPB
M�B
NB
O�B
PHB
PHB
PB
PB
P}B
PbB
PbB
P}B
P�B
P�B
P}B
P}B
P�B
P�B
Q4B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
SB
SB
S�B
TB
TFB
T{B
T�B
T�B
UMB
U�B
V9B
VB
VB
VB
V�B
V�B
V�B
V�B
V�B
V�B
W$B
W$B
W?B
WYB
W�B
W�B
X�B
X�B
Y1B
YKB
YKB
Y�B
ZB
Z�B
ZQB
Z�B
[=B
[WB
[WB
[�B
[�B
[�B
[�B
[�B
[�B
\B
\xB
\�B
\�B
]dB
]�B
]�B
^B
^B
^5B
^jB
^OB
^�B
^�B
^�B
_!B
_VB
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`vB
`\B
`\B
`vB
`�B
`�B
`�B
`�B
`�B
aB
abB
a�B
bB
bhB
b�B
b�B
cB
cTB
c�B
c�B
c�B
c�B
d&B
d@B
d�B
d�B
d�B
e,B
eFB
e`B
ezB
e�B
f2B
fLB
fLB
f2B
fLB
fB
f2B
f�B
f�B
gB
g�B
g�B
g�B
h>B
hsB
hXB
h�B
h�B
iB
i_B
i_B
i�B
iyB
iyB
i�B
i�B
jB
j�B
j�B
kB
kQB
k�B
k�B
l"B
lWB
l�B
l�B
l�B
m]B
m]B
mwB
nB
nB
nB
nIB
n}B
n�B
o5B
oOB
o�B
poB
p�B
p�B
q'B
q'B
qvB
r�B
s�B
s�B
tB
t�B
t�B
t�B
t�B
tnB
t�B
uZB
uZB
u?B
u?B
u?B
u%B
uB
t�B
u%B
utB
u�B
u�B
u�B
vB
vzB
v�B
w�B
w�B
w�B
w�B
x8B
x8B
xRB
x�B
x�B
x�B
x�B
y	B
y$B
y$B
y�B
y�B
y�B
y�B
zB
zB
z^B
z^B
zxB
z�B
z�B
z�B
z�B
{B
{dB
{�B
|B
|PB
|jB
|�B
|�B
|�B
|�B
}"B
}qB
}�B
}�B
}�B
}�B
~wB
~]B
~�B
~�B
~�B
~�B
~�B
�B
�B
�B
� B
�4B
��B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  ByXBy�ByXBxBw�Bu�Bt�Bt�Bt�Bt�Bt�Bt�Bt�Bt�Bt�Bt�Bt�Bt�Bt�Bt�BuButButBu�Bv+Bw2ByrB}BʦB՛B	0B	[#B	��B	�B	�#B	�LB
{B
%B
>�B
^�B
lB
s3B
w�B
��B
�AB
�vB
ڠB
�B
�B
��BGBDBqB(�B(�B2�B]dBi*B��B��B�7B��B�oB��B��B��B�%B��B�B��B��B�AB�B��B�B~]BXB�B
�iB
��B
�B
�2B
�yB
��B
��B
��B
�-B
�B
�B
gB
R B
8�B
*�B
�B	��B	��B	�MB	��B	n}B	Q B	'mB	�B��B��B�fB�B��B�*B�DB��B�B��B�TB��B��B��B��B�B�jB�B��B	{B	 B	OB	�hB	� B	�0B	�B	ۦB	̈́B	�NB	�B	�,B	�MB	�VB	�+B	�FB	��B	}�B	^�B	Q�B	@�B	=qB	?}B	S�B	YeB	aB	Y�B	Z�B	`�B	j�B	x�B	�JB	��B	�~B	��B	��B	�IB	� B	�2B	��B	�PB	��B	��B	��B	�wB	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	��B	�.B	��B	��B	��B	��B	�*B	��B	��B	�"B	�mB	�1B	�B	�VB	��B	��B	�)B	�B	�mB	�@B	��B	�B	�B	ؓB	�@B	�B	��B	�lB	ΥB	�hB	�.B	�<B	�B	͹B	��B	��B	��B	�^B	�#B	�RB	�KB	�XB	��B	� B	՛B	�SB	�yB	�B	ϫB	�jB	�^B	��B	��B	��B	�~B	��B	��B	�#B	�)B	�}B	οB	�jB	�XB	��B	ʦB	�B	��B	�0B	̘B	��B	�~B	�zB	�7B	��B	̈́B	�B	��B	��B	ѝB	�@B	՛B	ՁB	�gB	�MB	��B	�NB	�\B	�HB	�4B	��B	��B	ɆB	��B	��B	͹B	бB	�.B	��B	бB	�aB	�gB	��B	��B	��B	��B	רB	֡B	�$B	��B	��B	�kB	��B	�QB	�=B	�WB	��B	��B	�WB	�	B	��B	�qB	��B	�B	�qB	��B	ܬB	ݲB	�B	��B	�5B	��B	�jB	�OB	�dB	�/B	�B	ބB	��B	�B	�~B	ܬB	�]B	ޞB	��B	�B	�CB	�#B	�kB	�~B	�B	��B	��B	�B	�&B	�B	�B	�B	�&B	�B	�tB	�ZB	�B	��B	�B	�LB	��B	�B	��B	��B	�
B	�B	�B	�B	�B	�B	�*B	�*B	�B	�B	�B	�B	�RB	�B	�
B	��B	�B	�B	�DB	�B	�B	��B	�B	�B	�/B	��B	�IB	�/B	�B	��B	��B	�UB	�vB	�vB	�B	�3B	��B	�3B	�3B	�B	�3B	��B	�B	��B	�9B	�B	�B	�TB	�B	��B	��B	��B	�B	�B	�'B	�-B	�B	�9B	��B	�B	�fB	��B	��B	��B	��B	��B	��B	�jB	�B	�<B	��B	��B	��B	�qB	�VB	��B	��B	�}B	�HB	�HB	�HB	�HB
 OB
�B
uB
�B
�B
aB
B
uB
AB
aB
B
3B
�B
�B
�B
�B
�B
	�B
	�B
	�B
	�B

=B

XB
xB
�B
"B
pB
B
�B
�B
6B
B
jB
�B
pB
(B
\B
�B
�B
\B
(B
NB
4B
.B
�B
�B
�B
:B
�B
�B
�B
�B
�B
�B
�B
B
eB
�B
�B
�B
�B
�B
�B
IB
B
CB
]B
�B
]B
B
�B
�B
VB
�B
�B
�B
 �B
!|B
!�B
!�B
"4B
"�B
"hB
"NB
# B
#TB
#�B
#�B
#:B
# B
#B
#:B
#:B
# B
#nB
#�B
#�B
#�B
$@B
$tB
$�B
$�B
$�B
$tB
$&B
$&B
$ZB
#�B
"B
 vB
B
�B
 'B
!�B
$@B
$�B
$&B
$tB
$B
"�B
!�B
"4B
!�B
"B
$�B
&B
'�B
(�B
(�B
(�B
)_B
)_B
)�B
*0B
*�B
*B
)�B
*�B
+6B
,B
,�B
./B
./B
.}B
.�B
/ B
/ B
/�B
/�B
/�B
/�B
0;B
0�B
0�B
0�B
1B
1�B
1vB
1[B
1�B
1�B
2�B
3MB
3MB
3MB
3�B
3�B
3�B
3�B
3MB
1�B
0�B
2�B
4TB
6B
6�B
6�B
7fB
7�B
7�B
7�B
7�B
7�B
7�B
8lB
8�B
8�B
8�B
9$B
:B
:B
9>B
8�B
7�B
7�B
9�B
9>B
9	B
8�B
8�B
8�B
9�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
:�B
;0B
;�B
<B
<�B
<�B
=B
="B
=VB
=�B
=�B
=�B
>]B
>]B
>�B
>�B
?B
?.B
?}B
@4B
@�B
AB
AoB
BB
B[B
BuB
B�B
B�B
B�B
C-B
C�B
C�B
D�B
DgB
D�B
DgB
DgB
DB
DB
D3B
D3B
D�B
D�B
D�B
D�B
E�B
E�B
FYB
F�B
F�B
F�B
GB
F�B
F�B
F�B
F�B
GzB
G_B
G�B
G�B
G�B
G�B
G�B
HfB
H�B
H�B
I7B
IRB
IlB
I�B
I�B
J	B
JXB
J�B
J�B
JrB
J�B
J�B
J�B
J�B
J�B
K)B
K�B
K�B
K�B
K�B
L0B
LdB
LJB
L~B
L�B
M6B
MjB
MjB
M�B
N"B
O�B
PbB
PbB
P.B
P.B
P�B
P}B
PbB
P�B
P�B
P�B
P}B
P�B
P�B
P�B
Q4B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
SB
S&B
S&B
S�B
T,B
TFB
T�B
T�B
T�B
UgB
U�B
VSB
VB
V9B
V9B
V�B
V�B
W
B
W
B
V�B
W
B
W?B
W?B
WYB
WYB
W�B
XB
X�B
X�B
Y1B
YeB
YeB
ZB
ZkB
Z�B
Z�B
[	B
[WB
[qB
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\)B
\�B
\�B
\�B
]~B
]�B
]�B
^B
^B
^OB
^�B
^jB
^�B
^�B
_B
_VB
_pB
_�B
_�B
`B
`'B
_�B
_�B
_�B
`B
`'B
`�B
`vB
`vB
`�B
`�B
`�B
`�B
`�B
`�B
aHB
a�B
a�B
b4B
b�B
b�B
b�B
cB
cnB
c�B
dB
c�B
dB
d@B
d@B
d�B
d�B
d�B
eFB
e`B
ezB
e�B
e�B
f2B
f2B
fLB
f2B
ffB
f2B
ffB
f�B
f�B
gRB
g�B
g�B
g�B
hXB
h�B
hXB
h�B
h�B
i*B
iyB
iyB
i�B
i�B
iyB
i�B
j0B
j�B
j�B
j�B
kB
kkB
k�B
lB
l=B
lqB
l�B
l�B
l�B
m]B
mwB
mwB
nB
n/B
n/B
ncB
n�B
o B
oB
oOB
o�B
p�B
p�B
qB
qAB
q'B
qvB
sB
s�B
s�B
tB
t�B
t�B
t�B
t�B
t�B
t�B
utB
utB
u?B
uZB
u?B
u?B
u%B
t�B
u?B
u�B
u�B
u�B
u�B
v+B
v�B
v�B
w�B
xB
xB
xB
xRB
xRB
xRB
x�B
x�B
x�B
y	B
y	B
y>B
y>B
y�B
y�B
y�B
y�B
zB
z*B
zxB
zDB
z�B
z�B
z�B
z�B
z�B
{0B
{B
|B
|B
|jB
|�B
|�B
|�B
}B
}B
}<B
}�B
}�B
}�B
}�B
~(B
~�B
~wB
~�B
~�B
~�B
B
B
�B
�B
�B
� B
�4B
��B
�iB
��B
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.05(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202005190042122020051900421220200519004212202207271135522022072711355220220727113552202207271538092022072715380920220727153809  JA  ARFMdecpA30a                                                                20200508063821  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200508063821  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200508063822  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200508063823  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200508063823  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200508063823  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200508063824                      G�O�G�O�G�O�                JA  ARUP                                                                        20200508065707                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200509000000  CF  PSAL_ADJUSTED_QC@��@��G�O�                JM  ARCAJMQC2.0                                                                 20200518154212  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200518154212  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023552  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063809  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091505                      G�O�G�O�G�O�                