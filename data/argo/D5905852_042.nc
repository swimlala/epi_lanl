CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-03-29T00:39:27Z creation;2020-03-29T00:39:29Z conversion to V3.1;2022-08-02T05:11:13Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200329003927  20220818091505  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               *A   JA  A30_8420_042                    2C  D   APEX                            8420                            2.11.2                          846 @���#E�1   @�����@/�a��e��c:��[W?1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @33@�  @�  A   A   A>ffA`  A�  A�  A�  A���A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B̙�B�33B�  B�33B�  B�  B�  B�  B�33BB�  B�  B�  C   C  C  C  C  C
ffC  C�C��C  C  C  C  C  C  C  C   C"  C$�C&�C'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX�CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D"��D#� D$  D$� D%  D%� D&  D&� D'  D'�fD(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|�fD}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�3D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @ff@���@�G�A ��A ��A?�
A`Q�A�=qA�Q�A�z�A��HA�ffA�Q�A��\A�RB p�BQ�B(�BG�B Q�B(ffB0{B8{B@(�BH(�BP33BX33B`=qBh\)Bp33Bx{B��B�(�B��B��B��B�#�B�#�B��B�#�B�#�B�
=B�{B��B�B�\B��B��B�{B��B̽qB�Q�B�\B�8RB��B�#�B�
=B�
=B�8RB���B�B�  B��C �C�C�CC\C
p�CC#�C�HC�C�C�C\C�C�C�C �C"�C$(�C&�C'�RC*�C,
=C.�C0C2�C4
=C6\C8�C:�C<
=C>\C@\CB�CD
=CF�CH\CJ
=CLCN
=CP�CR�CT�CV�CX�CZ
C\C^\C`�Cb\Cd�Cf�Ch�Cj
=Cl
=Cn�Cp�Cr
=Ct�Cv�Cx
=Cz�C|�C~�C��C�fC�fC��C��C��C��C�HC��C��C��C��C�fC�C�C�fC��C�fC��C��C��C�fC��C��C��C�C�HC��C��C��C��C��C�fC��C��C��C��C��C��C��C��C��C��C�
=C��C��C��C�HC�HC�C��C�fC��C�C��C�C��C��C�
=C�fC��C�fC�C�fC��C�fC��C��C��C��C�
=C��C��C�fC�fC�fC�fC��C�fC��C��C�fC�fC�C��C�fC��C��C�
=C��C��C�C�C��C�HC�C�fC��C�C�fC��C��C�
=C�\C�C��C�HC��C�C��C��C�fC��C�fC�fC�C��C�C��C��C�C��C��C�fC��C�C�fC�D �D �HD�D�D3D��D�D��D�D��D3D��D{D��D�D��D3D�{D	D	�3D
�D
�3D�D�{DD�fD�D��D3D�{D{D��D�D��D �D� D�D�D{D��DHD�HD�D�3D�D�{D{D�3D�D��D�D��D �D��DHD��D{D��D�D�D�D��D�D�HD �D ��D!�D!�3D"3D"�HD"�\D#��D$�D$��D%�D%��D&{D&��D'D'�
D(�D(��D)HD)��D*�D*�{D+D+��D,�D,��D-�D-��D.3D.��D/{D/�{D03D0��D1�D1�3D2�D2�3D3{D3��D4HD4��D5�D5��D6D6��D7�D7�{D8{D8��D93D9�3D:3D:�3D;�D;��D<�D<�3D= �D=��D>{D>�D?{D?�3D@�D@��DADA��DB�DB�{DC�DC��DDHDD��DE3DE�{DF{DF��DG�DG�3DH3DH��DI3DI��DJ�DJ�DK3DK��DL�DL��DM3DM�{DNDN��DO�DO�{DP3DP�HDQ�DQ�{DR{DR�3DSDS�{DT3DT�3DU�DU�3DVHDV��DW3DW��DX3DX�3DY�DY�HDZ�DZ��D[{D[�3D\3D\�3D]�D]�D^3D^��D_HD_��D`�D`��Da�Da�HDbHDb�HDc{Dc�3Dd3Dd�3De3De�{Df�Df�3Dg�Dg�Dh{Dh�{Di�Di��Dj�Dj�{Dk�Dk�fDl{Dl��Dm �Dm��Dn �Dn�HDo{Do�{Dp�Dp�{Dq�Dq�Dr{Dr�3Ds�Ds��DtDt�3Du�Du�Dv�Dv�3Dw�Dw�Dx{Dx�{Dy{Dy�3Dz�Dz��D{{D{�{D|�D|�
D}�D}�HD~HD~��D�D��D�=D�C3D��=D��HD��D�C3D��=D���D��D�A�D���D���D��D�@�D���D���D� �D�B=D���D��HD� �D�@�D���D���D� �D�@�D���D���D��D�A�D���D��HD�=D�A�D��=D���D�HD�A�D���D���D��D�@�D���D���D�HD�@�D���D��=D�HD�@�D��HD���D�HD�@�D���D���D��D�B=D���D���D��D�B=D���D���D� �D�AHD���D��RD� �D�@�D���D��HD��D�A�D���D���D�=D�B�D��3D��HD�HD�@�D���D���D�HD�@�D���D���D� �D�@�D���D���D�=D�A�D��HD���D��D�A�D���D��HD� �D�@�D��HD��HD��D�AHD���D���D�=D�A�D���D���D��D�@�D��RD���D��D�B�D���D��HD� �D�AHD��=D���D��D�B=D���D���D�HD�AHD��=D���D� �D�A�D���D��=D�=D�B=D���D���D� �D�A�D���D���D��D�B�D��=D���D��D�A�D���D���D�=D�B=D���D���D��D�@�D���D���D��D�C3D���D��RD� �D�@�D���D���D�HD�A�D���D��HD��D�A�D���D��=D�HD�@�D���D��HD��D�B�D���D���D��D�B�D��=D��HD� �D�@�D���D��=D��D�A�D��HD��HD��D�A�D���D���D� �D�@�D���D��HD� �D�@�D���D��=D� �D�A�D���D���D��D�@�D���D���D�=D�B=D���D���D��D�B=D��HD���D�HD�@�D��HD���D��D�A�D���D��RD�HD�B=D���D��=D��D�B�D��HD��HD��D�B�D���D���D��D�A�D���D���D��D�@RD���D���D��D�B�D��=D��=D��D�A�D���D���D�=D�B=D��=D��=D� �D�@RDHD�D�=D�A�DÀ�D���D�=D�B�DāHD��RD� �D�A�DŁ�D��HD��D�A�DƂ�D���D��D�B=Dǁ�D��3D��D�B=DȂ=D���D�=D�B=Dɂ=D���D� �D�A�Dʁ�D���D��D�A�Dˁ�D��=D��D�B=D̂=D���D�HD�B�D́�D���D��D�AHD΁�D�D��D�@�DρHD���D� �D�A�DЁ�D���D��D�@�DрRD��HD�HD�@�DҀRD��RD� �D�@�DӁ�D���D� �D�@RDԁHD���D�=D�B�DՁ�D��HD� �D�@�Dր�D���D�HD�@�Dׂ=D��=D� �D�AHD؁�D��HD�HD�AHDـ�D���D��D�@�Dڀ�D���D� RD�@RDہ�D��=D��D�A�D܂�D���D�HD�B�D݂�D�D�3D�C3Dނ=D���D��D�B�D߂�D��=D��D�A�D��HD���D� �D�@�DၚD��=D�HD�@�D�HD���D�HD�B�DずD���D� �D�A�D䂏D��=D�HD�@�D��D���D��D�@�D�RD���D��D�@�D��D���D� �D�A�D肏D��=D��D�A�D遚D���D� RD�AHD��D�D��D�AHD�=D�D�=D�B=D��D�D��D�@�D큚D��HD��D�B=DD���D��D�A�DD��HD�  D�@RD��HD���D� �D�B=D�D��HD��D�B�D�D���D��D�AHD�HD���D��D�B�D�=D���D� �D�@RD���D�D��D�A�D���D��HD��D�B=D��=D���D�=D�B�D���D���D��D�AHD���D���D��D�A�D���D��=D��D�A�D���D��{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�.IA�1[A�/�A�-�A�-wA�-�A�5?A�:^A�;0A�;dA�7A�.}A�1[A�2�A�-�A�0�A�%zA��A� �A��A�A��A�~A� \A�!�A�OA�IA�OA��A�A��A��A�xA��A��A��A��A�	A�CA��A��A�VA�!-A�"4A�#:A�$A�#�A� 'A�A�7A��A�{JA�[�A���A��.A�.�A��OA�P}A���A�9�A�9�A���A���A��=A���A�n�A�K)A�'RA��A�^A��#A�K�A��QA�خA��A�Y�A��A�jA}A{ߤAq�oAndZAl]dAjIRAh�Ad��Ack�Aa6zA]�FAV�^AS�=AP�vAL��AJ iAG9�AF,�AE��AC1�A?�UA<~�A9�CA9 �A8�A64�A4H�A3G�A2�A2�A1��A0�AA.xA-8�A,h
A*q�A(VA&��A%M�A$��A"A ��A�AOvA��AߤA/�A��A1�A@A��A	A�eARTA\�A�NA'RAR�As�AE9ADgA�KA�A�AxlA1A�-A�+A�"A��A�AJ�A�Ax�A,�A�PA�HA�PAU�A
=A%�A��A�zA�hA��A�Am]A�fA�UA��A;�A��A�AH�A�Ae�A�A��A�A�A�\AN<A�	A��AR�A-wA�A
�A
��A
��A
F�A	�A	��A	\�A֡Av�AA�[AH�A��A��Au%A�AϫA��AG�A�A��A�EA��Ac A,�A�A�6A9XA�`A�gA˒Ar�A�mAw�AMA �A �"A B�@�`B@���@��$@�C-@�5�@��U@���@���@�5?@���@�C�@��@�t�@�C�@��{@��I@� �@�:@�	@��@�\�@��@��@�
=@�YK@�w2@��,@�z@��D@�U�@��@�,=@�S@�/�@��`@�Z�@�˒@�iD@��@���@�9�@�Q@���@�N<@���@�"h@ង@�s@��H@�tT@ߟV@ޕ�@�,=@��K@�%F@���@܌�@�R�@��@۔�@�Y�@�@@�q@�RT@���@ؐ.@��@�G�@��@�
=@��M@��X@֌�@��@�RT@�;@ԆY@��@��@�A�@�l�@�ں@Л�@�u�@�-@ϲ�@γh@Ͳ�@̡b@̃@��@�0�@ʩ�@�	@ɀ4@�!�@��@���@���@��v@ȕ�@�Q�@ǿH@�a@��@ƻ�@�q�@�!@���@ŉ7@�X�@�&@ĺ�@��@���@��@�@�Q@�	@��@�.I@�ѷ@���@�!�@���@�U�@��I@�C-@�e�@�#�@��9@�s�@�}V@�B[@���@��:@�E9@�a|@���@��[@�\�@��@�z�@�6@���@�m]@�ں@�j@�K^@�Ft@�E�@���@��@��+@�3�@�b@���@��w@��@��X@��@�v`@��"@�T�@��@�ں@��x@���@�w�@�;�@��@��]@��>@���@��f@�P�@��]@�.�@��@�� @���@��S@�_p@��@��F@�PH@��@���@��S@�\)@��|@��R@�i�@� �@�@���@�Y@���@��@�h
@�6�@�a@��8@���@��\@���@�_@�=q@�@�7�@�	@���@�H�@�ں@�B[@� �@���@��6@��n@���@�Mj@��@��@��@��'@���@���@�bN@�-@�	@���@�7L@���@��`@��v@���@�[�@�u@���@�rG@�j�@�l�@��@��!@��]@��V@���@�p�@�C�@��@��@�8�@���@�]�@�@��|@��H@�͟@�kQ@�  @��X@�s�@�@O@�V@��@��'@���@���@���@�Ta@�@��o@��@�ϫ@�m]@�c@�1�@�z@��u@���@�8�@��;@��n@�|@�7L@��4@�@�@��@��@@�}�@�]�@�O�@�6z@��@��@��I@�D�@�%�@�1@�s@�8�@�ߤ@��'@���@�;�@���@�o�@�"�@��m@���@��u@�n�@��@�9X@�2�@��.@���@�O@�Y@�(@���@��e@�)�@��}@���@�g�@�K�@�:�@�!�@�
=@���@���@��g@��t@���@�|�@�[W@�=�@�!-@��K@�Ft@���@��S@�iD@�F�@��@��P@���@���@���@�?@��o@���@�6z@�v�@��F@�p�@�O@��@��[@��$@���@�oi@��.@�t�@��@���@��@�?@�4@���@���@�v`@�Vm@�\)@�b�@�a�@�?}@��@��y@���@�?�@ݘ@~�@~1�@}�M@}S&@}Q�@}?}@}<6@}5�@}*0@|�@z��@z-@y��@yj@x��@x��@xj@x~@wH�@v�@vu%@v��@v�@v͟@v��@v}V@v�@u�~@t�@s�A@s�f@sZ�@r� @q��@q�7@qo @q@p��@ptT@p�@o�m@o�*@oj�@n��@n�@n.�@n
�@n@m�@m�=@mQ�@lĜ@l_@k��@k�6@k�a@k��@k�V@k�P@k@j�@ja|@j=q@i��@i��@io @ia�@i�@h�@h%�@hx@g�r@gMj@g�@f�r@e�@e��@ec@eA @d��@d��@c�@c�f@cS�@c@O@b��@bc @a�@au�@a�@`��@`�u@`2�@_��@_�@^�@]�.@]�9@]�S@]�@\��@\<�@[�;@[\)@Z�X@ZQ@Z
�@Y��@Y��@Y�-@YB�@X�E@Xy>@X9X@Xx@Wخ@WiD@V�]@V��@V�b@V��@V�@VV@U�@UQ�@U@T�	@T�Y@S�@S�6@S��@S�@R�m@R1�@Q��@Qhs@P�P@Pq@O�@O�4@N��@NR�@N-@NJ@M��@Ma�@M/@L��@L<�@L�@K�;@KiD@K�@J�'@JM�@JO@I��@I��@I�>@I�7@H�Y@G�]@G� @G�0@G�[@G�@GdZ@G�@Fz@E��@Ehs@D��@Du�@D9X@C��@Ct�@CH�@CS@B�@B�<@Bxl@BR�@B:*@Be@A��@A�@@�z@@j@@]d@@PH@@$@?�
@?dZ@>��@>�<@>d�@>�@=��@=��@=�7@=�@=�S@=+@<��@<V�@<?�@<(�@<�@;��@;A�@:�@:u%@:{@9�M@9Y�@8��@8�j@8[�@7��@7�4@7Z�@7@6͟@6�r@6^5@6+k@5��@5�H@5��@5=�@4�E@4��@47�@3��@3b�@3=@3+@3�@2��@2��@2�@2@�@1��@1zx@1Y�@1[W@1Y�@1N<@10�@1	l@0�@0m�@0-�@/�@/��@/�@.��@.n�@.:*@-ϫ@-x�@-L�@-+�@-%@,�@,�9@,q@+�]@+��@+x@*��@*��@*i�@*M�@*�@)�>@)��@)O�@)	l@(��@(��@(h�@(@'�;@'t�@'6z@'@&��@&�L@&?@&@%�j@%��@%��@%�h@%�M@%:�@$��@$�@$I�@$�@#��@#e�@#H�@#S@"�<@"�A@"?@"&�@"u@!�@!�z@!��@!��@!��@!<6@ �/@ Ĝ@ �4@ PH@ 	�@خ@�@��@b�@F�@,�@�@�B@�1@^5@1�@�@4@�T@�@x�@N<@+�@�@�@9X@�@��@W?@�@S@�@xl@;�@��@ϫ@�^@�=@�"@w2@L�@�	@�j@�@�I@��@l"@S�@$@�@	�@�@|�@P�@�"@��@�b@p;@5?@��@�o@�T@�@��@hs@S&@��@bN@PH@Q�@S�@!@�@��@�&@�@�a@s@A�@(@�b@�@_�@e@11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�.IA�1[A�/�A�-�A�-wA�-�A�5?A�:^A�;0A�;dA�7A�.}A�1[A�2�A�-�A�0�A�%zA��A� �A��A�A��A�~A� \A�!�A�OA�IA�OA��A�A��A��A�xA��A��A��A��A�	A�CA��A��A�VA�!-A�"4A�#:A�$A�#�A� 'A�A�7A��A�{JA�[�A���A��.A�.�A��OA�P}A���A�9�A�9�A���A���A��=A���A�n�A�K)A�'RA��A�^A��#A�K�A��QA�خA��A�Y�A��A�jA}A{ߤAq�oAndZAl]dAjIRAh�Ad��Ack�Aa6zA]�FAV�^AS�=AP�vAL��AJ iAG9�AF,�AE��AC1�A?�UA<~�A9�CA9 �A8�A64�A4H�A3G�A2�A2�A1��A0�AA.xA-8�A,h
A*q�A(VA&��A%M�A$��A"A ��A�AOvA��AߤA/�A��A1�A@A��A	A�eARTA\�A�NA'RAR�As�AE9ADgA�KA�A�AxlA1A�-A�+A�"A��A�AJ�A�Ax�A,�A�PA�HA�PAU�A
=A%�A��A�zA�hA��A�Am]A�fA�UA��A;�A��A�AH�A�Ae�A�A��A�A�A�\AN<A�	A��AR�A-wA�A
�A
��A
��A
F�A	�A	��A	\�A֡Av�AA�[AH�A��A��Au%A�AϫA��AG�A�A��A�EA��Ac A,�A�A�6A9XA�`A�gA˒Ar�A�mAw�AMA �A �"A B�@�`B@���@��$@�C-@�5�@��U@���@���@�5?@���@�C�@��@�t�@�C�@��{@��I@� �@�:@�	@��@�\�@��@��@�
=@�YK@�w2@��,@�z@��D@�U�@��@�,=@�S@�/�@��`@�Z�@�˒@�iD@��@���@�9�@�Q@���@�N<@���@�"h@ង@�s@��H@�tT@ߟV@ޕ�@�,=@��K@�%F@���@܌�@�R�@��@۔�@�Y�@�@@�q@�RT@���@ؐ.@��@�G�@��@�
=@��M@��X@֌�@��@�RT@�;@ԆY@��@��@�A�@�l�@�ں@Л�@�u�@�-@ϲ�@γh@Ͳ�@̡b@̃@��@�0�@ʩ�@�	@ɀ4@�!�@��@���@���@��v@ȕ�@�Q�@ǿH@�a@��@ƻ�@�q�@�!@���@ŉ7@�X�@�&@ĺ�@��@���@��@�@�Q@�	@��@�.I@�ѷ@���@�!�@���@�U�@��I@�C-@�e�@�#�@��9@�s�@�}V@�B[@���@��:@�E9@�a|@���@��[@�\�@��@�z�@�6@���@�m]@�ں@�j@�K^@�Ft@�E�@���@��@��+@�3�@�b@���@��w@��@��X@��@�v`@��"@�T�@��@�ں@��x@���@�w�@�;�@��@��]@��>@���@��f@�P�@��]@�.�@��@�� @���@��S@�_p@��@��F@�PH@��@���@��S@�\)@��|@��R@�i�@� �@�@���@�Y@���@��@�h
@�6�@�a@��8@���@��\@���@�_@�=q@�@�7�@�	@���@�H�@�ں@�B[@� �@���@��6@��n@���@�Mj@��@��@��@��'@���@���@�bN@�-@�	@���@�7L@���@��`@��v@���@�[�@�u@���@�rG@�j�@�l�@��@��!@��]@��V@���@�p�@�C�@��@��@�8�@���@�]�@�@��|@��H@�͟@�kQ@�  @��X@�s�@�@O@�V@��@��'@���@���@���@�Ta@�@��o@��@�ϫ@�m]@�c@�1�@�z@��u@���@�8�@��;@��n@�|@�7L@��4@�@�@��@��@@�}�@�]�@�O�@�6z@��@��@��I@�D�@�%�@�1@�s@�8�@�ߤ@��'@���@�;�@���@�o�@�"�@��m@���@��u@�n�@��@�9X@�2�@��.@���@�O@�Y@�(@���@��e@�)�@��}@���@�g�@�K�@�:�@�!�@�
=@���@���@��g@��t@���@�|�@�[W@�=�@�!-@��K@�Ft@���@��S@�iD@�F�@��@��P@���@���@���@�?@��o@���@�6z@�v�@��F@�p�@�O@��@��[@��$@���@�oi@��.@�t�@��@���@��@�?@�4@���@���@�v`@�Vm@�\)@�b�@�a�@�?}@��@��y@���@�?�@ݘ@~�@~1�@}�M@}S&@}Q�@}?}@}<6@}5�@}*0@|�@z��@z-@y��@yj@x��@x��@xj@x~@wH�@v�@vu%@v��@v�@v͟@v��@v}V@v�@u�~@t�@s�A@s�f@sZ�@r� @q��@q�7@qo @q@p��@ptT@p�@o�m@o�*@oj�@n��@n�@n.�@n
�@n@m�@m�=@mQ�@lĜ@l_@k��@k�6@k�a@k��@k�V@k�P@k@j�@ja|@j=q@i��@i��@io @ia�@i�@h�@h%�@hx@g�r@gMj@g�@f�r@e�@e��@ec@eA @d��@d��@c�@c�f@cS�@c@O@b��@bc @a�@au�@a�@`��@`�u@`2�@_��@_�@^�@]�.@]�9@]�S@]�@\��@\<�@[�;@[\)@Z�X@ZQ@Z
�@Y��@Y��@Y�-@YB�@X�E@Xy>@X9X@Xx@Wخ@WiD@V�]@V��@V�b@V��@V�@VV@U�@UQ�@U@T�	@T�Y@S�@S�6@S��@S�@R�m@R1�@Q��@Qhs@P�P@Pq@O�@O�4@N��@NR�@N-@NJ@M��@Ma�@M/@L��@L<�@L�@K�;@KiD@K�@J�'@JM�@JO@I��@I��@I�>@I�7@H�Y@G�]@G� @G�0@G�[@G�@GdZ@G�@Fz@E��@Ehs@D��@Du�@D9X@C��@Ct�@CH�@CS@B�@B�<@Bxl@BR�@B:*@Be@A��@A�@@�z@@j@@]d@@PH@@$@?�
@?dZ@>��@>�<@>d�@>�@=��@=��@=�7@=�@=�S@=+@<��@<V�@<?�@<(�@<�@;��@;A�@:�@:u%@:{@9�M@9Y�@8��@8�j@8[�@7��@7�4@7Z�@7@6͟@6�r@6^5@6+k@5��@5�H@5��@5=�@4�E@4��@47�@3��@3b�@3=@3+@3�@2��@2��@2�@2@�@1��@1zx@1Y�@1[W@1Y�@1N<@10�@1	l@0�@0m�@0-�@/�@/��@/�@.��@.n�@.:*@-ϫ@-x�@-L�@-+�@-%@,�@,�9@,q@+�]@+��@+x@*��@*��@*i�@*M�@*�@)�>@)��@)O�@)	l@(��@(��@(h�@(@'�;@'t�@'6z@'@&��@&�L@&?@&@%�j@%��@%��@%�h@%�M@%:�@$��@$�@$I�@$�@#��@#e�@#H�@#S@"�<@"�A@"?@"&�@"u@!�@!�z@!��@!��@!��@!<6@ �/@ Ĝ@ �4@ PH@ 	�@خ@�@��@b�@F�@,�@�@�B@�1@^5@1�@�@4@�T@�@x�@N<@+�@�@�@9X@�@��@W?@�@S@�@xl@;�@��@ϫ@�^@�=@�"@w2@L�@�	@�j@�@�I@��@l"@S�@$@�@	�@�@|�@P�@�"@��@�b@p;@5?@��@�o@�T@�@��@hs@S&@��@bN@PH@Q�@S�@!@�@��@�&@�@�a@s@A�@(@�b@�@_�@e@11111111111111111111111111111111111111111111111111113311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BI�BI�BI�BI�BI�BJ	BI�BJ	BI�BI�BJ	BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BJ�BKxBK�B|B�*B	��B	�B
�wB
�(B
��B
�jB
�'B
�B
��B
�vB
֡B
�wB
�\B
�B
��B
�/B
��B
g�B
O�B
1B	�wB	�B	ϫB	�UB	�*B	��B	�NB	��B	k�B	X+B	R:B	M�B	J�B	EB	@iB	7B	*0B	�B	'B��B�aB�]B�$B�B��B�KB�B��B	�B	�B		�B	@B	9B	�B	�B	�B	jB	�B	EB	yB	B	+B	�B	�B	�B		�B��B��B�	B��B	 B	9B	dB	�B	�B	OB	'�B	)DB	1B	=qB	S�B	dZB	j�B	nB	q[B	xB	�B	�B	��B	�=B	��B	��B	�B	�8B	��B	��B	�2B	��B	��B	�+B	��B	�B	��B	��B	�dB	�BB	��B	�}B	�B	��B	��B	�jB	��B	�dB	�oB	��B	�NB	�oB	�hB	уB	уB	�B	ԕB	�_B	�B	�NB	�B	�ZB	�B	��B	�B	��B	�8B	��B	��B	�DB	�DB	�B	�B	�QB	��B	�B	��B	�UB	�B	�B	��B	��B	�FB	��B	��B	�8B	�lB	�8B	�lB	��B	��B	�XB	�	B	��B	��B	��B	��B	�lB	��B	�RB	�lB	�8B	��B	�2B	�B	�fB	��B	��B	�`B	�%B	�B	��B	�B	�B	�hB	�3B	�aB	�-B	�B	�'B	�B	��B	� B	�B	�B	�]B	��B	��B	�"B	��B	�B	�B	�B	��B	�_B	��B	�B	�$B	�B	�RB	�B	�B	��B	�B	�B	�B	��B	��B	��B	�B	�B	�fB	�fB	��B	�B	�8B	�B	�RB	�B	�B	�mB	�B	�mB	�B	�B	�8B	�B	��B	�RB	�B	�B	�mB	�RB	�mB	�B	�mB	�B	�mB	�
B	�B	�sB	�B	�XB	�B	�B	�B	��B	�B	��B	�_B	�B	�B	�0B	�>B	��B	�$B	��B	��B	��B	�>B	��B	�*B	�yB	�_B	�B	�*B	�yB	�0B	�eB	�B	��B	��B	��B	��B	��B	�B	�B	�B	��B	�B	��B	��B	��B	��B	�}B	�cB	��B	�B	�CB	�B	��B	��B	�IB	� B	�}B	��B	�5B	�B	�B	��B	��B	��B	�DB	��B	��B	�XB	��B	�B	��B	�xB	��B	��B	��B	�xB	��B	�xB	�*B	�JB	�0B	�B	��B	�qB	�<B	�VB	�VB	��B	�.B
UB
UB
�B
B
AB
'B
B
�B
B
-B
{B
gB
�B
B
?B
�B
�B
�B
�B
�B
�B
%B
�B
�B
B
�B
�B
gB
{B
B
�B
3B
9B
%B
�B
�B
�B
�B
EB
_B
B
�B
�B
�B
�B
	B
	�B

�B

�B

�B

�B
	�B

#B

�B

�B
B
�B
�B
JB
6B
B
B
�B
�B
�B
�B
�B
pB
�B
�B
�B
(B
�B
�B
4B
B
4B
NB
 B
hB
�B
�B
:B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
{B
{B
,B
aB
{B
�B
{B
�B
2B
B
mB
9B
9B
�B
�B
EB
B
�B
�B
B
=B
�B
7B
QB
�B
�B
	B
�B
xB
�B
�B
IB
IB
dB
~B
B
�B
!B
�B
�B
;B
�B
�B
�B
 B
�B
;B
�B
�B
�B
pB
;B
!B
�B
"NB
#nB
#TB
# B
"�B
#B
"�B
"�B
#:B
#�B
$�B
$�B
$�B
$tB
$ZB
$@B
$B
$ZB
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%B
%,B
&B
&�B
&�B
&�B
'�B
(�B
)DB
)�B
*KB
*�B
+QB
+�B
+B
*B
)_B
)*B
)*B
)*B
)DB
)DB
)*B
)B
(�B
)�B
*B
*�B
+B
+kB
+�B
,=B
,B
,�B
-]B
-�B
-�B
-�B
-]B
-]B
-�B
-�B
.IB
.�B
.�B
/�B
0!B
0UB
0UB
0;B
0;B
0!B
0!B
/�B
/�B
/�B
/iB
/B
.�B
.�B
/ B
.�B
/B
/B
0;B
1'B
4nB
6�B
7�B
7�B
8B
8�B
8�B
8B
8B
8�B
8�B
9�B
9>B
8�B
8�B
8�B
8RB
8RB
8B
7�B
7�B
7B
6�B
7B
7fB
7�B
7�B
7�B
7�B
8lB
9$B
9>B
9>B
9�B
9�B
9�B
:B
:xB
;�B
<�B
<�B
<�B
=VB
=�B
=�B
=�B
=�B
>B
>wB
>BB
>(B
>wB
>wB
?B
?B
?B
?}B
?�B
?�B
@ B
@OB
@4B
@4B
@B
@�B
A B
A�B
A�B
A�B
A�B
B�B
C{B
C�B
D3B
DgB
D�B
D�B
EB
ESB
E9B
EmB
E�B
E�B
F?B
F�B
F�B
F�B
F�B
F�B
F�B
GB
GEB
G_B
G_B
GzB
G�B
H�B
H�B
H�B
H�B
H�B
H�B
IB
I7B
I7B
I7B
I�B
J	B
J	B
JrB
J�B
J�B
KDB
K�B
K�B
K�B
LJB
L~B
L�B
MjB
M�B
M�B
N"B
NpB
NVB
NpB
OBB
P}B
P�B
Q B
Q4B
QhB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R B
R�B
R�B
R�B
R�B
R�B
S&B
S@B
S�B
S�B
T,B
T�B
T�B
T�B
UgB
UgB
U�B
VB
U�B
VB
VSB
V�B
V�B
VmB
V�B
W?B
W�B
W�B
W�B
W�B
W�B
XB
X�B
X�B
X�B
YB
YeB
YB
Y�B
ZQB
[#B
[qB
[�B
\)B
\xB
\xB
\]B
\]B
\]B
\xB
\�B
]IB
]�B
^B
^B
^jB
^jB
^jB
^B
^5B
^5B
^�B
^�B
_B
^�B
_!B
_VB
_�B
_�B
`B
`vB
`�B
`�B
`�B
`�B
aB
aB
a-B
a-B
a|B
a�B
a�B
b�B
cB
b�B
b�B
b�B
b�B
b�B
cnB
cnB
c�B
c�B
c�B
c�B
d@B
d�B
eB
eFB
ezB
e�B
e�B
e�B
e�B
fB
fB
fLB
f�B
f�B
f�B
gRB
gmB
g�B
g�B
g�B
g�B
g�B
h
B
h>B
hXB
h�B
h�B
h�B
h�B
h�B
i*B
i_B
iyB
iyB
i�B
i�B
jKB
jB
jB
jB
jeB
j�B
kQB
kQB
k�B
k�B
l=B
lqB
l�B
l�B
mCB
mwB
m�B
m�B
m�B
m�B
m�B
nB
nIB
ncB
n�B
n�B
n�B
n�B
o5B
oiB
o�B
o�B
o�B
o�B
pB
p!B
pUB
p�B
p�B
q'B
qvB
q�B
q�B
q�B
q�B
q�B
q�B
r-B
r|B
r�B
s3B
s�B
tB
t�B
t�B
t�B
t�B
uZB
utB
u�B
u�B
u�B
v+B
v+B
vB
vFB
v�B
v�B
v�B
wB
wB
w2B
wLB
w�B
w�B
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
y	B
y	B
y>B
yXB
yXB
z^B
z^B
zDB
zDB
z*B
zxB
z�B
zxB
zxB
zxB
z�B
z�B
z�B
z�B
{B
{B
|PB
|�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BI�BI�BI�BI�BI�BI�BI�BJ	BI�BI�BJ	BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BI�BJ�BL~BO\B�aBʌB	��B	�RB
� B
�uB
�B
��B
�B
��B
�YB
��B
�eB
��B
�`B
ǔB
�/B
��B
�9B
oB
ZB
=<B
9B	�B	�B	�%B	�qB	�HB	�gB	�HB	o�B	Z�B	U2B	P�B	N<B	G_B	C�B	<jB	1�B	4B	%B��B��B�UB�B�DB�0B�B�B�B	�B	
�B	dB	gB	sB	yB	�B	�B	 \B	 \B	�B	B	)B	�B	�B	�B	HB	dB�jB�B��B��B	 B	%B	6B	,B	�B	�B	(XB	)�B	1[B	=VB	SuB	c�B	j�B	m�B	q�B	xRB	��B	�B	��B	��B	�cB	�B	��B	��B	�2B	�`B	�B	�LB	�zB	��B	�FB	�zB	�8B	��B	�B	�HB	��B	��B	�B	��B	�BB	��B	��B	�PB	��B	�0B	��B	��B	ѷB	�B	�:B	ԕB	ԕB	�_B	�bB	�B	��B	��B	�B	�fB	�B	�B	�XB	�B	��B	�B	��B	�B	�B	�B	�wB	�B	�B	��B	�AB	�OB	�GB	�ZB	��B	�fB	��B	��B	��B	��B	��B	�	B	�>B	��B	�rB	�$B	�XB	�	B	��B	�	B	�rB	��B	��B	��B	�8B	��B	��B	��B	�2B	��B	�B	��B	��B	�TB	�B	��B	��B	�B	�MB	��B	�-B	��B	��B	�oB	��B	��B	��B	��B	�wB	�B	�B	�B	��B	��B	�0B	�eB	��B	�DB	��B	�sB	�
B	�B	�mB	�B	�mB	�RB	�B	�B	�LB	�fB	�LB	�B	��B	�B	�B	�RB	�8B	�B	��B	�B	��B	��B	�B	�B	�B	��B	�B	�B	�B	�B	�B	�RB	�B	�B	�mB	�mB	�B	�B	��B	��B	�B	��B	��B	��B	�B	��B	�B	�B	�B	��B	�B	��B	�KB	�B	��B	�sB	�>B	�B	�8B	�8B	�RB	�XB	��B	�DB	�yB	�yB	�B	�yB	��B	�eB	�B	�B	��B	�B	��B	�0B	�B	�KB	��B	��B	�6B	�6B	�B	��B	��B	�IB	��B	�B	�B	��B	�B	��B	�cB	�/B	��B	�5B	�B	�B	�5B	��B	�?B	��B	�B	�RB	�xB	��B	�DB	��B	�B	�DB	��B	��B	�0B	�B	��B	�xB	��B	��B	�xB	��B	�JB	�B	��B	�qB	�VB	�qB	�VB	�B	�.B
�B
�B
�B
'B
AB
AB
'B
B
-B
-B
�B
�B
B
�B
�B
B
�B
�B
�B
�B
�B
tB
�B
�B
9B
B
�B
�B
�B
GB
�B
3B
�B
YB
�B
1B
�B
�B
�B
�B
EB
�B
�B
�B
�B
	7B
	�B
B

�B
DB

�B

	B

XB

�B
B
)B
�B
�B
dB
6B
"B
<B
�B
�B
�B
�B
�B
�B
"B
�B
�B
BB
�B
�B
�B
NB
hB
hB
 B
�B
TB
�B
TB
B
B
�B
�B
B
�B
B
�B
MB
B
�B
�B
�B
2B
�B
�B
aB
�B
�B
�B
�B
�B
MB
B
�B
SB
SB
�B
�B
_B
�B
7B
�B
7B
qB
�B
kB
�B
7B
�B
WB
B
�B
�B
B
dB
IB
~B
�B
B
!B
;B
!B
;B
VB
�B
�B
�B
 BB
�B
�B
�B
 B
�B
�B
;B
VB
�B
"hB
#�B
#�B
#TB
# B
#B
"�B
#B
#nB
#�B
$�B
$�B
$�B
$tB
$ZB
$ZB
$ZB
$�B
$�B
$�B
$�B
$�B
$�B
%B
$�B
%FB
%zB
&fB
&�B
&�B
&�B
'�B
(�B
)DB
)�B
*eB
*�B
+�B
+�B
+QB
+B
)�B
)DB
)*B
)_B
)yB
)DB
)DB
)DB
)_B
)�B
*eB
+B
+QB
+�B
,B
,=B
,=B
,�B
-wB
-�B
-�B
-�B
-]B
-wB
-�B
-�B
.}B
.�B
/5B
/�B
0;B
0oB
0UB
0;B
0;B
0;B
0;B
0;B
0B
0!B
/�B
/5B
/B
/ B
/ B
.�B
/iB
/OB
0;B
1B
4nB
6�B
7�B
7�B
88B
8�B
8�B
8RB
88B
8�B
9>B
9�B
9XB
8�B
8�B
8�B
8lB
8RB
8B
7�B
7�B
7LB
72B
72B
7�B
7�B
7�B
7�B
8B
8�B
9>B
9XB
9XB
9�B
9�B
9�B
:*B
:�B
<B
<�B
<�B
=B
=VB
=�B
>B
>B
>B
>BB
>�B
>]B
>]B
>�B
>�B
?HB
?.B
?.B
?}B
?�B
?�B
@B
@iB
@OB
@OB
@OB
@�B
A;B
A�B
A�B
A�B
BB
B�B
C�B
C�B
DgB
D�B
D�B
D�B
E9B
EmB
ESB
E�B
E�B
FB
FtB
F�B
F�B
F�B
F�B
GB
F�B
G+B
G_B
GzB
G_B
G�B
HB
H�B
HfB
H�B
H�B
H�B
H�B
IRB
I7B
IRB
IlB
I�B
J#B
J#B
J�B
J�B
K)B
KxB
K�B
LB
LB
L~B
L�B
MB
M�B
M�B
M�B
N<B
N�B
NpB
N�B
O\B
P�B
P�B
QB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R B
RTB
R�B
R�B
R�B
R�B
R�B
S[B
S[B
S�B
TB
TaB
T�B
T�B
T�B
U�B
UgB
U�B
VB
U�B
VB
VmB
V�B
V�B
V�B
V�B
WsB
W�B
W�B
W�B
W�B
W�B
XEB
X�B
X�B
X�B
Y1B
YeB
Y�B
Y�B
Z7B
[=B
[�B
\B
\CB
\�B
\�B
\xB
\�B
\�B
\�B
\�B
]dB
]�B
^B
^OB
^�B
^�B
^�B
^B
^5B
^5B
^�B
^�B
_B
_B
_;B
_pB
_�B
_�B
`BB
`�B
`�B
`�B
`�B
aB
aB
aB
aHB
aHB
a�B
a�B
b4B
b�B
b�B
b�B
b�B
b�B
b�B
cB
cnB
c�B
c�B
c�B
c�B
c�B
d@B
d�B
e,B
e`B
ezB
e�B
e�B
e�B
e�B
fB
f2B
f�B
f�B
f�B
gB
gmB
g�B
g�B
g�B
g�B
g�B
g�B
h$B
hXB
hsB
h�B
h�B
h�B
h�B
h�B
iDB
iyB
iyB
i�B
jB
i�B
jKB
jB
jeB
j�B
jB
j�B
kkB
kkB
k�B
k�B
lWB
l�B
l�B
l�B
mCB
m�B
m�B
m�B
m�B
m�B
m�B
nB
ncB
n}B
n�B
o B
n�B
o B
oOB
o�B
o�B
o�B
o�B
pB
pB
p;B
poB
p�B
p�B
qAB
qvB
q�B
q�B
q�B
rB
q�B
rB
r-B
r�B
r�B
sMB
s�B
tB
t�B
t�B
t�B
uB
utB
u�B
u�B
u�B
vB
v+B
vFB
vB
v`B
v�B
v�B
v�B
wB
wB
wLB
wLB
w�B
w�B
w�B
w�B
w�B
xB
xRB
xRB
xlB
x�B
x�B
x�B
x�B
y	B
y	B
yXB
yrB
yrB
z^B
zDB
zDB
zDB
zDB
z�B
z�B
zxB
zxB
zxB
z�B
z�B
z�B
{B
{�B
{�B
|PB
|�B
|�31111111111111111111111111111111111111111111111111113311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�o<#�
<t!<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<%zx<#�
<'�<B�8<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.05(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202004090046482020040900464820200409004648202207271135202022072711352020220727113520202207271537412022072715374120220727153741  JA  ARFMdecpA30a                                                                20200329003910  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200329003927  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200329003928  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200329003929  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200329003929  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200329003929  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200329003929                      G�O�G�O�G�O�                JA  ARUP                                                                        20200329005607                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200408154648  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200408154648  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20220727000000  CF  PSAL_ADJUSTED_QC@33B��G�O�                JM  ARSQJMQC2.0                                                                 20220727000000  CF  TEMP_ADJUSTED_QCB���B��G�O�                JM  ARCAJMTM1.0                                                                 20220727023520  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063741  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091505                      G�O�G�O�G�O�                