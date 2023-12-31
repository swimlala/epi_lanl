CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-08-01T09:37:13Z creation;2019-08-01T09:37:15Z conversion to V3.1;2022-08-02T05:12:18Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190801093713  20220818081508  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_018                    2C  D   APEX                            8420                            2.11.2                          846 @��X�q� 1   @��Y+��@,l�!-�d�H˒1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @@  @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB33B33B'��B0  B8  B?��BH  BP  BX  Bb��BfffBn��Bx  B��B�  B�  B�  B�  B�ffB�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  BЙ�Bԙ�B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C33C
  C33C�3C�fC  C  C  C�fC  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0L�C1��C3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@y�DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJfDJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @>�R@~�R@���@�
=A�A@z�A_�
A�
A�=qA�Q�A�  A�  A�Q�A�  A�  B   B{BQ�B{B  B'�RB0
=B8  B?BH  BO�BW�BbBf�\Bn�
Bw�
BB��B�  B���B�
=B�\)B�(�B��)B���B���B���B�  B��B���B�B���B���B���B�  B�\BЅBԙ�B�Q�B��)B��B���B�B���B��B���B�  B�
=C �C�RC��CC@ C	��C33C�qC�fC��C  C��C�3C�RC��C��C C"C#�qC&  C(C*�C,  C.
=C0G�C1C3�C5�qC7�RC9��C;�qC>  C@�CBCD  CE��CHCJ  CK��CM��CP  CR�CT  CU�qCW�qCY��C[��C]�RC`  Cb
=Cd�Cf
=Ch�Ci�qCk��Cm�RCo�RCr  Ct�Cv�Cx  CzC|  C~  C��C��C���C��qC�  C��C��C��C���C���C�  C�  C��C��C�HC�  C�HC��C��C�  C���C�HC��C��C��qC�HC�  C�  C���C�  C�  C�  C���C�  C��C�HC��)C���C�  C���C���C��qC���C��C��C�  C�HC�  C�  C��qC��)C�HC���C���C��C��C�HC�  C�  C��C��C��C��C�  C��qC�  C��qC��qC��C��C�HC�  C��qC��C��C�HC���C��)C��)C��qC�  C�  C�  C��C�HC��C�  C��)C�HC��C��C�HC�HC��qC�  C��C�HC�  C�  C���C��qC���C��qC��)C���C���C��)C�  C�HC��C��C�HC�HC�HC�  C�  C�HC�HC��qC���C��qC�HC���C��)C���C���C��)C��qC���D ~D �D\D�\D� DHD\D �D�HD  D~�D �D�HD �D�HD  D� D	�D	�HD	��D
� D�D� D�\D\D�D~�D  D� D �D� DHD��D�D~�D �D��DHD~D��D\D �D��D�D�HDHD� D�D|�D�\D��D�D~�D�\D� D��D\D  D��D �D��DHD��D HD � D ��D!~D!�\D"\D"�qD#\D$ �D$~�D$�D%\D&HD&��D'�D'�HD(HD(��D)�D)��D)�\D*~�D+�D+�3D,HD,��D- �D-�HD-�\D.� D/ �D/��D0�D0��D0��D1� D2  D2��D3�D3��D4  D4~�D5  D5�HD6�D6��D7�D7��D8  D8�HD9  D9~�D9�D:~�D;HD;��D< �D<\D<��D=\D>  D>��D? �D?~�D?��D@|)D@��DA~�DA�DB��DCHDC�HDC�\DD� DE  DE~�DF  DF��DG �DG~DG��DH~�DH�qDI��DJ�DJ��DK  DK��DL�DL��DL�\DM~DN �DN��DN�qDO~DP  DP�HDQ�DQ��DR �DR� DS  DS�HDTHDT��DT�\DU\DV �DV��DV�\DW~�DW�DX\DYHDY�HDZ �DZ� D[ �D[��D[��D\}qD] �D]��D]�\D^~D^�D_~�D` �D`�HDa �Da�HDb�Db��Dc  Dc\Dd  Dd��De�De�HDf  Df��Dg�Dg��Dg�qDh\DiHDi��DjHDj\Dk  Dk��Dl�Dl��Dl��Dm� DnHDn��Do�Do��DpHDp�HDq�Dq��Dr �Dr~�Ds �Ds�HDt�Dt��Du  Du��Dv�Dv�HDw  Dw��Dw�\Dx~�DyHDy��Dz  Dz\D{  D{��D{�\D|�HD}�D}�HD~ �D~� D  D�HD��D�@�D��D��\D��\D�@ D�� D�� D� �D�@�D��RD��RD���D�?\D�� D��RD� �D�@�D��RD��RD� RD�@RD���D��RD� RD�?�D�\D��
D�  D�@RD��D���D�  D�@�D�� D���D� �D�AHD���D���D� �D�?�D�\D���D�  D�?\D�
D��
D���D�@�D���D��RD���D�@RD�� D�� D� RD�@ D��D���D���D�?
D�~�D��RD� �D�?�D��D���D���D�?�D��D��\D� RD�@ D�~fD���D���D�?\D��RD�� D��\D�@�D��RD���D��\D�?�D�� D��HD�HD�AHD��HD��RD� �D�@RD�� D���D��\D�>�D�\D��RD� RD�@�D���D�� D��\D�?\D��D���D� �D�@ D�\D���D��\D�@ D���D��RD� �D�@�D���D��
D��\D�@ D�� D��RD�HD�@�D���D���D� �D�@ D�� D�� D� RD�@�D���D���D� �D�@ D�\D��
D���D�>�D�\D��RD� �D�?�D�\D�� D� RD�@ D�� D��RD� �D�@RD�\D���D� �D�?
D�\D�� D� RD�?\D��D��RD�  D�@RD��HD���D� �D�@�D�� D��
D�  D�?�D�� D��RD��\D�@ D��HD���D�  D�?�D��D�� D� RD�@�D���D���D�  D�?
D�� D��\D�  D�?�D�� D�� D� RD�@�D�\D��\D�  D�@�D���D��\D�  D�@RD��D��RD� �D�@�D���D��RD���D�@RD��D��\D�  D�?�D�� D���D�HD�@�D��RD�� D� RD�AHD���D��RD� �D�@RD��D��RD� �D�@RD��D���D� RD�@�D��HD���D�  D�?�D�~�D�� D�  D�?
D��D���D���D�@ D�\D���D�  D�@RD���D���D�  D�@ D�\D��
D���D�@�D�� D���D��\D�?\D D¿\D��\D�?�D��Dÿ\D���D�?
D��DĿ�D� RD�AHDŀ�D��RD� �D�@�Dƀ�D�� D��\D�>�Dǀ Dǿ�D���D�@RD�\D�� D�HD�A�DɁHDɿ�D��\D�@ Dʀ Dʿ�D��\D�@ DˀRD˿�D���D�@ D̀ D̿�D�  D�@�D̀ DͿ�D� RD�@ D΀RD��RD� RD�@�Dπ DϿ\D� RD�@RD�\D��RD� �D�@�D��Dѿ\D���D�@�DҀ�Dҿ�D���D�?
D��Dӿ\D���D�@�Dԁ�D���D� RD�@�D��Dտ\D��\D�?
D�~fD�� D� RD�?�D׀�D���D���D�@ D؀RDؿ\D���D�?�Dـ�D���D���D�>fDڀ�D���D��\D�?\Dۀ�D���D� RD�@�D܁HDܿ�D���D�AHD݀�D��RD�  D�@ Dހ�D޿�D���D�?�D߀�D�� D��\D�?\D�
D���D� �D�?\D�\D�� D�  D�?�D�\D���D�HD�?�D� D��HD� �D�@RD䀤D��RD� RD�@RD�RD��RD���D�?
D�
D�\D��\D�@ D�RD翮D��\D�@ D��D���D���D�?
D� D���D� �D�@ D�
D�\D� RD�@RD�RD��RD� �D�@�D��D�� D� �D�@�D��D���D� �D�@RD��D��RD���D�?�D�
D�� D���D�@ D���D���D���D�?
D� D���D�HD�AHD�D�� D�  D�@ D�RD���D� �D�?
D�\D��RD���D�?\D��D�� D� �D�@ D�� D��RD���D�@ D�� D��RD��\D�?
D���D���D���D�@RD���D�� D���D�?�D�
D��\D�  D�@�D���D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A�{A��A�A�
�A�	lA�fA�	�A�	�A�~A��A�A�~A�(A�%A��A��MA�ҽA�A��A�K�A��(A�M�A؀�A�n/AԆ%A�!�A�l�A�/OA��A�UgA�VmAȾwAǈfA�|�AŭAĒoA�A��6A��A��xA�7A�D3A�?A��A��A�6A��A��
A��A�0�A�x8A���A��A��A�49A���A�SA��2A�l"A���A��VA�/�A�B�A��[A�e,A�]/A��A�D�A�A��A��A���A� \A�LdA�{�A���A��A�-CA�WsA�N�A��PA��QA���A��A��UA{یAy��At��An5?AjkQAh��Ae��Aa�TA`�?A_�2A^�)AZ��AW�"AS�]AOzxAK��AH��AE�AB�wA@��A>��A;[WA9<�A5�A2��A1u%A0��A06A.�hA-1'A,��A,��A,�FA,�A,�A,��A,��A,�XA-l�A.�A.��A.�]A.�A-�A,S�A,d�A,�A+�A)`�A'\�A&��A&YKA&P�A&+kA&*�A%�mA%�A#��A"�KA"|A"/A!��A �gA�[A�"AU2A,�AP�A�A��A�sAs�AtTA2�A�mA��AS�A	�A�1A*0A�)AOvA�sA_�A+kA�qA|�Aw�AD�A1�A��A#:A�A�A�AߤA]dA��AzxA<6A1�A��Aw2A*0A�)A��A��A\�AԕA�"Ae�A+A
�^A
<�A	�CA	hsA��A��AK�A-�A�A�jAi�A#:A�A:�A�9A��A�LA�"A^�A �A��A��A%�A=AcA	AVA˒A�.A�A�A�A��A�$A��A:*A ��@�1�@�Q@���@�;@���@��@���@�m�@���@��@� �@��[@���@��@���@���@�	l@��@�(�@�q�@��?@��u@��o@�l�@�"�@��@�x�@�_@�&�@�A @��@�W?@@��@�
�@�˒@��`@�j@ꅈ@��)@�0�@�ϫ@��5@��@�@�|�@�H@�4n@�0U@�1@�8@䭬@�|�@��@�<6@�Mj@�A @�PH@�Vm@�%�@ߋ�@�[W@�X@�Q�@��@ݖS@�9�@��@���@ܻ�@܉�@�^5@ܚ�@��@ܝI@�S�@�[�@�Ov@۾w@�}V@�e@��D@٢�@��[@ؑ�@�N�@��@ׅ@��@�ff@���@�F@Ԭ�@�W�@�	@��Z@��Z@��g@�@�V�@�1@�7L@��?@�0U@�Q�@�{@�Q�@��P@̍�@��Q@�g�@�:�@��@�z�@ɱ[@��@��@ƾ�@Ƈ+@�H�@��>@�E9@Īe@�1�@�h�@�o�@�%F@��@��F@�.�@��D@��@�@�oi@�M@�($@��C@�P�@��D@��Z@�RT@��@��]@��f@��:@��@�ϫ@�e�@�\�@�Mj@��@���@��@��Y@�$@�\�@��@�G�@�;@��m@���@�i�@�(�@���@�X@��"@��@���@�@��C@��f@�U�@���@���@�>�@� i@��@�e�@�>B@�&�@��w@���@�bN@�1�@�u@�[W@���@��Y@�l�@�Ta@��D@���@�f�@� i@��!@�_�@��@�8@�c�@�$@�4@��&@�Vm@��@��|@���@�YK@���@��k@�E9@�Y@��@�@���@���@���@��@�}�@�$t@��	@��@�m]@�*0@�i�@��@�	@���@��@�G�@��E@���@���@�@��@�P�@�2a@���@��[@���@��@�e�@�~@��@�ϫ@���@�m]@�IR@��2@�C-@�!�@�O@��@���@���@��k@��	@�t�@�e,@�J#@��@���@�`�@��9@���@���@�x�@�$t@���@�<�@���@���@���@���@�(�@���@�d�@�A�@�1�@��@���@���@�Ta@�x@��@���@�U�@�7L@�&@���@�{@��@�zx@�s@�_p@�6z@��@���@��U@���@��j@��@�g8@�($@�`B@��@���@��x@��Y@�W�@� �@��)@���@�a@�8@��$@�U2@�x@�u�@�l�@�iD@�f�@�c�@�^�@�/@���@��@�bN@��@�˒@���@�L�@��8@��U@��A@�K^@�)�@��d@�j@�@@��@���@�~�@�R�@��@��@@�w2@�?}@��@���@���@�q@�x@�a@RT@�@~�8@~�s@~L0@}�@|��@{��@{�@{(@zȴ@y��@yk�@y+�@x��@x4n@w�@w��@w�@va|@v@u��@uG�@u�@tɆ@s�&@sO@r�@r��@rC�@q�@qa�@qL�@q7L@qV@p�[@p��@p�Y@pb@o��@oo�@o
=@n�m@nkQ@m�@m�@lA�@ka@k"�@k i@k
=@j�<@j$�@i��@i�M@iIR@h��@h	�@g�[@g�f@g.I@fȴ@f��@f@e�H@ex�@d��@b�@b�x@bB[@b_@aL�@`�9@`|�@`D�@_� @_X�@_�@^�6@^i�@]�@]m]@\m�@[�}@[b�@Z��@Z�@Y�@Y��@Y��@Y��@Y�@X��@XV�@Xb@W�0@W�f@WZ�@W$t@V�c@V�@U�j@U4@Tq@S�@S�a@S{J@S1�@R��@R��@R��@RM�@R($@Q�>@Q��@Qa�@Q(�@P�p@P�z@P~(@P[�@PA�@P4n@O��@O��@On/@O�@N�b@M��@M�@Lh�@K�@K�*@K��@K��@KC@J�H@J��@J��@J�@I}�@H�j@H��@H*�@GX�@F�]@F��@Fc @F#:@E�@E[W@E5�@E�@D�@DZ@C�]@Cn/@B�@B_@A�^@Ap�@A�@@��@@��@@�@@��@@U2@@*�@?��@?b�@?/�@>��@>1�@=�@<��@<�@;ƨ@;)_@:�]@:�R@:�+@:)�@9�X@9�@8��@8��@8�@8y>@8S�@7�V@6��@6� @6n�@6R�@6-@5�z@5j@5�@5@4��@4��@4�/@4]d@3�@3�@3�A@3�@3�@3�&@3��@3W?@3S@2҉@2��@2-@1��@1o @0�P@0�@/�@/�V@/s@/l�@/4�@.��@.+k@-�@-�@-(�@,Ĝ@,l"@,M@,6@,�@+��@+4�@*�c@*�+@*6�@)�j@)�'@)/@(ی@(�.@(<�@'�F@'|�@'C@&�h@&_�@&�@%�3@%��@%e,@%%F@$�f@$��@$��@$z�@$K^@$/�@$	�@#��@#l�@#"�@"�@#
=@"�@"h
@"&�@"($@"5?@"�@!��@!e,@!L�@!�@ �@ Ĝ@ �@ ��@ �u@ q@ 4n@�@�K@��@v`@/�@�@�B@Z�@�@�D@��@�d@��@<6@%@�[@�@c�@PH@Ft@/�@$@�@�+@��@~�@RT@U�@,�@�@�@҉@�x@8�@&�@�)@��@�@�^@[W@�P@r�@j@r�@D�@�;@�V@�f@j�@Mj@@�'@�A@V@kQ@J�@-@��@}�@!�@�P@�	@�@�@��@�I@A�@�@n/@A�@S@�@�R@��@h
@��@�^@��@^�@V@�K@��@�j@�.@g8@K^@'R@�@�A@�&@��@��@s@1�@�@a|@O@	@�@�@�@�@��@7L@�@�|@�`@��@��@�@~(@$@�m@�@�@�[@|�@K�@+@�@�@
��@
��@
��@
u%@
.�@	�@	��@	G�@	�@��@�$@��@�@�o11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A�{A��A�A�
�A�	lA�fA�	�A�	�A�~A��A�A�~A�(A�%A��A��MA�ҽA�A��A�K�A��(A�M�A؀�A�n/AԆ%A�!�A�l�A�/OA��A�UgA�VmAȾwAǈfA�|�AŭAĒoA�A��6A��A��xA�7A�D3A�?A��A��A�6A��A��
A��A�0�A�x8A���A��A��A�49A���A�SA��2A�l"A���A��VA�/�A�B�A��[A�e,A�]/A��A�D�A�A��A��A���A� \A�LdA�{�A���A��A�-CA�WsA�N�A��PA��QA���A��A��UA{یAy��At��An5?AjkQAh��Ae��Aa�TA`�?A_�2A^�)AZ��AW�"AS�]AOzxAK��AH��AE�AB�wA@��A>��A;[WA9<�A5�A2��A1u%A0��A06A.�hA-1'A,��A,��A,�FA,�A,�A,��A,��A,�XA-l�A.�A.��A.�]A.�A-�A,S�A,d�A,�A+�A)`�A'\�A&��A&YKA&P�A&+kA&*�A%�mA%�A#��A"�KA"|A"/A!��A �gA�[A�"AU2A,�AP�A�A��A�sAs�AtTA2�A�mA��AS�A	�A�1A*0A�)AOvA�sA_�A+kA�qA|�Aw�AD�A1�A��A#:A�A�A�AߤA]dA��AzxA<6A1�A��Aw2A*0A�)A��A��A\�AԕA�"Ae�A+A
�^A
<�A	�CA	hsA��A��AK�A-�A�A�jAi�A#:A�A:�A�9A��A�LA�"A^�A �A��A��A%�A=AcA	AVA˒A�.A�A�A�A��A�$A��A:*A ��@�1�@�Q@���@�;@���@��@���@�m�@���@��@� �@��[@���@��@���@���@�	l@��@�(�@�q�@��?@��u@��o@�l�@�"�@��@�x�@�_@�&�@�A @��@�W?@@��@�
�@�˒@��`@�j@ꅈ@��)@�0�@�ϫ@��5@��@�@�|�@�H@�4n@�0U@�1@�8@䭬@�|�@��@�<6@�Mj@�A @�PH@�Vm@�%�@ߋ�@�[W@�X@�Q�@��@ݖS@�9�@��@���@ܻ�@܉�@�^5@ܚ�@��@ܝI@�S�@�[�@�Ov@۾w@�}V@�e@��D@٢�@��[@ؑ�@�N�@��@ׅ@��@�ff@���@�F@Ԭ�@�W�@�	@��Z@��Z@��g@�@�V�@�1@�7L@��?@�0U@�Q�@�{@�Q�@��P@̍�@��Q@�g�@�:�@��@�z�@ɱ[@��@��@ƾ�@Ƈ+@�H�@��>@�E9@Īe@�1�@�h�@�o�@�%F@��@��F@�.�@��D@��@�@�oi@�M@�($@��C@�P�@��D@��Z@�RT@��@��]@��f@��:@��@�ϫ@�e�@�\�@�Mj@��@���@��@��Y@�$@�\�@��@�G�@�;@��m@���@�i�@�(�@���@�X@��"@��@���@�@��C@��f@�U�@���@���@�>�@� i@��@�e�@�>B@�&�@��w@���@�bN@�1�@�u@�[W@���@��Y@�l�@�Ta@��D@���@�f�@� i@��!@�_�@��@�8@�c�@�$@�4@��&@�Vm@��@��|@���@�YK@���@��k@�E9@�Y@��@�@���@���@���@��@�}�@�$t@��	@��@�m]@�*0@�i�@��@�	@���@��@�G�@��E@���@���@�@��@�P�@�2a@���@��[@���@��@�e�@�~@��@�ϫ@���@�m]@�IR@��2@�C-@�!�@�O@��@���@���@��k@��	@�t�@�e,@�J#@��@���@�`�@��9@���@���@�x�@�$t@���@�<�@���@���@���@���@�(�@���@�d�@�A�@�1�@��@���@���@�Ta@�x@��@���@�U�@�7L@�&@���@�{@��@�zx@�s@�_p@�6z@��@���@��U@���@��j@��@�g8@�($@�`B@��@���@��x@��Y@�W�@� �@��)@���@�a@�8@��$@�U2@�x@�u�@�l�@�iD@�f�@�c�@�^�@�/@���@��@�bN@��@�˒@���@�L�@��8@��U@��A@�K^@�)�@��d@�j@�@@��@���@�~�@�R�@��@��@@�w2@�?}@��@���@���@�q@�x@�a@RT@�@~�8@~�s@~L0@}�@|��@{��@{�@{(@zȴ@y��@yk�@y+�@x��@x4n@w�@w��@w�@va|@v@u��@uG�@u�@tɆ@s�&@sO@r�@r��@rC�@q�@qa�@qL�@q7L@qV@p�[@p��@p�Y@pb@o��@oo�@o
=@n�m@nkQ@m�@m�@lA�@ka@k"�@k i@k
=@j�<@j$�@i��@i�M@iIR@h��@h	�@g�[@g�f@g.I@fȴ@f��@f@e�H@ex�@d��@b�@b�x@bB[@b_@aL�@`�9@`|�@`D�@_� @_X�@_�@^�6@^i�@]�@]m]@\m�@[�}@[b�@Z��@Z�@Y�@Y��@Y��@Y��@Y�@X��@XV�@Xb@W�0@W�f@WZ�@W$t@V�c@V�@U�j@U4@Tq@S�@S�a@S{J@S1�@R��@R��@R��@RM�@R($@Q�>@Q��@Qa�@Q(�@P�p@P�z@P~(@P[�@PA�@P4n@O��@O��@On/@O�@N�b@M��@M�@Lh�@K�@K�*@K��@K��@KC@J�H@J��@J��@J�@I}�@H�j@H��@H*�@GX�@F�]@F��@Fc @F#:@E�@E[W@E5�@E�@D�@DZ@C�]@Cn/@B�@B_@A�^@Ap�@A�@@��@@��@@�@@��@@U2@@*�@?��@?b�@?/�@>��@>1�@=�@<��@<�@;ƨ@;)_@:�]@:�R@:�+@:)�@9�X@9�@8��@8��@8�@8y>@8S�@7�V@6��@6� @6n�@6R�@6-@5�z@5j@5�@5@4��@4��@4�/@4]d@3�@3�@3�A@3�@3�@3�&@3��@3W?@3S@2҉@2��@2-@1��@1o @0�P@0�@/�@/�V@/s@/l�@/4�@.��@.+k@-�@-�@-(�@,Ĝ@,l"@,M@,6@,�@+��@+4�@*�c@*�+@*6�@)�j@)�'@)/@(ی@(�.@(<�@'�F@'|�@'C@&�h@&_�@&�@%�3@%��@%e,@%%F@$�f@$��@$��@$z�@$K^@$/�@$	�@#��@#l�@#"�@"�@#
=@"�@"h
@"&�@"($@"5?@"�@!��@!e,@!L�@!�@ �@ Ĝ@ �@ ��@ �u@ q@ 4n@�@�K@��@v`@/�@�@�B@Z�@�@�D@��@�d@��@<6@%@�[@�@c�@PH@Ft@/�@$@�@�+@��@~�@RT@U�@,�@�@�@҉@�x@8�@&�@�)@��@�@�^@[W@�P@r�@j@r�@D�@�;@�V@�f@j�@Mj@@�'@�A@V@kQ@J�@-@��@}�@!�@�P@�	@�@�@��@�I@A�@�@n/@A�@S@�@�R@��@h
@��@�^@��@^�@V@�K@��@�j@�.@g8@K^@'R@�@�A@�&@��@��@s@1�@�@a|@O@	@�@�@�@�@��@7L@�@�|@�`@��@��@�@~(@$@�m@�@�@�[@|�@K�@+@�@�@
��@
��@
��@
u%@
.�@	�@	��@	G�@	�@��@�$@��@�@�o11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�?B	�YB	�tB	�B	��B	�tB	��B	�^B	�]B	өB	�|B	��B	��B	��B
�B
9XB
9	B
-CB
-�B
'B
#�B
2�B
B�B
q[B
��B
�:B
ȀB
յB
��B
��B
�B
��B
��B�B�B#�B=�BR Bd�Bv+B�uB� B�'B��B�B�<B��B�zB��B��B�KB�gB�~B�vB�DB�JB�!B�:B�BB��B`�B
ߤB
��B
��B
�B
��B
��B
z�B
q�B
e�B
\�B
YeB
JrB
&�B
�B
 �B	��B	��B	��B	�\B	�"B	��B	�OB	n�B	g�B	aB	W�B	D�B	0oB	�B	UB��B�B�'B�uBڠB��B�uB��BԕB�B��B��B��B��B�]B��B��B	�B	$�B	0UB	3�B	9	B	@�B	VB	q�B	�	B	�IB	�bB	��B	�NB	�:B	�,B	��B	��B	��B	��B	�vB	��B	��B	��B	��B	�B	��B	چB	߾B	�B	�:B	�zB	�>B	��B	��B	�=B	��B	�LB	�0B	�2B	�B
 iB
6B
�B
\B
}B
�B
EB
�B
dB
CB
�B
�B
�B
�B
�B
!-B
!�B
#�B
!�B
OB
�B
�B
VB
�B

�B
	�B

	B
�B
B
B
�B
�B
�B
B
�B
dB
B
�B
pB
B
WB
�B
MB
�B
�B
	�B
�B
B
�B

rB

rB
6B
�B
�B
vB
B
}B
�B
TB
�B
�B
uB
�B
�B
#�B
./B
/�B
-�B
'�B
(XB
'�B
&�B
&�B
(>B
)�B
&�B
dB
@B
\B
B
	7B
1B

rB
<B
B
�B
IB
�B
MB
hB
\B
�B
	�B
�B

�B
;B
VB
#�B
'RB
&LB
$B
#�B
"�B
 �B
 �B
�B
/B
�B
�B
B
kB
�B
B
�B
�B
�B
�B
�B
~B
�B
	�B
	7B

	B
~B
}B
4B
�B
�B
"B
B
�B
PB
NB
B
BB
�B
	B
�B
zB
B
	�B
�B
KB
EB
�B
�B
�B
	B
	�B
^B
~B
�B
0B
B
vB
�B
�B
�B
pB
�B
�B
VB
(B
�B
B
BB
�B
�B
B
�B
�B
�B
VB
B
�B
&B
B
�B
}B
bB
NB
HB
�B
�B
�B
\B
�B
�B
�B
�B
�B
�B
�B

rB
	lB
�B
KB
_B
YB
MB
�B	�B	�B	�B	��B	�?B	��B	�B	�nB	��B	�3B	�B	��B	�nB	�%B	�+B	��B	�fB	�B	��B	�DB
 �B
�B
	�B
^B
�B
0B
<B
B
�B
�B
�B
"B
JB
�B

�B

=B
	7B
	B
1B
fB
	�B

�B

�B
DB

�B

�B
^B
0B

rB
�B
zB
�B
�B
+B
+B
+B
�B
	B
	�B
	lB
	RB

rB

�B
B
B
B
�B
�B
B
�B
B
PB
�B
BB
B
}B
bB
}B
B
NB
NB
NB
B
 B
�B
�B
&B
&B
&B
&B
B
&B
,B
{B
�B
aB
B
B
B
sB
sB
�B
�B
B
�B
�B
yB
_B
B
eB
�B
�B
B
7B
QB
QB
kB
=B
qB
�B
�B
B
CB
/B
5B
B
B
5B
�B
�B
�B
�B
�B
�B
!B
�B
 �B
 �B
!|B
!-B
!B
!B
 �B
!|B
"B
!�B
"4B
#�B
$�B
$ZB
#:B
$�B
%FB
%�B
&B
&LB
&�B
&�B
'B
'B
'mB
'�B
'�B
'�B
(�B
(�B
(�B
)B
(�B
)B
)DB
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*B
+QB
+�B
,B
,B
,"B
,=B
,qB
,qB
,�B
-B
-B
-�B
./B
.�B
0UB
0UB
0;B
0;B
0!B
/�B
/�B
/�B
/�B
0B
0oB
0�B
1[B
1vB
1'B
1AB
1�B
1vB
1AB
1[B
1�B
2GB
2�B
2�B
3B
2�B
3�B
4B
4B
4TB
49B
5B
4�B
5�B
5�B
5�B
5�B
6B
6FB
6FB
6zB
6�B
6�B
8B
8RB
88B
8�B
9rB
9rB
9�B
9�B
:DB
:DB
:DB
:^B
:DB
:DB
:^B
:DB
:�B
;JB
<B
<jB
<�B
<�B
<�B
=<B
=�B
=�B
=�B
=�B
=�B
=qB
=�B
>B
>B
=�B
=�B
=�B
>B
=�B
=B
<�B
<�B
<�B
=B
=<B
=<B
=�B
=�B
>(B
>]B
>wB
>�B
>�B
>�B
?B
?}B
?�B
@OB
?�B
?�B
@B
AoB
AoB
A�B
A�B
BuB
B[B
B�B
B�B
CB
CGB
CaB
C{B
C�B
C�B
C�B
EB
EB
EmB
E�B
FB
FB
FB
FB
E�B
E�B
FB
F�B
GzB
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
IRB
I�B
I�B
I�B
J=B
K)B
K^B
KDB
KB
K^B
K�B
K�B
K�B
LB
LdB
L�B
L�B
L�B
L�B
L�B
M6B
M6B
M�B
M�B
M�B
L�B
L�B
L�B
MjB
MjB
M6B
M6B
M�B
M�B
M�B
M�B
M�B
NVB
N�B
N�B
O(B
O�B
O�B
O�B
P.B
PHB
PbB
P�B
P�B
P�B
QNB
QhB
Q�B
Q�B
R�B
R�B
S&B
SuB
S�B
S�B
S�B
S�B
S�B
TFB
T{B
T�B
T�B
T�B
U�B
U�B
U�B
V�B
W$B
W�B
XEB
XB
XEB
X_B
X�B
X�B
YB
YB
Y�B
Y�B
Y�B
Y�B
Z�B
Z�B
[#B
[=B
[=B
[�B
[�B
\�B
]/B
]IB
]~B
]�B
]�B
^�B
^�B
^�B
^�B
_B
_;B
_VB
`�B
abB
a�B
a�B
a�B
a�B
bB
b�B
b�B
bNB
a�B
a�B
bB
c B
c�B
d@B
d�B
d�B
d�B
d�B
d�B
d�B
dZB
d@B
dZB
d�B
d�B
eB
eB
eB
eFB
e`B
e�B
e�B
e�B
f2B
f�B
gB
gmB
gmB
g�B
g�B
h
B
h�B
h�B
i*B
iDB
j0B
kB
k�B
l"B
lB
k�B
lqB
l�B
lqB
lWB
l�B
m)B
l�B
mB
m�B
m�B
n/B
n�B
o B
o B
oB
oB
o5B
oOB
oiB
o�B
o�B
p;B
p!B
pB
p;B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
qvB
q�B
qAB
qB
q'B
qB
q'B
qvB
q�B
q�B
r�B
r�B
sB
sB
sMB
sMB
shB
sB
sMB
shB
sB
s3B
sB
r�B
sB
r�B
tB
sMB
sB
s3B
tB
t9B
tnB
tnB
tTB
t�B
t�B
t�B
t�B
t�B
t�B
u�B
vzB
w2B
v�B
v�B
v�B
wB
w�B
x�B
xlB
xRB
x8B
w�B
w�B
w�B
w�B
w�B
xB
xRB
xRB
x�B
x�B
x�B
x�B
y	B
y�B
z*B
z*B
z�B
{0B
{JB
{�B
{�B
{�B
{�B
|B
{�B
|jB
|�B
|�B
|�B
}�B
~BB
~]B
~BB
~(B
~(B
~B
~B
~BB
~�B
~�B
~�B
~�B
~�B
~�B
�B
�B
�B
�iB
�B
��B
��B
��B
��B
� B
�;B
�;B
��B
��B
�B
�oB
��B
�B
�uB
��B
��B
��B
�B
�B
�-11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�YB	�tB	��B	�?B	��B	��B	�=B	�\B	�-B	��B	��B	��B	�PB	��B
"�B
=B
>BB
0oB
2aB
+�B
%�B
4nB
E�B
s�B
��B
��B
��B
רB
��B
�,B
� B
�B
�BDB/B%�B@OBS�Be�BwLB�3B��B��B�yB��B�B��B��B��B�B�B�sB��B�hB��B�cB��B�LB��B�HBl�B
��B
��B
��B
�B
�B
��B
~B
w�B
i�B
_�B
]�B
PHB
*0B
KB
�B	�
B	�B	��B	��B	��B	�xB	�3B	poB	i*B	c�B	\xB	H�B	5�B	# B	%B��B�B� B�SB��B��B�%BߤB�+B�{B��B�2B��B�TB��B��B��B	B	$tB	0UB	4B	8�B	?�B	U�B	qB	�#B	�OB	��B	��B	��B	�B	��B	��B	��B	��B	�pB	��B	��B	�3B	�>B	��B	ɆB	�B	�#B	�\B	�nB	�ZB	��B	�_B	��B	��B	�qB	�B	�RB	�B	��B	�lB
 �B
B
�B
B
 B
�B
�B
qB
5B
�B
7B
B
IB
�B
�B
!|B
!�B
$tB
"�B
;B
�B
yB
�B
~B
DB

XB

XB
�B
�B
�B
4B
MB
B
7B
B
B
jB
;B
�B
�B
B
�B
�B
�B
�B

�B
�B
?B
1B
B

�B
�B
BB
\B
�B
HB
�B
B
�B
�B
�B
@B
qB
;B
#:B
.IB
0;B
.}B
($B
(�B
($B
'RB
&�B
(�B
*�B
(�B
�B
�B
�B
�B
	�B
1B

rB
"B
�B
OB
B
�B
B
B
HB
JB

�B
_B
	�B
VB
VB
#�B
'�B
&�B
$tB
$@B
#:B
!bB
!HB
 �B
B
B
�B
#B
�B
�B
�B
�B
2B
�B
&B
�B
B
�B

	B
	lB

=B
�B
�B
hB
4B
HB
VB
�B
6B
jB
�B
�B
�B
�B
	lB
�B
zB
KB

	B
JB
�B
zB
�B
B
�B
	B
	�B
DB
�B
B
JB
<B
�B
HB
�B
B
�B
pB
�B
�B
vB
.B
}B
�B
bB
\B
pB
6B
B
"B
pB
HB
&B
�B
:B
 B
�B
 B
B
B
.B
(B
B
�B
�B
�B
�B
B
VB
B
JB

�B
	�B
	B
�B
�B
�B
�B
3B
 �B	�RB	�FB	��B	�tB	�B	��B	��B	�B	�hB	�B	��B	��B	��B	��B	�+B	��B	�B	��B	��B
 �B
�B
	�B
xB
�B
dB
pB
(B
B
BB
BB
�B
�B
�B

�B

rB
	RB
	7B
�B
�B
	�B

�B
)B
�B
)B
)B
�B
�B
B
�B
�B
+B
B
EB
_B
�B
1B
	�B
	�B
	�B
	�B

�B
)B
)B
)B
^B
�B
0B
dB
B
PB
�B
pB
�B
HB
�B
�B
�B
NB
hB
�B
�B
TB
oB
�B
B
@B
&B
@B
@B
@B
�B
{B
�B
�B
�B
mB
mB
�B
�B
�B
�B
�B
_B
�B
�B
�B
�B
eB
B
�B
�B
7B
QB
�B
�B
�B
WB
�B
�B
B
CB
�B
�B
OB
5B
5B
jB
�B
�B
�B
�B
�B
B
;B
 B
 �B
!HB
!�B
!-B
!HB
!bB
!HB
!�B
"NB
"B
"NB
#�B
%,B
$�B
#nB
$�B
%`B
&B
&�B
'B
&�B
'B
'B
'8B
'�B
'�B
(
B
(
B
(�B
(�B
(�B
)*B
)B
)*B
)DB
)�B
)�B
)�B
)�B
)�B
)�B
*KB
*�B
+�B
,B
,"B
,"B
,WB
,qB
,�B
,�B
-B
-CB
-wB
-�B
.}B
.�B
0UB
0UB
0;B
0;B
0;B
0B
/�B
0!B
0!B
0;B
0�B
1'B
1�B
1�B
1vB
1[B
1�B
1�B
1�B
1�B
1�B
2aB
2�B
33B
3MB
3MB
4B
4TB
4TB
4�B
4nB
5%B
5%B
5�B
5�B
5�B
5�B
6+B
6`B
6�B
6�B
7LB
7fB
8RB
8lB
8lB
8�B
9rB
9�B
9�B
9�B
:^B
:^B
:xB
:�B
:^B
:^B
:�B
:^B
:�B
;�B
<PB
<�B
<�B
<�B
=B
=qB
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>(B
>(B
>(B
=�B
>(B
>]B
=�B
=VB
="B
<�B
=B
=B
=VB
=qB
=�B
>B
>BB
>�B
>�B
>�B
?B
?B
?HB
?�B
?�B
@�B
@B
@B
@�B
A�B
A�B
A�B
A�B
B�B
BuB
B�B
B�B
CGB
C{B
C�B
C�B
C�B
DB
DMB
E9B
E9B
E�B
FB
F%B
FB
F%B
F%B
FB
F%B
F%B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
HKB
H�B
IB
I�B
J	B
I�B
J	B
JXB
KDB
KxB
K^B
K)B
KxB
K�B
K�B
K�B
L0B
L~B
L�B
L�B
L�B
MB
MB
MjB
MPB
M�B
M�B
M�B
M6B
MB
MB
MjB
M�B
MPB
MPB
M�B
M�B
M�B
M�B
N"B
N�B
OB
N�B
OvB
O�B
O�B
O�B
PHB
PbB
P}B
P�B
P�B
P�B
QhB
Q�B
Q�B
R B
R�B
SB
S[B
S�B
S�B
S�B
S�B
S�B
S�B
TaB
T{B
T�B
UB
U2B
U�B
U�B
VB
W
B
WYB
W�B
X_B
X+B
X_B
X�B
X�B
Y1B
Y�B
Y�B
Y�B
ZB
Y�B
ZB
Z�B
[	B
[=B
[WB
[WB
[�B
[�B
\�B
]/B
]IB
]~B
]�B
]�B
^�B
^�B
^�B
^�B
_!B
_VB
_pB
`�B
a|B
a�B
a�B
a�B
a�B
bNB
b�B
b�B
b�B
a�B
a�B
b4B
c:B
c�B
dtB
d�B
d�B
d�B
eB
d�B
d�B
dtB
dZB
dtB
d�B
eB
eFB
e,B
e,B
e`B
e�B
e�B
fB
fLB
ffB
f�B
gB
g�B
g�B
g�B
h$B
h$B
h�B
i*B
iDB
i_B
jeB
kB
k�B
l=B
l"B
lB
l�B
l�B
l�B
lWB
mB
mwB
mB
mB
mwB
m�B
nIB
n�B
oB
oB
o5B
o5B
o5B
o5B
oiB
o�B
pB
poB
p;B
p!B
pUB
p�B
p�B
p�B
qB
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q[B
q'B
q'B
qB
q'B
qvB
q�B
q�B
r�B
r�B
sMB
sB
shB
shB
shB
s3B
sMB
s�B
s3B
s3B
s3B
r�B
s3B
s3B
tB
s�B
sB
s3B
t9B
tTB
t�B
t�B
tnB
t�B
t�B
t�B
t�B
t�B
t�B
u�B
v�B
wfB
v�B
v�B
wB
wB
w�B
x�B
x�B
xlB
xRB
xB
w�B
w�B
w�B
w�B
x8B
x�B
xlB
x�B
x�B
x�B
x�B
y$B
y�B
z*B
z^B
z�B
{0B
{B
{�B
{�B
{�B
{�B
|6B
|B
|�B
|�B
|�B
}B
}�B
~BB
~wB
~wB
~(B
~(B
~(B
~(B
~wB
~�B
~�B
~�B
~�B
~�B
B
�B
�B
�B
��B
�B
�B
�B
�B
��B
�;B
�UB
�UB
�B
�B
� B
��B
��B
�'B
��B
��B
��B
��B
�B
�B
�G33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<(�U<?�[<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201908120042102019081200421020190812004210202207271132112022072711321120220727113211202207271534492022072715344920220727153449  JA  ARFMdecpA30a                                                                20190801093642  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190801093713  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190801093714  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190801093715  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190801093715  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190801093715  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190801093715                      G�O�G�O�G�O�                JA  ARUP                                                                        20190801095713                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190802000000  CF  PSAL_ADJUSTED_QC@>�R@~�RG�O�                JM  ARCAJMQC2.0                                                                 20190811154210  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190811154210  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023211  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063449  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818081508                      G�O�G�O�G�O�                