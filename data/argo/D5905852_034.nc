CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-01-09T09:38:40Z creation;2020-01-09T09:38:42Z conversion to V3.1;2022-08-02T05:11:35Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200109093840  20220818091504  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               "A   JA  A30_8420_034                    2C  D   APEX                            8420                            2.11.2                          846 @���5� 1   @�����~ @,��w�kQ�c3�g��1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @,��@�  @���@���A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B���B�  B�33B�  B�  B�  B�  B�  B�  B�ffB���B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B���B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C�C33C  C�fC!�fC$  C&  C(  C*  C,  C.  C0  C1�fC4  C6  C8  C:  C<  C>  C@  CB  CC�fCF  CH  CJ  CL  CN  CP  CR�CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw�fDx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @+�@\)@��R@��RA (�A@Q�A`Q�A�=qA�  A�(�A�{A�(�A�=qA�(�A�  B 
=B�B(�B{B �B(�B0{B8
=B@(�BH33BO�BX{B`(�Bg��Bo��Bx
=B�
=B�#�B�B�#�B��B�\B�=qB�\B���B��B�B�
=B�  B�ffB���B���B���B�B�B���B�
=B�B�B�
=B�
=B�
=B�{B��B���B���B��fB���C   C�C�qC  C�qC	��C�CC�C  CCC�C
CEC��C�C!�C#��C%��C(  C*�C,C.  C/�qC1�RC4�C6C8�C:  C;�qC>�C@�CB�CC�RCE�qCH�CJCL�CNCP�CR{CTCV  CW��CY�qC\C^C`�Cb
=Cd�Ce�qChCj�Cl�Cn
=Cp�CrCt�Cv�Cx�CzC|�C}��C�qC�HC��C�HC���C��qC��qC��qC��)C���C��C�  C�  C�C�HC��)C�HC��C�  C�HC��C��C��C��C��C��C��C��qC��qC��C��C��C�C��C��C��C�C��C�C�C�HC�HC�HC�HC�  C�  C�  C���C�  C��C��C��C��C�  C���C��C��C�  C�C�C�  C���C���C�HC��C�C�HC��C�fC�C��C�  C��qC�HC��C��C��C���C��qC��qC��C��C��C�fC��C��C�C�C��C�HC��C��C��C��C��qC�HC��C�fC��C��C��C��C��C�HC�HC�HC�HC�HC�HC��C�  C��C��C�fC��C�  C��C�C��C�C�C�HC�HC��C��C���C��C��D  �D ��DHD\D  D��D�D��D�D� DHD��DHD��DHD� DHD�HD�\D	��D
  D
\D �D\D�\D�HDHD�HDHD� DHD��D�D� D  D\D �D��D  D��D�D��D3D��D�D��D�D��D�\D� D�D�D�D��DHD�HD  D��D �D� D  D�HD{D�3D �D ��D!�D!��D"�D"�HD#HD#��D$  D$��D%  D%� D&HD&�3D'�D'��D(3D(��D)  D)� D*�D*��D+HD+�HD,�D,��D,��D-~D.  D.�HD/HD/� D/�\D0� D1 �D1�HD2HD2� D2�\D3��D4HD4� D5  D5��D6�D6� D6�\D7\D7�\D8~�D8�\D9\D9�\D:��D; �D;�HD<�D<� D=  D=�HD> �D>�HD?�D?�HD@  D@� DA3DA�3DB�DB�HDC�DC��DD3DD�{DE�DE�HDFHDF�HDGHDG��DH3DH�HDH�\DI~�DJ�DJ��DK  DK�HDL�DL��DM�DM��DNHDN�HDOHDO��DP�DP� DQ �DQ��DQ�\DR��DS3DS��DT�DT�3DUHDU\DU��DV\DWHDW��DX3DX�HDY �DY��DZ�DZ��D[  D[�HD\ �D\� D] �D]��D^HD^�3D_�D_� D` �D`��Da�Da�HDa��Db\Dc  Dc�HDd  Dd�HDe3De��Df3Df�{Dg3Dg�HDh  Dh� Dh�\Di��Dj�Dj�HDk�Dk��DlHDl��Dm3Dm�3Dn�Dn�3Do3Do�HDp  Dp� Dq  Dq��Dr�Dr�3Ds�Ds�HDtHDt��Du �Du��Dv  Dv� Dw�Dw�Dx�Dx��Dy  Dy��Dz3Dz�HD{ �D{� D{�\D|~�D}�D}��D}�\D~� D  D��D� RD�@ D���D���D�HD�A�D���D���D� RD�@�D���D���D� RD�@�D��HD���D�  D�@�D��HD��HD� RD�?�D���D��HD�HD�@RD�� D���D��D�@�D���D��HD� �D�@�D��HD��HD� RD�?�D��D���D��D�A�D���D��RD���D�?
D�� D���D� �D�@�D��RD���D�  D�?�D��D��RD� RD�?�D��D�� D� RD�A�D���D��=D� �D�@�D��=D���D� �D�@�D���D��RD� �D�@RD��D�� D� RD�@�D���D���D� �D�@RD��RD�� D�  D�@ D�� D���D� �D�@�D��RD��RD� �D�@RD��RD���D�  D�@�D���D��RD�  D�@RD��RD���D�  D�?\D�� D���D�HD�@RD���D��HD� �D�@RD��RD���D� �D�@RD��RD��\D���D�?�D�
D��\D��\D�?�D�� D���D� �D�@�D��RD��RD� RD�?\D�
D�� D� �D�@�D���D��HD� �D�AHD���D�� D� �D�A�D���D���D�HD�A�D���D���D��\D�@RD��HD���D��\D�@RD��HD�� D� �D�B=D���D��RD��\D�@ D���D���D�HD�@�D�� D���D��D�A�D��HD���D� �D�@�D��D���D��\D�?�D��RD�� D���D�@RD��D���D� �D�@�D���D���D� �D�@RD��D��\D� �D�AHD��HD���D���D�?\D�� D���D�HD�AHD���D���D� RD�@RD��HD�� D��\D�@RD���D���D��D�@�D���D��HD� �D�@RD�� D�� D� RD�@�D���D�� D� �D�A�D���D���D� �D�@RD��RD��RD� �D�@�D��RD�� D� �D�@RD��D���D�HD�@ D���D���D�HD�AHD���D���D� RD�AHD���D���D��D�AHD��HD��HD�HD�AHD��RD��RD��D�@�D��D���D�HD�@ DÀ�D���D�  D�?�D�\DĿ\D���D�@�Dŀ�D���D��D�@ Dƀ�D�� D�  D�@�Dǀ�D���D� �D�AHDȀ�D���D� RD�@ DɀRD���D��D�@�DʀRD���D�HD�@�DˀRD���D� �D�@RD̀�D���D� �D�@RD̀�D���D� �D�A�D΀�D���D�HD�@�D��D�� D� �D�@�DЀRD��RD� �D�@�DсHD���D���D�@ DҀRD���D� RD�@ DӀ D��HD� �D�@RDԁ�D���D� �D�@RDՀRD���D� �D�@ D��D��HD�HD�@�D׀�D��HD�=D�@�D��D��RD�HD�@�DـRD���D�HD�@RDڀ Dڿ�D��\D�?�Dۀ D��RD� �D�@RD܀RD���D��D�@�D݀�D��HD�HD�AHDށHD��HD�HD�@RD߀RD��HD�HD�@RD��RD���D� RD�@�D��D���D� RD�@ D��D���D�HD�A�D�RD���D��D�@RD� D���D� �D�?
D�\D�\D�  D�@ D� D��HD� �D�@RD瀤D�� D���D�@ D��D���D� �D�A�D��D��RD� �D�AHD�HD���D�=D�AHD��D���D� �D�A�D��D��HD� �D�AHD��D���D��D�@�DD��RD� RD�@�DD��HD� �D�@ D��HD���D��D�A�D�D��RD�  D�?\D�
D�\D��\D�@ D��D���D� RD�@�D�\D�� D� RD�?�D���D���D�HD�@RD��D���D���D�@ D��RD���D���D�?
D�� D��HD� �D�@�D��RD���D� �D�@�D��HD��HD�HD�@RD���D��{11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A���A�MA�SA��A�A�
rA�A��A��A��A��A��A�(A�hA��A�bA��A��A��A�.A��A�4A�PA��A�VA��A��A�	lA���A���A��A��Aв�A�=A��A�GA��VA��A�یA� �A��A�;�A�q�A�� A��mA���A��FA�q�A��A��A��A�MA���A�wfA��FA���A�!bA��bA��A���A�4�A��A��]A�A�U2A�ޞA��DA�)�A�e,A�,�A�A|u�Ay~(Aw�Au��Ar�Ap�AlZ�Ae��A`x�A[?�AXoiAU�+ATU�APXyAN-�AK�fAH��ACs�A@��A?Z�A=�A<XA;XA:��A9c A7-A6�A5�A4�A2��A2zxA2T�A1��A14A0�A0�A/%A-��A-GEA,�;A+��A*� A*�A'��A$_pA!}�ArGA��AxAK�A�A4A�pA�"A�_ARTAVA8�AGAcA_�A�yA�rA&�Ah�A�.A�HA&�Ah�A(�AϫA^�A��A͟A~(AI�A�XA&�A<6AHA�Ar�A_ACAѷA�!A��A	Af�A��A_pA;A	AVmA�AAA�7A�A)�AB�A˒A��A
�.A
��A	�)A	J#A�]AC-A�AL0A;A��AMA�'AJ�A�)Ah
A�A8A��A�A��Ar�A9XA �yA �0A rG@��Q@��_@���@�4�@���@��@���@��7@�6z@�K^@��z@���@���@��@��@�@@��@�@�w�@�@��"@�!@�4�@�;�@�}�@��@�kQ@��K@�.I@���@�i�@��@�7@��@�@�`�@��@�U�@��@�`B@�g8@�YK@�  @��@�c�@�>�@��@�YK@�l�@�-@߿H@�qv@�y>@�Dg@��@�U2@���@�2a@ڳh@��
@��@ؾ@ؕ�@ؐ.@�2�@��Q@��M@�@�2a@��U@�L0@��@��@҂A@��@�]�@�&�@��/@Ў�@�u%@�g8@�H�@���@ϲ-@�Vm@�҉@�-@�6z@̡b@��@�g�@��c@ʧ@�x@�@ȿ�@�?�@��A@��z@ǚk@�9�@��@���@�J�@��@�|�@��@�~�@�	�@�4�@��s@°�@�K^@�e@�� @�g�@��@���@�d�@�@�6z@���@��L@��@�7�@���@��h@��@�u�@�1'@�@��@��3@��@�o�@���@���@�PH@��$@��s@��b@�|�@��@�C�@�!@��.@��~@��"@���@���@�z�@�-@���@���@�$t@���@���@�5?@��@���@�Vm@�(@���@���@���@��@�j�@�ی@���@�tT@�W�@�<�@��#@��P@�[W@��@��y@��f@��$@��6@�=@���@��E@��@���@�xl@��@�ԕ@��@�/@���@��x@�!@���@�
=@���@�R�@��@���@�@��F@�U2@�*�@��Q@�k�@�.I@��	@��@�u�@�33@�֡@�ں@���@��Y@�2�@��=@�A�@��@�?@�˒@���@�^�@�(�@��@��@�M@��@���@�p�@�%F@��[@��A@�7�@��@���@���@�o @��8@�ѷ@��[@��@��@��@�r�@�
�@�ԕ@�ƨ@���@��@�B[@��@���@��@�ȴ@���@�_@�%�@��@���@�+@��@���@���@���@�oi@�h�@�[�@���@���@�iD@�%@���@�p;@�~@��>@��*@�f�@�Y@��H@��h@��@�c @�?@��@�{J@��@��v@��e@�_�@�!�@��@��@���@���@���@���@�dZ@�0�@��?@���@�~@��@@��P@��R@��F@�2�@��;@��[@���@�\)@�Y@��@���@�r�@�:�@��@�w2@��@��p@��@�p;@�#:@��F@��	@�w2@�l�@�@O@��c@��B@���@�u�@�^5@�?�@�  @���@�O@���@�ff@�E�@�!�@�4@��@��:@�g�@�"�@� i@��@���@�Ta@��@~�@~R�@}�T@}��@}8�@};@|u�@{��@{iD@{$t@z��@zJ@yS&@yq@y	l@x�	@x�?@x�o@w�w@wF�@w�@v�@v{�@u��@uo @t��@tz�@t:�@t�@s��@r^5@r6�@q�o@q(�@p��@pK^@p/�@o��@ol�@o!-@n��@n��@n+k@m�@m��@mX@l�v@l�I@l%�@k�[@kb�@k�@j�@jd�@i�Z@ic�@i+�@h��@h�/@h�E@hN�@g�]@g��@g��@g��@f�@fff@f6�@e�3@e(�@d�	@d�9@dtT@c�}@c+@b�2@b��@b�@b5?@a�@aY�@aQ�@a	l@`��@`(�@_˒@_s@_Z�@^�@^Z�@]�)@]hs@]5�@] \@]%@\��@\�9@[�@[6z@Z� @Z4@Y�o@Y�z@Y�"@X�@X�@X"h@W��@Wݘ@WO@V�L@V��@V}V@V-@Ux�@Tq@S��@S�a@S�{@S33@S.I@R�m@RH�@Q�@Q��@Q:�@QV@P��@PU2@P!@P$@P	�@O��@O�@N�]@N�x@N0U@M�"@MDg@L�`@L"h@Kƨ@K�@KdZ@K�@J�!@JkQ@J-@Izx@I�@H1'@G��@Gs@GF�@G
=@F��@F�B@Fq�@F1�@E�@E4@D�p@D��@DtT@D*�@Cݘ@C�q@Cy�@C9�@C�@B��@B�@B��@BC�@A�=@A=�@@��@@��@@��@@e�@?�r@?��@?1�@>�@>��@>��@>��@>q�@>$�@=�#@=��@=u�@=	l@<��@<��@<x@;t�@:�]@:�b@:c @:W�@:
�@9<6@8�U@8��@8z�@8]d@8/�@7�k@7e�@7E9@7/�@7�@6��@6i�@6@�@6�@5�z@5�7@5�@4�E@4�@3�A@3�k@3X�@2��@2�@2s�@2B[@1�Z@1��@1x�@1*0@0��@0l"@/�}@/b�@/&@.��@.�@.l�@.3�@-��@-��@-X@,ѷ@,�@+�r@+��@+qv@+A�@*��@*��@*R�@*5?@)�Z@)�=@)�@)s�@)Vm@)#�@(�@(�[@(�@(:�@'�W@'˒@'�k@'U�@&�c@&a|@&@%�t@%S&@$�	@$�@$ی@$Ɇ@$�@$Z@$,=@#خ@#��@#e�@#;d@"�@"͟@"�X@"�@"��@"v�@"�@!��@!��@!\�@ ��@ S�@ �@��@�0@x@W?@=@1�@/�@ں@�@�\@~�@J�@!�@��@��@u�@`B@T�@q@�|@�E@��@>B@6@,=@�@��@�Q@��@��@�4@y�@|�@�4@iD@1�@��@v�@YK@=q@;�@ �@��@A @�@�o@e�@S�@I�@*�@�@��@�@�*@Z�@@�'@��@:*@.�@($@($@{@�H@�~@Vm@�@�|@��@|�@PH@H@:�@~@�&@�0@��@�:@~�@9�@�@�1@�@��@7L@�@��@��@�/@Ɇ@��@�I@�o@q@S�@H@Ft@Ft@C-@��@�@�@@|�@C�@&@�@�B@��@h
@L0@3�@��@�@�3@��@^�@A @<6@8�@8�@/@�@�K@�`@Ɇ@�$@��@�@��@�u@j@?�@�@��@g�@1�@
�@
��@
�@
c @
?@
)�@
!�@
J@	��@	�9@	��@	�~@	��@	��@	?}@	;@��@ѷ@�@�@I�@��@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A���A�MA�SA��A�A�
rA�A��A��A��A��A��A�(A�hA��A�bA��A��A��A�.A��A�4A�PA��A�VA��A��A�	lA���A���A��A��Aв�A�=A��A�GA��VA��A�یA� �A��A�;�A�q�A�� A��mA���A��FA�q�A��A��A��A�MA���A�wfA��FA���A�!bA��bA��A���A�4�A��A��]A�A�U2A�ޞA��DA�)�A�e,A�,�A�A|u�Ay~(Aw�Au��Ar�Ap�AlZ�Ae��A`x�A[?�AXoiAU�+ATU�APXyAN-�AK�fAH��ACs�A@��A?Z�A=�A<XA;XA:��A9c A7-A6�A5�A4�A2��A2zxA2T�A1��A14A0�A0�A/%A-��A-GEA,�;A+��A*� A*�A'��A$_pA!}�ArGA��AxAK�A�A4A�pA�"A�_ARTAVA8�AGAcA_�A�yA�rA&�Ah�A�.A�HA&�Ah�A(�AϫA^�A��A͟A~(AI�A�XA&�A<6AHA�Ar�A_ACAѷA�!A��A	Af�A��A_pA;A	AVmA�AAA�7A�A)�AB�A˒A��A
�.A
��A	�)A	J#A�]AC-A�AL0A;A��AMA�'AJ�A�)Ah
A�A8A��A�A��Ar�A9XA �yA �0A rG@��Q@��_@���@�4�@���@��@���@��7@�6z@�K^@��z@���@���@��@��@�@@��@�@�w�@�@��"@�!@�4�@�;�@�}�@��@�kQ@��K@�.I@���@�i�@��@�7@��@�@�`�@��@�U�@��@�`B@�g8@�YK@�  @��@�c�@�>�@��@�YK@�l�@�-@߿H@�qv@�y>@�Dg@��@�U2@���@�2a@ڳh@��
@��@ؾ@ؕ�@ؐ.@�2�@��Q@��M@�@�2a@��U@�L0@��@��@҂A@��@�]�@�&�@��/@Ў�@�u%@�g8@�H�@���@ϲ-@�Vm@�҉@�-@�6z@̡b@��@�g�@��c@ʧ@�x@�@ȿ�@�?�@��A@��z@ǚk@�9�@��@���@�J�@��@�|�@��@�~�@�	�@�4�@��s@°�@�K^@�e@�� @�g�@��@���@�d�@�@�6z@���@��L@��@�7�@���@��h@��@�u�@�1'@�@��@��3@��@�o�@���@���@�PH@��$@��s@��b@�|�@��@�C�@�!@��.@��~@��"@���@���@�z�@�-@���@���@�$t@���@���@�5?@��@���@�Vm@�(@���@���@���@��@�j�@�ی@���@�tT@�W�@�<�@��#@��P@�[W@��@��y@��f@��$@��6@�=@���@��E@��@���@�xl@��@�ԕ@��@�/@���@��x@�!@���@�
=@���@�R�@��@���@�@��F@�U2@�*�@��Q@�k�@�.I@��	@��@�u�@�33@�֡@�ں@���@��Y@�2�@��=@�A�@��@�?@�˒@���@�^�@�(�@��@��@�M@��@���@�p�@�%F@��[@��A@�7�@��@���@���@�o @��8@�ѷ@��[@��@��@��@�r�@�
�@�ԕ@�ƨ@���@��@�B[@��@���@��@�ȴ@���@�_@�%�@��@���@�+@��@���@���@���@�oi@�h�@�[�@���@���@�iD@�%@���@�p;@�~@��>@��*@�f�@�Y@��H@��h@��@�c @�?@��@�{J@��@��v@��e@�_�@�!�@��@��@���@���@���@���@�dZ@�0�@��?@���@�~@��@@��P@��R@��F@�2�@��;@��[@���@�\)@�Y@��@���@�r�@�:�@��@�w2@��@��p@��@�p;@�#:@��F@��	@�w2@�l�@�@O@��c@��B@���@�u�@�^5@�?�@�  @���@�O@���@�ff@�E�@�!�@�4@��@��:@�g�@�"�@� i@��@���@�Ta@��@~�@~R�@}�T@}��@}8�@};@|u�@{��@{iD@{$t@z��@zJ@yS&@yq@y	l@x�	@x�?@x�o@w�w@wF�@w�@v�@v{�@u��@uo @t��@tz�@t:�@t�@s��@r^5@r6�@q�o@q(�@p��@pK^@p/�@o��@ol�@o!-@n��@n��@n+k@m�@m��@mX@l�v@l�I@l%�@k�[@kb�@k�@j�@jd�@i�Z@ic�@i+�@h��@h�/@h�E@hN�@g�]@g��@g��@g��@f�@fff@f6�@e�3@e(�@d�	@d�9@dtT@c�}@c+@b�2@b��@b�@b5?@a�@aY�@aQ�@a	l@`��@`(�@_˒@_s@_Z�@^�@^Z�@]�)@]hs@]5�@] \@]%@\��@\�9@[�@[6z@Z� @Z4@Y�o@Y�z@Y�"@X�@X�@X"h@W��@Wݘ@WO@V�L@V��@V}V@V-@Ux�@Tq@S��@S�a@S�{@S33@S.I@R�m@RH�@Q�@Q��@Q:�@QV@P��@PU2@P!@P$@P	�@O��@O�@N�]@N�x@N0U@M�"@MDg@L�`@L"h@Kƨ@K�@KdZ@K�@J�!@JkQ@J-@Izx@I�@H1'@G��@Gs@GF�@G
=@F��@F�B@Fq�@F1�@E�@E4@D�p@D��@DtT@D*�@Cݘ@C�q@Cy�@C9�@C�@B��@B�@B��@BC�@A�=@A=�@@��@@��@@��@@e�@?�r@?��@?1�@>�@>��@>��@>��@>q�@>$�@=�#@=��@=u�@=	l@<��@<��@<x@;t�@:�]@:�b@:c @:W�@:
�@9<6@8�U@8��@8z�@8]d@8/�@7�k@7e�@7E9@7/�@7�@6��@6i�@6@�@6�@5�z@5�7@5�@4�E@4�@3�A@3�k@3X�@2��@2�@2s�@2B[@1�Z@1��@1x�@1*0@0��@0l"@/�}@/b�@/&@.��@.�@.l�@.3�@-��@-��@-X@,ѷ@,�@+�r@+��@+qv@+A�@*��@*��@*R�@*5?@)�Z@)�=@)�@)s�@)Vm@)#�@(�@(�[@(�@(:�@'�W@'˒@'�k@'U�@&�c@&a|@&@%�t@%S&@$�	@$�@$ی@$Ɇ@$�@$Z@$,=@#خ@#��@#e�@#;d@"�@"͟@"�X@"�@"��@"v�@"�@!��@!��@!\�@ ��@ S�@ �@��@�0@x@W?@=@1�@/�@ں@�@�\@~�@J�@!�@��@��@u�@`B@T�@q@�|@�E@��@>B@6@,=@�@��@�Q@��@��@�4@y�@|�@�4@iD@1�@��@v�@YK@=q@;�@ �@��@A @�@�o@e�@S�@I�@*�@�@��@�@�*@Z�@@�'@��@:*@.�@($@($@{@�H@�~@Vm@�@�|@��@|�@PH@H@:�@~@�&@�0@��@�:@~�@9�@�@�1@�@��@7L@�@��@��@�/@Ɇ@��@�I@�o@q@S�@H@Ft@Ft@C-@��@�@�@@|�@C�@&@�@�B@��@h
@L0@3�@��@�@�3@��@^�@A @<6@8�@8�@/@�@�K@�`@Ɇ@�$@��@�@��@�u@j@?�@�@��@g�@1�@
�@
��@
�@
c @
?@
)�@
!�@
J@	��@	�9@	��@	�~@	��@	��@	?}@	;@��@ѷ@�@�@I�@��@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�B��B�B��B�KB�eB�KB�eB�KB�KB�eB�B�B�B�B�B�B��B�B�B��B�B��B��B��B��B��B��B�B�B�B�B�jB�B	��B
`\B
~B
��B
�zB
��B
��B
d�B
`B
VmB
\�B
bNB
��B
�vB
��B	lBVB \B �B-B
�B
��B
|�B
bB
`'B
X�B
=�B
5�B
,qB	��B	�'B	�#B	��B	��B	��B	�lB	�B	�WB	�=B	�PB	�UB	��B	� B	v`B	K�B	/�B	B	�B�0B�6B�B��B�zB�CB�AB��B�B�MB��B��B	�B	 �B	*�B	+kB	*eB	:�B	>BB	F�B	LdB	N�B	PHB	PbB	PHB	P�B	U2B	[WB	i_B	i�B	c�B	\�B	N�B	<�B	�B�JB�B�BңB��B	KB	!�B	&B	+QB	P�B	l�B	HB	��B	��B	�vB	� B	�|B	��B	�BB	�TB	�$B	�8B	��B	��B	��B	�"B	��B	�cB	�}B	��B	�B	�HB	�BB	�9B	ѷB	�FB	��B	�vB	�|B	�B	�-B	�B	�-B	޸B	��B	�xB	چB	��B	��B	�1B	�B	�EB	��B	��B	��B	�B	ٚB	�B	�KB	��B	�B	��B	��B	�xB	ܒB	ܬB	ܒB	ۦB	�xB	�B	�pB	��B	�B	�hB	�~B	��B	ۦB	��B	�B	�qB	��B	�~B	��B	ߤB	�B	ݘB	�B	�B	��B	޸B	ބB	�5B	��B	ݘB	یB	�kB	ٴB	ڠB	�B	��B	��B	�EB	��B	��B	��B	�YB	յB	ԕB	�FB	�MB	�gB	��B	�B	��B	�mB	�9B	յB	ՁB	�gB	՛B	�gB	�2B	��B	�,B	ԯB	�B	�mB	��B	ٚB	��B	ٴB	�1B	��B	��B	�+B	ևB	��B	�?B	��B	��B	�_B	�CB	�B	��B	�~B	�B	�jB	��B	ޞB	��B	�B	�VB	ߤB	��B	�B	�B	�B	��B	��B	��B	��B	�B	�zB	�B	��B	�B	�sB	�B	��B	�6B	�KB	�KB	�kB	�WB	�B	�]B	�cB	�B	��B	� B	� B	�5B	�B	�OB	�5B	��B	�oB	��B	��B	�B	�B	�vB	�B	�vB	��B	��B	�-B	�-B	�-B	��B	�GB	��B	�B	�B	�B	��B	�B	�9B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�8B	��B	��B	��B	�rB	��B	�JB	��B	�VB	�"B	��B	�PB	��B	��B	��B	��B	�B	��B	�.B	�}B	�cB	��B	��B	��B	�.B	��B
 4B
 �B
oB
UB
�B
�B
�B
AB
[B
AB
AB
�B
[B
[B
AB
B
gB
9B
%B
�B
tB
�B
EB
EB
zB
�B
	�B

�B

XB

XB

�B

�B

#B
	�B

=B
	�B
	�B

�B
�B
�B
�B
dB
�B
�B
�B
jB
�B
"B
B
�B
HB
4B
�B
�B
B
<B
�B
�B
BB
vB
vB
vB
�B
�B
�B
�B
:B
�B
TB
oB
�B
B
�B
uB
�B
uB
uB
�B
SB
9B
SB
9B
SB
B
+B
+B
�B
�B
�B
+B
yB
B
�B
�B
�B
1B
�B
�B
WB
�B
�B
B
�B
�B
�B
�B
�B
5B
OB
�B
�B
B
�B
 B
 �B
!bB
"�B
#TB
#�B
#�B
#�B
#�B
#:B
"NB
!�B
!�B
# B
#:B
#�B
$�B
&B
&fB
&�B
&�B
'B
'RB
'mB
'�B
'�B
'�B
(>B
)yB
)�B
)�B
*0B
*�B
+6B
+�B
-wB
,�B
-]B
-�B
-�B
.cB
.�B
.�B
-�B
-wB
-]B
-)B
,�B
+�B
,"B
,�B
,�B
-�B
-�B
-wB
-]B
-�B
-�B
-�B
.B
./B
.cB
/B
/�B
/�B
/�B
/�B
/�B
0;B
0UB
0�B
0�B
0�B
1[B
1AB
1�B
2�B
2�B
3B
33B
3hB
3hB
3�B
49B
4TB
4TB
4�B
5%B
5�B
5�B
5�B
5�B
5�B
6B
6�B
6�B
6�B
6�B
72B
7�B
7�B
8B
8lB
8�B
8�B
8�B
9�B
9�B
9�B
:*B
:�B
:�B
:�B
:�B
;dB
;�B
;�B
<B
<jB
<�B
<�B
<�B
=B
=VB
=�B
=�B
=�B
>B
>BB
>wB
>�B
?cB
?}B
@ B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
AoB
A�B
A�B
A�B
B'B
BB
B[B
BAB
B�B
CB
CB
C-B
B�B
CaB
C�B
C�B
C�B
C�B
DB
D�B
D�B
D�B
D�B
EB
E�B
E�B
FB
F?B
F?B
F?B
F%B
F%B
F�B
F�B
GB
GEB
GEB
G_B
GzB
HB
H�B
IRB
I�B
I�B
J�B
J�B
J�B
JrB
J�B
J�B
LJB
LJB
LdB
L�B
L�B
L�B
L�B
MPB
MjB
MjB
MPB
MjB
M�B
M�B
M�B
M�B
M�B
NVB
N�B
N�B
N�B
OB
OvB
OvB
O�B
P�B
P�B
P�B
P�B
Q4B
QNB
Q�B
Q�B
R:B
R:B
S&B
SuB
S�B
S�B
S�B
S�B
S�B
T,B
TFB
T�B
UB
U�B
U�B
U�B
U�B
VB
VB
V9B
VmB
VmB
V�B
VmB
VSB
VmB
V�B
V�B
W$B
WYB
W�B
W�B
XEB
XyB
X�B
X�B
X�B
X�B
YB
Y1B
Y�B
Y�B
Y�B
ZB
ZkB
Z�B
Z�B
[#B
[=B
[WB
[qB
[WB
[	B
[=B
\)B
\xB
\]B
\xB
\xB
\�B
]�B
]�B
^B
^B
^jB
^�B
^�B
_B
_!B
_pB
_�B
`\B
`\B
`\B
`�B
aB
abB
abB
abB
abB
a�B
b4B
b�B
b�B
cB
c B
cnB
dB
dZB
dZB
d�B
dtB
dtB
d�B
d�B
d�B
d�B
eFB
ezB
e�B
fLB
ffB
f�B
f�B
f�B
g8B
gRB
g�B
g�B
g�B
h
B
h
B
hXB
h�B
h�B
h�B
i*B
i_B
i_B
iyB
i�B
i�B
jKB
jB
j�B
kQB
k�B
k�B
k�B
k�B
k�B
l"B
l=B
l�B
l�B
m]B
m�B
m�B
nB
nB
nB
n/B
ncB
n�B
o B
oB
o5B
o�B
p!B
p;B
p;B
pUB
poB
p�B
p�B
p�B
pUB
p�B
qB
q'B
qAB
q�B
q�B
q�B
q�B
q�B
rB
rB
r|B
r|B
raB
r�B
r�B
r�B
r�B
r�B
sB
sB
sMB
s3B
s�B
shB
shB
shB
s�B
s�B
s�B
tTB
tTB
tnB
t9B
t�B
t�B
u%B
utB
u�B
vB
u�B
vB
vFB
v`B
v`B
vzB
vzB
v�B
v�B
wB
w2B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
x8B
xlB
x�B
x�B
x�B
x�B
y	B
yXB
y�B
y�B
y�B
y�B
y�B
y�B
z�B
{B
{�B
|B
|PB
|PB
|PB
|6B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}qB
}�B
}�B
}�B
~B
~B
~]B
~wB
~�B
~�B
~�B
cB
HB
}B
�B
�B
�B
� B
� B
� B
� B
�4B
�4B
�OB
�OB
�iB
�iB
�iB
�iB
�OB
�iB
��B
��B
��B
�UB
�oB
��B
�B
�'B
�[B
��B
��B
��B
��B
��B
��B
��B
�GB
�-B
�B
�{B
��B
�GB
�aB
�{B
�{B
��B
�B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�kB�B��B�B��B�KB�eB�KB�eB�KB�KB�eB�B�B�B�B�B�B��B�B�B��B�B��B��B��B��B��B�B�!B��B�B��B�B�RB	ðB
c�B
�-B
��B
�B
��B
�B
h�B
g�B
ZB
a�B
d@B
��B
�hB
�]B�B �B#TB&�B
�B
�;B
�B
�B
c�B
dtB
_VB
AoB
;�B
4�B	��B	��B	ЗB	�cB	�RB	��B	�B	�B	޸B	�B	��B	�B	��B	��B	~BB	R:B	5�B	�B		B�(B	 �B	 B	 iB�B�B�9B�B�*B��B��B�B	KB	#TB	,"B	,qB	,WB	<6B	>�B	G+B	M6B	O\B	Q B	Q�B	Q�B	R:B	U�B	\CB	j�B	kB	e�B	_�B	S@B	@OB	 \B�(B�mB�.B�B��B	1B	!�B	%�B	*�B	P.B	l�B	�B	�)B	��B	�BB	��B	��B	��B	��B	��B	��B	��B	�$B	�B	�dB	��B	��B	��B	� B	�UB	�B	�B	�(B	��B	�B	�{B	�9B	��B	��B	�|B	��B	�B	��B	�VB	ބB	ݘB	�WB	�1B	�B	ٚB	ٚB	�B	�B	�B	�kB	�B	�QB	�7B	�7B	ڠB	ڠB	ڠB	�qB	�/B	�B	�IB	�/B	�CB	�B	ݲB	�B	�B	�B	�:B	�B	�xB	�B	�)B	�xB	��B	�qB	�5B	��B	�BB	�pB	�B	�OB	�OB	�!B	�!B	�!B	޸B	ޞB	�B	�CB	�#B	�kB	�#B	چB	ٴB	�KB	��B	�yB	�yB	ؓB	��B	�B	��B	��B	՛B	յB	�gB	�MB	�gB	ּB	ևB	�B	��B	��B	�9B	�B	՛B	��B	�{B	��B	�MB	֡B	�QB	�B	چB	�kB	ٚB	�KB	ٚB	��B	��B	�?B	רB	�B	�_B	��B	ܬB	�IB	��B	ݘB	�jB	��B	�OB	�;B	ߊB	�pB	߾B	�B	�HB	�vB	�vB	�\B	��B	��B	�B	��B	�4B	�B	��B	��B	��B	��B	�B	�kB	�B	��B	�B	�B	�B	��B	��B	�B	�cB	� B	�5B	�5B	�B	�B	�B	�B	�;B	�B	��B	��B	�'B	�B	��B	�B	��B	�B	�-B	�|B	�|B	�|B	�aB	�B	�MB	�B	�B	��B	�B	�B	�B	�nB	��B	��B	��B	��B	��B	��B	��B	�>B	��B	�8B	�RB	��B	��B	��B	�JB	�"B	�qB	�<B	�<B	��B	��B	�B	��B	�B	�HB	�.B	�}B	��B	�}B	�B	��B	��B	�HB
  B
 iB
 �B
�B
�B
aB
B
�B
uB
uB
uB
�B
�B
�B
�B
uB
-B
�B
�B
tB
�B
�B
�B
_B
zB
�B
	B
	�B

�B

�B

�B
DB
DB

�B

=B

rB

#B
	�B
B
�B
�B
0B
�B
�B
B
B
B
B
VB
BB
�B
bB
hB
 B
HB
\B
�B
(B
BB
vB
�B
�B
�B
�B
�B
B
�B
oB
 B
�B
�B
�B
@B
�B
�B
�B
�B
�B
�B
SB
9B
SB
SB
�B
B
EB
_B
_B
KB
�B
_B
�B
1B
1B
1B
B
KB
B
�B
�B
�B
�B
/B
�B
�B
�B
�B
B
jB
�B
�B
�B
;B
�B
 BB
 �B
!�B
"�B
#nB
#�B
$B
$B
$B
#�B
"�B
"4B
"B
#TB
#nB
#�B
$�B
&2B
&�B
&�B
&�B
'B
'�B
'�B
'�B
'�B
(
B
(�B
)�B
)�B
)�B
*eB
*�B
+QB
+�B
-�B
-)B
-�B
-�B
-�B
.�B
/ B
/ B
./B
-�B
-�B
-]B
,�B
+�B
,=B
,�B
-)B
-�B
-�B
-�B
-wB
.B
-�B
./B
./B
.�B
.�B
/OB
/�B
/�B
/�B
/�B
0B
0oB
0�B
0�B
0�B
1B
1�B
1�B
2-B
2�B
2�B
3B
3MB
3�B
3�B
3�B
4TB
4nB
4�B
4�B
5?B
6B
5�B
6B
5�B
6+B
6`B
6�B
6�B
6�B
72B
7fB
7�B
8B
8RB
8�B
8�B
8�B
9$B
9�B
9�B
:B
:^B
:�B
:�B
:�B
;0B
;B
;�B
;�B
<6B
<�B
<�B
<�B
<�B
="B
=�B
=�B
=�B
=�B
>(B
>]B
>�B
>�B
?}B
?�B
@ B
?�B
@ B
@�B
@�B
@�B
@�B
AB
A�B
A�B
A�B
BB
BAB
B'B
BuB
BuB
B�B
C-B
C-B
CGB
C-B
C�B
C�B
C�B
C�B
D3B
DMB
D�B
D�B
D�B
D�B
E9B
E�B
E�B
F%B
F?B
F?B
FYB
FYB
FtB
F�B
F�B
GEB
G_B
G_B
GzB
G�B
H1B
H�B
IlB
I�B
I�B
J�B
J�B
J�B
J�B
J�B
KDB
L~B
LdB
L~B
L�B
L�B
L�B
MB
MjB
M�B
M�B
MjB
M�B
M�B
M�B
M�B
M�B
NB
NpB
N�B
N�B
N�B
OBB
O�B
O�B
O�B
P�B
P�B
P�B
P�B
Q4B
QhB
Q�B
Q�B
RoB
R�B
S@B
S�B
S�B
S�B
S�B
S�B
TB
TFB
T{B
T�B
U2B
U�B
U�B
U�B
U�B
VB
V9B
VSB
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W
B
W?B
WsB
W�B
XB
XyB
X�B
X�B
X�B
X�B
X�B
Y1B
YKB
Y�B
Y�B
ZB
Z7B
Z�B
Z�B
Z�B
[WB
[qB
[WB
[�B
[qB
[=B
[qB
\CB
\xB
\xB
\�B
\�B
\�B
^B
^B
^B
^5B
^�B
_B
_B
_!B
_;B
_�B
_�B
`vB
`vB
`�B
`�B
a-B
a|B
a|B
a|B
a|B
a�B
bNB
b�B
b�B
cB
cTB
c�B
d&B
dZB
dZB
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e`B
e�B
fB
ffB
f�B
f�B
f�B
gB
gRB
gmB
g�B
g�B
h
B
h
B
h$B
hsB
h�B
h�B
h�B
iDB
iyB
i_B
i�B
i�B
jB
jKB
j�B
j�B
kkB
k�B
k�B
k�B
k�B
lB
l=B
l=B
l�B
l�B
m]B
m�B
m�B
nB
nB
nB
nIB
ncB
n�B
oB
o5B
oOB
o�B
p;B
pUB
p;B
poB
p�B
poB
p�B
p�B
pUB
qB
p�B
qAB
q[B
q�B
q�B
q�B
rB
q�B
rB
r-B
r�B
r|B
r|B
r�B
r�B
r�B
r�B
sB
s3B
s3B
sMB
sMB
s�B
shB
shB
s�B
s�B
s�B
s�B
tnB
tnB
tnB
t9B
t�B
t�B
u?B
u�B
u�B
vB
u�B
v+B
v`B
vzB
vzB
v�B
vzB
v�B
v�B
w2B
wLB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
x8B
xRB
x�B
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
zB
z�B
{JB
{�B
|B
|PB
|6B
|PB
|6B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
|�B
}<B
}�B
}�B
}�B
}�B
~(B
~(B
~wB
~�B
~�B
~�B
~�B
cB
cB
}B
�B
�B
�B
�B
� B
� B
� B
�4B
�4B
�OB
�OB
�OB
�iB
�iB
�iB
�iB
��B
��B
��B
��B
�oB
��B
��B
�'B
�AB
�uB
��B
��B
��B
��B
��B
��B
�B
�GB
�-B
�-B
��B
��B
�-B
�{B
��B
��B
��B
�B
��33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<(�U<]/<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.02(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202001200052392020012000523920200120005239202207271134182022072711341820220727113418202207271536432022072715364320220727153643  JA  ARFMdecpA30a                                                                20200109093747  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200109093840  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200109093841  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200109093841  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200109093841  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200109093842  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200109093842                      G�O�G�O�G�O�                JA  ARUP                                                                        20200109095457                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200110000000  CF  PSAL_ADJUSTED_QC@*=qD���G�O�                JM  ARCAJMQC2.0                                                                 20200119155239  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200119155239  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023418  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063643  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091504                      G�O�G�O�G�O�                