CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-11-29T09:38:43Z creation;2019-11-29T09:38:45Z conversion to V3.1;2022-08-02T05:11:45Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20191129093843  20220818091504  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_030                    2C  D   APEX                            8420                            2.11.2                          846 @��S� 1   @���[ @.�������c_/�V��1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @&ff@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�33A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BZ��B`  Bf  Bp  Bw��B�  B�  B�ffB���B�  B�  B�  B�  B�33B���B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
L�C��C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(�fD)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�3D�C3D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @(��@}p�@��A ��A Q�A?�A_�A�=qA��
A�  A�  A��
AυA��
A��
B   B
=B��B�B {B(  B/�B8
=B@�BH(�BP{BZ��B`33Bf�Bo�HBw�RB�  B�  B�ffB���B��B�  B���B�  B�G�B��RB��
B���B��fB��B��B��B�  B�B�
=B�B�B�33B��fB��B��B���B�  B�B���B���B���B���B��C�qC�C  C  C
EC�\C��C��C�qC��C��C�qC��C�qC�C   C"  C#��C%��C(  C)�RC+��C.  C/��C1��C3�qC5�qC7�RC9��C;�qC=�qC?��CA��CDCF�CG��CJCL  CM��CP�CR
=CS�qCU�qCX  CZC\�C^  C`  Cb  Cc��Ce��Ch  Cj�Cl�Cn�CpCr  Cs��Cv�Cx�Cz  C{��C}�RC�  C��C�HC���C��C�  C���C�  C�HC�  C��)C���C��C��C�HC�HC�  C��C��C�C��C���C��qC���C��)C���C��C�  C���C�HC�  C�HC�  C�  C��qC��)C��qC���C��qC���C��C��C��C��C�HC�HC�HC�  C���C��qC���C�HC�HC���C�  C��C��C��qC�  C��C��C�  C���C�C��C��C�HC�  C�  C���C��qC��qC�HC��C�HC�HC��C�  C���C�  C��)C�  C��C�  C��C�HC��C��C�HC�  C���C�  C��C�HC���C�  C��C�HC�HC��C�  C��qC�HC��qC���C�  C�HC��C�C��C���C�  C�  C��C�fC��C�
=C��C��C�HC�  C�  C���C���C��qC�  C��C��D �D ��D�D��D�\D~D �D��D  D��DHD\D��D��D�D�HD �D� D	�D	� D
  D
�HD  D��D  D~D��D��D �D�HD  D\D  D��D �D��DHD� D  D��D�D\D��D|�D�D\D �D� D�\D� D�qD~DHD��D�D��D�qD\D  D��D  D~D�\D~�D�qD � D!  D!\D"HD"�HD#HD#��D$  D$�HD%  D%\D%�D&~�D' �D'� D( �D(�{D)3D)� D*  D*� D+  D+\D+��D,~�D,�\D-� D-�\D.� D/HD/\D/�\D0� D0�\D1� D2  D2~D2�D3\D3�\D4� D5  D5��D6HD6~�D7 �D7\D7�D8� D9HD9�HD9�\D:~D:�qD;~D;�\D<� D<��D=~�D=��D>� D>�\D?~�D?�D@\DAHDA��DA�DB}qDC  DC�HDC�\DD� DEHDE��DFHDF��DG �DG~�DHHDH�HDH��DI\DJ  DJ~DJ�\DK�HDL �DL� DM  DM� DNHDN� DN��DO��DPHDP~�DQ �DQ��DR �DR� DS  DS� DT  DT� DU  DU� DV �DV��DW  DW\DXHDX��DY �DY� DY�\DZ~�DZ��D[~�D[�\D\~�D\�\D]��D]�\D^~�D^�\D_��D_�\D`~�D`��Da\DbHDb� Dc  Dc�HDd  Dd� De�De��De��Df~�Dg  Dg\Dh  Dh�HDh��Di}qDi�\Dj�HDj��Dk|�Dk��Dl\Dl�Dm~Dm��Dn�HDo�Do��Dp  Dp�HDq  Dq\Dr  Dr� Ds �Ds\Ds��Dt� Du�Du��Dv�Dv��DwHDw��Dx �Dx~�Dx�\Dy�HDy�\Dz��D{�D{�HD{�\D|��D|��D}~D}��D~\DHD� D�\D�?
D��RD���D�  D�>�D�
D���D�  D�@RD��D��RD� �D�?�D�\D��\D�  D�@RD��D�� D� �D�@�D��RD��RD�  D�@ D�\D���D���D�AHD��RD��\D���D�?�D��RD���D���D�?
D�\D��\D� RD�@RD�\D��RD� RD�?
D�
D��\D��\D�>�D��D���D�HD�@�D��D�� D�  D�@�D��RD��
D�  D�@ D�
D���D� �D�@�D��D��RD��D�@ D�~�D�� D�  D�@RD���D��\D���D�@ D�
D�� D���D�?�D��D���D� RD�?�D�\D��\D���D�@�D�� D��
D� �D�@�D�\D���D�  D�>�D��D��RD���D�?\D�
D���D��D�?
D��RD���D� �D�@ D���D��RD� RD�@RD�� D���D� �D�@RD�
D�� D���D�?�D��D���D��
D�@RD���D��\D���D�@ D���D���D���D�@ D�� D��
D���D�@ D���D��RD� RD�@�D��RD���D��\D�?�D�� D��RD�  D�?\D��D��\D��
D�?\D��RD���D���D�@ D�
D��\D� �D�?�D�
D��
D�  D�AHD�� D���D���D�@RD���D���D� �D�@�D��D��\D���D�?\D�
D��
D���D�@ D�� D�� D��
D�?\D�\D���D�  D�?\D�
D��
D�  D�?�D�\D��\D� RD�@�D�\D��
D��\D�?�D���D�� D���D�@�D���D���D���D�@�D�� D��RD� �D�@ D�� D���D��
D�?\D��D���D� RD�?�D�
D���D���D�?�D��D���D� RD�?�D�
D�� D��D�AHD���D���D� RD�@ D��D���D���D�?�D��D��RD� �D�@�D��D��fD�  D�AHD���D��RD� RD�@�D��HD�� D��
D�@ D���D���D���D�?\D��RD���D��
D�?�D D���D���D�>fD�~�Dÿ\D�  D�@�DĀ DĿ\D� �D�@�Dŀ�D���D���D�?\Dƀ Dƾ�D��
D�?\D��D�� D� �D�@ D�\D��RD�  D�@ Dɀ�D��HD� �D�@�DʀRD�� D�  D�?
D�\D��RD� RD�@RD�\D̿�D� �D�@�D̀�DͿ�D�  D�@ D�
Dο\D�  D�@ D��D��HD� �D�?�D�\Dп�D�  D�?\D��D���D� RD�?�D�\Dҿ�D��\D�?\DӀRD�� D� RD�@ DԀ D���D� �D�A�DՁHDտ�D�  D�@�Dր�D���D� �D�AHD׀�D���D� �D�@ D�\Dؿ
D���D�?
D��D���D��D�AHDڀ�D��RD�  D�@ DۀRD��HD� �D�@RD܀RDܿ�D��\D�@�D݀RDݿ�D��\D�>�DހRD��RD� RD�@RD�~�D߿�D� RD�?�D�
D�� D��D�A�D��D���D�  D�?�D�
D���D� �D�@RD� D�� D� �D�?�D� D���D���D�@RD� D忮D���D�@RD��D濮D���D�@�D��D��RD� RD�@RD�\D�
D�  D�@ D�RD���D�  D�?
D�
D�� D�  D�@RD��D�
D� RD�@RD�\D�
D��
D�@ D큚D�� D��
D�?\D� D���D� RD�@ D� D�� D� �D�@ D�\D�\D�  D�@RD�RD�
D���D�?�D�RD��RD��\D�?�D�RD�� D��\D�>�D�\D��RD� RD�?�D�
D���D� �D�@�D���D���D��\D�>�D��D���D� �D�?\D�� D���D���D�?�D��D�� D� �D�@�D���D��RD� RD�@�D���D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A��jA���A��TA���A��,A�՛A��A���A���A��aA�ѷA�ѷA�� A��NA��vA��pA�̘A�ɆAݴ�AݴAݵAݵ�Aݲ�AݲaAݳ�A�s�A�J#A�2�A�ʌAı�A½qA��CA�n�A���A��A��EA��GA��#A�(�A���A��EA��JA��A���A���A�YKA�n/A�yrA���A��NA�VmA�v�A�b�A�3�A��A��A�یA�`vA���A�A�W�A��RA�6A{�AwVmAr��Am��Ag�A_�0AW�yAV��AV�fAU��AP�bAM�"AL��AJ�AF��AEg�AD!AB�YA@4�A>g�A=0�A:ffA9�A8+�A5�CA55?A5�A4��A4B[A3�A2�A0*0A-�A,�zA,�\A+f�A+MA+�jA+�sA,_A,��A,��A,��A,�CA,K�A+qA):�A'��A'b�A&l"A$�dA$ �A#&�A"aA!l"A MA�.A�eAK^A?}A��A�HA \A4A��A�A�A�fAs�AFtA��A��Aw�AMA�A��A�A�AiDA�A�AA�<A��A�\Ai�AOA8�A��A��ArGA_�AxAp;A"hA�A��AA�A�jA�A�MAe,A;�A�A��A�A�A
MA	S&A�A֡A�A��A�'AJ#A�yA�A\�A�AZA�2A_pA��A&�A��A��A|�A �A ��A j�A 1@�*0@�  @�(@�I�@�	�@�x@�&@��H@�,=@��Q@���@���@��9@�T�@���@���@�_p@�@�5�@�Y@���@��X@�e�@��@�@�#:@ﰊ@�Mj@��,@�@��|@�z�@�Ft@�~@��`@��U@��@鯸@�F@�_@�S�@��@�~�@�L0@�($@�j�@�e�@�H@�a@�q@�  @Ṍ@�J�@�ߤ@�GE@�,�@ގ�@�:*@�0U@��@��}@�/@��]@ܘ_@�Xy@��@۠�@��@�C-@���@��@ؚ�@��@���@�m]@�+@�?�@Պ�@� \@Ԗ�@�{@�v`@���@Ҙ_@�E�@��@ѐ�@�?}@��5@О�@�7�@��j@���@�Ɇ@�҉@�֡@��`@���@�)�@͏�@̹$@�@�@���@˄M@�6z@�֡@�@�U�@ȶ�@�h�@�7�@���@�g�@�Z�@�@O@�C�@��@Ƶ@�M@�ԕ@Ń{@�o@�tT@� �@��@�~�@���@�@��@�RT@���@��.@�(�@�˒@�m]@���@�9X@��@�g�@��@�� @�?�@��@��@��d@��@��@��@���@��u@�	�@���@�/�@��@��D@�8�@�e@��
@�l�@��@�u%@�D�@��@��>@��H@�,�@��4@���@�M�@���@��@�/@���@���@�j@���@�9�@��@�c @�@���@�O�@��@��\@�u�@�l"@�A�@���@�Q�@��@���@��b@�V@��@��@�Q�@��@��P@�֡@��F@�n�@�C�@��@��@��3@��M@��@��I@�p;@�/�@��@�@���@�s�@�{J@��@��@��+@��@�O@���@�?�@���@���@�a@�;d@��H@���@�ff@���@���@�}�@�)_@��@�Ĝ@��u@�}V@�5?@���@�?}@��@�z�@���@�Ov@��&@�Q�@���@��D@�v�@�[�@�;�@�)�@��;@�w2@�@@���@��o@�K^@�%�@��W@���@���@��h@�N<@�*0@�V@��O@�<�@��@���@���@�t�@��@��M@���@��r@�D�@��@��@�b�@�>�@�.I@��@�ی@�i�@�!�@��@��n@��:@��{@�v`@�/@��]@��@�V@��@���@��6@�}�@�E9@�(@�ی@��@�s�@��@�b�@�C�@� i@��c@���@��@���@���@��L@��@��.@�J�@��@��@��S@�T�@��@��9@��@�z�@�L0@��@��@�m]@�L�@��@��@���@�d�@�L0@��.@��k@�X�@��@�ی@���@��@�|�@�`B@�S�@�=�@�(@���@��L@�d�@�{@�˒@��V@�x@�`B@��@���@���@�K^@��@�� @��)@�ݘ@�9�@��@���@��x@�s�@�V�@�1�@��@��q@��S@�T�@�+@��@�	l@�͟@�c @��@~�<@~:*@~	@}Vm@|C-@{خ@{��@{�a@{;d@z��@z&�@y�X@y��@yB�@x?�@we�@v�8@v�+@u�@uDg@u�@t��@tC-@sy�@s�@r��@r~�@rOv@r!�@q�z@qs�@q�@pu�@p2�@o�@o��@ob�@o�@n�8@n҉@n�6@ns�@n �@m��@mo @m0�@l�f@l��@lS�@k��@k@j�@ic@h��@hV�@g�&@g�4@f�@f.�@eϫ@e	l@d�D@dx@c�q@c�f@ce�@cC@b�H@bV@b �@a^�@`?�@_�g@_�4@^�@^��@^h
@]�@]+@\�u@\I�@[��@[8@Z�F@Z_�@Z:*@Z�@Y�3@Yu�@Ye,@Y+�@X�)@X�@Xc�@Xx@X�@W�@W��@W_p@V�'@VQ@U��@U8�@U�@T�4@T(�@S�@@SS@R��@RH�@R&�@Q�#@Q�~@Q\�@Q!�@P��@P�.@Px@O�6@O��@O{J@O>�@N�y@N�@NB[@N=q@M��@Mu�@M2a@L�@L�j@L��@L��@LN�@L-�@K�W@Kx@KU�@KY@J��@J@�@J�@I��@Ihs@H�P@H��@H~@G�$@Gy�@GK�@GS@F��@F@�@E��@E�@Em]@E<6@E�@D�|@D��@D?�@Cݘ@C�f@CP�@Bȴ@B��@B:*@B@A�@A�t@Aq@@�9@@�D@@Xy@@%�@@@@�@?�@?�g@?��@?v`@?K�@?(@>�8@>�2@>�@>�@>�X@>��@>C�@>4@=�@=|@<�5@<Ɇ@<��@<K^@<b@;��@;S�@:�@:��@:Ov@9��@9s�@9A @8��@8��@8�o@8 �@7�
@7�@7o�@7,�@7�@6�8@6�y@6�@6��@6Z�@6@5�j@5��@5��@5p�@55�@5�@5�@4֡@4oi@4"h@3�@3\)@34�@3Y@2ߤ@2� @2�@1�D@1��@1�@1�h@1��@1X@1�@0�O@0l"@0"h@/�W@/�;@/�@/��@/RT@.��@.�<@.��@.u%@.u@-�@-�@-4@-%F@-�@,�P@,�[@,�_@,-�@+˒@+�V@+@O@*�@*��@*;�@)�@)��@)L�@)-w@(�P@(�_@(|�@(`�@(U2@(I�@(!@'��@'�*@'t�@'>�@'�@&�2@&�@&�@%�@%�^@%zx@%#�@$��@$�9@$w�@$~@$  @#�@#��@#�V@#1�@"�8@"�X@"�A@"M�@"�@!@!x�@!L�@!�@ �K@ ��@ ��@ ~(@ r�@ j@ e�@ V�@��@&@�@�m@��@��@��@O@��@S&@2a@-w@q@��@�[@�@Q�@	�@�@��@��@,�@�@�b@q�@4@��@�@��@L�@�@@�f@�@�@�4@�@6@�@1@��@�@��@@O@ں@�<@��@?@$�@�@�#@��@�@8�@��@��@��@c�@1'@"h@@  @�W@ݘ@��@�V@J#@�"@͟@�x@�r@z@kQ@J�@)�@�o@�3@�@��@��@T�@q@�@�@�?@�@h�@H@%�@ �@� @o�@o�@qv@b�@C�@!-@
=@�M@�m@�+@=q@J@_@�Z@�N@�=@o @G�@7L@-w11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A��jA���A��TA���A��,A�՛A��A���A���A��aA�ѷA�ѷA�� A��NA��vA��pA�̘A�ɆAݴ�AݴAݵAݵ�Aݲ�AݲaAݳ�A�s�A�J#A�2�A�ʌAı�A½qA��CA�n�A���A��A��EA��GA��#A�(�A���A��EA��JA��A���A���A�YKA�n/A�yrA���A��NA�VmA�v�A�b�A�3�A��A��A�یA�`vA���A�A�W�A��RA�6A{�AwVmAr��Am��Ag�A_�0AW�yAV��AV�fAU��AP�bAM�"AL��AJ�AF��AEg�AD!AB�YA@4�A>g�A=0�A:ffA9�A8+�A5�CA55?A5�A4��A4B[A3�A2�A0*0A-�A,�zA,�\A+f�A+MA+�jA+�sA,_A,��A,��A,��A,�CA,K�A+qA):�A'��A'b�A&l"A$�dA$ �A#&�A"aA!l"A MA�.A�eAK^A?}A��A�HA \A4A��A�A�A�fAs�AFtA��A��Aw�AMA�A��A�A�AiDA�A�AA�<A��A�\Ai�AOA8�A��A��ArGA_�AxAp;A"hA�A��AA�A�jA�A�MAe,A;�A�A��A�A�A
MA	S&A�A֡A�A��A�'AJ#A�yA�A\�A�AZA�2A_pA��A&�A��A��A|�A �A ��A j�A 1@�*0@�  @�(@�I�@�	�@�x@�&@��H@�,=@��Q@���@���@��9@�T�@���@���@�_p@�@�5�@�Y@���@��X@�e�@��@�@�#:@ﰊ@�Mj@��,@�@��|@�z�@�Ft@�~@��`@��U@��@鯸@�F@�_@�S�@��@�~�@�L0@�($@�j�@�e�@�H@�a@�q@�  @Ṍ@�J�@�ߤ@�GE@�,�@ގ�@�:*@�0U@��@��}@�/@��]@ܘ_@�Xy@��@۠�@��@�C-@���@��@ؚ�@��@���@�m]@�+@�?�@Պ�@� \@Ԗ�@�{@�v`@���@Ҙ_@�E�@��@ѐ�@�?}@��5@О�@�7�@��j@���@�Ɇ@�҉@�֡@��`@���@�)�@͏�@̹$@�@�@���@˄M@�6z@�֡@�@�U�@ȶ�@�h�@�7�@���@�g�@�Z�@�@O@�C�@��@Ƶ@�M@�ԕ@Ń{@�o@�tT@� �@��@�~�@���@�@��@�RT@���@��.@�(�@�˒@�m]@���@�9X@��@�g�@��@�� @�?�@��@��@��d@��@��@��@���@��u@�	�@���@�/�@��@��D@�8�@�e@��
@�l�@��@�u%@�D�@��@��>@��H@�,�@��4@���@�M�@���@��@�/@���@���@�j@���@�9�@��@�c @�@���@�O�@��@��\@�u�@�l"@�A�@���@�Q�@��@���@��b@�V@��@��@�Q�@��@��P@�֡@��F@�n�@�C�@��@��@��3@��M@��@��I@�p;@�/�@��@�@���@�s�@�{J@��@��@��+@��@�O@���@�?�@���@���@�a@�;d@��H@���@�ff@���@���@�}�@�)_@��@�Ĝ@��u@�}V@�5?@���@�?}@��@�z�@���@�Ov@��&@�Q�@���@��D@�v�@�[�@�;�@�)�@��;@�w2@�@@���@��o@�K^@�%�@��W@���@���@��h@�N<@�*0@�V@��O@�<�@��@���@���@�t�@��@��M@���@��r@�D�@��@��@�b�@�>�@�.I@��@�ی@�i�@�!�@��@��n@��:@��{@�v`@�/@��]@��@�V@��@���@��6@�}�@�E9@�(@�ی@��@�s�@��@�b�@�C�@� i@��c@���@��@���@���@��L@��@��.@�J�@��@��@��S@�T�@��@��9@��@�z�@�L0@��@��@�m]@�L�@��@��@���@�d�@�L0@��.@��k@�X�@��@�ی@���@��@�|�@�`B@�S�@�=�@�(@���@��L@�d�@�{@�˒@��V@�x@�`B@��@���@���@�K^@��@�� @��)@�ݘ@�9�@��@���@��x@�s�@�V�@�1�@��@��q@��S@�T�@�+@��@�	l@�͟@�c @��@~�<@~:*@~	@}Vm@|C-@{خ@{��@{�a@{;d@z��@z&�@y�X@y��@yB�@x?�@we�@v�8@v�+@u�@uDg@u�@t��@tC-@sy�@s�@r��@r~�@rOv@r!�@q�z@qs�@q�@pu�@p2�@o�@o��@ob�@o�@n�8@n҉@n�6@ns�@n �@m��@mo @m0�@l�f@l��@lS�@k��@k@j�@ic@h��@hV�@g�&@g�4@f�@f.�@eϫ@e	l@d�D@dx@c�q@c�f@ce�@cC@b�H@bV@b �@a^�@`?�@_�g@_�4@^�@^��@^h
@]�@]+@\�u@\I�@[��@[8@Z�F@Z_�@Z:*@Z�@Y�3@Yu�@Ye,@Y+�@X�)@X�@Xc�@Xx@X�@W�@W��@W_p@V�'@VQ@U��@U8�@U�@T�4@T(�@S�@@SS@R��@RH�@R&�@Q�#@Q�~@Q\�@Q!�@P��@P�.@Px@O�6@O��@O{J@O>�@N�y@N�@NB[@N=q@M��@Mu�@M2a@L�@L�j@L��@L��@LN�@L-�@K�W@Kx@KU�@KY@J��@J@�@J�@I��@Ihs@H�P@H��@H~@G�$@Gy�@GK�@GS@F��@F@�@E��@E�@Em]@E<6@E�@D�|@D��@D?�@Cݘ@C�f@CP�@Bȴ@B��@B:*@B@A�@A�t@Aq@@�9@@�D@@Xy@@%�@@@@�@?�@?�g@?��@?v`@?K�@?(@>�8@>�2@>�@>�@>�X@>��@>C�@>4@=�@=|@<�5@<Ɇ@<��@<K^@<b@;��@;S�@:�@:��@:Ov@9��@9s�@9A @8��@8��@8�o@8 �@7�
@7�@7o�@7,�@7�@6�8@6�y@6�@6��@6Z�@6@5�j@5��@5��@5p�@55�@5�@5�@4֡@4oi@4"h@3�@3\)@34�@3Y@2ߤ@2� @2�@1�D@1��@1�@1�h@1��@1X@1�@0�O@0l"@0"h@/�W@/�;@/�@/��@/RT@.��@.�<@.��@.u%@.u@-�@-�@-4@-%F@-�@,�P@,�[@,�_@,-�@+˒@+�V@+@O@*�@*��@*;�@)�@)��@)L�@)-w@(�P@(�_@(|�@(`�@(U2@(I�@(!@'��@'�*@'t�@'>�@'�@&�2@&�@&�@%�@%�^@%zx@%#�@$��@$�9@$w�@$~@$  @#�@#��@#�V@#1�@"�8@"�X@"�A@"M�@"�@!@!x�@!L�@!�@ �K@ ��@ ��@ ~(@ r�@ j@ e�@ V�@��@&@�@�m@��@��@��@O@��@S&@2a@-w@q@��@�[@�@Q�@	�@�@��@��@,�@�@�b@q�@4@��@�@��@L�@�@@�f@�@�@�4@�@6@�@1@��@�@��@@O@ں@�<@��@?@$�@�@�#@��@�@8�@��@��@��@c�@1'@"h@@  @�W@ݘ@��@�V@J#@�"@͟@�x@�r@z@kQ@J�@)�@�o@�3@�@��@��@T�@q@�@�@�?@�@h�@H@%�@ �@� @o�@o�@qv@b�@C�@!-@
=@�M@�m@�+@=q@J@_@�Z@�N@�=@o @G�@7L@-w11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B�AB��B�'B�BªB��B��B�GB�-B�-BðB��BĶB�mB�B�tB��B�zB�B��B��B��B��B�B�lB�B>�B	s�B	ٴB	�B
�B
B
PB
V�B
FB
W�B
a-B
\xB
h�B
gmB
e�B
Z�B
RoB
N�B
UMB
kkB
T�B
D�B
;B
>B
M�B
72B
;0B
e�B
Q�B
I�B
G_B
?B
/�B
�B	�@B	��B	�B	��B	��B	��B	u%B	P�B	(�B	�B	3B	�B��B�
B�CB�,B�xB��B�DB��B��B�B�	B�B�xB�8B�$BƎBیB�B	B	+B	7B	#B	}B��B��B��B�FB	�B	*�B	F�B	O�B	c�B	gRB	h�B	oB	v`B	y�B	��B	�[B	��B	��B	��B	�B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�bB	��B	�_B	��B	�AB	�B	��B	�fB	�LB	��B	��B	�wB	�B	�'B	��B	�B	��B	�'B	��B	�oB	�B	�-B	�9B	ŢB	ňB	�B	�B	�zB	�+B	��B	ȀB	�EB	��B	��B	��B	��B	��B	ƎB	ƨB	�%B	�B	�B	�#B	��B	� B	�B	�6B	�)B	��B	�B	�dB	�JB	�(B	�bB	�4B	��B	��B	ЗB	��B	ѷB	�uB	�[B	�uB	ӏB	ӏB	�aB	ԯB	ԯB	�FB	յB	��B	��B	�B	֡B	ևB	ևB	��B	��B	�B	��B	�sB	��B	�?B	��B	ּB	�SB	��B	ՁB	ՁB	�sB	ևB	��B	��B	�$B	��B	��B	��B	��B	��B	�
B	��B	רB	�EB	�B	��B	��B	�QB	�QB	�7B	�]B	��B	�B	ٚB	ٴB	�1B	�B	��B	ؓB	ؓB	��B	�B	�B	��B	��B	��B	�B	�B	ٚB	ٴB	��B	ٚB	ٚB	�eB	ٴB	�	B	�WB	�)B	�xB	��B	�WB	��B	�CB	�]B	�xB	ܒB	�/B	ݘB	ݲB	��B	�5B	�jB	�;B	�B	��B	�B	��B	�B	�B	��B	�yB	��B	�"B	�=B	�B	�]B	�CB	�wB	�wB	��B	��B	�OB	�B	�oB	�oB	��B	�B	�B	�[B	�[B	�'B	�vB	�[B	��B	�aB	�B	�aB	��B	��B	�B	��B	��B	�B	�B	�`B	��B	�LB	��B	��B	�8B	��B	�>B	�rB	�*B	�DB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�PB	�B	��B	��B	�VB	��B	��B
 B
 �B
B
UB
;B
UB
 �B
;B
oB
UB
�B
�B
{B
GB
GB
B
�B
gB
B
SB
�B
�B
YB
_B
EB
EB
zB
	B
	�B
	�B
	�B

=B

=B

=B

�B
DB
DB
B
)B
)B
DB
DB
�B
^B
DB
B
xB
JB
JB
B
^B
^B
xB
�B
B
�B
"B
<B
�B
�B
�B
4B
4B
hB
�B
:B
B
&B
&B
�B
�B
�B
�B
�B
aB
{B
,B
aB
2B
gB
MB
�B
�B
�B
�B
2B
�B
�B
FB
�B
B
EB
kB
#B
qB
�B
qB
�B
qB
�B
)B
B
)B
CB
]B
CB
xB
xB
CB
B
B
)B
CB
xB
�B
�B
B
B
�B
B
jB
�B
�B
 �B
�B
�B
 \B
 �B
 �B
 �B
 �B
 �B
 �B
!-B
!bB
!bB
!HB
!�B
"�B
#�B
#�B
$&B
$�B
$tB
#�B
#B
!�B
!�B
"�B
"�B
"�B
$�B
&LB
&�B
&�B
&�B
'�B
'�B
'�B
(>B
'�B
(XB
(�B
(XB
(sB
)_B
)_B
)�B
)�B
)�B
)�B
)�B
)yB
)�B
)�B
*eB
+6B
+�B
,B
,B
,WB
,�B
-�B
.B
.}B
/ B
/B
/�B
/�B
0oB
0�B
1'B
1�B
1�B
1�B
2aB
2�B
2�B
2B
1�B
2aB
2�B
2�B
3�B
2�B
2�B
2-B
2GB
2GB
2�B
2�B
2�B
2�B
3MB
3MB
3hB
3hB
4TB
5B
4�B
49B
3MB
33B
4�B
4�B
4nB
4�B
5�B
5�B
6+B
6FB
5�B
5�B
5�B
7B
72B
72B
7�B
8RB
88B
8RB
8lB
8�B
8�B
8�B
9XB
9XB
9�B
9�B
9�B
9�B
:xB
:�B
:�B
:�B
;B
;dB
;�B
;�B
;�B
<B
<B
<jB
<�B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
>]B
>�B
?.B
?}B
?�B
@4B
@�B
A;B
AoB
A�B
BB
BAB
BuB
B�B
B�B
B�B
B�B
CaB
C{B
C�B
D�B
EB
E9B
E�B
E�B
E�B
E�B
FYB
F�B
F�B
G+B
GzB
G�B
H1B
HfB
H�B
H�B
H�B
H�B
IB
IRB
IRB
IlB
IlB
IRB
IRB
IlB
I�B
I�B
J	B
J�B
J�B
J�B
J�B
KDB
K�B
LJB
L�B
L�B
L�B
L�B
MB
MB
MB
MB
MjB
M�B
M�B
M�B
NB
N"B
NVB
N�B
N�B
N�B
N�B
OB
O(B
OBB
O\B
O\B
OvB
O�B
O�B
O�B
P.B
PB
PHB
P�B
Q B
P�B
Q4B
Q�B
Q�B
Q�B
R B
RTB
RTB
RTB
RTB
R�B
R�B
R�B
R�B
S&B
S@B
S[B
S@B
SuB
S�B
TB
TB
TFB
T�B
T�B
UB
U2B
U2B
U2B
U�B
U�B
U�B
U�B
VSB
VSB
VmB
V�B
V�B
V�B
V�B
W$B
W$B
W$B
W$B
W$B
W$B
W$B
W$B
W�B
WsB
WsB
W�B
X_B
XEB
XyB
X�B
YB
YB
YB
Y�B
ZB
ZB
ZQB
ZkB
Z�B
Z�B
Z�B
Z�B
[�B
[�B
[�B
\CB
\xB
\�B
\�B
\�B
\�B
\�B
]B
]IB
]dB
]dB
]�B
]�B
]�B
]�B
]�B
^B
^jB
^�B
^�B
_VB
_pB
_pB
_�B
_�B
`B
`'B
`\B
`vB
`vB
`vB
`�B
`�B
abB
a|B
a�B
a�B
a�B
bB
a�B
a�B
bNB
bNB
bNB
b�B
b�B
b�B
c B
cTB
cTB
cnB
cnB
c�B
c�B
d@B
dtB
dtB
d�B
d�B
eB
e�B
e�B
f2B
fLB
ffB
f�B
f�B
gB
gB
gB
gB
gB
g�B
g�B
g�B
h
B
h$B
h$B
h�B
iB
i*B
i*B
iyB
i�B
i�B
jB
j0B
jB
j�B
j�B
j�B
j�B
k6B
kQB
kkB
k�B
k�B
lB
l=B
l�B
l�B
l�B
mB
mCB
mCB
mwB
m]B
m]B
mCB
mCB
m�B
ncB
n}B
n�B
n�B
n�B
n}B
o5B
oOB
oOB
oOB
oOB
oiB
o�B
o�B
o�B
o�B
p;B
p;B
poB
p�B
p�B
q'B
q'B
q[B
q�B
r-B
raB
r�B
s3B
sMB
shB
s�B
s�B
s�B
s�B
s�B
t9B
t9B
tTB
tTB
tTB
tnB
t�B
u?B
u?B
utB
u�B
u�B
vB
vB
v+B
vFB
vzB
wB
wB
wB
wLB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
xlB
x�B
x�B
y	B
y	B
y	B
y$B
yXB
y�B
y�B
y�B
y�B
y�B
zB
z^B
z�B
z�B
z�B
{B
{B
{�B
{�B
{�B
{�B
|6B
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
~B
~(B
~(B
~]B
~�B
~�B
~�B
~�B
~]11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B� B�AB��B�'B�BªB��B��B�GB�-B�-BðB��BĶB�mB�B�tB�BǔB�B��B��B�B�RBʦB�(B$�B[qB	|PB	��B	��B

#B
#�B
V�B
e�B
KxB
[�B
d�B
b4B
m�B
j�B
j�B
_�B
VSB
S&B
[	B
o�B
YeB
H�B
>]B
B�B
S�B
=�B
>�B
h
B
R�B
J�B
IB
B[B
5�B
�B	�B	� B	�B	�B	�B	��B	}VB	[qB	0!B	
�B	9B	MB	;B�6BޞB�YBϑB��B�6B��B�^B�lB�0B�.B�6B��B��B�EB��B�UB	�B	1B	�B	�B	�B��B�fB�8B�zB	_B	*�B	FtB	OBB	cnB	gmB	h�B	pB	xB	|�B	�uB	�-B	�KB	��B	��B	�MB	�B	�2B	�mB	��B	��B	��B	�
B	��B	�B	�qB	��B	��B	�-B	��B	��B	��B	��B	�"B	��B	�nB	�tB	�B	�RB	��B	�XB	�.B	��B	��B	��B	��B	�uB	�uB	��B	��B	�uB	ÖB	ňB	��B	�%B	��B	ȀB	��B	ǔB	ǮB	�B	�zB	�B	�B	�B	�EB	�+B	�+B	ǔB	�_B	�B	�zB	�XB	�6B	ѝB	�uB	ΥB	��B	�JB	̈́B	�B	�6B	��B	�4B	�B	ѝB	�bB	��B	�TB	�oB	��B	��B	�B	�FB	�aB	��B	�2B	��B	ԕB	��B	�B	�9B	�9B	��B	��B	�?B	רB	�EB	ևB	�mB	�B	خB	�YB	��B	��B	֡B	�9B	�B	�mB	��B	��B	�?B	ևB	��B	�9B	�B	�mB	�SB	�9B	׍B	�EB	�B	��B	�B	�KB	�KB	چB	ڠB	��B	��B	�WB	چB	�7B	�B	�B	�eB	�1B	�B	�KB	�1B	�KB	�1B	�B	�1B	�eB	ٴB	ٴB	��B	�B	�B	�B	�7B	��B	�QB	�WB	��B	�xB	��B	�B	�B	�xB	ܒB	��B	��B	��B	ݘB	��B	��B	�B	ބB	޸B	ߊB	��B	�FB	��B	�RB	�B	�B	��B	�B	�B	�B	��B	�B	��B	��B	��B	��B	�B	�B	��B	�!B	�B	�B	��B	��B	�'B	�vB	�vB	�[B	��B	�B	�GB	�B	�|B	��B	��B	�3B	�B	�9B	�%B	�tB	�zB	��B	�B	��B	�B	�RB	��B	��B	�rB	��B	�^B	��B	��B	��B	��B	��B	��B	�B	�JB	�B	�jB	�jB	�6B	�6B	�PB	��B	�<B	�B	��B	��B	�(B	�.B
 OB
B
;B
�B
�B
�B
B
oB
�B
�B
'B
�B
�B
{B
�B
gB
MB
�B
SB
�B
�B
�B
�B
zB
_B
zB
�B
	lB
	�B
	�B

	B

rB

rB

rB

�B
xB
^B
)B
^B
DB
^B
xB
�B
xB
^B
^B
�B
dB
~B
JB
xB
xB
�B
�B
jB
B
VB
�B
�B
.B
HB
�B
hB
�B
�B
�B
&B
[B
uB
FB
�B
�B
�B
B
�B
�B
{B
�B
�B
�B
�B
�B
B
B
2B
gB
�B
�B
aB
�B
B
�B
�B
�B
�B
#B
�B
�B
�B
B
CB
CB
]B
]B
�B
�B
�B
�B
]B
)B
CB
xB
]B
�B
�B
/B
IB
dB
�B
5B
�B
�B
 'B
!B
 'B
 'B
 �B
 �B
 �B
 �B
 �B
 �B
!B
!bB
!�B
!|B
!|B
!�B
# B
#�B
$&B
$ZB
$�B
$�B
#�B
# B
"4B
!�B
"�B
"�B
"�B
$�B
&fB
'B
&�B
&�B
'�B
($B
(
B
(sB
(>B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
*0B
*B
)�B
)�B
)�B
)�B
*�B
+QB
+�B
,=B
,WB
,�B
-CB
-�B
./B
.�B
/5B
/OB
/�B
0B
0�B
0�B
1AB
1�B
1�B
2-B
2�B
2�B
2�B
2GB
1�B
2aB
2�B
3hB
3�B
2�B
2�B
2GB
2aB
2|B
2�B
3B
2�B
2�B
3�B
3hB
3�B
3�B
4�B
5ZB
5ZB
4nB
3MB
3�B
4�B
4�B
4�B
4�B
5�B
6+B
6`B
6zB
6B
6B
6+B
7fB
7LB
7fB
8B
8lB
8RB
8lB
8�B
8�B
8�B
9	B
9rB
9rB
9�B
9�B
:B
:B
:�B
:�B
:�B
:�B
;�B
;B
;�B
;�B
;�B
<B
<PB
<�B
<�B
<�B
<�B
<�B
=B
="B
=<B
=�B
>�B
?B
?cB
?�B
?�B
@�B
A B
AoB
A�B
A�B
BAB
B[B
B�B
B�B
B�B
B�B
CB
C�B
C�B
DgB
EB
ESB
EmB
E�B
E�B
FB
F%B
F�B
F�B
F�B
G_B
G�B
G�B
HfB
H�B
H�B
H�B
H�B
IB
I7B
IlB
IlB
I�B
IlB
IlB
IlB
I�B
I�B
J#B
J=B
J�B
J�B
J�B
KB
KxB
K�B
L~B
L�B
L�B
MB
MB
MB
M6B
M6B
MPB
M�B
M�B
M�B
M�B
N"B
N<B
NpB
N�B
N�B
N�B
N�B
O(B
OBB
O\B
OvB
OvB
O�B
O�B
O�B
PB
PHB
P.B
P}B
P�B
QB
Q B
QhB
Q�B
Q�B
R B
R:B
RoB
RoB
RoB
R�B
R�B
SB
R�B
SB
S[B
S@B
SuB
S[B
S�B
TB
TFB
T,B
T{B
T�B
T�B
U2B
UMB
UMB
U�B
U�B
U�B
VB
VB
VSB
V9B
V�B
V�B
V�B
V�B
V�B
W?B
W$B
W?B
W$B
W$B
W?B
W?B
W?B
W�B
W�B
W�B
XB
XyB
XyB
X�B
YB
Y1B
YKB
Y�B
ZB
ZB
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
[	B
[�B
[�B
[�B
\CB
\�B
\�B
\�B
\�B
\�B
\�B
]/B
]dB
]dB
]~B
]�B
]�B
]�B
^B
^B
^OB
^�B
^�B
_B
_pB
_�B
_�B
_�B
`B
`'B
`BB
`vB
`vB
`�B
`�B
`�B
a-B
a�B
a�B
a�B
a�B
a�B
bB
bB
bB
bhB
bhB
bhB
b�B
b�B
b�B
c:B
cTB
cTB
cnB
c�B
c�B
c�B
dZB
d�B
d�B
d�B
d�B
eFB
e�B
e�B
fLB
ffB
f�B
f�B
gB
gB
gB
gB
g8B
g8B
g�B
g�B
g�B
h$B
h$B
h>B
h�B
i*B
iDB
iDB
i�B
i�B
i�B
j0B
j0B
j�B
j�B
j�B
j�B
j�B
kQB
kkB
kkB
k�B
lB
l"B
lWB
l�B
l�B
mB
mCB
m]B
mCB
mwB
m]B
m]B
mCB
mwB
nB
ncB
n�B
n�B
n�B
n�B
n�B
oiB
oiB
oiB
oOB
oOB
o�B
o�B
o�B
o�B
pB
p;B
pUB
p�B
p�B
qB
q[B
qAB
q[B
q�B
rGB
r|B
r�B
sMB
sMB
sMB
s�B
s�B
s�B
s�B
tB
tTB
t9B
tTB
tnB
tnB
t�B
u%B
uZB
uZB
u�B
u�B
u�B
v+B
v+B
v+B
v`B
v�B
w2B
wB
w2B
wfB
w�B
w�B
w�B
w�B
w�B
w�B
xB
xB
x�B
x�B
x�B
y	B
y	B
y	B
y>B
yrB
y�B
zB
y�B
y�B
y�B
z*B
zxB
z�B
z�B
z�B
{0B
{�B
{�B
{�B
{�B
{�B
|PB
|�B
|�B
|jB
|�B
|�B
|�B
}B
}B
}<B
}�B
}�B
~B
~(B
~BB
~wB
~�B
~�B
~�B
~�B
~]33111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<P�<�1�<#�
<#�
<#�
<#�
<#�
<#�
<we�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<(�U<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201912100053592019121000535920191210005359202207271133462022072711334620220727113346202207271536152022072715361520220727153615  JA  ARFMdecpA30a                                                                20191129093721  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20191129093843  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20191129093844  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20191129093845  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20191129093845  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20191129093845  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20191129093845                      G�O�G�O�G�O�                JA  ARUP                                                                        20191129095457                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20191130000000  CF  PSAL_ADJUSTED_QC@(��@}p�G�O�                JM  ARCAJMQC2.0                                                                 20191209155359  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191209155359  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023346  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063615  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091504                      G�O�G�O�G�O�                