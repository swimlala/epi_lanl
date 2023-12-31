CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-04-18T03:37:48Z creation;2020-04-18T03:37:50Z conversion to V3.1;2022-08-02T05:11:08Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200418033748  20220818091505  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               ,A   JA  A30_8420_044                    2C  D   APEX                            8420                            2.11.2                          846 @���l 1   @���I�@0{��a@�c?��rG1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@�  @�  A   A   A@  A`  A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�ffB�33Bҙ�B�  B�  B���B�  B�  B�  B�  B�  B�  B�33C   C�fC  C�fC  C
  C  C  C33C�fC33C�fC��C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C033C2�C3��C6  C8  C:  C<  C=�fC@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@~�R@�\)@�\)A z�A@Q�A_�
A�Q�A��
A���A�  A�(�A�{A�  A��
A��B(�B{B��B�B'�HB0
=B7��B?�BH  BP
=BX33B`\)Bg��Bp  Bx{B�
=B�  B�B�
=B�  B��B��B��B���B�  B�  B�  B���B���B�\B�{B�  B���B�u�B̀ B��BҔ{B�  B�
=B߸RB���B���B�  B��B�\B�
=B��C �C��C��C�C�C
  C��C
=C:�C�C&fC��C�{C��C�qC�RC   C"�C#�qC&  C(�C)�qC,�C.�C0.C2!HC3��C5�qC8C:  C;��C=�3C?�RCB  CC��CE�RCG��CJ�CK��CM��CO��CR�CT  CU��CX�CY�qC\C^�C`CbCd�Cf  Ch  Cj�Ck�qCm�qCp  Cq�RCt  Cv�CxCzC|  C}�qC�qC���C�  C�C��C���C���C��qC���C��C��C��C��C�HC�  C��qC�HC�HC�  C�  C���C�  C�HC��C�HC��)C�  C�C�HC���C��qC���C��)C��C�  C���C���C���C��)C��qC�HC��C�  C�HC��C�C��C�C��C��C�  C�HC���C��qC�  C�  C��C�  C�  C�  C�  C��C��C�HC���C���C�HC�  C���C�  C�HC�HC�HC�  C��C���C�  C��C�HC�  C��C��C�HC���C���C�  C�  C�  C�  C��C�  C���C���C�  C�  C�HC��C��C��C���C�HC�  C��qC�HC�  C�HC�C��C��C��C�  C���C���C��qC�  C��C�fC��C��qC�  C���C���C��qC��qC��C��C�HC���C���D ~�D �\D\D  D~�D�D\D�\D\D�\D� D��D~�DHD��DHD��D	HD	��D
  D
� D
�\D��D�D��D�D��D �D��D�\D~D  D~�D�D� D�D��D  D� D�qD~�D  D~�D�\D��D�\D\D  D�HD �D� D��D~�D�\D~D�\D� D��D� D  D~D��D|�D�qD \D �\D!}qD!�D"� D"�\D#� D$�D$��D%  D%�HD&3D&��D'�D'��D( �D(~D)  D)� D)�D*\D+HD+� D+��D,~D,�D-~�D-�qD.\D/HD/��D0 �D0�HD1HD1\D1�qD2\D3 �D3\D3�D4��D5  D5\D5�\D6~�D6�\D7� D8 �D8\D9�D9��D9�\D:\D:�\D;~D;�D<��D=�D=� D> �D>� D>��D?\D@  D@� DA�DA��DB �DB� DB�\DC~�DC�qDD\DE �DE� DF  DF��DGHDG~�DH  DH��DH�\DI~�DI�\DJ~DJ�qDK� DL �DL��DL�DM~�DM�\DN\DO  DO� DO��DP\DP�\DQ\DQ�\DR\DS �DS��DS�\DT\DU �DU��DU��DV~DV��DW~DW��DX\DY �DY��DY�\DZ~�DZ�\D[~�D[��D\\D\�D]\D^HD^\D^�D_��D` �D`� Da  Da� Db�Db��Db�\Dc�HDd  Dd~�DeHDe�HDf  Df~�Dg  Dg�HDg�\Dh~Dh�qDi\DjHDj\Dj�Dk~�Dl  Dl}qDl�qDm� Dm�\Dn~�DoHDo��DpHDp\Dq �Dq�HDr�Dr\Dr�Ds~�Dt  Dt�HDu �Du��Dv�Dv� Dv��Dw~�Dw��Dx\Dx��Dy\Dz �Dz�HD{�D{��D{��D|\D} �D}��D~�D~��DHD��D� �D�?�D�� D��RD�  D�@ D�� D���D���D�?�D�� D���D�  D�@�D��D���D� �D�@�D���D���D��\D�?
D�\D���D� RD�@RD��RD��RD�  D�?
D�
D��\D��\D�?�D�� D��RD��\D�?�D�� D��
D���D�@ D��D���D� �D�@ D�
D��\D�  D�?\D�� D���D� RD�@ D�� D�� D� RD�?�D���D���D� RD�?�D��D��
D��\D�@ D��RD��\D���D�?�D��D�� D� �D�?�D��RD���D��fD�?
D��RD��RD� �D�?�D�~fD��\D� �D�@�D��RD��RD���D�@ D�\D��
D���D�>�D��D���D� �D�A�D���D���D� �D�?�D�\D��\D�  D�?�D��D��\D���D�@ D�� D���D��\D�@RD��RD��
D�  D�@RD�
D���D��
D�@ D��HD��RD��
D�?\D��RD���D� �D�@ D�� D���D�  D�?�D��D��RD�  D�@ D��RD���D� RD�@RD��RD���D� RD�@RD���D���D�  D�?�D�\D��\D���D�@ D��RD�� D�  D�@RD���D��HD� �D�@�D���D��HD� �D�@ D��HD���D��\D�@RD���D���D� �D�AHD���D���D�  D�?\D�\D���D� �D�?�D��RD���D���D�@ D���D�� D�  D�@ D��RD���D�  D�?\D�� D��RD� �D�@ D�
D��RD� �D�@�D��HD��RD��\D�?�D��RD��RD� �D�@�D��D��
D���D�@ D��RD��HD� �D�@RD���D�� D���D�@ D�� D��\D��\D�?�D��D���D���D�@RD�\D��\D��
D�?\D�� D���D��\D�@ D��RD�� D�  D�@�D��D��\D� RD�@ D��D�� D��\D�?\D�\D���D� RD�@ D�� D���D� RD�?
D�� D���D� �D�@�D�� D���D� RD�@RD�\D¾fD���D�?�D�\D�� D� �D�@RDĀ D�� D� RD�AHDŀ�D��RD���D�?�D��D��RD� RD�@RD��Dǿ�D� �D�@RD��D���D� �D�@RDɀRD���D�  D�?�D��Dʿ�D� RD�?\D��D��HD�HD�@RD�\D̿\D� RD�@RD̀�D��HD� �D�@�D΀�D�� D��\D�@ DπRDϿ\D�  D�?�D�
D�� D�  D�@ Dр�D���D� �D�?�D�~�Dҿ�D��\D�?�DӀ DӾ�D���D�?\DԀ DԿ�D�  D�?
D��D��RD��\D�@RDր Dֿ�D���D�?
D׀ D�� D��\D�?
D�\Dؿ\D���D�@ Dـ�Dٿ�D��
D�@RDڀRDڿ\D���D�@RD��D۾�D��\D�@�D܀�D���D� RD�@ D݀ D��RD� RD�@RD�\D�� D�HD�@�D߀RD�� D�  D�?\D��D�� D�  D�@ D�~�DᾸD���D�@ D� D�� D�  D�?�D� D㿮D��\D�@RD䀤D��RD���D�?�D� D忮D���D�?�D��D���D��
D�?\D�RD�� D�  D�@RD�RD���D�HD�@�D� D�
D���D�@ DꀤD���D�  D�?�D��D뿮D�  D�@RD� D쿮D���D�@�D� D��\D� �D�@ D�\D���D� RD�@�D�HD���D� RD�@RD�~�D�
D� RD�@�D�
D�\D��D�@�D�RD�� D�  D�@RD�~�D�D� �D�@RD� D���D���D�>�D�\D��\D���D�@RD��D���D���D�@�D��RD��RD� �D�?�D�~�D�� D� �D�@ D�
D���D� �D�?�D�~�D��fD��D�?�D�xR1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�h�A�kQA�h�A�c�A�^�A�c�A�a|A�QA�^5A�U�A�QA�8�A�.�A�0!A�.�A�%FA��A�A�"A�A�~A�
rA��A��"A�  A�;A���A��VA���A��,A��;A��A��AϥFA��AΞÀ�A��Aʡ-A�[�A�	7A�{JA���A���A���AȦLAȑ4A�{�A�kA�֡A�a�A�N<A�JA���A���A���A��_A�OBA���A�<6A���A�'�A���A�ӏA���A�-CA�FtA��A���A�A��PA��	A�&�A��A��A��bA�d�A��A�LdA�0�A� �A���A���A��A}sAy�Av�Av/�At҉Aq��Ajx�Ag�OAf�NAe%�Ac<6A^p�AZ:*AVl�AQ��AP��AM��AMk�AMOAL�AK�~AJ��AH�NAG�HAF�LAB��A@��A?xA>��A>�zA>�A=QA=QA<��A;"�A:��A:A7�gA7)�A7B�A6�A6�2A6kQA5n�A2�gA1�}A1:*A0�KA0Q�A0-�A/�A.zxA+�<A)h�A'��A&=�A&!A&�A%��A%{A$��A#��A"��A!s�A ��A �Al�A�oA��AȴA�4AZ�A�4AA�A_A�AuA��A.�A{�A&�A6�AMjAiDA1�A��A4nAeA�)AU2A@A��Ap;A�A�KA�eA��A�A��A�nA��AL0A�MAU�A2aA�rA�A�A��A��A�oA�HA��A��AffA��AxlADgA��A��A�HAh�AB�A
��A
[WA
H�A
A
33A
VmA
0UA	��A	�rA	FA	-�A	�A	$�A	.IA	&A�"A�A_AϫAE�A,�A��AGEAU2AHA4nASA��A �dA =A  �@��V@�H�@��@�+�@�y>@��[@��4@�$t@��@�r�@�I�@�;d@���@��@�H�@���@���@�:*@��9@�@�x�@��'@�ϫ@�tT@�{@�֡@�?�@�S�@�!-@���@�^5@���@�@�0�@��@�6@�C�@���@�ϫ@�P�@�s�@勬@��@��H@�D@��T@�~@�[W@�rG@�c�@�l�@�c@�!-@�&�@�}@��&@�ff@�J@ށo@�o @�o�@�qv@�c@ݴ�@��>@�ϫ@݌~@܀�@��
@ۤ@@�y�@��K@�m�@���@ٚk@�<6@ؿ�@�-�@�'�@ֺ�@�K�@���@ҸR@�1@���@�C�@��U@�&�@ϓ@�@O@��@�a@�b�@�a�@��@��@΁o@�	�@���@�ϫ@͗$@͎�@͑h@�qv@�ߤ@��@�	l@ʓu@�YK@�E�@��T@�C�@ȉ�@�@� �@��@Ǯ�@�	l@��B@�C-@��o@��z@Ť@@�5�@���@�*�@��D@��@�\�@�/@���@�Ft@��g@�Y�@��"@���@�_�@��@��-@�8@���@�u@�K�@���@���@���@�V@��Q@�$@��@�H@���@�|�@�\�@���@� \@�|�@���@���@�@��)@���@��F@�A�@�F�@�YK@��j@��0@�q@��]@��$@�=q@��V@��7@��@�C�@���@�:*@���@�qv@�p�@�u�@�p�@�v`@�^�@�dZ@�@O@��@�l�@��@���@�$�@�!�@� �@��@�u�@�K�@�+@��@��r@�K^@�"h@��@�P�@�,�@�Y@��P@���@��@� �@�G@���@���@�J#@��@��"@��X@���@��}@���@�$t@���@���@��h@��r@��@��Z@��0@���@���@��'@��@���@��T@��q@�S�@�33@��@�p;@�$�@���@��P@��@��P@���@���@���@���@�9X@�b@��w@�zx@�'�@��	@���@��.@�V@��@���@��'@���@�s�@��@���@�g8@�D�@�"h@��Q@��t@���@��	@�v`@�RT@�*0@��@���@�v�@�M�@�$�@��@��"@���@��q@��@���@���@�l�@�)_@���@��_@�W�@�0U@��@���@��@�s�@��@�%@��@�l�@�5?@���@�c�@�q@��K@���@�w�@�V@�B[@��@��j@�;d@��5@��@��@�͟@���@�a|@�+k@�}�@�F�@�+�@��E@���@��z@��Y@�ff@���@���@�qv@�e,@��@���@��4@�?�@��@��@��k@��@���@���@���@�_@��H@�|@�C@�ں@�Ov@�b@�v`@�H�@�x@��D@��@��]@�ߤ@��@���@��A@�I�@�	�@���@���@�a@���@��m@��e@�6�@���@���@�5�@��@��u@�Z�@�)�@�  @$t@~ff@~�@}�=@|�@|Ft@{�@z�8@z� @z	@y|@x�@xM@w��@wC@v��@vߤ@v��@v��@wMj@wK�@v��@v�@v?@vJ@u�@uc@uB�@t��@te�@t*�@sݘ@sA�@r�@r��@r��@r�@q��@p��@p%�@o�Q@oA�@n�c@nH�@n_@m�#@m��@mrG@l�@l�@l��@l�Y@lXy@lI�@lM@ks@kO@k4�@k+@j��@j��@j3�@i�C@i[W@iB�@h�@h2�@g��@g�@fkQ@e��@ex�@e^�@e?}@e*0@d�5@d�Y@c�r@c�:@b�@b��@b@�@a�=@`��@`j@_��@^�@^�@^J@]�=@]@@\�@\��@\'R@[��@[$t@Z��@Z�@Y��@Y��@Yc�@Y2a@X�@W�@W9�@V�!@Vv�@U�)@Ua�@T��@T�u@Th�@S�a@R�@Ri�@R4@Q�T@Q@Qs�@Q�@P��@P	�@OF�@N��@NE�@N3�@N+k@N	@M��@M�@L�.@L�@K�[@K�q@Kl�@K4�@J�@I�z@I�"@Ia�@I�@H�@HZ@HD�@G�@G\)@G�@F�y@F��@F($@E@Ew2@D�@DD�@C�6@C��@Cj�@C i@B��@BTa@Bu@A�T@Ax�@A%F@@��@@�D@@Xy@@K^@@G@?Mj@>�s@>͟@>�L@>ff@>-@>�@=�Z@=�=@=o @<��@<��@<tT@<"h@;��@;E9@;1�@;)_@;o@:�y@:n�@:($@: �@9�@9�d@9�H@9��@9��@9m]@9	l@8�D@8�o@8|�@8�@7�@7�@7��@733@7�@7(@6�B@5��@5��@5�^@5��@5�n@5��@5 \@4��@4�@4�e@4��@4N�@4�@3~�@2�R@2@1ϫ@1p�@1A @0��@0��@0�v@0�@0M@0/�@/��@/��@/)_@/)_@/$t@/o@.�@.�M@.��@.�B@.��@.)�@-�>@-��@--w@-�@,�P@,�p@,9X@,!@+��@+�@+��@+�{@*�c@*ff@)��@)�@)�9@)��@)c�@)	l@(��@(l"@(-�@'�@'�w@'�f@'>�@&҉@&�h@&�F@&q�@&.�@%��@%�j@%�h@%o @%<6@%<6@% \@$��@$|�@$h�@$�@#�+@#��@#��@#��@#O@"��@"V@"?@!��@!e,@!��@!�S@!�M@!Dg@ �P@ �P@ Ĝ@ �o@ r�@ z�@ oi@ C-@ �@�@�0@��@�$@U�@(@��@��@��@Ov@!�@�Z@��@�@��@N<@�@;@�p@�D@,=@ƨ@�k@��@�	@�f@~�@o�@RT@+@�@҉@�!@\�@+k@�@�t@�@f�@/@�	@�/@�@9X@'R@�@�W@ƨ@��@��@x@Z�@8@o@�@�<@�\@z@s�@kQ@J�@.�@��@F@/@�@	l@�@��@�/@��@�.@�u@u�@_@7�@��@s@C�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�h�A�kQA�h�A�c�A�^�A�c�A�a|A�QA�^5A�U�A�QA�8�A�.�A�0!A�.�A�%FA��A�A�"A�A�~A�
rA��A��"A�  A�;A���A��VA���A��,A��;A��A��AϥFA��AΞÀ�A��Aʡ-A�[�A�	7A�{JA���A���A���AȦLAȑ4A�{�A�kA�֡A�a�A�N<A�JA���A���A���A��_A�OBA���A�<6A���A�'�A���A�ӏA���A�-CA�FtA��A���A�A��PA��	A�&�A��A��A��bA�d�A��A�LdA�0�A� �A���A���A��A}sAy�Av�Av/�At҉Aq��Ajx�Ag�OAf�NAe%�Ac<6A^p�AZ:*AVl�AQ��AP��AM��AMk�AMOAL�AK�~AJ��AH�NAG�HAF�LAB��A@��A?xA>��A>�zA>�A=QA=QA<��A;"�A:��A:A7�gA7)�A7B�A6�A6�2A6kQA5n�A2�gA1�}A1:*A0�KA0Q�A0-�A/�A.zxA+�<A)h�A'��A&=�A&!A&�A%��A%{A$��A#��A"��A!s�A ��A �Al�A�oA��AȴA�4AZ�A�4AA�A_A�AuA��A.�A{�A&�A6�AMjAiDA1�A��A4nAeA�)AU2A@A��Ap;A�A�KA�eA��A�A��A�nA��AL0A�MAU�A2aA�rA�A�A��A��A�oA�HA��A��AffA��AxlADgA��A��A�HAh�AB�A
��A
[WA
H�A
A
33A
VmA
0UA	��A	�rA	FA	-�A	�A	$�A	.IA	&A�"A�A_AϫAE�A,�A��AGEAU2AHA4nASA��A �dA =A  �@��V@�H�@��@�+�@�y>@��[@��4@�$t@��@�r�@�I�@�;d@���@��@�H�@���@���@�:*@��9@�@�x�@��'@�ϫ@�tT@�{@�֡@�?�@�S�@�!-@���@�^5@���@�@�0�@��@�6@�C�@���@�ϫ@�P�@�s�@勬@��@��H@�D@��T@�~@�[W@�rG@�c�@�l�@�c@�!-@�&�@�}@��&@�ff@�J@ށo@�o @�o�@�qv@�c@ݴ�@��>@�ϫ@݌~@܀�@��
@ۤ@@�y�@��K@�m�@���@ٚk@�<6@ؿ�@�-�@�'�@ֺ�@�K�@���@ҸR@�1@���@�C�@��U@�&�@ϓ@�@O@��@�a@�b�@�a�@��@��@΁o@�	�@���@�ϫ@͗$@͎�@͑h@�qv@�ߤ@��@�	l@ʓu@�YK@�E�@��T@�C�@ȉ�@�@� �@��@Ǯ�@�	l@��B@�C-@��o@��z@Ť@@�5�@���@�*�@��D@��@�\�@�/@���@�Ft@��g@�Y�@��"@���@�_�@��@��-@�8@���@�u@�K�@���@���@���@�V@��Q@�$@��@�H@���@�|�@�\�@���@� \@�|�@���@���@�@��)@���@��F@�A�@�F�@�YK@��j@��0@�q@��]@��$@�=q@��V@��7@��@�C�@���@�:*@���@�qv@�p�@�u�@�p�@�v`@�^�@�dZ@�@O@��@�l�@��@���@�$�@�!�@� �@��@�u�@�K�@�+@��@��r@�K^@�"h@��@�P�@�,�@�Y@��P@���@��@� �@�G@���@���@�J#@��@��"@��X@���@��}@���@�$t@���@���@��h@��r@��@��Z@��0@���@���@��'@��@���@��T@��q@�S�@�33@��@�p;@�$�@���@��P@��@��P@���@���@���@���@�9X@�b@��w@�zx@�'�@��	@���@��.@�V@��@���@��'@���@�s�@��@���@�g8@�D�@�"h@��Q@��t@���@��	@�v`@�RT@�*0@��@���@�v�@�M�@�$�@��@��"@���@��q@��@���@���@�l�@�)_@���@��_@�W�@�0U@��@���@��@�s�@��@�%@��@�l�@�5?@���@�c�@�q@��K@���@�w�@�V@�B[@��@��j@�;d@��5@��@��@�͟@���@�a|@�+k@�}�@�F�@�+�@��E@���@��z@��Y@�ff@���@���@�qv@�e,@��@���@��4@�?�@��@��@��k@��@���@���@���@�_@��H@�|@�C@�ں@�Ov@�b@�v`@�H�@�x@��D@��@��]@�ߤ@��@���@��A@�I�@�	�@���@���@�a@���@��m@��e@�6�@���@���@�5�@��@��u@�Z�@�)�@�  @$t@~ff@~�@}�=@|�@|Ft@{�@z�8@z� @z	@y|@x�@xM@w��@wC@v��@vߤ@v��@v��@wMj@wK�@v��@v�@v?@vJ@u�@uc@uB�@t��@te�@t*�@sݘ@sA�@r�@r��@r��@r�@q��@p��@p%�@o�Q@oA�@n�c@nH�@n_@m�#@m��@mrG@l�@l�@l��@l�Y@lXy@lI�@lM@ks@kO@k4�@k+@j��@j��@j3�@i�C@i[W@iB�@h�@h2�@g��@g�@fkQ@e��@ex�@e^�@e?}@e*0@d�5@d�Y@c�r@c�:@b�@b��@b@�@a�=@`��@`j@_��@^�@^�@^J@]�=@]@@\�@\��@\'R@[��@[$t@Z��@Z�@Y��@Y��@Yc�@Y2a@X�@W�@W9�@V�!@Vv�@U�)@Ua�@T��@T�u@Th�@S�a@R�@Ri�@R4@Q�T@Q@Qs�@Q�@P��@P	�@OF�@N��@NE�@N3�@N+k@N	@M��@M�@L�.@L�@K�[@K�q@Kl�@K4�@J�@I�z@I�"@Ia�@I�@H�@HZ@HD�@G�@G\)@G�@F�y@F��@F($@E@Ew2@D�@DD�@C�6@C��@Cj�@C i@B��@BTa@Bu@A�T@Ax�@A%F@@��@@�D@@Xy@@K^@@G@?Mj@>�s@>͟@>�L@>ff@>-@>�@=�Z@=�=@=o @<��@<��@<tT@<"h@;��@;E9@;1�@;)_@;o@:�y@:n�@:($@: �@9�@9�d@9�H@9��@9��@9m]@9	l@8�D@8�o@8|�@8�@7�@7�@7��@733@7�@7(@6�B@5��@5��@5�^@5��@5�n@5��@5 \@4��@4�@4�e@4��@4N�@4�@3~�@2�R@2@1ϫ@1p�@1A @0��@0��@0�v@0�@0M@0/�@/��@/��@/)_@/)_@/$t@/o@.�@.�M@.��@.�B@.��@.)�@-�>@-��@--w@-�@,�P@,�p@,9X@,!@+��@+�@+��@+�{@*�c@*ff@)��@)�@)�9@)��@)c�@)	l@(��@(l"@(-�@'�@'�w@'�f@'>�@&҉@&�h@&�F@&q�@&.�@%��@%�j@%�h@%o @%<6@%<6@% \@$��@$|�@$h�@$�@#�+@#��@#��@#��@#O@"��@"V@"?@!��@!e,@!��@!�S@!�M@!Dg@ �P@ �P@ Ĝ@ �o@ r�@ z�@ oi@ C-@ �@�@�0@��@�$@U�@(@��@��@��@Ov@!�@�Z@��@�@��@N<@�@;@�p@�D@,=@ƨ@�k@��@�	@�f@~�@o�@RT@+@�@҉@�!@\�@+k@�@�t@�@f�@/@�	@�/@�@9X@'R@�@�W@ƨ@��@��@x@Z�@8@o@�@�<@�\@z@s�@kQ@J�@.�@��@F@/@�@	l@�@��@�/@��@�.@�u@u�@_@7�@��@s@C�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�zB��B��B��B��B��B�zB�EB�+B�_B��B�zB�EB��B�+B�+B��B��B��B��B��B�tB�YB��B��B��B�mB�SB��B��B�B��B�[B{BmwB_�BD�B$�BB[BB3B�AB�!B�MB�B�[B�'B��BD3BGB�wB	��B	�B	��B
FYB
�9B
οB
�&B
��B
�/B
�iB
��B
�B
�B
��B
��B
�`B
�EB
ۦB
��B
��B
�B
�B
�B
��B
K�B
�B

=B	��B	��B	�mB	�B	��B	��B	��B	��B	��B	��B	lWB	RB	J�B	J#B	T�B	WsB	E�B	7�B	(
B	�B	�B	B	5�B	i�B	��B	�}B	�5B	��B	��B	��B	��B
�B	��B
B
�B
!�B
$ZB
9$B
?HB
?�B
>�B
=VB
:*B
>�B
L�B
J	B
PbB
QhB
MB
<jB
2B
0!B
1'B
/5B
/ B
*0B
�B
	�B	�aB	�B	�B	�B	�B	�`B	�6B	��B
 �B	��B	��B	�B	��B
%B	�^B	�`B	��B	��B
;B	�.B	�B	�RB	��B	��B	�?B	�B	�B	�`B	�2B	�mB	�*B	�B	��B	�iB	��B	� B	�B	�oB	��B	�-B	�B	�OB	�5B	��B	�yB	�B	�B	�tB	�B	�6B
 �B
B

=B
�B
�B
 B

#B
&B
@B
�B
yB
�B
�B
 B
�B
B

XB
�B
�B
dB

	B
�B

�B
xB
�B
FB
�B
�B
QB
�B
B
YB
B
�B
CB
�B
zB
B	�B	�LB	��B	�B	�B	��B	��B	�?B	�ZB	�B	�B	�B	�;B	�B	�]B	�QB	��B	�B	�B	�B	�B	�kB	��B	�/B	�B	��B	�tB	�B	�zB	��B	�B	��B	�mB	�>B	�sB	�fB	�B	��B	�B	�hB	��B	�B	�B	��B	��B	�hB	�nB	��B	�@B	�B	�B	�&B	�B	߾B	��B	�`B	�B	�mB	��B	�B	�B	�WB	�B	�5B	�B	�oB	��B
�B
 4B	��B	��B	��B	�|B	�B	�B	�FB	��B	��B	��B	�B	��B	�9B	�MB	��B	�B	�B	�|B	��B	�vB	�'B	�B	�B	��B	�B	�LB	�tB	�nB	��B	�nB	�B	�B	��B	��B	�B	�wB	�B	�?B	��B	�2B	��B	�lB	�$B	�^B	��B	��B	��B	�DB	�B	��B	�B	�aB	�AB	��B	�UB	�B	�/B	�]B	�B	�B	�IB	�iB	�OB	��B	��B	��B	�B	�B	�B	��B	�'B	�'B	�vB	�'B	�AB	��B	�B	�B	��B	�B	�B	�vB	�B	��B	��B	��B	�B	�wB	�B	��B	�TB	��B	�B	�/B	��B	��B	�kB	�B	��B	�QB	�"B	�B	�B	��B	�B	�/B	�]B	�]B	��B	�}B	�IB	�/B	�B	�B	��B	�oB	��B	�B	�9B	�9B	�hB	�|B	��B	�B	�nB	�?B	�B	�zB	�zB	�fB	�RB	��B	��B	�HB	�}B
 4B
UB
B
AB
B
aB
�B
�B
�B
{B
B
uB
uB
uB
uB
AB
�B
�B
 �B
 �B
 �B
 B
UB
�B
B
�B
�B
AB
GB
�B
uB
AB
B
AB
�B
�B
�B
�B
	�B

�B
DB
6B
vB
�B
�B
�B
B
�B
�B
B
"B
�B
B
}B
}B
�B
�B
�B
�B
�B
hB
4B
�B
�B
�B
TB
FB
�B
�B
�B
2B
B
mB
�B
�B
�B
EB
�B
�B
�B
�B
�B
�B
B
�B
7B
7B
kB
QB
�B
�B
�B
xB
�B
�B
xB
xB
xB
dB
B
�B
�B
�B
�B
;B
�B
�B
 'B
!HB
!|B
!�B
"�B
"�B
#�B
#nB
#�B
#�B
$&B
#�B
#:B
#B
"�B
"�B
"�B
"�B
"�B
#nB
$�B
#�B
# B
#�B
$�B
$ZB
$@B
$@B
$&B
$�B
%FB
%FB
$�B
%zB
%`B
%�B
&fB
&fB
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'mB
($B
(�B
)B
(�B
)�B
(�B
'�B
'�B
(>B
.IB
.�B
.�B
.�B
.�B
/5B
0!B
0UB
0oB
1vB
1�B
1�B
1�B
1�B
1B
0�B
0�B
0�B
1[B
1�B
2aB
2|B
2�B
3MB
3hB
3�B
3�B
3�B
4B
4�B
4�B
4�B
4TB
4B
4�B
4�B
5%B
5tB
5�B
6FB
6�B
:*B
:�B
;B
;JB
;�B
;�B
;�B
;JB
;0B
;B
;dB
;B
;�B
<�B
=B
=B
<�B
=qB
=�B
>]B
?B
?�B
?�B
?HB
?�B
@B
@�B
@�B
@iB
@4B
@4B
@iB
@�B
@�B
@�B
A�B
B�B
B�B
B�B
B�B
B�B
BuB
B�B
CB
B�B
B�B
B�B
CaB
C�B
C�B
D3B
C�B
C�B
C�B
D�B
EB
ESB
E�B
FB
FtB
F�B
F�B
F�B
F�B
GEB
GEB
G�B
G+B
F�B
F�B
FtB
F�B
FtB
F�B
GB
F�B
F�B
GzB
G�B
G�B
G�B
GzB
G+B
F�B
G�B
HKB
H1B
HB
H�B
H�B
H�B
IB
H�B
I�B
J	B
J#B
I�B
J#B
J	B
J#B
JXB
J�B
K^B
K�B
LJB
LJB
LdB
LJB
L0B
LdB
L�B
M6B
M�B
NpB
O�B
O�B
O�B
O�B
N�B
N�B
OB
O\B
O�B
O�B
O�B
PB
P�B
Q4B
QB
Q4B
Q�B
Q�B
Q�B
R�B
R�B
SuB
SuB
SuB
S�B
TFB
T{B
T�B
T�B
UgB
U�B
U�B
V9B
VSB
VSB
V�B
WsB
W�B
W�B
W�B
X+B
X+B
X_B
X_B
X�B
X�B
Y�B
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[�B
[�B
\B
\]B
\�B
\�B
\�B
\xB
\CB
\)B
\�B
\�B
\�B
]/B
]IB
]/B
]�B
]�B
]�B
]dB
]�B
^OB
^5B
^B
^B
^B
^B
^�B
_B
_pB
_�B
`'B
`vB
`vB
`BB
`vB
`BB
`�B
`�B
`�B
a-B
a-B
aB
a|B
a�B
a�B
a�B
bB
b�B
bhB
bhB
b�B
b�B
b�B
b�B
b�B
cB
c:B
cnB
c�B
c�B
dB
dB
d&B
d�B
d�B
d�B
d�B
d�B
d�B
e,B
e�B
fB
fB
fB
f2B
ffB
ffB
f�B
gB
g8B
g�B
h
B
h$B
hsB
h�B
h�B
h�B
h�B
iB
i_B
i_B
i�B
i�B
jeB
jB
jB
j�B
j�B
kB
k6B
k6B
kkB
kkB
kkB
kQB
j�B
kB
k�B
k�B
lB
mB
mwB
m�B
mwB
m�B
nB
m�B
m�B
nB
n�B
o5B
oB
oOB
o�B
o�B
o�B
pB
o�B
oiB
oB
oOB
pB
p�B
poB
poB
poB
poB
poB
p�B
p�B
p�B
p�B
p�B
q[B
q�B
q�B
rB
r-B
rB
r-B
r-B
r-B
raB
r�B
r�B
r�B
s3B
sMB
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
t9B
tnB
t�B
t�B
uB
uB
utB
u�B
u�B
u�B
u�B
v+B
vzB
v�B
v�B
v�B
v�B
v�B
v�B
wfB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
x�B
x�B
x�B
x�B
x�B
y	B
y	B
y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�zB��B��B��B��B��B��B�EB�EB�zB��B��B�EB��B�EB�EB�B��B��B��B��B�tB�YB��B��B��B�mB�mB��B��B�'B��B��B|�Bo BbBG�B&�B�B,B�BB�B�UB�B��B��B�-B�BF�BQ4B	�B	��B	�FB	��B
K�B
��B
҉B
�$B
��B
�|B
�'B
��B
�AB
��B
�B
�B
�*B
��B
�-B
��B
�zB
�B
�B
�EB
��B
R B
YB
�B
 �B	��B	�/B	��B	��B	�,B	��B	�jB	�xB	��B	sMB	U2B	L0B	L�B	XEB	\�B	J�B	<�B	,�B	B	�B	�B	6zB	jeB	�GB	��B	��B	�]B	�)B	�B	�]B
-B	��B
gB
�B
"hB
$�B
:*B
@�B
@�B
@B
?}B
:�B
>�B
MB
JXB
QhB
S&B
O�B
=�B
2�B
0�B
1�B
/�B
0B
,qB
"�B
�B	�B	�DB	�B	��B	�B	�LB	�B	��B
AB
oB	��B	�B	�	B
�B	��B	�2B	�B	�*B
B	��B	��B	�lB	�B	�8B	�`B	�aB	�B	�`B	�2B	�mB	�B	��B	�vB	�B	�OB	�B	��B	��B	�B	�B	�B	�B	��B	�B	�B	��B	�|B	��B	��B	�jB
 �B
 �B

=B
&B
B
�B

#B
@B
[B
�B
B
�B
,B
�B
�B
�B

�B
<B
6B
6B

XB
�B
)B
^B
�B
�B
�B
�B
�B
�B
9B
YB
B
B
�B
,B
�B
�B	�"B	��B	�B	��B	�B	� B	��B	��B	�+B	��B	�TB	��B	��B	� B	��B	�"B	�KB	��B	��B	��B	��B	��B	��B	� B	�B	�FB	��B	�,B	��B	�,B	�ZB	�B	�B	��B	�B	�RB	�ZB	�tB	�B	�B	�bB	�B	��B	�B	�B	�B	�B	�B	�B	�FB	�FB	�B	�:B	�BB	�-B	��B	�`B	��B	�
B	�*B	�B	�B	�[B	�B	�B	�B	�B
AB
B	�>B	�lB	�tB	�B	�B	��B	�+B	��B	�B	�JB	��B	�fB	�nB	�B	�hB	�B	��B	��B	�B	��B	�B	�;B	�}B	��B	�B	��B	��B	��B	�ZB	��B	�B	�tB	�B	��B	�B	�wB	�B	�tB	�B	��B	�8B	��B	�>B	�xB	��B	��B	��B	��B	��B	�nB	�hB	�B	�vB	�'B	��B	�vB	�}B	�wB	�)B	�]B	�B	�B	�B	�!B	�B	�'B	�B	�[B	�vB	��B	�[B	�vB	�B	�vB	��B	�B	�hB	��B	�-B	��B	��B	��B	��B	�-B	�UB	�/B	�WB	��B	�;B	�3B	��B	��B	�B	�B	�)B	�WB	�"B	�B	�kB	��B	��B	�]B	�wB	�B	�)B	�}B	��B	�B	�IB	��B	�cB	�B	��B	�B	�;B	��B	�B	��B	�nB	�B	��B	��B	�B	�B	�nB	�?B	�B	��B	�zB	�fB	��B	�xB	�.B	��B	��B
 OB
oB
'B
�B
aB
�B
B
3B
�B
�B
aB
�B
�B
�B
�B
uB
'B
�B
B
 �B
 B
UB
oB
�B
AB
�B
�B
�B
�B
�B
�B
uB
AB
�B
�B
�B
�B
�B
	�B

�B
)B
6B
�B
.B
�B
�B
vB
(B
"B
VB
�B
�B
.B
�B
�B
�B
 B
�B
�B
�B
�B
hB
B
 B
 B
�B
{B
�B
�B
�B
�B
SB
�B

B
�B
�B
_B
�B
�B
�B
�B
B
B
1B
�B
QB
kB
�B
kB
�B
�B
�B
xB
�B
�B
�B
�B
�B
~B
5B
�B
�B
�B
�B
pB
 B
 'B
 \B
!|B
!�B
"4B
"�B
# B
#�B
#�B
#�B
#�B
$ZB
$B
#�B
#TB
"�B
"�B
#B
"�B
# B
#�B
%B
$&B
#:B
#�B
$�B
$tB
$ZB
$tB
$tB
$�B
%`B
%`B
%FB
%�B
%�B
&B
&�B
&�B
&�B
'B
'�B
'�B
'�B
'�B
(>B
'�B
'�B
(sB
(�B
)_B
)DB
*KB
(�B
'�B
'RB
'�B
.IB
/ B
.�B
/B
/ B
/iB
0;B
0�B
0�B
1�B
1�B
1�B
1�B
1�B
1[B
1'B
1B
1B
1�B
2-B
2�B
2�B
3MB
3�B
3�B
3�B
3�B
4B
4TB
4�B
4�B
4�B
4�B
4TB
4�B
4�B
5?B
5�B
5�B
6FB
6�B
:B
:�B
;JB
;dB
;�B
;�B
;�B
;dB
;dB
;JB
;B
;�B
<B
<�B
="B
="B
=B
=�B
>B
>�B
?HB
?�B
?�B
?}B
?�B
@4B
@�B
@�B
@�B
@OB
@OB
@�B
@�B
@�B
AB
B'B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C-B
B�B
B�B
C-B
C�B
D3B
D3B
DgB
C�B
C�B
C�B
D�B
EB
E�B
E�B
F?B
F�B
G+B
GB
GB
GB
GzB
G�B
G�B
G_B
F�B
F�B
F�B
F�B
F�B
F�B
GEB
G+B
G+B
G�B
G�B
G�B
G�B
G�B
G_B
GEB
G�B
H�B
HKB
HKB
H�B
IB
IB
IB
IB
I�B
J=B
J=B
J	B
J=B
J#B
JXB
J�B
K)B
K�B
K�B
LdB
LJB
LdB
LdB
LdB
L�B
MB
MjB
M�B
N�B
O�B
O�B
PB
O�B
OB
OB
OBB
O�B
O�B
O�B
O�B
PHB
P�B
QNB
Q4B
QhB
Q�B
Q�B
RB
R�B
R�B
SuB
S�B
S�B
S�B
TaB
T�B
T�B
T�B
U�B
U�B
U�B
VSB
VmB
V�B
V�B
W�B
W�B
W�B
XB
XEB
XEB
XyB
XyB
X�B
Y1B
ZB
ZB
ZB
ZQB
Z�B
Z�B
Z�B
Z�B
[#B
[WB
[�B
\B
\)B
\xB
\�B
\�B
\�B
\�B
\xB
\]B
\�B
\�B
\�B
]IB
]IB
]IB
]�B
]�B
]�B
]�B
]�B
^OB
^5B
^B
^B
^5B
^B
^�B
_!B
_�B
_�B
`BB
`�B
`�B
`�B
`�B
`BB
`�B
`�B
aB
a-B
aHB
a-B
a�B
a�B
a�B
a�B
b4B
b�B
bhB
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
d&B
d&B
dZB
d�B
d�B
d�B
d�B
d�B
eB
eFB
e�B
fB
f2B
f2B
fLB
f�B
ffB
f�B
g8B
gRB
g�B
h$B
h>B
h�B
h�B
h�B
h�B
h�B
i*B
iyB
i_B
i�B
jB
jB
j�B
j�B
j�B
j�B
k6B
kQB
kQB
k�B
k�B
k�B
k�B
kB
k6B
k�B
k�B
k�B
mB
m�B
m�B
m�B
m�B
nIB
nB
m�B
n/B
n�B
oOB
o5B
oiB
o�B
o�B
pB
p!B
o�B
o�B
oB
o�B
p;B
p�B
p�B
poB
poB
poB
p�B
p�B
p�B
p�B
p�B
p�B
qvB
q�B
q�B
rB
r-B
r-B
r-B
r-B
rGB
r|B
r�B
r�B
sB
s3B
shB
s�B
s�B
s�B
tB
s�B
s�B
s�B
t9B
t9B
t�B
t�B
t�B
u%B
uB
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
wB
v�B
wB
wfB
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xB
x�B
y	B
x�B
x�B
x�B
y$B
y$B
y�3311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<2��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.0(dbar)                                                                                                                                                                                                                                                    None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202004290051332020042900513320200429005133202207271135362022072711353620220727113536202207271537552022072715375520220727153755  JA  ARFMdecpA30a                                                                20200418033747  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200418033748  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200418033749  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200418033749  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200418033749  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200418033750  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200418033750                      G�O�G�O�G�O�                JA  ARUP                                                                        20200418035406                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200419000000  CF  PSAL_ADJUSTED_QC@��@~�RG�O�                JM  ARCAJMQC2.0                                                                 20200428155133  IP  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200428155133  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023536  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063755  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091505                      G�O�G�O�G�O�                