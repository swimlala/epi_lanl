CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-09-30T09:39:09Z creation;2019-09-30T09:39:11Z conversion to V3.1;2022-08-02T05:12:02Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190930093909  20220818081508  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_024                    2C  D   APEX                            8420                            2.11.2                          846 @��pU$� 1   @��p� @/(�TɅ��c������1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�ffB�  B�  B�  B�33B���B�  B�  B�  B�  B���B���B�  B�  B�  B�ffB�33B�  B�  B���B�  B�  B�  Bܙ�B�33B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0L�C1�fC3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB�fDCfDC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�3D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�p 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @ff@�G�@�ff@�
=A�
A?�
A`(�A�  A��
A�  A��A�  A��A�A��
A�B�B�B�HB�
B'�B0
=B8
=B@
=BG��BO�BW��B_��Bg��Bp{Bx
=B�k�B��HB��B�B��B���B��HB��B���B���B���B��3B���B�  B�B�L�B�8RB��B��HB��)B��B��B�\B܀ B��B�  B�B�
=B��fB��B���B��fB��C  C��C�qC�C	�qC  C�RC�qC�qC�qC�qC�qC��C��CC   C!�3C#��C&�C'��C)��C+��C.�C0@ C1�)C3�C5�RC7��C9��C<  C=��C?��CB  CD  CE�qCHCJ�CK�qCN�CP  CR�CTCU��CW�RCY�RC[�qC^�C`  Ca�qCc��Cf�Ch�Cj�Cl\Cm�RCo��Cq��Cs�RCu�RCw�RCy��C{�qC~  C��C���C���C�  C�  C�  C�HC�HC��qC��qC�  C���C��qC�  C���C���C�HC�HC���C��qC�HC���C��)C��qC��)C���C�  C��C�  C���C���C�HC��qC��)C���C��)C�HC�fC�HC��)C���C��qC���C���C��)C���C���C�HC�  C���C���C�  C�  C���C��qC�  C�HC��qC��)C��)C���C��C���C���C���C��RC��)C��qC���C��)C��qC��)C��)C��qC��)C���C���C��qC�HC�C�HC���C���C��)C�  C���C���C�  C�  C��)C���C�  C��C�HC��qC�  C�HC�HC�HC��C��C��C��C��C�  C��)C���C��qC��)C�  C��qC��qC��)C��RC��qC��C��C���C���C�  C��C��qC���C�HC�  C�  C��C�HC���D ��D �D~�D  D\D��D~D��D\D�\D� D�\D� D�\D}qD�qD\D	  D	~�D	�qD
~D
�qD~�D��D~�DHD� D�\D��D��D~�D  D��D  D� D�D��D  D��DHD� D  D��D�D� D�D\D�\D~�D�\D}qD�\D��D �D~�DHD��D �D� D�\D\D  D~�D��D ~�D �D!~�D!��D"� D#HD#� D#�D$� D%�D%��D%��D&}qD&��D'\D( �D(�HD(�\D)|�D)��D*��D*�D+~D,�D,��D-  D-~D-�\D.��D/ �D/��D0 �D0\D0��D1� D2 �D2�HD3  D3� D3�\D4}qD4��D5~D5��D6\D6�D7|�D8 �D8�HD8�D9~�D9�\D:~D:�qD;}qD;�D<� D<��D=~�D> �D>� D?  D?� D@HD@� DA  DA��DB �DB�DC�DC\DC��DD~�DD�qDE}qDE�DF~DF��DG� DG��DH~�DH��DI}qDI�DJ~DJ��DK~�DK�\DL��DL��DM~DM�DN}qDN��DO~DP �DP� DQ  DQ\DQ�DR� DS  DS}qDS��DT�HDUHDU� DV  DV�HDW �DW\DW�DX|�DX��DY��DY�\DZ\DZ�\D[}qD[��D\\D] �D]\D]�qD^~D^�\D_~D_�\D`� D`�qDa~Db �Db��Db�\Dc��Dd�Dd� Dd�De~�De��Df~�Df�Dg\DhHDh~�Dh��Di}qDi�Dj~Dj�Dk}qDk��Dl|�Dl�qDm~�Dn  Dn�HDn��Do~Do�qDp~Dp�\Dq~Dq�qDr}qDr��Ds��Dt �Dt\Dt�\Du~Du�\Dv��DwHDw��Dw�\Dx\Dy  Dy\Dy�\Dz~Dz�\D{\D{��D|\D}  D}� D~  D~�HDHD��D� RD�@RD��D��
D��
D�>�D�
D�� D�  D�@ D�� D�� D���D�>�D�~�D��
D�  D�?�D�
D��RD���D�?\D��RD���D��
D�?
D�
D��\D��\D�?�D��RD���D���D�?\D��D�� D��\D�?\D���D�� D��\D�?
D��D���D��\D�@RD��D��\D��
D�?
D�\D���D� RD�@ D��D�� D��
D�@ D��RD��RD�  D�@ D��RD���D��\D�@ D���D��RD� RD�?\D�
D��\D��
D�?\D�� D��\D��
D�@ D���D��RD���D�?�D��D��
D��
D�@RD��RD��RD� �D�@ D�\D��
D���D�?�D�� D�� D�  D�@ D�\D���D��\D�>�D�\D��\D��\D�?\D��RD��RD���D�?
D�
D��
D�  D�@ D�
D���D�  D�?\D�~�D�� D���D�>fD�\D���D��
D�>�D�� D��RD���D�?
D�
D��
D��
D�?\D��D���D�  D�?\D�
D���D���D�@�D���D��
D��fD�?\D��D�� D�  D�@ D��D���D��\D�?
D�\D���D� RD�?\D�\D���D�  D�?�D�� D��\D���D�?�D�\D��
D� RD�@RD��D��\D�  D�@�D�� D��\D���D�@�D��RD��RD��\D�>fD�
D��
D��\D�?
D�\D���D���D�?�D��D�� D� �D�@ D��RD�� D��\D�?
D�
D���D��
D�?\D��RD�� D���D�>�D�\D���D���D�?�D�\D��D���D�?\D�
D��\D��fD�>�D�~�D���D� RD�?�D�\D���D���D�?
D��D���D���D�@RD��D��RD���D�?�D�� D���D��\D�@RD��RD��
D��\D�?�D�� D���D�  D�?
D��D�� D��
D�?
D�\D���D���D�?
D�� D��RD�  D�?
D��D���D� RD�@�D��D��fD�  D�@ D�\D��RD�  D�?\D�
Dÿ
D���D�?�DĀRDĿ\D���D�>�Dŀ D���D��\D�>fD�~�Dƾ�D��
D�>�D�
Dǿ
D��\D�@RDȀ Dȿ\D��\D�?
D��Dɿ\D���D�?�DʀRDʿ�D��\D�@RDˀ D�� D��\D�>fD�~fD��RD��D�@�D�\DͿ�D���D�?\D΀ Dο�D��\D�?\D�\DϿ�D���D�?
D�\Dп�D���D�@ Dр�D�� D��\D�?�DҀ�Dҿ�D��\D�@�DӀRDӿ\D��\D�?
D�~fDԾ�D��\D�@RDՀRDտ�D���D�@�Dր�D�� D��\D�?\D׀ D׿�D���D�@�D؀ D��RD� �D�@�D��Dٿ
D� RD�@RD�
D�� D�HD�AHDۀRDۿ\D��
D�>�D�\D�� D� �D�?�D�\Dݿ\D���D�@ Dހ�D��HD� �D�?�D߀�D��RD���D�@ D��RD��RD��
D�?�D��D��RD���D�?
D��D���D���D�>�D�
D���D� �D�@RD� D�� D� RD�@RD��D�\D��\D�>�D�
D��RD� RD�@RD�RD�� D���D�?�D��D迮D�  D�?\D�
D�
D��
D�?
D�\D꿮D���D�?\D�\D뾸D��\D�@ D� D�
D��\D�?�D�~�D���D���D�>�D�~D�\D��\D�>fD� D���D�  D�?
D�~fD�D���D�@ D�\D�
D��\D�@RD��D�fD��
D�>�D�~fD�D��fD�>fD�~fD���D��\D�@�D��RD��
D��D�?�D��D���D��\D�@ D�\D��
D��\D�@ D���D��RD� RD�?\D�
D��RD� RD�>fD�\D���D� �D�@RD�o\1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�W
A�V�A�XA�YKA�Z�A�[#A�ZQA�X�A�T,A�XA�YA�]dA�;dA�5�A�1�A���A���A�PHA�B�A�5A�-CA�)_A�A��(A��A��NAߡ-A�|A�GEA�	7A���A܋A��0A�B�Aڷ�AشA׊�A�P�A�'A�_AЈ�A�#�A�gmA�DA��(A��A��aA��.A�S�A��A���A���A�{A��A��<A�m)A��A��aA��1A�OvA�YA�=<A���A��_A���A���A�~]A���A�PA���A�"4A���A�՛A��A���A���A��@A���A�IA��A�N�A��A��aA���A���A|�Ax��Aq�NAkI�Ag;Aa��A_/A^�A[-wAY��AW��AUϫAS]dAQ��AP��AO|�AL��AJoAG��AF�-AF/AE��AD`�AB�AA:�A?*0A=u�A<$A:}VA9�.A8��A7�A5-A4J#A2M�A.�A.҉A.�-A-�A-S&A,�uA+ߤA*�A)}VA(�RA(:�A'u�A&�SA%rGA$�A$*0A#��A#	A!�&A�A:�A�$A($Ag8A@AȴAm]A�A��A<�A�AĜAbAIRA�A��A��A]dA�fA~�A�?A?A($A�AXyA��A_�A��A��A�KA��Ao�A��A�Am]A��AqA$tA�AƨA�	AN�A
��A
/�A	�vA	�A�hA"�A͟A4nA��Aw�AE9A��A6A��AV�A�A��ARTA��A;dA��AA �1@�|�@�_@���@�tT@��R@�R�@�p;@���@���@�%@�'�@�a|@�~�@�z�@���@�>�@�Y�@�!�@��@�@���@�ں@� �@�C�@���@�v�@�5?@�˒@���@�$�@�ݘ@�=@�o@�@��@���@��@�Z�@���@�rG@�@�3�@�7@��@�_@�4@�$t@���@�+k@��@�m]@��@�k@��@�;@�?�@�x@�A�@�@�C�@��@ߦ�@�8�@޸R@�Ta@���@�s�@��@ݞ�@�c�@�}V@�:�@�%@�7@�\�@��@�Q�@��@��@�$�@չ�@Ղ�@��s@�d�@�C�@��&@�n/@��@�L0@�J�@��@п�@Гu@�J�@ϫ�@�@Ι1@�H@��@�^�@��@�l"@�B[@���@��@�J@ɗ$@�=�@Ȏ�@�.�@ǀ4@��@Ƒ�@�+k@�ԕ@Ń{@� \@���@ēu@���@���@��@�}�@�@�A�@�	@�ƨ@��	@�(@��@��@��0@��a@��X@��@��@��@���@�>B@�ƨ@��@���@���@�~@���@��P@��@���@�� @�ff@�Ov@�{@��a@�u�@��@�֡@�n�@�H@�M@��0@�4@��@��L@���@�j@��@��P@�"�@�Ɇ@�|�@�D�@��@��@�P�@�%F@�ѷ@��9@��F@�GE@��)@���@�k�@�5�@�/@�+@���@�r�@��@��0@�L�@��@��U@�kQ@���@�ԕ@���@��M@�#�@���@��@�B[@��@��P@�g�@�4�@�(�@�;@�q�@��@�� @��d@��n@�Dg@�(�@���@��'@���@�H�@�A�@��@�J@��@��@��3@�4@���@��@�2�@��]@��A@��H@���@��=@���@�y�@�t�@�j@�U�@�V@��I@�Q�@��A@��@�8�@��@�[�@�O@�خ@�iD@�"�@��p@��9@���@�YK@��@��@���@�L�@��2@���@�� @�/�@��]@��@���@��N@���@�@�q@��@��@�@��>@���@�[W@�>�@�B�@��@��!@���@�m�@�$@��@�hs@�Ĝ@���@��z@��b@���@�n�@��@��6@��@�IR@�=@�4�@��@��@��@��x@��o@�K^@��@���@���@��@�u�@�8@� \@��@���@��@�|�@�h�@�`�@�E�@�:�@�3�@�-@��@�ƨ@��@�F�@�4�@�,�@�"�@��2@���@��A@�C�@��@���@��@��@@�O�@�#�@���@�Q@�_@��a@��^@��@@�F�@��@��@��$@��e@�u%@���@�:�@��	@�ȴ@�c�@�O@�@��@��7@�T�@�
=@��8@���@�$�@��z@��7@�c�@�4�@���@��h@���@�N�@��@��a@��$@�)_@�@���@��O@�:*@�˒@���@���@�E9@���@���@�H@�~@��@��n@�b�@��@�ȴ@���@���@��_@�u%@�-@��A@��3@���@�{J@�a@�/�@���@��s@���@���@�q@�<�@�/�@��@�@�V@�P@|�@~�,@~E�@}�t@}w2@}	l@|�I@|A�@{�A@{��@{��@{�4@{\)@{P�@z�B@z	@y��@yf�@yDg@y+�@y;@xoi@w��@v��@v$�@uVm@u \@t�j@tV�@tG@s��@s=@r��@r��@rR�@q�t@qs�@q-w@p�@pS�@pM@oRT@n��@n��@nh
@m@m^�@l�@l�@k�K@k��@k33@j��@j+k@i�@i�n@i�@h��@h_@h �@g��@gZ�@g33@f�"@f��@f@�@e�>@e�@d�|@dXy@d	�@c�6@c�f@b�H@b�@bȴ@b��@be@aԕ@au�@a%@_��@^ߤ@^H�@]�.@]��@]V@\�p@\�I@\1@[� @[H�@Z��@Z}V@Z6�@Z�@ZO@Y�j@YL�@X�U@XtT@X%�@Wݘ@W��@Wt�@WE9@V҉@V�@U��@U}�@U[W@T��@Tl"@TD�@Tb@S�q@SC�@S�@R҉@R��@R6�@R�@Q�T@Q��@Qhs@Q@P֡@P��@PD�@PM@O��@OS�@N��@Nc @N@M��@MVm@M-w@L�$@LQ�@K�A@K��@K��@KE9@Jȴ@J�+@Jl�@I�N@IV@Hq@G�W@G�@G��@G��@GZ�@F��@F8�@E�@E�@Ezx@EDg@Eq@D�@DK^@D"h@C�
@CX�@B�R@B$�@A�@A��@AS&@A	l@@�E@@_@?��@?&@>��@>�@=�^@=�7@<��@;�a@;,�@;@:��@:�c@:��@9��@9f�@94@8Ɇ@8�@8*�@7˒@7qv@6�@6��@6z@5�@55�@4Ɇ@4`�@3��@3�w@3l�@2��@2��@2a|@2Ov@2@1�h@0��@0�u@0]d@06@0M@/��@/n/@/>�@/�@.�h@.a|@.5?@-@-Dg@,��@,z�@,oi@,]d@,S�@,>B@+�@+S�@+&@+Y@*�@*��@*�X@*��@*R�@*-@)�H@)a�@)7L@)�@(��@(ѷ@(��@([�@(x@';d@'
=@&��@&�+@&C�@&1�@&$�@%�Z@%�@%c�@%Y�@%X@%N<@%/@%�@$�z@$w�@$~@#�@#��@#�@#�g@#��@#�@"�@"��@"z@"W�@")�@!�d@!rG@!�@ �`@ Ĝ@ ~(@ [�@ @ƨ@�$@��@U�@"�@��@��@W�@_@ԕ@�C@x�@/@�f@ѷ@��@e�@(�@�A@qv@Y@��@�6@��@6�@�3@��@��@a�@F@=�@+�@��@S�@(�@1@�+@��@�W@�F@��@]�@͟@� @c @W�@GE@&�@�C@4@�@%@�@�|@��@Ĝ@�_@�o@c�@9X@�m@�*@��@��@|�@RT@�@�B@�6@�@q�@i�@l�@kQ@^5@Ov@@ԕ@�S@-w@�@�@%@�@��@�4@z�@h�@Ft@ �@x@ƨ@�V@t�@\)@�@�@��@�1@xl1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�W
A�V�A�XA�YKA�Z�A�[#A�ZQA�X�A�T,A�XA�YA�]dA�;dA�5�A�1�A���A���A�PHA�B�A�5A�-CA�)_A�A��(A��A��NAߡ-A�|A�GEA�	7A���A܋A��0A�B�Aڷ�AشA׊�A�P�A�'A�_AЈ�A�#�A�gmA�DA��(A��A��aA��.A�S�A��A���A���A�{A��A��<A�m)A��A��aA��1A�OvA�YA�=<A���A��_A���A���A�~]A���A�PA���A�"4A���A�՛A��A���A���A��@A���A�IA��A�N�A��A��aA���A���A|�Ax��Aq�NAkI�Ag;Aa��A_/A^�A[-wAY��AW��AUϫAS]dAQ��AP��AO|�AL��AJoAG��AF�-AF/AE��AD`�AB�AA:�A?*0A=u�A<$A:}VA9�.A8��A7�A5-A4J#A2M�A.�A.҉A.�-A-�A-S&A,�uA+ߤA*�A)}VA(�RA(:�A'u�A&�SA%rGA$�A$*0A#��A#	A!�&A�A:�A�$A($Ag8A@AȴAm]A�A��A<�A�AĜAbAIRA�A��A��A]dA�fA~�A�?A?A($A�AXyA��A_�A��A��A�KA��Ao�A��A�Am]A��AqA$tA�AƨA�	AN�A
��A
/�A	�vA	�A�hA"�A͟A4nA��Aw�AE9A��A6A��AV�A�A��ARTA��A;dA��AA �1@�|�@�_@���@�tT@��R@�R�@�p;@���@���@�%@�'�@�a|@�~�@�z�@���@�>�@�Y�@�!�@��@�@���@�ں@� �@�C�@���@�v�@�5?@�˒@���@�$�@�ݘ@�=@�o@�@��@���@��@�Z�@���@�rG@�@�3�@�7@��@�_@�4@�$t@���@�+k@��@�m]@��@�k@��@�;@�?�@�x@�A�@�@�C�@��@ߦ�@�8�@޸R@�Ta@���@�s�@��@ݞ�@�c�@�}V@�:�@�%@�7@�\�@��@�Q�@��@��@�$�@չ�@Ղ�@��s@�d�@�C�@��&@�n/@��@�L0@�J�@��@п�@Гu@�J�@ϫ�@�@Ι1@�H@��@�^�@��@�l"@�B[@���@��@�J@ɗ$@�=�@Ȏ�@�.�@ǀ4@��@Ƒ�@�+k@�ԕ@Ń{@� \@���@ēu@���@���@��@�}�@�@�A�@�	@�ƨ@��	@�(@��@��@��0@��a@��X@��@��@��@���@�>B@�ƨ@��@���@���@�~@���@��P@��@���@�� @�ff@�Ov@�{@��a@�u�@��@�֡@�n�@�H@�M@��0@�4@��@��L@���@�j@��@��P@�"�@�Ɇ@�|�@�D�@��@��@�P�@�%F@�ѷ@��9@��F@�GE@��)@���@�k�@�5�@�/@�+@���@�r�@��@��0@�L�@��@��U@�kQ@���@�ԕ@���@��M@�#�@���@��@�B[@��@��P@�g�@�4�@�(�@�;@�q�@��@�� @��d@��n@�Dg@�(�@���@��'@���@�H�@�A�@��@�J@��@��@��3@�4@���@��@�2�@��]@��A@��H@���@��=@���@�y�@�t�@�j@�U�@�V@��I@�Q�@��A@��@�8�@��@�[�@�O@�خ@�iD@�"�@��p@��9@���@�YK@��@��@���@�L�@��2@���@�� @�/�@��]@��@���@��N@���@�@�q@��@��@�@��>@���@�[W@�>�@�B�@��@��!@���@�m�@�$@��@�hs@�Ĝ@���@��z@��b@���@�n�@��@��6@��@�IR@�=@�4�@��@��@��@��x@��o@�K^@��@���@���@��@�u�@�8@� \@��@���@��@�|�@�h�@�`�@�E�@�:�@�3�@�-@��@�ƨ@��@�F�@�4�@�,�@�"�@��2@���@��A@�C�@��@���@��@��@@�O�@�#�@���@�Q@�_@��a@��^@��@@�F�@��@��@��$@��e@�u%@���@�:�@��	@�ȴ@�c�@�O@�@��@��7@�T�@�
=@��8@���@�$�@��z@��7@�c�@�4�@���@��h@���@�N�@��@��a@��$@�)_@�@���@��O@�:*@�˒@���@���@�E9@���@���@�H@�~@��@��n@�b�@��@�ȴ@���@���@��_@�u%@�-@��A@��3@���@�{J@�a@�/�@���@��s@���@���@�q@�<�@�/�@��@�@�V@�P@|�@~�,@~E�@}�t@}w2@}	l@|�I@|A�@{�A@{��@{��@{�4@{\)@{P�@z�B@z	@y��@yf�@yDg@y+�@y;@xoi@w��@v��@v$�@uVm@u \@t�j@tV�@tG@s��@s=@r��@r��@rR�@q�t@qs�@q-w@p�@pS�@pM@oRT@n��@n��@nh
@m@m^�@l�@l�@k�K@k��@k33@j��@j+k@i�@i�n@i�@h��@h_@h �@g��@gZ�@g33@f�"@f��@f@�@e�>@e�@d�|@dXy@d	�@c�6@c�f@b�H@b�@bȴ@b��@be@aԕ@au�@a%@_��@^ߤ@^H�@]�.@]��@]V@\�p@\�I@\1@[� @[H�@Z��@Z}V@Z6�@Z�@ZO@Y�j@YL�@X�U@XtT@X%�@Wݘ@W��@Wt�@WE9@V҉@V�@U��@U}�@U[W@T��@Tl"@TD�@Tb@S�q@SC�@S�@R҉@R��@R6�@R�@Q�T@Q��@Qhs@Q@P֡@P��@PD�@PM@O��@OS�@N��@Nc @N@M��@MVm@M-w@L�$@LQ�@K�A@K��@K��@KE9@Jȴ@J�+@Jl�@I�N@IV@Hq@G�W@G�@G��@G��@GZ�@F��@F8�@E�@E�@Ezx@EDg@Eq@D�@DK^@D"h@C�
@CX�@B�R@B$�@A�@A��@AS&@A	l@@�E@@_@?��@?&@>��@>�@=�^@=�7@<��@;�a@;,�@;@:��@:�c@:��@9��@9f�@94@8Ɇ@8�@8*�@7˒@7qv@6�@6��@6z@5�@55�@4Ɇ@4`�@3��@3�w@3l�@2��@2��@2a|@2Ov@2@1�h@0��@0�u@0]d@06@0M@/��@/n/@/>�@/�@.�h@.a|@.5?@-@-Dg@,��@,z�@,oi@,]d@,S�@,>B@+�@+S�@+&@+Y@*�@*��@*�X@*��@*R�@*-@)�H@)a�@)7L@)�@(��@(ѷ@(��@([�@(x@';d@'
=@&��@&�+@&C�@&1�@&$�@%�Z@%�@%c�@%Y�@%X@%N<@%/@%�@$�z@$w�@$~@#�@#��@#�@#�g@#��@#�@"�@"��@"z@"W�@")�@!�d@!rG@!�@ �`@ Ĝ@ ~(@ [�@ @ƨ@�$@��@U�@"�@��@��@W�@_@ԕ@�C@x�@/@�f@ѷ@��@e�@(�@�A@qv@Y@��@�6@��@6�@�3@��@��@a�@F@=�@+�@��@S�@(�@1@�+@��@�W@�F@��@]�@͟@� @c @W�@GE@&�@�C@4@�@%@�@�|@��@Ĝ@�_@�o@c�@9X@�m@�*@��@��@|�@RT@�@�B@�6@�@q�@i�@l�@kQ@^5@Ov@@ԕ@�S@-w@�@�@%@�@��@�4@z�@h�@Ft@ �@x@ƨ@�V@t�@\)@�@�@��@�1@xl1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�B	�B	�B	�B	�TB	�B	�TB	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�hB	�NB	�NB	��B	��B	��B	�B	�pB	�#B	�B	��B	�YB	��B	~�B	u?B	poB	h>B	X�B	LdB	KxB	N�B	V�B	^�B	|�B	��B
�B
�B
/B
U�B
U�B
��B
�@B
�DB
�VB
�;B
{JB
ezB
^�B
W?B
g�B
l�B
pB
��B
�4B
��B?BXEBpoBi�BD�B2aB#B&B[B
�B
�2B
�,B
ΥB
��B
�B
v�B
c�B
[�B
A�B
-�B
�B	�|B	�HB	�$B	��B	fLB	EB	'�B	B	
XB	�B	�B	�B��B�6B�9B�B�B�8B�B	 �B	*B	*B	,�B	=VB	DB	B�B	?�B	A�B	MPB	[�B	Z�B	^5B	_;B	h�B	kB	^5B	e�B	~BB	�B	�dB	�6B	��B	��B	��B	��B	��B	��B	��B	��B	�B	~�B	��B	�{B	�5B	��B	�HB	��B	��B	��B	��B	�NB	��B	�vB	�pB	�4B	�nB	�ZB	��B	�fB	��B	��B	��B	�eB	�B	��B	��B	�B	��B	��B	��B	�CB	��B	�]B	�B	�'B	�B	�	B	��B	�XB	�XB	�XB	�^B	��B	��B	��B	�(B	�.B	��B	�GB	�B	��B	�9B	�_B	��B	�9B	āB	��B	ǮB	ƨB	��B	�mB	ǔB	�B	�7B	ȴB	�B	�fB	��B	�RB	��B	��B	ȴB	�%B	��B	�OB	�+B	�=B	�dB	�BB	�~B	ǔB	�4B	�OB	�tB	�fB	�YB	�B	�XB	ʦB	��B	�=B	ɺB	��B	�#B	ɠB	�rB	˒B	�xB	�B	��B	ɆB	�XB	��B	ΥB	�hB	��B	уB	�{B	�MB	ՁB	��B	��B	��B	ּB	�+B	�7B	�KB	�KB	�yB	�?B	�B	�B	�yB	ܒB	�IB	ܒB	��B	�IB	�5B	�IB	�~B	��B	ߤB	��B	�BB	�'B	��B	��B	�kB	�qB	�=B	�}B	��B	�B	�*B	�RB	�B	��B	�FB	�B	�FB	�B	��B	��B	�`B	��B	�B	�yB	��B	��B	�_B	�$B	�B	�B	�B	�8B	�B	��B	�XB	�kB	�wB	�]B	��B	�wB	�B	��B	�eB	��B	�B	�B	�B	��B	��B	�CB	��B	�B	��B	�5B	��B	�B	�B	��B	�B	��B	�5B	� B	� B	�5B	�iB	�;B	�B	�B	�iB	�UB	�B	��B	�3B	�MB	�B	��B	�9B	�?B	��B	��B	��B	�RB	��B	�	B	�$B	�*B	�B	�B	�dB	��B	��B	�jB	��B	�"B	��B	��B	�qB	�]B	��B	��B	��B	��B	�B	��B
 B
 �B
 �B
 �B
;B
B
 �B
 �B
 �B
 �B
oB
�B
B
3B
�B
�B
�B
B
mB
�B
B
?B
�B
+B
�B
�B
�B
�B
�B
�B
	RB
	�B
	�B
	�B

=B

rB

rB

�B

XB

XB

�B

�B

�B

�B
B
�B
�B
�B
dB
B
PB
PB
�B
�B
�B
�B
B
BB
�B
�B
}B
}B
}B
�B
�B
�B
�B
 B
�B
�B
�B
B
�B
�B
�B
�B
:B
B
B
@B
uB
�B
{B
�B
{B
�B
�B
�B
�B
�B
MB
�B
�B
MB
2B
B
�B
mB
�B
YB
�B
sB
�B
sB
�B
_B
EB
yB
yB
_B
B
YB
yB
B
�B
sB
�B
_B
yB
�B
�B
�B
eB
�B
�B
B
�B
�B
kB
qB
�B
�B
B
~B
B
�B
�B
B
B
;B
VB
�B
 B
�B
 �B
!�B
"4B
"NB
"�B
"�B
"�B
"hB
"NB
!�B
!�B
!B
!�B
"�B
#B
"�B
"�B
"�B
"�B
#B
#TB
#�B
$B
%�B
&�B
%�B
%�B
%`B
%FB
%FB
%zB
%�B
%�B
%�B
%�B
&LB
&�B
&�B
'B
'8B
'8B
'�B
(XB
(�B
(�B
)DB
)DB
)�B
)�B
*KB
*eB
*B
*�B
*�B
+6B
+kB
+�B
+�B
,"B
,�B
,�B
,�B
.cB
/5B
/B
.�B
/iB
/iB
/�B
0B
0;B
0oB
0�B
1vB
1vB
1�B
2B
2aB
2�B
3hB
3hB
3�B
4nB
4�B
4�B
4�B
4�B
4�B
4�B
4�B
5%B
5tB
5tB
5�B
5�B
6+B
6zB
6�B
6�B
6�B
6�B
6�B
6�B
7�B
7�B
8B
8B
8RB
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
8�B
9	B
9�B
9rB
9rB
9>B
9�B
9�B
9�B
:B
9rB
9�B
9>B
9>B
9XB
9>B
9�B
9�B
9�B
:B
:xB
:xB
:�B
:�B
:�B
;B
;�B
;�B
<6B
<PB
<jB
<�B
=<B
=�B
>B
>B
>]B
>�B
?B
?HB
?HB
?�B
?�B
?�B
?�B
@B
@OB
@4B
@�B
AUB
A�B
A�B
A�B
A�B
A;B
AUB
AoB
A�B
BAB
BB
B'B
BAB
B�B
B�B
CB
C-B
DMB
D�B
EB
EB
ESB
E�B
FYB
FtB
FYB
F?B
F�B
F�B
GB
G+B
G_B
G_B
GzB
G�B
HKB
HKB
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
KB
KDB
KDB
K�B
K�B
K�B
K�B
LB
LJB
L�B
L�B
L�B
L�B
M6B
MB
MB
M�B
NB
N"B
N"B
N�B
N�B
N�B
N�B
O(B
O�B
O�B
O�B
O�B
P}B
P}B
P}B
P�B
Q�B
Q�B
R:B
RTB
R�B
R�B
R�B
R�B
S[B
SuB
SuB
S�B
S�B
S�B
T{B
T�B
T�B
T�B
T�B
UMB
U�B
U�B
VB
VB
U�B
VB
V�B
V�B
W$B
W?B
W�B
W�B
X_B
Y1B
Y�B
ZkB
ZQB
ZQB
Z7B
Z�B
[�B
[qB
[�B
[�B
[�B
\xB
\)B
\)B
\xB
\�B
\�B
]/B
]/B
]~B
^B
^jB
^jB
^�B
^�B
_pB
_pB
_�B
_�B
_�B
`'B
`�B
`�B
`�B
`�B
aB
abB
abB
abB
a�B
a�B
a�B
bhB
b�B
cTB
cnB
cnB
c�B
c�B
cnB
c�B
dZB
dtB
dtB
d�B
d�B
d�B
d�B
d�B
d�B
e`B
e�B
e�B
e�B
e�B
fB
f2B
f2B
f�B
gmB
gmB
g�B
g�B
h
B
h$B
h
B
h>B
h�B
h�B
h�B
h�B
h�B
h�B
h�B
iDB
iDB
i�B
i�B
i�B
i�B
i�B
i�B
jKB
j�B
j�B
j�B
j�B
j�B
kQB
k�B
k�B
k�B
lB
lWB
lWB
l�B
l�B
l�B
l�B
m)B
mCB
mCB
m�B
m�B
nIB
ncB
n�B
n�B
n�B
o B
oB
o�B
o�B
o�B
o�B
p;B
poB
p�B
p�B
p�B
q'B
q�B
q�B
q�B
q�B
rB
rB
q�B
r|B
r�B
sB
sMB
sMB
sMB
shB
s�B
s�B
s�B
t�B
uB
u%B
uZB
utB
uZB
u�B
v`B
vzB
vzB
vzB
v�B
v�B
v�B
wB
wB
wLB
w�B
w�B
xB
x8B
xRB
xRB
x�B
x�B
yXB
yXB
yXB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
zB
zDB
z�B
z�B
z�B
z�B
{B
z�B
{B
{JB
{JB
{dB
{B
{B
{�B
{�B
{�B
|B
|PB
|B
|�B
}B
}1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�B	�B	�B	�B	�TB	�9B	�nB	�9B	��B	��B	��B	�B	��B	��B	��B	��B	��B	� B	��B	�hB	�hB	��B	� B	��B	�hB	��B	��B	��B	��B	��B	�B	��B	v�B	raB	lB	[�B	O�B	PB	S�B	[�B	g�B	��B	�4B

#B
�B
2�B
W�B
[�B
�IB
�XB
�}B
�aB
��B
�B
h�B
a|B
\�B
jeB
o B
uZB
�kB
�aB
��B@�BY�BuZBo�BH�B5ZB�BsB�B
��B
�B
յB
� B
��B
� B
y�B
fLB
`BB
E9B
1[B
"hB	��B	�9B	�B	�
B	k�B	J�B	*�B	9B	�B	�B	B	)B	�B�BB��B�MB�B�B	 �B	!�B	*�B	+kB	.�B	?}B	F?B	E9B	BB	C�B	OBB	\�B	\CB	_�B	bNB	j�B	m�B	a�B	fLB	~�B	�B	�6B	�<B	��B	�B	��B	��B	��B	�B	��B	�?B	��B	�B	��B	��B	�B	��B	�NB	��B	�,B	��B	�hB	��B	�NB	��B	�vB	��B	��B	��B	��B	�8B	�LB	��B	�
B	��B	�B	�WB	��B	��B	��B	��B	��B	�B	��B	�}B	�OB	�-B	�lB	�rB	��B	��B	�B	�*B	��B	�PB	�<B	��B	��B	��B	��B	��B	ĜB	ňB	�?B	�B	ǮB	�B	�9B	�EB	�B	�EB	��B	�?B	��B	ȴB	ɠB	�7B	��B	�B	��B	�#B	ʦB	��B	�lB	��B	�gB	�iB	�_B	�XB	�B	�B	͟B	ȚB	��B	�iB	ƎB	�B	��B	�?B	ʌB	��B	�XB	��B	�rB	˒B	ʦB	��B	��B	��B	��B	ˬB	�=B	��B	��B	�B	�B	ѷB	�B	��B	�2B	յB	��B	�eB	�B	�$B	�$B	��B	��B	ٚB	ٚB	��B	׍B	�9B	�9B	�_B	��B	ݲB	�B	�jB	ݘB	ޞB	ݘB	��B	�!B	�B	�\B	��B	�vB	�B	��B	��B	�B	�WB	��B	�B	�WB	��B	�$B	�8B	��B	�zB	�B	�B	�`B	�`B	�`B	�B	�DB	��B	��B	�QB	�kB	�B	�XB	��B	�mB	�mB	�B	�mB	�DB	��B	��B	��B	��B	��B	��B	�]B	�B	�B	�B	�B	�kB	�"B	�CB	�]B	�B	��B	� B	�5B	�B	� B	�B	�B	��B	��B	�OB	�iB	�5B	�5B	�B	��B	�B	�B	��B	�B	��B	��B	��B	�MB	�B	��B	�9B	�TB	��B	�B	�XB	��B	��B	�>B	�XB	�>B	�DB	�0B	�JB	��B	�JB	��B	��B	�<B	�VB	�"B	�<B	��B	��B	��B	��B	�B	�B	�cB
 4B
 iB
 �B
 �B
 B
oB
;B
 �B
 �B
 B
 �B
�B
�B
GB
MB
�B
B
B
mB
�B
B
YB
�B
�B
_B
�B
KB
�B
�B
�B
	B
	�B
	�B
	�B

=B

�B

�B

�B

�B

�B

�B

�B

�B
B
B
^B
�B
�B
0B
�B
6B
jB
jB
�B
�B
�B
�B
pB
�B
�B
.B
�B
�B
�B
�B
 B
�B
B
 B
 B
 B
4B
hB
�B
B
B
:B
�B
@B
@B
[B
�B
B
�B
�B
�B
�B
�B
�B
B
B
�B
B
�B
�B
MB
2B
�B
�B
�B
�B
�B
�B
B
�B
B
�B
yB
�B
�B
�B
EB
�B
�B
_B
�B
�B
_B
yB
�B
�B
�B
�B
�B
�B
�B
7B
�B
�B
�B
�B
�B
B
/B
�B
OB
�B
�B
!B
;B
pB
pB
�B
 'B
 B
!B
!�B
"NB
"hB
"�B
"�B
"�B
"�B
"�B
"4B
!�B
!-B
!�B
#B
#:B
#B
# B
#B
"�B
#:B
#nB
#�B
$ZB
%�B
'B
&2B
&2B
%�B
%`B
%`B
%�B
%�B
%�B
%�B
%�B
&�B
'8B
'8B
'8B
'mB
'�B
($B
(sB
(�B
)DB
)yB
)yB
)�B
*0B
*�B
*�B
*�B
*�B
+B
+QB
+�B
+�B
+�B
,WB
,�B
,�B
-CB
.}B
/iB
/OB
/ B
/�B
/�B
/�B
0;B
0oB
0�B
1'B
1�B
1�B
1�B
2-B
2�B
2�B
3hB
3�B
3�B
4�B
4�B
4�B
4�B
4�B
4�B
5B
5B
5ZB
5�B
5�B
5�B
5�B
6FB
6�B
6�B
6�B
6�B
7B
6�B
6�B
7�B
8B
88B
8RB
8�B
8�B
8�B
8�B
9	B
9	B
9	B
8�B
8�B
8�B
8�B
9>B
9�B
9�B
9rB
9�B
9�B
:*B
9�B
:DB
9�B
9�B
9rB
9XB
9�B
9�B
:B
9�B
9�B
:DB
:�B
:�B
:�B
;B
;B
;dB
<B
<B
<jB
<jB
<�B
<�B
=qB
>B
>BB
>BB
>�B
>�B
?.B
?cB
?}B
?�B
?�B
@ B
@ B
@OB
@�B
@OB
A B
AoB
A�B
A�B
A�B
A�B
AUB
AoB
A�B
A�B
BAB
B'B
B[B
BuB
B�B
C-B
CGB
C�B
D�B
EB
E9B
EmB
E�B
E�B
FtB
F�B
F�B
FtB
F�B
F�B
G+B
GEB
G�B
GzB
G�B
HB
HfB
HfB
H�B
H�B
H�B
H�B
H�B
IB
I�B
I�B
J	B
J#B
J�B
J�B
J�B
J�B
K)B
K^B
K^B
K�B
K�B
K�B
LB
L0B
LdB
L�B
L�B
MB
MB
MjB
MPB
MPB
M�B
N"B
N<B
NVB
N�B
N�B
N�B
O(B
OBB
O�B
O�B
O�B
PB
P�B
P�B
P�B
QB
Q�B
R B
R:B
RoB
R�B
R�B
SB
S&B
SuB
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
U�B
U�B
U�B
VB
VB
VB
VmB
V�B
W$B
WYB
WsB
W�B
W�B
X�B
YB
ZB
Z�B
ZkB
ZkB
ZkB
[	B
[�B
[�B
[�B
[�B
[�B
\�B
\]B
\]B
\�B
\�B
\�B
]dB
]dB
]�B
^OB
^�B
^�B
^�B
_!B
_pB
_�B
_�B
`B
`'B
`BB
`�B
`�B
`�B
`�B
aHB
abB
a|B
a|B
a�B
a�B
a�B
b�B
cB
cnB
cnB
cnB
c�B
c�B
c�B
c�B
dtB
dtB
d�B
d�B
d�B
d�B
d�B
d�B
e,B
ezB
e�B
e�B
e�B
fB
f2B
fLB
ffB
f�B
g�B
g�B
g�B
g�B
h
B
h>B
h$B
hXB
h�B
h�B
h�B
h�B
h�B
h�B
h�B
i_B
i_B
i�B
i�B
i�B
i�B
i�B
jB
jeB
j�B
j�B
j�B
j�B
kB
kkB
k�B
k�B
lB
l"B
lqB
lqB
l�B
l�B
l�B
l�B
m]B
mwB
m]B
m�B
m�B
ncB
n}B
n�B
n�B
o B
oB
o5B
o�B
o�B
o�B
pB
pUB
p�B
p�B
p�B
qB
qAB
q�B
q�B
q�B
q�B
rB
r-B
r-B
r�B
r�B
s3B
sMB
sMB
sMB
s�B
s�B
s�B
tB
t�B
u%B
u%B
utB
u�B
u�B
vB
vzB
vzB
vzB
vzB
v�B
v�B
v�B
w2B
w2B
wfB
w�B
w�B
xB
x8B
xRB
xlB
x�B
x�B
yrB
yXB
yrB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
zDB
z^B
z�B
z�B
z�B
z�B
{B
z�B
{0B
{JB
{JB
{B
{�B
{�B
{�B
{�B
|B
|B
|jB
|6B
|�B
}B
|�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.01(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201910110051342019101100513420191011005134202207271132582022072711325820220727113258202207271535322022072715353220220727153532  JA  ARFMdecpA30a                                                                20190930093727  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190930093909  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190930093910  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190930093910  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190930093911  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190930093911  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190930093911                      G�O�G�O�G�O�                JA  ARUP                                                                        20190930095501                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20191001000000  CF  PSAL_ADJUSTED_QC@
=@
=G�O�                JM  ARCAJMQC2.0                                                                 20191010155134  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20191010155134  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023258  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063532  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818081508                      G�O�G�O�G�O�                