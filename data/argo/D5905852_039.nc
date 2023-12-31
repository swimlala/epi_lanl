CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2020-02-27T21:37:31Z creation;2020-02-27T21:37:33Z conversion to V3.1;2022-08-02T05:11:21Z update;     
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
_FillValue                 �  ]t   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ad   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  q$   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  u   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �t   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �$   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  Ϥ   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �d   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �t   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �x   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �|   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200227213731  20220818091505  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               'A   JA  A30_8420_039                    2C  D   APEX                            8420                            2.11.2                          846 @�-Pg(�1   @�.J�B @-��|����c���-�1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   BffB  B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�ffB˙�BЙ�B���Bי�Bܙ�B���B�  B�  B�  B�  B�  B�  B�  C   C  C�C33C  C	�fC  C  C  C  C  C  C  C  C  C  C �C"  C$  C&  C(  C*  C+�fC.  C0  C2  C4  C6  C8  C:  C<  C>  C?�fCB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV�CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�fC��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DGfDG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@z�@���@���A Q�A!�A@(�A`Q�A�ffA�Q�A�Q�A�z�A�ffA�ffA�Q�A�{B =qB�B  B�RB�B(  B0
=B8
=B@�BH(�BP
=BX
=B`{Bh{Bp�Bx
=B�B��B��B�{B�{B�\B�
=B�
=B�{B�{B��B��B�
=B�8RB�L�B�B�{B�\B�k�BˮB�B��fBר�Bܙ�B��)B���B��B�{B�  B�
=B�{B�C   C
=C�C=qC
=C	�C  C�C  C  C�C�C
=C
=C�C\C �C"�C$�C&\C(\C*�C+�RC.  C0  C2C4�C6
=C8
=C:�C<\C>
=C?��CB  CD�CF
CH�CJ�CL�CN�CPCQ�qCTCV(�CX�CZ�C\�C^C`
=Cb\Cd�Cf
=Ch�Cj
=Cl�Cn�Cp�Cr  CtCv�Cx�Cz�C|�C~�C�RC��qC���C��C��C�fC�C�fC��C�C�C�
=C�
=C�fC�C��C�fC��C��C�C�C�HC�  C�  C��C�C��C�C��C��C��C�C��C��C�C�C��C�C��C��C��C�HC�C�C��C�C�fC��C�HC��C��C��C��C�HC�  C��C�C�C�C�C��C�HC�C�HC��C�fC��C��C��C�HC��C�C�HC�HC��C��C�fC��C��C�C��C�C�HC��C�C�C���C�  C�fC�fC��C��C�fC��C��C��C��C�C��C�C��C��C�C��C��C��C�fC��C���C�  C�HC�C��C��C��C�fC�fC��C�HC��C�C��C��C��C�C��C�HC��D HD ��D �D��DHD�HD  D� D�D��D�D��DHD�HD�D��DHD��D	3D	��D
HD
��DHD��D{D�3D3D�{D{D��DHD��DHD��D3D��D�D��D �D� D  D�HD�D��D�D��D�D�HD�D��D�D�3D �D��DHD��D{D��D�D�HD �D�3DD��D  �D ��D!�D!��D" �D"��D#�D#� D#�\D$��D%�D%��D&{D&��D' �D'��D(�D(�3D)3D)��D* �D*�HD+�D+��D,�D,��D-�D-�{D.{D.��D/�D/��D0�D0��D1�D1��D2HD2�HD3�D3�3D4�D4�3D5{D5�3D6�D6��D7�D7��D8HD8��D9  D9� D:�D:��D;  D;�HD<�D<��D=HD=�HD>�D>��D?�D?�HD@ �D@� DA �DA�HDB�DB�{DC�DC��DD3DD� DE �DE�3DF�DF�3DG�DG��DHHDH��DIHDI��DJ�DJ��DK �DK��DL�DL��DMHDM��DN�DN�HDOHDO��DP �DP�HDQ�DQ�HDR �DR��DS  DS��DT�DT��DUHDU��DV�DV�HDW3DW�3DX{DX��DY�DY��DZ�DZ�HD[�D[��D\HD\�HD]HD]�3D^�D^��D_3D_�HD`  D`��DaHDa��Db�Db��Dc�Dc��Dc��Dd\De  De��Df�Df��Dg�Dg�HDh�Dh�DiDi�{Dj{Dj�{DkHDk~�Dl  Dl��Dm  Dm� Dn  Dn� Do�Do��Dp�Dp� Dq �Dq�HDr �Dr��Ds  Ds��Dt�Dt��Du3Du�3Dv3Dv��Dw  Dw�HDx�Dx��Dy�Dy��Dz�Dz��D{  D{� D|�D|��D}HD}��D~{D~��DHD\D� RD�@�D���D��RD� �D�AHD���D���D� RD�@�D���D���D� �D�@�D���D���D� �D�@�D��HD��HD�HD�@�D��RD���D� �D�@�D��HD���D�HD�AHD���D���D��D�A�D���D���D�HD�A�D���D��RD� �D�A�D���D���D� �D�?\D���D��=D�HD�@�D���D���D�HD�AHD���D��HD� �D�@RD��RD���D��D�@�D��D��HD�HD�@�D���D���D� �D�@RD�� D��RD� �D�@�D��HD���D�=D�B�D���D�� D� RD�@RD���D���D�HD�A�D���D���D��D�AHD��HD���D� �D�@�D�� D���D��D�AHD���D���D� �D�AHD���D���D��D�@�D�
D���D� RD�?
D��HD���D� �D�@RD���D���D�=D�AHD��RD��RD� �D�@�D���D���D� �D�AHD��HD���D� �D�@RD���D���D� �D�AHD���D���D� �D�@�D���D��HD� �D�@ D�\D��RD��D�@�D�� D���D��D�A�D��HD���D� �D�@�D��RD���D� �D�@�D���D��RD�  D�@RD���D���D� �D�@�D���D���D� �D�@ D��D���D��D�A�D���D�� D� �D�B�D��HD��HD� �D�@RD���D���D� �D�A�D��HD���D� �D�@RD���D���D��D�@�D��RD��RD�HD�A�D��HD���D� RD�@�D���D���D� �D�A�D���D��RD��D�@�D�� D��RD��D�A�D���D��RD� �D�@�D��RD�� D� RD�@�D���D���D��D�@RD���D���D�  D�@�D���D���D� �D�@RD���D��RD�  D�@RD���D��HD� �D�@RD��HD�D�HD�@RD���D�� D��\D�@�D��RD��RD� �D�@�D���D���D�HD�@RD���D���D� �D�@�D���D���D� �D�@�D�D���D� �D�AHDÁ�D��=D�HD�@ DĀRD���D�=D�@�DŁ�D�D� �D�@�DƁHD���D�HD�AHDǁHD���D� �D�AHDȁHD���D��D�@�DɁ�D���D�HD�A�Dʁ�D���D�  D�@�Dˁ�D���D�HD�B=D̂=D���D� RD�@�D̀�D���D� �D�@RD΀�D��HD�HD�AHDπ�D���D��D�A�DЀ�D��RD�  D�@RDр�D��HD� �D�@�DҁHD���D� RD�AHDӁ�D���D�  D�@�DԁHD���D�HD�@ D��D���D��D�A�Dւ�D�D��D�@RD׀ D��HD��D�@ D؀�D���D��D�AHDفHD��HD� RD�@ DځHD�D�=D�B=DہHD���D� �D�@RD܀RD��RD� �D�A�D݁HD���D� �D�AHDށ�D���D�HD�A�D߁HD���D��D�@�D�� D��RD� �D�@�DဤD���D� �D�B=D⁚D��RD� �D�A�D�HD��HD��D�@�D䀤D��HD��D�A�D��D���D��D�@�D�RD���D��D�AHD��D���D��D�A�D��D���D� �D�AHD�=D���D� �D�@�D�HD���D��D�A�D�HD�\D���D�AHD쁚D���D� �D�?�D��D���D� �D�@�D� D��RD� �D�AHDD���D�HD�@�D��HD�D��D�AHD� D�� D� RD�@RD�RD���D� �D�A�D��D��RD� �D�A�D�HD��HD�HD�@ D���D���D��D�A�D��=D��HD�  D�@�D��HD���D� �D�@�D��RD�� D��D�A�D���D��RD�  D�?�D�\D���D� �D�@ D��HD��HD�H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�bA�aA�d&A�a|A�c�A�e`A�d�A�m]A�o5A�poA�qAA�r|A�r|A�r|A�s�A�tA�t�A�^�A��.Aȯ�AȨ�AȦ�Aȣ�Aȝ�Aț	Aȕ�AȎ�AȊ�AȂ�A�|PA�v�A�ncA�j�A�iA�e`A�d�A�e�A�cTA�aA�aA�a|A�bNA�f2A�f2A���A���A�K^A��A�ѷA�:�A�oiA��)A�lWA�T�A�(�A��.A��dA��A��"A��?A�ܒA�zA�,=A�qA��~A��dA���A�֡A�iyA��|A�V�A���A� �A�C-A~�bA{�At�Aq�wAo��AjW�Ag��Ad�ZAb��A`�]A\�RAW�]AR�xAQ1�AO�AN�=AN�AMU�AJ��AH�KAG1�AE�hACZ�ABݘAB�SABs�AA�oA@eA<�hA:Z�A9&�A7)�A5FA4�~A3�]A3S&A2d�A1��A0�A/�jA.�A,��A)��A&o�A%��A%H�A#��A!��A bAA�:A 1A =A��AzxAIRA�BA��Ay>A2�A  A��A��A�uA=qAbA�Aw�A�#A*0A"hA�OAd�A$A��A<6A��A��A�]A��AS�A�A~�A��A`�A!�ARTA�6A|A�AA��A~A�)A�SA��A��A8A��A�7Ad�A�fAf�A�A
�3A
��A
!�A	��A	VAݘA��Ao ATaA��A%FA��Aq�AN<AeA�fAzA�A��A33A��A33A�PA0UA��AIRA �A @�� @�C�@�h�@��P@���@���@�x@�q@��@�o @�G�@�c @���@��y@�_@���@�<6@��K@�R�@�@�,=@��@��m@�D@��@�!-@��@�@�|@�`�@��Q@�*0@�@�C�@��5@�$@�^@��@�"@��]@�5�@�o@�;�@���@�<@�h�@��@�dZ@�N�@�n/@��@��@�Y�@ެ�@�E�@���@ݝ�@ܥz@� �@ې�@�'�@�M@���@�4@�~(@��@�v`@��@֮}@֌@֗�@֚@֙1@�!�@�@Պ	@��@Ի�@��@��@���@���@��[@җ�@��r@љ�@�,�@���@�e�@��Q@ϐ�@�p�@�-w@ν<@�V�@��#@�m]@�>�@̹�@̐.@́o@��g@��@ʰ�@��@ɻ0@�L�@�%@��v@�8�@���@ǻ0@Ǌ	@�L�@��@��X@�d�@��@Ř�@�Vm@��@Ě@�6@öF@�=�@��K@�@�?�@�u@��g@�T�@�
=@��s@�i�@� �@��;@��V@��@���@�W�@��~@��@�ی@��]@�]�@���@�_�@�7�@�1�@���@�o�@�=�@��/@��u@�=q@��g@��$@��H@�d�@�@��d@�T�@��)@��@�$�@���@�خ@��:@�2a@�Y@��j@�_@��;@�~�@��@���@���@�=�@��@�ߤ@��@���@��r@�z@�K^@��@��+@��0@�;d@��'@�\�@�$@��F@�!-@��f@��@�>B@�
�@��&@���@�rG@�+@��P@���@��r@��\@�@��@��@�(�@�	�@���@��6@�~�@�]�@�)_@��r@�7�@��7@�S&@�C@��@��M@��@�oi@�Ft@� �@���@���@���@���@�?}@��@��8@��m@��_@�l�@�
�@��}@��C@�[W@��|@�ں@��1@�E�@�/�@�+k@��@�b@��.@��}@��{@�_p@�6z@��|@���@���@�6@�@���@�� @���@��h@���@�y�@�E9@�IR@��f@���@��A@�I�@��@��@�9�@�o@���@���@���@���@�e�@�(�@�J@���@��V@�33@�;@���@��r@��P@�Vm@�*0@��@��@���@�M@��@���@�u�@�F�@��@�M@�#:@���@�x@�Vm@�q@��@��@�}V@�J�@��D@���@�-w@��v@�|�@�V�@�&�@���@�x@�`B@�=�@��@���@�n�@���@��@��`@��@��@�i�@�3�@��Q@�qv@�s@�\�@��j@�h�@�~@�@���@��C@���@�f�@�@O@��@��@���@�l�@�1�@��@�	�@��F@�j@�E9@�<6@��@��@�ȴ@���@�v�@�Ft@�u@���@��@��z@�f�@��	@��_@�i�@�Ov@���@���@���@�~�@�s@�)_@��@��,@���@�[�@�(�@�a@(@~��@~��@~�@}�h@}p�@|��@|�@|>B@{�]@{��@{�	@{>�@{C@z͟@z�1@zM�@y�@y�@y�>@y�H@y0�@xj@x~@w�K@w��@w�@w!-@v�,@v��@v��@v�L@v3�@u��@uJ�@t֡@t��@s�r@s��@sMj@s"�@r�X@q�-@q4@q�@p@ov`@n��@n�@n($@m�#@m�z@m\�@lѷ@k�m@j��@j3�@i�@ie,@i@@h�@h7@g��@f��@f^5@e�j@d�@d�E@d��@du�@d2�@c�0@c�:@b�M@b�\@b�@a�@aVm@a+@`ѷ@`2�@_n/@_�@^?@]��@]o @]�@\H@[�K@[��@[�@Z��@Z)�@Y�S@YS&@Y=�@Xh�@W��@WO@W�@Vl�@V�@U \@T�/@T��@S��@SO@R��@R�@Q�j@Q�~@QS&@P��@P��@PFt@O�]@O��@O��@Oa@O;d@O4�@O.I@Oo@N�@N�!@NE�@M�@M�z@M�h@M+@L�@Le�@K� @K��@K�f@KO@J��@Jȴ@J��@J($@I��@I�X@I�7@I^�@H�j@H �@G�@G��@G\)@G�@F�B@F�r@FR�@E�X@E@@D�@D�4@D��@DV�@C�0@B�H@Bq�@BE�@B	@A�@A��@A<6@@�@@M@@  @?�r@?��@?��@?�6@?iD@>͟@>u%@>6�@=��@=�'@=�M@=+@<�j@<*�@;�@;x@;�@:�@:0U@:_@9�o@9�n@9T�@9+@8�@8��@8[�@7�A@7S�@6��@6Q@6@5�#@5��@5f�@5�@4��@4�o@4Q�@4-�@3��@3�$@3{J@3o�@2�@2��@2+k@1ԕ@1w2@14@0��@0e�@/�+@/�V@/j�@.�y@.z@.B[@-��@-c@-IR@-#�@-@,��@,g8@,�@+�w@+e�@*�"@*�@*5?@)��@)�#@)��@)L�@)A @)	l@(Ĝ@(m�@'��@'�@'O@'4�@&ߤ@&�}@&}V@&^5@&H�@&-@&&�@&�@%�.@%��@%c@%�@$�)@$��@$��@$M@$@#�@#a@#�@"�]@"�1@"B[@".�@"J@!�9@!��@!rG@ �	@ ��@ Z@��@��@g�@��@�y@��@�@#:@�Z@�C@a�@Vm@?}@��@�@�)@Xy@�@�@��@��@��@�@]�@C�@
=@�B@�1@:*@
�@�>@��@X@@@Ĝ@��@h�@]d@[�@9X@�@ƨ@�*@�{@A�@�@�@��@ں@�h@~�@kQ@YK@4@��@��@�^@��@/@��@��@bN@`�@Xy@�@˒@��@O@>�@1�@�@��@�H@�@�]@��@��@�+@��@xl@C�@�@�t@`B@��@֡@�I@Z@*�@��@ݘ@�K@x@U�@@O@.I@o@��@�8@�8@��@�2@�B@��@&�@�@��@��@S&@-w@+@��@��@>B@"h@�@�@�;@��@~�@a@C�@
��@
�X@
�m@
��@
�@
c @
8�@
-@
$�@
_@	��@	��@	j@	T�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�bA�aA�d&A�a|A�c�A�e`A�d�A�m]A�o5A�poA�qAA�r|A�r|A�r|A�s�A�tA�t�A�^�A��.Aȯ�AȨ�AȦ�Aȣ�Aȝ�Aț	Aȕ�AȎ�AȊ�AȂ�A�|PA�v�A�ncA�j�A�iA�e`A�d�A�e�A�cTA�aA�aA�a|A�bNA�f2A�f2A���A���A�K^A��A�ѷA�:�A�oiA��)A�lWA�T�A�(�A��.A��dA��A��"A��?A�ܒA�zA�,=A�qA��~A��dA���A�֡A�iyA��|A�V�A���A� �A�C-A~�bA{�At�Aq�wAo��AjW�Ag��Ad�ZAb��A`�]A\�RAW�]AR�xAQ1�AO�AN�=AN�AMU�AJ��AH�KAG1�AE�hACZ�ABݘAB�SABs�AA�oA@eA<�hA:Z�A9&�A7)�A5FA4�~A3�]A3S&A2d�A1��A0�A/�jA.�A,��A)��A&o�A%��A%H�A#��A!��A bAA�:A 1A =A��AzxAIRA�BA��Ay>A2�A  A��A��A�uA=qAbA�Aw�A�#A*0A"hA�OAd�A$A��A<6A��A��A�]A��AS�A�A~�A��A`�A!�ARTA�6A|A�AA��A~A�)A�SA��A��A8A��A�7Ad�A�fAf�A�A
�3A
��A
!�A	��A	VAݘA��Ao ATaA��A%FA��Aq�AN<AeA�fAzA�A��A33A��A33A�PA0UA��AIRA �A @�� @�C�@�h�@��P@���@���@�x@�q@��@�o @�G�@�c @���@��y@�_@���@�<6@��K@�R�@�@�,=@��@��m@�D@��@�!-@��@�@�|@�`�@��Q@�*0@�@�C�@��5@�$@�^@��@�"@��]@�5�@�o@�;�@���@�<@�h�@��@�dZ@�N�@�n/@��@��@�Y�@ެ�@�E�@���@ݝ�@ܥz@� �@ې�@�'�@�M@���@�4@�~(@��@�v`@��@֮}@֌@֗�@֚@֙1@�!�@�@Պ	@��@Ի�@��@��@���@���@��[@җ�@��r@љ�@�,�@���@�e�@��Q@ϐ�@�p�@�-w@ν<@�V�@��#@�m]@�>�@̹�@̐.@́o@��g@��@ʰ�@��@ɻ0@�L�@�%@��v@�8�@���@ǻ0@Ǌ	@�L�@��@��X@�d�@��@Ř�@�Vm@��@Ě@�6@öF@�=�@��K@�@�?�@�u@��g@�T�@�
=@��s@�i�@� �@��;@��V@��@���@�W�@��~@��@�ی@��]@�]�@���@�_�@�7�@�1�@���@�o�@�=�@��/@��u@�=q@��g@��$@��H@�d�@�@��d@�T�@��)@��@�$�@���@�خ@��:@�2a@�Y@��j@�_@��;@�~�@��@���@���@�=�@��@�ߤ@��@���@��r@�z@�K^@��@��+@��0@�;d@��'@�\�@�$@��F@�!-@��f@��@�>B@�
�@��&@���@�rG@�+@��P@���@��r@��\@�@��@��@�(�@�	�@���@��6@�~�@�]�@�)_@��r@�7�@��7@�S&@�C@��@��M@��@�oi@�Ft@� �@���@���@���@���@�?}@��@��8@��m@��_@�l�@�
�@��}@��C@�[W@��|@�ں@��1@�E�@�/�@�+k@��@�b@��.@��}@��{@�_p@�6z@��|@���@���@�6@�@���@�� @���@��h@���@�y�@�E9@�IR@��f@���@��A@�I�@��@��@�9�@�o@���@���@���@���@�e�@�(�@�J@���@��V@�33@�;@���@��r@��P@�Vm@�*0@��@��@���@�M@��@���@�u�@�F�@��@�M@�#:@���@�x@�Vm@�q@��@��@�}V@�J�@��D@���@�-w@��v@�|�@�V�@�&�@���@�x@�`B@�=�@��@���@�n�@���@��@��`@��@��@�i�@�3�@��Q@�qv@�s@�\�@��j@�h�@�~@�@���@��C@���@�f�@�@O@��@��@���@�l�@�1�@��@�	�@��F@�j@�E9@�<6@��@��@�ȴ@���@�v�@�Ft@�u@���@��@��z@�f�@��	@��_@�i�@�Ov@���@���@���@�~�@�s@�)_@��@��,@���@�[�@�(�@�a@(@~��@~��@~�@}�h@}p�@|��@|�@|>B@{�]@{��@{�	@{>�@{C@z͟@z�1@zM�@y�@y�@y�>@y�H@y0�@xj@x~@w�K@w��@w�@w!-@v�,@v��@v��@v�L@v3�@u��@uJ�@t֡@t��@s�r@s��@sMj@s"�@r�X@q�-@q4@q�@p@ov`@n��@n�@n($@m�#@m�z@m\�@lѷ@k�m@j��@j3�@i�@ie,@i@@h�@h7@g��@f��@f^5@e�j@d�@d�E@d��@du�@d2�@c�0@c�:@b�M@b�\@b�@a�@aVm@a+@`ѷ@`2�@_n/@_�@^?@]��@]o @]�@\H@[�K@[��@[�@Z��@Z)�@Y�S@YS&@Y=�@Xh�@W��@WO@W�@Vl�@V�@U \@T�/@T��@S��@SO@R��@R�@Q�j@Q�~@QS&@P��@P��@PFt@O�]@O��@O��@Oa@O;d@O4�@O.I@Oo@N�@N�!@NE�@M�@M�z@M�h@M+@L�@Le�@K� @K��@K�f@KO@J��@Jȴ@J��@J($@I��@I�X@I�7@I^�@H�j@H �@G�@G��@G\)@G�@F�B@F�r@FR�@E�X@E@@D�@D�4@D��@DV�@C�0@B�H@Bq�@BE�@B	@A�@A��@A<6@@�@@M@@  @?�r@?��@?��@?�6@?iD@>͟@>u%@>6�@=��@=�'@=�M@=+@<�j@<*�@;�@;x@;�@:�@:0U@:_@9�o@9�n@9T�@9+@8�@8��@8[�@7�A@7S�@6��@6Q@6@5�#@5��@5f�@5�@4��@4�o@4Q�@4-�@3��@3�$@3{J@3o�@2�@2��@2+k@1ԕ@1w2@14@0��@0e�@/�+@/�V@/j�@.�y@.z@.B[@-��@-c@-IR@-#�@-@,��@,g8@,�@+�w@+e�@*�"@*�@*5?@)��@)�#@)��@)L�@)A @)	l@(Ĝ@(m�@'��@'�@'O@'4�@&ߤ@&�}@&}V@&^5@&H�@&-@&&�@&�@%�.@%��@%c@%�@$�)@$��@$��@$M@$@#�@#a@#�@"�]@"�1@"B[@".�@"J@!�9@!��@!rG@ �	@ ��@ Z@��@��@g�@��@�y@��@�@#:@�Z@�C@a�@Vm@?}@��@�@�)@Xy@�@�@��@��@��@�@]�@C�@
=@�B@�1@:*@
�@�>@��@X@@@Ĝ@��@h�@]d@[�@9X@�@ƨ@�*@�{@A�@�@�@��@ں@�h@~�@kQ@YK@4@��@��@�^@��@/@��@��@bN@`�@Xy@�@˒@��@O@>�@1�@�@��@�H@�@�]@��@��@�+@��@xl@C�@�@�t@`B@��@֡@�I@Z@*�@��@ݘ@�K@x@U�@@O@.I@o@��@�8@�8@��@�2@�B@��@&�@�@��@��@S&@-w@+@��@��@>B@"h@�@�@�;@��@~�@a@C�@
��@
�X@
�m@
��@
�@
c @
8�@
-@
$�@
_@	��@	��@	j@	T�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�2B��B�2B��B��B�B��B��B�lB��B��B��B�B�wB��B��B��BB;0BZ�B]IB_�Bb4BcnBf�BhsBh>Bh�BiDBi�Bi�BjKBjKBjBj0BjeBj�Bj�Bj�Bk�Bk�BlWBncBr�B	�B	�B	��B	�sB
�B
-�B
E9B
y�B
��B
��B
�LB
�LB
��B
��B
�B
z�B
mB
lqB
zDB
��B
�pB�B
�LB
��B
��B
5�B	��B	��B	�B	�+B	�xB	�1B	p�B	Z�B	T�B	CB	6+B	-]B	&LB	�B	�B	;B�B�/B�kB�B�B� B�\B�B��B�B��B��B	�B	�B	�B	B	B	<B	6B	�B	�B	�B	jB	#�B	(�B	,=B	1'B	0;B	/iB	%B	
B	B	)B	�B	sB	VB	 B	%�B	*eB	8lB	K^B	V�B	Z�B	\B	aB	ezB	tnB	�B	��B	�B	��B	�~B	�2B	�DB	��B	�B	�iB	�B	�XB	�B	�.B	�B	�B	żB	��B	�B	ȀB	ɆB	�B	�rB	ȴB	�B	�tB	��B	�'B	��B	�B	��B	�SB	ðB	āB	ɠB	�^B	��B	�)B	�#B	�7B	ȴB	�=B	��B	ʦB	�B	ȀB	�DB	�B	οB	�jB	ΊB	ѷB	�B	�(B	�BB	ΥB	��B	�&B	ԕB	��B	�MB	ӏB	ԯB	��B	�B	�B	خB	�?B	�_B	��B	��B	�YB	ؓB	רB	�sB	�?B	�gB	�B	��B	ּB	�sB	ּB	�9B	��B	�YB	�
B	�EB	�B	�B	׍B	��B	�B	ٴB	��B	�=B	�KB	�B	�!B	��B	��B	��B	��B	ބB	ޞB	߾B	�\B	��B	��B	��B	�8B	��B	�4B	ބB	��B	��B	�fB	�B	�NB	�B	�4B	�nB	�nB	�bB	�-B	߾B	�5B	�B	�\B	�BB	�VB	�B	��B	��B	�4B	�vB	�VB	ބB	�!B	ߊB	�BB	��B	�B	�B	��B	�&B	�B	��B	�tB	�B	�B	��B	�FB	��B	�B	�B	��B	�`B	�B	�&B	��B	�,B	�B	�LB	�B	�8B	��B	��B	�*B	��B	��B	��B	�DB	�yB	�B	�B	�B	�B	�B	�B	�wB	�CB	��B	�B	�)B	��B	�CB	��B	�wB	��B	�B	�B	�oB	��B	�B	��B	�[B	��B	�-B	�-B	�-B	�|B	�|B	�|B	��B	�B	��B	�9B	�B	�B	��B	��B	�?B	��B	�tB	�B	��B	�ZB	��B	�zB	��B	��B	��B	��B	��B	��B	�JB	��B	�6B	�PB	�PB	��B	��B	�jB	��B	�VB	�B	��B	�BB	�wB	�]B	��B
 OB
B
�B
uB
AB
UB
-B
�B
MB
aB
�B
�B
MB
�B
�B
?B
B
KB
�B
	�B

�B

�B
B
�B
�B
�B
�B
�B
�B
vB
(B
�B
(B
\B
(B
�B
�B
�B
�B
(B
�B
�B
hB
B
TB
TB
�B
TB
�B
�B
TB
�B
oB
&B
�B
@B
�B
oB
�B
uB
�B
�B
�B
�B
FB
FB
�B
2B
2B
�B
�B
�B
gB
�B
�B
2B
�B
�B
B
B
SB
mB
�B
SB
�B
�B
�B
�B
�B
+B
�B
�B
sB
YB
$B
�B
?B
B
B
�B
B
B
B
�B
�B
�B
_B
+B
B
EB
B
�B
B
1B
1B
B
kB
=B
#B
�B
#B
�B
�B
�B
WB
�B
�B
IB
�B
/B
�B
�B
;B
VB
 B
 �B
 �B
!B
!�B
!�B
"B
"�B
#nB
#�B
$�B
%�B
%�B
&LB
&fB
'�B
(sB
(sB
)B
)�B
)�B
)�B
(�B
'RB
&�B
'�B
'mB
'�B
(�B
)�B
)�B
)�B
)DB
(�B
'�B
'mB
'B
'RB
'�B
(>B
(sB
(XB
(XB
(�B
)B
)_B
)�B
)�B
)�B
*eB
*�B
*�B
*�B
+B
+B
+QB
+�B
+�B
+�B
-wB
-wB
-�B
-�B
.�B
/�B
/iB
/�B
/iB
/�B
/�B
/�B
/iB
/OB
/�B
/�B
0;B
0oB
0�B
1vB
1�B
2�B
2�B
2�B
2�B
3B
3MB
3�B
3�B
4B
49B
4�B
5B
5ZB
5�B
6FB
7�B
7�B
7fB
7LB
7B
6�B
7LB
8B
8RB
8�B
9�B
9�B
:*B
:DB
:DB
:*B
:^B
:�B
:xB
;B
;B
;�B
<B
<B
<PB
<6B
<PB
<�B
<�B
<�B
=�B
=�B
=�B
>B
>wB
>wB
>wB
?B
?cB
?�B
?�B
?�B
@ B
@ B
@�B
A;B
A�B
A�B
AoB
A;B
AUB
A�B
B�B
C{B
C-B
CB
B�B
C�B
C{B
C�B
C�B
C�B
C�B
D3B
C�B
D�B
D�B
EB
E�B
F?B
FtB
F�B
G+B
G_B
G+B
G�B
G�B
G�B
G�B
G_B
G+B
GzB
G_B
G�B
G�B
G�B
G�B
H1B
HKB
H�B
IB
IB
IRB
J	B
JrB
J�B
J�B
K^B
K�B
K�B
LdB
L�B
L�B
MB
MB
MB
MB
MB
MB
M6B
M�B
N<B
N"B
N"B
NVB
NpB
N�B
OB
O(B
N�B
OB
O\B
OvB
O�B
O�B
O�B
PB
PB
P.B
P�B
P�B
P�B
Q4B
QhB
Q�B
Q�B
R B
R:B
R�B
S&B
S@B
S[B
S[B
SuB
S�B
T�B
T�B
T�B
U2B
U2B
U�B
U�B
V9B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W�B
W�B
W�B
W�B
X+B
X+B
X�B
X�B
YKB
YKB
YeB
Y�B
Z7B
ZkB
ZkB
ZQB
Z�B
Z�B
Z�B
[WB
[�B
[�B
[qB
[�B
\)B
\]B
\�B
\�B
\�B
\�B
]/B
]~B
]�B
]�B
]�B
]�B
^B
^B
]�B
^jB
^jB
^�B
_B
_;B
_;B
_�B
_�B
`BB
`vB
`�B
`�B
a�B
a�B
bB
bNB
b�B
b�B
b�B
b�B
bhB
b�B
bNB
a�B
bB
b�B
c:B
c�B
c�B
d�B
e`B
e�B
e�B
fB
e�B
fB
f�B
gB
gB
g�B
g�B
g�B
h>B
h�B
h�B
h�B
i_B
i�B
i�B
i�B
jKB
jKB
j�B
j�B
j�B
j�B
kB
kB
j�B
kB
kkB
k�B
l=B
l�B
l�B
m]B
mwB
m]B
mwB
mwB
m]B
mwB
m�B
m�B
o B
oOB
oiB
o�B
o�B
o�B
oOB
oiB
o�B
o�B
p!B
p!B
p�B
qAB
q[B
qvB
q�B
q�B
q�B
rB
r-B
r|B
r�B
r�B
sB
s3B
s3B
s�B
s�B
s�B
t9B
t9B
tnB
tnB
tnB
t�B
t�B
uB
uB
uZB
u�B
u�B
u�B
u�B
u�B
v+B
vzB
vzB
vzB
v�B
v�B
v�B
v�B
w2B
wfB
w�B
w�B
w�B
w�B
w�B
xlB
x8B
x�B
x�B
x�B
x�B
y>B
y�B
y�B
y�B
y�B
y�B
z*B
zDB
zDB
z*B
z*B
y�B
zxB
z�B
{0B
{JB
{�B
{�B
|B
|PB
|6B
|PB
|�B
|�B
|�B
|�B
}"B
}"B
}"B
}"B
}"B
}B
|�B
}qB
}�B
}�B
~B
~BB
~�B
~�B
~�B
~�B
B
}B
}B
}B
�B
�B
� B
�B
�B
� B
�B
�B
��B
��B
��B
��B
��B
��B
�B
�;B
� B
��B
�oB
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�2B��B�2B��B��B�B��B��B�lB��B��B��B�B�wB��B  B B0B;�BZkB]IB_�Bb4BcnBf�BhsBhXBh�Bi_Bi�BjBjKBjKBjBj0BjeBj�Bj�Bj�Bk�Bk�BlqBn�BuB	mB	&B	��B	�kB
�B
8lB
R�B
��B
��B
��B
�B
�"B
��B
��B
�AB
��B
q�B
oOB
|PB
�7B
��BgB
��B
�JB
�yB
CaB	�B	�XB	��B	�B	��B	��B	t�B	^jB	Z�B	F�B	9�B	0UB	)�B	#:B	�B	�B��B�B��B�B�
B�B� B�@B��B��B�B�B	YB	fB	VB	B	�B	.B	�B	�B	�B	�B	pB	$�B	)�B	-�B	2�B	2-B	2�B	(�B	�B	_B	~B	�B	B	 �B	 vB	%�B	*B	8�B	K�B	V�B	Z�B	\�B	abB	e�B	t�B	�4B	��B	�9B	��B	�B	��B	��B	��B	��B	�oB	�$B	��B	��B	��B	ðB	��B	ƎB	ƎB	�B	��B	�	B	ˬB	�)B	ɆB	��B	��B	��B	��B	�uB	�+B	�EB	��B	�gB	��B	�	B	�0B	�rB	ˬB	ʦB	ɆB	�B	��B	̈́B	�)B	�lB	�B	��B	ΥB	�vB	͹B	οB	�:B	�bB	��B	�.B	�BB	�B	�@B	��B	�B	��B	�B	�B	ؓB	ڠB	�kB	�B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	�SB	�mB	�sB	�EB	��B	ևB	�mB	��B	רB	خB	�yB	ؓB	��B	�KB	ٚB	��B	�=B	��B	ٚB	ܒB	ߤB	��B	�4B	�B	�|B	��B	�B	��B	�BB	�
B	�B	�yB	�
B	�B	� B	�B	�B	�B	�B	��B	�B	�B	�B	�&B	�B	��B	��B	�BB	޸B	�OB	�B	��B	��B	ߊB	�BB	�TB	�B	��B	��B	�B	ߊB	��B	��B	�B	�-B	�B	��B	�@B	�ZB	�B	�B	�B	�FB	�zB	��B	�B	��B	�B	�B	��B	��B	�tB	�B	�B	��B	�B	�B	�mB	�>B	�B	�_B	�B	��B	�DB	�yB	�B	�0B	�6B	�QB	��B	��B	�]B	��B	�wB	�B	�/B	�]B	�B	�wB	��B	��B	�/B	�OB	�B	��B	�AB	�[B	�AB	��B	�-B	�|B	�|B	�aB	�B	�B	��B	�B	��B	�9B	�nB	�TB	�TB	�B	�%B	��B	�FB	��B	�ZB	�FB	��B	�ZB	��B	��B	��B	�>B	��B	��B	�B	�B	�B	��B	��B	��B	�<B	��B	��B	�B	��B	�<B	�B	�wB	��B	��B	�HB
 �B
;B
AB
�B
�B
�B
{B
�B
�B
�B
�B
�B
gB
�B
�B
tB
EB
�B
	B

#B
DB

�B
JB
�B
dB
�B
�B
�B
�B
�B
\B
�B
BB
�B
�B
�B
�B
�B
�B
(B
�B
�B
�B
4B
oB
�B
�B
�B
&B
@B
oB
�B
oB
[B
B
uB
�B
�B
�B
�B
�B
�B
�B
,B
aB
{B
B
gB
gB
�B
�B
B
�B
�B
�B
gB
B
�B
B
9B
SB
�B
�B
SB
�B
�B
�B
$B
�B
EB
�B
�B
�B
sB
?B

B
YB
+B
KB
�B
B
KB
KB
1B
�B
�B
yB
+B
+B
_B
�B
�B
�B
KB
B
QB
�B
qB
qB
qB
WB
	B
�B
�B
�B
CB
B
�B
/B
dB
5B
!B
pB
�B
 \B
 �B
!B
!HB
!�B
!�B
"4B
"�B
#�B
$&B
%,B
%�B
%�B
&fB
&�B
(
B
(�B
(�B
)DB
)�B
*B
*0B
)*B
'RB
&�B
(
B
'�B
'�B
(�B
)�B
)�B
)�B
)�B
(�B
($B
'�B
'RB
'�B
'�B
(XB
(�B
(sB
(sB
)B
)DB
)�B
)�B
)�B
*B
*�B
+B
*�B
*�B
+6B
+B
+kB
+�B
,B
,"B
-�B
-�B
-�B
-�B
/ B
0!B
/�B
/�B
/�B
/�B
/�B
/�B
/�B
/iB
/�B
/�B
0oB
0�B
1'B
1�B
2B
2�B
2�B
2�B
3B
3B
3�B
3�B
3�B
4B
49B
4�B
5%B
5ZB
5�B
6`B
7�B
7�B
7fB
7LB
72B
6�B
7�B
8B
8lB
8�B
9�B
9�B
:DB
:*B
:DB
:DB
:�B
:�B
:�B
;�B
;�B
;�B
<6B
<6B
<jB
<jB
<�B
<�B
<�B
=<B
=�B
=�B
=�B
>BB
>�B
>�B
>�B
?HB
?�B
@ B
?�B
?�B
@B
@4B
@�B
AoB
A�B
BB
A�B
AoB
A�B
A�B
B�B
C�B
CGB
CGB
B�B
C�B
C�B
C�B
C�B
DB
DB
DgB
D3B
D�B
EB
EmB
E�B
F?B
F�B
F�B
GEB
GzB
G_B
G�B
G�B
G�B
G�B
G_B
GzB
G�B
G�B
G�B
G�B
G�B
HB
H1B
HfB
H�B
IRB
IRB
I�B
J#B
J�B
J�B
J�B
K^B
K�B
K�B
LdB
L�B
MB
MB
MB
MB
MB
MB
M6B
MjB
M�B
N<B
N<B
NVB
NVB
N�B
OB
OB
O(B
N�B
O(B
OvB
O�B
O�B
O�B
O�B
PB
P.B
PbB
P�B
Q B
Q B
QNB
Q�B
Q�B
RB
R:B
RoB
SB
S&B
S@B
SuB
SuB
S�B
T,B
T�B
T�B
UB
U2B
UMB
U�B
U�B
VmB
V�B
V�B
V�B
V�B
V�B
V�B
W
B
W�B
W�B
W�B
XB
X+B
X_B
X�B
X�B
YB
YeB
YB
Y�B
ZQB
Z�B
ZkB
ZkB
Z�B
Z�B
[	B
[qB
[�B
[�B
[�B
[�B
\CB
\xB
\�B
\�B
\�B
]B
]/B
]�B
]�B
]�B
]�B
]�B
^5B
^B
^B
^�B
^�B
^�B
_!B
_VB
_VB
_�B
`'B
`BB
`�B
`�B
aB
a�B
bB
b4B
bhB
b�B
b�B
b�B
b�B
b�B
b�B
bhB
b4B
bNB
b�B
c:B
c�B
c�B
eB
e`B
e�B
e�B
f2B
e�B
f2B
f�B
g8B
g8B
g�B
g�B
h
B
h>B
h�B
h�B
h�B
iyB
i�B
i�B
jB
jKB
jeB
j�B
j�B
j�B
kB
kQB
kB
j�B
k6B
k�B
k�B
lWB
l�B
l�B
mwB
m�B
m�B
m�B
mwB
mwB
m�B
m�B
m�B
oB
oiB
o�B
o�B
o�B
o�B
oOB
o�B
o�B
o�B
p;B
pUB
p�B
qAB
q[B
qvB
q�B
q�B
rB
r-B
rGB
r�B
r�B
r�B
s3B
sMB
sMB
s�B
s�B
s�B
tTB
tTB
tnB
tnB
tnB
t�B
t�B
u%B
u%B
utB
u�B
u�B
u�B
u�B
vB
v+B
vzB
v�B
vzB
v�B
v�B
wB
v�B
wLB
w�B
w�B
w�B
w�B
w�B
w�B
x�B
xRB
x�B
x�B
x�B
y	B
y>B
y�B
y�B
y�B
y�B
zB
zDB
z*B
zDB
zDB
zDB
zB
z�B
z�B
{JB
{dB
{�B
|B
|6B
|PB
|6B
|jB
|�B
|�B
|�B
|�B
}"B
}B
}"B
}"B
}<B
}"B
}B
}�B
}�B
}�B
~(B
~BB
~�B
~�B
~�B
~�B
.B
}B
}B
�B
�B
�B
�B
� B
� B
� B
�4B
�B
��B
�B
��B
�B
��B
��B
�B
�;B
�;B
��B
�oB
��311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<(�U<Y�><#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�+<V�b<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.03(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         202003100048172020031000481720200310004817202207271134572022072711345720220727113457202207271537192022072715371920220727153719  JA  ARFMdecpA30a                                                                20200227213729  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20200227213731  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20200227213731  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20200227213732  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20200227213732  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20200227213733  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20200227213733                      G�O�G�O�G�O�                JA  ARUP                                                                        20200227215328                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20200229000000  CF  PSAL_ADJUSTED_QC@�\@�\G�O�                JM  ARCAJMQC2.0                                                                 20200309154817  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20200309154817  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023457  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063719  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818091505                      G�O�G�O�G�O�                