CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-05-23T10:00:28Z creation;2019-05-23T10:00:29Z conversion to V3.1;2022-08-02T05:12:45Z update;     
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
_FillValue                 �  ]@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  a,   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �T   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �$   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20190523100028  20220818081507  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_008                    2C  D   APEX                            8420                            2.11.2                          846 @ظ7)Vـ1   @ظ8A��@,��}Vm�d���C-1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  Aљ�A�  A�33A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bw33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  C �C  C  C  C  C
  C�C  C  C  C  C  C  C  C  C  C�fC"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<�C>  C?�fCB  CD  CF  CH  CJ  CL  CM�fCP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx�Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D fD �fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�3D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�  A Q�A z�A@(�A`Q�A�{A�(�A�=qA�=qA�Q�A�(�A�{A�A��
B  B�B{B �B({B0
=B833B@=qBHG�BP{BX  B`{Bh
=Bo��Bwz�B�8RB�B���B�B�B�B�  B���B�  B�B��B��qB��B�{B�\B�{B�\B��B�#�B��)B���B�  B�\B�.B�\B���B�
=B���B�B�
=B�
=B�#�C {C�C�C\C�C
\C#�C�qC  C�C
=C�CC�C�C�C�3C"C$�C&�C(�C*
=C,C.C0C2�C4
=C6�C8�C:C<
C>\C?��CB�CD  CFCH�CJ�CLCM��CP�CR�CT�CV�CX
=CZ�C\�C^  C`  Cb�CdCf�Ch�Cj�Cl
=CnCp�Cr
=Ct�Cv�Cx{Cz
=C|�C~�C��C��C�fC��C�HC��C�fC��C�  C���C���C���C��C��C��C��C�  C���C�HC��C�HC��C�C�fC��C�C�C��C��C��C��C�  C�  C�  C��C�fC��C�C��C�
=C��C�C��C��C�fC��C�\C�fC�C��C�C�fC�fC��C��C�HC��C�C��C��C�HC��C��C��C��C�HC��C��C�  C��C�fC��C�C�fC�fC��C�HC��C�C��C��C�C��C�C��C��C��C�HC���C�HC�C�HC���C��C�fC��C��C��C��C�fC�fC��C�HC��C�  C���C�  C�  C�  C��C�C��C�
=C��C��C��C�fC��C��C�fC��C�fC�fC��C�C�C�C�fD fD �
D3D��D �D��D�D�HD�D��DHD��DHD� D �D��D�D��D	HD	�HD
 �D
��DHD�HD�D�3DHD��D�D�HD  D��D3D��D�\D�HD�D��D�D��DHD��D�D��D�D� D�\D�HD�D�HD�D��D�\D��D�D�HD�D��D�D�3D3D�HDHD��D  �D ��D!�D!��D"{D"��D#�D#�3D$�D$��D%�D%��D&HD&�D'D'��D(�D(�HD)�D)�D*{D*�3D+ �D+� D, �D,��D- �D-�HD.HD.�HD/ �D/�HD0�D0��D1{D1�D2D2��D33D3��D4HD4� D5 �D5��D6HD6��D7�D7�3D83D8��D9�D9��D:�D:��D;3D;�3D<�D<�HD=�D=��D>3D>��D?�D?��D@HD@\D@�\DA�HDB�DB�HDC �DC��DD�DD��DE �DE��DF�DF�HDG�DG��DHHDH��DIDI��DJHDJ��DK�DK��DL�DL�3DM�DM�DNDN�HDO �DO�HDP �DP\DQ �DQ��DR �DR��DS�DS�HDT�DT�3DU3DU��DVHDV� DWHDW��DX3DX��DY�DY��DZ�DZ�HD[�D[�3D\�D\�3D] �D]��D^HD^��D_  D_�3D`3D`��DaHDa��DbDb��Dc �Dc�HDd�Dd�3De�De�{Df�Df\Dg �Dg��Dh3Dh��DiHDi��DjDj��Dk�Dk�3DlHDl\Dm  Dm��Dn�Dn��Do{Do�3Dp�Dp�HDp�\Dq��DrHDr�3Ds�Ds�HDt�Dt��Du3Du��Dv�Dv�Dw�Dw��Dx�Dx�3Dy�Dy�{Dz�Dz�HD{  D{��D|�D|��D|�\D}~�D}��D~� D �D�3D��D�B�D���D��RD� �D�@�D��RD��HD� �D�@�D���D���D� �D�AHD���D��HD��D�@�D���D���D� RD�@RD���D��=D� �D�@RD���D���D� �D�@�D���D���D� RD�@ D���D���D��D�B=D���D���D� RD�AHD���D��RD�  D�@ D���D���D�  D�@ D�� D���D� �D�@�D��HD���D� �D�@RD��RD���D�HD�@RD��D���D�HD�@�D��RD���D�=D�A�D���D��RD� �D�@ D��D��RD� �D�@�D���D���D� �D�@�D���D�D� �D�@RD��HD���D�HD�A�D���D���D��\D�@ D�� D��HD� �D�@�D���D�� D���D�@�D���D��=D��D�@RD���D���D�HD�A�D��HD��HD� �D�@�D���D���D�HD�@�D���D���D� �D�@ D�� D�� D���D�@�D��HD��RD� RD�@RD�� D�� D� RD�@�D��RD���D��D�A�D��RD���D� �D�@�D���D��RD� RD�A�D��=D��HD� RD�A�D���D��HD� �D�@�D���D���D�HD�@�D���D��HD� �D�@�D���D���D�HD�@�D��HD���D� �D�@�D�� D��RD�HD�@�D��RD���D��D�@�D���D���D� �D�AHD���D��RD� �D�@�D��RD�� D� RD�@�D��HD���D� RD�@ D��D�� D� RD�@�D���D���D�HD�A�D���D�� D�HD�AHD���D���D� �D�@�D���D��HD�HD�@�D���D���D�HD�A�D��HD��HD�HD�AHD��HD��RD�  D�@ D���D���D� �D�?�D��HD���D�HD�AHD��RD��\D� RD�@RD���D���D�  D�@�D���D�� D� �D�@�D���D���D� �D�AHD���D��RD� RD�@RD���D���D� �D�@RD��RD��HD� �D�@ D���D���D�  D�@�D�D�D��D�B�DÁ�D���D� RD�@�DāHD���D� �D�@�Dŀ�D��HD�HD�AHDƀ�D���D� �D�@�DǁHD��HD�HD�@�DȀ�D��HD� �D�AHDɀ�D���D� �D�@�Dʁ�D��HD�HD�B=Dˁ�D���D� �D�@�D́HD���D� �D�@�D̀�D���D��D�A�D΁�D���D��D�A�Dπ�D���D�HD�@�DЁ�D���D��D�AHDсHD��HD� RD�@ DҀ�D��RD� �D�@�DӀ�D���D��D�AHDԀ�D���D� �D�@�DՀ�D��HD��D�@ Dր D���D� �D�AHDׁ�D���D� RD�@�D؁HD��RD� �D�AHDـ�D�� D�  D�@�Dځ�D��HD� �D�@RDۀ�D���D� �D�@�D܀�D��HD��D�A�D݁HD��HD�HD�AHDށHD���D� RD�AHD߀�D���D�HD�A�D���D��HD� RD�?�D��D���D��D�A�D��D��=D� �D�@ D��D���D�=D�@�D䁚D��=D�HD�A�D��D���D��D�@�D��D���D��D�B=D�HD���D�  D�?�D�HD���D� �D�A�D逤D��HD�=D�A�DꁚD���D��D�@�D��D���D�HD�A�D쁚D���D� �D�@ D� D���D�HD�AHDD�� D� �D�A�D��D��HD� �D�@�D��HD���D��D�B=D�D��HD�HD�A�D�HD���D�HD�@�D�\D�� D� �D�@�D�D���D� RD�@RD��RD���D� RD�?�D���D���D�HD�@RD���D���D� �D�@�D���D���D���D�?�D��RD���D��D�AHD��RD���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aқ	AҕAҙeAҔ�Aғ�AҔ�AҖ�Aҗ�Aҗ$Aҗ�AҘ+AҚAҙ�Aқ	AҐ�A�q�A�I�A�YA���A�R�A�3�A�$�A�~A�@A��A��A��A��A��A�CA��[A˛�A�sA��vA�\�A��A�u%A�fA���A�_pA��)A�:^A�v`A���A�o�A�c A�XyA�=qA�oA���A�VmA��A��A���A�OBA��~A��qA��A�v�A��A���A���A���A�$A�2�A�qA�k�A�ɆA��A��2A��8A�Y�A�רA�S�A���A�уA��A���A��gA�K�A�~(A��A�U�A���A��DA��A��RA�A~�IA}#:Az[WAtp�AnɆAj6Af:�Ac�Aa��A^�{A]VmA\�A\��A[��AV�WAQ��AN�PAL��AKXAJ��AI��AHJ�AE8�A@�A?��A>��A>
�A=W?A<P�A;S&A9�jA6�PA68�A5�vA5qvA4ѷA4*�A3�]A3m]A2a�A1��A0��A0�A.��A.@�A-�$A,A�A*��A)�VA'L0A%�A$�eA#�#A"�tA"Q�A"a�A"\�A"R�A"��A"RTA!��A!kQA!8Ao�A��A��AzxAY�A�>A(�A�A�0AQ�A3�A~(AxA�Ah�A�A(A�A�zAffA4�A��AdZA�TA��Ao A\)A�A�A�A
�2A	�~A^�A�9A:�A�A��A�A8�A��AffAW?A.�A�mAW�A��A\�AQ�A�WA�A0UA��A�A ȴA ��A ��A a|A QA IRA 	@��N@�1@�b�@�"�@�Ĝ@���@�@�@���@��@��$@��h@��+@��]@���@�rG@�B�@��`@���@�q�@��^@�@�kQ@���@�r�@�Dg@��"@�S�@�>�@�g8@��W@���@��@�h�@��@�C@炪@椩@���@垄@��@�v�@�,�@�@�@��|@��@��r@߹�@�|�@�,�@��K@�Ov@���@�)_@�a|@��@��@�h�@�V�@���@רX@׮@��@׎"@�5�@���@ֆY@�9X@�1@��@�hs@ө*@�@���@�1�@њk@��2@�_@��N@ϲ-@Ϫ�@�o @��@�<�@�{�@�?@�J@�{J@�PH@ɿH@��?@��T@�$t@�Ɇ@Ɵ�@Ƅ�@�1'@�J@�k�@ĭ�@�l�@�&@��5@��/@�6@�7@�_@��+@��+@���@��H@�Ɇ@�Ov@��0@�/@�	l@��@��5@��@���@��6@�~(@�W�@�#:@�#:@� �@��#@�b�@��@�Q�@��@���@���@��P@�2a@��@��b@�+k@��&@��@��q@�j@�C�@�+@���@��D@��@�l�@��.@��d@���@�X�@�8�@�@��@�҉@���@�GE@�a�@���@�bN@��@��@���@��t@��"@��@�bN@�  @�/�@�^5@���@�S�@�%@���@�v�@�N�@�0U@�!�@��T@�w2@�8@�C@���@���@��.@�Xy@�5?@�  @��~@�d�@��3@�m]@�c�@�]�@�O�@�B�@�*0@�n�@��@���@���@�x@�J#@��|@��@�_@�ԕ@���@�{J@��O@�=q@���@��@�\�@��E@���@�`�@�J�@��@��@��@���@��V@�x�@�dZ@�_p@�]�@�G�@�4@�+@���@��@�҉@���@��L@�ԕ@�zx@�=�@�2a@�,�@��@��@��v@���@���@�U2@�7�@��@��@��9@���@���@���@�s�@�Q�@��@��]@��6@���@��=@�F�@�>�@�8�@�.I@��@��@�ں@��@��@�C-@�!@��@��@���@�_@���@��0@���@���@�+@��@��6@��r@�oi@�`�@�PH@��@��}@�zx@�=@��@��@�H�@���@�v`@�33@�V@��@�ی@��b@�v�@�i�@���@��F@���@���@��M@�n/@�W?@�8@��2@���@�r�@�V@�I�@�C�@�<�@�0U@�!�@���@��z@��C@���@�P�@��@��9@�?�@��@���@��@�e�@�D�@��@���@���@��@��{@�C�@���@���@���@�q@�\�@�6@�  @���@���@�X�@�@�@���@���@���@��2@�ی@��@��z@�l"@�PH@� �@�G@��3@�e,@��@�Ĝ@�!@�G@��N@�qv@�Vm@�Dg@�~(@�M@��@$t@~�@}��@}L�@|��@|�_@{�@{iD@x��@w�F@w;d@w!-@w�@w�@v�@v_�@vO@u�@u��@u��@u�M@uN<@t��@t��@tw�@t?�@s�+@r�8@r5?@p�|@p*�@n��@nL0@m�@m��@mY�@m@l��@k/�@j�@i��@io @iL�@i=�@i+@h��@hg8@g��@g��@g�@f�+@e��@d�@d��@c��@c�V@c�	@c+@b��@a�)@a�@as�@am]@a[W@a&�@a#�@a#�@a#�@a \@a@a@a�@a;@`֡@`��@`4n@_��@^�B@^Q@]�d@]�S@]�@\w�@\"h@[��@Z��@ZO@Y��@Y�3@Y��@YF@X��@W{J@WE9@V��@V�@U%@T��@Tm�@T�@Sy�@R��@R+k@Q0�@P�/@Pg8@O��@O��@Oo�@Og�@O_p@O\)@O/�@O�@N�@N�X@N��@N��@N-@MJ�@K{J@J͟@J��@JOv@J;�@J�@I�@I�@I�@I�j@I��@If�@H�	@H�@G��@F��@E�Z@E�'@E�~@Ef�@E?}@E5�@E�@D�@D��@D	�@C�K@C��@Cx@CS�@CH�@B��@B�<@B��@B��@B��@BV@B!�@A�@A��@A@@��@@r�@@�@?�{@?@O@>�X@>�@>q�@=�@=�d@=�@==�@<K^@<(�@;�A@;X�@;�@:�X@:{�@:GE@:�@9ϫ@9hs@8��@7�k@7F�@7@6�@6ȴ@6��@6�@6v�@6J@5#�@4��@4Xy@4�@3�
@3��@3iD@3'�@2�@2��@2�r@2J�@1�o@1��@1�X@1+�@0��@0c�@0H@0K^@0K^@0A�@07�@0�@/��@/��@/a@/O@/,�@.҉@.� @.z@.a|@.;�@.1�@.O@-��@-ϫ@-��@,��@,��@,��@,�_@,��@,|�@,m�@,[�@+�K@+{J@+F�@+4�@+�@*ں@*�@*��@*��@*�+@*}V@*\�@)��@)��@)rG@)S&@)�@(��@(�.@(l"@(<�@'�+@'��@'�:@'�4@'\)@&��@&�+@&W�@&C�@&4@%��@%�@$��@$��@$�o@$H@$*�@#��@#�K@#l�@#=@"��@"_�@".�@!�D@!X@!;@ �5@ �p@ ��@ ��@ ~(@ [�@ 7�@�}@C@��@0U@{@	@��@��@�@�N@�t@�h@e,@S&@5�@�@�v@�?@��@M@7@�@��@�:@RT@�@�@c @��@j@=�@<6@7L@!�@�@u�@*�@��@iD@E9@1�@&@�@Y@�@�@Y@�@�@�H@��@_�@3�@4@��@��@`B@�@�j@��@oi@D�@!@�]@��@ƨ@��@4�@�@�!@�@�@�\@?@ϫ@�X@c�@%@Ɇ@��@h�@7�@'R@�@�F@��@�4@RT@8@.I@)_@�@
=@S@��@d�@?@ �@�9@�z@�^@�@p�@B�@0�@��@��@S�@,=@�r@��@خ@˒@��@|�@
��@
l�@
#:@	�Z@	�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aқ	AҕAҙeAҔ�Aғ�AҔ�AҖ�Aҗ�Aҗ$Aҗ�AҘ+AҚAҙ�Aқ	AҐ�A�q�A�I�A�YA���A�R�A�3�A�$�A�~A�@A��A��A��A��A��A�CA��[A˛�A�sA��vA�\�A��A�u%A�fA���A�_pA��)A�:^A�v`A���A�o�A�c A�XyA�=qA�oA���A�VmA��A��A���A�OBA��~A��qA��A�v�A��A���A���A���A�$A�2�A�qA�k�A�ɆA��A��2A��8A�Y�A�רA�S�A���A�уA��A���A��gA�K�A�~(A��A�U�A���A��DA��A��RA�A~�IA}#:Az[WAtp�AnɆAj6Af:�Ac�Aa��A^�{A]VmA\�A\��A[��AV�WAQ��AN�PAL��AKXAJ��AI��AHJ�AE8�A@�A?��A>��A>
�A=W?A<P�A;S&A9�jA6�PA68�A5�vA5qvA4ѷA4*�A3�]A3m]A2a�A1��A0��A0�A.��A.@�A-�$A,A�A*��A)�VA'L0A%�A$�eA#�#A"�tA"Q�A"a�A"\�A"R�A"��A"RTA!��A!kQA!8Ao�A��A��AzxAY�A�>A(�A�A�0AQ�A3�A~(AxA�Ah�A�A(A�A�zAffA4�A��AdZA�TA��Ao A\)A�A�A�A
�2A	�~A^�A�9A:�A�A��A�A8�A��AffAW?A.�A�mAW�A��A\�AQ�A�WA�A0UA��A�A ȴA ��A ��A a|A QA IRA 	@��N@�1@�b�@�"�@�Ĝ@���@�@�@���@��@��$@��h@��+@��]@���@�rG@�B�@��`@���@�q�@��^@�@�kQ@���@�r�@�Dg@��"@�S�@�>�@�g8@��W@���@��@�h�@��@�C@炪@椩@���@垄@��@�v�@�,�@�@�@��|@��@��r@߹�@�|�@�,�@��K@�Ov@���@�)_@�a|@��@��@�h�@�V�@���@רX@׮@��@׎"@�5�@���@ֆY@�9X@�1@��@�hs@ө*@�@���@�1�@њk@��2@�_@��N@ϲ-@Ϫ�@�o @��@�<�@�{�@�?@�J@�{J@�PH@ɿH@��?@��T@�$t@�Ɇ@Ɵ�@Ƅ�@�1'@�J@�k�@ĭ�@�l�@�&@��5@��/@�6@�7@�_@��+@��+@���@��H@�Ɇ@�Ov@��0@�/@�	l@��@��5@��@���@��6@�~(@�W�@�#:@�#:@� �@��#@�b�@��@�Q�@��@���@���@��P@�2a@��@��b@�+k@��&@��@��q@�j@�C�@�+@���@��D@��@�l�@��.@��d@���@�X�@�8�@�@��@�҉@���@�GE@�a�@���@�bN@��@��@���@��t@��"@��@�bN@�  @�/�@�^5@���@�S�@�%@���@�v�@�N�@�0U@�!�@��T@�w2@�8@�C@���@���@��.@�Xy@�5?@�  @��~@�d�@��3@�m]@�c�@�]�@�O�@�B�@�*0@�n�@��@���@���@�x@�J#@��|@��@�_@�ԕ@���@�{J@��O@�=q@���@��@�\�@��E@���@�`�@�J�@��@��@��@���@��V@�x�@�dZ@�_p@�]�@�G�@�4@�+@���@��@�҉@���@��L@�ԕ@�zx@�=�@�2a@�,�@��@��@��v@���@���@�U2@�7�@��@��@��9@���@���@���@�s�@�Q�@��@��]@��6@���@��=@�F�@�>�@�8�@�.I@��@��@�ں@��@��@�C-@�!@��@��@���@�_@���@��0@���@���@�+@��@��6@��r@�oi@�`�@�PH@��@��}@�zx@�=@��@��@�H�@���@�v`@�33@�V@��@�ی@��b@�v�@�i�@���@��F@���@���@��M@�n/@�W?@�8@��2@���@�r�@�V@�I�@�C�@�<�@�0U@�!�@���@��z@��C@���@�P�@��@��9@�?�@��@���@��@�e�@�D�@��@���@���@��@��{@�C�@���@���@���@�q@�\�@�6@�  @���@���@�X�@�@�@���@���@���@��2@�ی@��@��z@�l"@�PH@� �@�G@��3@�e,@��@�Ĝ@�!@�G@��N@�qv@�Vm@�Dg@�~(@�M@��@$t@~�@}��@}L�@|��@|�_@{�@{iD@x��@w�F@w;d@w!-@w�@w�@v�@v_�@vO@u�@u��@u��@u�M@uN<@t��@t��@tw�@t?�@s�+@r�8@r5?@p�|@p*�@n��@nL0@m�@m��@mY�@m@l��@k/�@j�@i��@io @iL�@i=�@i+@h��@hg8@g��@g��@g�@f�+@e��@d�@d��@c��@c�V@c�	@c+@b��@a�)@a�@as�@am]@a[W@a&�@a#�@a#�@a#�@a \@a@a@a�@a;@`֡@`��@`4n@_��@^�B@^Q@]�d@]�S@]�@\w�@\"h@[��@Z��@ZO@Y��@Y�3@Y��@YF@X��@W{J@WE9@V��@V�@U%@T��@Tm�@T�@Sy�@R��@R+k@Q0�@P�/@Pg8@O��@O��@Oo�@Og�@O_p@O\)@O/�@O�@N�@N�X@N��@N��@N-@MJ�@K{J@J͟@J��@JOv@J;�@J�@I�@I�@I�@I�j@I��@If�@H�	@H�@G��@F��@E�Z@E�'@E�~@Ef�@E?}@E5�@E�@D�@D��@D	�@C�K@C��@Cx@CS�@CH�@B��@B�<@B��@B��@B��@BV@B!�@A�@A��@A@@��@@r�@@�@?�{@?@O@>�X@>�@>q�@=�@=�d@=�@==�@<K^@<(�@;�A@;X�@;�@:�X@:{�@:GE@:�@9ϫ@9hs@8��@7�k@7F�@7@6�@6ȴ@6��@6�@6v�@6J@5#�@4��@4Xy@4�@3�
@3��@3iD@3'�@2�@2��@2�r@2J�@1�o@1��@1�X@1+�@0��@0c�@0H@0K^@0K^@0A�@07�@0�@/��@/��@/a@/O@/,�@.҉@.� @.z@.a|@.;�@.1�@.O@-��@-ϫ@-��@,��@,��@,��@,�_@,��@,|�@,m�@,[�@+�K@+{J@+F�@+4�@+�@*ں@*�@*��@*��@*�+@*}V@*\�@)��@)��@)rG@)S&@)�@(��@(�.@(l"@(<�@'�+@'��@'�:@'�4@'\)@&��@&�+@&W�@&C�@&4@%��@%�@$��@$��@$�o@$H@$*�@#��@#�K@#l�@#=@"��@"_�@".�@!�D@!X@!;@ �5@ �p@ ��@ ��@ ~(@ [�@ 7�@�}@C@��@0U@{@	@��@��@�@�N@�t@�h@e,@S&@5�@�@�v@�?@��@M@7@�@��@�:@RT@�@�@c @��@j@=�@<6@7L@!�@�@u�@*�@��@iD@E9@1�@&@�@Y@�@�@Y@�@�@�H@��@_�@3�@4@��@��@`B@�@�j@��@oi@D�@!@�]@��@ƨ@��@4�@�@�!@�@�@�\@?@ϫ@�X@c�@%@Ɇ@��@h�@7�@'R@�@�F@��@�4@RT@8@.I@)_@�@
=@S@��@d�@?@ �@�9@�z@�^@�@p�@B�@0�@��@��@S�@,=@�r@��@خ@˒@��@|�@
��@
l�@
#:@	�Z@	�3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B��BؓB�_BؓBؓBخBخB��BؓB��B��B�1B�eB��B��B��B��B	�B	 �B	8�B	<�B	=B	="B	<�B	=�B	=�B	>BB	>BB	>�B	�zB	��B
ʦB
�B
��B^B�B"NB,WBC�BP�B[�BgBm]B�sB��B�B��B��B��B��B��B�B�]B%BJB�BB�B"B�B�BJB
�BSB �B�LB�B��B��BffBZkBS�BNVBHB>�B)B�B
BmB
�B
��B
�!B
�B
h
B
W�B
DB
$B
�B	�.B	�B	�B	� B	�WB	}�B	e�B	S&B	I�B	:DB	3�B	1B	.�B	'�B	B��B�3B��B�B��B�@B��B�xB��B�#B��B��B޸B��B� B�eB�TB�MB�9B�%B��B��B�	B��B��B��B	UB	�B	fB		�B	
�B	B	�B	TB	�B	!�B	"�B	(XB	5ZB	7�B	?B	ESB	G_B	KxB	RB	g�B	�B	��B	��B	��B	�sB	�dB	��B	��B	��B	�3B	�B	��B	�B	��B	�GB	��B	��B	��B	�nB	�CB	��B	��B	��B	�#B	��B	�B	��B	��B	�IB	��B	�xB	�B	��B	�'B	��B	�"B	��B	��B	�RB	�mB	��B	��B	�)B	�cB	��B	��B	��B	�iB	�iB	�B	�uB	��B	��B	ȴB	�"B	�.B	�B	ӏB	�9B	�kB	��B	�`B	�>B	�DB	�B	�2B	�mB	�KB	��B	��B	�PB	��B	��B	��B	�B	�wB	�wB	�(B	�BB	��B	�<B	�PB	�xB	�XB	��B	��B	�B	�B	��B	�5B	��B	�]B	�5B	��B	�/B	�B	��B	��B	��B	��B	�CB	�CB	�"B	��B	��B	�B	�IB	��B	�'B	��B	�MB	��B	��B	��B	��B	�kB	�B	�B	��B	��B	��B	��B	�B	�B	�B	�)B	�B	�B	�B	�"B	�B	�B	�)B	��B	�B	� B	�5B	�B	��B	�;B	�B	��B	�oB	��B	�B	�B	�aB	�GB	�aB	�MB	�B	�B	��B	�9B	�nB	�nB	��B	��B	��B	�B	�%B	��B	�ZB	��B	�FB	�B	��B	�fB	��B	�LB	�2B	�B	�LB	�lB	�lB	�>B	�rB	�rB	�rB	��B	��B	��B	��B	��B	�DB	�B	��B	��B	��B	��B	�dB	�B	�6B	��B	��B	��B	��B	�B	�BB	��B	��B	��B	�B	�}B	�HB	�cB	�}B	��B
�B
UB
�B
B
AB
[B
uB
�B
�B
�B
�B
�B
�B
gB
�B
�B
B
9B
B
B
%B
�B
�B
�B
�B
	RB
	�B

	B

rB

�B

�B

�B

�B
B
�B
�B
xB
�B
�B
0B
dB
dB
�B
B
\B
�B
.B
.B
HB
bB
HB
}B
�B
�B
B
 B
oB
TB
�B
B
�B
aB
�B
�B
�B
{B
B
mB

B
�B
�B
B
B
_B
+B
EB
�B
�B
B
1B
1B
B
KB
eB
�B
B
7B
kB
QB
QB
CB
�B
�B
�B
�B
�B
�B
/B
IB
~B
�B
�B
B
5B
5B
jB
�B
�B
�B
�B
�B
�B
B
�B
 vB
 \B
 BB
 BB
 BB
 'B
 'B
 \B
 BB
 �B
 �B
 vB
 'B
!�B
!�B
"hB
"NB
"�B
"�B
"�B
"�B
#nB
#TB
#�B
#�B
#�B
#�B
#�B
$B
$ZB
$tB
$�B
$�B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'B
'B
&�B
'�B
'�B
'�B
'�B
($B
(>B
(XB
(XB
(�B
(�B
)yB
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*B
*B
*0B
*B
*�B
+6B
+kB
+�B
,WB
-wB
.B
-�B
.IB
.IB
./B
.IB
./B
.�B
/5B
/B
/B
/5B
/5B
/OB
/�B
/OB
/�B
0B
0UB
0oB
0oB
0oB
0oB
0�B
0�B
0oB
0�B
0�B
1B
1AB
1AB
1�B
1�B
2-B
2�B
3�B
3�B
3�B
49B
4B
3�B
5tB
5�B
5�B
5�B
6B
6�B
6�B
6�B
6�B
7B
7B
9XB
9rB
9�B
9�B
9�B
9�B
9�B
9�B
:*B
:*B
:*B
:DB
:^B
:xB
:�B
:�B
;B
;B
;B
;�B
<B
<�B
<�B
=�B
=�B
>B
>(B
>(B
>BB
>(B
?�B
@�B
@�B
@�B
AB
@�B
AB
A B
AUB
A�B
A�B
BB
B[B
C-B
CaB
C�B
DB
D3B
D3B
DgB
D�B
E9B
EmB
EmB
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
FB
F�B
GB
G_B
G�B
G�B
HKB
H�B
H�B
H�B
IB
I�B
I�B
I�B
I�B
J#B
J�B
KDB
J�B
K�B
L0B
L�B
L�B
MB
M6B
M�B
M�B
N�B
OBB
OvB
O�B
P}B
P�B
P�B
P�B
P�B
P}B
P�B
P�B
P�B
P�B
Q B
P�B
QB
RB
S�B
T,B
TaB
T{B
T{B
T�B
T�B
T�B
T{B
T�B
TaB
T�B
UB
UB
U�B
VB
V�B
W
B
W$B
W?B
WYB
W?B
WsB
WsB
W�B
W�B
XB
XEB
XEB
X_B
X_B
X�B
X�B
X�B
X�B
X�B
X�B
YB
Y1B
YeB
Y�B
Y�B
Z7B
ZkB
Z�B
Z�B
[#B
[=B
[=B
[�B
[�B
[�B
\B
\�B
\�B
\�B
]~B
]~B
]�B
^B
^B
^5B
^5B
^�B
_VB
`\B
`vB
`�B
`�B
`�B
`�B
`�B
`�B
a-B
a�B
b4B
b�B
b�B
b�B
cB
c:B
cTB
c�B
c�B
c�B
c�B
dB
d&B
d&B
dtB
d�B
e,B
eFB
eFB
eFB
eFB
eFB
e`B
e�B
e�B
e�B
e�B
e�B
ffB
f�B
f�B
f�B
f�B
f�B
gB
gB
gB
g8B
h
B
h$B
h>B
h>B
h>B
h>B
h>B
h>B
h�B
iB
iDB
iDB
iyB
i�B
i�B
i�B
i�B
i�B
i�B
i�B
j0B
jB
j�B
j�B
k6B
kkB
k�B
k�B
l=B
l�B
l�B
mB
mB
m)B
mCB
m�B
m�B
m�B
nIB
ncB
nIB
n}B
ncB
n}B
n�B
n�B
o5B
oOB
o�B
o�B
o�B
p;B
poB
poB
qAB
q[B
q[B
q�B
q�B
q�B
q�B
rB
r-B
r|B
s3B
s�B
s�B
s�B
s�B
tB
tB
tB
t9B
tTB
tnB
t�B
t�B
t�B
u%B
u?B
uZB
u�B
u�B
u�B
v+B
vFB
v�B
v�B
v�B
v�B
v�B
v�B
wB
wfB
wfB
wfB
w�B
w�B
xB
xlB
y	B
y>B
yXB
yrB
y�B
y�B
y�B
y�B
y�B
y�B
y�B
y�B
z^B
z�B
z�B
z�B
z�B
z�B
z�B
z�B
{JB
{B
{�B
{�B
{�B
|B
|PB
|PB
|�B
|�B
}VB
}�B
}�B
~B
}�B
}�B
~]B
~�B
~�B
~�B
cB
�B
�B
�B
� B
�B
�4B
�iB
�iB
�iB
��B
��B
��B
��B
��B
��B
��B
� B
�oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�AB
�AB
��B
�uB
��B
��B
��B
��B
��B
��B
��B
��B
�B
�MB
�g111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�BخB�yBخBخB��B��B��BخB��B��B�KB�B�1B�WB�XB�$B	pB	!�B	8�B	="B	="B	=<B	=B	=�B	>(B	>�B	?cB	C�B	�2B	�gB
ȚB
�HB
��BdB�B#�B.}BEBQ�B]dBj�Bs3B�$B��B�DB�_B�B�}B�FB�IB��B �B
�BB�B!�B#B(XB�BBBBJB�B{B�JB��B��B��Bh�B[�BUMBO�BJXBBuB+B�B�B�B
�'B
�*B
�TB
��B
kB
\�B
J	B
KB

�B
�B	�B	�B	�tB	��B	��B	iyB	U�B	L�B	;�B	4�B	2B	1AB	-�B	�B	�B�tB�B��B�B�B��B��B�dB�xB߾B��B�BB�B�B�wB��B��B��B�B��B�rB�B��B��B	 �B	�B	mB		7B	
�B	�B	B	�B	B	�B	"�B	$&B	)�B	5�B	7�B	?.B	ESB	G_B	K�B	R�B	hsB	��B	�tB	��B	�B	��B	��B	��B	�wB	�MB	��B	��B	��B	��B	�B	��B	�OB	��B	�B	�,B	��B	��B	�B	�QB	�]B	��B	�B	�LB	�B	�B	�!B	��B	��B	�cB	�|B	�]B	��B	�QB	�
B	��B	��B	�_B	�QB	�]B	��B	�'B	��B	��B	��B	��B	��B	��B	�aB	żB	�lB	�pB	�bB	�NB	��B	�SB	چB	�;B	��B	�DB	�B	�RB	�B	�B	�B	�fB	��B	��B	�VB	�(B	�B	�BB	��B	��B	�wB	�wB	��B	��B	��B	��B	��B	�lB	��B	�TB	�B	�B	��B	�}B	�IB	�UB	�5B	�B	�cB	��B	�cB	�cB	��B	�cB	�IB	��B	��B	��B	�IB	�B	�B	�AB	�B	�B	�B	�|B	��B	��B	��B	�B	�B	�KB	��B	��B	�0B	�B	�B	��B	�]B	��B	��B	�=B	�WB	�]B	�wB	�/B	�5B	��B	�iB	�B	�!B	�UB	��B	�B	�B	�B	��B	�B	�|B	�B	�B	��B	��B	�9B	�B	�nB	��B	��B	�B	��B	��B	��B	�B	��B	��B	�B	��B	�zB	�zB	��B	��B	��B	�fB	�LB	�fB	��B	��B	��B	��B	�rB	��B	��B	��B	��B	��B	��B	��B	�^B	�B	��B	�*B	�DB	�0B	��B	�PB	�jB	��B	��B	�<B	��B	�BB	��B	��B	�B	�B	�HB	��B	�}B	��B	��B
 �B
'B
�B
�B
'B
uB
uB
�B
�B
�B
�B
�B
aB
MB
�B
�B
B
9B
SB
SB
�B
tB
�B
zB
KB
	B
	�B
	�B

#B

�B

�B

�B

�B

�B
^B
�B
�B
�B
�B
B
dB
�B
�B
B
�B
�B
�B
.B
HB
HB
}B
}B
 B
�B
�B
 B
TB
�B
�B
�B
[B
FB
�B
FB
aB
2B
gB
�B
�B
YB
�B
�B
B
+B
yB
EB
_B
�B
�B
1B
1B
1B
1B
eB
B
�B
7B
7B
�B
�B
�B
xB
�B
�B
�B
�B
�B
B
IB
dB
�B
�B
�B
B
OB
OB
�B
�B
�B
�B
�B
B
!B
;B
;B
 �B
 \B
 BB
 \B
 \B
 'B
 BB
 vB
 vB
 �B
 �B
 �B
 �B
"B
!�B
"�B
"hB
"�B
"�B
"�B
#B
#�B
#nB
#�B
#�B
#�B
#�B
#�B
$@B
$�B
$�B
%,B
%,B
%�B
%�B
&�B
&�B
&�B
&�B
&�B
'B
'8B
'8B
'�B
(
B
(
B
(
B
(>B
(XB
(sB
(�B
(�B
)*B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*B
*0B
*0B
*eB
*�B
+B
+kB
+�B
+�B
,�B
-�B
./B
./B
.cB
.cB
.IB
.}B
.cB
.�B
/5B
/5B
/5B
/OB
/OB
/iB
/�B
/�B
/�B
0;B
0UB
0oB
0oB
0oB
0�B
0�B
0�B
0�B
0�B
1B
1AB
1[B
1[B
1�B
2B
2|B
3B
3�B
3�B
4B
4TB
4TB
4TB
5�B
5�B
6+B
6+B
6`B
6�B
7B
7B
72B
7�B
7�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:B
:*B
:*B
:*B
:DB
:xB
:�B
:�B
:�B
;B
;JB
;B
<6B
<jB
<�B
=VB
=�B
>B
>(B
>(B
>]B
>�B
>�B
@B
@�B
@�B
AB
A B
AB
A B
AUB
AoB
A�B
A�B
B[B
B�B
C{B
C�B
C�B
D3B
D3B
DMB
D�B
D�B
ESB
EmB
EmB
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
E�B
FB
FYB
F�B
G+B
G�B
G�B
HB
H�B
IB
H�B
IB
I7B
I�B
I�B
I�B
I�B
JrB
KB
KDB
K)B
K�B
L~B
L�B
L�B
M6B
MjB
M�B
NB
OB
OvB
O�B
P.B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
Q B
P�B
QB
Q B
QhB
R�B
S�B
TFB
T{B
T�B
T�B
T�B
T{B
T�B
T{B
T�B
T�B
UB
UMB
UgB
U�B
VSB
V�B
W$B
W$B
W?B
W?B
WYB
W�B
W�B
W�B
XB
X+B
XEB
X_B
XyB
XyB
X�B
X�B
X�B
X�B
X�B
YB
Y1B
YKB
Y�B
Y�B
ZB
ZkB
Z�B
Z�B
[	B
[=B
[WB
[qB
[�B
[�B
[�B
\]B
\�B
\�B
\�B
]�B
]�B
]�B
^B
^5B
^OB
^jB
^�B
_�B
`vB
`�B
`�B
`�B
`�B
`�B
`�B
`�B
a|B
bB
bNB
b�B
b�B
cB
c B
cTB
cnB
c�B
c�B
c�B
c�B
dB
d@B
dZB
d�B
d�B
e,B
eFB
eFB
eFB
eFB
e`B
ezB
e�B
e�B
fB
fB
fB
f�B
f�B
f�B
f�B
f�B
gB
gB
g8B
gB
gmB
h
B
h$B
h>B
h$B
h>B
hXB
hXB
hsB
h�B
i*B
iDB
i_B
i�B
i�B
i�B
i�B
i�B
i�B
i�B
jB
jKB
j�B
j�B
j�B
k6B
kkB
k�B
k�B
lWB
l�B
l�B
m)B
mB
m]B
mwB
m�B
nB
nB
ncB
n�B
ncB
n�B
n}B
n}B
n�B
n�B
oOB
oiB
o�B
o�B
pB
pUB
poB
p�B
q[B
qAB
qvB
q�B
q�B
q�B
rB
r-B
raB
r�B
shB
s�B
s�B
s�B
s�B
tB
tB
tB
tTB
tnB
t�B
t�B
t�B
t�B
u?B
uZB
utB
u�B
vB
u�B
vFB
v`B
v�B
v�B
v�B
v�B
v�B
v�B
w2B
wfB
wLB
w�B
w�B
xB
x8B
x�B
y$B
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
y�B
zxB
z�B
z�B
z�B
z�B
z�B
z�B
{B
{dB
{B
{�B
{�B
{�B
|B
|PB
|jB
|�B
|�B
}VB
}�B
}�B
~B
~B
~B
~]B
~�B
~�B
~�B
}B
�B
�B
� B
�B
� B
�4B
�iB
��B
�iB
��B
��B
��B
��B
��B
��B
��B
�;B
�oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�[B
�[B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�3B
�gB
�g111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<-��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.03(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201905040041492019050400414920190504004149202207271130522022072711305220220727113052202207271533362022072715333620220727153336  JA  ARFMdecpA30a                                                                20190523095841  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190523100028  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190523100028  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190523100029  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190523100029  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20190523100029                      G�O�G�O�G�O�                JA  ARUP                                                                        20190523111516                      G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190503154149  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190503154149  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023052  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063336  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818081507                      G�O�G�O�G�O�                