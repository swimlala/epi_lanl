CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-05-23T10:00:30Z creation;2019-05-23T10:00:31Z conversion to V3.1;2022-08-02T05:12:42Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190523100030  20220818081507  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               	A   JA  A30_8420_009                    2C  D   APEX                            8420                            2.11.2                          846 @غ�W�� 1   @غ��g(�@,�N;�6�d��A��1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @33@�  @�  A   A   A@  A`  A�  A�  A�  A�  A���A�ffA���A�ffA�33B  B  B  B   B(  B0  B8  B@  BH  BP  BZ  B^ffBg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�ffB�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�33B���C   C�fC  C  C  C
  C  C  C  C  C  C33C  C�fC  C  C   C"  C$  C&  C'�fC*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz�C|33C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǃ3D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�<�D�� D�� D�  D�@ D�� D�� D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�@�G�@��A (�A ��A@��A`z�A�ffA�=qA�Q�A�ffA���A�z�A��A��HA�\)B  B(�BG�B =qB(=qB033B833B@33BH(�BP(�BZ(�B^�\Bg�HBp
=Bx  B�
=B�{B�\B�
=B�\B�\B�\B�
=B�{B�#�B�.B�#�B�u�B�33B�Q�B�ǮB��B�\B�\B�B��B��B�\B�  B�\B�
=B�{B��B�W
B�B�B�8RB��)C �C�CCC\C
�C�C
=C�C\C
C=qC�C�CC
=C 
=C"
=C$\C&\C'�RC*�C,
=C.�C0�C2�C4�C6
=C8\C:�C<�C>&fC@CBCD
=CFCHCJCL�CN�CP�CR\CT�CVCX�CZ�C\�C^�C`�Cb�CdCf�Ch�Cj\Cl{Cn
Cp�Cr
=Ct
=Cv�Cx\Cz)C|EC~�C�HC�HC��C��C�fC�fC�fC�C��C��C��C��C��C��C�
=C��C�fC�fC��C��C��C�fC�C��C��C��C�fC�C��C��C�fC��C��C�C��C��C��C�fC��C�
=C��C��C�C��C�C�HC��C�fC��C�  C�  C��C��C�fC��C��C�fC�C��C��C�C�C��C��C��C�C�C�C��C��C�fC��C��C�fC��C��C��C�fC�C��C��C��C��C�C��C��C��C��C�fC�fC�C�fC�C��C��C�
=C�fC��C�fC��C�C�fC��C�  C��C��C��C��C��C��C��C�C��C�fC��C��C��C��C��C��C�C��C��C�fC�C�
=C��C�fD D ��DfD�{D3D�3D�D��D3D��D�D��D�D��DHD�HD3D�3D	3D	�{D
3D
��D{D�{D�D��D�D� D �D��D�D�D�D��D{D�{D�D��D�D�{D�D��D�D��D�D��D�D�3D3D�3D3D�3D�D�D{D�{D�D��D�D��D3D��D�D�{D �D �{D!�D!�D"�D"�3D#�D#��D$�D$�{D%�D%�{D&�D&�3D'HD'�HD(HD(��D)3D)��D*�D*��D+HD+��D,�D,��D-�D-��D.�D.�HD/�D/��D0HD0�HD1 �D1��D2HD2�3D3�D3��D4 �D4��D5�D5��D63D6��D7�D7��D8 �D8�3D9�D9��D:{D:�{D;�D;�3D<3D<��D=�D=�3D>3D>�{D?{D?�{D@�D@��DA�DA��DB{DB�3DC3DC��DD�DD��DE�DE��DF�DF��DG�DG��DHDH�DI�DI�3DJ{DJ�{DK�DK� DLHDL�3DM�DM�3DNDN�{DODO��DP3DP��DQ�DQ��DR{DR�HDS3DS��DT�DT��DU�DU��DV�DV�{DW{DW��DX{DX�DY�DY�3DZ3DZ��D[�D[��D\D\��D]3D]�{D^{D^��D_�D_�3D`�D`��Da�Da��Db�Db�{Dc�Dc�3Dd3Dd��De�De�HDf�Df�{Dg�Dg��Dh�Dh��Di3Di�Dj�Dj�3Dk�Dk��Dl3Dl�{Dm�Dm�3Dn3Dn�3Do�Do��Dp�Dp��DqHDq��Dr3Dr�{Ds�Ds�{DtHDt��Du�Du��Dv3Dv�3Dw�Dw��Dx �Dx��Dy{Dy��Dz3Dz�{D{{D{��D|D|��D}3D}��D~fD~��D{D��D��D�B=D���D���D�HD�AHD��HD���D� �D�@�D���D���D��D�@�D��HD���D��D�A�D���D���D��D�A�D��HD���D��D�A�D��RD��HD�HD�AHD��=D��HD�HD�A�D���D��HD� �D�AHD��HD��HD� �D�A�D��=D��=D�HD�@�D���D��HD��D�A�D��HD���D��D�A�D���D���D�=D�A�D���D���D� �D�@�D��HD��=D��D�A�D��HD���D� �D�@�D���D��HD� �D�@�D���D�D�=D�A�D��=D���D� �D�B�D��3D���D�=D�B�D���D�D��D�B�D��=D���D�HD�@�D���D���D�=D�B�D���D���D�3D�B�D���D���D�HD�AHD���D���D� �D�@RD���D���D� RD�@�D��HD���D��D�B=D��=D��HD��D�B�D���D���D��D�B�D���D���D�HD�@�D���D��RD�  D�AHD���D�ÅD��D�AHD���D�D��D�AHD���D��=D��D�A�D���D���D��D�@�D���D��HD��D�A�D���D���D��D�B=D���D��HD� RD�@�D���D���D��D�@�D���D���D��D�B=D���D�D��D�B=D���D�D�HD�A�D��HD��HD��D�AHD���D���D� �D�AHD��HD���D��D�@�D���D���D��D�AHD���D���D� RD�@�D��HD���D� �D�@�D���D���D�HD�AHD���D���D� �D�@RD��HD���D�HD�@�D��HD���D� �D�A�D��=D��HD�HD�A�D���D��HD��D�A�D��RD���D��D�B�D���D���D� �D�@�D���D�D��D�C3D��=D��HD��D�B�D���D���D��D�A�D���D���D��D�@�D���D���D� �D�@�D���D���D� �D�AHD���D���D� RD�@�D��HD���D� �D�@�D�D���D�HD�@�DÀ�D��HD� �D�@ DāHD�D�=D�B=Dł�D��HD� RD�@�DƁ�D���D��D�B=Dǃ�D���D��D�A�DȁHD�D�3D�A�Dɀ�D��HD��D�A�DʁHD���D�HD�@�DˁHD��=D�HD�AHD́�D��HD��D�A�D͂=D��HD�HD�A�D΁HD���D�=D�B=Dς=D���D�HD�A�DЂ=D�D�=D�A�DсHD��HD� RD�AHDҁ�D��HD� �D�@RDӁHD���D��D�A�Dԁ�D���D�  D�@RDՁHD���D��D�B�Dց�D���D��D�A�Dׂ�D��=D�=D�B�D؂=D���D�HD�B=Dق=D���D�HD�@�DځHD���D�HD�B=Dہ�D�D�3D�C3D܁�D���D��D�A�D݂=D���D��D�A�Dށ�D��HD� �D�A�D߂�D���D��D�A�D���D���D��D�A�D�HD��HD� �D�@RD�RD���D� �D�@�DずD���D��D�B=D䁚D���D��D�@�D�HD���D��D�AHD�HD��HD�HD�AHD��D���D�=D�A�D��D��=D��D�@�D逤D���D��D�B�D��D��=D��D�AHD��D�D��D�B�D�3D�D�=D�B�D��D�D�3D�B�D�3D���D�HD�@�D� D��HD� �D�@RD���D��=D�HD�B=D��D���D��D�@�D�RD��RD� RD�B=D��D���D� �D�@�D�D���D��D�C3D���D�D�=D�A�D���D���D��D�A�D���D���D��D�A�D��=D���D� �D�@�D��RD���D��D�?�D��HD���D��D�B�D���D��=D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�EmA�GzA�FA�>�A�<A�5�A�'�A�(�A�'�A�"�A�=A��A��A��A��A�$A�\A��A�ҽAӘ�A�{�A�J�AҿHA�^�A�
�A�poA�~�AΩ_Aˣ�A�	lAʓ@A��GAɟ�A�G�A��A�k�A���AǑhA��A��zAƪeAƐbA�i�A��A��lAś�A��Aē�A��AÞ�A�T,A��A�A�^A�%A�_;A��*A�A�m�A�Q�A�s�A��]A���A��A��,A��LA��lA���A�}�A��A�Q�A���A��A�ٴA��zA��A��A�1A��WA�MA�wfA�e,A���A�	�A�@�A���A�t�A�OA��A���A���A��/A���A�>�A~��A}0UAsĜAp��Ang�Ah��Af�AcI�AaMA^�XA^
=A](�A\�AVX�ARr�APoAMYAI��AHe�AE�"ADU2AC�VABL�A@��A?}�A?$tA>��A=��A=�eA=-A:�RA8�DA7+A3��A1MjA0�3A04�A/�A.��A.a�A.2aA.2aA-��A,\�A+>�A)��A)MA'�A'�A&��A%ȴA%sA%�A#j�A!��A!2aA �oA!
=A �'A�A��A?�A��A��AϫA��A+kA^�A�A�AoAoA�.AuA�zA�ZA$A�:A�EA��A��A=�A(�A��A�VAd�AA�A
�A
��A
r�A
qvA
m�A
i�A
c A
[WA
A�A	��A	�3A	�!A	U�A	�A�A�xAN<A��A�[A�A�nAw�A1�A�|A�IAB�A�4A!�A�fA��A�A�AƨA�0A��AVmA�`Aa|A ��A �NA ��@�u�@���@��v@�H@�!�@�j@���@�n/@� \@��@�@�}V@��@��s@��@��@�7@�w2@��@��@�@핁@�=@�@�R@�C�@��@�n@�H�@���@��@�&�@�F�@�8�@�P@�*0@��@�6@���@��@��@�Y�@�7L@�@ὥ@�$t@ߛ=@�]d@�@�@�F�@�+@��/@ڜx@�@�@ج@؂A@�h
@�C�@�-@��@��@���@�ѷ@�l�@��a@�]�@��@ԡb@��&@Ӡ'@���@��>@�J#@�~�@�)�@��o@��@�{J@ΆY@́@̽<@�-�@�ϫ@˧�@�|@�W?@�#�@�>B@���@���@�1�@ƿ�@�c @�(�@�ϫ@�{J@�\)@��f@Ĺ$@�W�@�iD@�@��m@@�c�@�9X@��A@��@�@��z@�>B@��@��F@�{J@���@�u�@�Z�@�)�@��@�:�@�V@��@��K@��D@���@�Ĝ@�k�@��@��m@�g8@�@��@���@�Z�@��@��@��2@���@�J@��a@��P@�E9@�o@��v@�� @�A�@��@��@�2a@��H@���@���@��2@�_@��t@�8�@���@�C-@��D@��j@��@���@��@�ȴ@�Q@��@���@�c@�s�@�S�@�Dg@�=�@��@��@���@���@��@�g8@��@��[@�R�@��@���@�hs@��@�ߤ@��@�PH@�,=@�M@�  @��@�]�@���@�,=@���@��a@���@�9�@�҉@�L0@���@���@��j@��*@��{@�c�@�W?@�7L@���@���@��A@�Z@�:*@�1'@�O@��@��:@�x@�0�@��m@���@�~�@�kQ@�YK@�?�@�O�@���@�*�@��>@��@��W@��$@��)@��r@�'R@���@��C@���@�Dg@�#�@��@���@�j@�Ft@�O@��@�|�@��@�@���@�H@�O@��@��@��@@���@��=@���@��"@�n/@�@@��@�z�@�q@�Xy@�#:@��@��&@���@�k�@�1�@��_@���@�8@�@@���@��?@���@�h�@���@�=�@�C@��@���@��@��}@�Z@��@���@���@�b�@�U�@�=�@��@��@���@�A�@��@��@��@��:@�-w@��@�[�@�=q@�(�@��.@��}@���@���@���@�&�@��@�?@�� @�e�@�ȴ@���@�{�@�\�@�8�@�'R@��@��@��Z@��@��T@���@���@�k�@��@���@���@���@�v�@�W�@�3�@��j@��P@�"�@��"@��@�J@���@��@@�o�@�7L@�@@��@�<�@�}@Y@~�2@~ں@~�m@~YK@|�[@|g8@|I�@{�
@z�@y��@y4@xی@x��@x�o@xe�@x:�@w��@w+@v͟@vGE@u�Z@u�M@t�f@s�@r�@rZ�@q�t@qIR@p֡@p��@pr�@o�]@o�0@oU�@n͟@nkQ@m�@m��@m\�@m?}@m#�@l�P@l�p@l�z@l~(@lM@l%�@lG@k˒@kZ�@kY@j��@k i@j��@j��@i�@i;@h��@gخ@gs@go@f��@f�\@fC�@e��@e@ek�@d�@c�K@c��@c&@b��@bh
@b\�@a�C@a*0@`��@_�a@_�@_b�@_A�@_�@^��@^W�@]�)@\S�@[�@Z	@X�p@Xu�@X	�@W��@W.I@V~�@U�H@U��@U!�@T�@TK^@S��@S��@Rp;@QS&@P��@O�A@Os@N�@M�~@L�_@L�@K�V@Ko�@K4�@J�2@J�}@J0U@I�)@I�@I�X@I�h@I@@H�j@H��@H�Y@HK^@H$@Gݘ@G��@G��@G�@F�m@Fh
@E�@E�@E��@EN<@E@D�K@D�@D�@D�)@D��@D�z@DS�@D�@C�@Cƨ@C��@C{J@Cqv@Ct�@Cy�@C|�@CiD@Bff@B�@A��@A�@Ae,@A!�@@�|@@Ĝ@@H@?��@?{J@?l�@?@O@?Y@>�@>��@>�A@>R�@>&�@>	@=�@=s�@=�@<�`@<�D@;�@;\)@:��@:v�@:Z�@:@�@:J@9�9@9Vm@8�@8��@8�@7��@7��@7n/@7U�@7;d@7$t@6�@6�}@6��@6)�@5��@5`B@4��@4��@4e�@4S�@4C-@4$@41@3�+@3�}@3qv@3@O@3�@2��@2�r@23�@2�@1�@1?}@0�@0H@/��@/
=@.�\@.\�@-�D@-��@-B�@- \@-�@,�	@,��@,��@,��@,|�@,h�@,Q�@,7�@,!@,1@+��@+خ@+�0@+�@+g�@*�@*�6@*�x@*��@*W�@*-@)�@)��@)��@)��@)F@(�K@(Z@(N�@(@'��@'l�@&��@&��@&M�@&�@%�@%��@%�@%@%�H@%��@%��@%rG@%*0@$Ɇ@$[�@$!@#�@#�a@#�*@#��@#�@#qv@#.I@"��@"�m@"u%@!�C@!�S@!��@!}�@!?}@ ��@ `�@ 9X@��@o�@Z�@E9@�@�8@��@_�@3�@$�@{@�@��@�/@z�@e�@Q�@2�@b@�&@�6@�K@ƨ@�a@��@�0@�F@�F@�V@'�@�@ȴ@�@�\@C�@e@��@�@f�@T�@5�@�@�P@�K@�)@�@M@1'@,=@'R@ �@�@�@�@��@P�@�@�}@YK@5?@�@ϫ@�t@��@Q�@V@�@Ĝ@�e@�u@:�@�}@P�@)_@�@��@B[@!�@_@��@�@�@��@�h@�"@�"@��@X@�@�I@��@g8@��@��@��@�@��@�m@��@}V@R�@)�@O@@�@ԕ@�z@��@��@�-@�t@�t@��@e,@�@q@�@�|@Ɇ@�9@��@��@��@��@w�@Q�@I�@?�@<�@ �@��@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�EmA�GzA�FA�>�A�<A�5�A�'�A�(�A�'�A�"�A�=A��A��A��A��A�$A�\A��A�ҽAӘ�A�{�A�J�AҿHA�^�A�
�A�poA�~�AΩ_Aˣ�A�	lAʓ@A��GAɟ�A�G�A��A�k�A���AǑhA��A��zAƪeAƐbA�i�A��A��lAś�A��Aē�A��AÞ�A�T,A��A�A�^A�%A�_;A��*A�A�m�A�Q�A�s�A��]A���A��A��,A��LA��lA���A�}�A��A�Q�A���A��A�ٴA��zA��A��A�1A��WA�MA�wfA�e,A���A�	�A�@�A���A�t�A�OA��A���A���A��/A���A�>�A~��A}0UAsĜAp��Ang�Ah��Af�AcI�AaMA^�XA^
=A](�A\�AVX�ARr�APoAMYAI��AHe�AE�"ADU2AC�VABL�A@��A?}�A?$tA>��A=��A=�eA=-A:�RA8�DA7+A3��A1MjA0�3A04�A/�A.��A.a�A.2aA.2aA-��A,\�A+>�A)��A)MA'�A'�A&��A%ȴA%sA%�A#j�A!��A!2aA �oA!
=A �'A�A��A?�A��A��AϫA��A+kA^�A�A�AoAoA�.AuA�zA�ZA$A�:A�EA��A��A=�A(�A��A�VAd�AA�A
�A
��A
r�A
qvA
m�A
i�A
c A
[WA
A�A	��A	�3A	�!A	U�A	�A�A�xAN<A��A�[A�A�nAw�A1�A�|A�IAB�A�4A!�A�fA��A�A�AƨA�0A��AVmA�`Aa|A ��A �NA ��@�u�@���@��v@�H@�!�@�j@���@�n/@� \@��@�@�}V@��@��s@��@��@�7@�w2@��@��@�@핁@�=@�@�R@�C�@��@�n@�H�@���@��@�&�@�F�@�8�@�P@�*0@��@�6@���@��@��@�Y�@�7L@�@ὥ@�$t@ߛ=@�]d@�@�@�F�@�+@��/@ڜx@�@�@ج@؂A@�h
@�C�@�-@��@��@���@�ѷ@�l�@��a@�]�@��@ԡb@��&@Ӡ'@���@��>@�J#@�~�@�)�@��o@��@�{J@ΆY@́@̽<@�-�@�ϫ@˧�@�|@�W?@�#�@�>B@���@���@�1�@ƿ�@�c @�(�@�ϫ@�{J@�\)@��f@Ĺ$@�W�@�iD@�@��m@@�c�@�9X@��A@��@�@��z@�>B@��@��F@�{J@���@�u�@�Z�@�)�@��@�:�@�V@��@��K@��D@���@�Ĝ@�k�@��@��m@�g8@�@��@���@�Z�@��@��@��2@���@�J@��a@��P@�E9@�o@��v@�� @�A�@��@��@�2a@��H@���@���@��2@�_@��t@�8�@���@�C-@��D@��j@��@���@��@�ȴ@�Q@��@���@�c@�s�@�S�@�Dg@�=�@��@��@���@���@��@�g8@��@��[@�R�@��@���@�hs@��@�ߤ@��@�PH@�,=@�M@�  @��@�]�@���@�,=@���@��a@���@�9�@�҉@�L0@���@���@��j@��*@��{@�c�@�W?@�7L@���@���@��A@�Z@�:*@�1'@�O@��@��:@�x@�0�@��m@���@�~�@�kQ@�YK@�?�@�O�@���@�*�@��>@��@��W@��$@��)@��r@�'R@���@��C@���@�Dg@�#�@��@���@�j@�Ft@�O@��@�|�@��@�@���@�H@�O@��@��@��@@���@��=@���@��"@�n/@�@@��@�z�@�q@�Xy@�#:@��@��&@���@�k�@�1�@��_@���@�8@�@@���@��?@���@�h�@���@�=�@�C@��@���@��@��}@�Z@��@���@���@�b�@�U�@�=�@��@��@���@�A�@��@��@��@��:@�-w@��@�[�@�=q@�(�@��.@��}@���@���@���@�&�@��@�?@�� @�e�@�ȴ@���@�{�@�\�@�8�@�'R@��@��@��Z@��@��T@���@���@�k�@��@���@���@���@�v�@�W�@�3�@��j@��P@�"�@��"@��@�J@���@��@@�o�@�7L@�@@��@�<�@�}@Y@~�2@~ں@~�m@~YK@|�[@|g8@|I�@{�
@z�@y��@y4@xی@x��@x�o@xe�@x:�@w��@w+@v͟@vGE@u�Z@u�M@t�f@s�@r�@rZ�@q�t@qIR@p֡@p��@pr�@o�]@o�0@oU�@n͟@nkQ@m�@m��@m\�@m?}@m#�@l�P@l�p@l�z@l~(@lM@l%�@lG@k˒@kZ�@kY@j��@k i@j��@j��@i�@i;@h��@gخ@gs@go@f��@f�\@fC�@e��@e@ek�@d�@c�K@c��@c&@b��@bh
@b\�@a�C@a*0@`��@_�a@_�@_b�@_A�@_�@^��@^W�@]�)@\S�@[�@Z	@X�p@Xu�@X	�@W��@W.I@V~�@U�H@U��@U!�@T�@TK^@S��@S��@Rp;@QS&@P��@O�A@Os@N�@M�~@L�_@L�@K�V@Ko�@K4�@J�2@J�}@J0U@I�)@I�@I�X@I�h@I@@H�j@H��@H�Y@HK^@H$@Gݘ@G��@G��@G�@F�m@Fh
@E�@E�@E��@EN<@E@D�K@D�@D�@D�)@D��@D�z@DS�@D�@C�@Cƨ@C��@C{J@Cqv@Ct�@Cy�@C|�@CiD@Bff@B�@A��@A�@Ae,@A!�@@�|@@Ĝ@@H@?��@?{J@?l�@?@O@?Y@>�@>��@>�A@>R�@>&�@>	@=�@=s�@=�@<�`@<�D@;�@;\)@:��@:v�@:Z�@:@�@:J@9�9@9Vm@8�@8��@8�@7��@7��@7n/@7U�@7;d@7$t@6�@6�}@6��@6)�@5��@5`B@4��@4��@4e�@4S�@4C-@4$@41@3�+@3�}@3qv@3@O@3�@2��@2�r@23�@2�@1�@1?}@0�@0H@/��@/
=@.�\@.\�@-�D@-��@-B�@- \@-�@,�	@,��@,��@,��@,|�@,h�@,Q�@,7�@,!@,1@+��@+خ@+�0@+�@+g�@*�@*�6@*�x@*��@*W�@*-@)�@)��@)��@)��@)F@(�K@(Z@(N�@(@'��@'l�@&��@&��@&M�@&�@%�@%��@%�@%@%�H@%��@%��@%rG@%*0@$Ɇ@$[�@$!@#�@#�a@#�*@#��@#�@#qv@#.I@"��@"�m@"u%@!�C@!�S@!��@!}�@!?}@ ��@ `�@ 9X@��@o�@Z�@E9@�@�8@��@_�@3�@$�@{@�@��@�/@z�@e�@Q�@2�@b@�&@�6@�K@ƨ@�a@��@�0@�F@�F@�V@'�@�@ȴ@�@�\@C�@e@��@�@f�@T�@5�@�@�P@�K@�)@�@M@1'@,=@'R@ �@�@�@�@��@P�@�@�}@YK@5?@�@ϫ@�t@��@Q�@V@�@Ĝ@�e@�u@:�@�}@P�@)_@�@��@B[@!�@_@��@�@�@��@�h@�"@�"@��@X@�@�I@��@g8@��@��@��@�@��@�m@��@}V@R�@)�@O@@�@ԕ@�z@��@��@�-@�t@�t@��@e,@�@q@�@�|@Ɇ@�9@��@��@��@��@w�@Q�@I�@?�@<�@ �@��@�@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B��B��B�B��B��B�oB��B�'B��B��B�hB�B�QB	-B	�B	�B	)DB	,=B	.�B	BAB	W�B	l�B	� B
 �B
/OB
�B
�B
�tB
��B
�+B
��B
��B
�B�B!HB;dBDBJ�BM�BN�BS�BU�B^Bi�BuZB�B�xB�B�'B��B��B��BοB�RB�B�B�BmB,B�B�B]B%zByBB BBB�B	B�B'B��B�/B�B�.B��B�%B�]B�HB�B��BU�B vB�B
�<B
�B
�'B
�4B
z^B
iDB
J=B
$�B
�B	�mB	�RB	��B	}B	m�B	`�B	UMB	J�B	F%B	@�B	9	B	)B	B	�B	�B	GB�B�qB��B��B��B	 �B	 �B	 4B	  B	  B	 B	�B	oB	�B	uB	$ZB	%�B	+�B	/�B	=<B	AUB	E�B	HKB	I�B	M�B	R�B	VB	Y�B	[WB	bB	gB	m)B	r|B	r�B	r�B	tB	r�B	q'B	p�B	r�B	vzB	z�B	{B	uB	qAB	mB	o�B	oB	o�B	poB	u%B	v�B	y	B	|B	�B	��B	�oB	�B	��B	�VB	��B	��B	�PB	�bB	�BB	��B	��B	�aB	��B	�MB	�SB	�$B	�$B	�YB	�YB	�YB	�YB	�sB	��B	��B	��B	��B	�qB	��B	��B	�IB	��B	�hB	��B	�fB	��B	�XB	�B	��B	�GB	��B	��B	�dB	��B	��B	��B	��B	�B	��B	��B	�AB	��B	�B	ɺB	ʦB	�B	�B	ѷB	�oB	ӏB	��B	�B	�mB	��B	ּB	��B	՛B	�?B	֡B	��B	�B	�B	�KB	ٚB	��B	�QB	��B	ۦB	��B	��B	ޞB	�pB	��B	�B	�B	�TB	�B	�FB	�sB	�mB	�$B	�B	�B	��B	�B	�>B	��B	�RB	��B	��B	�B	�;B	�B	�cB	�wB	�wB	�]B	�B	��B	�B	�cB	��B	��B	�]B	�]B	��B	�B	�CB	�]B	�wB	��B	�UB	�UB	�UB	�B	��B	�oB	�AB	�'B	�B	��B	��B	�B	��B	�B	�B	�GB	��B	�B	�B	��B	�B	�B	�B	��B	�hB	�B	�nB	��B	�%B	�%B	��B	��B	��B	�+B	��B	�B	�LB	�LB	��B	�RB	�$B	��B	�^B	��B	��B	�^B	��B	�*B	�DB	�DB	��B	�xB	�^B	�^B	�^B	�0B	��B	��B	��B	��B	�0B	�6B	��B	��B	��B	�BB	�wB	��B	��B	��B	�cB	�HB	�.B	��B
 B
 4B
 iB
 �B
 �B
 �B
;B
oB
�B
�B
[B
uB
�B
aB
�B
mB
mB
�B
�B
�B
EB
EB
zB
�B
B
fB
�B
	RB
	�B
	�B
	�B
	�B
	�B
	�B

=B

XB

�B

rB

rB

�B

�B
)B
�B
B
pB
B
vB
�B
B
�B
B
.B
B
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
aB
�B
�B
�B
�B
gB
9B
SB
�B
�B
�B
�B
�B
YB
?B
�B
B
+B
+B
+B
B
�B
1B
�B
kB
kB
kB
B
�B
WB
#B
�B
B
CB
]B
�B
�B
�B
dB
~B
�B
�B
5B
B
�B
�B
 �B
!B
!|B
!�B
"4B
"NB
"hB
"NB
"NB
!�B
!�B
!�B
"4B
"B
!�B
"B
"NB
"4B
"NB
"NB
"�B
"�B
#�B
$tB
$�B
$�B
$�B
%B
%B
%,B
&fB
&�B
&�B
&�B
&�B
&�B
'B
'RB
'�B
(
B
(>B
(�B
(�B
(�B
(�B
(�B
)*B
)�B
)�B
*B
)�B
*KB
*�B
+�B
+�B
+�B
+�B
+�B
,B
,"B
,"B
,=B
,�B
-B
-�B
-�B
.}B
/5B
/iB
/iB
/�B
/�B
/�B
/�B
/�B
/�B
/�B
/�B
/�B
0!B
0;B
0�B
1B
1'B
1'B
1�B
1�B
1�B
2GB
2aB
33B
2�B
3�B
49B
4TB
4nB
4�B
4�B
4�B
5ZB
5�B
6FB
6`B
6FB
6+B
5�B
6+B
6�B
6�B
6zB
6�B
7�B
8�B
8�B
9$B
9>B
9$B
9$B
9	B
9�B
9�B
9�B
9�B
9�B
9�B
:*B
;B
<PB
<�B
="B
=VB
=�B
=�B
=�B
=�B
>B
>BB
>wB
>�B
>�B
?.B
?HB
?.B
?HB
?cB
?}B
?�B
?�B
?�B
?�B
?�B
@ B
@�B
@iB
@�B
@OB
@4B
@ B
A B
A�B
A�B
B'B
BuB
B�B
B�B
CB
CGB
CGB
CGB
CaB
D3B
D�B
D�B
EB
E�B
E�B
ESB
FB
F%B
F�B
GB
GEB
G+B
GEB
GEB
GzB
GzB
GzB
IB
I�B
J=B
K^B
K^B
K�B
L0B
LdB
L�B
MPB
MPB
M�B
NB
NB
NVB
NVB
O\B
O�B
PbB
P�B
P�B
P�B
RB
R�B
R�B
SuB
S�B
S�B
S�B
TB
T{B
T�B
T�B
T�B
T�B
U2B
UMB
UgB
UgB
U�B
U�B
U�B
VB
U�B
VSB
VmB
V�B
V�B
W
B
W$B
W?B
WsB
W�B
WsB
WsB
W�B
W�B
W�B
W�B
XB
XB
XEB
XEB
X_B
X_B
XyB
X_B
XEB
XB
Y1B
Y1B
YeB
YB
Y�B
Y�B
Y�B
Y�B
ZkB
Z�B
Z�B
Z�B
[	B
[#B
[=B
[WB
[�B
[�B
[�B
[�B
[�B
\)B
\�B
\xB
\�B
]/B
]�B
^jB
^jB
^OB
^jB
^�B
^�B
_B
_!B
_�B
`'B
`B
`BB
`\B
`vB
`�B
`�B
`�B
`�B
`�B
aB
aHB
a�B
a�B
a�B
bB
b4B
b4B
bNB
b4B
b4B
bhB
b�B
b�B
b�B
b�B
c B
c:B
c B
c�B
c�B
dB
d&B
d�B
e`B
e�B
e�B
f2B
f�B
f�B
gB
f�B
gB
g8B
gRB
gmB
gmB
g�B
g�B
g�B
g�B
g�B
g�B
h
B
g�B
h$B
h$B
h�B
h�B
h�B
h�B
iB
i*B
i_B
i�B
iyB
i_B
i�B
i�B
jB
jeB
j�B
kB
kB
k�B
k�B
lB
l=B
l=B
lWB
lqB
lqB
lqB
lqB
l�B
l�B
l�B
mB
mwB
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
ncB
ncB
n�B
oiB
oiB
oiB
oiB
o�B
p!B
p�B
poB
p�B
qB
p�B
p�B
qB
qB
q[B
q�B
q�B
q�B
q�B
q�B
rB
r�B
shB
shB
s�B
s�B
s�B
s�B
tB
tB
tB
tB
tB
tB
tB
tB
tB
t�B
t�B
t�B
t�B
t�B
utB
u�B
u�B
u�B
v+B
v+B
vFB
v`B
v`B
v`B
vzB
v�B
v�B
wB
wB
w2B
w2B
wLB
w2B
w2B
wLB
w�B
w�B
xB
xRB
xlB
x�B
x�B
x�B
x�B
yXB
yXB
yXB
y�B
y�B
y�B
y�B
zDB
z�B
z�B
z�B
{JB
{B
{�B
{�B
{�B
{�B
{�B
|B
|B
|B
{�B
{�B
|B
|�B
|�B
|�B
|�B
}�B
~B
}�B
~B
}�B
~(B
~(B
~]B
~�B
~�B
~�B
~�B
B
B
B
.B
.B
HB
.B
.B
B
�B
�OB
�4B
�4B
�OB
��B
�B
��B
��B
�;B
�B
�'B
�[B
��B
��B
�B
�-B
�aB
�{B
�{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B�B�B��B��B�;B��B��B��B��B�AB��B��B��B�+B��B	aB	�B	pB	)�B	,�B	/�B	CGB	YB	o B	��B
�B
49B
��B
�B
��B
��B
��B
��B
��B
�(B�B"NB;�BDMBKDBM�BOvBTBV�B_VBj�Bv`B� B�0B�B��B�fB�eB�GBѷB��B�B��B�B
�B�BOB"NB%�B)yB�BEBTBbB�B
XB	lB�B��B�vB�B�aB��B��B��B��B�7B�aB\xB#�B�B�B
�B
�B
�XB
.B
qB
Q�B
)_B
VB	ڠB	�jB	��B	��B	p�B	cnB	W�B	K�B	G�B	C�B	?�B	-�B	 vB	�B	[B	SB	 �B�HB�B�qB	 �B	�B	UB	B	B	 �B	�B	�B	�B	B	sB	&�B	&�B	,�B	0�B	=�B	A�B	E�B	H�B	J�B	O\B	TFB	W�B	Z�B	\�B	b�B	h>B	nB	sB	s�B	uB	u�B	s�B	qvB	qB	sMB	w�B	|jB	}VB	vB	raB	m�B	poB	o�B	q'B	r-B	v�B	w�B	y$B	|6B	�B	� B	�oB	�SB	��B	�}B	�4B	��B	�vB	� B	�hB	��B	�MB	��B	�B	��B	��B	�$B	�?B	�sB	�sB	�sB	�sB	��B	��B	��B	�eB	��B	��B	�B	�IB	�5B	��B	�TB	�FB	��B	�B	��B	��B	�UB	�B	�fB	�*B	�B	��B	��B	�B	�"B	�VB	�]B	��B	��B	�tB	�RB	�=B	ˬB	οB	�B	� B	�&B	�B	�gB	�B	֡B	�$B	�
B	��B	רB	�yB	׍B	�yB	�_B	�eB	�B	��B	�B	��B	�)B	��B	�)B	�/B	��B	߾B	�B	��B	�B	��B	�tB	�B	�DB	�B	�sB	��B	��B	�
B	�B	�sB	�$B	��B	�B	�B	�B	�'B	�oB	��B	��B	��B	�B	��B	�/B	�/B	�B	��B	��B	�wB	�wB	�CB	�B	�]B	�wB	��B	�cB	�B	�B	�B	�AB	�AB	�B	��B	�B	�|B	��B	��B	��B	�-B	�GB	�B	�B	��B	��B	��B	��B	�AB	��B	�vB	�B	�B	�B	��B	�B	�ZB	�tB	��B	��B	��B	�FB	�FB	��B	��B	�fB	�B	��B	�>B	��B	��B	��B	��B	��B	�B	�^B	�xB	��B	��B	��B	��B	��B	��B	�JB	�B	�0B	�B	��B	�B	�B	�B	�B	�(B	�wB	��B	��B	��B	�.B	�}B	�cB	�cB	��B
 4B
 iB
 �B
 �B
B
 �B
oB
�B
B
B
�B
�B
aB
B
9B
�B
�B
�B
?B
+B
EB
_B
�B
�B
KB
�B
	B
	�B
	�B
	�B
	�B
	�B

	B

	B

XB

rB

�B

rB

�B
B
)B
�B
B
<B
�B
BB
�B
�B
HB
�B
B
HB
HB
B
 B
B
�B
�B
�B
B
B
�B
�B
�B
�B
B
{B
�B
�B
�B
B
�B
9B
mB
�B
�B
�B
�B

B
YB
YB
�B
+B
EB
EB
EB
_B
EB
�B
B
�B
kB
�B
QB
	B
�B
qB
B
CB
CB
xB
�B
�B
B
�B
�B
�B
B
jB
!B
 B
 'B
 �B
!-B
!�B
!�B
"NB
"NB
"hB
"NB
"hB
"B
"4B
"4B
"NB
"4B
"B
"4B
"NB
"NB
"�B
"�B
"�B
#B
$&B
$�B
$�B
$�B
$�B
%FB
%FB
%�B
&�B
&�B
&�B
&�B
&�B
'B
'RB
'�B
(
B
(>B
(>B
(�B
(�B
(�B
(�B
(�B
)yB
)�B
*B
*B
*KB
*�B
+QB
+�B
+�B
+�B
+�B
+�B
,"B
,"B
,=B
,qB
,�B
-]B
-�B
./B
.�B
/OB
/iB
/iB
/�B
/�B
/�B
/�B
/�B
0B
0B
/�B
0B
0UB
0oB
0�B
1'B
1AB
1AB
1�B
1�B
1�B
2�B
2�B
3MB
3B
3�B
4nB
4�B
4�B
4�B
5%B
5%B
5�B
5�B
6zB
6zB
6`B
6+B
6FB
6zB
7B
6�B
6�B
72B
8B
8�B
8�B
9>B
9XB
9>B
9>B
9$B
9�B
9�B
9�B
:*B
9�B
:*B
:�B
;�B
<jB
<�B
=VB
=qB
=�B
=�B
=�B
>B
>BB
>]B
>�B
>�B
>�B
?HB
?cB
?.B
?HB
?}B
?�B
?�B
?�B
?�B
@ B
?�B
@B
@�B
@�B
@�B
@iB
@4B
@OB
AoB
A�B
A�B
BAB
B�B
B�B
B�B
C-B
CaB
CGB
C{B
C�B
DgB
D�B
D�B
E9B
E�B
E�B
E�B
F?B
FYB
F�B
G+B
G_B
GEB
G_B
GzB
G�B
G�B
HB
IlB
J	B
J�B
K^B
K�B
K�B
LdB
L�B
MB
MPB
M�B
M�B
N"B
N<B
NpB
N�B
O�B
P.B
P}B
P�B
Q B
QNB
R:B
R�B
S&B
SuB
S�B
S�B
S�B
TFB
T�B
T�B
T�B
T�B
T�B
U2B
UgB
UgB
UgB
U�B
U�B
U�B
VB
U�B
VmB
V�B
V�B
W
B
W$B
W$B
W?B
WsB
W�B
WsB
W�B
W�B
W�B
W�B
W�B
X+B
X+B
X_B
X_B
X_B
X_B
XyB
X_B
X_B
X_B
YKB
YKB
YB
Y�B
Y�B
Y�B
ZB
ZB
Z�B
Z�B
[	B
[	B
[	B
[=B
[WB
[qB
[�B
[�B
[�B
[�B
\B
\]B
\�B
\�B
\�B
]IB
^B
^�B
^jB
^jB
^�B
^�B
^�B
_!B
_VB
_�B
`BB
`B
`\B
`vB
`vB
`�B
`�B
`�B
`�B
`�B
aHB
a|B
a�B
a�B
a�B
a�B
b4B
bNB
bhB
b4B
bNB
b�B
b�B
b�B
b�B
b�B
c B
cTB
cTB
c�B
c�B
d@B
dZB
d�B
ezB
e�B
e�B
fLB
f�B
f�B
f�B
gB
gB
g8B
gRB
gmB
gRB
g�B
g�B
g�B
g�B
g�B
g�B
h
B
h
B
h$B
hXB
h�B
h�B
h�B
h�B
i*B
iDB
iyB
i�B
iyB
iyB
i�B
j0B
j�B
jB
j�B
kB
k6B
k�B
k�B
l"B
l"B
l=B
lWB
lqB
lqB
lqB
l�B
l�B
l�B
l�B
m)B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
n}B
n}B
n�B
oOB
oOB
oiB
o�B
o�B
p;B
p�B
p�B
p�B
qB
p�B
p�B
q'B
q'B
qvB
q�B
q�B
q�B
q�B
rB
rGB
r�B
sMB
shB
s�B
s�B
s�B
s�B
tB
s�B
s�B
tB
tB
tB
tB
tB
tB
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
vB
v+B
v+B
v`B
v`B
v`B
vzB
v�B
v�B
v�B
v�B
wB
w2B
w2B
wLB
wLB
wLB
w�B
w�B
w�B
xB
xlB
x�B
x�B
x�B
x�B
x�B
yXB
yrB
yrB
y�B
y�B
y�B
zB
z^B
z�B
z�B
z�B
{dB
{�B
{B
{�B
{�B
{�B
{�B
|B
|B
{�B
{�B
|B
|PB
|�B
|�B
|�B
|�B
}�B
~B
}�B
~B
~B
~(B
~BB
~wB
~�B
~�B
~�B
B
B
B
B
.B
.B
.B
B
B
.B
�B
�OB
�4B
�OB
�iB
��B
�B
��B
��B
�;B
�B
�AB
�[B
�uB
��B
�-B
�GB
�GB
�aB
�{311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.05(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201905140043472019051400434720190514004347202207271131002022072711310020220727113100202207271533442022072715334420220727153344  JA  ARFMdecpA30a                                                                20190523095841  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190523100030  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190523100030  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190523100031  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190523100031  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20190523100031                      G�O�G�O�G�O�                JA  ARUP                                                                        20190523111516                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190504000000  CF  PSAL_ADJUSTED_QC@z�@z�G�O�                JM  ARCAJMQC2.0                                                                 20190513154347  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190513154347  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023100  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063344  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818081507                      G�O�G�O�G�O�                