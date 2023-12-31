CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-05-23T10:00:23Z creation;2019-05-23T10:00:25Z conversion to V3.1;2022-08-02T05:12:50Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190523100023  20220818081507  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_006                    2C  D   APEX                            8420                            2.11.2                          846 @س0�c�1   @س1Er�@+P|�����d����r1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @33@�  @�  A   A   AA��A`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BJ  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B���B���B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�ffB�33B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C�C�C  C  C  C�fC   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB�CD�CF33CH  CI��CK�fCN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv�Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2fD2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DFfDF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D��3D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@���@�=qA�A ��AB�RA`Q�A�=qA�ffA�z�A��\A��\A���A�(�A�RB   B33BG�B33B =qB(�B033B833B@(�BJp�BP{BX\)B`(�Bh=qBp(�Bx33B��B��B�{B�#�B��B��B�.B�33B�33B��B��B�=qB���B��
B�\B�#�B�#�B�33B��B��B��B�  B�B�(�B�.B��B�k�B�W
B�B�
=B��B�#�C \C
=C
=C�C�C

=C\C�C{C
C&fC(�C�C{C�C�RC C"\C$
=C&C(
=C*
C,�C.�C0�C2�C4
=C6�C8�C:{C<
C>\C@{CB)CD)CFO\CH�CI�HCL  CN�CP
=CR�CT\CV\CX�CZ{C\
C^�C`�Cb\Cd
Cf
Ch�Cj
Cl�Cn
=Cp
=Cr�Ct�Cv(�Cx\Cz\C|\C~\C��C�fC�C�C�fC�
=C��C��C��C�fC��C��C��C�C�fC�
=C��C��C��C��C��C��C��C��C��C��C��C�fC�
=C�C��C��C�
=C�fC�
=C�fC�C��C��C��C��C��C�C��C��C��C��C��C�fC�C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�C�fC�fC�fC��C��C��C��C��C��C�C�HC�fC�
=C�
=C��C�fC�
=C��C��C��C�C��C��C�fC�fC��C��C��C��C��C�C��C��C��C�fC��C��C�fC��C��C��C��C��C�fC�
=C�
=C��C�C��C�fC�fC��C�
=C��C��C��C�fC�fC�fC�fC��C��C��D {D ��D{D��D�D�{D3D��DD�D�D�3D�D��D�D�D�D�fD	fD	�{D
{D
��D�D�3D3D��D{D�3D�D��D�D��D�D��D3D��D3D�3D3D��D�D��D{D�3D�D�D{D�D{D��D3D��D3D�3D�D�{DD�3D3D��D{D�D{D�3D �D �{D!D!��D"�D"��D#3D#�{D$�D$��D%D%��D& �D&��D'fD'�{D(�D(��D)D)��D*�D*�3D+{D+�
D,fD,�{D-�D-��D.fD.�D/{D/�{D0{D0�D1�D1�
D2�D2�
D3
D3�
D4fD4�{D5{D5�D6�D6��D7{D7��D8�D8��D9�D9�D:{D:�{D;�D;��D<3D<�D=3D=��D>{D>�{D?3D?��D@�D@��DA�DA�3DBDB�3DC�DC�{DD�DD��DEDE�fDF�DF�fDG�DG��DH�DH��DI�DI��DJ�DJ�3DK3DK�DLDL�3DM�DM��DN�DN��DO3DO�3DP3DP��DQ3DQ�{DR�DR��DS�DS�3DT{DT�DUDU��DV�DV��DWDW�DX�DX�3DY�DY�{DZ{DZ��D[D[�D\D\�fD]{D]��D^D^�D_3D_�{D`3D`��Da3Da��Db�Db��Dc�Dc��DdHDd��DeDe�HDf�Df��Dg�Dg�{Dh�Dh�{Di�Di��Dj�Dj��Dk3Dk�{DlDl�{Dm{Dm�{Dn�Dn�fDo�Do��Dp�Dp��DqDq��DrHDr�3Ds�Ds��Dt�Dt��Du{Du�{DvDv�fDw�Dw��DxHDx��Dy�Dy��Dz{Dz��D{�D{�{D|{D|�{D}�D}�HD~HD~�HDHD��D� RD�A�D��3D�D�=D�B�D���D�D�=D�A�D��=D��=D��D�B=D���D��HD�HD�A�D��HD��HD� �D�A�D���D��=D�=D�A�D��HD��=D�=D�A�D��HD���D�=D�A�D���D��HD��D�B=D��=D���D�=D�A�D���D��HD��D�B�D���D���D��D�@�D���D���D�HD�A�D���D���D��D�B=D���D��=D��D�A�D���D�D��D�B=D��=D���D�HD�B�D���D�D�3D�B=D���D���D��D�B�D���D���D��D�B�D��=D��=D��D�AHD���D���D�=D�A�D���D���D��D�B�D��=D��=D��D�B=D���D���D�=D�AHD��HD���D��D�A�D���D��RD� �D�@�D���D���D��D�A�D��=D��3D��D�B�D���D���D�=D�B�D���D�D��D�B=D��=D���D��D�A�D��HD��HD�HD�A�D���D��HD� �D�B=D���D���D��D�B=D���D�D�3D�B�D���D���D��D�A�D��=D���D� �D�A�D���D���D�HD�B�D���D���D�HD�@�D���D�D�HD�@RD���D��=D�=D�B�D��3D�D��D�B=D���D��HD�=D�A�D��HD�D��D�B=D��=D���D��D�@�D��=D��3D��D�B=D��=D��=D�=D�B=D���D���D��D�B�D���D��RD�HD�B=D���D���D��D�A�D��HD���D��D�A�D���D��=D�3D�C3D��=D���D��D�C3D���D�D��D�A�D���D�D��D�A�D���D���D��D�B�D���D��=D��D�A�D���D���D�=D�A�D��=D���D��D�A�D���D���D�HD�AHD���D��RD� �D�A�D��=D��HD� �D�A�D��3D��=D�=D�B�D��=D���D��D�B�D��HD���D�3D�AHDHD���D� �D�A�DÂ=D��HD�=D�C3Dă3D���D� �D�@�DŁ�D���D��D�A�DƁ�D��HD� RD�AHDǂ=D���D��D�@�DȁHD��=D�=D�A�DɁHD��HD� �D�A�Dʁ�D��=D�=D�A�D˂=D���D��D�A�D́�D��=D��D�@�D́HD���D��D�AHD΀RD��HD�HD�@RDρHD�D��D�@�DЁHD���D� �D�@�DсHD��=D�=D�AHD҂�D���D��D�A�DӀ�D���D��D�@�Dԁ�D���D�HD�B�DՂ�D��HD��D�C3Dւ=D��HD�=D�A�Dׁ�D��=D� �D�A�D؃3D���D��D�A�DفHD�D�=D�A�Dڃ3D��3D��D�A�Dہ�D���D� �D�AHD܁�D���D�HD�@�D݂=D�ÅD�=D�B�Dރ3D���D��D�AHD߀�D���D�HD�AHD��HD���D��D�B�DႏD���D� �D�A�D⁚D��HD��D�A�DずD��HD��D�A�D�=D��=D�=D�A�D��D���D�HD�A�D悏D���D��D�AHD��D���D��D�A�D�=D���D��D�A�D�=D�D��D�@�DꁚD�D��D�B�D��D���D�=D�A�D��D��=D�=D�A�D��D���D��D�AHDD��HD�=D�A�D�=D�D��D�A�D��=D���D��D�A�D��D�D�3D�B�D�D�D�=D�A�D�HD��=D�=D�@�D��D��=D�=D�A�D���D���D� �D�@�D��HD�D��D�A�D���D���D�HD�@�D���D���D��D�A�D��HD��HD� �D�A�D���D��=D��D�AHD��HD���D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�$A��A�A� 'A�#A�&�A�&�A�*�A�2�A�4�A�6�A�;�A�@�A�>BA�=qA�1�A�SA��A��Aϛ	A�	A��mAΆ%A�R�A�cTA���A̾�A�y	A���A�wfA�5tA��Aț=A�	A�1A��Aǝ�A�FA���AƞOA��Aŗ$A�[�A�J�Aľ�AĆ�A�GA�-�A��zAèXA�a|A�GEA�	�A��A�gA���A���A�o5A���A���A�A�A��{A�poA���A��A���A�3hA��RA��YA� �A�  A���A��*A���A��A��A��cA�*�A���A�GzA��A���A�c�A�.A��3A�e`A���A��A���A��A�B'A�^�A�t�A�l�A���A��FA�o�A�2-A~x�Az��Ar(�Am-wAi�NAg iAe$�Ac�A]�AZ�|AY�eAY�AX@�AS��AM�AH?}AG@AB�1A>*0A:f�A7�pA6h
A5�MA4H�A4�A3��A2�A1� A0��A0-wA/�XA/B[A..IA,�
A+�A*��A*�A(_�A'A$-wA"dZA!��A ��A�rAY�A�QAg�AB[A�A4nA��AOA��A�aA�A�XA��A�OA��A}�A<6A�2A�hAT�A7A��A�A��AF�A�0A��A�WA�DAN�A!-A�A�'A{A
i�A	خA	��A	:�A��A�2A<6A��A��A��An/A%�A�Ah�A:*A�A�NA��A�kAq�A-wA�2A�Al�A1'A�A��A��Ae�A �A p�A �@���@�@O@�#�@��@��j@���@�C-@��Q@�F�@���@��@�@O@���@�_�@� \@���@���@�ݘ@��@��I@�[@�@��@��]@�O@���@��@��@��@���@�N�@� \@��|@��)@�$@�@�L@�~�@���@�7@�o�@�*0@�l"@릵@��v@��@�_@�@�4@�e,@�F�@��`@�J�@�F@�x@�Ĝ@�=@��@��@�5�@��?@�c @��z@�U�@ޟ�@�?@��@�ƨ@�[W@��@�͟@�A�@�"h@�qv@ر�@��.@׿H@�|�@�B�@���@֕@�  @���@��@Ҿ@��@��H@�!@�K�@�c @�
=@�ߤ@̬@�6�@�	@��@�X�@ʌ@ɯ�@ȕ@Ǚ�@�U�@ƸR@ńM@��@āo@��@���@�a|@�	@��]@���@��@���@��$@�	l@���@��@@�k�@���@�Ta@�9X@��@���@�+�@���@�5?@�f�@��@��b@���@�v�@�L0@�  @���@�}�@�Mj@�'�@��	@�	�@�ȴ@�bN@�M�@�3�@���@��	@��@���@���@��A@�-�@��@���@�1�@��X@�4n@���@���@�F@��I@�~(@�u%@�a|@�1'@��k@��9@�2�@���@� \@���@�� @�*0@��O@�:*@���@�2a@��@�ߤ@���@�?�@�@�B�@�ߤ@�\�@��3@�W?@��j@�?@��A@���@�@O@�o@��@�z@��@�9�@�@��f@�u%@�V@�D�@�-�@�_@��@�ԕ@��q@�Z�@���@�c�@��@���@�IR@��@��/@�Ɇ@��!@�m�@�\�@�Ta@�A�@�'R@��C@�J�@�Y@��@��?@���@�Z�@���@�H�@�+�@��@��U@��D@�r�@�h
@�M�@�6�@�$�@��D@�ݘ@��h@���@�e�@�G�@�H�@��@��@��5@�Ɇ@��9@�ff@� �@��q@�(�@���@��@�֡@��@��!@��6@��F@�g8@�GE@�&�@��m@��'@�y�@�b�@�B�@��|@�u�@�	@��@���@��t@�s�@�B�@�"�@��@��@�M�@��@��Z@���@�H�@���@��,@���@�1�@��@���@���@��P@�l�@�_p@�\)@�<6@��@��m@�S�@��@��@��~@�hs@�H�@��@�_@��@���@���@���@���@��~@�|�@�C@��!@�-�@�x@��@��>@��k@��`@�K^@���@���@�zx@�Q�@�8�@�"�@�@��@��u@�U2@���@��@�q�@���@��$@��K@�4n@� @��@�:@|�@ i@~�m@~Z�@}��@}@@{��@{C�@z��@z��@z��@zC�@z �@y�@yY�@x�p@xy>@w�@v_�@vR�@vYK@vOv@u��@t�O@t1@sƨ@r��@r�@qc@q`B@qJ�@q?}@q<6@q*0@p�f@p�@o�A@o�	@o8@o�@n�@n��@m��@m�X@l�P@ly>@l�@k��@k/�@j��@i�@g��@g�@g+@f�@e^�@dPH@c��@c��@c>�@c�@b��@b�r@bv�@bW�@bL0@b)�@a��@aIR@`�[@`�$@`Q�@`@_�A@_�V@_W?@_/�@^��@^�@^R�@]}�@\��@\�@\��@\�u@\[�@\D�@\ �@[��@[C�@Z��@Z��@Z~�@ZQ@Y�)@Yhs@YT�@Y2a@Y@X�|@X�)@Xq@W��@Wqv@WJ#@WY@V�<@Vd�@Uu�@T��@T��@T��@T��@T��@T��@T�o@TH@T!@S�@Sb�@S@R��@R�1@RO@Q�@Q�=@Q�@Qhs@Q�@PɆ@P�D@Oƨ@OMj@O
=@N�@N�x@N8�@M�#@M��@M%@L�U@Ly>@L�@K�@KC�@K)_@K�@J�@J�@J��@J��@Ja|@J@�@J	@I��@Izx@Iu�@IJ�@H�P@H�e@HA�@G�@Ge�@G=@G�@Fȴ@Fu%@F�@E��@E��@E�M@E:�@D��@D��@D/�@C�@C��@C~�@C�@B^5@A��@@��@@M@@6@@1@?Mj@>��@>v�@>5?@=�3@=V@=V@<�?@<Q�@<I�@;خ@;�@:u@8�f@8��@8w�@8c�@8N�@8I�@82�@7ݘ@7)_@6�@6s�@6Ov@6#:@5��@5#�@4 �@3H�@3�@2��@2�@2n�@2kQ@2d�@2=q@1��@17L@0��@0[�@0x@/�Q@/�:@/iD@/dZ@/]�@/]�@/U�@/8@.�X@.��@.l�@.Ta@.#:@-��@-�T@-��@-B�@,�@,�v@,�@,�/@,�[@,��@,��@,S�@+�w@+��@+�P@+n/@*�@*�h@*��@*l�@*1�@)�o@)Y�@) \@)�@(�z@(�o@(~(@(Z@(7�@(1'@(*�@(x@'��@'��@'�@'��@'�f@'~�@'t�@'P�@'"�@&��@&��@&��@&�A@&E�@&0U@&!�@&#:@&@%ϫ@%x�@%p�@%`B@%^�@%T�@%7L@%;@$�E@$Ĝ@$�@$r�@$4n@$�@#�@#�@#��@#ƨ@#��@#��@#@O@#
=@"�@"�@"�1@"�r@"�+@"��@"��@"~�@"C�@"�@" �@!�@!�H@!�@!w2@!(�@!�@ �|@ �[@ ��@ �.@ |�@ c�@ ~@ƨ@�*@��@�$@��@�f@~�@]�@�@��@�@��@��@^5@:*@)�@$�@�@ϫ@\�@q@�f@��@�@��@_@�@�[@��@�@F�@�@�r@($@!�@@�.@��@�"@@@��@�_@�.@��@�@[�@?�@"h@�+@�W@j�@8@@@��@��@.�@�@�@�S@��@}�@c�@G�@ \@�5@�[@��@��@�.@g8@Xy@Ft@6@@�@��@o�@/�@�@�@��@�@��@	@��@�j@�@��@�^@�S@��@hs@O�@�@��@w�@q@bN@9X@@�]@�W@�K@�q@l�@,�@��@�R@�+@s�@Z�@6�@($@	@u@�D@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�$A��A�A� 'A�#A�&�A�&�A�*�A�2�A�4�A�6�A�;�A�@�A�>BA�=qA�1�A�SA��A��Aϛ	A�	A��mAΆ%A�R�A�cTA���A̾�A�y	A���A�wfA�5tA��Aț=A�	A�1A��Aǝ�A�FA���AƞOA��Aŗ$A�[�A�J�Aľ�AĆ�A�GA�-�A��zAèXA�a|A�GEA�	�A��A�gA���A���A�o5A���A���A�A�A��{A�poA���A��A���A�3hA��RA��YA� �A�  A���A��*A���A��A��A��cA�*�A���A�GzA��A���A�c�A�.A��3A�e`A���A��A���A��A�B'A�^�A�t�A�l�A���A��FA�o�A�2-A~x�Az��Ar(�Am-wAi�NAg iAe$�Ac�A]�AZ�|AY�eAY�AX@�AS��AM�AH?}AG@AB�1A>*0A:f�A7�pA6h
A5�MA4H�A4�A3��A2�A1� A0��A0-wA/�XA/B[A..IA,�
A+�A*��A*�A(_�A'A$-wA"dZA!��A ��A�rAY�A�QAg�AB[A�A4nA��AOA��A�aA�A�XA��A�OA��A}�A<6A�2A�hAT�A7A��A�A��AF�A�0A��A�WA�DAN�A!-A�A�'A{A
i�A	خA	��A	:�A��A�2A<6A��A��A��An/A%�A�Ah�A:*A�A�NA��A�kAq�A-wA�2A�Al�A1'A�A��A��Ae�A �A p�A �@���@�@O@�#�@��@��j@���@�C-@��Q@�F�@���@��@�@O@���@�_�@� \@���@���@�ݘ@��@��I@�[@�@��@��]@�O@���@��@��@��@���@�N�@� \@��|@��)@�$@�@�L@�~�@���@�7@�o�@�*0@�l"@릵@��v@��@�_@�@�4@�e,@�F�@��`@�J�@�F@�x@�Ĝ@�=@��@��@�5�@��?@�c @��z@�U�@ޟ�@�?@��@�ƨ@�[W@��@�͟@�A�@�"h@�qv@ر�@��.@׿H@�|�@�B�@���@֕@�  @���@��@Ҿ@��@��H@�!@�K�@�c @�
=@�ߤ@̬@�6�@�	@��@�X�@ʌ@ɯ�@ȕ@Ǚ�@�U�@ƸR@ńM@��@āo@��@���@�a|@�	@��]@���@��@���@��$@�	l@���@��@@�k�@���@�Ta@�9X@��@���@�+�@���@�5?@�f�@��@��b@���@�v�@�L0@�  @���@�}�@�Mj@�'�@��	@�	�@�ȴ@�bN@�M�@�3�@���@��	@��@���@���@��A@�-�@��@���@�1�@��X@�4n@���@���@�F@��I@�~(@�u%@�a|@�1'@��k@��9@�2�@���@� \@���@�� @�*0@��O@�:*@���@�2a@��@�ߤ@���@�?�@�@�B�@�ߤ@�\�@��3@�W?@��j@�?@��A@���@�@O@�o@��@�z@��@�9�@�@��f@�u%@�V@�D�@�-�@�_@��@�ԕ@��q@�Z�@���@�c�@��@���@�IR@��@��/@�Ɇ@��!@�m�@�\�@�Ta@�A�@�'R@��C@�J�@�Y@��@��?@���@�Z�@���@�H�@�+�@��@��U@��D@�r�@�h
@�M�@�6�@�$�@��D@�ݘ@��h@���@�e�@�G�@�H�@��@��@��5@�Ɇ@��9@�ff@� �@��q@�(�@���@��@�֡@��@��!@��6@��F@�g8@�GE@�&�@��m@��'@�y�@�b�@�B�@��|@�u�@�	@��@���@��t@�s�@�B�@�"�@��@��@�M�@��@��Z@���@�H�@���@��,@���@�1�@��@���@���@��P@�l�@�_p@�\)@�<6@��@��m@�S�@��@��@��~@�hs@�H�@��@�_@��@���@���@���@���@��~@�|�@�C@��!@�-�@�x@��@��>@��k@��`@�K^@���@���@�zx@�Q�@�8�@�"�@�@��@��u@�U2@���@��@�q�@���@��$@��K@�4n@� @��@�:@|�@ i@~�m@~Z�@}��@}@@{��@{C�@z��@z��@z��@zC�@z �@y�@yY�@x�p@xy>@w�@v_�@vR�@vYK@vOv@u��@t�O@t1@sƨ@r��@r�@qc@q`B@qJ�@q?}@q<6@q*0@p�f@p�@o�A@o�	@o8@o�@n�@n��@m��@m�X@l�P@ly>@l�@k��@k/�@j��@i�@g��@g�@g+@f�@e^�@dPH@c��@c��@c>�@c�@b��@b�r@bv�@bW�@bL0@b)�@a��@aIR@`�[@`�$@`Q�@`@_�A@_�V@_W?@_/�@^��@^�@^R�@]}�@\��@\�@\��@\�u@\[�@\D�@\ �@[��@[C�@Z��@Z��@Z~�@ZQ@Y�)@Yhs@YT�@Y2a@Y@X�|@X�)@Xq@W��@Wqv@WJ#@WY@V�<@Vd�@Uu�@T��@T��@T��@T��@T��@T��@T�o@TH@T!@S�@Sb�@S@R��@R�1@RO@Q�@Q�=@Q�@Qhs@Q�@PɆ@P�D@Oƨ@OMj@O
=@N�@N�x@N8�@M�#@M��@M%@L�U@Ly>@L�@K�@KC�@K)_@K�@J�@J�@J��@J��@Ja|@J@�@J	@I��@Izx@Iu�@IJ�@H�P@H�e@HA�@G�@Ge�@G=@G�@Fȴ@Fu%@F�@E��@E��@E�M@E:�@D��@D��@D/�@C�@C��@C~�@C�@B^5@A��@@��@@M@@6@@1@?Mj@>��@>v�@>5?@=�3@=V@=V@<�?@<Q�@<I�@;خ@;�@:u@8�f@8��@8w�@8c�@8N�@8I�@82�@7ݘ@7)_@6�@6s�@6Ov@6#:@5��@5#�@4 �@3H�@3�@2��@2�@2n�@2kQ@2d�@2=q@1��@17L@0��@0[�@0x@/�Q@/�:@/iD@/dZ@/]�@/]�@/U�@/8@.�X@.��@.l�@.Ta@.#:@-��@-�T@-��@-B�@,�@,�v@,�@,�/@,�[@,��@,��@,S�@+�w@+��@+�P@+n/@*�@*�h@*��@*l�@*1�@)�o@)Y�@) \@)�@(�z@(�o@(~(@(Z@(7�@(1'@(*�@(x@'��@'��@'�@'��@'�f@'~�@'t�@'P�@'"�@&��@&��@&��@&�A@&E�@&0U@&!�@&#:@&@%ϫ@%x�@%p�@%`B@%^�@%T�@%7L@%;@$�E@$Ĝ@$�@$r�@$4n@$�@#�@#�@#��@#ƨ@#��@#��@#@O@#
=@"�@"�@"�1@"�r@"�+@"��@"��@"~�@"C�@"�@" �@!�@!�H@!�@!w2@!(�@!�@ �|@ �[@ ��@ �.@ |�@ c�@ ~@ƨ@�*@��@�$@��@�f@~�@]�@�@��@�@��@��@^5@:*@)�@$�@�@ϫ@\�@q@�f@��@�@��@_@�@�[@��@�@F�@�@�r@($@!�@@�.@��@�"@@@��@�_@�.@��@�@[�@?�@"h@�+@�W@j�@8@@@��@��@.�@�@�@�S@��@}�@c�@G�@ \@�5@�[@��@��@�.@g8@Xy@Ft@6@@�@��@o�@/�@�@�@��@�@��@	@��@�j@�@��@�^@�S@��@hs@O�@�@��@w�@q@bN@9X@@�]@�W@�K@�q@l�@,�@��@�R@�+@s�@Z�@6�@($@	@u@�D@��@�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�YBƎB��B�EBȚB��B�7B�B�vB�HB�uB�-B�B�@B��B�]B��B�B�)B�$B	0!B	N<B	a�B	qvB	�B	ҽB	��B
rB
�#B
�JB
ϑB
��B
��B�B�B�BBBOB#�B*B3�B=qBBBJrBRTB\�B^�Bh
BjKBp�Bt�B� B�FB�FBżBοB�0B�BqB�B#�B%,B$&B$&B%B&fB$ZB!|B	B:B9B��B�B�B�\B�dB��B��B��B}�Bk�BC�B�BUB
��B
�oB
�4B
��B
�B
fLB
I7B
>(B
0oB
'mB
#:B
�B
B
(B	�hB	��B	��B	�B	l�B	_!B	Q�B	6�B	"B	�B	SB	 B��B�B��B�DB��B��B��B�B�tB�B��B��B��B�<B�,B��B��B�ZB�B�B	B	�B	{B	�B	$�B	'B	*KB	/5B	7fB	8RB	8�B	9�B	9�B	8�B	8B	=B	D�B	Z�B	u�B	�'B	�YB	�B	�pB	��B	�9B	�tB	��B	�=B	�JB	��B	οB	�BB	�B	��B	��B	�JB	̘B	��B	�9B	��B	��B	�B	�?B	��B	��B	��B	�PB	�B	�OB	�[B	ðB	��B	żB	�B	��B	ƨB	�%B	�B	�%B	�tB	ƎB	��B	��B	��B	�B	��B	ȚB	�B	�B	�dB	�B	�"B	�\B	бB	�FB	ԕB	��B	��B	�+B	��B	�B	ؓB	��B	�eB	��B	�/B	�B	��B	� B	��B	�B	�B	��B	�#B	�WB	�)B	��B	�7B	�WB	�CB	�B	�B	�B	�8B	�*B	�QB	�)B	��B	��B	��B	�B	�B	��B	�UB	�B	�B	��B	�B	��B	�B	�+B	�tB	��B	�B	�GB	�B	��B	�B	�[B	�[B	��B	��B	�oB	�UB	�B	��B	��B	�UB	��B	�AB	�B	�B	�!B	�!B	�B	�;B	�B	�OB	��B	�5B	�}B	�B	�B	�UB	�'B	�B	�B	�UB	�B	�OB	��B	�B	�AB	�B	�B	�9B	��B	�B	��B	��B	�B	�FB	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�*B	�*B	��B	�B	�*B	��B	�dB	�]B	��B	�wB	�(B	�]B	�qB	�<B	�<B	�<B	�BB	�wB	��B
 B
 �B
 �B
 �B
 �B
 �B
B
 B
 �B
 B
 �B
 �B
UB
-B
[B
[B
uB
�B
B
�B
�B
�B
�B
gB
�B
�B
B
B
%B
%B
tB
�B
�B
�B
�B
�B
�B
fB
	B
	lB
	7B

�B

rB
dB
�B
jB
�B
�B
�B
�B
�B
�B
�B
�B
�B
hB
�B
uB
�B
�B
�B
2B
B
B
B
�B
B
YB
�B
sB
?B
�B
EB
�B
�B
�B
�B
�B
1B
eB
�B
kB
�B
=B
)B
�B
�B
�B
�B
IB
/B
/B
�B
�B
�B
�B
�B
�B
�B
�B
�B
VB
B
�B
;B
VB
�B
�B
�B
�B
�B
�B
�B
 'B
 'B
 \B
 vB
 �B
 \B
 �B
 �B
 vB
 �B
 vB
 �B
 �B
!|B
!|B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"hB
"4B
"NB
"�B
"�B
"�B
"�B
"�B
#:B
$&B
$B
$tB
$ZB
$tB
$�B
$�B
$�B
%B
%`B
%�B
%�B
%�B
&2B
&fB
&�B
&�B
'B
'�B
'�B
'�B
'�B
($B
(>B
(>B
($B
(XB
(XB
(�B
)�B
*eB
*KB
*�B
*B
*�B
+�B
,qB
,�B
-)B
-CB
-wB
-�B
-�B
-�B
./B
.}B
/5B
/5B
/5B
/B
/5B
/�B
0�B
1B
1AB
1'B
1AB
1vB
1vB
1[B
1vB
1�B
1�B
2GB
2�B
3�B
4B
3�B
4�B
5�B
6+B
6FB
6B
6+B
6`B
6FB
6�B
6�B
7B
7�B
88B
8lB
8�B
8lB
8�B
8�B
8�B
9	B
9$B
9$B
9�B
:^B
:*B
9�B
9�B
:DB
:�B
;B
;0B
;�B
<PB
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
=qB
=�B
=�B
=�B
=�B
=�B
>wB
>�B
>�B
?.B
?}B
?�B
?�B
@ B
@OB
A�B
A�B
A�B
BAB
B�B
CaB
C{B
C�B
C�B
C�B
C�B
DMB
D3B
DMB
DMB
DMB
DMB
D�B
E9B
E9B
E�B
E�B
E�B
FtB
F�B
F�B
F�B
F�B
GEB
G�B
G�B
G�B
G�B
HB
H1B
H1B
H1B
H�B
IB
IB
I�B
I�B
I�B
I�B
J=B
J=B
JXB
JrB
J=B
J=B
J�B
J�B
KB
K)B
K^B
KxB
K^B
K�B
LB
LB
LB
LdB
L~B
LdB
L�B
L�B
L�B
MPB
M�B
M�B
M�B
M�B
NVB
N�B
O(B
O(B
OBB
O�B
O�B
O�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
QB
Q�B
Q�B
Q�B
RB
R�B
R�B
R�B
R�B
R�B
R�B
S&B
S[B
SuB
S[B
S�B
T,B
T,B
T,B
TFB
T�B
T�B
UB
U�B
U�B
U�B
V9B
VmB
V�B
W$B
WsB
WsB
WsB
W�B
W�B
W�B
XEB
X_B
XyB
X�B
X�B
YKB
YB
ZQB
Z�B
Z�B
Z�B
[�B
[�B
\)B
\�B
]B
]�B
]�B
^B
^�B
^OB
^�B
_!B
_�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
a�B
bB
bB
bB
bNB
bNB
b�B
cTB
c�B
c�B
c�B
dB
d&B
dB
c�B
d&B
d&B
d�B
eFB
e�B
e�B
e�B
ffB
ffB
fLB
ffB
fLB
fLB
f�B
f�B
f�B
gB
gB
g8B
g8B
gRB
gmB
g�B
h$B
h$B
h$B
h$B
h>B
h
B
h$B
h�B
h�B
iB
iB
iB
i�B
i�B
i�B
i�B
jB
j0B
j�B
j�B
j�B
kB
kB
kB
k6B
kQB
k6B
k6B
kkB
k�B
k�B
k�B
l"B
l"B
l=B
l=B
lWB
l�B
l�B
l�B
l�B
mB
mCB
m]B
mCB
m]B
mCB
m�B
m�B
m�B
m�B
m�B
m�B
m�B
nB
nB
n/B
ncB
n}B
n�B
n�B
n�B
n�B
oB
o B
o5B
o5B
o�B
o�B
o�B
pB
p;B
pUB
p;B
p;B
p!B
p;B
p�B
poB
p�B
p�B
p�B
p�B
q'B
q[B
qvB
q�B
q�B
q�B
q�B
q�B
q�B
r-B
r|B
r|B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
s3B
s3B
s3B
s3B
s�B
s�B
s�B
s�B
s�B
s�B
tB
tB
t9B
tnB
tnB
t�B
t�B
uB
uZB
u?B
u?B
u�B
u�B
vFB
v�B
v�B
v�B
v�B
v�B
v�B
w�B
xB
xB
xB
xB
xB
xRB
xRB
xRB
x�B
xlB
y$B
y$B
yXB
yXB
yXB
y�B
zDB
zDB
z�B
z�B
z�B
z�B
z�B
z�B
{0B
{dB
{B
{B
{B
{�B
{�B
{�B
|B
|6B
|�B
|�B
}"B
}�B
~B
~(B
~]B
~wB
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
B
~�B
�B
�B
� B
�B
� B
�B
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
��B
��B
�B
�B
�B
�;B
� B
�B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�tBƨB��B�_BȴB��B�7B�)B�vB�bB�uB�B�B�ZB�B��B�!B�B�B�xB	0�B	N�B	b�B	shB	��B	��B	�B
rGB
��B
��B
�.B
��B
��BBHB�B�B7B;B%B+B4B=�BC-BJ�BR�B\�B_�BhXBj�Bq[Bu�B��B��B��B�lB��B �B�B BB"�B%�B%�B$tB$�B&�B($B'RB%FB�BSBKB�qB�B�B��B�.B��B�PB�B�[Br�BI�B�B�B
��B
��B
�B
��B
�B
jeB
K�B
@OB
1�B
'�B
$B
!-B
VB
�B	��B	��B	��B	�-B	o�B	b�B	W?B	9�B	#�B	B	B	YB	B�KBөB��B�+B�jB�rB��B��B�VB�GB�gB�EBϑB�gBܬB�B�`B�0B�?B	mB	 B	B	B	'B	*eB	,WB	0oB	8�B	9>B	9rB	:�B	:�B	:�B	:^B	?}B	EmB	Y�B	utB	��B	�YB	��B	�'B	��B	�SB	ƨB	�1B	ʦB	̘B	�<B	�B	��B	��B	ϑB	ΊB	̈́B	��B	� B	��B	��B	��B	�FB	�+B	��B	��B	��B	��B	�wB	��B	��B	��B	żB	�tB	�_B	�+B	��B	ƨB	�tB	�tB	��B	��B	�+B	�B	�+B	�zB	�KB	��B	�lB	�DB	̘B	�6B	�pB	��B	ѝB	ԕB	�B	�$B	�B	�EB	�+B	�EB	خB	�1B	��B	�=B	�~B	ބB	�B	�B	�`B	�B	��B	ݘB	ۦB	ۦB	��B	�dB	ڠB	�qB	ܬB	�`B	�RB	�B	�B	�B	�kB	�B	�CB	�/B	�B	�B	�B	��B	��B	�B	�[B	��B	�TB	�FB	��B	��B	�`B	��B	�aB	�|B	�-B	��B	�[B	��B	��B	�AB	��B	�[B	��B	�!B	�oB	�B	�B	�[B	�B	�B	��B	�UB	�;B	�B	��B	�B	��B	��B	�B	� B	��B	��B	��B	�[B	�[B	�[B	��B	��B	�!B	�B	�'B	��B	�B	�B	��B	��B	�FB	��B	��B	�+B	�zB	�B	�LB	��B	��B	��B	�lB	�8B	�XB	�8B	��B	�^B	��B	��B	�^B	�DB	��B	�*B	�DB	��B	��B	��B	��B	��B	��B	�wB	��B	�qB	�qB	��B	��B	��B	�B
 iB
 �B
 �B
 �B
 �B
 �B
;B
UB
 B
;B
B
UB
B
GB
uB
uB
�B
GB
aB
B
�B
�B
�B
�B
�B
�B
mB
mB
YB
?B
�B
B
�B
�B
�B
B
KB
�B
	�B
	�B
	�B
B

�B
�B
B
�B
B
B
�B
�B
HB
�B
4B
NB
B
�B
B
�B
&B
FB
B
gB
gB
2B
MB
MB
�B
�B
�B
�B
sB
�B
EB
�B
�B
�B
�B
B
B
�B
B
�B
�B
qB
]B
�B
�B
�B
B
IB
/B
IB
B
B
B
B
B
B
B
B
OB
pB
B
!B
pB
�B
�B
�B
�B
�B
�B
�B
�B
 BB
 BB
 vB
 vB
 �B
 vB
 �B
 �B
 vB
 �B
 �B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"NB
"�B
"�B
"�B
"�B
"�B
#:B
#�B
$ZB
$&B
$tB
$ZB
$�B
$�B
$�B
%,B
%FB
%�B
%�B
%�B
%�B
&�B
&�B
&�B
&�B
'mB
'�B
'�B
'�B
'�B
(>B
(XB
(>B
($B
(�B
(�B
(�B
)�B
*B
*eB
*�B
*�B
+B
,"B
,�B
,�B
-CB
-]B
-�B
-�B
-�B
-�B
.}B
.�B
/OB
/OB
/OB
/iB
/�B
0!B
0�B
1'B
1[B
1AB
1[B
1�B
1�B
1vB
1�B
1�B
2-B
2�B
3MB
3�B
4TB
4nB
5ZB
5�B
6FB
6+B
6+B
6FB
6zB
6zB
6�B
6�B
7�B
7�B
88B
8�B
8�B
8�B
8�B
8�B
9	B
9>B
9XB
9XB
9�B
:DB
:*B
:B
:*B
:�B
;0B
;0B
;B
<B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
<�B
=qB
=�B
=�B
=�B
>B
>(B
>�B
>�B
?B
?cB
?�B
?�B
@ B
@iB
@�B
A�B
A�B
A�B
B�B
B�B
C{B
C�B
C�B
C�B
C�B
DB
DMB
D3B
DgB
DgB
DgB
DgB
EB
E9B
ESB
E�B
E�B
E�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
HB
H1B
H1B
H1B
HKB
H�B
I7B
I7B
I�B
I�B
I�B
J#B
JXB
JXB
JXB
JrB
JXB
JXB
J�B
J�B
KB
K)B
KxB
K�B
K�B
L0B
LB
LB
L0B
LdB
LdB
L~B
L�B
L�B
MB
MPB
M�B
M�B
M�B
N"B
NpB
N�B
OBB
OBB
O\B
O�B
O�B
PB
P�B
P�B
P�B
P�B
P�B
Q B
P�B
QNB
Q�B
Q�B
R B
R:B
R�B
R�B
R�B
SB
R�B
SB
S@B
SuB
S�B
SuB
S�B
T,B
T,B
TFB
TaB
T�B
T�B
U2B
U�B
U�B
U�B
V9B
VmB
V�B
W?B
W�B
W�B
W�B
W�B
XB
X+B
X_B
XyB
X�B
X�B
YKB
YeB
Y�B
ZkB
Z�B
[	B
[=B
[�B
\B
\CB
\�B
]IB
]�B
]�B
^OB
^�B
^�B
^�B
_pB
`B
`�B
`�B
`�B
`�B
`�B
`�B
`�B
a-B
a�B
b4B
b4B
b4B
b�B
b�B
b�B
c�B
c�B
c�B
c�B
dB
d&B
c�B
c�B
dZB
dZB
eB
e`B
e�B
e�B
fB
f�B
fLB
fLB
ffB
f2B
fLB
f�B
f�B
gB
gB
gB
g8B
gRB
gmB
g�B
g�B
h
B
h$B
h$B
h$B
h$B
h$B
hXB
h�B
iB
iB
i*B
iDB
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
kB
kB
kQB
kQB
k6B
kQB
k�B
kkB
k�B
lB
l"B
l"B
l=B
l=B
lqB
l�B
l�B
l�B
l�B
m)B
m)B
m]B
m)B
m]B
m]B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
nB
nIB
n}B
n�B
n�B
n�B
n�B
o B
oB
o B
o5B
oOB
o�B
o�B
o�B
p!B
p;B
pUB
p!B
p;B
p!B
pUB
p�B
p�B
p�B
p�B
p�B
p�B
q'B
q[B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
rGB
r|B
raB
r�B
r�B
r|B
r|B
r�B
r�B
r�B
sB
s3B
sMB
sMB
s�B
s�B
s�B
s�B
s�B
s�B
tB
tB
t9B
tTB
t�B
t�B
t�B
uB
u?B
u?B
u?B
u�B
u�B
v`B
vzB
v�B
v�B
v�B
v�B
w2B
w�B
xB
xB
w�B
xB
xB
xRB
xRB
xlB
x�B
x�B
y>B
y>B
y>B
yrB
yrB
y�B
z^B
zDB
z�B
z�B
z�B
z�B
{B
z�B
{JB
{dB
{B
{B
{�B
{�B
{�B
{�B
|B
|PB
|�B
}B
}"B
}�B
~(B
~BB
~]B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
B
~�B
~�B
B
B
�B
�B
� B
�B
� B
�B
�iB
�iB
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
�B
�B
�B
� B
� B
�B
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.06(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201904130042012019041300420120190413004201202207271130362022072711303620220727113036202207271533222022072715332220220727153322  JA  ARFMdecpA30a                                                                20190523095840  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190523100023  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190523100024  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190523100024  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190523100025  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20190523100025                      G�O�G�O�G�O�                JA  ARUP                                                                        20190523111515                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190403000000  CF  PSAL_ADJUSTED_QC@�@�G�O�                JM  ARCAJMQC2.0                                                                 20190412154201  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190412154201  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023036  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063322  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818081507                      G�O�G�O�G�O�                