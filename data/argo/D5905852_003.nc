CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-05-23T10:00:17Z creation;2019-05-23T10:00:19Z conversion to V3.1;2022-08-02T05:12:58Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190523100017  20220818081507  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_003                    2C  D   APEX                            8420                            2.11.2                          846 @ث�c�9 1   @ث�o� @*���C�]�d��ݗ�+1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@�33@�  A   A   A@  A`  A�  A�  A�33A�  A�  A�  A�  A�  A�33B  B  B  B ffB(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB�  B�  B�  B���B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C�C  C  C  C�C  C�fC�fC   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:�C;�fC=�fC@  CB  CD�CE�fCH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!�fD"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�R@��
@�Q�A ��A!G�A@Q�Aa�A�z�A�ffA�{A�ffA�z�AЏ\A��A��\B 
=B{Bp�Bp�B ��B(p�B033B8(�B@(�BH33BPQ�BX=qB`=qBhG�Bp=qBxG�B�33B�.B�(�B�(�B��B�#�B��B�{B�{B�{B��B�{B��B�aHB�z�B��B�33B�33B��fB��BО�B��HB�\B��B�#�B�(�B�{B�#�B��B��B�
=B�
=C �C
=CC�CC
�C�C
C+�C�C\C
C!HC{C�C�qC C"\C$�C&�C(�C*
=C,
=C.C0
=C2\C4\C6{C8
=C:.C;��C>�C@�CB�CD#�CE�CHCJ
=CL
=CN
=CP
=CR�CT�CV�CX\CZ\C\�C^�C`{Cb�Cd�Cf{Ch
=CjCl
=Cn�Cp�Cr�Ct�Cv
=Cx\Cz�C|
C~�C��C�C��C��C�C�C�C�C�C�
=C��C�C��C��C��C�
=C��C�fC�fC�
=C��C��C�fC��C��C��C��C��C��C��C�fC�fC�fC�fC�fC�fC��C�C��C��C�
=C�fC�C�fC��C��C��C�C�
=C�\C�
=C�fC�fC��C��C�C�C��C�
=C��C��C��C�
=C��C�fC�C�C�fC�fC��C�fC�C��C�
=C��C��C��C��C�fC��C��C��C�C��C��C��C��C��C�
=C�C��C��C��C��C�
=C�fC��C�fC��C�fC��C�
=C�\C�C��C��C��C��C��C�
=C��C��C��C��C��C�
=C��C��C��C��C�fC�fC��C�C�C�C��C�fD �D ��D�D�{D{D��D{D��D{D�{D3D��D{D�3D3D��D�D�{D	�D	��D
�D
��DD��D�D�3D�D�3D�D��D3D��DHD�3D3D�3D{D�3D�D�D�D�D
D�fD�D�{D{D�D3D�{DfD��D�D�3D�D�DD�{D�D�{DD��D�D��D {D ��D!�D!��D"�D"�3D#�D#�3D$�D$��D%3D%�{D&D&��D'�D'��D(3D(�3D)HD)��D*fD*�{D+{D+�{D,3D,�3D-�D-��D.3D.��D/3D/�{D0�D0�D1D1��D2{D2��D3�D3�{D4{D4�3D5{D5��D6�D6��D7�D7��D8D8�D9�D9��D:�D:�3D;�D;�HD<HD<��D=3D=�fD>�D>�D?{D?�D@{D@�{DA�DA��DB{DB��DC
DC�{DD �DD��DE�DE�{DF3DF�HDG3DG�DH{DH��DI�DI�3DJ�DJ��DK�DK��DL�DL�3DM�DM��DN{DN�{DO{DO�{DP�DP�{DQ�DQ�3DR{DR�
DSDS�DT{DT�{DU�DU�{DVDV��DW�DW�3DX{DX�3DY�DY�DZ{DZ�D[�D[��D\D\�{D]3D]�3D^D^�{D_�D_�{D`�D`��Da�Da��Db{Db�{Dc{Dc�{Dd�Dd�De{De�{DfDf��Dg3Dg��DhHDh��Di�Di��Dj3Dj�3Dk{Dk�fDlDl��DmDm��Dn{Dn�Do{Do��Dp�Dp�{DqfDq�Dr�Dr��DsDs��Dt�Dt�3DuDu�Dv�Dv�{Dw{Dw�3Dx�Dx��Dy�Dy��Dz�Dz�{D{D{��D|3D|��D}{D}��D~{D~�{D�D�3D��D�B=D���D���D��D�A�D���D���D�=D�C3D���D�ÅD��D�B=D��=D���D��D�B�D���D���D��D�AHD���D���D��D�B=D���D��HD��D�C3D���D��=D��D�A�D���D��HD��D�A�D���D���D�=D�A�D���D�D�=D�AHD��HD��=D��D�A�D���D���D��D�B�D���D���D� �D�AHD���D���D��D�A�D��=D�D��D�A�D���D���D��D�B�D���D���D� �D�@RD���D�D��D�@�D���D�D��D�B�D���D�D�HD�A�D���D���D��D�B=D���D��=D�=D�B=D���D��=D� �D�@�D���D���D�=D�A�D���D��=D��D�B�D���D���D��D�A�D��=D���D�HD�B=D���D��=D�=D�B�D���D�D��D�B�D��=D���D��D�B=D��=D���D�=D�A�D���D���D��D�B=D��HD���D�HD�AHD���D���D� �D�AHD��=D��=D��D�AHD���D���D�=D�A�D���D���D�=D�B�D���D���D��D�B=D���D��=D�=D�C3D���D���D��D�AHD���D��HD� �D�A�D���D���D��D�A�D���D��=D�=D�A�D���D��HD��D�A�D���D��HD��D�A�D���D�D��D�AHD���D��3D��D�AHD���D���D��D�B=D���D���D��D�@�D���D��HD��D�B�D���D��HD��D�A�D���D��HD�HD�AHD��HD���D��D�B�D���D���D�=D�B�D��3D�ÅD�=D�A�D��=D�D�=D�A�D��HD��HD��D�C3D���D��HD�=D�B�D���D���D��D�B=D���D��HD��D�AHD���D��=D�=D�B�D���D���D�HD�B=D���D���D��D�B=D���D���D��D�A�D���D���D��D�A�D�D��=D��D�B�DÂ�D��3D��D�B�Dă3D���D�HD�A�DŁ�D��HD��D�B=DƁ�D��HD� �D�@RDǁ�D���D� �D�A�Dȁ�D��HD� RD�A�DɁ�D���D��D�B=Dʂ=D��=D�=D�B�Dˁ�D���D��D�B�D̂�D�D��D�A�D͂=D��HD��D�C3D΁�D��=D�3D�C�Dσ3D���D�HD�A�DЁ�D��HD��D�A�Dс�D���D�=D�B=D҂�D���D��D�B=DӁ�D���D��D�B=DԂ�D��=D�HD�B=DՁ�D���D��D�A�DցHD��HD� �D�AHDׁ�D���D�HD�A�D؁HD���D��D�@�DفHD���D�=D�B�Dڂ=D��HD� �D�@�Dہ�D���D�HD�A�D܁�D���D��D�C�D݂=D���D��D�B�Dނ�D�ÅD��D�B�D߂�D��3D��D�A�D��=D���D� RD�@�D�HD���D�=D�B�D₏D���D�HD�B=D�=D���D��D�A�D䂏D��=D��D�@�D��D���D��D�@�D�HD���D��D�B=D��D��=D�3D�B=D聚D���D��D�B=D�HD���D��D�@�D��D��HD� �D�AHD끚D��=D��D�B=D�3D�D��D�B=D큚D���D��D�A�DD���D�HD�B=D�=D���D��D�B�D���D��=D�=D�B=D�3D���D� �D�A�D�=D�D��D�A�D��D���D��D�@�D��D���D��D�A�D���D���D��D�A�D���D��=D�=D�B�D���D���D�HD�@�D���D�D��D�B�D���D���D� �D�A�D���D�D��D�@RD��HD�D� �111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A� 'A�7�A�;0A�<�A�>A�A�A�B'A�B'A�A A�B[A�JXA�JXA�K�A�K^A�G�A�4�A���A�l�A�\�A�8�A��A���A��5A��HA�� Aʱ[AʖSA�qAA�Z�A�L�A�:*A�7�A�1[A�=A��A��|A���Aɶ�Aɻ0Aɲ�A�jKA�6�AȬ�A�(XA�=<A��A�D3A�zA�ΥA�D3A�VA��A��9A�N�A��A���A�rA�7�A��~A�P}A�/�A�	7A��
A�7A��zA��rA�oA��A�A�A��fA�A��YA��A�3�A��A�I�A��OA�%zA���A�oA��A���A�G�A�Q�A�=�A��VA���A�IA��A�c�Aq�A}�Au6Ap��Al��Ak?Aj�Ah��A^��AT�fAS��ASVARW�AP�&AON<AM$AI[WAFu%AD��AC<6AA�A@��A>��A;�A:J�A8	A4FA3,=A2J�A2�A1��A1��A1+A0�A.��A-��A,��A+g�A)K^A'uA%Q�A%
=A$��A$A"�A!�CA K�A�A�	A�A�AYKA�A�A� A�At�A�+A�=A^5A<6A�bA�A�A��AE�A�'AĜA*�A��A��A6A�TA{JAo�A8A˒AVmA(�A�=A��A��A_pA/�AA��A��A�A�A�A
��A
��A
�A	{AA A��A��AN<A,=A%A�AsA�A~�A@�A5�A��Aw2A��A�}A�[A��AxlA�AQ�A��AM�A.IAMA
�A+Al�A �>A 4�@�o�@���@���@�e@��@��3@�ی@���@��r@���@��@�M�@��C@��)@��@��@�C-@�q@�ѷ@�C�@�@���@�S@�?@��@���@例@��@��P@��@�E9@���@��@��@�E9@�r@��W@�X@��@�x@�@㇔@�Vm@�Ĝ@�j�@��@�9X@߱[@�Q@��@�O@�$�@�b@�e@��&@ݼ�@�`B@���@�^5@ۊ	@ڽ<@��@�dZ@�ȴ@�y>@�C-@�u�@��/@�#:@��@��.@Ӥ@@�=�@��"@Қ@���@��d@ѭC@шf@�=@� \@���@���@Ѓ@�@�e�@�-w@���@Ψ�@��@�	l@̾�@̘_@�:�@�\)@���@�p�@�@@�M�@�7L@��@���@�^5@��K@ŕ�@�e,@�!�@ď\@�g�@���@�d�@�M@��@��o@���@��@�0U@��@��U@�Q�@���@�w2@�+�@���@��p@��@���@���@�~�@�Q�@��@���@�>�@�n�@��@�Y�@�;@��)@���@�@�@�3�@�*�@�
�@�ԕ@���@�rG@�W?@��|@��}@�>B@��2@���@���@��@��T@�5�@�	l@��`@��o@��@��O@�2�@�	@���@��A@��&@���@��@�y�@�4@�q@���@���@���@�Z�@�1'@�!�@��.@��@@��@�$@�t�@�?}@�$t@� i@�͟@��\@���@��z@���@�y�@��@��@�K�@�C@���@�M�@�$@��@�f�@�@���@���@��@�[W@���@�A�@�%�@�~@�@��.@���@�;d@��@��@��E@�|�@�Q@�1'@���@��P@���@�|�@�c�@�33@��@�@��@��	@��@��j@���@�� @���@�q@�S�@���@��@�g8@��@���@�B�@��@���@���@��@��@���@��@���@���@�g�@�q@���@��U@�j@��@���@�n/@�A�@�+�@��@��@��r@���@�u%@�6@�@��a@��@�J#@��@��	@���@��6@�e�@��@��+@��A@���@�4�@�&@��@��v@�֡@��L@��@�l�@�kQ@�M�@��@���@��:@�a�@��<@��@�G�@���@�c�@�4n@��@�8@��p@���@�`�@��@���@���@�o�@���@��?@��O@��@��_@���@��@��@���@���@�Vm@�A @�+@��@��@��@�ی@��@��t@�E9@��@�{�@���@��0@�j@���@��X@�N�@��@�9�@��"@�Ĝ@��@�@~Q@}e,@|�K@|m�@|Q�@|7@{�@{RT@y�o@y�M@yc�@yF@x��@x�U@x�_@x��@x�@xN�@w�@@w~�@we�@wJ#@w�@v�@v�b@v#:@u��@u^�@t��@tS�@s�a@s�@r�X@rv�@rGE@r�@q�T@p��@o��@oo@n��@n{@m��@m�-@mrG@m;@l�@l�v@lɆ@l-�@k�@k\)@kRT@kC�@k$t@j�A@i�9@i�7@h��@hK^@g�A@g@f�@f�@e��@e�@d��@d_@d1@c�r@c�0@c"�@b�m@b��@bu%@b^5@bB[@`��@_�
@_=@^��@^��@^E�@]��@]�@\��@\(�@[��@[�@[1�@[.I@[o@Z�c@Z#:@X�@Xl"@X�@W'�@V?@Ve@U�@U�@U[W@U2a@T�@T]d@T�@S�@S@O@R��@R~�@Rc @Q�X@Q#�@P��@P�@O�m@O�P@Oo�@O�@N�2@N��@N^5@N&�@M�)@M�@M�'@M��@Me,@MDg@L�@L��@Lu�@LQ�@LA�@L4n@L*�@L%�@L �@L�@L�@L	�@K��@K�r@K�@Kݘ@K|�@J�@J��@Jl�@J6�@J-@I@I��@I��@I��@IG�@H�5@HFt@G��@F�L@Fu%@FW�@F-@F($@F&�@E��@E�@E�S@Eq@C�
@B�L@B\�@BC�@B@B�@A�@A��@A��@A��@Ap�@@��@@�Y@@S�@@A�@?�;@>�<@=�@=a�@<�Y@<-�@;˒@;��@;�f@;X�@:��@:�@:�@:n�@:O@9�@:@9�D@9��@9w2@9#�@8��@8�$@8��@8z�@89X@8@7_p@7�@6�!@61�@5��@5�>@5�@5��@5��@5m]@5�@4�@3��@3l�@3=@2�y@2��@2{@1��@1�@0A�@/��@/X�@/�@/�@.ߤ@.xl@-�Z@-��@-|@-F@,�/@,��@,��@,w�@,PH@,2�@,�@+ݘ@+x@+,�@*��@*͟@*�@*GE@)�9@)�@)@)�@)��@)��@)�=@)��@)��@)��@)�"@)x�@)*0@(�`@(�[@(��@(�4@(oi@'��@'6z@&�@&��@&xl@&)�@&u@%�Z@%��@%c�@%=�@%�@$�P@$�)@$�O@$�I@$�@$,=@$�@#�F@#X�@#/�@#�@"�2@"��@"��@"B[@!��@!u�@!q@ �@ M@ 7@�r@��@�}@��@qv@��@��@GE@1�@�D@�@hs@��@�p@�@�@]d@/�@�+@ƨ@{J@+@�@�M@�@��@��@J�@ϫ@rG@j@Y�@Vm@Q�@F@/@@��@�@ѷ@��@�z@�u@oi@:�@�@��@�w@�q@�:@P�@4�@C@�@ں@�@	@�@��@�@��@�'@e,@IR@�@��@�@�@N�@,=@�@�6@S�@!-@��@�}@s�@R�@H�@+k@�@�>@��@��@a�@0�@@@�@�f@�K@��@��@j@S�@@��@��@��@J#@$t@
=@�@͟@��@�R@�@��@��@�@z@YK@C�@�@u@�9@�^@�^@��@��@��@��@u�@Dg@�	@ی@��@�e@�@�Y@w�@,=@�@�@��@v`@_p@K�@A�@)_@
��@
��@	ԕ@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A� 'A�7�A�;0A�<�A�>A�A�A�B'A�B'A�A A�B[A�JXA�JXA�K�A�K^A�G�A�4�A���A�l�A�\�A�8�A��A���A��5A��HA�� Aʱ[AʖSA�qAA�Z�A�L�A�:*A�7�A�1[A�=A��A��|A���Aɶ�Aɻ0Aɲ�A�jKA�6�AȬ�A�(XA�=<A��A�D3A�zA�ΥA�D3A�VA��A��9A�N�A��A���A�rA�7�A��~A�P}A�/�A�	7A��
A�7A��zA��rA�oA��A�A�A��fA�A��YA��A�3�A��A�I�A��OA�%zA���A�oA��A���A�G�A�Q�A�=�A��VA���A�IA��A�c�Aq�A}�Au6Ap��Al��Ak?Aj�Ah��A^��AT�fAS��ASVARW�AP�&AON<AM$AI[WAFu%AD��AC<6AA�A@��A>��A;�A:J�A8	A4FA3,=A2J�A2�A1��A1��A1+A0�A.��A-��A,��A+g�A)K^A'uA%Q�A%
=A$��A$A"�A!�CA K�A�A�	A�A�AYKA�A�A� A�At�A�+A�=A^5A<6A�bA�A�A��AE�A�'AĜA*�A��A��A6A�TA{JAo�A8A˒AVmA(�A�=A��A��A_pA/�AA��A��A�A�A�A
��A
��A
�A	{AA A��A��AN<A,=A%A�AsA�A~�A@�A5�A��Aw2A��A�}A�[A��AxlA�AQ�A��AM�A.IAMA
�A+Al�A �>A 4�@�o�@���@���@�e@��@��3@�ی@���@��r@���@��@�M�@��C@��)@��@��@�C-@�q@�ѷ@�C�@�@���@�S@�?@��@���@例@��@��P@��@�E9@���@��@��@�E9@�r@��W@�X@��@�x@�@㇔@�Vm@�Ĝ@�j�@��@�9X@߱[@�Q@��@�O@�$�@�b@�e@��&@ݼ�@�`B@���@�^5@ۊ	@ڽ<@��@�dZ@�ȴ@�y>@�C-@�u�@��/@�#:@��@��.@Ӥ@@�=�@��"@Қ@���@��d@ѭC@шf@�=@� \@���@���@Ѓ@�@�e�@�-w@���@Ψ�@��@�	l@̾�@̘_@�:�@�\)@���@�p�@�@@�M�@�7L@��@���@�^5@��K@ŕ�@�e,@�!�@ď\@�g�@���@�d�@�M@��@��o@���@��@�0U@��@��U@�Q�@���@�w2@�+�@���@��p@��@���@���@�~�@�Q�@��@���@�>�@�n�@��@�Y�@�;@��)@���@�@�@�3�@�*�@�
�@�ԕ@���@�rG@�W?@��|@��}@�>B@��2@���@���@��@��T@�5�@�	l@��`@��o@��@��O@�2�@�	@���@��A@��&@���@��@�y�@�4@�q@���@���@���@�Z�@�1'@�!�@��.@��@@��@�$@�t�@�?}@�$t@� i@�͟@��\@���@��z@���@�y�@��@��@�K�@�C@���@�M�@�$@��@�f�@�@���@���@��@�[W@���@�A�@�%�@�~@�@��.@���@�;d@��@��@��E@�|�@�Q@�1'@���@��P@���@�|�@�c�@�33@��@�@��@��	@��@��j@���@�� @���@�q@�S�@���@��@�g8@��@���@�B�@��@���@���@��@��@���@��@���@���@�g�@�q@���@��U@�j@��@���@�n/@�A�@�+�@��@��@��r@���@�u%@�6@�@��a@��@�J#@��@��	@���@��6@�e�@��@��+@��A@���@�4�@�&@��@��v@�֡@��L@��@�l�@�kQ@�M�@��@���@��:@�a�@��<@��@�G�@���@�c�@�4n@��@�8@��p@���@�`�@��@���@���@�o�@���@��?@��O@��@��_@���@��@��@���@���@�Vm@�A @�+@��@��@��@�ی@��@��t@�E9@��@�{�@���@��0@�j@���@��X@�N�@��@�9�@��"@�Ĝ@��@�@~Q@}e,@|�K@|m�@|Q�@|7@{�@{RT@y�o@y�M@yc�@yF@x��@x�U@x�_@x��@x�@xN�@w�@@w~�@we�@wJ#@w�@v�@v�b@v#:@u��@u^�@t��@tS�@s�a@s�@r�X@rv�@rGE@r�@q�T@p��@o��@oo@n��@n{@m��@m�-@mrG@m;@l�@l�v@lɆ@l-�@k�@k\)@kRT@kC�@k$t@j�A@i�9@i�7@h��@hK^@g�A@g@f�@f�@e��@e�@d��@d_@d1@c�r@c�0@c"�@b�m@b��@bu%@b^5@bB[@`��@_�
@_=@^��@^��@^E�@]��@]�@\��@\(�@[��@[�@[1�@[.I@[o@Z�c@Z#:@X�@Xl"@X�@W'�@V?@Ve@U�@U�@U[W@U2a@T�@T]d@T�@S�@S@O@R��@R~�@Rc @Q�X@Q#�@P��@P�@O�m@O�P@Oo�@O�@N�2@N��@N^5@N&�@M�)@M�@M�'@M��@Me,@MDg@L�@L��@Lu�@LQ�@LA�@L4n@L*�@L%�@L �@L�@L�@L	�@K��@K�r@K�@Kݘ@K|�@J�@J��@Jl�@J6�@J-@I@I��@I��@I��@IG�@H�5@HFt@G��@F�L@Fu%@FW�@F-@F($@F&�@E��@E�@E�S@Eq@C�
@B�L@B\�@BC�@B@B�@A�@A��@A��@A��@Ap�@@��@@�Y@@S�@@A�@?�;@>�<@=�@=a�@<�Y@<-�@;˒@;��@;�f@;X�@:��@:�@:�@:n�@:O@9�@:@9�D@9��@9w2@9#�@8��@8�$@8��@8z�@89X@8@7_p@7�@6�!@61�@5��@5�>@5�@5��@5��@5m]@5�@4�@3��@3l�@3=@2�y@2��@2{@1��@1�@0A�@/��@/X�@/�@/�@.ߤ@.xl@-�Z@-��@-|@-F@,�/@,��@,��@,w�@,PH@,2�@,�@+ݘ@+x@+,�@*��@*͟@*�@*GE@)�9@)�@)@)�@)��@)��@)�=@)��@)��@)��@)�"@)x�@)*0@(�`@(�[@(��@(�4@(oi@'��@'6z@&�@&��@&xl@&)�@&u@%�Z@%��@%c�@%=�@%�@$�P@$�)@$�O@$�I@$�@$,=@$�@#�F@#X�@#/�@#�@"�2@"��@"��@"B[@!��@!u�@!q@ �@ M@ 7@�r@��@�}@��@qv@��@��@GE@1�@�D@�@hs@��@�p@�@�@]d@/�@�+@ƨ@{J@+@�@�M@�@��@��@J�@ϫ@rG@j@Y�@Vm@Q�@F@/@@��@�@ѷ@��@�z@�u@oi@:�@�@��@�w@�q@�:@P�@4�@C@�@ں@�@	@�@��@�@��@�'@e,@IR@�@��@�@�@N�@,=@�@�6@S�@!-@��@�}@s�@R�@H�@+k@�@�>@��@��@a�@0�@@@�@�f@�K@��@��@j@S�@@��@��@��@J#@$t@
=@�@͟@��@�R@�@��@��@�@z@YK@C�@�@u@�9@�^@�^@��@��@��@��@u�@Dg@�	@ی@��@�e@�@�Y@w�@,=@�@�@��@v`@_p@K�@A�@)_@
��@
��@	ԕ@	��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B��B��B��B��B��B�B�.B�B�BЗBбBЗBуB��B	{B	��B
��B
�aB
��B
�B
�(B
��B
��B
ƨB
�aB
��B
�BB
�BB
��B
�B
�B
��B
��B
��B
��B
��B
�cB
ĶB
�<B
��B
�KB�BezB��B��B�B�_B��BB�B�B%�B,�B-�B.�B/ B.}B1'B2-B0UB-CB*�B�B�B9B�B	�B�B �B�XB�B�$B�XB��B��B��B}VBO�B	B
�B
��B
�B
�7B
��B
{B
p;B
K�B
%�B	�B	ݘB	�HB	��B	� B	n�B	g�B	c�B	X�B	3B	WB	"B	,qB	;JB	M6B	X+B	]/B	i�B	r|B	u%B	xB	r�B	s�B	qAB	g�B	eB	k6B	v`B	~�B	�MB	��B	��B	��B	��B	�B	�SB	��B	�AB	��B	��B	� B	�aB	��B	��B	��B	�MB	�@B	��B	�*B	��B	�6B	��B	�;B	�B	��B	��B	�WB	��B	��B	�bB	�'B	�pB	��B	��B	��B	��B	��B	��B	�iB	��B	��B	�nB	�`B	�]B	��B	��B	�%B	�B	ȚB	�gB	یB	��B	��B	��B	� B	��B	�B	��B	�B	�B	�qB	��B	��B	�
B	�hB	�dB	ּB	��B	�B	��B	ٚB	�B	�5B	��B	�HB	�B	�B	�B	�B	��B	�^B	��B	�}B	�.B	�^B	��B	�tB	�GB	�-B	��B	�|B	��B	��B	��B	�B	��B	��B	�)B	��B	��B	��B	�B	�KB	�B	�B	��B	��B	��B	��B	�rB	��B	�FB	�nB	��B	�hB	��B	�dB	�xB	��B	��B	��B
SB
0B
B

�B
1B
�B
B
-B
 4B	��B	�jB	��B	��B	��B	�LB	��B	��B	��B	��B	�B	�+B	��B	�B	��B	�MB	��B	��B	��B	��B	�B	�^B	��B	��B	��B	�RB	�LB	��B	�DB	�$B	�8B	�`B	�TB	�MB	�B	�hB	�hB	�B	�hB	��B	�nB	�nB	�B	��B	�?B	�tB	��B	��B	��B	�`B	�fB	��B	��B	��B	��B	�XB	��B	�B	��B	��B	�B	��B	��B	�B	�B	��B	��B	�6B	�"B	�B	��B	�wB
  B
AB
�B
'B
�B
;B
 �B
 �B
oB
B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
B
EB
�B
�B
�B
�B
�B
�B
B
tB
YB
YB
YB
YB
YB
�B
�B
�B
�B
�B
�B
EB
KB
	lB
	B
�B
�B
	�B

	B
	lB
	B

XB

�B
^B
�B
�B
�B
�B
�B
�B
�B
�B
�B
dB
�B
~B
0B
�B
�B
�B
�B
B
�B
<B
BB
BB
\B
�B
�B
}B
�B
B
 B
 B
�B
�B
aB
B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
B
B
9B
B
B
B
�B
gB
�B
�B
_B
1B
B
�B
B
�B
�B
�B
�B
�B
)B
B
�B
)B
)B
)B
)B
CB
)B
B
�B
]B
�B
�B
B
B
B
�B
�B
�B
�B
�B
�B
B
B
B
OB
�B
�B
�B
�B
 vB
!-B
!HB
!B
!B
!�B
!|B
!bB
!HB
!bB
!�B
!�B
!�B
"B
"4B
"hB
"�B
"�B
"�B
"�B
#:B
#B
"�B
#�B
#�B
#�B
$B
$&B
$@B
$�B
$�B
$�B
$�B
$�B
%FB
%,B
%FB
%FB
%�B
&�B
'�B
(sB
(�B
(�B
(�B
)�B
*eB
*eB
*�B
+6B
+QB
+�B
+�B
,�B
-CB
-wB
-wB
-�B
-]B
-)B
-]B
-]B
-�B
.cB
.�B
.�B
.�B
/ B
/B
/ B
/5B
0;B
0oB
0oB
1'B
1�B
1�B
2B
2|B
2aB
2�B
33B
3�B
3�B
3�B
4�B
5ZB
5�B
6`B
6�B
6�B
6�B
6�B
6�B
6�B
88B
8B
88B
88B
8RB
8�B
8�B
8�B
8lB
8�B
9$B
9	B
9	B
9$B
9>B
9>B
9XB
9�B
9�B
9�B
:xB
:�B
:�B
;dB
;B
;�B
;�B
;�B
;B
<�B
="B
=�B
=�B
>BB
>]B
>]B
>�B
>�B
>�B
>�B
>�B
?HB
?cB
?cB
?cB
?HB
?HB
@4B
@B
@4B
A;B
@�B
AUB
BB
BAB
B�B
BuB
C-B
C{B
C�B
C�B
C�B
C�B
DgB
D�B
D�B
D�B
D�B
D�B
E�B
F?B
FtB
F�B
F�B
G+B
GzB
G�B
HKB
H�B
H�B
IB
IlB
IlB
IRB
IB
I�B
J�B
KDB
KDB
K�B
LdB
LJB
L~B
L�B
L�B
L�B
MB
M�B
M�B
M�B
N"B
NVB
N<B
NpB
N�B
O(B
O(B
O(B
O�B
O�B
O�B
P.B
PB
P.B
P�B
P�B
Q B
QB
QB
Q�B
Q�B
Q�B
Q�B
RB
R B
RTB
RoB
RoB
RoB
R�B
RoB
RoB
RoB
R�B
R�B
RoB
RoB
RTB
R�B
S&B
SB
S@B
S&B
S&B
S�B
S�B
S�B
S�B
S�B
TFB
T�B
UMB
VmB
VmB
V�B
V�B
V�B
VmB
V�B
V�B
V�B
W$B
XyB
Y1B
Y1B
YKB
YeB
YeB
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
ZQB
ZkB
ZQB
Z7B
Z�B
Z�B
[=B
[�B
\CB
\�B
\�B
\�B
\�B
\�B
]/B
]IB
]dB
]�B
^jB
_;B
_VB
_VB
_�B
_�B
_�B
`B
`'B
`B
_�B
_�B
`�B
`�B
aB
abB
a|B
a|B
a|B
abB
abB
abB
a�B
a�B
b�B
b�B
b�B
b�B
b�B
c B
cB
c�B
dB
dZB
d�B
d�B
d�B
eB
e`B
e�B
f2B
ffB
f�B
gB
gB
gB
gmB
g�B
g�B
g�B
g�B
h$B
h�B
h�B
h�B
h�B
iDB
i�B
i�B
i�B
i�B
i�B
i�B
jB
jB
jB
i�B
jB
i�B
jB
j�B
j�B
j�B
j�B
j�B
k�B
k�B
lB
l"B
lWB
l�B
l�B
l�B
lWB
mB
mB
mCB
mwB
m�B
m�B
m�B
m�B
m�B
m�B
n/B
n}B
n�B
n�B
n�B
n�B
n�B
o B
o�B
o�B
o�B
p;B
pUB
poB
p�B
p�B
p�B
p�B
p�B
q�B
q�B
raB
r|B
r�B
r�B
shB
s�B
s�B
tB
tB
tnB
t�B
t�B
uB
uZB
u�B
u�B
u�B
u�B
u�B
u�B
vB
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w2B
wLB
wfB
w�B
w�B
w�B
w�B
xB
x8B
x8B
xlB
xlB
x�B
x�B
x�B
x�B
x�B
y	B
y$B
y�B
y�B
y�B
zB
y�B
y�B
z*B
zDB
zxB
z�B
z�B
z�B
{0B
{JB
{JB
{B
{�B
|B
|6B
|jB
|�B
|�B
|�B
|�B
|�B
|�B
}"B
}VB
}qB
}�B
}�B
}�B
}�B
}�B
~B
~BB
~BB
~]B
~�B
~�B
~�B
HB
�B
�B
�B
�B
� B
�B
�B
�B
�OB
�iB
�OB
�iB
��B
��B
��B
��B
� B
�;B
�;B
�UB
�UB
�UB
�UB
�UB
��B
��B
��B
��B
��B
��B
�B
�B
�[B
�[B
��B
��B
��B
��B
�B
��B
�-B
�-B
�aB
��B
�M111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B� B�B�B�B��B��B�.B�HB�.B�BбB��BбB��BںB	�B	��B
��B
�|B
�B
�jB
�]B
��B
��B
��B
ðB
��B
�wB
�]B
��B
�(B
�4B
��B
��B
��B
�B
��B
�}B
�B
οB
خB
�BEBf�B�	BªBބB�B�B�B�B�B($B-]B.�B/�B0�B2aB5�B5B3�B2-B/iBjB�BKBBBBB��B��B�KB̳B��B�"B�B�GBV9B"B
�sB
�~B
�B
��B
�#B
B
vzB
S�B
,�B	��B	��B	خB	�QB	�SB	p�B	i�B	h�B	d�B	;�B	�B	#:B	-�B	=qB	O�B	[qB	a�B	m)B	t�B	w2B	z*B	t�B	v`B	t�B	jB	hsB	oB	w�B	�B	��B	�9B	�?B	��B	�KB	��B	��B	��B	�MB	�zB	��B	��B	��B	��B	��B	�FB	��B	��B	��B	��B	��B	�WB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�BB	��B	�$B	�kB	�kB	�OB	��B	�UB	��B	��B	��B	��B	�)B	��B	�`B	��B	�BB	��B	��B	�qB	��B	�fB	�FB	�nB	�LB	�)B	�B	��B	�B	�]B	�eB	��B	�DB	�B	޸B	�?B	�MB	�9B	�B	��B	��B	��B	�BB	�B	�B	�'B	�B	�}B	��B	�^B	��B	��B
 B	��B	��B	��B	�|B	�GB	��B	�aB	��B	��B	��B	�B	�2B	��B	�B	�B	�B	�XB	�B	�eB	��B	�B	�0B
  B	�qB	�JB	�B	�B	��B	��B	�MB	�B	�JB	��B	��B	�B	�LB	��B
9B
�B
�B
)B
	RB
	B
�B
�B
 �B	�(B	��B	�JB	�B	�	B	��B	��B	�2B	��B	�B	�zB	��B	��B	�9B	��B	�3B	��B	��B	��B	��B	�dB	��B	��B	�0B	�^B	��B	��B	�DB	�^B	�rB	��B	��B	��B	�B	�B	�B	�B	��B	�B	�9B	�B	�B	��B	��B	�ZB	��B	��B	�B	�FB	��B	��B	��B	��B	�RB	�$B	��B	�B	�xB	�B	��B	�PB	�dB	�dB	��B	�PB	�B	�B	��B	�VB	�<B	��B	��B
 �B
�B
�B
[B
�B
UB
UB
oB
�B
uB
aB
B
3B
3B
�B
B
B
�B
B
�B
+B
zB
�B
B
KB
KB
	B
B
EB
�B
tB
tB
tB
tB
tB
�B
�B
�B
�B
�B
B
�B
	B
	lB
	7B
	7B
	RB

=B

#B
	�B
	�B

�B
)B
�B
�B
�B
�B
�B
B
�B
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
�B
�B
"B
�B
\B
\B
vB
�B
B
�B
B
:B
:B
�B
uB
2B
{B
FB
�B
�B
B
B
2B
�B
�B
B
�B
B
B
B
9B
9B
9B
9B
SB
9B
?B
?B
yB
eB
B
�B
)B
�B
�B
�B
�B
B
)B
B
�B
CB
CB
CB
CB
]B
)B
)B
]B
�B
B
B
IB
dB
5B
�B
�B
�B
�B
�B
�B
jB
OB
OB
jB
�B
�B
!B
 BB
 �B
!HB
!HB
!B
!HB
!�B
!|B
!bB
!HB
!|B
!�B
!�B
!�B
"NB
"NB
"�B
"�B
"�B
"�B
"�B
#TB
# B
#B
#�B
#�B
#�B
$B
$@B
$ZB
$�B
$�B
$�B
$�B
$�B
%`B
%`B
%zB
%�B
&�B
'B
(XB
(�B
(�B
(�B
)yB
*0B
*B
*�B
+B
+QB
+kB
+�B
,"B
,�B
-CB
-�B
-]B
-�B
-]B
-wB
-]B
-�B
.B
.}B
.�B
.�B
/ B
/ B
/5B
/OB
/�B
0oB
0�B
0�B
1�B
1�B
1�B
2aB
2�B
2�B
3MB
3�B
4B
49B
4TB
5?B
5�B
5�B
6zB
6�B
6�B
6�B
6�B
6�B
7fB
8RB
88B
8RB
88B
8lB
8�B
8�B
8�B
8�B
8�B
9>B
9$B
9	B
9>B
9>B
9XB
9rB
9�B
:*B
:B
:�B
:�B
;B
;dB
;�B
;�B
;�B
;�B
;�B
="B
=VB
=�B
>(B
>BB
>wB
>]B
>�B
>�B
>�B
>�B
>�B
?}B
?}B
?cB
?}B
?cB
?}B
@iB
@4B
@iB
A;B
A B
A�B
BAB
BuB
B�B
B�B
CaB
C�B
C�B
C�B
C�B
C�B
DgB
D�B
D�B
D�B
D�B
D�B
F?B
FtB
F�B
GB
G+B
GEB
G�B
H1B
H�B
H�B
H�B
IB
IRB
I�B
IlB
IlB
J#B
K)B
K^B
KxB
L0B
LdB
LdB
L�B
L�B
L�B
L�B
M6B
M�B
M�B
NB
NVB
NVB
N<B
N�B
OB
O(B
OBB
OvB
O�B
O�B
O�B
PHB
PB
PbB
P�B
P�B
QB
Q4B
Q4B
Q�B
Q�B
Q�B
RB
R B
R B
RTB
RoB
RoB
RoB
RoB
RTB
RoB
RoB
R�B
RoB
RoB
RoB
RoB
R�B
S@B
S&B
S@B
S@B
S@B
S�B
S�B
S�B
S�B
T,B
T{B
UB
U�B
V�B
V�B
V�B
V�B
V�B
V�B
V�B
W
B
V�B
W�B
X�B
Y1B
YKB
YeB
YeB
YB
Y�B
Y�B
Y�B
Y�B
ZB
ZB
ZQB
Z�B
Z�B
Z�B
[#B
[	B
[�B
\B
\]B
\�B
\�B
\�B
\�B
]B
]IB
]IB
]~B
]�B
^OB
_!B
_VB
_�B
_�B
_�B
_�B
`'B
`BB
`'B
_�B
_�B
`�B
`�B
aHB
abB
a|B
a�B
a|B
abB
abB
a�B
a�B
bB
b�B
b�B
b�B
b�B
b�B
cTB
c:B
c�B
d@B
dtB
d�B
d�B
d�B
e,B
e�B
e�B
fLB
f�B
gB
gB
g8B
gB
gmB
g�B
g�B
g�B
g�B
h>B
h�B
h�B
h�B
iB
iDB
i�B
i�B
i�B
i�B
i�B
i�B
jB
jB
i�B
i�B
j0B
jB
j�B
jB
j�B
j�B
j�B
kB
k�B
k�B
lB
l"B
lqB
l�B
l�B
l�B
l�B
m)B
m)B
m]B
mwB
m�B
m�B
m�B
m�B
m�B
m�B
n/B
n}B
n�B
n�B
n�B
n�B
o B
o5B
o�B
o�B
pB
pUB
poB
p�B
p�B
p�B
p�B
p�B
qB
q�B
rB
r|B
r�B
r�B
s3B
s�B
s�B
s�B
tB
tB
t�B
t�B
t�B
u%B
uZB
u�B
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
v�B
v�B
v�B
v�B
wB
w2B
wLB
wfB
wfB
w�B
w�B
w�B
xB
x8B
x8B
xlB
xlB
x�B
x�B
x�B
x�B
x�B
y	B
yXB
y�B
y�B
y�B
zB
y�B
zB
z*B
zDB
z�B
z�B
z�B
{B
{JB
{JB
{dB
{�B
{�B
|B
|6B
|�B
|�B
|�B
|�B
|�B
|�B
}B
}<B
}qB
}�B
}�B
}�B
}�B
}�B
}�B
~B
~BB
~BB
~wB
~�B
~�B
B
cB
�B
�B
�B
�B
� B
�B
�B
�4B
�OB
�iB
�OB
�iB
��B
��B
��B
�B
� B
�;B
� B
�UB
�;B
�UB
�UB
�oB
��B
��B
��B
��B
��B
��B
�B
�'B
�uB
�uB
��B
��B
��B
��B
�B
�B
�GB
�GB
��B
�B
�M311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<D�e<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.06(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201903140041292019031400412920190314004129202207271130122022072711301220220727113012202207271533012022072715330120220727153301  JA  ARFMdecpA30a                                                                20190523095839  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190523100017  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190523100018  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190523100018  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190523100018  QCP$                G�O�G�O�G�O�            FB40JA      jafc1.0                                                                 20190523100019                      G�O�G�O�G�O�                JA  ARUP                                                                        20190523111514                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190304000000  CF  PSAL_ADJUSTED_QC@�H@�HG�O�                JM  ARCAJMQC2.0                                                                 20190313154129  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190313154129  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20220727023012  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063301  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818081507                      G�O�G�O�G�O�                