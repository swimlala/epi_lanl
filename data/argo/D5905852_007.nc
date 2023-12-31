CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-05-23T10:00:25Z creation;2019-05-23T10:00:27Z conversion to V3.1;2022-08-02T05:12:47Z update;     
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20190523100025  20220818081507  5905852                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA  A30_8420_007                    2C  D   APEX                            8420                            2.11.2                          846 @ص��Ř 1   @ص��� @+������d�����1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @��@�33@�  A   AffA@  A`  A�  A�  A�  A�33A�  A�  A�  A�  B ffB  B  B  B   B(  B0  B8ffB?��BG��BP  BW��B`  Bh  Bp  Bx  B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�33B���C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C�C �C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL�CN�CO�fCR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|�C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ D�|�D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@(�@��@�  A (�A�A@Q�A`��A�=qA�(�A�  A��A�  A�ffA�ffA�z�B �B=qB�B(�B {B((�B0G�B8p�B?�RBG�
BO��BW�B`
=Bh{Bp33Bx{B�
B�  B���B�
=B��B�#�B��B��B�\B�
=B�  B�B�
=B�\B�
=B��B�B��fB���B���B�  B�  B�\B��B��B�
=B�
=B��B�Q�B�L�B�{B�W
B�ǮC�3CC
=C�C
�C�C�CCC�C�C�C
=C�C
C &fC"C$�C&  C(  C*�C,�C.
=C0�C2�C4
=C6\C8�C:
=C<
=C>
=C@
=CB
=CDCF�CH�CJ�CL!HCN#�CO�RCR�CTCV�CX
=CZ
=C\�C^�C`
=Cb
=Cd�Cf�Ch�Cj
=Cl�CnCpCrCt�Cv  Cx  Cz\C|�C~�C��C�fC��C��C��C�fC�fC�C��C�C�fC��C�fC�C�C�fC�
=C�fC�C��C��C��C�HC�fC��C��C��C�HC��C�C�HC�HC�fC�C��C�C��C�HC��C��C��C�HC��C�fC�fC��C��C��C�C��C�fC��C��C�fC��C��C��C�HC��C��C�C��C��C�fC�fC��C��C�HC��C��C��C�C�HC��C��C�fC��C�fC�C��C��C��C�C�HC�HC��C��C��C��C��C�HC��C�fC��C�  C��C��C��C��C�
=C��C�C�C�HC��C��C��C�fC�C�fC��C�fC�  C��C�fC��C��C�C�C��C�C�C��C�fC��C��C�HC�D 3D �3D�D��D{D�{D�D�{D3D��D3D�{D�D��D�D�3D�D�3D	�D	��D
�D
��D3D��D�D��D�D��D�D��D �D��D �D�HD�D�3D3D�HD �D��D�D�3D�D��D3D��D�D� D�D��D{D�{D{D��DHD��D{D�HD�D�{D�D��D�D��D �D �{D!3D!��D"�D"��D#D#��D$�D$�HD%�D%��D&{D&��D'�D'�HD( �D(��D)�D)� D*HD*�3D+�D+��D,3D,�3D-3D-��D.�D.�D/{D/��D0�D0�3D1 �D1��D2�D2��D3�D3�HD4�D4�3D5�D5��D63D6��D7HD7�HD8�D8�D9{D9��D:�D:�3D;3D;�3D<�D<��D={D=�3D>�D>��D?�D?�3D@3D@�DA{DA��DBHDB�3DC�DC��DD3DD��DE �DE��DF�DF��DG3DG��DH�DH��DI�DI��DJ�DJ��DK�DK�{DL{DL��DM�DM�{DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT �DT�HDU�DU�3DV�DV�3DW�DW��DX�DX��DY�DY��DZ�DZ��D[3D[��D\�D\�3D]�D]��D^�D^�HD_HD_�3D`3D`��Da3Da�3Db3Db��Dc�Dc��Dd{Dd��De�De�3Df3Df��Dg{Dg��DhHDh�HDi�Di�3Dj3Dj�3Dk3Dk��Dl�Dl�3Dm �Dm��Dn�Dn�3Do3Do��DpHDp��Dq�Dq��Dr �Dr��DsHDs� Dt�Dt��Du �Du��Dv�Dv��Dw�Dw�{Dx3Dx��Dy�Dy� Dy�\Dz��D{ �D{��D| �D|�HD}�D}��D~�D~��D�D��D� RD�@ D��D��\D��\D�?�D��D�� D� �D�@�D���D���D� �D�AHD���D���D� �D�@�D��HD���D� RD�@RD���D���D� �D�@�D���D���D�HD�A�D���D���D�HD�AHD��HD���D��D�AHD���D��HD�  D�@�D��HD��RD�HD�AHD�� D��RD� �D�A�D���D���D��D�@�D���D��=D� �D�@ D���D���D��D�AHD��RD�� D� �D�A�D���D���D� �D�@�D��RD���D��D�@�D���D��RD�  D�A�D���D��RD�  D�@ D���D���D��D�A�D��=D���D�3D�@�D��HD���D� �D�@ D���D���D��D�AHD���D���D�HD�AHD���D��HD� �D�@�D���D���D��D�A�D���D���D� �D�@�D���D��HD� RD�@�D���D��RD�HD�A�D��RD���D� �D�@�D���D���D� �D�AHD���D���D��D�A�D���D��HD��D�A�D���D���D� �D�@ D���D���D��D�@�D��RD���D��D�B=D���D���D� RD�@RD��RD���D��D�A�D��HD���D��D�A�D���D���D��D�@�D���D���D� RD�@�D���D���D��D�A�D��HD���D� �D�A�D���D���D��D�B=D���D��RD� RD�@�D���D���D� �D�A�D���D��RD� �D�A�D��RD���D��D�AHD�� D���D��D�C�D���D��HD� �D�@�D��RD��HD��D�A�D���D��HD� �D�@�D���D��HD� �D�@�D��HD��HD� �D�@�D���D���D��D�AHD���D���D� �D�@�D��RD��RD��D�B�D���D���D� �D�A�D��HD��RD��D�A�D��HD���D�HD�@RD��HD���D��D�B=D��=D���D�HD�B=D��=D��HD� RD�@�D���D���D� �D�A�D��HD���D�=D�A�DHD���D� RD�A�DÂ=D���D�=D�A�DāHD��=D��D�@�Dŀ�D���D�=D�A�D�\D��RD�HD�A�Dǁ�D�� D� �D�A�DȁHD���D� RD�@ DɁHD�D�=D�A�Dʂ=D��HD��D�A�DˀRD���D�HD�A�D́�D���D� �D�B=D́HD���D�HD�@�D΀�D��HD� �D�@�Dπ�D���D� �D�A�DЁ�D���D��D�@�Dр�D��HD�HD�AHDҀ�D��RD� RD�A�DӁ�D���D��D�A�Dԁ�D��=D��D�A�DՁHD���D� �D�A�DցHD���D�HD�@RD׀�D���D� �D�@�D؂=D���D�HD�@�Dـ�D���D� RD�@�Dځ�D���D� �D�AHDہHD�� D� �D�A�D܀�D��HD� RD�@ D݁�D���D� �D�AHDނ=D���D�HD�@�D߀�D���D�HD�A�D��=D�D��D�A�D�HD���D� �D�A�D��D���D��D�@RD��D���D�=D�B�D䁚D��RD� �D�@�D��D���D� �D�AHD��D��HD�HD�B=D�HD���D��D�C3D�=D���D�HD�A�D��D��RD� �D�A�D��D��HD��D�AHD끚D���D� �D�A�D쁚D���D� �D�@RD퀤D���D��D�AHDD��RD��D�A�D�HD��HD� �D�@�D���D��=D�=D�A�D��D�� D� RD�@�D�D���D� �D�@RD�RD���D�=D�A�D��D���D��D�@�D���D���D�=D�@�D�� D��HD�=D�A�D��=D���D� RD�AHD��RD���D��D�@�D���D��HD��D�A�D��HD��HD��D�B=D���D��HD��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A��A��A�1A�$A��A��A��A��A�:A�uA�A��A��A��A��A�\A�.A��A��A�~A��A��AѤ�AђA�A��dA�C�Ǎ~A�}�Aˀ4A˂AA�o�AʫkAɾBA�ޞA�y>A���A�MAƱ'A�]/A�1A���Aœ�AŊrAŁA�j�A�4A�e�A�Y�A�jA�b�A�0UA�+6A�[�A�	7A�ߤA�F?A�l�A�^A��QA�|�A���A���A�F?A�_�A��IA�YA�U�A�a�A�_A��#A�#nA���A���A�3hA��mA�UgA��A�e�A�q�A�pA��1A��A��,A���A�j�A�0UA���A��A� �A���A��QA��KA~�A{�Az��Ay(Aw��Asq�AqAo,=Am|�Ag_pAbd�A_A[��AY�PAX��AV�AT��APC�AJu�AB��A@��A?��A>qvA<FtA8(�A3ԕA.�A.6zA-�A-��A-w2A--A,~A+N<A+-A*�A)ѷA)��A(�tA'��A'Y�A$�A$+�A#��A"�5A"h
A!3�A ��A h�AL�A�fAx�A�ZA"�Ae�A�vA��A��A�!A��A��A�A�+Au�A%A��Ax�A��A�5Ay�A�AƨA�AW�A�\A�zA��Av�A��A��A�rA��A/A�KA��AffA	��A	 \A͟A�*Av`A[WA4A��AF�AA��AY�A�pAOA_�A�-A+kA�A�zA?�A ��A ��A qA ^5A PHA {@��C@�G�@��@��[@��F@�\�@��@��A@��*@�^�@�(�@��@���@�Ɇ@���@��@�h�@�U2@�@���@��@��@�"h@��A@�1@��'@�G�@��@��@���@�@��@�e�@��@��@�A�@��@@�S�@�ѷ@�U2@��&@�<6@�b@��@�x@�_@�'�@橓@�d�@�@�8�@��@�.@�^5@�(�@���@�$@���@�z�@�m�@��I@�8�@�'R@��@���@���@�	@۳�@�RT@�ߤ@ڂA@�7�@�/�@�_@��@�`B@�Q�@�~�@��@֭�@֓u@�kQ@�N�@�4n@ռ�@��@��?@Ԁ�@�H�@���@��f@�D�@�,=@�@�J#@��@�Ft@ϙ�@�@�ں@�1�@�'�@��@�~@��c@�L0@�_@ɣn@�@O@� i@Ƚ<@Ȼ�@ȟ�@�d�@�ݘ@�:�@��c@�c�@œ�@�Z�@�?}@�@@���@�r�@�,=@��@�`B@�RT@��H@��A@���@���@���@���@���@�� @��@��'@���@�|�@���@���@��X@���@�d�@�6�@���@�b�@��`@��@��@�˒@��@�X@�+�@�V@���@��@��'@���@���@�{�@�Z�@�,=@��@���@���@���@��z@���@��-@��*@���@�S�@��@�7@�2a@�u@��h@�s@�@O@�;d@�%F@���@��@���@���@�Z�@��@��[@��@���@�7@��@���@�e�@��@�*�@� �@��3@�j@��@�j@�!@�x@�<�@��-@��"@�hs@�J#@�%F@��@���@�u�@�kQ@���@�|@�\�@�5�@���@�K^@�@�o�@�Z�@�4@��,@���@���@��@�L0@��K@�y�@�<6@�(�@��@��\@�,=@�@���@���@�y�@�.I@��@� i@��@���@�e�@�~@�@��@�  @��o@�]�@��P@���@��[@��@�j@��
@�hs@�O@��@��8@��K@�Ov@��@�}�@��|@��<@���@���@�Q@�+k@��@���@��"@�l�@�dZ@�Vm@�Y@�Ɇ@�M@�&�@�{@���@��@���@��@�҉@��}@���@�bN@�L0@�C-@��:@���@��@��?@���@�z�@�Q@�?@�(�@��@�l�@�Y@�ߤ@���@�7@���@�a@�O@�H�@�@O@�@�Ĝ@�M@�8�@�_@��S@�E9@�B�@�=@�'�@��@�֡@��!@��@�M�@�$@���@���@��	@���@��@�f�@�b�@�K�@��@���@���@�?@�v`@�ں@�-�@�ϫ@���@�L�@�+@�C@��f@��@�ȴ@�M@�GE@�1�@��@�A@~��@~�\@~3�@~@}�>@}��@}��@}�~@}s�@}Dg@|��@|PH@{��@{1�@{ i@z�H@zu%@y�M@y@@x��@x��@x1@w��@w\)@w4�@w(@vu%@vJ@u�^@u�t@uO�@t�E@t��@tM@s��@s6z@s@r�s@r�L@rL0@q��@q�@p��@p7�@o�f@n�b@n.�@mVm@l`�@l�@k�@k��@k"�@j��@jv�@i�.@i��@h��@g��@g.I@f��@f�@f��@f_�@e��@eO�@e5�@e+@eV@d�f@dѷ@d��@d`�@dG@c��@c��@c��@cv`@cE9@b�@b��@b�@bR�@b	@a�@aJ�@`��@`y>@`%�@_K�@^ȴ@^v�@^{@]��@]m]@\ѷ@\]d@[�]@[�&@Z�@Zp;@Y��@X�@X�p@XĜ@X��@X�u@XXy@X�@WJ#@V��@VQ@U�N@UG�@T��@Tx@SO@R��@R��@R��@R��@R��@R{�@R&�@R�@Q�@Q��@Qzx@PXy@O�W@O�&@O��@Ov`@OP�@O�@N�b@N;�@M�@M�@L�$@L1'@K��@K��@K�{@Kv`@KW?@K$t@J�2@JV@Ik�@IN<@I�@Hی@H�j@HC-@G��@G��@G\)@F��@Fn�@E��@E��@D��@D��@Dl"@DH@C��@C�@Cb�@B�@B}V@B@A�@A��@A�n@A��@A�@@l"@@~@?n/@>ں@>�'@>W�@=�o@=�n@=+@<��@<~(@<'R@<1@;��@;�W@;��@;
=@:�X@:�A@:O@9��@9ϫ@9��@9��@98�@8�[@8�Y@8N�@8 �@7��@7��@7�4@7j�@7H�@7�@6�@6��@6Ta@6R�@68�@6_@5��@5�@5��@5f�@4�U@4�@4S�@3�@3��@3C�@3$t@3�@2��@2c @2#:@2{@1��@0��@0y>@09X@/�@/��@/{J@/Z�@.�R@.8�@-�.@-hs@-2a@-#�@,w�@,?�@,~@+�[@+=@+$t@+C@*��@*��@*=q@)�o@)��@)�N@)��@)^�@)-w@)�@(�f@(�j@(]d@'�Q@'�@@'dZ@'S�@'�@&ں@&�L@&��@&p;@&E�@&.�@&u@%�o@%��@%�=@%N<@%!�@$�K@$ �@#�w@#��@#]�@#J#@#6z@#!-@#S@"�6@"#:@!��@!�@!�H@!��@!hs@!T�@!q@ ��@ C-@�@a@
=@�@^5@+k@�@�j@�@��@��@+�@	l@��@�`@��@C-@�@	�@��@��@�q@qv@\)@1�@��@�y@�!@�L@��@{�@C�@�.@�j@�z@��@�h@j@G�@2a@��@��@PH@/�@%�@"h@,=@�A@�@��@�6@RT@��@�2@��@�@��@�R@��@�@��@�1@�1@�@��@�!@�<@q�@C�@1�@&�@_@��@��@}�@e,@�[@w�@Q�@�@1@��@�F@z@q�@ff@^5@Ta@L0@1�@ �@�@j@8�@q@�j@��@��@�@�u@�u@~(@g8@H@?�@7�@ �@�@7@M@�@�&@��@�@b�@�]@�A@~�@p;@W�@1�@�@�9@@��@�-@��@a�@�K@�?@�U@��@�@��@��@~(@%�111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A��A��A�1A�$A��A��A��A��A�:A�uA�A��A��A��A��A�\A�.A��A��A�~A��A��AѤ�AђA�A��dA�C�Ǎ~A�}�Aˀ4A˂AA�o�AʫkAɾBA�ޞA�y>A���A�MAƱ'A�]/A�1A���Aœ�AŊrAŁA�j�A�4A�e�A�Y�A�jA�b�A�0UA�+6A�[�A�	7A�ߤA�F?A�l�A�^A��QA�|�A���A���A�F?A�_�A��IA�YA�U�A�a�A�_A��#A�#nA���A���A�3hA��mA�UgA��A�e�A�q�A�pA��1A��A��,A���A�j�A�0UA���A��A� �A���A��QA��KA~�A{�Az��Ay(Aw��Asq�AqAo,=Am|�Ag_pAbd�A_A[��AY�PAX��AV�AT��APC�AJu�AB��A@��A?��A>qvA<FtA8(�A3ԕA.�A.6zA-�A-��A-w2A--A,~A+N<A+-A*�A)ѷA)��A(�tA'��A'Y�A$�A$+�A#��A"�5A"h
A!3�A ��A h�AL�A�fAx�A�ZA"�Ae�A�vA��A��A�!A��A��A�A�+Au�A%A��Ax�A��A�5Ay�A�AƨA�AW�A�\A�zA��Av�A��A��A�rA��A/A�KA��AffA	��A	 \A͟A�*Av`A[WA4A��AF�AA��AY�A�pAOA_�A�-A+kA�A�zA?�A ��A ��A qA ^5A PHA {@��C@�G�@��@��[@��F@�\�@��@��A@��*@�^�@�(�@��@���@�Ɇ@���@��@�h�@�U2@�@���@��@��@�"h@��A@�1@��'@�G�@��@��@���@�@��@�e�@��@��@�A�@��@@�S�@�ѷ@�U2@��&@�<6@�b@��@�x@�_@�'�@橓@�d�@�@�8�@��@�.@�^5@�(�@���@�$@���@�z�@�m�@��I@�8�@�'R@��@���@���@�	@۳�@�RT@�ߤ@ڂA@�7�@�/�@�_@��@�`B@�Q�@�~�@��@֭�@֓u@�kQ@�N�@�4n@ռ�@��@��?@Ԁ�@�H�@���@��f@�D�@�,=@�@�J#@��@�Ft@ϙ�@�@�ں@�1�@�'�@��@�~@��c@�L0@�_@ɣn@�@O@� i@Ƚ<@Ȼ�@ȟ�@�d�@�ݘ@�:�@��c@�c�@œ�@�Z�@�?}@�@@���@�r�@�,=@��@�`B@�RT@��H@��A@���@���@���@���@���@�� @��@��'@���@�|�@���@���@��X@���@�d�@�6�@���@�b�@��`@��@��@�˒@��@�X@�+�@�V@���@��@��'@���@���@�{�@�Z�@�,=@��@���@���@���@��z@���@��-@��*@���@�S�@��@�7@�2a@�u@��h@�s@�@O@�;d@�%F@���@��@���@���@�Z�@��@��[@��@���@�7@��@���@�e�@��@�*�@� �@��3@�j@��@�j@�!@�x@�<�@��-@��"@�hs@�J#@�%F@��@���@�u�@�kQ@���@�|@�\�@�5�@���@�K^@�@�o�@�Z�@�4@��,@���@���@��@�L0@��K@�y�@�<6@�(�@��@��\@�,=@�@���@���@�y�@�.I@��@� i@��@���@�e�@�~@�@��@�  @��o@�]�@��P@���@��[@��@�j@��
@�hs@�O@��@��8@��K@�Ov@��@�}�@��|@��<@���@���@�Q@�+k@��@���@��"@�l�@�dZ@�Vm@�Y@�Ɇ@�M@�&�@�{@���@��@���@��@�҉@��}@���@�bN@�L0@�C-@��:@���@��@��?@���@�z�@�Q@�?@�(�@��@�l�@�Y@�ߤ@���@�7@���@�a@�O@�H�@�@O@�@�Ĝ@�M@�8�@�_@��S@�E9@�B�@�=@�'�@��@�֡@��!@��@�M�@�$@���@���@��	@���@��@�f�@�b�@�K�@��@���@���@�?@�v`@�ں@�-�@�ϫ@���@�L�@�+@�C@��f@��@�ȴ@�M@�GE@�1�@��@�A@~��@~�\@~3�@~@}�>@}��@}��@}�~@}s�@}Dg@|��@|PH@{��@{1�@{ i@z�H@zu%@y�M@y@@x��@x��@x1@w��@w\)@w4�@w(@vu%@vJ@u�^@u�t@uO�@t�E@t��@tM@s��@s6z@s@r�s@r�L@rL0@q��@q�@p��@p7�@o�f@n�b@n.�@mVm@l`�@l�@k�@k��@k"�@j��@jv�@i�.@i��@h��@g��@g.I@f��@f�@f��@f_�@e��@eO�@e5�@e+@eV@d�f@dѷ@d��@d`�@dG@c��@c��@c��@cv`@cE9@b�@b��@b�@bR�@b	@a�@aJ�@`��@`y>@`%�@_K�@^ȴ@^v�@^{@]��@]m]@\ѷ@\]d@[�]@[�&@Z�@Zp;@Y��@X�@X�p@XĜ@X��@X�u@XXy@X�@WJ#@V��@VQ@U�N@UG�@T��@Tx@SO@R��@R��@R��@R��@R��@R{�@R&�@R�@Q�@Q��@Qzx@PXy@O�W@O�&@O��@Ov`@OP�@O�@N�b@N;�@M�@M�@L�$@L1'@K��@K��@K�{@Kv`@KW?@K$t@J�2@JV@Ik�@IN<@I�@Hی@H�j@HC-@G��@G��@G\)@F��@Fn�@E��@E��@D��@D��@Dl"@DH@C��@C�@Cb�@B�@B}V@B@A�@A��@A�n@A��@A�@@l"@@~@?n/@>ں@>�'@>W�@=�o@=�n@=+@<��@<~(@<'R@<1@;��@;�W@;��@;
=@:�X@:�A@:O@9��@9ϫ@9��@9��@98�@8�[@8�Y@8N�@8 �@7��@7��@7�4@7j�@7H�@7�@6�@6��@6Ta@6R�@68�@6_@5��@5�@5��@5f�@4�U@4�@4S�@3�@3��@3C�@3$t@3�@2��@2c @2#:@2{@1��@0��@0y>@09X@/�@/��@/{J@/Z�@.�R@.8�@-�.@-hs@-2a@-#�@,w�@,?�@,~@+�[@+=@+$t@+C@*��@*��@*=q@)�o@)��@)�N@)��@)^�@)-w@)�@(�f@(�j@(]d@'�Q@'�@@'dZ@'S�@'�@&ں@&�L@&��@&p;@&E�@&.�@&u@%�o@%��@%�=@%N<@%!�@$�K@$ �@#�w@#��@#]�@#J#@#6z@#!-@#S@"�6@"#:@!��@!�@!�H@!��@!hs@!T�@!q@ ��@ C-@�@a@
=@�@^5@+k@�@�j@�@��@��@+�@	l@��@�`@��@C-@�@	�@��@��@�q@qv@\)@1�@��@�y@�!@�L@��@{�@C�@�.@�j@�z@��@�h@j@G�@2a@��@��@PH@/�@%�@"h@,=@�A@�@��@�6@RT@��@�2@��@�@��@�R@��@�@��@�1@�1@�@��@�!@�<@q�@C�@1�@&�@_@��@��@}�@e,@�[@w�@Q�@�@1@��@�F@z@q�@ff@^5@Ta@L0@1�@ �@�@j@8�@q@�j@��@��@�@�u@�u@~(@g8@H@?�@7�@ �@�@7@M@�@�&@��@�@b�@�]@�A@~�@p;@W�@1�@�@�9@@��@�-@��@a�@�K@�?@�U@��@�@��@��@~(@%�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	qB	WB		B	�B	�B	kB	eB	KB	1B	KB	1B	KB	B	�B	B	�B	B	�B	�B	�B	+B	+B	�B	+B	B	�B	)B	!bB	>B	��B
�B
@�B
L�BH�B	�DB	��B	�%B	��B
tB
!�B
D�B
Q�B
d&B
xlB
�RB
�B
��B
��B
�B
��B
��B
��B
�B�B)B~B_B��B҉B
=B�B�B$tB# B"4B=B@BPB��B��B�B�B��B�B�IB�yB�tBvzBncBh�Ba�BU�BH�B=�B3�B#�B�B
�LB
ںB
�B
�7B
�lB
o�B
CB
#B
�B	�nB	�B	�vB	�B	�]B	�QB	�B	��B	h�B	O(B	;�B	+�B	# B	�B	�B	�B��B�kB�xBĶB��B�'BB�pBܒB��B�B�FB�B�fB��B�qB	B	aB	�B	�B	�B	$B	'mB	)B	/�B	1�B	4TB	88B	8lB	=�B	?.B	B'B	FtB	FYB	FB	GB	GEB	M�B	dB	s�B	��B	�B	��B	�gB	�mB	�1B	�B	�yB	�sB	�sB	��B	��B	��B	��B	�!B	��B	��B	��B	�]B	��B	��B	��B	�QB	�)B	��B	�B	��B	�}B	�/B	�B	�nB	��B	��B	��B	�rB	�xB	�qB	�iB	�GB	ƨB	ňB	�B	��B	��B	��B	��B	�B	ɺB	��B	�.B	��B	уB	ѝB	��B	�&B	��B	�9B	֡B	׍B	�yB	�B	�kB	ڠB	�qB	�xB	�~B	�OB	��B	�!B	߾B	ߊB	ߤB	߾B	�B	��B	�B	�B	�B	��B	�6B	�KB	�B	�B	�KB	�B	�B	��B	��B	��B	�OB	�'B	�AB	�'B	��B	�B	�B	�B	�B	��B	�B	�!B	�B	��B	�B	�'B	�oB	�;B	�AB	��B	��B	�UB	��B	�?B	��B	��B	��B	�rB	��B	��B	��B	��B	��B	� B	�cB	�B	� B	�OB	�!B	�B	�3B	�ZB	�2B	�xB	��B	�B	��B	�RB	�8B	�B	�B	�RB	��B	�B	��B	��B	�?B	�ZB	��B	�B	�TB	��B	�3B	�B	�hB	�3B	��B	�hB	�B	�B	�9B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�zB	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�dB	�PB	�BB	��B	��B	�cB	��B	�cB	��B
  B
 iB
�B
�B
�B
-B
�B
{B
�B
�B
�B
mB
tB
�B
�B
�B
�B
�B
+B
+B
zB
�B
�B
�B
KB
1B
1B
B
B
�B
B
�B
�B
1B
�B
�B
�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B
	�B

rB

�B

�B
�B
B
�B
�B
B
\B
\B
�B
�B
bB
�B
hB
TB
aB
�B
�B
B
MB
MB
MB
2B
�B
YB
�B
�B
�B
�B
B
�B
�B
kB
kB
kB
=B
�B
�B
�B
�B
)B
B
�B
)B
�B
�B
B
OB
�B
�B
B
;B
;B
;B
!B
!B
�B
�B
�B
�B
�B
�B
 �B
 �B
 vB
 vB
 vB
 �B
!-B
!|B
!|B
!�B
!�B
!HB
"NB
"NB
"�B
#�B
#�B
#�B
#�B
$@B
$@B
$�B
$�B
$�B
%B
$�B
$�B
%B
%FB
%�B
%�B
%�B
&�B
'8B
'B
'B
'B
'8B
'RB
'mB
'RB
&�B
(�B
)DB
)DB
)DB
)DB
)�B
)�B
)�B
)�B
*0B
+6B
+�B
+�B
+�B
-)B
-�B
-�B
-�B
-�B
-wB
-�B
-�B
.cB
.IB
.}B
.�B
.�B
.cB
.}B
.�B
.�B
.�B
.�B
/ B
/5B
/OB
/�B
0oB
0oB
0�B
0oB
0�B
0�B
0�B
1'B
1[B
1[B
1�B
2�B
2�B
3�B
4B
4nB
4�B
4�B
4�B
5B
4�B
4�B
5�B
5�B
5�B
5�B
5�B
6�B
6�B
6�B
6�B
6�B
6�B
7B
7B
7B
7B
7�B
7�B
8B
8RB
8RB
88B
8lB
9$B
9$B
9rB
9>B
9�B
9�B
9�B
9�B
9�B
:*B
:^B
:xB
:^B
:�B
:�B
;B
;JB
;�B
;�B
<B
<B
<B
<6B
<jB
=B
="B
="B
=VB
=�B
=�B
>]B
?HB
?HB
?HB
?cB
?�B
?�B
?�B
@B
@4B
@�B
A�B
A�B
BAB
BAB
B[B
B[B
B�B
CaB
CaB
C{B
CaB
CaB
C{B
C{B
C�B
C�B
C�B
C�B
DB
DB
D3B
DgB
D�B
D�B
D�B
D�B
D�B
EB
EmB
E�B
E�B
FYB
F�B
F�B
GB
G+B
GzB
HB
H�B
H�B
HfB
I�B
IlB
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
KxB
K�B
K�B
LJB
L�B
MB
MjB
NB
NVB
NVB
NVB
NpB
NpB
N�B
OB
N�B
OB
N�B
N�B
PHB
PHB
PHB
P}B
P}B
P}B
P�B
P�B
QNB
Q�B
R B
R B
R�B
R�B
S&B
SB
SB
S&B
S[B
S[B
S�B
T{B
T{B
T�B
T�B
T�B
UMB
U�B
U�B
U�B
VB
V�B
W
B
WsB
W�B
XEB
XEB
XEB
X�B
X�B
X�B
Y1B
YB
Y�B
Y�B
ZB
ZB
Y�B
ZkB
Z�B
Z�B
[�B
[�B
[�B
\B
\B
\]B
\�B
\�B
]B
]dB
]dB
]dB
]/B
]�B
]�B
^5B
^jB
^�B
_!B
_VB
_VB
_VB
_�B
`B
`vB
`\B
`�B
`�B
aB
aB
aB
a-B
aHB
a|B
a�B
a�B
a�B
a�B
a�B
b4B
bNB
bB
bhB
cB
b�B
c:B
c�B
c�B
dB
c�B
dB
d&B
dtB
d�B
dtB
d�B
e�B
e�B
e�B
f2B
ffB
ffB
f2B
f�B
f�B
f�B
gRB
g8B
gB
g�B
g�B
g�B
h
B
hXB
h>B
h>B
hXB
h�B
iyB
jB
j0B
jKB
j�B
j�B
j�B
kB
kB
kQB
k�B
l=B
l=B
l�B
l�B
l�B
l�B
m)B
m)B
mCB
m]B
mwB
m�B
m�B
mwB
m�B
m�B
m�B
m�B
n�B
n�B
o B
o5B
o5B
o5B
oOB
o5B
o�B
p!B
p;B
pUB
poB
p�B
p�B
p�B
p�B
q'B
q�B
q�B
rB
raB
rGB
r�B
r�B
s3B
shB
shB
shB
s�B
t9B
tTB
tnB
tTB
t�B
u%B
u%B
u%B
u%B
uZB
utB
u�B
u�B
u�B
u�B
vB
vFB
vFB
vFB
v`B
v�B
v�B
v�B
w2B
w2B
wfB
w�B
w�B
xB
xlB
x8B
xRB
x�B
x�B
x�B
x�B
y$B
yXB
yXB
y�B
y�B
y�B
z�B
z�B
z�B
z�B
{0B
{0B
{0B
{JB
{B
{�B
{�B
{�B
|B
|PB
|6B
|6B
|�B
|�B
}B
}�B
}�B
}�B
}�B
~BB
~B
~BB
~�B
~�B
.B
cB
HB
HB
HB
HB
HB
HB
HB
~�B
~�B
HB
cB
�B
�B
�B
� B
� B
� B
�B
� B
� B
� B
� B
�B
�4B
�B
�B
�4B
�B
�4B
�iB
�OB
�4B
��B
� B
� B
�;B
�;B
�oB
��B
��B
�B
�B
�B
�'B
�[B
�-B
�GB
�-B
�GB
�GB
�GB
�-B
�aB
��111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B	�B	qB	#B	�B	�B	�B	B	eB	KB	eB	KB	eB	�B	�B	�B	�B	�B	B	�B	B	EB	_B	yB	�B	�B	�B	B	$�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	�+B	�2B
�B
"�B
E�B
R�B
d�B
x�B
��B
�	B
�#B
�@B
�CB
��B
�B
�@B
�BBB �BabB� B�,B�BB!|B(>B)yB($B~B�B�B�(B�B�B��B�)B�dB�GB��B�Bw�Bo�BjKBd@BW�BJ�B?�B6zB'RBB
��B
ߊB
��B
��B
��B
w�B
I�B
;B
B	�+B	��B	� B	�EB	�UB	�/B	��B	�+B	n�B	S[B	?�B	-�B	%FB	~B	�B	(B�^B�4B��B�?B�YB��B�B��B�|B�tB�B��B��B�B��B�]B	{B	3B	�B	KB	B	%,B	(�B	+�B	0�B	2�B	5?B	9	B	9�B	>]B	@ B	CaB	F�B	GB	F�B	HB	HB	NVB	d&B	tB	�B	�&B	��B	��B	�SB	��B	��B	��B	��B	�yB	��B	��B	��B	� B	�AB	��B	��B	��B	�}B	��B	��B	��B	�B	��B	�B	�cB	�OB	�OB	�B	��B	��B	��B	�B	��B	��B	�0B	�B	��B	��B	�+B	�tB	�B	�"B	ϫB	�vB	�)B	ɠB	�XB	̘B	�bB	� B	ѝB	��B	�:B	�[B	�2B	�mB	��B	��B	خB	ٚB	ڠB	ںB	ۦB	ܬB	ݘB	�OB	��B	�;B	��B	ߤB	߾B	��B	��B	�hB	��B	�ZB	��B	�*B	�B	�B	�B	��B	�B	�6B	�B	�B	�B	�B	�B	�vB	�vB	�[B	�AB	�[B	�B	�'B	�'B	�[B	��B	�B	�B	�aB	��B	�B	�AB	�[B	�B	�B	�B	��B	�'B	�ZB	��B	��B	��B	��B	��B	�^B	��B	��B	�|B	�OB	�B	��B	�OB	�B	�!B	�'B	�B	��B	��B	��B	�XB	�B	��B	�RB	�RB	�8B	�lB	��B	�B	�LB	��B	�`B	��B	��B	��B	��B	��B	�9B	�B	�B	�B	�B	�B	�B	��B	��B	��B	�?B	�?B	�ZB	��B	��B	�B	�B	��B	�B	�FB	��B	�LB	�B	�8B	�B	�B	�8B	�RB	�lB	��B	��B	�$B	�$B	�	B	�rB	�^B	��B	�6B	�VB	��B	�]B	�cB	�}B	��B	��B	�}B
 �B
 B
[B
�B
�B
�B
�B
�B
�B
�B
B
�B
tB
�B
�B
�B
�B
B
EB
EB
zB
�B
�B
�B
fB
1B
1B
B
B
B
B
�B
1B
�B
fB
	lB
	7B

	B
	�B
	�B
	�B
	�B
	�B
	�B

	B
	�B

	B

#B

#B

�B
DB
DB
�B
6B
�B
"B
BB
�B
�B
�B
HB
�B
�B
B
B
�B
�B
�B
2B
MB
�B
�B
�B
B
�B
�B
�B
�B
+B
_B
1B
B
�B
�B
�B
=B
�B
�B
�B
)B
]B
CB
B
]B
�B
�B
B
OB
�B
B
;B
VB
VB
VB
;B
pB
�B
 B
 B
 B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
!|B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#:B
#�B
#�B
#�B
$&B
$@B
$tB
$�B
$�B
$�B
%,B
$�B
$�B
%`B
%zB
&B
&B
%�B
&�B
'RB
'8B
'B
'8B
'RB
'mB
'�B
'�B
'mB
)*B
)_B
)DB
)_B
)_B
)�B
)�B
)�B
)�B
*B
+QB
+�B
,B
,WB
-wB
-�B
-�B
-�B
-�B
-�B
-�B
.IB
.}B
.}B
.�B
/ B
.�B
.}B
.}B
.�B
.�B
.�B
/ B
/B
/iB
/iB
/�B
0�B
0oB
0�B
0�B
0�B
0�B
1B
1[B
1�B
1�B
2GB
33B
3hB
49B
49B
4�B
4�B
4�B
4�B
5B
5B
5?B
5�B
5�B
5�B
5�B
5�B
6�B
6�B
6�B
7B
7B
6�B
7B
72B
72B
7LB
7�B
7�B
88B
8lB
8lB
8lB
8�B
9XB
9>B
9�B
9XB
9�B
9�B
9�B
9�B
9�B
:DB
:xB
:�B
:�B
:�B
:�B
;JB
;B
;�B
;�B
<B
<6B
<PB
<PB
<�B
="B
=VB
=VB
=�B
>B
>B
>�B
?cB
?cB
?cB
?}B
?�B
?�B
@ B
@4B
@�B
A B
A�B
BB
B[B
B[B
BuB
BuB
CB
C{B
C{B
C{B
C{B
C{B
C�B
C�B
C�B
C�B
C�B
DB
D3B
D3B
DMB
D�B
D�B
D�B
D�B
D�B
EB
ESB
E�B
E�B
FB
F�B
F�B
F�B
G+B
G_B
G�B
HKB
H�B
H�B
H�B
I�B
I�B
J=B
J�B
J�B
J�B
J�B
J�B
J�B
K)B
K�B
K�B
LB
LdB
L�B
MPB
M�B
N<B
NVB
NVB
NVB
N�B
N�B
N�B
O(B
OB
O(B
O(B
O\B
PbB
PHB
PHB
P}B
P�B
P�B
P�B
QB
Q�B
Q�B
RTB
RTB
R�B
R�B
S&B
SB
S&B
S@B
SuB
S�B
T,B
T�B
T�B
T�B
T�B
UB
UgB
U�B
U�B
VB
VSB
V�B
W?B
W�B
XB
X_B
X_B
XyB
X�B
X�B
YB
YeB
Y�B
Y�B
ZB
ZB
ZB
ZB
Z�B
Z�B
[#B
[�B
[�B
[�B
\CB
\)B
\�B
\�B
\�B
]/B
]dB
]dB
]~B
]dB
]�B
]�B
^OB
^�B
^�B
_;B
_VB
_pB
_�B
`B
`'B
`�B
`vB
`�B
`�B
aB
a-B
a-B
aHB
aHB
a�B
a�B
a�B
a�B
a�B
a�B
b4B
bhB
bB
b�B
cB
cB
cnB
c�B
c�B
d&B
c�B
d&B
d@B
d�B
d�B
d�B
e,B
e�B
e�B
fB
fLB
f�B
ffB
ffB
f�B
gB
f�B
gRB
gRB
gB
h
B
g�B
g�B
h$B
h>B
h>B
hXB
hXB
h�B
i�B
jB
jKB
jeB
j�B
j�B
kB
k6B
k6B
k�B
k�B
l=B
lWB
l�B
l�B
l�B
mB
m)B
mCB
m]B
mwB
m�B
mwB
m�B
m�B
m�B
m�B
nB
nB
n�B
n�B
oB
o5B
o5B
oOB
oOB
oiB
o�B
p;B
p;B
poB
p�B
p�B
p�B
p�B
qB
q[B
q�B
q�B
r-B
r|B
raB
sB
sB
s3B
s�B
shB
s�B
s�B
t9B
tTB
tnB
tnB
t�B
u%B
u%B
u%B
u?B
utB
u�B
u�B
u�B
u�B
u�B
v+B
v+B
v+B
v`B
vzB
v�B
v�B
v�B
w2B
wLB
w�B
w�B
w�B
xB
xlB
xRB
xlB
x�B
x�B
x�B
y	B
y$B
yXB
yrB
y�B
y�B
y�B
zxB
z�B
z�B
z�B
{B
{B
{0B
{JB
{dB
{B
{�B
{�B
|B
|jB
|PB
|B
|jB
}B
}"B
}�B
~B
}�B
~(B
~]B
~(B
~BB
~�B
~�B
cB
HB
HB
HB
HB
.B
HB
cB
cB
~�B
.B
cB
}B
�B
�B
�B
� B
� B
� B
�B
�B
� B
� B
� B
�B
�4B
� B
�B
�4B
�4B
�OB
��B
�iB
�iB
�B
� B
�B
�;B
�UB
��B
��B
��B
�B
��B
�'B
�'B
�uB
�-B
�-B
�-B
�GB
�GB
�-B
�-B
�{B
��311111111111111111111111111144444441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=-0.04(dbar)                                                                                                                                                                                                                                                  None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201904240042442019042400424420190424004244202207271130442022072711304420220727113044202207271533292022072715332920220727153329  JA  ARFMdecpA30a                                                                20190523095841  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190523100025  IP                  G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190523100026  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190523100027  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190523100027  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20190523100027  QCF$                G�O�G�O�G�O�            4000JA      jafc1.0                                                                 20190523100027                      G�O�G�O�G�O�                JA  ARUP                                                                        20190523111515                      G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20190415000000  CF  TEMP_ADJUSTED_QCB��B��fG�O�                JM  ARCAJMQC2.0                                                                 20190423154244  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190423154244  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQJMQC2.0                                                                 20220727000000  CF  PSAL_ADJUSTED_QC@��B���G�O�                JM  ARCAJMTM1.0                                                                 20220727023044  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 CTD2021V2                                                       20220727063329  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20220818081507                      G�O�G�O�G�O�                